################################################################################
## File Name: 03_create_adsl.R
## Purpose: create ADSL
## Author: Yifeng Wu
################################################################################

################################################################################
## 0. Setup
################################################################################

# Clear workspace
rm(list = ls())

# Required packages
# install.packages(c("tidyverse", "admiral", "pharmaversesdtm"))
library(dplyr)
library(admiral)

################################################################################
## 1. Input Data
################################################################################

# SDTM domains
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

# Convert blanks to NA
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# Initialize ADSL from DM
adsl <- dm |>
  select(-DOMAIN)

################################################################################
##  2. Derived Variables
################################################################################

# ---- TRTSDTM / TRTSTMF / TRTEDTM / TRTETMF
# Derive exposure start/end datetime (time imputed to 00:00:00)
# Valid dose: EXDOSE > 0 or (EXDOSE == 0 & EXTRT contains "PLACEBO")

ex_ext <- ex |>
  derive_vars_dtm(
    new_vars_prefix = "EXST",
    dtc = EXSTDTC,
    highest_imputation = "h",
    time_imputation = "00:00:00",
    flag_imputation = "time",
    ignore_seconds_flag = TRUE
  ) |>
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "00:00:00"
  )

adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# ---- AGEGR9 / AGEGR9N
# Age groups: <18, 18–50, >50

agegr9_lookup <- exprs(
  ~condition,           ~AGEGR9,
  AGE < 18,             "<18",
  between(AGE, 18, 50), "18-50",
  AGE > 50,             ">50",
  is.na(AGE),           "Missing"
)

agegr9n_lookup <- exprs(
  ~condition,           ~AGEGR9N,
  AGE < 18,             1,
  between(AGE, 18, 50), 2,
  AGE > 50,             3,
  is.na(AGE),           NA
)

adsl <- adsl |>
  derive_vars_cat(definition = agegr9_lookup) |>
  derive_vars_cat(definition = agegr9n_lookup)

# ---- LSTALVDT
# Last known alive date from VS, AE, DS, or treatment end

adsl <- adsl |>
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = (!is.na(VSSTRESN) | !is.na(VSSTRESC)) &
          !is.na(as.Date(VSDTC)),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "M"),
          seq = VSSEQ
        )
      ),
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
          seq = AESEQ
        )
      ),
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M"),
          seq = DSSEQ
        )
      ),
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDTM),
        set_values_to = exprs(LSTALVDT = TRTEDTM, seq = 0)
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTALVDT)
  )

# ---- ITTFL
# Randomized subjects (ARM not missing)

adsl <- adsl |>
  derive_var_merged_exist_flag(
    dataset_add = dm,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = ITTFL,
    false_value = "N",
    missing_value = "N",
    condition = !is.na(ARM)
  )

# ---- ABNSBPFL
# Supine SYSBP <100 or ≥140 mmHg

adsl <- adsl |>
  derive_var_merged_exist_flag(
    dataset_add = vs,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = ABNSBPFL,
    false_value = "N",
    missing_value = "N",
    condition = VSTESTCD == "SYSBP" &
      VSSTRESU == "mmHg" &
      (VSSTRESN >= 140 | VSSTRESN < 100)
  )

# ---- CARPOPFL
# Cardiac disorder AE (AESOC)

adsl <- adsl |>
  derive_var_merged_exist_flag(
    dataset_add = ae,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = CARPOPFL,
    false_value = "N",
    missing_value = "N",
    condition = toupper(AESOC) == "CARDIAC DISORDERS"
  )

################################################################################
## 3. Output
################################################################################

# Verify required variables
vars_to_check <- c(
  "STUDYID", "USUBJID", "AGEGR9", "AGEGR9N",
  "TRTSDTM", "TRTSTMF", "ITTFL",
  "ABNSBPFL", "LSTALVDT", "CARPOPFL"
)

if (!all(vars_to_check %in% names(adsl))) {
  stop("Some required variables are missing.", call. = FALSE)
}

# Save dataset
save(adsl, file = "question_3_adam/adsl.rda")

