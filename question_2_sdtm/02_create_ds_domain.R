################################################################################
## File Name: 02_create_ds_domain.R
## Purpose: create SDTM DS domain
## Author: Yifeng Wu
################################################################################

################################################################################
## 0. Setup
################################################################################

# Clear workspace
rm(list = ls())

# Required packages
# install.packages(c("tidyverse", "sdtm.oak",
#                    "pharmaverseraw", "pharmaversesdtm"))
library(dplyr)
library(sdtm.oak)


################################################################################
## 1. Input Data
################################################################################

# Raw DS and SDTM DM
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

# Controlled terminology (CT) from Github
url <- "https://raw.githubusercontent.com/pharmaverse/examples/main/metadata/sdtm_ct.csv"
study_ct <- readr::read_csv(url)

################################################################################
## 2. Variables by mapping
################################################################################

# Create oak_id_vars (row-level keys)
ds_raw <- ds_raw |>
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# ---- DSTERM & DSDECOD (no CT mapping)
# If OTHERSP present -> map from OTHERSP
# Else -> map from IT.DSTERM / IT.DSDECOD
ds <-
  assign_no_ct(tgt_var = "DSTERM",
               raw_dat = ds_raw,
               raw_var = "OTHERSP"
  ) |>
  assign_no_ct(tgt_var = "DSTERM",
               raw_dat = ds_raw,
               raw_var = "IT.DSTERM"
  ) |>
  assign_no_ct(tgt_var = "DSDECOD",
               raw_dat = ds_raw,
               raw_var = "OTHERSP"
  ) |>
  assign_no_ct(tgt_var = "DSDECOD",
               raw_dat = ds_raw,
               raw_var = "IT.DSDECOD"
  )

# ---- DSSTDTC (ISO8601 from IT.DSSTDAT)
# ---- DSDTC (ISO8601 from date + time)
ds <- ds |>
  assign_datetime(
    tgt_var = "DSSTDTC",
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    raw_fmt = "m-d-y"
  ) |>
  assign_datetime(
    tgt_var = "DSDTC",
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    raw_fmt = c("m-d-y", "H:M"),
  )

# ---- VISIT & VISITNUM (CT mapping from INSTANCE)
# Note: unscheduled visits may not map
ds <- ds |>
  assign_ct(
    tgt_var = "VISIT",
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    ct_spec = study_ct,
    ct_clst = "VISIT"
  ) |>
  assign_ct(
    tgt_var = "VISITNUM",
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    ct_spec = study_ct,
    ct_clst = "VISITNUM"
  )

################################################################################
## 3. Derived Variables
################################################################################

# ---- DSCAT
# If OTHERSP present -> "OTHER EVENT"
# If IT.DSDECOD = "Randomized" -> "PROTOCOL MILESTONE"
# Else -> "DISPOSITION EVENT"
ds_raw <- ds_raw |>
  mutate(
    DSCAT = case_when(
      !is.na(OTHERSP)            ~ "OTHER EVENT",
      IT.DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",
      .default                   = "DISPOSITION EVENT"
    )
  )

# ---- STUDYID, DOMAIN, USUBJID
# follow the Pharmaverse AE Examples
ds <- ds |>
  mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    DSCAT   = ds_raw$DSCAT
  )

# ---- DSSEQ (within USUBJID by DSSTDTC)
ds <- ds |>
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC")
  )

# ---- DSSTDY (relative to DM.RFXSTDTC)
ds <- ds |>
  derive_study_day(
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  )

# Only keep the variables indicated in the expected result
ds <- ds |>
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM,
         VISIT, DSDTC, DSSTDTC, DSSTDY
  )

################################################################################
## 4. Output
################################################################################

save(ds, file = "question_2_sdtm/ds.rda")


