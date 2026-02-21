################################################################################
## File Name: 04_03_create_ae_listing.R
## Purpose: Create detailed listing of all AEs
## Author: Yifeng Wu
################################################################################

################################################################################
## 0. Setup
################################################################################

# Clear workspace
rm(list = ls())

# Required packages
# install.packages(c("tidyverse", "gt"))
library(dplyr)
library(gt)

################################################################################
## 1. Input Data
################################################################################

# Treatment-emergent AEs
# Sort by subject and start datetime
# Create display-only subject and arm columns (shown once per subject)
adae <- pharmaverseadam::adae |>
  filter(TRTEMFL == "Y") |>
  arrange(USUBJID, ASTDTM) |>
  group_by(USUBJID, ACTARM) |>
  mutate(
    USUBJID_GRP = if_else(row_number() == 1, USUBJID, ""),
    ACTARM_GRP  = if_else(row_number() == 1, ACTARM, "")
  ) |>
  ungroup() |>
  select(
    USUBJID_GRP, ACTARM_GRP,
    AETERM, AESEV, AREL,
    ASTDTM, AENDTM
  )


################################################################################
## 2. AE Listing Output
################################################################################

# Subject-level AE listing (HTML)

gt_tbl <- adae |>
  gt() |>
  tab_header(
    title = "Listing of Treatment-Emergent Adverse Events by Subject",
    subtitle = "Excluding Screen Failure Patients"
  ) |>
  cols_label(
    USUBJID_GRP = "Unique Subject Identifier",
    ACTARM_GRP  = "Description of Actual Arm",
    AETERM      = "Reported Term for the Adverse Event",
    AESEV       = "Severity/Intensity",
    AREL        = "Causality",
    ASTDTM      = "Start Date/Time of Adverse Event",
    AENDTM      = "End Date/Time of Adverse Event"
  ) |>
  cols_align(align = "left")

# Save output
gtsave(gt_tbl, "question_4_tlg/Question_4_3_AE_Listing.html")
