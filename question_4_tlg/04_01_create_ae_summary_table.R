################################################################################
## File Name: 04_01_create_ae_summary_table.R
## Purpose: Create a summary table of treatment-emergent adverse events (TEAEs)
## Author: Yifeng Wu
################################################################################

################################################################################
## 0. Setup
################################################################################

# Clear workspace
rm(list = ls())

# Required packages
# install.packages(c("tidyverse", "gtsummary", "pharmaverseadam"))
library(dplyr)
library(gtsummary)

################################################################################
## 1. Input Data
################################################################################

# ADaM datasets
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Treatment-emergent AEs
te_adae <- adae |>
  filter(TRTEMFL == "Y")

################################################################################
## 2. TEAE Summary Table
################################################################################

# FDA Table 10-style output
# Rows: AESOC -> AETERM
# Columns: ACTARM
# Cells: n (%), denominator = ADSL

te_adae |>
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    id = USUBJID,
    denominator = adsl,
    by = ACTARM,
    overall_row = TRUE,
    label = list(..ard_hierarchical_overall.. = "Treatment Emergent AEs"),
    digits = everything() ~ list(p = 1)
  ) |>
  modify_caption("**Table 10. Subjects With Treatment Emergent Adverse Events by System Organ Class and Preferred Term**") |>
  as_gt() |>
  gt::gtsave(filename = "question_4_tlg/Question_4_1_AE_Summary_Table.html")
