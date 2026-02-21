################################################################################
## File Name: 04_02_create_visualizations.R
## Purpose: Create AE severity distribution by treatment (bar chart or heatmap)
##          Create top 10 most frequent AEs (with 95% CI for incidence rates)
## Author: Yifeng Wu
################################################################################

################################################################################
## 0. Setup
################################################################################

# Clear workspace
rm(list = ls())

# Required packages
# install.packages(c("tidyverse", "patchwork", "pharmaverseadam"))
library(dplyr)
library(ggplot2)
library(patchwork)

################################################################################
## 1. Input Data
################################################################################

# ADaM dataset
adae <- pharmaverseadam::adae

################################################################################
## 2. AE Severity Distribution
################################################################################

# Bar chart: AESEV by ACTARM (counts)
p1 <- adae |>
  ggplot(aes(x = ACTARM, fill = AESEV)) +
  geom_bar() +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment Arm",
    y = "Count of AEs"
  ) +
  theme_minimal()

################################################################################
## 3. Top 10 Most Frequent AEs
################################################################################

# Subject-level incidence with 95% Clopper-Pearson CIs
# Top 10 AETERM by percentage
ct_aeterm <- adae |>
  distinct(USUBJID, AETERM) |>
  count(AETERM, name = "n") |>
  mutate(
    total_n = n_distinct(adae$USUBJID),
    pct   = 100 * n / total_n,
    lower = 100 * qbeta(0.025, n, total_n - n + 1),
    upper = 100 * qbeta(0.975, n + 1, total_n - n)
  ) |>
  arrange(desc(pct)) |>
  slice_head(n = 10) |>
  mutate(AETERM = reorder(AETERM, pct))

# Forest plot: incidence (%) with 95% CI
p2 <- ct_aeterm |>
  ggplot(aes(x = pct, y = AETERM)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_point(size = 3) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0(
      "n = ", unique(ct_aeterm$total_n),
      " subjects; 95% Clopper-Pearson CIs"
    ),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

################################################################################
## 4. Output
################################################################################

# Combine and save
p <- free(p1) / p2

ggsave(
  "Question_4_2_AE_Visualizations.png",
  p,
  width = 8.5,
  height = 11,
  dpi = 300
)
