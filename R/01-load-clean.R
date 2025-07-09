# R/01-load-clean.R ------------------------------------------------------------
# Purpose : Read, clean, and prepare the AI‑survey data for analysis
# ------------------------------------------------------------------------------
# Packages
suppressPackageStartupMessages({
  library(tidyverse)   # readr, dplyr, tidyr, ggplot2, forcats, etc.
  library(janitor)     # clean_names()
})

# ── 1. Read raw CSV ───────────────────────────────────────────────────────────
# Use a relative path so it works on GitHub Pages / Quarto Pub.
survey_raw <- read_csv(
  "data/Survey_AI.csv",
  show_col_types = FALSE
)

# ── 2. Tidy column names ──────────────────────────────────────────────────────
# Example: "Q2#1.Internet"  →  "q2_number_1_internet"
survey <- survey_raw %>% clean_names()

# ── 3. Identify binary (0/1) columns ──────────────────────────────────────────
binary_cols <- grep("^q2_number_|^q6_number_", names(survey), value = TRUE)

# ── 4. Basic variable conversions ─────────────────────────────────────────────
survey <- survey %>%
  # 4.1 0/1 → logical
  mutate(across(all_of(binary_cols), ~ .x == 1)) %>%
  # 4.2 gender factor
  mutate(
    q12_gender = factor(
      q12_gender,
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  ) %>%
  # 4.3 year of study ordered factor
  mutate(q13_year_of_study = factor(q13_year_of_study, ordered = TRUE)) %>%
  # 4.4 knowledge‑level buckets
  mutate(
    knowledge_level = case_when(
      q1_ai_knowledge <= 3  ~ "Low",
      q1_ai_knowledge <= 7  ~ "Medium",
      TRUE                  ~ "High"
    ) %>% factor(levels = c("Low", "Medium", "High"))
  )

# ── 5. Convenience renames ────────────────────────────────────────────────────
survey <- survey %>%
  rename(
    major            = q14_major,
    gpa              = q16_gpa,
    utility_grade    = q7_utility_grade,
    advantage_teach  = q8_advantage_teaching,
    advantage_learn  = q9_advantage_learning,
    advantage_eval   = q10_advantage_evaluation,
    disadvantage_ed  = q11_disadvantage_educational_process
  )

# ── 6. Derived variables ──────────────────────────────────────────────────────
# Binary “trust” indicator: utility_grade ≥ 4  ⇒ Agree
survey <- survey %>%
  mutate(
    grading_trust = factor(
      if_else(utility_grade >= 4, "Agree", "Disagree"),
      levels = c("Disagree", "Agree")
    )
  )

# ── 7. Export cleaned data (optional) ─────────────────────────────────────────
write_rds(survey, "data/ai_clean.rds", compress = "xz")

# Make cleaned tibble available as survey_data
survey_data <- survey
