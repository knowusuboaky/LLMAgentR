# tests/testthat/test_build_data_cleaning_agent.R

context("build_data_cleaning_agent()")

library(testthat)

# ── skip if tidyverse stack missing ────────────────────────────────────────
needed <- c("dplyr", "tidyr", "stringr", "lubridate", "magrittr")
for (pkg in needed) skip_if_not_installed(pkg)

# ── Helper: sample data with some NAs and a duplicate row ──────────────────
set.seed(100)
raw_df <- data.frame(
  id     = c(1:5, 5),                 # duplicate id 5
  score  = c(10, NA, 30, NA, 50, 50), # NAs to impute
  group  = c("A", "A", "B", "B", "A", "A"),
  stringsAsFactors = FALSE
)

# ── Fake LLM generator (plain R code, no fences) ─────────────────────────
make_fake_model <- function(valid_code = TRUE) {
  good_fun <- paste(
    "data_cleaner <- function(data_raw) {",
    "  library(dplyr)",
    "  library(tidyr)",
    "  data_cleaned <- data_raw %>%",
    "    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%",
    "    distinct()",
    "  return(data_cleaned)",
    "}",
    sep = "\n"
  )

  bad_fun <- "data_cleaner_bad <- function(x) stop('oops')"

  function(prompt) {
    if (grepl("Data Cleaning Agent specialized", prompt, fixed = TRUE)) {
      if (valid_code) good_fun else bad_fun
    } else {
      "Here are some cleaning steps..."
    }
  }
}

# ── 1. Happy path: cleaner returns a de‑duplicated, imputed data set ──────
test_that("data‑cleaning agent produces cleaned data with valid code", {
  agent <- build_data_cleaning_agent(
    model                    = make_fake_model(TRUE),
    bypass_recommended_steps = TRUE,   # start at code gen
    bypass_explain_code      = TRUE
  )

  st <- list(data_raw = raw_df)

  res <- suppressWarnings(agent(st))

  # No error should be recorded
  expect_null(res$data_cleaner_error)

  # Should produce a data frame of 5 unique ids with no NA score
  clean_df <- as.data.frame(res$data_cleaned, stringsAsFactors = FALSE)

  expect_true(is.data.frame(clean_df))
  expect_equal(nrow(clean_df), 5)          # duplicate removed
  expect_false(any(is.na(clean_df$score))) # NAs imputed
})

# ── 2. Failure path: agent throws when code is invalid ───────────────────
test_that("data‑cleaning agent errors clearly when code is invalid", {
  bad_agent <- build_data_cleaning_agent(
    model                    = make_fake_model(FALSE),
    bypass_recommended_steps = TRUE,
    bypass_explain_code      = TRUE
  )

  st_bad <- list(data_raw = raw_df)

  expect_error(
    suppressWarnings(bad_agent(st_bad)),
    "No valid 'data_cleaner' function detected"
  )
})
