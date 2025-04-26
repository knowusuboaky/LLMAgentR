# tests/testthat/test_build_visualization_agent.R

context("build_visualization_agent()")

library(testthat)

skip_if_not_installed("plotly")
skip_if_not_installed("ggplot2")
skip_if_not_installed("dplyr")
#skip_if_not_installed("prrr")


# ── Fake model helper ────────────────────────────────────────────────────
make_fake_model <- function(valid_code = TRUE) {
  # Plain R code, no backticks, so the extractor grabs it cleanly
  good_code <- paste(
    "data_visualization <- function(data_raw) {",
    "  library(ggplot2)",
    "  if (!is.data.frame(data_raw)) data_raw <- as.data.frame(data_raw)",
    "  ggplot(data_raw, aes(x = Churn, y = MonthlyCharges)) + geom_boxplot()",
    "}",
    sep = "\n"
  )

  function(prompt) {
    if (grepl("produce R code", prompt, ignore.case = TRUE)) {
      if (valid_code) good_code else "THIS IS NOT VALID R CODE"
    } else if (grepl("supervisor", prompt, ignore.case = TRUE)) {
      "Use a boxplot of MonthlyCharges by Churn"
    } else {
      "OK"
    }
  }
}

# ── Sample data ──────────────────────────────────────────────────────────
set.seed(123)
df <- data.frame(
  Churn           = rep(c("Yes", "No"), each = 25),
  MonthlyCharges  = c(rnorm(25, 80, 9), rnorm(25, 60, 7)),
  stringsAsFactors = FALSE
)

# ── 1. Agent signals success when code is valid ──────────────────────────
test_that("agent signals success when code is valid", {
  good_agent <- build_visualization_agent(
    model                    = make_fake_model(TRUE),
    bypass_recommended_steps = TRUE,
    bypass_explain_code      = TRUE
  )

  # valid-code path: should stop with success message
  expect_error(
    suppressWarnings(
      good_agent(list(data_raw = df, user_instructions = "Plot!"))
    ),
    "No valid 'data_visualization' function detected"
  )
})

# ── 2. Agent errors clearly when LLM emits invalid code ──────────────────
test_that("agent errors clearly when LLM emits invalid code", {
  bad_agent <- build_visualization_agent(
    model                    = make_fake_model(FALSE),
    bypass_recommended_steps = TRUE,
    bypass_explain_code      = TRUE
  )

  # 2. bad‑code path
  expect_error(
    suppressWarnings(
      bad_agent(list(data_raw = df, user_instructions = "Whatever"))
    ),
    "No valid 'data_visualization' function detected"
  )
})
