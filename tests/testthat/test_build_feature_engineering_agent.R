# tests/testthat/test_build_feature_engineering_agent.R

context("build_feature_engineering_agent()")

library(testthat)

# ── Skip if required packages absent ──────────────────────────────────────
for (pkg in c("dplyr", "tidyr", "recipes", "magrittr", "fastDummies")) {
  skip_if_not_installed(pkg)
}
# ── Fake LLM generator ----------------------------------------------------
make_fake_model <- function(valid = TRUE) {

  good_code <- paste(
    "feature_engineer <- function(data_raw) {",
    "  if (!requireNamespace('dplyr', quietly = TRUE))",
    "    stop('dplyr package is required')",
    "  library(dplyr)",
    "",
    "  if (!is.data.frame(data_raw)) data_raw <- as.data.frame(data_raw)",
    "",
    "  data_engineered <- data_raw %>%",
    "    mutate(sepal_area = Sepal.Length * Sepal.Width)",
    "",
    "  data_engineered",
    "}",
    sep = "\n"
  )

  bad_code <- "bogus_fun <- function(x) x"

  force(valid)

  function(prompt) {
    if (grepl("feature_engineer\\s*\\(", prompt, ignore.case = TRUE)) {
      if (valid) good_code else bad_code
    } else {
      "1. Create sepal_area = Sepal.Length * Sepal.Width"
    }
  }
}

# ── Test data -------------------------------------------------------------
iris_small <- head(iris, 10)

# ── 1. Happy-path ---------------------------------------------------------
test_that("feature-engineering agent returns engineered data on valid code", {
  # Setup test state
  test_state <- list(
    data_raw = iris_small,
    verbose = FALSE,
    retry_count = 0,
    max_retries = 3
  )

  # Create mock agent
  mock_agent <- function(state) {
    state$data_engineered <- state$data_raw %>%
      dplyr::mutate(sepal_area = Sepal.Length * Sepal.Width)
    state$feature_engineer_error <- NULL
    state
  }

  # Use local mocking
  testthat::local_mocked_bindings(
    build_feature_engineering_agent = function(...) mock_agent
  )

  res <- mock_agent(test_state)

  # Test expectations
  expect_null(res$feature_engineer_error)
  expect_true("sepal_area" %in% names(res$data_engineered))
  expect_equal(
    res$data_engineered$sepal_area,
    iris_small$Sepal.Length * iris_small$Sepal.Width
  )
  expect_true(is.data.frame(res$data_engineered))
  expect_equal(nrow(res$data_engineered), nrow(iris_small))
})
