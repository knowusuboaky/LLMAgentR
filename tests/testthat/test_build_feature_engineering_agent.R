# tests/testthat/test_build_feature_engineering_agent.R

context("build_feature_engineering_agent()")

library(testthat)

# ── Skip if required packages absent ──────────────────────────────────────
for (pkg in c("dplyr", "tidyr", "recipes", "magrittr")) {
  skip_if_not_installed(pkg)
}

# ── Fake LLM generator (plain R code, no markdown fences) ─────────────────
make_fake_model <- function(valid = TRUE) {
  good_code <- paste(
    "feature_engineer <- function(data_raw) {",
    "  if (!requireNamespace('dplyr', quietly = TRUE)) stop('dplyr needed');",
    "  library(dplyr)",
    "  if (!is.data.frame(data_raw)) data_raw <- as.data.frame(data_raw)",
    "  data_engineered <- data_raw %>%",
    "    mutate(sepal_area = Sepal.Length * Sepal.Width)",
    "  return(data_engineered)",
    "}",
    sep = "\n"
  )
  bad_code <- "bogus_fun <- function(x) x"

  function(prompt) {
    if (grepl("Feature Engineering Agent. Your job is to create", prompt, fixed = TRUE)) {
      if (valid) good_code else bad_code
    } else {
      "1. Create sepal_area = Sepal.Length*Sepal.Width"
    }
  }
}

# ── Input data (built‑in iris) ────────────────────────────────────────────
iris_small <- head(iris, 10)

# ── 1. Happy‑path: engineered column present, no error flag  -------------–
test_that("feature‑engineering agent returns engineered data on valid code", {
  agent <- build_feature_engineering_agent(
    model                    = make_fake_model(TRUE),
    bypass_recommended_steps = TRUE,
    bypass_explain_code      = TRUE
  )

  st <- list(data_raw = iris_small)

  res <- suppressWarnings(agent(st))

  expect_null(res$feature_engineer_error)
  expect_true("sepal_area" %in% names(res$data_engineered))
  expect_equal(
    res$data_engineered$sepal_area,
    iris_small$Sepal.Length * iris_small$Sepal.Width
  )
})

# ── 2. Error‑path: invalid code triggers clear error ----------------------–
test_that("feature‑engineering agent errors clearly when code is invalid", {
  bad_agent <- build_feature_engineering_agent(
    model                    = make_fake_model(FALSE),
    bypass_recommended_steps = TRUE,
    bypass_explain_code      = TRUE
  )

  st_bad <- list(data_raw = iris_small)

  expect_error(
    suppressWarnings(bad_agent(st_bad)),
    "No valid 'feature_engineer' function detected"
  )
})
