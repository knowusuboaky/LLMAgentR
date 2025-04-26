# tests/testthat/test_build_data_wrangling_agent.R

# tests/testthat/test_build_data_wrangling_agent.R

context("build_data_wrangling_agent()")

library(testthat)

# ── skip if core tidyverse packages missing ───────────────────────────────
for (pkg in c("dplyr", "tidyr", "magrittr", "purrr")) {
  skip_if_not_installed(pkg)
}

# ── Sample data: two frames that share an id column ───────────────────────
df1 <- data.frame(id = 1:3, val1 = c("a", "b", "c"), stringsAsFactors = FALSE)
df2 <- data.frame(id = 2:4, val2 = c(10, 20, 30), stringsAsFactors = FALSE)

# ── Fake LLM helper (plain R code, no fences) ────────────────────────────
make_fake_model <- function(valid_code = TRUE) {
  good_fun <- paste(
    "data_wrangler <- function(data_list) {",
    "  library(dplyr)",
    "  if (!is.list(data_list)) data_list <- list(main = data_list)",
    "  df <- purrr::reduce(data_list, dplyr::full_join, by = 'id')",
    "  return(df)",
    "}",
    sep = "\n"
  )

  bad_fun <- "bogus_fun <- function(x) x"

  function(prompt) {
    if (grepl("Data Wrangling Coding Agent", prompt, fixed = TRUE)) {
      if (valid_code) good_fun else bad_fun
    } else {
      "--step list--"
    }
  }
}

# ── 1. Happy-path: wrangler merges the frames correctly ───────────────────
test_that("wrangling agent returns merged data on valid code", {
  agent <- build_data_wrangling_agent(
    model                    = make_fake_model(TRUE),
    bypass_recommended_steps = TRUE,
    bypass_explain_code      = TRUE,
    verbose                  = FALSE
  )

  st <- list(data_raw = list(df1 = df1, df2 = df2))

  res <- suppressWarnings(agent(st))

  expect_null(res$data_wrangler_error)
  expect_true(is.data.frame(res$data_wrangled))
  expect_setequal(names(res$data_wrangled), c("id", "val1", "val2"))
  expect_equal(nrow(res$data_wrangled), 4)
})

# ── 2. Failure-path: invalid code triggers clear error at execution node ─
test_that("wrangling agent errors clearly when code is invalid", {
  bad_agent <- build_data_wrangling_agent(
    model                    = make_fake_model(FALSE),
    bypass_recommended_steps = TRUE,
    bypass_explain_code      = TRUE,
    verbose                  = FALSE
  )

  st_bad <- list(data_raw = list(df1, df2))

  # Expect the extraction error from execute node
  expect_error(
    suppressWarnings(bad_agent(st_bad)),
    "No R code could be extracted from the data wrangler function"
  )
})
