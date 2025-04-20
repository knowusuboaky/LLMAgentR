# tests/testthat/test_build_forecasting_agent.R ------------------------------
# Silence every package‑startup message right away
suppressPackageStartupMessages({
  library(testthat)
  library(LLMAgentR)
  library(tibble)
  library(lubridate)
})

# ── helper stubs ─────────────────────────────────────────────────────────────
stub_install          <- function(...) invisible(NULL)
stub_requireNamespace <- function(pkg, ...) TRUE
stub_library          <- function(pkg, ...) TRUE

stub_forecast_ts <- function(...) {
  tibble(
    id      = rep("A", 3),
    date    = Sys.Date() + 1:3,
    value   = c(100, 110, 115),
    conf_lo = c( 95, 105, 110),
    conf_hi = c(105, 115, 120)
  )
}
stub_plot_forecast <- function(...) "plotly_stub"

make_fake_llm <- function(valid_json = TRUE) {
  good_json <- jsonlite::toJSON(
    list(
      params_value      = "sales",
      params_date       = "date",
      params_group      = "store_id",
      params_horizon    = 3,
      params_conf_level = 0.9
    ),
    auto_unbox = TRUE
  )
  bad_json <- "{ this is : not valid JSON }"

  function(prompt) {
    if (grepl("elite time-series forecasting assistant", prompt, fixed = TRUE)) {
      if (valid_json) good_json else bad_json
    } else {
      "--step list--"
    }
  }
}

sales_tbl <- tibble(
  store_id = c("A", "A", "A", "B", "B", "B"),
  date     = Sys.Date() - 5:0,
  sales    = c(90, 91, 94, 80, 82, 85)
)

# tests
test_that("forecasting agent succeeds with valid LLM JSON", {

  local_mocked_bindings(
    install.packages = stub_install,
    .package = "utils"
  )
  local_mocked_bindings(
    requireNamespace = stub_requireNamespace,
    library          = stub_library,
    .package = "base"
  )
  local_mocked_bindings(
    forecast_ts   = stub_forecast_ts,
    plot_forecast = stub_plot_forecast,
    .package = "LLMAgentR"
  )

  agent <- build_forecasting_agent(
    model       = make_fake_llm(TRUE),
    mode        = "light",
    line_width  = 1
  )

  st <- list(
    data_raw          = sales_tbl,
    user_instructions = "Forecast next 3 days of sales per store."
  )

  res <- agent(st)

  expect_true(res$execution_success)
  expect_s3_class(res$forecasting_data, "tbl_df")
  expect_equal(nrow(res$forecasting_data), 3)
  expect_equal(res$forecasting_result, "plotly_stub")
})

test_that("forecasting agent errors clearly with invalid LLM JSON", {

  local_mocked_bindings(
    install.packages = stub_install,
    .package = "utils"
  )
  local_mocked_bindings(
    requireNamespace = stub_requireNamespace,
    library          = stub_library,
    .package = "base"
  )
  local_mocked_bindings(
    forecast_ts   = stub_forecast_ts,
    plot_forecast = stub_plot_forecast,
    .package = "LLMAgentR"
  )

  bad_agent <- build_forecasting_agent(
    model      = make_fake_llm(FALSE),
    mode       = "light",
    line_width = 1
  )

  st_bad <- list(
    data_raw          = sales_tbl,
    user_instructions = "Forecast next 3 days of sales per store."
  )

  expect_error(
    suppressWarnings(bad_agent(st_bad)),   # <- warning now silenced
    "Could not parse valid forecasting parameters"
  )
})
