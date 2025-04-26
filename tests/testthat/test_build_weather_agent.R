# tests/testthat/test_build_weather_agent.R

# tests/testthat/test_build_weather_agent.R

context("build_weather_agent()")

library(testthat)

# ── Fake LLM ──────────────────────────────────────────────────────────────

fake_llm <- function(prompt) "IGNORED"

# ── 1. Missing API key is detected ───────────────────────────────────────

test_that("errors when API key is missing", {
  Sys.setenv(OPENWEATHERMAP_API_KEY = "")

  expect_error(
    build_weather_agent(
      llm            = fake_llm,
      location_query = "Nowhere",
      verbose        = FALSE
    ),
    "OPENWEATHERMAP_API_KEY not provided"
  )
})

