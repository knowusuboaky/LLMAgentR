# tests/testthat/test_build_weather_agent.R

context("build_weather_agent()")

library(testthat)
library(jsonlite)

# ── Helpers ──────────────────────────────────────────────────────────────

make_weather_payload <- function(
    city = "Paris",
    country = "FR",
    description = "clear sky",
    temp = 20.5,
    humidity = 55,
    speed = 3.5,
    pressure = 1013
) {
  list(
    name    = city,
    sys     = list(country = country),
    weather = list(list(description = description)),
    main    = list(temp = temp, humidity = humidity, pressure = pressure),
    wind    = list(speed = speed)
  )
}

fake_response <- function(status_code = 200L, payload = list()) {
  structure(
    list(
      status_code = status_code,
      url         = "https://api.openweathermap.org/data/2.5/weather",
      headers     = c("Content-Type" = "application/json"),
      all_headers = list(list(
        status  = status_code,
        version = "HTTP/1.1",
        headers = c("Content-Type" = "application/json")
      )),
      content     = charToRaw(toJSON(payload, auto_unbox = TRUE)),
      date        = Sys.time()
    ),
    class = "response"
  )
}

fake_llm_ok    <- function(prompt) "FAKE LLM WEATHER REPORT"
fake_llm_error <- function(prompt) stop("LLM blew up")

# ── 1. Happy path & cache ────────────────────────────────────────────────

test_that("build_weather_agent returns expected structure and caches data", {
  Sys.setenv(OPENWEATHERMAP_API_KEY = "dummy_key")

  # clear any existing cache
  if (exists("weather_cache", envir = .GlobalEnv)) {
    rm(list = ls(envir = weather_cache), envir = weather_cache)
  }

  with_mocked_bindings(
    GET = function(url, query, ...) fake_response(200L, make_weather_payload()),
    .package = "httr",
    {
      res1 <- build_weather_agent(
        llm            = fake_llm_ok,
        location_query = "Paris, FR",
        units          = "metric",
        n_tries        = 1,
        backoff        = 0,
        cache_ttl      = 30
      )

      expect_true(res1$success)
      expect_identical(res1$llm_response, "FAKE LLM WEATHER REPORT")

      # second call → guaranteed cache hit
      res2 <- build_weather_agent(
        llm            = fake_llm_ok,
        location_query = "Paris, FR",
        units          = "metric",
        n_tries        = 1,
        backoff        = 0,
        cache_ttl      = 30
      )

      expect_true(res2$cache_hit)
    }
  )
})

# ── 2. Weather API error is propagated ───────────────────────────────────

test_that("build_weather_agent stops on weather API error", {
  Sys.setenv(OPENWEATHERMAP_API_KEY = "dummy_key")

  with_mocked_bindings(
    GET = function(url, query, ...)
      fake_response(404L, list(cod = "404", message = "city not found")),
    .package = "httr",
    {
      expect_error(
        build_weather_agent(
          llm            = fake_llm_ok,
          location_query = "NowhereVille",
          units          = "metric",
          n_tries        = 1,
          backoff        = 0
        ),
        "Weather data error"
      )
    }
  )
})

# ── 3. Missing API key is detected ───────────────────────────────────────

test_that("build_weather_agent errors when API key is missing", {
  Sys.setenv(OPENWEATHERMAP_API_KEY = "")

  expect_error(
    build_weather_agent(
      llm            = fake_llm_ok,
      location_query = "Paris, FR",
      units          = "metric"
    ),
    "OPENWEATHERMAP_API_KEY not set"
  )
})

# ── 4. LLM failure handled gracefully ────────────────────────────────────

test_that("build_weather_agent handles LLM failure gracefully", {
  Sys.setenv(OPENWEATHERMAP_API_KEY = "dummy_key")

  with_mocked_bindings(
    GET = function(url, query, ...) fake_response(200L, make_weather_payload()),
    .package = "httr",
    {
      res <- suppressWarnings(
        build_weather_agent(
          llm            = fake_llm_error,
          location_query = "Paris, FR",
          units          = "metric",
          n_tries        = 1,
          backoff        = 0
        )
      )

      expect_false(res$success)
      expect_match(res$llm_response, "Could not generate weather report")
    }
  )
})
