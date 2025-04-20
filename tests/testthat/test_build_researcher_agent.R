# tests/testthat/test_build_researcher_agent.R

context("build_researcher_agent()")

library(testthat)
library(jsonlite)

# ── Helpers ──────────────────────────────────────────────────────────────

make_tavily_payload <- function(n = 2) {
  list(
    results = lapply(seq_len(n), function(i) {
      list(
        title   = sprintf("News %d", i),
        content = sprintf("Content line %d about Messi.", i),
        url     = sprintf("https://example.com/%d", i)
      )
    })
  )
}

fake_httr_response <- function(status_code = 200L, payload = list()) {
  structure(
    list(
      status_code = status_code,
      url         = "https://api.tavily.com/search",
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

fake_llm_ok <- function(prompt) "FAKE LLM SUMMARY"
fake_llm_error <- function(prompt) stop("LLM failed")

# ── 1. Happy path with search results ────────────────────────────────────

test_that("researcher agent returns search results and LLM summary", {
  Sys.setenv(TAVILY_API_KEY = "dummy_key")

  with_mocked_bindings(
    POST = function(url, encode, body, ...) {
      fake_httr_response(200L, make_tavily_payload())
    },
    .package = "httr",
    {
      researcher <- build_researcher_agent(
        llm          = fake_llm_ok,
        max_results  = 2         # keep payload small
      )

      res <- researcher("Latest Messi news")
      expect_type(res, "list")
      expect_identical(res$response, "FAKE LLM SUMMARY")
      expect_length(res$search_results$results, 2)
      expect_match(res$prompt, "News 1")            # prompt contains search text
    }
  )
})

# ── 2. No API key → search skipped, agent still replies ──────────────────

test_that("agent works with no Tavily key (skips search)", {
  Sys.setenv(TAVILY_API_KEY = "")

  researcher <- build_researcher_agent(
    llm = fake_llm_ok,
    max_results = 3
  )

  res <- suppressWarnings(researcher("Describe R language"))
  expect_null(res$search_results)                   # search skipped
  expect_match(res$prompt, "Research Query")        # prompt built
  expect_identical(res$response, "FAKE LLM SUMMARY")
})

# ── 3. Tavily API HTTP error handled gracefully ──────────────────────────

test_that("agent warns and continues when Tavily API errors", {
  Sys.setenv(TAVILY_API_KEY = "dummy_key")

  with_mocked_bindings(
    POST = function(url, encode, body, ...) {
      fake_httr_response(500L, list(error = "server down"))
    },
    .package = "httr",
    {
      researcher <- build_researcher_agent(
        llm = fake_llm_ok
      )

      expect_warning(
        res <- researcher("Any topic"),
        "Tavily API error"
      )

      expect_null(res$search_results)               # no results on failure
      expect_identical(res$response, "FAKE LLM SUMMARY")
    }
  )
})

# ── 4. LLM failure bubbles into response field ───────────────────────────

test_that("LLM failure is surfaced in response text", {
  Sys.setenv(TAVILY_API_KEY = "dummy_key")

  with_mocked_bindings(
    POST = function(url, encode, body, ...) {
      fake_httr_response(200L, make_tavily_payload(1))
    },
    .package = "httr",
    {
      researcher <- build_researcher_agent(
        llm = fake_llm_error
      )

      res <- researcher("Topic")
      expect_match(res$response, "LLM call failed")
    }
  )
})
