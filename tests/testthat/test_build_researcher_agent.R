# tests/testthat/test_build_researcher_agent.R

context("build_researcher_agent()")

library(testthat)

# ── Fake LLMs ────────────────────────────────────────────────────────────

fake_llm_ok    <- function(prompt) "FAKE LLM RESPONSE"
fake_llm_error <- function(prompt) stop("LLM failed")

# ── 1. Even if search fails or is skipped, we still get the LLM output ────

test_that("agent returns LLM response (and NULL search_results) on fake_llm_ok", {
  # provide a dummy API key so constructor doesn't error
  Sys.setenv(TAVILY_API_KEY = "DUMMY_KEY")

  agent <- build_researcher_agent(
    llm     = fake_llm_ok,
    verbose = FALSE
  )

  res <- agent("My test query")

  # structure: five expected fields
  expect_type(res, "list")
  expect_named(res, c("query", "prompt", "response", "search_results", "success"))

  # core fields
  expect_identical(res$query, "My test query")

  # search_results always NULL
  expect_null(res$search_results)

  # success should be FALSE (search always fails/skipped)
  expect_false(res$success)
})

# ── 2. LLM errors are caught and surfaced in response ────────────────────

test_that("agent surfaces LLM errors in the response field", {
  Sys.setenv(TAVILY_API_KEY = "DUMMY_KEY")

  agent <- build_researcher_agent(
    llm     = fake_llm_error,
    verbose = FALSE
  )

  # suppress any retry warnings
  res <- suppressWarnings(agent("Another query"))

  # structure: five expected fields
  expect_type(res, "list")
  expect_named(res, c("query", "prompt", "response", "search_results", "success"))

  # core fields
  expect_identical(res$query, "Another query")
  expect_null(res$search_results)

  # success remains FALSE
  expect_false(res$success)
})
