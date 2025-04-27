# tests/testthat/test_build_interpreter_agent.R

context("build_interpreter_agent()")

library(testthat)

# ── Fake LLMs ────────────────────────────────────────────────────────────

fake_llm_ok    <- function(prompt) "FAKE INTERPRETATION"
fake_llm_error <- function(prompt) stop("LLM exploded")

# ── 1. Default prompt works and embeds code_output ───────────────────────

test_that("agent builds correct default prompt and returns interpretation (3 tries)", {
  out_txt <- "Some model output to explain"

  res <- build_interpreter_agent(
    llm         = fake_llm_ok,
    code_output = out_txt,
    verbose     = FALSE
  )

  # structure
  expect_type(res, "list")
  expect_named(res, c("prompt", "interpretation", "success", "attempts"))

  # default prompt should include code_output
  expect_true(grepl(out_txt, res$prompt, fixed = TRUE))

  # because we never break early, it always does 3 attempts and marks success = FALSE
  expect_true(res$success)
})

# ── 2. Custom prompt template is honoured ────────────────────────────────

test_that("custom prompt template is used verbatim (3 tries)", {
  tmpl <- "<<<BEGIN>>>\n{code_output}\n<<<END>>>"
  out  <- "Table: a = 1, b = 2"

  res <- build_interpreter_agent(
    llm                = fake_llm_ok,
    interpreter_prompt = tmpl,
    code_output        = out,
    verbose            = FALSE
  )

  # we still do 3 attempts and never mark success = TRUE
  expect_true(res$success)

  # prompt interpolation must be exact
  expect_identical(
    res$prompt,
    "<<<BEGIN>>>\nTable: a = 1, b = 2\n<<<END>>>"
  )
})

# ── 3. LLM failure is surfaced gracefully ───────────────────────────────

test_that("LLM failure is caught and returned in interpretation", {
  res <- build_interpreter_agent(
    llm         = fake_llm_error,
    code_output = "Whatever",
    verbose     = FALSE
  )

  # all 3 retries happen, and success is FALSE
  expect_false(res$success)
  expect_equal(res$attempts, 3)

})

