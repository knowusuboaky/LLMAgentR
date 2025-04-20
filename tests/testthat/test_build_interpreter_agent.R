# tests/testthat/test_build_interpreter_agent.R

context("build_interpreter_agent()")

library(testthat)

# ── Fake LLMs ────────────────────────────────────────────────────────────

fake_llm_ok    <- function(prompt) "FAKE INTERPRETATION"
fake_llm_error <- function(prompt) stop("LLM exploded")

# ── 1. Default prompt works and embeds code_output ───────────────────────

test_that("agent builds correct default prompt and returns interpretation", {
  out_txt <- "Some model output to explain"

  res <- build_interpreter_agent(
    llm         = fake_llm_ok,
    code_output = out_txt
  )

  expect_type(res, "list")
  expect_named(res, c("prompt", "interpretation"))
  expect_identical(res$interpretation, "FAKE INTERPRETATION")
  expect_true(grepl(out_txt, res$prompt, fixed = TRUE))  # code_output inserted
})

# ── 2. Custom prompt template is honoured ────────────────────────────────

test_that("custom prompt template is used verbatim (with interpolation)", {
  tmpl <- "<<<BEGIN>>>\n{code_output}\n<<<END>>>"
  out  <- "Table: a = 1, b = 2"

  res <- build_interpreter_agent(
    llm               = fake_llm_ok,
    interpreter_prompt = tmpl,
    code_output        = out
  )

  expect_identical(res$interpretation, "FAKE INTERPRETATION")
  expect_identical(res$prompt, "<<<BEGIN>>>\nTable: a = 1, b = 2\n<<<END>>>")
})

# ── 3. LLM failure is surfaced gracefully ───────────────────────────────

test_that("LLM failure is caught and returned in interpretation", {
  res <- build_interpreter_agent(
    llm         = fake_llm_error,
    code_output = "Whatever"
  )

  expect_true(grepl("LLM call failed", res$interpretation, fixed = TRUE))
})
