# tests/testthat/test_state_graph.R
suppressPackageStartupMessages({
  library(testthat)
  library(LLMAgentR)    #  ⬅ adjust if your package is named differently
})

# helper (%||% from rlang for brevity)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ─────────────────────────────────────────────────────────────────────────────
test_that("make_node() wraps a function and carries a name", {
  fn   <- function(state) list()
  node <- make_node(fn, "starter")

  expect_type(node, "list")
  expect_identical(node$func, fn)
  expect_equal(node$name, "starter")
})

test_that("make_edge() stores the requested fields", {
  cond <- function(s) "A"
  edge <- make_edge("from", "to", condition = cond, label = "A")

  expect_equal(edge$from, "from")
  expect_equal(edge$to,   "to")
  expect_identical(edge$condition, cond)
  expect_equal(edge$label, "A")
})

test_that("linear graph executes nodes in sequence and returns final state", {
  gr <- StateGraph()
  gr$add_node("start",  function(state) make_command("mid"))
  gr$add_node("mid",    function(state) list(hit = TRUE, goto = "end"))
  gr$add_node("end",    function(state) list())
  gr$set_entry_point("start")

  agent <- gr$compile()
  res   <- agent(list())

  expect_true(res$hit)
  expect_equal(res$current_node, gr$END_NODE_NAME)
})

test_that("conditional edges branch according to label", {
  gr <- StateGraph()
  gr$add_node("A", function(state) list())          # decision node
  gr$add_node("B", function(state) list(flag = "b"))
  gr$add_node("C", function(state) list(flag = "c"))

  gr$add_conditional_edges(
    "A",
    condition_fun = function(s) s$choice,
    mapping_list  = list(toB = "B", toC = "C")
  )
  gr$set_entry_point("A")
  run <- gr$compile()

  res1 <- run(list(choice = "toB"))
  res2 <- run(list(choice = "toC"))

  expect_equal(res1$flag, "b")
  expect_equal(res2$flag, "c")
})

test_that("make_command() updates state and jumps to the requested node", {
  gr <- StateGraph()
  gr$add_node("first", function(state) make_command("second", list(msg = "hello")))
  gr$add_node("second", function(state) list())
  gr$set_entry_point("first")

  agent <- gr$compile()
  res   <- agent(list())

  expect_equal(res$msg, "hello")
  expect_equal(res$current_node, gr$END_NODE_NAME)
})

test_that("missing node raises a clear error", {
  gr <- StateGraph()
  gr$add_node("only", function(state) make_command("ghost"))
  gr$set_entry_point("only")
  agent <- gr$compile()

  expect_error(agent(list()), "Node 'ghost' not found")
})

test_that("interrupt() returns stubbed user input (no console wait)", {
  with_mocked_bindings(
    readline = function(prompt = "") "yes",
    .package = "base",
    {
      res <- interrupt("continue?")
      expect_equal(res, "yes")
    }
  )
})
