context("build_custom_agent()")

library(testthat)

test_that("build_custom_agent runs a linear workflow", {
  agent <- build_custom_agent(
    node_functions = list(
      start = function(state) make_command("step_one"),
      step_one = function(state) list(done = TRUE)
    ),
    entry_point = "start",
    edges = list(c("step_one", "__end__")),
    default_state = list(counter = 0)
  )

  out <- agent(list())

  expect_true(isTRUE(out$done))
  expect_equal(out$counter, 0)
  expect_equal(out$current_node, "__end__")
})

test_that("build_custom_agent supports conditional routing", {
  agent <- build_custom_agent(
    node_functions = list(
      route = function(state) list(),
      A = function(state) list(path = "A"),
      B = function(state) list(path = "B")
    ),
    entry_point = "route",
    conditional_edges = list(
      list(
        from = "route",
        condition = function(state) state$branch,
        mapping = list(a = "A", b = "B")
      )
    ),
    edges = list(c("A", "__end__"), c("B", "__end__"))
  )

  out_a <- agent(list(branch = "a"))
  out_b <- agent(list(branch = "b"))

  expect_equal(out_a$path, "A")
  expect_equal(out_b$path, "B")
})

test_that("build_custom_agent validates input contracts", {
  expect_error(
    build_custom_agent(
      node_functions = list(start = "not-a-function"),
      entry_point = "start"
    ),
    "not a function"
  )

  expect_error(
    build_custom_agent(
      node_functions = list(start = function(state) list()),
      entry_point = "missing"
    ),
    "entry_point"
  )
})

test_that("build_custom_agent can return Mermaid text", {
  mermaid <- build_custom_agent(
    node_functions = list(
      start = function(state) make_command("done"),
      done = function(state) list(ok = TRUE)
    ),
    entry_point = "start",
    edges = list(c("done", "__end__")),
    output = "mermaid"
  )

  expect_type(mermaid, "character")
  expect_match(mermaid, "flowchart TD")
  expect_match(mermaid, "__start__")
  expect_match(mermaid, "__end__")
})

test_that("build_custom_agent output='both' returns run, graph, mermaid", {
  compiled <- build_custom_agent(
    node_functions = list(
      start = function(state) make_command("route"),
      route = function(state) list(path = "ok")
    ),
    entry_point = "start",
    edges = list(c("route", "__end__")),
    output = "both",
    subgraphs = list(Router = c("start", "route"))
  )

  expect_true(is.list(compiled))
  expect_true(is.function(compiled$run))
  expect_true(is.list(compiled$graph))
  expect_type(compiled$mermaid, "character")
  expect_match(compiled$mermaid, "subgraph")

  out <- compiled$run(list())
  expect_equal(out$path, "ok")
})

test_that("compile_graph and as_mermaid produce LangGraph-style output", {
  compiled <- compile_graph(
    node_functions = list(
      start = function(state) list(),
      A = function(state) list(path = "A"),
      B = function(state) list(path = "B")
    ),
    entry_point = "start",
    conditional_edges = list(
      list(
        from = "start",
        condition = function(state) state$branch,
        mapping = list(a = "A", b = "B")
      )
    ),
    edges = list(c("A", "__end__"), c("B", "__end__")),
    direction = "LR"
  )

  expect_true(is.function(compiled$run))
  expect_true(is.list(compiled$graph))
  expect_match(compiled$mermaid, "flowchart LR")
  expect_match(compiled$mermaid, "--\\|a\\|-->")

  m2 <- as_mermaid(compiled, direction = "TD")
  expect_match(m2, "flowchart TD")
})

test_that("save_mermaid_png errors clearly when mmdc is missing", {
  expect_error(
    save_mermaid_png(
      x = "flowchart TD\nA --> B",
      file = tempfile(fileext = ".png"),
      mmdc = ""
    ),
    "Mermaid CLI"
  )
})

test_that("save_mermaid_png renders PNG when mmdc is available", {
  mmdc_path <- Sys.which("mmdc")
  if (!nzchar(mmdc_path)) {
    skip("mmdc is not available in PATH")
  }

  out_file <- tempfile(fileext = ".png")
  res <- tryCatch(
    {
      save_mermaid_png(
        x = "flowchart TD\nA --> B",
        file = out_file,
        mmdc = mmdc_path
      )
      TRUE
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("spawn EPERM|Failed to execute mmdc|mmdc failed to render PNG", msg, ignore.case = TRUE)) {
        skip("mmdc is present but rendering is blocked in this environment")
      }
      stop(e)
    }
  )
  expect_true(isTRUE(res))
  expect_true(file.exists(out_file))
})
