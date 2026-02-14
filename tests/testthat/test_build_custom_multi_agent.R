context("build_custom_multi_agent()")

library(testthat)

`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("build_custom_multi_agent runs supervisor-worker loop", {
  supervisor_fn <- function(state) {
    if ((state$turn %||% 0L) == 0L) "Analyst"
    else if ((state$turn %||% 0L) == 1L) "Writer"
    else "FINISH"
  }

  workers <- list(
    Analyst = function(state) {
      list(trace = c(state$trace %||% character(0), "Analyst"))
    },
    Writer = function(state) {
      list(trace = c(state$trace %||% character(0), "Writer"))
    }
  )

  team <- build_custom_multi_agent(
    supervisor = supervisor_fn,
    workers = workers,
    output = "agent"
  )

  out <- team(list(trace = character(0)))
  expect_equal(out$trace, c("Analyst", "Writer"))
  expect_equal(out$last_worker, "Writer")
  expect_equal(out$turn, 2)
  expect_equal(out$current_node, "__end__")
})

test_that("build_custom_multi_agent can return mermaid artifacts", {
  supervisor_fn <- function(state) "FINISH"
  workers <- list(WorkerA = function(state) list())

  compiled <- build_custom_multi_agent(
    supervisor = supervisor_fn,
    workers = workers,
    output = "both"
  )

  expect_true(is.function(compiled$run))
  expect_true(is.list(compiled$graph))
  expect_type(compiled$mermaid, "character")
  expect_match(compiled$mermaid, "supervisor")
  expect_match(compiled$mermaid, "WorkerA")
})

test_that("build_custom_multi_agent applies non-repeat guard", {
  supervisor_fn <- function(state) {
    if ((state$turn %||% 0L) < 2L) "A" else "FINISH"
  }

  workers <- list(
    A = function(state) list(trace = c(state$trace %||% character(0), "A")),
    B = function(state) list(trace = c(state$trace %||% character(0), "B"))
  )

  team <- build_custom_multi_agent(
    supervisor = supervisor_fn,
    workers = workers,
    allow_repeat = FALSE
  )

  out <- team(list(trace = character(0)))
  expect_equal(out$trace, c("A", "B"))
})

test_that("build_custom_multi_agent captures worker errors when configured", {
  supervisor_fn <- function(state) {
    if (length(state$errors %||% list()) > 0) "FINISH" else "Failer"
  }

  workers <- list(
    Failer = function(state) stop("worker exploded")
  )

  team <- build_custom_multi_agent(
    supervisor = supervisor_fn,
    workers = workers,
    worker_error_policy = "return_to_supervisor"
  )

  out <- team(list())
  expect_true(length(out$errors) >= 1)
  expect_match(out$last_error, "worker exploded")
})

test_that("build_custom_multi_agent accepts compiled worker objects with $run", {
  supervisor_fn <- function(state) {
    if (isTRUE(state$sub_done)) "FINISH" else "Researcher"
  }

  compiled_worker <- compile_graph(
    node_functions = list(
      start = function(state) {
        list(
          sub_done = TRUE,
          trace = c(state$trace %||% character(0), "Researcher")
        )
      }
    ),
    entry_point = "start"
  )

  team <- build_custom_multi_agent(
    supervisor = supervisor_fn,
    workers = list(Researcher = compiled_worker)
  )

  out <- team(list(trace = character(0)))
  expect_true(isTRUE(out$sub_done))
  expect_equal(out$trace, "Researcher")
  expect_equal(out$turn, 1L)
})

test_that("build_custom_multi_agent validates required contracts", {
  expect_error(
    build_custom_multi_agent(
      supervisor = "not-fn",
      workers = list(A = function(state) list())
    ),
    "supervisor"
  )

  expect_error(
    build_custom_multi_agent(
      supervisor = function(state) "FINISH",
      workers = list(supervisor = function(state) list())
    ),
    "cannot contain a node named 'supervisor'"
  )

  expect_error(
    build_custom_multi_agent(
      supervisor = function(state) "FINISH",
      workers = list(A = list(run = "not-a-function"))
    ),
    "compiled agent object"
  )
})
