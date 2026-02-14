# Building Multi-Agent Teams

## Overview

[`build_custom_multi_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_custom_multi_agent.md)
creates supervisor-style teams where:

- a `supervisor` decides which worker runs next,
- each worker updates shared `state`,
- execution ends when the supervisor returns the finish token (default
  `"FINISH"`).

This pattern is useful when you want specialist agents (research,
writing, QA) to coordinate through a single router node.

## Minimal Team

``` r
library(LLMAgentR)

supervisor_fn <- function(state) {
  if (is.null(state$research_done) || !isTRUE(state$research_done)) {
    return("Researcher")
  }
  if (is.null(state$draft_done) || !isTRUE(state$draft_done)) {
    return("Writer")
  }
  "FINISH"
}

workers <- list(
  Researcher = function(state) {
    old_log <- if (is.null(state$log)) character(0) else state$log
    list(
      research_done = TRUE,
      findings = c("Customer churn is highest in month 2."),
      log = c(old_log, "Researcher completed evidence gathering.")
    )
  },
  Writer = function(state) {
    old_log <- if (is.null(state$log)) character(0) else state$log
    draft <- paste(
      "Executive summary:",
      paste(state$findings, collapse = " ")
    )
    list(
      draft_done = TRUE,
      draft = draft,
      log = c(old_log, "Writer produced first draft.")
    )
  }
)

team <- build_custom_multi_agent(
  supervisor = supervisor_fn,
  workers = workers,
  max_turns = 6L
)

result <- team(list(log = character(0)))
str(result)
```

## Workers As Custom Agents

Workers can also be compiled custom-agent objects (for example from
[`compile_graph()`](https://knowusuboaky.github.io/LLMAgentR/reference/as_mermaid.md)
or `build_custom_agent(..., output = "both")`).

``` r
research_worker <- compile_graph(
  node_functions = list(
    start = function(state) {
      old_log <- if (is.null(state$log)) character(0) else state$log
      list(
        research_done = TRUE,
        findings = c("Retention drops sharply after onboarding."),
        log = c(old_log, "Research worker graph completed.")
      )
    }
  ),
  entry_point = "start"
)

writer_worker <- compile_graph(
  node_functions = list(
    start = function(state) {
      old_log <- if (is.null(state$log)) character(0) else state$log
      list(
        draft_done = TRUE,
        draft = paste("Memo:", paste(state$findings, collapse = " ")),
        log = c(old_log, "Writer worker graph completed.")
      )
    }
  ),
  entry_point = "start"
)

team_with_compiled_workers <- build_custom_multi_agent(
  supervisor = supervisor_fn,
  workers = list(
    Researcher = research_worker, # uses research_worker$run internally
    Writer = writer_worker
  ),
  max_turns = 6L
)

result2 <- team_with_compiled_workers(list(log = character(0)))
str(result2)
```

## Mermaid Graph Output

Request `output = "both"` to get:

- `run`: the runnable team function,
- `graph`: graph spec object,
- `mermaid`: Mermaid diagram text.

``` r
compiled_team <- build_custom_multi_agent(
  supervisor = supervisor_fn,
  workers = workers,
  output = "both",
  direction = "LR"
)

cat(compiled_team$mermaid)

# Optional: render PNG (requires Mermaid CLI `mmdc`)
# save_mermaid_png(compiled_team, file = "multi_agent_team.png")
```

## Advanced Controls

You can control team behavior with:

- `worker_error_policy = "return_to_supervisor"`: worker errors are
  collected in `state$errors` and execution continues.
- `worker_error_policy = "stop"`: stop immediately on worker error.
- `allow_repeat = FALSE`: prevent the same worker from running twice in
  a row when alternatives exist.
- `max_turns`: hard cap on worker turns.

``` r
resilient_team <- build_custom_multi_agent(
  supervisor = supervisor_fn,
  workers = workers,
  worker_error_policy = "return_to_supervisor",
  allow_repeat = FALSE,
  max_turns = 8L
)
```
