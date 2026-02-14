# Build a Custom Multi-Agent Team (Supervisor Style)

Build a supervisor-routed multi-agent workflow, similar to LangGraph
team orchestration. The supervisor chooses the next worker by setting
`state$next` to one of the worker names or `FINISH`.

## Usage

``` r
build_custom_multi_agent(
  supervisor,
  workers,
  finish_token = "FINISH",
  max_turns = 10L,
  allow_repeat = FALSE,
  worker_error_policy = c("return_to_supervisor", "stop"),
  default_state = list(),
  checkpointer = NULL,
  output = c("agent", "mermaid", "both"),
  direction = c("TD", "LR"),
  subgraphs = NULL,
  style = TRUE
)
```

## Arguments

- supervisor:

  Function that accepts `state` and returns:

  - worker name (character scalar), or

  - list with [`next`](https://rdrr.io/r/base/Control.html), optionally
    additional updates.

- workers:

  Named list of workers. Each worker can be:

  - a function that accepts `state` and returns a named list of updates,
    or

  - a compiled custom-agent object with a callable `$run` function (for
    example from
    [`build_custom_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_custom_agent.md)
    with `output = "both"` or
    [`compile_graph()`](https://knowusuboaky.github.io/LLMAgentR/reference/as_mermaid.md)).

- finish_token:

  Character label used by the supervisor to terminate.

- max_turns:

  Maximum worker turns before forcing finish.

- allow_repeat:

  Logical; allow the same worker twice in a row.

- worker_error_policy:

  `"return_to_supervisor"` (default) or `"stop"`.

- default_state:

  Optional defaults merged into incoming `state`.

- checkpointer:

  Optional callback `function(state, current_node)`.

- output:

  Output mode:

  - `"agent"` (default): return runnable agent,

  - `"mermaid"`: return Mermaid text,

  - `"both"`: return list with `run`, `graph`, and `mermaid`.

- direction:

  Mermaid direction when `output` includes Mermaid.

- subgraphs:

  Optional Mermaid subgraph groups. If `NULL`, defaults to
  `list(Supervisor = "supervisor", Workers = names(workers))`.

- style:

  Logical; include default Mermaid class styling.

## Value

- If `output = "agent"`: runnable `function(state)`.

- If `output = "mermaid"`: Mermaid text.

- If `output = "both"`: list with `run`, `graph`, and `mermaid`.

## Examples

``` r
if (FALSE) { # \dontrun{
supervisor_fn <- function(state) {
  if (is.null(state$turn) || state$turn == 0) "Researcher" else "FINISH"
}

workers <- list(
  Researcher = function(state) {
    list(result = "Research complete")
  },
  Writer = function(state) {
    list(result = "Draft complete")
  }
)

team <- build_custom_multi_agent(
  supervisor = supervisor_fn,
  workers = workers,
  output = "both"
)

cat(team$mermaid)
team$run(list())
} # }
```
