# Building Custom Agents

## Who This Is For

This vignette is written for beginners who want to build their first
graph-based agents in `LLMAgentR`.

If you already use built-in functions like
[`build_code_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_code_agent.md)
and
[`build_researcher_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_researcher_agent.md),
custom agents let you define your own workflow steps.

## What You Will Learn

- how state flows through a custom agent,
- how to define nodes and edges,
- how to route with conditions,
- how to generate a Mermaid graph for debugging and documentation.

## Quick Mental Model

A custom agent is a small state machine:

- `state`: a named list shared by all nodes.
- `node`: a function `function(state)` that returns a named list update,
  or `make_command(goto = "...", update = list(...))`.
- `edge`: a transition from one node to the next.
- `entry_point`: the first node to run.
- `__end__`: special terminal node.

You build this with
[`build_custom_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_custom_agent.md)
or
[`compile_graph()`](https://knowusuboaky.github.io/LLMAgentR/reference/compile_graph.md).

## First Agent: Linear Workflow

This first example has two steps:

1.  route to a handler,
2.  write an answer and stop.

``` r
library(LLMAgentR)

agent <- build_custom_agent(
  node_functions = list(
    start = function(state) {
      make_command("answer")
    },
    answer = function(state) {
      query <- if (is.null(state$query)) "" else state$query
      list(
        answer = paste("You asked:", query),
        handled = TRUE
      )
    }
  ),
  entry_point = "start",
  edges = list(
    c("answer", "__end__")
  ),
  default_state = list(handled = FALSE)
)

result <- agent(list(query = "How do I build a custom agent?"))
str(result)
```

What to notice:

- `start` uses `make_command("answer")` to jump directly.
- `answer` returns a normal named list to update `state`.
- edge `answer -> __end__` finishes execution.

## Router Pattern: Conditional Behavior

Most real workflows need routing. Here, one node sets `state$route`, and
conditional edges map each route label to a target node.

``` r
library(LLMAgentR)

router_agent <- build_custom_agent(
  node_functions = list(
    start = function(state) {
      make_command("classify")
    },
    classify = function(state) {
      q <- if (is.null(state$query)) "" else tolower(state$query)
      route <- if (grepl("weather", q)) "weather" else "general"
      list(route = route)
    },
    weather_handler = function(state) {
      list(answer = "Routing to weather workflow.")
    },
    general_handler = function(state) {
      list(answer = "Routing to general workflow.")
    }
  ),
  entry_point = "start",
  conditional_edges = list(
    list(
      from = "classify",
      condition = function(state) state$route,
      mapping = list(
        weather = "weather_handler",
        general = "general_handler"
      )
    )
  ),
  edges = list(
    c("weather_handler", "__end__"),
    c("general_handler", "__end__")
  )
)

router_agent(list(query = "weather in Accra"))
router_agent(list(query = "summarize this report"))
```

## Compile + Mermaid Diagram

Use
[`compile_graph()`](https://knowusuboaky.github.io/LLMAgentR/reference/compile_graph.md)
when you want both:

- a runnable function (`$run`), and
- graph artifacts (`$graph`, `$mermaid`).

``` r
library(LLMAgentR)

compiled <- compile_graph(
  node_functions = list(
    start = function(state) make_command("classify"),
    classify = function(state) {
      q <- if (is.null(state$query)) "" else tolower(state$query)
      list(route = if (grepl("weather", q)) "weather" else "general")
    },
    weather_handler = function(state) list(answer = "Weather path"),
    general_handler = function(state) list(answer = "General path")
  ),
  entry_point = "start",
  conditional_edges = list(
    list(
      from = "classify",
      condition = function(state) state$route,
      mapping = list(
        weather = "weather_handler",
        general = "general_handler"
      )
    )
  ),
  edges = list(
    c("weather_handler", "__end__"),
    c("general_handler", "__end__")
  ),
  subgraphs = list(
    Router = c("start", "classify"),
    Handlers = c("weather_handler", "general_handler")
  )
)

cat(compiled$mermaid)
out <- compiled$run(list(query = "weather tomorrow"))
str(out)

# Optional PNG render (requires Mermaid CLI `mmdc`)
# save_mermaid_png(compiled, file = "router_graph.png")
```

## Common Beginner Mistakes

- Node returns non-list output: return a named list or
  `make_command(...)`, not a bare string/number.
- `entry_point` typo: it must match one of the names in
  `node_functions`.
- Missing route mapping: if `condition(state)` returns a label not found
  in `mapping`, execution fails.
- No path to `__end__`: make sure every route eventually terminates.

## Build Multi-Agent Teams Next

When one node should coordinate multiple specialist workers, use
[`build_custom_multi_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_custom_multi_agent.md).

See: `vignettes/multi-agent-teams.Rmd` for the full beginner
walkthrough.
