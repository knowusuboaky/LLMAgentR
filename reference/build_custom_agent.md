# Build a Custom Graph-Based Agent

Build a reusable agent by wiring user-defined node functions into a
state graph. This is the public extension point for creating custom
LLMAgentR workflows.

## Usage

``` r
build_custom_agent(
  node_functions,
  entry_point,
  edges = list(),
  conditional_edges = list(),
  default_state = list(),
  checkpointer = NULL,
  output = c("agent", "mermaid", "both"),
  direction = c("TD", "LR"),
  subgraphs = NULL,
  style = TRUE
)
```

## Arguments

- node_functions:

  Named list of node functions. Each function takes \`state\` and
  returns a named list or \[make_command()\].

- entry_point:

  Name of the start node (must exist in \`node_functions\`).

- edges:

  Optional list of static edges. Each item can be: - a character vector
  of length 2: \`c("from", "to")\`, or - a list with \`from\` and
  \`to\`.

- conditional_edges:

  Optional list where each item contains: - \`from\` (or
  \`node_name\`), - \`condition\` (or \`condition_fun\`), - \`mapping\`
  (or \`mapping_list\`) as a named list of label -\> node.

- default_state:

  Optional named list merged into \`state\` for missing keys.

- checkpointer:

  Optional callback \`function(state, current_node)\` executed after
  each transition.

- output:

  Output mode: - \`"agent"\` (default): return runnable agent
  function, - \`"mermaid"\`: return Mermaid text only, - \`"both"\`:
  return list with \`run\`, \`graph\`, and \`mermaid\`.

- direction:

  Mermaid direction used when \`output\` includes Mermaid.

- subgraphs:

  Optional named list of subgraph groups for Mermaid rendering.

- style:

  Logical; include default Mermaid class styling.

## Value

\- If \`output = "agent"\`: a function that accepts \`state\` and
returns final state. - If \`output = "mermaid"\`: Mermaid flowchart
text. - If \`output = "both"\`: list with \`run\`, \`graph\`, and
\`mermaid\`.

## Examples

``` r
if (FALSE) { # \dontrun{
custom <- build_custom_agent(
  node_functions = list(
    start = function(state) make_command("classify"),
    classify = function(state) {
      if (grepl("weather", state$query, ignore.case = TRUE)) {
        make_command("weather")
      } else {
        make_command("general")
      }
    },
    weather = function(state) list(answer = "Routing to weather handler"),
    general = function(state) list(answer = "Routing to general handler")
  ),
  entry_point = "start",
  edges = list(c("weather", "__end__"), c("general", "__end__")),
  output = "both",
  subgraphs = list(
    Router = c("start", "classify"),
    Handlers = c("weather", "general")
  )
)

cat(custom$mermaid)
custom$run(list(query = "weather in Accra"))
} # }
```
