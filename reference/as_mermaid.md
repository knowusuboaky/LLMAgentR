# Mermaid and Graph Compilation Helpers

`as_mermaid()` converts a compiled graph spec into Mermaid syntax.

`save_mermaid_png()` renders Mermaid text or compiled graph objects to
PNG using Mermaid CLI (`mmdc`).

`compile_graph()` is a convenience wrapper around
`build_custom_agent(..., output = "both")` for LangGraph-style compile
output.

## Usage

``` r
as_mermaid(
  x,
  direction = c("TD", "LR"),
  subgraphs = NULL,
  include_start_end = TRUE,
  style = TRUE
)

save_mermaid_png(
  x,
  file,
  mmdc = Sys.which("mmdc"),
  direction = c("TD", "LR"),
  subgraphs = NULL,
  include_start_end = TRUE,
  style = TRUE,
  width = NULL,
  height = NULL,
  scale = NULL,
  background = "white",
  theme = "default",
  quiet = TRUE
)

compile_graph(
  node_functions,
  entry_point,
  edges = list(),
  conditional_edges = list(),
  default_state = list(),
  checkpointer = NULL,
  direction = c("TD", "LR"),
  subgraphs = NULL,
  style = TRUE
)
```

## Arguments

- x:

  Graph spec list (from
  `build_custom_agent(..., output = "both")$graph`) or a compiled object
  returned by `build_custom_agent(..., output = "both")` or
  `compile_graph()`.

- direction:

  Mermaid direction: `"TD"` (top-down) or `"LR"` (left-right).

- subgraphs:

  Optional named list of subgraph groupings.

- include_start_end:

  Logical; include `__start__` and `__end__` nodes.

- style:

  Logical; include default LangGraph-like Mermaid styling.

- file:

  Output `.png` path.

- mmdc:

  Path to Mermaid CLI executable. Defaults to `Sys.which("mmdc")`.

- width:

  Optional diagram width passed to `mmdc`.

- height:

  Optional diagram height passed to `mmdc`.

- scale:

  Optional diagram scale passed to `mmdc`.

- background:

  Background color for Mermaid rendering.

- theme:

  Mermaid theme (for example `"default"`, `"neutral"`, `"dark"`).

- quiet:

  Logical; suppress Mermaid CLI output when `TRUE`.

- node_functions:

  Named list of node functions. Each function takes `state` and returns
  a named list or
  [`make_command()`](https://knowusuboaky.github.io/LLMAgentR/reference/state_graph_utils.md).

- entry_point:

  Name of the start node (must exist in `node_functions`).

- edges:

  Optional list of static edges. Each item can be:

  - a character vector of length 2: `c("from", "to")`, or

  - a list with `from` and `to`.

- conditional_edges:

  Optional list where each item contains:

  - `from` (or `node_name`),

  - `condition` (or `condition_fun`),

  - `mapping` (or `mapping_list`) as a named list of label -\> node.

- default_state:

  Optional named list merged into `state` for missing keys.

- checkpointer:

  Optional callback `function(state, current_node)` executed after each
  transition.

## Value

`as_mermaid()` returns Mermaid flowchart text.

`save_mermaid_png()` invisibly returns the output file path.

`compile_graph()` returns a list with `run`, `graph`, and `mermaid`.
