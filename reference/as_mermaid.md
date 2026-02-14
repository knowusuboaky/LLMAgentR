# Convert a Custom Graph Spec to Mermaid

Convert a Custom Graph Spec to Mermaid

## Usage

``` r
as_mermaid(
  x,
  direction = c("TD", "LR"),
  subgraphs = NULL,
  include_start_end = TRUE,
  style = TRUE
)
```

## Arguments

- x:

  Graph spec list (from \`build_custom_agent(..., output =
  "both")\$graph\`) or a compiled object returned by
  \`build_custom_agent(..., output = "both")\` or \[compile_graph()\].

- direction:

  Mermaid direction: \`"TD"\` (top-down) or \`"LR"\` (left-right).

- subgraphs:

  Optional named list of subgraph groupings.

- include_start_end:

  Logical; include \`\_\_start\_\_\` and \`\_\_end\_\_\` nodes.

- style:

  Logical; include default LangGraph-like Mermaid styling.

## Value

Mermaid flowchart text.
