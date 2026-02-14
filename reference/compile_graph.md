# Compile a Custom Agent Graph (LangGraph-Style Output)

Convenience wrapper around \[build_custom_agent()\] that returns both
runnable agent and Mermaid graph artifacts.

## Usage

``` r
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

- direction:

  Mermaid direction used when \`output\` includes Mermaid.

- subgraphs:

  Optional named list of subgraph groups for Mermaid rendering.

- style:

  Logical; include default Mermaid class styling.

## Value

A list with \`run\`, \`graph\`, and \`mermaid\`.
