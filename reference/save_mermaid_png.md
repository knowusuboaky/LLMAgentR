# Save Mermaid Diagram as PNG

Render Mermaid text (or a compiled graph object) to a PNG file using
Mermaid CLI (\`mmdc\`).

## Usage

``` r
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
```

## Arguments

- x:

  Mermaid text, graph spec, or compiled object returned by
  \[build_custom_agent()\] with \`output = "both"\` or
  \[compile_graph()\].

- file:

  Output \`.png\` path.

- mmdc:

  Path to Mermaid CLI executable. Defaults to \`Sys.which("mmdc")\`.

- direction:

  Mermaid direction used when \`x\` is not plain Mermaid text.

- subgraphs:

  Optional named list of subgraph groupings.

- include_start_end:

  Logical; include \`\_\_start\_\_\` and \`\_\_end\_\_\` nodes.

- style:

  Logical; include default Mermaid class styling.

- width:

  Optional diagram width passed to \`mmdc\`.

- height:

  Optional diagram height passed to \`mmdc\`.

- scale:

  Optional diagram scale passed to \`mmdc\`.

- background:

  Background color for Mermaid rendering.

- theme:

  Mermaid theme (for example \`"default"\`, \`"neutral"\`, \`"dark"\`).

- quiet:

  Logical; suppress Mermaid CLI output when \`TRUE\`.

## Value

Invisibly returns the output file path.
