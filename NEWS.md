# LLMAgentR 0.3.1 (Upcoming Release - February 2026)

## New Features

- **Custom Agent Builder API**  
  Added `build_custom_agent()` for creating user-defined graph/state-machine agents from custom node functions, static edges, and conditional edges.

- **Exported State-Graph Primitives**  
  Exported `StateGraph()`, `make_node()`, `make_edge()`, `make_command()`, and `interrupt()` to support custom workflow construction.

- **LangGraph-Style Mermaid Export**  
  Added `compile_graph()` and `as_mermaid()` for Mermaid visualization, including optional subgraph grouping and graph direction control.

- **PNG Graph Export Utility**  
  Added `save_mermaid_png()` to render Mermaid graph text (or compiled graph objects) into PNG files via Mermaid CLI (`mmdc`).

- **Reusable Code Agent with Builder Pattern**  
  The `build_code_agent()` function now supports a builder pattern. If `user_input` is omitted, the function returns a reusable agent (closure) that can handle multiple queries interactively. This allows for flexible integration in iterative coding workflows, similar to the existing `build_researcher_agent()`.

- **Reusable Interpreter Agent**  
  The `build_interpreter_agent()` function now also supports a builder pattern. When `code_output` is omitted, it returns a reusable interpreter function that can process multiple outputs (tables, model results, charts, etc.) on demand. This provides a consistent interface for explaining analysis results dynamically.

## Improvements

- **Dual Invocation Support**  
  Users can now call both `build_code_agent()` and `build_interpreter_agent()` in two ways:
  1. **One-shot mode** – Provide the input (e.g., `user_input`, `code_output`) for immediate interpretation or code generation.
  2. **Builder mode** – Omit the input to return a persistent agent function.

- **Enhanced Documentation**  
  The roxygen examples have been expanded to illustrate both usage patterns clearly for both agents, including structured return objects and typical use cases.

## Compatibility

- This update is fully backward-compatible. Existing calls to `build_code_agent()` and `build_interpreter_agent()` that use the one-shot pattern will continue to work without modification.

