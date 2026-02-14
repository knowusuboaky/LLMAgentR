# Changelog

## LLMAgentR 0.3.1 (Upcoming Release - February 2026)

### New Features

- **Custom Agent Builder API**  
  Added
  [`build_custom_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_custom_agent.md)
  for creating user-defined graph/state-machine agents from custom node
  functions, static edges, and conditional edges.

- **Exported State-Graph Primitives**  
  Exported
  [`StateGraph()`](https://knowusuboaky.github.io/LLMAgentR/reference/state_graph_utils.md),
  [`make_node()`](https://knowusuboaky.github.io/LLMAgentR/reference/state_graph_utils.md),
  [`make_edge()`](https://knowusuboaky.github.io/LLMAgentR/reference/state_graph_utils.md),
  [`make_command()`](https://knowusuboaky.github.io/LLMAgentR/reference/state_graph_utils.md),
  and
  [`interrupt()`](https://knowusuboaky.github.io/LLMAgentR/reference/state_graph_utils.md)
  to support custom workflow construction.

- **LangGraph-Style Mermaid Export**  
  Added
  [`compile_graph()`](https://knowusuboaky.github.io/LLMAgentR/reference/compile_graph.md)
  and
  [`as_mermaid()`](https://knowusuboaky.github.io/LLMAgentR/reference/as_mermaid.md)
  for Mermaid visualization, including optional subgraph grouping and
  graph direction control.

- **PNG Graph Export Utility**  
  Added
  [`save_mermaid_png()`](https://knowusuboaky.github.io/LLMAgentR/reference/save_mermaid_png.md)
  to render Mermaid graph text (or compiled graph objects) into PNG
  files via Mermaid CLI (`mmdc`).

- **Reusable Code Agent with Builder Pattern**  
  The
  [`build_code_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_code_agent.md)
  function now supports a builder pattern. If `user_input` is omitted,
  the function returns a reusable agent (closure) that can handle
  multiple queries interactively. This allows for flexible integration
  in iterative coding workflows, similar to the existing
  [`build_researcher_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_researcher_agent.md).

- **Reusable Interpreter Agent**  
  The
  [`build_interpreter_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_interpreter_agent.md)
  function now also supports a builder pattern. When `code_output` is
  omitted, it returns a reusable interpreter function that can process
  multiple outputs (tables, model results, charts, etc.) on demand. This
  provides a consistent interface for explaining analysis results
  dynamically.

### Improvements

- **Dual Invocation Support**  
  Users can now call both
  [`build_code_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_code_agent.md)
  and
  [`build_interpreter_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_interpreter_agent.md)
  in two ways:
  1.  **One-shot mode** – Provide the input (e.g., `user_input`,
      `code_output`) for immediate interpretation or code generation.
  2.  **Builder mode** – Omit the input to return a persistent agent
      function.
- **Enhanced Documentation**  
  The roxygen examples have been expanded to illustrate both usage
  patterns clearly for both agents, including structured return objects
  and typical use cases.

### Compatibility

- This update is fully backward-compatible. Existing calls to
  [`build_code_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_code_agent.md)
  and
  [`build_interpreter_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_interpreter_agent.md)
  that use the one-shot pattern will continue to work without
  modification.
