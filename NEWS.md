# LLMAgentR 0.3.0 (Upcoming Release – May 2025)

## New Features

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
