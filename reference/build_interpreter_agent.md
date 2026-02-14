# Build an Interpreter Agent

Constructs an LLM-powered agent that explains plots, tables, text, or
other outputs for both technical and non-technical audiences.

## Arguments

- llm:

  Function that takes `prompt` and returns an LLM response (may or may
  not accept `verbose`).

- interpreter_prompt:

  Optional template for the prompt (default supplied).

- code_output:

  The output to interpret (plot caption, table text, model summary,
  etc.). \*\*Default `NULL`\*\*.

- max_tries:

  Max LLM retry attempts (default `3`).

- backoff:

  Seconds between retries (default `2`).

- verbose:

  Logical; print progress (default `TRUE`).

## Value

- If `code_output` is `NULL`: a **function** (closure).

- Otherwise: a **list** with

  - prompt:

    The full prompt sent to the LLM.

  - interpretation:

    The LLM’s explanation (or error).

  - success:

    Logical; did it succeed?

  - attempts:

    Number of attempts made.

## Details

\*\*Two calling patterns\*\*

- \*\*Builder pattern\*\* – omit `code_output`; a reusable
  **interpreter-agent closure** is returned.

- \*\*One-shot pattern\*\* – provide `code_output`; the function runs
  immediately and returns the interpretation.

## Examples

``` r
if (FALSE) { # \dontrun{
## 1) Builder pattern --------------------------------------------
interp <- build_interpreter_agent(llm = my_llm_wrapper, verbose = FALSE)

table_txt <- "
| Region | Sales | Profit |
| North  | 2000  | 300    |
| South  | 1500  | 250    |"

res1 <- interp(table_txt)
res2 <- interp("R² = 0.87 for the fitted model …")

## 2) One-shot pattern -------------------------------------------
build_interpreter_agent(
  llm         = my_llm_wrapper,
  code_output = table_txt,
  verbose     = FALSE
)
} # }
```
