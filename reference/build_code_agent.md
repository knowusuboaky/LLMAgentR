# Build an R Code-Generation Agent

Constructs an LLM-powered agent for generating, debugging, explaining,
or optimizing R code. \*\*Two calling patterns are supported\*\*:

- \*\*Builder pattern\*\* – omit \`user_input\`. The function returns a
  reusable **coder-agent closure**. Call that closure with different
  queries whenever you need code help.

- \*\*One-shot pattern\*\* – provide \`user_input\`. The function
  executes immediately and returns the result once.

## Arguments

- llm:

  A function that accepts a character \`prompt\` and returns an LLM
  response (optionally accepts \`verbose\`).

- system_prompt:

  Optional system-level instructions that override the built-in default
  prompt.

- user_input:

  The coding task/query (e.g., \`"Write function to filter NAs"\`).
  \*\*Default \`NULL\`\*\* – omit to obtain a reusable agent.

- max_tries:

  Maximum LLM retry attempts (default \`3\`).

- backoff:

  Seconds to wait between retries (default \`2\`).

- verbose:

  Logical flag to show progress messages (default \`TRUE\`).

## Value

- If \`user_input\` is \`NULL\`: a **function** (the coder-agent
  closure).

- Otherwise: a **list** with the fields

  - input:

    The original user query.

  - llm_response:

    The LLM output (or error message).

  - system_prompt:

    Prompt actually sent.

  - success:

    Logical; did the call succeed?

  - attempts:

    Number of attempts made.

## Details

The agent automatically retries failed LLM calls (with exponential
back-off) and always returns a structured result.

## Examples

``` r
if (FALSE) { # \dontrun{
## ------------------------------------------------------------------
## 1)  Builder pattern – create a reusable coder agent
## ------------------------------------------------------------------
coder <- build_code_agent(
  llm       = my_llm_wrapper,   # your own wrapper around the LLM API
  max_tries = 3,
  backoff   = 2,
  verbose   = FALSE
)

# Use the agent multiple times
res1 <- coder("Write an R function that z-score–standardises all numeric columns.")
res2 <- coder("Explain what `%>%` does in tidyverse pipelines.")

## ------------------------------------------------------------------
## 2)  One-shot pattern – run a single request immediately
## ------------------------------------------------------------------
one_shot <- build_code_agent(
  llm        = my_llm_wrapper,
  user_input = "Create a ggplot2 bar chart of mpg by cyl in mtcars.",
  max_tries  = 3,
  backoff    = 2,
  verbose    = FALSE
)
} # }
```
