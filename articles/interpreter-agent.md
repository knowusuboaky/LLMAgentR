# Interpreter Agent

## What This Agent Does

[`build_interpreter_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_interpreter_agent.md)
turns technical outputs (tables, metrics, model summaries) into
plain-language interpretation.

## Step 1: Build a Reusable Interpreter

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

interpreter <- build_interpreter_agent(
  llm = my_llm_wrapper,
  verbose = FALSE
)
```

## Step 2: Interpret an Output

``` r
table_txt <- "
| Region | Sales | Profit |
| North  | 2000  | 300    |
| South  | 1500  | 250    |"

result <- interpreter(table_txt)
str(result)
```

## Notes for Beginners

- The closure pattern lets you reuse one configured agent.
- Main fields: `success`, `attempts`, and `interpretation`.
- Keep raw technical output in your records for traceability.
