# Code Generation Agent

## What This Agent Does

[`build_code_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_code_agent.md)
helps you generate or debug R code from natural-language instructions.

## Step 1: Create an LLM Wrapper

``` r
my_llm_wrapper <- function(prompt, verbose = FALSE) {
  # Replace with your provider call (OpenAI, Groq, Anthropic, etc.)
  "LLM response placeholder"
}
```

## Step 2: Build a Reusable Agent

``` r
library(LLMAgentR)

coder <- build_code_agent(
  llm = my_llm_wrapper,
  max_tries = 3,
  backoff = 2,
  verbose = FALSE
)
```

## Step 3: Run a Task

``` r
result <- coder(
  "Write an R function that standardizes all numeric columns in a data frame."
)

str(result)
```

## Notes for Beginners

- The returned object is a normal R list.
- Main fields to inspect: `success`, `attempts`, `llm_response`.
- You can reuse `coder` for many prompts.
