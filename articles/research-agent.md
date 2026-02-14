# Research Agent

## What This Agent Does

[`build_researcher_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_researcher_agent.md)
performs web search and asks an LLM to produce a research-style answer.

## Workflow Diagram

![](../research-agent-workflow.png)

## Step 1: Build the Agent

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

researcher <- build_researcher_agent(
  llm = my_llm_wrapper,
  tavily_search = NULL, # uses Sys.getenv("TAVILY_API_KEY")
  max_results = 5,
  max_tries = 3,
  backoff = 2,
  verbose = FALSE
)
```

## Step 2: Run a Query

``` r
result <- researcher("What are recent trends in open-source LLM tooling?")
str(result)
```

## Notes for Beginners

- Set your Tavily key first: `Sys.setenv(TAVILY_API_KEY = "...")`.
- Inspect `search_results` in the output for source sanity checks.
- Use this for research summaries, not legal/medical final advice.
