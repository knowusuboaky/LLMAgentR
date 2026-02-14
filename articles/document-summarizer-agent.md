# Document Summarizer Agent

## What This Agent Does

[`build_doc_summarizer_agent()`](https://knowusuboaky.github.io/LLMAgentR/reference/build_doc_summarizer_agent.md)
summarizes documents (PDF, DOCX, PPTX, TXT, or plain text) into
structured output.

## Workflow Diagram

![](../document-summarizer-agent-workflow.png)

## Step 1: Build the Summarizer

``` r
library(LLMAgentR)

my_llm_wrapper <- function(prompt, verbose = FALSE) "LLM response placeholder"

summarizer <- build_doc_summarizer_agent(
  llm = my_llm_wrapper,
  chunk_size = 4000,
  overlap = 200,
  verbose = FALSE
)
```

## Step 2: Summarize Input

``` r
result <- summarizer("This is a short policy document about customer support SLAs.")
str(result)
```

## Notes for Beginners

- Start with plain text to validate your setup.
- Then switch to file paths or URLs.
- Tune `chunk_size` and `overlap` for large documents.
