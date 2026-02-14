# Build a Document Summarizer Agent

Creates an LLM-powered document summarization workflow that processes
PDF, DOCX, PPTX, TXT, or plain text input and returns structured
markdown summaries.

## Usage

``` r
build_doc_summarizer_agent(
  llm,
  summary_template = NULL,
  chunk_size = 4000,
  overlap = 200,
  verbose = TRUE,
  output = c("agent", "mermaid", "both"),
  direction = c("TD", "LR"),
  subgraphs = NULL,
  style = TRUE
)
```

## Arguments

- llm:

  A function that accepts a character prompt and returns an LLM
  response.

- summary_template:

  Optional custom summary template in markdown format.

- chunk_size:

  Maximum character length for document chunks (default: 4000).

- overlap:

  Character overlap between chunks (default: 200).

- verbose:

  Logical controlling progress messages (default: TRUE).

- output:

  Output type: "agent" (default), "mermaid" for diagram only, or "both".

- direction:

  Mermaid diagram direction: "TD" (top-down) or "LR" (left-right).

- subgraphs:

  Optional named list for grouping nodes in Mermaid diagram.

- style:

  Logical; apply default styling to Mermaid diagram (default: TRUE).

## Value

A function that accepts file paths or text input and returns:

- summary - The generated markdown summary

- metadata - Document metadata if available

- chunks - Number of processing chunks used

- success - Logical indicating success

## Examples

``` r
if (FALSE) { # \dontrun{
# Build document summarizer agent
summarizer_agent <- build_doc_summarizer_agent(
  llm = my_llm_wrapper,
  summary_template = NULL,
  chunk_size = 4000,
  overlap = 200,
  verbose = FALSE
)

# Summarize document
final_state <- summarizer_agent("https://github.com/knowusuboaky/LLMAgentR/raw/main/\
tests/testthat/test-data/scrum.docx")
} # }
```
