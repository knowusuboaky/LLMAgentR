# Build a Web Researcher Agent

Constructs an LLM-powered research agent that performs web searches (via
Tavily API) and generates structured responses based on search results.
The agent handles different question types (general knowledge,
comparisons, controversial topics) with appropriate response formats.

## Arguments

- llm:

  A function that accepts a character prompt and returns an LLM
  response. (It must accept \`prompt\` and optionally \`verbose\`.)

- tavily_search:

  Tavily API key as a string or NULL to use
  \`Sys.getenv("TAVILY_API_KEY")\`.

- system_prompt:

  Optional custom system prompt for the researcher agent.

- max_results:

  Number of web search results to retrieve per query (default: 5).

- max_tries:

  Maximum number of retry attempts for search or LLM call (default: 3).

- backoff:

  Initial wait time in seconds between retries (default: 2).

- verbose:

  Logical flag to control progress messages (default: TRUE).

## Value

A function that accepts a user query string and returns a list with:

- query - The original research query.

- prompt - The full prompt sent to the LLM.

- response - The generated LLM response.

- search_results - Raw search results (if any were found).

- success - Logical indicating if research succeeded (both search and
  LLM).

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize researcher agent
researcher_agent <- build_researcher_agent(
  llm = my_llm_wrapper,
  tavily_search = NULL,
  system_prompt = NULL,
  max_results = 5,
  max_tries = 3,
  backoff = 2,
  verbose = FALSE
)

# Perform research
result <- researcher_agent("Who is Messi?")
} # }
```
