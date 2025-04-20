# ------------------------------------------------------------------------------
#' Build a Web Researcher Agent
#'
#' This function builds an LLM-powered researcher agent that performs web
#' searches (via the Tavily API) and uses the LLM to summarize, compare,
#' or analyze the information depending on the question type.
#'
#' @name build_researcher_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#' @param tavily_search API key string or NULL to use \code{Sys.getenv("TAVILY_API_KEY")}.
#' @param system_prompt Optional custom system prompt.
#' @param max_results Number of search results to return (default 5).
#'
#' @return A function that accepts a user query and returns a response list with the prompt, result, and sources.
#' @examples
#' \dontrun{
#' agent <- build_researcher_agent(llm = call_llm)
#' result <- agent("What are the latest trends in AI education#")
#' cat(result$response)
#' }
#' @export
NULL


build_researcher_agent <- function(
    llm,
    tavily_search = NULL,
    system_prompt = NULL,
    max_results = 5
) {
  cat("=== STARTING RESEARCHER AGENT ===\n")

  # 1. Ensure required packages (Suggests:) are available
  httr <- get_suggested("httr")
  jsonlite <- get_suggested("jsonlite")

  if (is.null(system_prompt)) {
    system_prompt <- "
You are a highly skilled web researcher tasked with providing accurate and reliable information tailored to the user's needs.
For data requests, you will gather relevant data and return it structured in a clear format.

For all other types of queries, follow the specified formats and ensure accuracy.

**Instructions**:

1. **Understand the Question**:
- Identify whether the question is:
  - A **general knowledge question** (e.g., \"Who is Messi#\")
  - A **data request** (e.g., historical stock prices, GDP, statistics)
  - A **comparative analysis** (e.g., \"Messi vs. Ronaldo\")
  - A **controversial or multifaceted question** (e.g., \"Is AI dangerous#\")
- Tailor your response based on the type of question and follow the appropriate structure below.

2. **Gather and Validate Information**:
- Use **trusted** and **up-to-date** sources:
  - For general knowledge, prioritize authoritative sources (e.g., Wikipedia, Britannica, biographies, news outlets)
  - For **specific data** requests, gather structured data from reliable sources
  - For **controversial topics**, ensure multiple perspectives are covered
- Always **verify** the reliability and accuracy of the sources
- **Cite** all sources clearly with links

3. **Provide a Well-Structured Response**:
- **General Knowledge**: Structure with summary, key facts, context, and sources
- **Data Requests**: Provide data in a structured format with source
- **Comparison**: Present side-by-side comparison with statistics and key points
- **Controversial/Multifaceted Questions**: Present balanced viewpoints with pros/cons
- Include links to supporting multimedia when applicable

**Response Formats**:

1. **General Knowledge Questions**:
```markdown
**Summary**: [Concise summary]
**Key Facts**: [Bullet points of important facts]
**Context**: [Background/significance]
**Sources**: [List of credible sources with URLs]
```

2. **Comparison Questions**:
```markdown
**Comparison**: [Side-by-side comparison]
- **[Entity 1]**: [Key stats/achievements]
- **[Entity 2]**: [Key stats/achievements]
**Expert Opinions**: [Summary of expert views]
**Sources**: [List of sources]
```

3. **Controversial Topic**:
```markdown
**Balanced View**:
- **Pro**: [Arguments supporting]
- **Con**: [Arguments opposing]
**Sources**: [List of balanced sources]
```

4. **Future Trends and Predictions**:
```markdown
**Trends**: [Summary of expert predictions]
**Sources**: [List of research reports]
```

Ensure responses are well-structured and provide all necessary information."
  }

  function(query) {
    # First perform web search
    search_results <- perform_tavily_search(
      query = query,
      tavily_search = tavily_search,
      max_results = max_results
    )

    # Combine search results with the query
    if (!is.null(search_results)) {
      web_content <- paste(
        sapply(search_results$results, function(r) {
          paste(r$title, r$content, r$url, sep = "\n")
        }),
        collapse = "\n\n"
      )

      full_prompt <- sprintf(
        "%s\n\nResearch Query:\n%s\n\nWeb Search Results:\n%s",
        system_prompt,
        query,
        web_content
      )
    } else {
      full_prompt <- sprintf(
        "%s\n\nResearch Query:\n%s",
        system_prompt,
        query
      )
    }

    # Call the LLM with retries
    response <- tryCatch({
      llm(prompt = full_prompt)
    }, error = function(e) {
      paste("LLM call failed:", e$message)
    })

    list(
      query = query,
      prompt = full_prompt,
      response = response,
      search_results = search_results
    )
  }
}

# Helper Functions -------------------------------------------------------------
perform_tavily_search <- function(query, tavily_search = NULL, max_results = 5) {
  # Determine the API key (either provided directly as string or from environment)
  api_key <- if (is.character(tavily_search)) {
    tavily_search
  } else {
    Sys.getenv("TAVILY_API_KEY")
  }

  if (!nzchar(api_key)) {
    warning("No TAVILY_API_KEY provided. Skipping web search.")
    return(NULL)
  }

  url <- "https://api.tavily.com/search"
  body <- list(
    api_key = api_key,
    query = query,
    max_results = max_results,
    search_depth = "advanced"  # Using advanced search as default
  )

  res <- httr::POST(
    url = url,
    encode = "json",
    body = body
  )

  if (httr::http_error(res)) {
    warning("Tavily API error: ", httr::content(res, "text"))
    return(NULL)
  }

  parsed <- httr::content(res, "parsed")
  results <- parsed$results
  if (length(results) == 0) {
    return(NULL)
  }

  # Return structured results
  list(
    results = results,
    raw_response = parsed
  )
}

