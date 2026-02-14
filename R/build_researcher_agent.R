# ------------------------------------------------------------------------------
#' Build a Web Researcher Agent
#'
#' Constructs an LLM-powered research agent that performs web searches (via Tavily API)
#' and generates structured responses based on search results. The agent handles different
#' question types (general knowledge, comparisons, controversial topics) with appropriate
#' response formats.
#'
#' @name build_researcher_agent
#' @param llm A function that accepts a character prompt and returns an LLM response.
#'             (It must accept `prompt` and optionally `verbose`.)
#' @param tavily_search Tavily API key as a string or NULL to use `Sys.getenv("TAVILY_API_KEY")`.
#' @param system_prompt Optional custom system prompt for the researcher agent.
#' @param max_results Number of web search results to retrieve per query (default: 5).
#' @param max_tries Maximum number of retry attempts for search or LLM call (default: 3).
#' @param backoff Initial wait time in seconds between retries (default: 2).
#' @param verbose Logical flag to control progress messages (default: TRUE).
#'
#' @return A function that accepts a user query string and returns a list with:
#' \itemize{
#'   \item query - The original research query.
#'   \item prompt - The full prompt sent to the LLM.
#'   \item response - The generated LLM response.
#'   \item search_results - Raw search results (if any were found).
#'   \item success - Logical indicating if research succeeded (both search and LLM).
#' }
#'
#' @examples
#' \dontrun{
#' # Initialize researcher agent
#' researcher_agent <- build_researcher_agent(
#'   llm = my_llm_wrapper,
#'   tavily_search = NULL,
#'   system_prompt = NULL,
#'   max_results = 5,
#'   max_tries = 3,
#'   backoff = 2,
#'   verbose = FALSE
#' )
#'
#' # Perform research
#' result <- researcher_agent("Who is Messi?")
#' }
#'
#' @export
NULL

build_researcher_agent <- function(
    llm,
    tavily_search = NULL,
    system_prompt = NULL,
    max_results   = 5,
    max_tries     = 3,
    backoff       = 2,
    verbose       = TRUE,
    output        = c("agent", "mermaid", "both"),
    direction     = c("TD", "LR"),
    subgraphs     = NULL,
    style         = TRUE
) {
  output <- match.arg(output)
  direction <- match.arg(direction)

  if (!identical(output, "agent")) {
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    node_functions <- list(
      resolve_tavily_key = function(state) {
        key <- state$tavily_api_key %||% tavily_search %||% Sys.getenv("TAVILY_API_KEY", unset = "")
        list(
          tavily_api_key = key,
          query = state$query %||% state$user_query,
          search_attempt = as.integer(state$search_attempt %||% 0L),
          llm_attempt = as.integer(state$llm_attempt %||% 0L)
        )
      },
      search_web = function(state) {
        out <- list(search_attempt = as.integer(state$search_attempt %||% 0L) + 1L)
        if (!nzchar(state$tavily_api_key %||% "")) {
          out$search_results <- NULL
          out$search_error <- "No Tavily API key provided."
          return(out)
        }
        result <- tryCatch(
          perform_tavily_search(
            query = state$query %||% "",
            tavily_search = state$tavily_api_key,
            max_results = max_results
          ),
          error = function(e) e
        )
        if (inherits(result, "error")) {
          out$search_results <- NULL
          out$search_error <- result$message
        } else {
          out$search_results <- result
          out$search_error <- NULL
        }
        out
      },
      check_search_results = function(state) {
        results <- state$search_results$results %||% list()
        route <- if (length(results) > 0) {
          "found"
        } else if (as.integer(state$search_attempt %||% 0L) < as.integer(state$max_tries %||% max_tries) &&
                   nzchar(state$tavily_api_key %||% "")) {
          "retry"
        } else {
          "none"
        }
        list(route = route)
      },
      search_backoff = function(state) {
        attempt <- as.integer(state$search_attempt %||% 1L)
        wait_sec <- as.numeric(state$backoff %||% backoff) * (2 ^ max(0, attempt - 1L))
        Sys.sleep(wait_sec)
        list()
      },
      build_prompt_from_results = function(state) {
        snippets <- paste(
          vapply(
            state$search_results$results,
            function(r) {
              sprintf("---\n%s\n%s\nSource: %s", r$title %||% "", r$content %||% "", r$url %||% "")
            },
            FUN.VALUE = character(1)
          ),
          collapse = "\n\n"
        )
        prompt_used <- system_prompt %||% "You are a highly skilled web researcher."
        full_prompt <- sprintf(
          "%s\n\nUSER QUERY:\n%s\n\nWEB SEARCH RESULTS:\n%s",
          prompt_used,
          state$query %||% "",
          snippets
        )
        list(prompt = full_prompt)
      },
      build_prompt_without_results = function(state) {
        prompt_used <- system_prompt %||% "You are a highly skilled web researcher."
        full_prompt <- sprintf("%s\n\nUSER QUERY:\n%s", prompt_used, state$query %||% "")
        list(prompt = full_prompt)
      },
      call_llm = function(state) {
        attempt <- as.integer(state$llm_attempt %||% 0L) + 1L
        llm_response <- NULL
        llm_error <- NULL
        tryCatch({
          llm_response <- if ("verbose" %in% names(formals(llm))) {
            llm(prompt = state$prompt, verbose = verbose)
          } else {
            llm(prompt = state$prompt)
          }
          if (is.null(llm_response) || !nzchar(trimws(llm_response))) {
            stop("Empty response received from LLM.")
          }
        }, error = function(e) {
          llm_error <<- e$message
        })
        list(llm_attempt = attempt, llm_response = llm_response, llm_error = llm_error)
      },
      check_llm_response = function(state) {
        route <- if (!is.null(state$llm_response) && nzchar(trimws(state$llm_response))) {
          "success"
        } else if (as.integer(state$llm_attempt %||% 0L) < as.integer(state$max_tries %||% max_tries)) {
          "retry"
        } else {
          "failed"
        }
        list(route = route)
      },
      llm_backoff = function(state) {
        attempt <- as.integer(state$llm_attempt %||% 1L)
        wait_sec <- as.numeric(state$backoff %||% backoff) * (2 ^ max(0, attempt - 1L))
        Sys.sleep(wait_sec)
        list()
      },
      return_research_output = function(state) {
        list(
          query = state$query %||% "",
          prompt = state$prompt %||% "",
          response = state$llm_response,
          search_results = state$search_results,
          success = !is.null(state$llm_response)
        )
      }
    )

    edges <- list(
      c("resolve_tavily_key", "search_web"),
      c("search_web", "check_search_results"),
      c("search_backoff", "search_web"),
      c("build_prompt_from_results", "call_llm"),
      c("build_prompt_without_results", "call_llm"),
      c("call_llm", "check_llm_response"),
      c("llm_backoff", "call_llm"),
      c("return_research_output", "__end__")
    )
    conditional_edges <- list(
      list(
        from = "check_search_results",
        condition = function(state) state$route %||% "none",
        mapping = list(
          found = "build_prompt_from_results",
          retry = "search_backoff",
          none = "build_prompt_without_results"
        )
      ),
      list(
        from = "check_llm_response",
        condition = function(state) state$route %||% "failed",
        mapping = list(
          success = "return_research_output",
          retry = "llm_backoff",
          failed = "return_research_output"
        )
      )
    )

    compiled <- build_custom_agent(
      node_functions = node_functions,
      entry_point = "resolve_tavily_key",
      edges = edges,
      conditional_edges = conditional_edges,
      output = "both",
      direction = direction,
      subgraphs = subgraphs,
      style = style
    )

    if (identical(output, "mermaid")) {
      return(compiled$mermaid)
    }
    return(compiled)
  }

  if (verbose) cat("=== STARTING RESEARCHER AGENT ===\n")

  ## ------------------------------------------------------------------------
  ## 0.  Resolve Tavily API key once
  ## ------------------------------------------------------------------------
  tavily_api_key <- if (!is.null(tavily_search) && nzchar(tavily_search)) {
    tavily_search
  } else {
    Sys.getenv("TAVILY_API_KEY", unset = "")
  }
  if (!nzchar(tavily_api_key)) {
    stop("No Tavily API key found.  Pass it or set TAVILY_API_KEY.", call. = FALSE)
  }

  ## ------------------------------------------------------------------------
  ## 1.  Default system-prompt (if none supplied)
  ## ------------------------------------------------------------------------
  if (is.null(system_prompt)) {
    system_prompt <- "
You are a highly skilled web researcher tasked with providing accurate and
You are a highly skilled web researcher tasked with providing accurate and reliable information tailored to the user's needs.
For data requests, you will gather relevant data and return it structured in a clear format.

For all other types of queries, follow the specified formats and ensure accuracy.

**Instructions**:

1. **Understand the Question**:
- Identify whether the question is:
  - A **general knowledge question** (e.g., \"Who is Messi?\")
  - A **data request** (e.g., historical stock prices, GDP, statistics)
  - A **comparative analysis** (e.g., \"Messi vs. Ronaldo\")
  - A **controversial or multifaceted question** (e.g., \"Is AI dangerous?\")
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

  ## ------------------------------------------------------------------------
  ## 2.  RETURN THE AGENT CLOSURE
  ## ------------------------------------------------------------------------
  function(query) {

    # ---- 2-a.  TAVILY SEARCH with retries ---------------------------------
    search_results <- NULL
    for (attempt in seq_len(max_tries)) {
      if (verbose)
        message(sprintf("Search attempt %d/%d - '%s'",
                        attempt, max_tries, query))

      search_results <- tryCatch(
        perform_tavily_search(
          query         = query,
          tavily_search = tavily_api_key,
          max_results   = max_results
        ),
        error = function(e) {
          if (verbose)
            warning(sprintf("Search attempt %d failed: %s", attempt, e$message))
          NULL
        }
      )

      if (!is.null(search_results) && length(search_results$results) > 0) {
        if (verbose) message("Search succeeded on attempt ", attempt)
        break
      }

      if (attempt < max_tries) Sys.sleep(backoff * (2 ^ (attempt - 1)))
    }

    # ---- 2-b.  BUILD PROMPT ----------------------------------------------
    if (!is.null(search_results) && length(search_results$results) > 0) {
      web_snippets <- paste(
        vapply(
          search_results$results,
          \(r) sprintf("---\n%s\n%s\nSource: %s",
                       r$title, r$content, r$url),
          FUN.VALUE = character(1)
        ),
        collapse = "\n\n"
      )

      full_prompt <- sprintf(
        "%s\n\nUSER QUERY:\n%s\n\nWEB SEARCH RESULTS:\n%s",
        system_prompt, query, web_snippets
      )
    } else {
      if (verbose) warning("Proceeding without search results.")
      full_prompt <- sprintf("%s\n\nUSER QUERY:\n%s",
                             system_prompt, query)
    }

    # ---- 2-c.  LLM CALL with retries -------------------------------------
    llm_response <- NULL
    for (attempt in seq_len(max_tries)) {
      if (verbose)
        message(sprintf("LLM attempt %d/%d", attempt, max_tries))

      llm_response <- tryCatch(
        llm(prompt = full_prompt, verbose = verbose),
        error = function(e) {
          if (verbose)
            warning(sprintf("LLM attempt %d failed: %s", attempt, e$message))
          NULL
        }
      )
      if (!is.null(llm_response)) break
      if (attempt < max_tries) Sys.sleep(backoff * (2 ^ (attempt - 1)))
    }

    # ---- 2-d.  RETURN -----------------------------------------------------
    list(
      query          = query,
      prompt         = full_prompt,
      response       = llm_response,
      search_results = search_results,
      success        = !is.null(search_results) && !is.null(llm_response)
    )
  }
}

###############################################################################
#  Helper: perform_tavily_search()  (unchanged signature)
###############################################################################
perform_tavily_search <- function(query,
                                  tavily_search = NULL,
                                  max_results   = 5) {

  api_key <- if (is.character(tavily_search) && nzchar(tavily_search)) {
    tavily_search
  } else {
    Sys.getenv("TAVILY_API_KEY", unset = "")
  }
  if (!nzchar(api_key))
    stop("No Tavily API key provided.", call. = FALSE)

  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE))
    stop('Install packages "httr" and "jsonlite".', call. = FALSE)

  res <- httr::POST(
    url   = "https://api.tavily.com/search",
    httr::add_headers("Content-Type" = "application/json"),
    body  = jsonlite::toJSON(
      list(
        api_key             = api_key,
        query               = query,
        max_results         = max_results,
        search_depth        = "advanced",
        include_raw_content = TRUE
      ),
      auto_unbox = TRUE
    ),
    encode = "json"
  )

  if (httr::http_error(res))
    stop(sprintf("Tavily API error [%s]: %s",
                 httr::status_code(res),
                 httr::content(res, "text", encoding = "UTF-8")),
         call. = FALSE)

  parsed <- httr::content(res, "parsed", encoding = "UTF-8")
  list(results = parsed$results, raw_response = parsed)
}
