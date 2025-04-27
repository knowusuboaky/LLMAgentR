###############################################################################
## 1) SIMPLE STATE GRAPH IMPLEMENTATION
###############################################################################

make_node <- function(func, name = NULL) {
  list(func = func, name = name)
}

make_edge <- function(from, to, condition = NULL, label = NULL) {
  list(from = from, to = to, condition = condition, label = label)
}

StateGraph <- function() {
  graph_env <- new.env(parent = emptyenv())
  graph_env$nodes <- list()
  graph_env$edges <- list()
  graph_env$entry_point <- NULL

  graph_env$add_node <- function(name, func) {
    graph_env$nodes[[name]] <- make_node(func, name)
  }

  graph_env$add_edge <- function(from, to) {
    edge <- make_edge(from, to)
    graph_env$edges <- c(graph_env$edges, list(edge))
  }

  graph_env$add_conditional_edges <- function(node_name, condition_fun, mapping_list) {
    for (lbl in names(mapping_list)) {
      e <- make_edge(
        from      = node_name,
        to        = mapping_list[[lbl]],
        condition = condition_fun,
        label     = lbl
      )
      graph_env$edges <- c(graph_env$edges, list(e))
    }
  }

  graph_env$set_entry_point <- function(node_name) {
    graph_env$entry_point <- node_name
  }

  END_NODE_NAME <- "__end__"

  graph_env$compile <- function(checkpointer = NULL) {
    function(state) {
      current_node <- if (!is.null(state$current_node)) {
        state$current_node
      } else {
        graph_env$entry_point
      }

      while (!identical(current_node, END_NODE_NAME)) {
        node_obj <- graph_env$nodes[[current_node]]
        if (is.null(node_obj)) {
          stop(sprintf("Node '%s' not found in graph.", current_node))
        }

        # Execute node
        result <- node_obj$func(state)

        # Merge returned list elements
        if (is.list(result)) {
          for (n in names(result)) {
            state[[n]] <- result[[n]]
          }
        }

        # If there's a Command-like object with goto & update:
        if (!is.null(result$goto)) {
          next_node <- result$goto
          if (is.list(result$update)) {
            for (k in names(result$update)) {
              state[[k]] <- result$update[[k]]
            }
          }

          if (identical(next_node, END_NODE_NAME)) {
            current_node <- END_NODE_NAME
            break
          } else {
            current_node <- next_node
            if (!is.null(checkpointer)) {
              checkpointer(state, current_node)
            }
            next
          }
        }

        # If no direct goto, look for edges
        edges_from_node <- Filter(function(e) e$from == current_node, graph_env$edges)
        if (length(edges_from_node) == 0) {
          current_node <- END_NODE_NAME
          break
        }

        # If exactly 1 edge and no condition
        if (length(edges_from_node) == 1 && is.null(edges_from_node[[1]]$condition)) {
          current_node <- edges_from_node[[1]]$to
          if (identical(current_node, END_NODE_NAME)) break
          if (!is.null(checkpointer)) checkpointer(state, current_node)
          next
        }

        # Otherwise we have conditional edges
        chosen_label <- edges_from_node[[1]]$condition(state)

        edge_matched <- NULL
        for (e in edges_from_node) {
          if (!is.null(e$label) && identical(e$label, chosen_label)) {
            edge_matched <- e
            break
          }
        }
        if (is.null(edge_matched)) {
          stop("No matching edge label found!")
        }

        current_node <- edge_matched$to
        if (identical(current_node, END_NODE_NAME)) break
        if (!is.null(checkpointer)) checkpointer(state, current_node)
      }

      state$current_node <- END_NODE_NAME
      invisible(state)
    }
  }

  list(
    add_node              = graph_env$add_node,
    add_edge              = graph_env$add_edge,
    add_conditional_edges = graph_env$add_conditional_edges,
    set_entry_point       = graph_env$set_entry_point,
    compile               = graph_env$compile,
    END_NODE_NAME         = END_NODE_NAME
  )
}


###############################################################################
## 2) HELPER FUNCTIONS
###############################################################################

interrupt <- function(value) {
  message("\n", value, "\n")
  readline("Enter your response: ")
}

make_command <- function(goto = NULL, update = list()) {
  list(goto = goto, update = update)
}


###############################################################################
## 3) NODE FUNCTIONS
###############################################################################

node_func_human_review <- function(
    state,
    prompt_text,
    yes_goto,
    no_goto,
    user_instructions_key = "user_instructions",
    recommended_steps_key = "recommended_steps"
) {
  message("    * HUMAN REVIEW")

  steps <- if (!is.null(state[[recommended_steps_key]])) state[[recommended_steps_key]] else ""
  user_input <- interrupt(sprintf(prompt_text, steps = steps))

  user_input_trim <- tolower(trimws(user_input))
  if (identical(user_input_trim, "yes")) {
    make_command(goto = yes_goto)
  } else {
    modifications <- paste0("Modifications:\n", user_input)
    old_val <- state[[user_instructions_key]]
    if (is.null(old_val)) old_val <- ""
    new_val <- paste(old_val, modifications, sep = "\n")

    make_command(
      goto   = no_goto,
      update = list(user_instructions = new_val)
    )
  }
}

node_func_execute_agent_from_sql_connection <- function(
    state,
    connection,
    code_snippet_key,
    result_key,
    error_key,
    agent_function_name,
    post_processing = NULL,
    error_message_prefix = "An error occurred during agent execution: ",
    verbose = TRUE
) {
  if (verbose) message("    * EXECUTING AGENT CODE ON SQL CONNECTION")

  agent_code  <- state[[code_snippet_key]]
  agent_error <- NULL
  result      <- NULL

  local_env <- new.env(parent = baseenv())

  tryCatch({
    eval(parse(text = agent_code), envir = local_env)

    agent_function <- local_env[[agent_function_name]]
    if (!is.function(agent_function)) {
      stop(sprintf("Agent function '%s' not found or not callable.", agent_function_name))
    }

    # Execute
    res <- agent_function(connection)

    if (!is.null(post_processing)) {
      result <- post_processing(res)
    } else {
      result <- res
    }

  }, error = function(e) {
    if (verbose) message("Error in agent code:", e$message)
    agent_error <<- paste0(error_message_prefix, e$message)
  })

  out <- list()
  out[[result_key]] <- result
  out[[error_key]]  <- agent_error
  out
}

node_func_explain_agent_code <- function(
    state,
    code_snippet_key,
    result_key,
    error_key,
    llm,
    role,
    explanation_prompt_template,
    success_prefix = "# Agent Explanation:\n\n",
    error_message = "The agent encountered an error and cannot be explained.",
    verbose = TRUE
) {
  if (verbose) message("    * EXPLAIN AGENT CODE")

  agent_error <- state[[error_key]]
  if (!is.null(agent_error)) {
    out <- list()
    out[[result_key]] <- list(
      list("content" = error_message, "role" = "ERROR")
    )
    return(out)
  }

  code_snippet <- state[[code_snippet_key]]

  prompt <- gsub("\\{code\\}", code_snippet, explanation_prompt_template)
  expl   <- llm(prompt)

  message_content <- paste0(success_prefix, expl)
  new_message     <- list("content" = message_content, "role" = role)

  out <- list()
  out[[result_key]] <- list(new_message)
  out
}


###############################################################################
## 4) CORE AGENT LOGIC
###############################################################################

# (a) RECOMMEND STEPS
node_recommend_sql_steps <- function(model, connection, n_samples, verbose = TRUE) {
  function(state) {
    if (verbose) {
      message("---SQL DATABASE AGENT---")
      message("    * RECOMMEND SQL QUERY STEPS")
    }

    user_instructions <- if (!is.null(state$user_instructions)) state$user_instructions else ""
    recommended_steps <- if (!is.null(state$recommended_steps)) state$recommended_steps else ""

    # Build database metadata summary
    # ------------------------------------------------------------------

    # --------- CAUTION STOP inserted here ----------
    tables <- DBI::dbListTables(connection)
    if (!DBI::dbIsValid(connection)) {
      stop("Database connection is invalid. Please reconnect before proceeding.")
    }
    if (length(tables) == 0) {
      stop("No tables found in the database. Ensure your database has tables before running the agent.")
    }
    # -------------------------------------------------


    # 1) headline with count and plain list
    headline <- sprintf(
      "### Available Tables (%d total)\n%s\n",
      length(tables),
      paste(sprintf("- %s", sprintf("[%s]", tables)), collapse = "\n")
    )

    # 2) per-table schema details
    detail_lines <- vapply(tables, function(tb) {
      cols <- DBI::dbListFields(connection, tb)
      col_bullets <- paste(sprintf("  - %s", sprintf("[%s]", cols)),
                           collapse = "\n")
      sprintf("\n#### %s\n%s", sprintf("[%s]", tb), col_bullets)
    }, character(1))

    meta <- paste0(headline, "\n---\n", paste(detail_lines, collapse = "\n"))
    # ------------------------------------------------------------------


    prompt_tmpl <- "
You are a SQL Database Instructions Expert. Given the following information about the SQL database,
recommend a series of numbered steps to take to collect the data and process it according to user instructions.
The steps should be tailored to the SQL database characteristics and should be helpful for a SQL database coding agent that will write the SQL code.

IMPORTANT INSTRUCTIONS:
- Take into account the user instructions and the previously recommended steps.
- If no user instructions are provided, just return the steps needed to understand the database.
- Take into account the database dialect and the tables and columns in the database.
- Pay attention to use only the column names you can see in the tables below. Be careful to not query for columns that do not exist. Also, pay attention to which column is in which table.
- IMPORTANT: Pay attention to the table names and column names in the database. Make sure to use the correct table and column names in the SQL code. If a space is present in the table name or column name, make sure to account for it.

User instructions / Question:
%1$s

Previously Recommended Steps (if any):
%2$s

Below are summaries of the database metadata and the SQL tables:
%3$s

Return steps as a numbered list. You can return short code snippets to demonstrate actions. But do not return a fully coded solution. The code will be generated separately by a Coding Agent.

Consider these:
1. Consider the database dialect and the tables and columns in the database.

Avoid these:
1. Do not include steps to save files.
2. Do not include steps to modify existing tables, create new tables or modify the database schema.
3. Do not include steps that alter the existing data in the database.
4. Make sure not to include unsafe code that could cause data loss or corruption or SQL injections.
5. Make sure to not include irrelevant steps that do not help in the SQL agent's data collection and processing. Examples include steps to create new tables, modify the schema, save files, create charts, etc.
"

    prompt <- sprintf(prompt_tmpl, user_instructions, recommended_steps, meta)
    rec_steps <- model(prompt)

    list(
      recommended_steps        = paste0("\n\n# Recommended SQL Database Steps:\n", rec_steps),
      all_sql_database_summary = meta
    )
  }
}

# (b) CREATE THE SQL QUERY CODE
node_create_sql_query_code <- function(model, connection, n_samples, bypass_recommended_steps = FALSE, verbose = TRUE) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b   # helper

  # ---------- helper: pull SQL from any response ---------------------------
  grab_sql <- function(txt) {
    pats <- c("(?s)```sql\\s*(.*?)```", "(?s)```\\s*(.*?)```")
    for (p in pats) {
      m <- regexpr(p, txt, perl = TRUE)
      if (m[1] > 0)
        return(trimws(sub(p, "\\1", regmatches(txt, m), perl = TRUE)))
    }
    trimws(txt)
  }

  # -------------------------------------------------------------------------
  function(state) {
    if (verbose) {
      if (bypass_recommended_steps) message("---SQL DATABASE AGENT---")
      message("    * CREATE SQL QUERY CODE")
    }

    user_instructions <- state$user_instructions %||% ""
    recommended_steps <- state$recommended_steps  %||% ""

    meta <- state$all_sql_database_summary %||% {
      tables <- DBI::dbListTables(connection)

      headline <- sprintf(
        "### Available Tables (%d total)\n%s\n",
        length(tables),
        paste(sprintf("- [%s]", tables), collapse = "\n")
      )

      detail_lines <- vapply(tables, function(tb) {
        cols <- DBI::dbListFields(connection, tb)
        col_bullets <- paste(sprintf("  - [%s]", cols), collapse = "\n")
        sprintf("\n#### [%s]\n%s", tb, col_bullets)
      }, character(1))

      paste0(headline, "\n---\n", paste(detail_lines, collapse = "\n"))
    }

    prompt_tmpl <- "
    You are an SQL Database Coding Expert. Using the details provided about the SQL database,
    please generate an optimized SQL query that retrieves and processes data strictly according
    to the user instructions. Your query must strictly adhere to the provided database metadata,
    including exact table names, column names (including spaces if any), and the specified SQL dialect.

    - IMPORTANT: Do NOT use a LIMIT clause unless the user explicitly includes a limit in their instructions.
    - IMPORTANT: Return the SQL code enclosed in triple backticks with 'sql' as the language tag (i.e., ```sql ... ```).
    - IMPORTANT: Provide exactly one complete SQL query without any extra SQL commands.
    - IMPORTANT: Use only the table names and column names exactly as provided in the metadata. Do not reference any tables or columns that are not mentioned. Every table and column in your query must exist in the provided metadata.
    - IMPORTANT: Do NOT create or invent any table or column names that are not found in the provided metadata.
    - IMPORTANT: Strictly follow the SQL dialect described in the metadata.
    - IMPORTANT: If your query involves joining tables, use the appropriate join type (such as INNER JOIN, LEFT JOIN, or RIGHT JOIN) based on the relationships and requirements provided in the metadata or recommended steps.
    - IMPORTANT: Ensure that your query is read-only. Do NOT include any commands that modify data, create, or alter database objects.
    - IMPORTANT: Do NOT include any extraneous text, commentary, or explanations beyond the SQL query itself.

    -------------------------------------------------------------------------------
     ### Example metadata block

      ### Available Tables (2 total)
      - [Customers]
      - [Orders]

      ---
        #### [Customers]
        - [CustomerID]
      - [CompanyName]

      #### [Orders]
      - [OrderID]
      - [CustomerID]
      - [OrderDate]

      ### Example of a correct query that respects the metadata

      ```sql
      SELECT [CustomerID], [CompanyName]
      FROM   [Customers]
      WHERE  [CustomerID] IN (
        SELECT [CustomerID]
        FROM   [Orders]
        WHERE  [OrderDate] >= '2024-01-01'
      );
Now write one complete SQL query that answers the user request, following all rules above.

User Instruction / Question:
%1$s

Recommended Steps:
%2$s

Database Metadata and Table Schemas:
%3$s

Expected Output:
- IMPORTANT: A complete and correct SQL query, formatted within a code block using triple backticks with 'sql' as the language tag.

Avoid the following:
- IMPORTANT: Do not include any file-saving or data export commands.
- IMPORTANT: Do not include any operations that modify the database schema or existing data.
- IMPORTANT: Do not provide any additional commentary or instructions beyond the SQL query.
"

    prompt <- sprintf(prompt_tmpl, user_instructions, recommended_steps, meta)
    sql_code_raw <- model(prompt)
    draft_sql <- grab_sql(sql_code_raw)
    #message(draft_sql)

    # --------------- 2) REFINE / FIX TABLE & COLUMN NAMES ---------------
    refine_prompt <- sprintf(
      "Please correct the following SQL so that **every** table and column name
matches the metadata exactly (including [] brackets). Do not add or remove
tables or columns. Return the fixed query ONLY inside ```sql``` fences.

--- METADATA ---------------------------------------------------
%s
----------------------------------------------------------------

--- ORIGINAL QUERY ---------------------------------------------
```sql
%s
----------------------------------------------------------------"
      , meta, draft_sql)

    refined_reply <- model(refine_prompt)
    final_sql     <- grab_sql(refined_reply)
    #message(final_sql)

    if (verbose) {
      message(sprintf("      draft -> refined SQL length: %d -> %d",
                      nchar(draft_sql), nchar(final_sql)))
    }

    # quick override for generic SHOW TABLES queries
    if (grepl("(?i)show\\s+tables", final_sql, perl = TRUE)) {
      final_sql <- "SELECT name FROM sqlite_master WHERE type='table';"
    }

    # --------------------- build R wrapper -------------------------------
    if (verbose) message("    * CREATE R FUNCTION TO RUN SQL CODE")

    sql_escaped <- gsub('"', '\\\\\"', final_sql)

    r_func <- sprintf(
      "sql_database_pipeline <- function(connection) {
  library(DBI)
  query <- \"%s\"
  DBI::dbGetQuery(connection, query)
}", sql_escaped)

    # parse-check the R wrapper; stop early if malformed
    tryCatch(parse(text = r_func), error = function(e) {
      warning(sprintf("Generated R wrapper did not parse: %s", e$message), call. = FALSE)
    })

    list(
      sql_query_code             = final_sql,
      sql_database_function      = r_func,
      sql_database_function_name = "sql_database_pipeline"
    )
  }
}

# (c) EXECUTE CODE
#    -> node_func_execute_agent_from_sql_connection (already defined above).

# (d) FIX CODE    now with bracket-validation & autocorrect
# ---------------------------------------------------------------------------
# (d) FIX CODE - robust extraction, bracket validation & safe fallback
# ---------------------------------------------------------------------------
node_fix_sql_database_code <- function(model, connection, verbose = TRUE) {

  first_ok <- function(x) {
    for (v in x) if (!is.na(v) && nzchar(v)) return(v)
    NULL
  }

  closest_name <- function(bad, candidates, max_dist = 2) {
    d <- utils::adist(tolower(bad), tolower(candidates))
    w <- which.min(d)
    if (d[w] <= max_dist) candidates[w] else NA_character_
  }

  # -------------------------------------------------------------------------
  function(state) {
    if (verbose) {
      message("    * FIX AGENT CODE")
      message("      retry_count:", state$retry_count)
    }

    code_snippet  <- state$sql_database_function
    error_message <- state$sql_database_error

    # ---------- build prompt (text unchanged) ------------------------------
    prompt_template <- "
You are an SQL Database Agent code fixer. Your task is to fix the R function sql_database_pipeline(connection).
The function is currently broken and needs to be fixed.
Make sure to only return the function definition for sql_database_pipeline(connection).

Fix it and return only the function as valid R code:

sql_database_pipeline <- function(connection) {
  ...
}

Return R code in ```r``` format with a single function definition: sql_database_pipeline(connection).
Include all necessary library imports inside the function if needed.
Do not modify other parts of the code outside of this function.

Broken code:
{code_snippet}

Last Known Error:
{error}
"
  prompt <- sub("\\{code_snippet\\}", code_snippet, prompt_template, fixed = TRUE)
  prompt <- sub("\\{error\\}",        error_message, prompt,        fixed = TRUE)

  response <- model(prompt)

  # ---------- extract wrapper ------------------------------------------
  fence_r   <- regmatches(response,
                          regexpr("(?s)```[rR]\\s*(.*?)```", response, perl = TRUE))
  fence_any <- regmatches(response,
                          regexpr("(?s)```\\s*(.*?)```",      response, perl = TRUE))
  naked     <- regmatches(response,
                          regexpr("(?s)(sql_database_pipeline\\s*<-\\s*function\\s*\\(connection\\).*?\\})",
                                  response, perl = TRUE))

  candidate <- first_ok(list(
    gsub("(?s)```[rR]\\s*|```", "", fence_r,   perl = TRUE),
    gsub("(?s)```\\s*|```",      "", fence_any, perl = TRUE),
    naked
  ))

  if (is.null(candidate)) {
    warning("No function found in fixer response - reverting.")
    return(list(sql_database_function = code_snippet,
                sql_database_error    = NULL,
                retry_count           = state$retry_count + 1))
  }
  candidate <- trimws(gsub("```+", "", candidate))

  # ---- syntax check ----------------------------------------------------
  ok <- TRUE
  tryCatch(parse(text = candidate), error = function(e) ok <<- FALSE)
  if (!ok) {
    warning("Fixed wrapper fails to parse - reverting.")
    return(list(sql_database_function = code_snippet,
                sql_database_error    = NULL,
                retry_count           = state$retry_count + 1))
  }

  # ---------------------------------------------------------------------
  # BRACKET VALIDATION / AUTOCORRECT
  # ---------------------------------------------------------------------
  # 1) harvest SQL string
  qry_pat <- 'query\\s*<-\\s*["\'](.*?)["\']'
  q_match <- regmatches(candidate, regexpr(qry_pat, candidate, perl = TRUE))
  if (!length(q_match)) {      # nothing to validate
    return(list(sql_database_function = candidate,
                sql_database_error    = NULL,
                retry_count           = state$retry_count + 1))
  }
  sql_query <- gsub(qry_pat, "\\1", q_match, perl = TRUE)

  # 2) real table names from saved metadata (fall back to DB if absent)
  real_tbls <- if (!is.null(state$all_sql_database_summary)) {
    unique(gsub("\\[(.*?)\\]", "\\1",
                regmatches(state$all_sql_database_summary,
                           gregexpr("\\[(.*?)\\]", state$all_sql_database_summary,
                                    perl = TRUE))[[1]]))
  } else DBI::dbListTables(connection)

  # 3) scan identifiers after FROM / JOIN
  tbl_pattern <- "(?i)(?:from|join)\\s+(?:\\[([^]]+)\\]|([A-Za-z0-9_ ]+))"
  raw <- trimws(gsub(tbl_pattern, "\\1\\2",
                     unlist(regmatches(sql_query,
                                       gregexpr(tbl_pattern, sql_query, perl = TRUE))),
                     perl = TRUE))

  found <- vapply(raw, function(tok) {
    if (grepl("^\\[.*\\]$", tok)) sub("^\\[(.*)\\]$", "\\1", tok)
    else strsplit(tok, "\\s+")[[1]][1]
  }, character(1))

  # 4) autocorrect & bracket missing names
  for (nm in found) {
    if (!(tolower(nm) %in% tolower(real_tbls))) {
      fix <- closest_name(nm, real_tbls, 2)
      if (!is.na(fix)) {
        if (verbose) message(sprintf("      autocorrect table: %s -> %s", nm, fix))
        sql_query <- gsub(sprintf("(?i)\\b%s\\b", nm),
                          sprintf("[%s]", fix),
                          sql_query, perl = TRUE)
      }
    } else {  # ensure brackets if they were missing
      sql_query <- gsub(sprintf("(?i)\\b%s\\b", nm),
                        sprintf("[%s]", nm),
                        sql_query, perl = TRUE)
    }
  }

  # 5) fail if anything still unknown
  still_bad <- setdiff(tolower(found), tolower(real_tbls))
  if (length(still_bad)) {
    warning(sprintf("Unknown table(s) remain: %s - reverting.",
                    paste(still_bad, collapse = ", ")))
    return(list(sql_database_function = code_snippet,
                sql_database_error    = NULL,
                retry_count           = state$retry_count + 1))
  }

  # ---------------------------------------------------------------------
  # re-embed the cleaned SQL, escaping any "
  # ---------------------------------------------------------------------
  sql_escaped <- gsub('"', '\\"', sql_query)
  candidate <- sub(qry_pat,
                   sprintf('query <- "%s"', sql_escaped),
                   candidate, perl = TRUE)

  # final parse check
  tryCatch(parse(text = candidate), error = function(e) {
    warning("Wrapper broke after reinsertion - reverting.")
    candidate <<- code_snippet
  })

  list(
    sql_database_function = candidate,
    sql_database_error    = NULL,
    retry_count           = state$retry_count + 1
  )
  }
}



###############################################################################
## 5) MAIN BUILDER FUNCTION
###############################################################################
# ------------------------------------------------------------------------------
#' Build a SQL Agent Graph
#'
#' This function constructs a full SQL database agent using a graph-based workflow.
#' It supports step recommendation, SQL code generation, error handling, optional human review,
#' and automatic explanation of the final code.
#'
#' @name build_sql_agent
#' @param model A function that accepts prompts and returns LLM responses.
#' @param connection A DBI connection object to the target SQL database.
#' @param n_samples Number of candidate SQL plans to consider (used in prompt).
#' @param human_validation Whether to include a human review node.
#' @param bypass_recommended_steps If TRUE, skip the step recommendation node.
#' @param bypass_explain_code If TRUE, skip the final explanation step.
#' @param verbose Logical indicating whether to print progress messages (default: TRUE).
#' @return A compiled SQL agent function that runs via a state machine (graph execution).
#' @examples
#' \dontrun{
#' agent <- build_sql_agent(
#'   model = call_llm,
#'   connection = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
#'   human_validation = FALSE,
#'   verbose = TRUE
#' )
#' state <- list(user_instructions = "Which Categories bring in the highest total revenue# Hint: ...")
#' agent(state)
#' }
#' @export
NULL

build_sql_agent <- function(
    model,
    connection,
    n_samples             = 10,
    human_validation      = FALSE,
    bypass_recommended_steps = FALSE,
    bypass_explain_code   = FALSE,
    verbose               = TRUE
) {

  # Create the graph
  workflow <- StateGraph()

  # NODES
  if (!bypass_recommended_steps) {
    workflow$add_node(
      "recommend_sql_steps",
      node_recommend_sql_steps(model, connection, n_samples, verbose)
    )
  }

  workflow$add_node(
    "create_sql_query_code",
    node_create_sql_query_code(model, connection, n_samples, bypass_recommended_steps, verbose)
  )

  workflow$add_node(
    "execute_sql_database_code",
    function(state) {
      node_func_execute_agent_from_sql_connection(
        state,
        connection          = connection,
        code_snippet_key    = "sql_database_function",
        result_key          = "data_sql",
        error_key           = "sql_database_error",
        agent_function_name = "sql_database_pipeline",
        post_processing     = function(df) {
          if (is.data.frame(df)) as.list(df) else df
        },
        error_message_prefix = "An error occurred executing the SQL pipeline: ",
        verbose = verbose
      )
    }
  )

  workflow$add_node(
    "fix_sql_database_code",
    node_fix_sql_database_code(model,connection, verbose)
  )

  if (human_validation) {
    workflow$add_node(
      "human_review",
      function(state) {
        prompt_text <- "Are these SQL agent instructions correct# (Type 'yes' or corrections)\n%1$s"
        review_fn <- node_func_human_review(
          prompt_text             = prompt_text,
          yes_goto                = if (!bypass_explain_code) "explain_sql_database_code" else "__end__",
          no_goto                 = "recommend_sql_steps",
          user_instructions_key   = "user_instructions",
          recommended_steps_key   = "recommended_steps"
        )
        review_fn(state)
      }
    )
  }

  if (!bypass_explain_code) {
    workflow$add_node(
      "explain_sql_database_code",
      function(state) {
        node_func_explain_agent_code(
          state,
          code_snippet_key         = "sql_database_function",
          result_key               = "messages",
          error_key                = "sql_database_error",
          llm                      = model,
          role                     = "sql_database_agent",
          explanation_prompt_template = "
Explain the SQL steps in this function. Keep it concise:\n\n{code}
",
          success_prefix = "# SQL Database Agent:\n\n",
          verbose = verbose
        )
      }
    )
  }

  # ENTRY
  entry_point <- if (bypass_recommended_steps) {
    "create_sql_query_code"
  } else {
    "recommend_sql_steps"
  }
  workflow$set_entry_point(entry_point)

  # EDGES
  if (!bypass_recommended_steps) {
    workflow$add_edge("recommend_sql_steps", "create_sql_query_code")
  }

  workflow$add_edge("create_sql_query_code", "execute_sql_database_code")
  workflow$add_edge("fix_sql_database_code", "execute_sql_database_code")

  error_and_can_retry <- function(s) {
    err  <- s$sql_database_error
    retr <- s$retry_count
    maxr <- s$max_retries
    !is.null(err) && !is.null(retr) && !is.null(maxr) && (retr < maxr)
  }

  if (human_validation) {
    workflow$add_conditional_edges(
      "execute_sql_database_code",
      function(s) {
        if (error_and_can_retry(s)) {
          "fix_code"
        } else {
          "human_review"
        }
      },
      list(
        "human_review" = "human_review",
        "fix_code"     = "fix_sql_database_code"
      )
    )
  } else {
    if (!bypass_explain_code) {
      workflow$add_conditional_edges(
        "execute_sql_database_code",
        function(s) {
          if (error_and_can_retry(s)) {
            "fix_code"
          } else {
            "explain_code"
          }
        },
        list(
          "fix_code"     = "fix_sql_database_code",
          "explain_code" = "explain_sql_database_code"
        )
      )
    } else {
      workflow$add_conditional_edges(
        "execute_sql_database_code",
        function(s) {
          if (error_and_can_retry(s)) "fix_code" else "END"
        },
        list(
          "fix_code" = "fix_sql_database_code",
          "END"      = workflow$END_NODE_NAME
        )
      )
    }
  }

  if (!bypass_explain_code) {
    workflow$add_edge("explain_sql_database_code", workflow$END_NODE_NAME)
  }

  # compile
  app <- workflow$compile()
  app
}
