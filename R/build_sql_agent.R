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
  cat("\n", value, "\n")
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
  cat("    * HUMAN REVIEW\n")

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
    error_message_prefix = "An error occurred during agent execution: "
) {
  cat("    * EXECUTING AGENT CODE ON SQL CONNECTION\n")

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
    cat("Error in agent code:", e$message, "\n")
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
    error_message = "The agent encountered an error and cannot be explained."
) {
  cat("    * EXPLAIN AGENT CODE\n")

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
node_recommend_sql_steps <- function(model, connection, n_samples) {
  function(state) {
    cat("---SQL DATABASE AGENT---\n")
    cat("    * RECOMMEND SQL QUERY STEPS\n")

    user_instructions <- if (!is.null(state$user_instructions)) state$user_instructions else ""
    recommended_steps <- if (!is.null(state$recommended_steps)) state$recommended_steps else ""

    # Build database metadata summary
    tables <- dbListTables(connection)
    meta_lines <- c()
    for (tb in tables) {
      cols <- dbListFields(connection, tb)
      meta_lines <- c(meta_lines, paste0("TABLE: ", tb, "\n  COLUMNS: ", paste(cols, collapse=", ")))
    }
    meta <- paste(meta_lines, collapse="\n\n")

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
node_create_sql_query_code <- function(model, connection, n_samples, bypass_recommended_steps = FALSE) {
  function(state) {
    # Display a header if bypassing recommended steps.
    if (bypass_recommended_steps) {
      cat("---SQL DATABASE AGENT---\n")
    }
    cat("    * CREATE SQL QUERY CODE\n")

    # Retrieve user instructions and recommended steps from state, defaulting to an empty string if not provided.
    user_instructions <- if (!is.null(state$user_instructions)) state$user_instructions else ""
    recommended_steps  <- if (!is.null(state$recommended_steps)) state$recommended_steps else ""

    # Use provided metadata if available; otherwise, build metadata summary from the DBI connection.
    if (!is.null(state$all_sql_database_summary)) {
      meta <- state$all_sql_database_summary
    } else {
      tables <- dbListTables(connection)
      meta_lines <- c()
      for (tb in tables) {
        cols <- dbListFields(connection, tb)
        meta_lines <- c(meta_lines, paste0("TABLE: ", tb, "\n  COLUMNS: ", paste(cols, collapse = ", ")))
      }
      meta <- paste(meta_lines, collapse = "\n\n")
    }

    # Define a detailed prompt template that instructs the model to use the proper join type if applicable.
    prompt_tmpl <- "
You are an SQL Database Coding Expert. Using the details provided about the SQL database, please generate an optimized SQL query that retrieves and processes data strictly according to the user instructions. Your query must strictly adhere to the provided database metadata, including exact table names, column names (including spaces if any), and the specified SQL dialect.

- IMPORTANT: Do NOT use a LIMIT clause unless the user explicitly includes a limit in their instructions.
- IMPORTANT: Return the SQL code enclosed in triple backticks with 'sql' as the language tag (i.e., ```sql ... ```).
- IMPORTANT: Provide exactly one complete SQL query without any extra SQL commands.
- IMPORTANT: Use only the table names and column names exactly as provided in the metadata. Do not reference any tables or columns that are not mentioned. Every table and column in your query must exist in the provided metadata.
- IMPORTANT: Do NOT create or invent any table or column names that are not found in the provided metadata.
- IMPORTANT: Strictly follow the SQL dialect described in the metadata.
- IMPORTANT: If your query involves joining tables, use the appropriate join type (such as INNER JOIN, LEFT JOIN, or RIGHT JOIN) based on the relationships and requirements provided in the metadata or recommended steps.
- IMPORTANT: Ensure that your query is read-only. Do NOT include any commands that modify data, create, or alter database objects.
- IMPORTANT: Do NOT include any extraneous text, commentary, or explanations beyond the SQL query itself.

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

    # Generate the complete prompt by substituting the placeholders with dynamic values.
    prompt <- sprintf(prompt_tmpl, user_instructions, recommended_steps, meta)

    # Call the model to generate the SQL code.
    # (Ensure that 'model' is a working function that accepts the prompt string and returns the model's output.)
    sql_code_raw <- model(prompt)

    # Extract the SQL query from the model's output, expecting it to be enclosed in triple backticks tagged with 'sql'.
    regex_pattern <- "```sql(.*#)```"
    match_result <- regexpr(regex_pattern, sql_code_raw, perl = TRUE)

    if (match_result[1] != -1) {
      captured_text <- regmatches(sql_code_raw, match_result)
      sql_query_code <- gsub("```sql|```", "", captured_text)
      sql_query_code <- trimws(sql_query_code)
    } else {
      # Fallback: if markdown formatting is not detected, assume the output is plain SQL.
      sql_query_code <- trimws(sql_code_raw)
    }

    # Remove any stray triple backticks remaining.
    sql_query_code <- gsub("```+", "", sql_query_code)

    # **** NEW PART: Clean up potential issues.
    # Remove any standalone 'sql' token (to avoid potential issues in SQLite).
    sql_query_code <- gsub("\\bsql\\b", "", sql_query_code, ignore.case = TRUE)
    # Collapse multiple whitespace characters into a single space for cleaner formatting.
    sql_query_code <- gsub("\\s+", " ", sql_query_code)
    sql_query_code <- trimws(sql_query_code)
    # **** END NEW PART ****

    # If the model mistakenly returns MySQL-specific syntax (like "SHOW TABLES;"), replace it with an SQLite-safe query.
    if (grepl("(#i)show\\s+tables", sql_query_code, perl = TRUE)) {
      sql_query_code <- "SELECT name FROM sqlite_master WHERE type='table';"
    }

    cat("    * CREATE R FUNCTION TO RUN SQL CODE\n")
    # Build an R function snippet that will execute the generated SQL code on a DBI connection.
    r_func <- sprintf("
sql_database_pipeline <- function(connection) {
  library(DBI)
  query <- \"%s\"
  df <- DBI::dbGetQuery(connection, query)
  df
}
", sql_query_code)

    # Return a list containing the cleaned SQL query and the R function snippet.
    list(
      sql_query_code             = sql_query_code,
      sql_database_function      = r_func,
      sql_database_function_name = "sql_database_pipeline"
    )
  }
}

# (c) EXECUTE CODE
#    -> node_func_execute_agent_from_sql_connection (already defined above).

# (d) FIX CODE: More robust to partial or truncated LLM responses.
node_fix_sql_database_code <- function(model) {
  function(state) {
    cat("    * FIX AGENT CODE\n")
    cat("      retry_count:", state$retry_count, "\n")

    code_snippet  <- state$sql_database_function
    error_message <- state$sql_database_error

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
  prompt <- gsub("\\{code_snippet\\}", code_snippet, prompt_template)
  prompt <- gsub("\\{error\\}",        error_message, prompt)

  # Call the LLM
  response <- model(prompt)

  # 1) Attempt triple backtick extraction (```r, ```R, or ```).
  code_pattern <- "```[rR]#\\s*(.*#)```"
  match <- regexpr(code_pattern, response, perl = TRUE)

  if (match[1] != -1) {
    # Found code in triple backticks
    captured_code <- regmatches(response, match)
    # remove triple backticks
    extracted <- gsub("```[rR]#\\s*|```", "", captured_code)
    new_code   <- trimws(extracted)
  } else {
    # 2) If no triple-backtick block, look for the function definition pattern:
    func_pattern <- "sql_database_pipeline\\s*<-\\s*function\\s*\\(connection\\)\\s*\\{.*#\\}"
    match2 <- regexpr(func_pattern, response, perl = TRUE, ignore.case = FALSE)

    if (match2[1] != -1) {
      captured_func <- regmatches(response, match2)
      new_code <- captured_func
    } else {
      # Instead of stopping, WARN and return the old snippet so we don't crash the agent.
      warning("Could not extract any valid R code from LLM response. Returning original snippet instead.")
      new_code <- code_snippet
    }
  }

  # Remove any leftover triple backticks inside
  new_code <- gsub("```+", "", new_code)

  new_retry_val <- state$retry_count + 1

  out <- list(
    sql_database_function = new_code,
    sql_database_error    = NULL,      # we reset to NULL (the new snippet may or may not work)
    retry_count           = new_retry_val
  )
  out
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
#'
#' @return A compiled SQL agent function that runs via a state machine (graph execution).
#' @examples
#' \dontrun{
#' agent <- build_sql_agent(
#'   model = call_llm,
#'   connection = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
#'   human_validation = FALSE
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
    human_validation     = FALSE,
    bypass_recommended_steps = FALSE,
    bypass_explain_code   = FALSE
) {

  # Create the graph
  workflow <- StateGraph()

  # NODES
  if (!bypass_recommended_steps) {
    workflow$add_node(
      "recommend_sql_steps",
      node_recommend_sql_steps(model, connection, n_samples)
    )
  }

  workflow$add_node(
    "create_sql_query_code",
    node_create_sql_query_code(model, connection, n_samples, bypass_recommended_steps)
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
        error_message_prefix = "An error occurred executing the SQL pipeline: "
      )
    }
  )

  workflow$add_node(
    "fix_sql_database_code",
    node_fix_sql_database_code(model)
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
          success_prefix = "# SQL Database Agent:\n\n"
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

  # Condition: if there's an error & we haven't retried too many times
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
      # if error => fix, else => end
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
