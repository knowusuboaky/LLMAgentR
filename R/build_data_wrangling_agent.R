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
        from = node_name,
        to = mapping_list[[lbl]],
        condition = condition_fun,
        label = lbl
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

# A simple summary function for a data frame
get_dataframe_summary <- function(df, n_sample = 30, skip_stats = FALSE) {
  info <- capture.output(str(df))
  summ <- capture.output(summary(df))
  head_txt <- capture.output(head(df, n_sample))
  types <- paste(sapply(df, function(col) paste(class(col), collapse = ", ")), collapse = ", ")

  if (!skip_stats) {
    summary_text <- paste(
      sprintf("Shape: %d rows x %d columns", nrow(df), ncol(df)),
      paste("Column types:", types),
      "Head:",
      paste(head_txt, collapse = "\n"),
      "Summary:",
      paste(summ, collapse = "\n"),
      "Structure:",
      paste(info, collapse = "\n"),
      sep = "\n\n"
    )
  } else {
    summary_text <- paste(
      sprintf("Shape: %d rows x %d columns", nrow(df), ncol(df)),
      paste("Column types:", types),
      "Head:",
      paste(head_txt, collapse = "\n"),
      sep = "\n\n"
    )
  }
  summary_text
}

###############################################################################
## 1) GENERIC GRAPH BUILDER (Equivalent to create_coding_agent_graph in Python)
###############################################################################

create_coding_agent_graph <- function(
    node_functions,
    recommended_steps_node_name,
    create_code_node_name,
    execute_code_node_name,
    fix_code_node_name,
    explain_code_node_name,
    error_key,
    max_retries_key = "max_retries",
    retry_count_key = "retry_count",
    human_validation = FALSE,
    human_review_node_name = "human_review",
    checkpointer = NULL,
    bypass_recommended_steps = FALSE,
    bypass_explain_code = FALSE
) {

  workflow <- StateGraph()

  # Always add create, execute, and fix nodes
  workflow$add_node(create_code_node_name, node_functions[[create_code_node_name]])
  workflow$add_node(execute_code_node_name, node_functions[[execute_code_node_name]])
  workflow$add_node(fix_code_node_name, node_functions[[fix_code_node_name]])

  # Conditionally add the recommended-steps node
  if (!bypass_recommended_steps) {
    workflow$add_node(recommended_steps_node_name, node_functions[[recommended_steps_node_name]])
  }

  # Conditionally add the human review node
  if (human_validation) {
    workflow$add_node(human_review_node_name, node_functions[[human_review_node_name]])
  }

  # Conditionally add the explanation node
  if (!bypass_explain_code) {
    workflow$add_node(explain_code_node_name, node_functions[[explain_code_node_name]])
  }

  # Set the entry point
  entry_point <- if (bypass_recommended_steps) create_code_node_name else recommended_steps_node_name
  workflow$set_entry_point(entry_point)

  if (!bypass_recommended_steps) {
    workflow$add_edge(recommended_steps_node_name, create_code_node_name)
  }

  workflow$add_edge(create_code_node_name, execute_code_node_name)
  workflow$add_edge(fix_code_node_name, execute_code_node_name)

  # Helper to check for error and retry possibility
  error_and_can_retry <- function(s) {
    err <- s[[error_key]]
    retr <- s[[retry_count_key]]
    maxr <- s[[max_retries_key]]
    !is.null(err) && !is.null(retr) && !is.null(maxr) && (retr < maxr)
  }

  if (human_validation) {
    workflow$add_conditional_edges(
      execute_code_node_name,
      function(s) {
        if (error_and_can_retry(s)) "fix_code" else "human_review"
      },
      list(
        human_review = human_review_node_name,
        fix_code = fix_code_node_name
      )
    )
  } else {
    if (!bypass_explain_code) {
      workflow$add_conditional_edges(
        execute_code_node_name,
        function(s) {
          if (error_and_can_retry(s)) "fix_code" else "explain_code"
        },
        list(
          fix_code = fix_code_node_name,
          explain_code = explain_code_node_name
        )
      )
    } else {
      workflow$add_conditional_edges(
        execute_code_node_name,
        function(s) {
          if (error_and_can_retry(s)) "fix_code" else "END"
        },
        list(
          fix_code = fix_code_node_name,
          END = workflow$END_NODE_NAME
        )
      )
    }
  }

  if (!bypass_explain_code) {
    workflow$add_edge(explain_code_node_name, workflow$END_NODE_NAME)
  }

  # Compile the workflow
  if (human_validation && !is.null(checkpointer)) {
    app <- workflow$compile(checkpointer = checkpointer)
  } else {
    app <- workflow$compile()
  }
  app
}

###############################################################################
## NODE FUNCTIONS FOR DATA WRANGLING (TRANSFORMATION FOCUS)
###############################################################################

node_recommend_wrangling_steps <- function(model) {
  function(state) {
    cat("---DATA WRANGLING AGENT----\n")
    cat("    * RECOMMEND TRANSFORMATION STEPS\n")

    user_instructions <- state$user_instructions %||% ""
    recommended_steps_prev <- state$recommended_steps %||% ""

    # Handle data inputs
    if (is.data.frame(state$data_raw)) {
      data_list <- list(main = state$data_raw)
    } else if (is.list(state$data_raw)) {
      data_list <- state$data_raw
      names(data_list) <- names(data_list) %||% paste0("dataset_", seq_along(data_list))
    }

    # Generate summaries
    summaries <- map(data_list, ~get_dataframe_summary(.x, skip_stats = TRUE))
    all_datasets_summary <- paste(unlist(summaries), collapse = "\n\n")

    prompt <- sprintf(
      "As a Data Wrangling Expert, recommend steps to transform and reshape the data. Focus on:
- Merging/joining datasets (specify keys)
- Reshaping (pivoting/stacking)
- Feature engineering
- Type conversions
- Aggregations
- Column renaming/reorganization

User instructions: %s
Previous steps: %s
Data summaries: %s

Return numbered steps with brief explanations.",
      user_instructions, recommended_steps_prev, all_datasets_summary
    )

    steps <- model(prompt)

    list(
      recommended_steps = paste("\nRecommended Wrangling Steps:\n", trimws(steps)),
      all_datasets_summary = all_datasets_summary
    )
  }
}

node_create_data_wrangler_code <- function(model,
                                           bypass_recommended_steps = FALSE) {
  function(state) {
    # -----------------------------------------------------------------
    # Initial Agent Notification and State Retrieval
    # -----------------------------------------------------------------
    if (bypass_recommended_steps) {
      cat("---DATA WRANGLING AGENT----\n")
    }
    cat("    * CREATE DATA WRANGLER CODE\n")

    # Retrieve recommended steps and dataset summary from the state.
    recommended_steps <- if (!is.null(state$recommended_steps)) state$recommended_steps else ""
    all_datasets_summary <- if (!is.null(state$all_datasets_summary)) state$all_datasets_summary else "No dataset summary available."

    # -----------------------------------------------------------------
    # Construct the Prompt with the Combined Instructions
    # -----------------------------------------------------------------
    prompt <- sprintf(
      "You are a Data Wrangling Coding Agent. Your job is to create a data_wrangler() function that can be run on the provided data.

      Follow these recommended steps:
      %s

      If multiple datasets are provided, you may need to merge or join them. Make sure to handle that scenario based on the recommended steps and user instructions.

      Below are summaries of all datasets provided. If more than one dataset is provided, you may need to merge or join them:
      %s

      Return R code in ```r``` format with a single function definition, data_wrangler(), that includes all package checks and loading inside the function and returns a single data frame.

      The function should:
      - Take a list of data frames as input (convert to list if not already)
      - Include proper package requirement checks
      - Handle all data wrangling steps
      - Return a single cleaned/wrangled data frame

      Example structure:
      ```r
      data_wrangler <- function(data_list) {
        # Check and load required packages
        if (!requireNamespace('dplyr', quietly = TRUE)) {
          stop(\"Package 'dplyr' is required but not installed.\")
        }
        library(dplyr)

        if (!requireNamespace('tidyr', quietly = TRUE)) {
          stop(\"Package 'tidyr' is required but not installed.\")
        }
        library(tidyr)

        if (!requireNamespace('magrittr', quietly = TRUE)) {
          stop(\"Package 'magrittr' is required for the pipe operator but is not installed.\")
        }
        library(magrittr)

        # If input isn't a list, convert it to one
        if (!is.list(data_list)) {
          data_list <- list(data_list)
        }

        # Implement the wrangling steps here

        # Return a single data frame
        return(data_wrangled)
      }
      ```

      Important requirements:
      1. Include all necessary package checks and imports inside the function
      2. Handle both single dataframe and list of dataframes as input
      3. Comment all non-trivial steps clearly
      4. Follow the recommended steps provided
      5. Ensure the output is properly formatted with r code blocks

      Your output should consist solely of the R function code wrapped in triple backticks as shown in the example.
      Please generate the function accordingly.",
      recommended_steps, all_datasets_summary)

    # -----------------------------------------------------------------
    # Generate the Code Using the Provided Model
    # -----------------------------------------------------------------
    code_raw <- model(prompt)

    # -----------------------------------------------------------------
    # Extract the R Code Enclosed in Triple Backticks Tagged with 'r'
    # -----------------------------------------------------------------
    regex_pattern <- "```r(.*#)```"
    match_result <- regexpr(regex_pattern, code_raw, perl = TRUE)
    if (match_result[1] != -1) {
      # Extract and clean the code captured between the backticks.
      captured <- regmatches(code_raw, match_result)
      code_extracted <- gsub("```r|```", "", captured)
      code_extracted <- trimws(code_extracted)
    } else {
      # Fallback to raw code if the expected pattern isn't found.
      code_extracted <- trimws(code_raw)
    }

    # Remove any stray triple backticks.
    code_extracted <- gsub("```+", "", code_extracted)

    # -----------------------------------------------------------------
    # Return the Generated Code as a List
    # -----------------------------------------------------------------
    list(
      data_wrangler_function = code_extracted,
      data_wrangler_function_path = NULL,
      data_wrangler_function_name = "data_wrangler"
    )
  }
}

node_execute_data_wrangler_code <- function(state) {
  cat("    * EXECUTING DATA WRANGLER CODE\n")

  # 1. Package Management
# Ensure all suggested packages are available
required_packages <- c(
  "utils", "stats", "base", "magrittr", "dplyr", "purrr"
)

invisible(lapply(required_packages, get_suggested))

  # Check and install missing packages
  missing_pkgs <- setdiff(required_packages, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    message("Installing required packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, quiet = TRUE, repos = "https://cloud.r-project.org")
  }

  # Load all packages
  suppressPackageStartupMessages({
    invisible(lapply(required_packages, require, character.only = TRUE))
  })

  # 2. Code Extraction
  extract_r_code_block <- function(text) {
    if (is.null(text)) return(NULL)

    # Pattern 1: Triple backtick code blocks
    pattern_r <- "(#s)```(#:r)#\\s*(.*#)\\s*```"
    matches_r <- regmatches(text, regexec(pattern_r, text, perl = TRUE))
    if (length(matches_r) > 0 && length(matches_r[[1]]) >= 2 && nzchar(matches_r[[1]][2])) {
      return(trimws(matches_r[[1]][2]))
    }

    # Pattern 2: Direct function definition
    pattern_func <- "(#s)(data_wrangler\\s*<-\\s*function\\s*\\([^\\)]*\\)\\s*\\{.*\\})"
    matches_func <- regmatches(text, regexec(pattern_func, text, perl = TRUE))
    if (length(matches_func) > 0 && length(matches_func[[1]]) >= 2 && nzchar(matches_func[[1]][2])) {
      return(trimws(matches_func[[1]][2]))
    }

    warning("Could not extract valid R code from text. Using raw text as code.")
    return(trimws(text))
  }

  # 3. Input Validation
  if (is.null(state$data_wrangler_function)) {
    stop("State is missing data_wrangler_function")
  }

  if (is.null(state$data_list) && is.null(state$data_raw)) {
    stop("State is missing input data (both data_list and data_raw are NULL)")
  }

  code_snippet <- extract_r_code_block(state$data_wrangler_function)
  if (is.null(code_snippet) || nchar(code_snippet) == 0) {
    stop("No R code could be extracted from the data wrangler function")
  }

  if (!grepl("data_wrangler\\s*<-\\s*function", code_snippet)) {
    stop("No valid 'data_wrangler' function detected in the extracted code.")
  }

  # 4. Execution Environment
  local_env <- new.env(parent = baseenv())

  # Load all required functions
  suppressPackageStartupMessages({
    # Core tidyverse
    local_env$`%>%` <- magrittr::`%>%`

    # Load functions from all required packages
    for (pkg in required_packages) {
      pkg_exports <- getNamespaceExports(pkg)
      for (f in pkg_exports) {
        local_env[[f]] <- get(f, envir = getNamespace(pkg))
      }
    }

    # Essential base R functions
    base_funs <- c("c", "list", "data.frame", "as.data.frame", "names",
                   "colnames", "rownames", "grep", "grepl", "sub", "gsub")
    for (f in base_funs) {
      local_env[[f]] <- get(f, envir = baseenv())
    }
  })

  # 5. Execution with Improved Input Handling
  agent_error <- NULL
  result <- NULL

  tryCatch({
    # Parse and evaluate the code
    parsed_code <- parse(text = code_snippet)
    eval(parsed_code, envir = local_env)

    if (!exists("data_wrangler", envir = local_env) || !is.function(local_env$data_wrangler)) {
      stop("'data_wrangler' function not found or invalid")
    }

    # Prepare input data - robust handling
    input_data <- if (!is.null(state$data_list)) {
      if (!is.list(state$data_list)) {
        list(main = state$data_list)
      } else {
        if (is.null(names(state$data_list))) {
          names(state$data_list) <- paste0("dataset_", seq_along(state$data_list))
        }
        state$data_list
      }
    } else {
      if (is.data.frame(state$data_raw)) {
        list(main = state$data_raw)
      } else {
        as.list(state$data_raw)
      }
    }

    # Execute with proper error handling
    res <- tryCatch(
      local_env$data_wrangler(input_data),
      error = function(e) stop("Execution failed: ", e$message)
    )

    # Standardize output
    result <- if (is.data.frame(res)) {
      res
    } else if (is.list(res)) {
      if (all(sapply(res, is.data.frame))) {
        do.call(bind_rows, res)
      } else {
        as.data.frame(res)
      }
    } else {
      data.frame(result = res)
    }

  }, error = function(e) {
    agent_error <<- paste("Data wrangling failed:", e$message)
    cat("ERROR:", agent_error, "\n")
  })

  # 6. Return Results
  list(
    data_wrangled = result,
    data_wrangler_error = agent_error,
    execution_success = is.null(agent_error),
    timestamp = Sys.time()
  )
}

node_explain_data_wrangler_code <- function(model) {
  function(state) {
    summary <- if (!is.null(state$data_wrangler_error)) {
      paste("Error occurred:", state$data_wrangler_error)
    } else {
      get_dataframe_summary(state$data_wrangled, skip_stats = TRUE)
    }

    prompt <- sprintf(
      "Explain these data transformations:\nSteps: %s\n\nResult:\n%s",
      state$recommended_steps, summary
    )

    explanation <- model(prompt)

    list(
      wrangling_report = explanation,
      data_summary = summary
    )
  }
}

node_fix_data_wrangler_code <- function(model) {
  function(state) {
    cat("    * FIX DATA WRANGLER CODE\n")
    cat("      retry_count:", state$retry_count, "\n")

    code_snippet <- state$data_wrangler_function
    error_message <- state$data_wrangler_error
    function_name <- "data_wrangler"

    data_wrangler_prompt <- sprintf(
      "You are a Data Wrangling Agent. Your job is to create a data_wrangler() function that can be run on the data provided. The function is currently broken and needs to be fixed.

      Make sure to only return the function definition for data_wrangler().

      Return R code in ```r``` format with a single function definition, data_wrangler(data_list), that includes all imports inside the function.

      Important requirements:
      1. Include all necessary package checks and imports inside the function
      2. Handle both single dataframe and list of dataframes as input
      3. Comment all non-trivial steps clearly
      4. Ensure proper error handling

      This is the broken code (please fix):
      %s

      Last Known Error:
      %s",
      code_snippet, error_message
    )

    response <- model(data_wrangler_prompt)

    regex_pattern <- "```r(.*#)```"
    match_result <- regexpr(regex_pattern, response, perl = TRUE)

    if (match_result[1] != -1) {
      extracted <- regmatches(response, match_result)
      new_code <- gsub("```r|```", "", extracted)
      new_code <- trimws(new_code)
    } else {
      warning("Could not extract valid R code from LLM response. Returning original snippet.")
      new_code <- code_snippet
    }

    new_retry_val <- state$retry_count + 1
    list(
      data_wrangler_function = new_code,
      data_wrangler_error = NULL,
      retry_count = new_retry_val
    )
  }
}

node_func_human_review <- function(
    prompt_text,
    yes_goto,
    no_goto,
    user_instructions_key = "user_instructions",
    recommended_steps_key = "recommended_steps") {
  function(state) {
    cat(" * HUMAN REVIEW\n")
    steps <- if (!is.null(state[[recommended_steps_key]])) state[[recommended_steps_key]] else ""
    prompt_filled <- sprintf(prompt_text, steps)
    user_input <- interrupt(prompt_filled)
    if (tolower(trimws(user_input)) == "yes") {
      return(list(goto = yes_goto, update = list()))
    } else {
      modifications <- paste("Modifications:", user_input, sep = "\n")
      old_val <- state[[user_instructions_key]]
      if (is.null(old_val)) old_val <- ""
      new_val <- paste(old_val, modifications, sep = "\n")
      return(list(goto = no_goto, update = list(user_instructions = new_val)))
    }
  }
}

###############################################################################
## DATA WRANGLING AGENT IMPLEMENTATION
###############################################################################
# ------------------------------------------------------------------------------

#' Build a Data Wrangling Agent
#'
#' Constructs a state graph-based agent that recommends, generates, executes, fixes,
#' and explains data wrangling transformations based on user instructions and dataset structure.
#' The resulting function handles list or single data frame inputs and produces a cleaned dataset.
#'
#' @name build_data_wrangling_agent
#'
#' @param model A function that takes a prompt string and returns LLM-generated output.
#' @param human_validation Logical; whether to enable manual review step before code execution.
#' @param bypass_recommended_steps Logical; skip initial recommendation of wrangling steps.
#' @param bypass_explain_code Logical; skip final explanation step after wrangling.
#'
#' @return A callable agent function that mutates a provided `state` list by populating:
#'   - `data_wrangled`: the final cleaned data frame,
#'   - `data_wrangler_function`: the code used,
#'   - `data_wrangler_error`: any execution error (if occurred),
#'   - `wrangling_report`: LLM-generated explanation (if `bypass_explain_code = FALSE`)
#'
#' @examples
#' \dontrun{
#' state <- list(
#'   data_raw = iris,
#'   user_instructions = "Merge the data frames on the ID column."
#' )
#' agent <- build_data_wrangling_agent(model = call_llm)
#' agent(state)
#' print(state$data_wrangled)
#' }
#'
#' @export
NULL


build_data_wrangling_agent <- function(
    model,
    human_validation = FALSE,
    bypass_recommended_steps = FALSE,
    bypass_explain_code = FALSE) {

  # Define node functions list
  node_functions <- list(
    recommend_wrangling_steps = node_recommend_wrangling_steps(model),
    human_review = node_func_human_review(
      prompt_text = "Are the following data wrangling instructions correct# (Answer 'yes' or provide modifications)\n%s",
      yes_goto = if (!bypass_explain_code) "explain_data_wrangler_code" else "__end__",
      no_goto = "recommend_wrangling_steps",
      user_instructions_key = "user_instructions",
      recommended_steps_key = "recommended_steps"
    ),
    create_data_wrangler_code = node_create_data_wrangler_code(
      model = model,
      bypass_recommended_steps = bypass_recommended_steps
    ),
    execute_data_wrangler_code = node_execute_data_wrangler_code,
    fix_data_wrangler_code = node_fix_data_wrangler_code(model),
    explain_data_wrangler_code = node_explain_data_wrangler_code(model)
  )

  # Create the agent graph
  app <- create_coding_agent_graph(
    node_functions = node_functions,
    recommended_steps_node_name = "recommend_wrangling_steps",
    create_code_node_name = "create_data_wrangler_code",
    execute_code_node_name = "execute_data_wrangler_code",
    fix_code_node_name = "fix_data_wrangler_code",
    explain_code_node_name = "explain_data_wrangler_code",
    error_key = "data_wrangler_error",
    max_retries_key = "max_retries",
    retry_count_key = "retry_count",
    human_validation = human_validation,
    human_review_node_name = "human_review",
    checkpointer = NULL,
    bypass_recommended_steps = bypass_recommended_steps,
    bypass_explain_code = bypass_explain_code
  )

  # Return a function that can be invoked with state
  function(state) {
    if (is.null(state$retry_count)) state$retry_count <- 0
    if (is.null(state$max_retries)) state$max_retries <- 3
    app(state)
  }
}


