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
## NODE FUNCTIONS FOR DATA VISUALIZATION
###############################################################################

node_recommend_visualization_steps <- function(model) {
  function(state) {
    cat("---DATA VISUALIZATION AGENT----\n")
    cat("    * RECOMMEND VISUALIZATION STEPS\n")

    user_instructions <- state$user_instructions %||% ""
    recommended_steps_prev <- state$recommended_steps %||% ""

    # Handle data inputs
    if (is.data.frame(state$data_raw)) {
      df <- state$data_raw
    } else if (is.list(state$data_raw)) {
      df <- as.data.frame(state$data_raw)
    } else {
      stop("Invalid data format - must be dataframe or list")
    }

    all_datasets_summary <- get_dataframe_summary(df, skip_stats = TRUE)

    prompt <- sprintf(
      "You are a supervisor that is an expert in providing instructions to a chart generator agent for plotting.

      You will take a question that a user has and the data that was generated to answer the question, and create instructions to create a chart from the data that will be passed to a chart generator agent.

      USER QUESTION / INSTRUCTIONS:
      %s

      Previously Recommended Instructions (if any):
      %s

      DATA SUMMARY:
      %s

      IMPORTANT:

      - Formulate chart generator instructions by informing the chart generator of what type of plotly plot to use (e.g. bar, line, scatter, etc) to best represent the data.
      - Think about how best to convey the information in the data to the user.
      - If the user does not specify a type of plot, select the appropriate chart type based on the data summary provided and the user's question and how best to show the results.
      - Come up with an informative title from the user's question and data provided. Also provide X and Y axis titles.

      CHART TYPE SELECTION TIPS:

      - If a numeric column has less than 10 unique values, consider this column to be treated as a categorical column. Pick a chart that is appropriate for categorical data.
      - If a numeric column has more than 10 unique values, consider this column to be treated as a continuous column. Pick a chart that is appropriate for continuous data.


      RETURN FORMAT:

      Return your instructions in the following format:
      CHART GENERATOR INSTRUCTIONS:
      FILL IN THE INSTRUCTIONS HERE

      Avoid these:
      1. Do not include steps to save files.
      2. Do not include unrelated user instructions that are not related to the chart generation.",
      user_instructions, recommended_steps_prev, all_datasets_summary
    )

    steps <- model(prompt)

    list(
      recommended_steps = paste("\nRecommended Visualization Steps:\n", trimws(steps)),
      all_datasets_summary = all_datasets_summary
    )
  }
}

node_create_visualization_code <- function(model, function_name = "data_visualization", bypass_recommended_steps = FALSE) {
  function(state) {
    if (bypass_recommended_steps) {
      cat("---DATA VISUALIZATION AGENT----\n")
    }
    cat("    * CREATE VISUALIZATION CODE\n")

    if (bypass_recommended_steps) {
      data_raw <- state$data_raw
      if (is.data.frame(data_raw)) {
        df <- data_raw
      } else {
        df <- as.data.frame(data_raw)
      }

      all_datasets_summary <- get_dataframe_summary(df, skip_stats = FALSE)
      chart_generator_instructions <- state$user_instructions
    } else {
      all_datasets_summary <- state$all_datasets_summary
      chart_generator_instructions <- state$recommended_steps
    }

    prompt <- sprintf(
      "You are a chart generator agent that is an expert in generating plotly charts in R. You must use plotly or ggplot2 to produce plots.

      Your job is to produce R code to generate visualizations with a function named %s.

      You will take instructions from a Chart Instructor and generate a plotly chart from the data provided.

      CHART INSTRUCTIONS:
      %s

      DATA:
      %s

      RETURN:

      Return R code in ```r``` format with a single function definition, %s(data_raw), that includes all imports inside the function.

      Return the plotly chart as a plotly object.

      Return code to provide the data visualization function:

      %s <- function(data_raw) {
        library(plotly)
        library(ggplot2)
        library(dplyr)

        # Convert input to data frame if needed
        if (!is.data.frame(data_raw)) {
          data_raw <- as.data.frame(data_raw)
        }

        # Create visualization here
        p <- ...

        return(p)
      }

      Avoid these:
      1. Do not include steps to save files.
      2. Do not include unrelated user instructions that are not related to the chart generation.",
      function_name, chart_generator_instructions, all_datasets_summary, function_name, function_name
    )

    code_raw <- model(prompt)

    # Extract R code from markdown
    regex_pattern <- "```r(.*#)```"
    match_result <- regexpr(regex_pattern, code_raw, perl = TRUE)
    if (match_result[1] != -1) {
      captured <- regmatches(code_raw, match_result)
      code_extracted <- gsub("```r|```", "", captured)
      code_extracted <- trimws(code_extracted)
    } else {
      code_extracted <- trimws(code_raw)
    }

    # Remove any stray triple backticks
    code_extracted <- gsub("```+", "", code_extracted)

    list(
      visualization_function = code_extracted,
      visualization_function_name = function_name
    )
  }
}

node_execute_visualization_code <- function(state) {
  cat("    * EXECUTING VISUALIZATION CODE\n")

  # 1. Package Management
  required_packages <- c("plotly", "ggplot2", "dplyr", "stringr")

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
    func_name <- state$visualization_function_name %||% "data_visualization"
    pattern_func <- sprintf("(#s)(%s\\s*<-\\s*function\\s*\\([^\\)]*\\)\\s*\\{.*\\})", func_name)
    matches_func <- regmatches(text, regexec(pattern_func, text, perl = TRUE))
    if (length(matches_func) > 0 && length(matches_func[[1]]) >= 2 && nzchar(matches_func[[1]][2])) {
      return(trimws(matches_func[[1]][2]))
    }

    warning("Could not extract valid R code from text. Using raw text as code.")
    return(trimws(text))
  }

  # 3. Input Validation
  if (is.null(state$visualization_function)) {
    stop("State is missing visualization_function")
  }

  if (is.null(state$data_raw)) {
    stop("State is missing input data (data_raw is NULL)")
  }

  code_snippet <- extract_r_code_block(state$visualization_function)
  if (is.null(code_snippet) || nchar(code_snippet) == 0) {
    stop("No R code could be extracted from the visualization function")
  }

  func_name <- state$visualization_function_name %||% "data_visualization"
  if (!grepl(paste0(func_name, "\\s*<-\\s*function"), code_snippet)) {
    stop(sprintf("No valid '%s' function detected in the extracted code.", func_name))
  }

  # 4. Execution Environment
  local_env <- new.env(parent = .GlobalEnv)

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

    if (!exists(func_name, envir = local_env) || !is.function(local_env[[func_name]])) {
      stop(sprintf("'%s' function not found or invalid", func_name))
    }

    # Prepare input data
    input_data <- if (is.data.frame(state$data_raw)) {
      state$data_raw
    } else {
      as.data.frame(state$data_raw)
    }

    # Execute with proper error handling
    res <- tryCatch(
      do.call(func_name, list(input_data), envir = local_env),
      error = function(e) stop("Execution failed: ", e$message)
    )

    # Validate output
    if (!inherits(res, "plotly") && !inherits(res, "ggplot")) {
      warning("Visualization function did not return a plotly or ggplot object")
    }

    result <- res

  }, error = function(e) {
    agent_error <<- paste("Visualization failed:", e$message)
    cat("ERROR:", agent_error, "\n")
  })

  # 6. Return Results
  list(
    visualization_result = result,
    visualization_error = agent_error,
    execution_success = is.null(agent_error),
    timestamp = Sys.time()
  )
}

node_fix_visualization_code <- function(model) {
  function(state) {
    cat("    * FIX VISUALIZATION CODE\n")
    cat("      retry_count:", state$retry_count, "\n")

    code_snippet <- state$visualization_function
    error_message <- state$visualization_error
    function_name <- state$visualization_function_name %||% "data_visualization"

    prompt <- sprintf(
      "You are a Data Visualization Agent in R. Your job is to fix the %s() function that currently contains errors.

      Make sure to only return the function definition for %s().

      Return R code in ```r``` format with a single function definition, %s(data_raw), that includes all imports inside the function.

      Important requirements:
      1. Include all necessary package checks and imports inside the function
      2. Handle both single dataframe and list of dataframes as input
      3. Comment all non-trivial steps clearly
      4. Ensure proper error handling

      This is the broken code (please fix):
      %s

      Last Known Error:
      %s",
      function_name, function_name, function_name, code_snippet, error_message
    )

    response <- model(prompt)

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
      visualization_function = new_code,
      visualization_error = NULL,
      retry_count = new_retry_val
    )
  }
}

node_explain_visualization_code <- function(model) {
  function(state) {
    summary <- if (!is.null(state$visualization_error)) {
      paste("Error occurred:", state$visualization_error)
    } else {
      "Visualization created successfully"
    }

    prompt <- sprintf(
      "Explain these visualization transformations:\nSteps: %s\n\nResult:\n%s",
      state$recommended_steps, summary
    )

    explanation <- model(prompt)

    list(
      visualization_report = explanation,
      visualization_summary = summary
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
## DATA VISUALIZATION AGENT IMPLEMENTATION
###############################################################################
# ------------------------------------------------------------------------------
#' Build a Data Visualization Agent
#'
#' Constructs a state graph-based visualization agent that:
#' recommends, generates, executes, fixes, and explains charting code using `plotly` or `ggplot2`.
#'
#' @name build_visualization_agent
#' @param model A function that takes a prompt and returns an LLM-generated result.
#' @param human_validation Logical; enable a manual review step.
#' @param bypass_recommended_steps Logical; skip initial step recommendation.
#' @param bypass_explain_code Logical; skip the final explanation step.
#' @param function_name Name for the generated visualization function (default: `"data_visualization"`).
#'
#' @return A callable agent function that mutates the given `state` list.
#' @examples
#' \dontrun{
#' state <- list(data_raw = iris, user_instructions = "Plot Sepal.Length by Species")
#' agent <- build_visualization_agent(model = call_llm)
#' agent(state)
#' print(state$visualization_result)
#' }
#' @export
NULL

build_visualization_agent <- function(
    model,
    human_validation = FALSE,
    bypass_recommended_steps = FALSE,
    bypass_explain_code = FALSE,
    function_name = "data_visualization") {

  # Ensure suggested packages are available
  required_suggested_packages <- c("magrittr", "graphics", "utils", "plotly")
  invisible(lapply(required_suggested_packages, get_suggested))

  # Define node functions list
  node_functions <- list(
    recommend_visualization_steps = node_recommend_visualization_steps(model),
    human_review = node_func_human_review(
      prompt_text = "Are the following visualization instructions correct# (Answer 'yes' or provide modifications)\n%s",
      yes_goto = if (!bypass_explain_code) "explain_visualization_code" else "__end__",
      no_goto = "recommend_visualization_steps",
      user_instructions_key = "user_instructions",
      recommended_steps_key = "recommended_steps"
    ),
    create_visualization_code = node_create_visualization_code(
      model = model,
      function_name = function_name,
      bypass_recommended_steps = bypass_recommended_steps
    ),
    execute_visualization_code = node_execute_visualization_code,
    fix_visualization_code = node_fix_visualization_code(model),
    explain_visualization_code = node_explain_visualization_code(model)
  )

  # Create the agent graph
  app <- create_coding_agent_graph(
    node_functions = node_functions,
    recommended_steps_node_name = "recommend_visualization_steps",
    create_code_node_name = "create_visualization_code",
    execute_code_node_name = "execute_visualization_code",
    fix_code_node_name = "fix_visualization_code",
    explain_code_node_name = "explain_visualization_code",
    error_key = "visualization_error",
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
