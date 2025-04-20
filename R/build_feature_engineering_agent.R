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
## NODE FUNCTIONS FOR FEATURE ENGINEERING
###############################################################################

node_recommend_feature_engineering_steps <- function(model) {
  function(state) {
    cat("---FEATURE ENGINEERING AGENT----\n")
    cat("    * RECOMMEND FEATURE ENGINEERING STEPS\n")


    # 1. Packages
    # Ensure all required packages from Suggests are installed
    required_packages <- c(
      "stringr", "prrr", "dplyr", "tidyr", "purrr", "lubridate", "forcats",
      "tibble", "magrittr", "recipes", "rsample", "yardstick", "glue", "stats"
    )

    invisible(lapply(required_packages, get_suggested))

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
      "You are a Feature Engineering Expert. Given the following information about the data,
      recommend a series of numbered steps to take to engineer features.
      The steps should be tailored to the data characteristics and should be helpful
      for a feature engineering agent that will be implemented.

      General Steps:
      Things that should be considered in the feature engineering steps:

      * Convert features to the appropriate data types based on their sample data values
      * Remove string or categorical features with unique values equal to the size of the dataset
      * Remove constant features with the same value in all rows
      * High cardinality categorical features should be encoded by a threshold <= 5 percent of the dataset, by converting infrequent values to \"other\"
      * Encoding categorical variables using OneHotEncoding
      * Numeric features should be left untransformed
      * Create datetime-based features if datetime columns are present
      * If a target variable is provided:
          * If a categorical target variable is provided, encode it using LabelEncoding
          * All other target variables should be converted to numeric and unscaled
      * Convert any Boolean (True/False) values to integer (1/0) values. This should be performed after one-hot encoding.

      Custom Steps:
      * Analyze the data to determine if any additional feature engineering steps are needed.
      * Recommend steps that are specific to the data provided. Include why these steps are necessary or beneficial.
      * If no additional steps are needed, simply state that no additional steps are required.

      IMPORTANT:
      Make sure to take into account any additional user instructions that may add, remove or modify some of these steps.
      Include comments in your code to explain your reasoning for each step.
      Include comments if something is not done because a user requested.
      Include comments if something is done because a user requested.

      User instructions: %s

      Previously Recommended Steps (if any): %s

      Below are summaries of all datasets provided:
      %s

      Return the steps as a numbered list. You can return short code snippets to demonstrate actions. But do not return a fully coded solution. The code will be generated separately by a Coding Agent.

      Avoid these:
      1. Do not include steps to save files.
      2. Do not include unrelated user instructions that are not related to the feature engineering.",
      user_instructions, recommended_steps_prev, all_datasets_summary
    )

    steps <- model(prompt)

    list(
      recommended_steps = paste("\nRecommended Feature Engineering Steps:\n", trimws(steps)),
      all_datasets_summary = all_datasets_summary
    )
  }
}

node_create_feature_engineering_code <- function(model, bypass_recommended_steps = FALSE) {
  function(state) {
    if (bypass_recommended_steps) {
      cat("---FEATURE ENGINEERING AGENT----\n")
    }
    cat("    * CREATE FEATURE ENGINEERING CODE\n")

    recommended_steps <- if (!is.null(state$recommended_steps)) state$recommended_steps else ""
    all_datasets_summary <- if (!is.null(state$all_datasets_summary)) state$all_datasets_summary else "No dataset summary available."
    target_variable <- state$target_variable %||% "None provided"

    prompt <- sprintf(
      "You are a Feature Engineering Agent. Your job is to create a feature_engineer() function that can be run on the data provided using the following recommended steps.

      Recommended Steps:
      %s

      Use this information about the data to help determine how to feature engineer the data:

      Target Variable (if provided): %s

      Data Summary:
      %s

      Return R code in ```r``` format with a single function definition, feature_engineer(data_raw), that includes all package checks and loading inside the function.

      The function should:
      - Take a data frame or list as input
      - Include proper package requirement checks
      - Handle all feature engineering steps
      - Return a single engineered data frame

      Example structure:
      ```r
      feature_engineer <- function(data_raw) {

        # Step 0: Check and load required packages
        required_packages <- c(
          'dplyr', 'tidyr', 'purrr', 'stringr',
          'lubridate', 'forcats', 'tibble', 'magrittr',
          'recipes', 'rsample', 'yardstick',
          'glue', 'stats'
        )

        for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(sprintf(\"Package '%%s' is required but not installed.\", pkg))
          }
          library(pkg, character.only = TRUE)
        }


        # Convert input to data frame if needed
        if (!is.data.frame(data_raw)) {
          data_raw <- as.data.frame(data_raw)
        }

        # Implement feature engineering steps here

        return(data_engineered)
      }
      ```

      Best Practices and Error Preventions
      - Validate input structure early: Ensure input is a data.frame or coercible to one, and fail fast if not.
      - Handle missing values before applying transformations like factor conversion or scaling:
      - Use appropriate imputation strategies for numeric (mean, median, etc.) and categorical (mode, etc.) features.
      - Convert logical (Boolean) values to integers (0/1) early, especially before modeling or encoding steps.
      - Avoid introducing highly correlated features (e.g., redundant dummies or engineered variables) unless explicitly required.
      - Exclude identifier fields (e.g., ID, record_id) from transformations like factor conversion or normalization.
      - Avoid transforming constant features, as they provide no predictive power.
      - Include clear, consistent comments to explain each transformation step for reproducibility and team clarity.
      - Design transformation pipelines to be idempotent (i.e., repeatable with consistent results on the same input).
      - Use tryCatch() and defensive programming to catch and explain common user errors early.
      - Document optional transformations, such as handling of missing values, for flexible customization.

      Return only the R function code wrapped in triple backticks.",
      recommended_steps, target_variable, all_datasets_summary
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
      feature_engineer_function = code_extracted,
      feature_engineer_function_path = NULL,
      feature_engineer_function_name = "feature_engineer"
    )
  }
}

node_execute_feature_engineering_code <- function(state) {
  cat("    * EXECUTING FEATURE ENGINEERING CODE\n")

  # 1. Package Management
  required_packages <- c(
    # Tidyverse core
    "dplyr", "tidyr", "purrr", "stringr",
    "lubridate", "forcats", "tibble", "magrittr",
    # Feature engineering
    "recipes", "rsample", "yardstick",
    # Additional utilities
    "glue", "stats"
  )

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
    pattern_func <- "(#s)(feature_engineer\\s*<-\\s*function\\s*\\([^\\)]*\\)\\s*\\{.*\\})"
    matches_func <- regmatches(text, regexec(pattern_func, text, perl = TRUE))
    if (length(matches_func) > 0 && length(matches_func[[1]]) >= 2 && nzchar(matches_func[[1]][2])) {
      return(trimws(matches_func[[1]][2]))
    }

    warning("Could not extract valid R code from text. Using raw text as code.")
    return(trimws(text))
  }

  # 3. Input Validation
  if (is.null(state$feature_engineer_function)) {
    stop("State is missing feature_engineer_function")
  }

  if (is.null(state$data_raw)) {
    stop("State is missing input data (data_raw is NULL)")
  }

  code_snippet <- extract_r_code_block(state$feature_engineer_function)
  if (is.null(code_snippet) || nchar(code_snippet) == 0) {
    stop("No R code could be extracted from the feature engineer function")
  }

  if (!grepl("feature_engineer\\s*<-\\s*function", code_snippet)) {
    stop("No valid 'feature_engineer' function detected in the extracted code.")
  }

  # 4. Execution Environment
  #local_env <- new.env(parent = baseenv())
  local_env <- new.env(parent = .GlobalEnv)  # Changed from baseenv() to .GlobalEnv


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

    if (!exists("feature_engineer", envir = local_env) || !is.function(local_env$feature_engineer)) {
      stop("'feature_engineer' function not found or invalid")
    }

    # Prepare input data
    input_data <- if (is.data.frame(state$data_raw)) {
      state$data_raw
    } else {
      as.data.frame(state$data_raw)
    }

    # Execute with proper error handling
    res <- tryCatch(
      local_env$feature_engineer(input_data),
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
    agent_error <<- paste("Feature engineering failed:", e$message)
    cat("ERROR:", agent_error, "\n")
  })

  # 6. Return Results
  list(
    data_engineered = result,
    feature_engineer_error = agent_error,
    execution_success = is.null(agent_error),
    timestamp = Sys.time()
  )
}

node_fix_feature_engineering_code <- function(model) {
  function(state) {
    cat("    * FIX FEATURE ENGINEERING CODE\n")
    cat("      retry_count:", state$retry_count, "\n")

    code_snippet <- state$feature_engineer_function
    error_message <- state$feature_engineer_error
    function_name <- "feature_engineer"

    prompt <- sprintf(
      "You are a Feature Engineering Agent. Your job is to fix the feature_engineer() function that currently contains errors.

      Make sure to only return the function definition for feature_engineer().

      Return R code in ```r``` format with a single function definition, feature_engineer(data_raw), that includes all imports inside the function.

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
      feature_engineer_function = new_code,
      feature_engineer_error = NULL,
      retry_count = new_retry_val
    )
  }
}

node_explain_feature_engineering_code <- function(model) {
  function(state) {
    summary <- if (!is.null(state$feature_engineer_error)) {
      paste("Error occurred:", state$feature_engineer_error)
    } else {
      get_dataframe_summary(state$data_engineered, skip_stats = TRUE)
    }

    prompt <- sprintf(
      "Explain these feature engineering transformations:\nSteps: %s\n\nResult:\n%s",
      state$recommended_steps, summary
    )

    explanation <- model(prompt)

    list(
      feature_engineering_report = explanation,
      data_summary = summary
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
## FEATURE ENGINEERING AGENT IMPLEMENTATION
###############################################################################
#' @importFrom pdftools pdf_text
#' @importFrom officer read_pptx docx_summary
#' @importFrom purrr map map_chr
#' @importFrom glue glue
#' @import DBI
NULL


# ------------------------------------------------------------------------------
#' Build a Feature Engineering Agent
#'
#' Constructs a graph-based feature engineering agent that guides the process of:
#' recommending, generating, executing, fixing, and explaining feature engineering code.
#'
#' @name build_feature_engineering_agent
#' @param model A function that accepts a prompt and returns an LLM-generated response.
#' @param human_validation Logical; include a manual review node before code execution.
#' @param bypass_recommended_steps Logical; skip the LLM-based recommendation phase.
#' @param bypass_explain_code Logical; skip final explanation step.
#'
#' @return A callable agent function that executes feature engineering via a state graph.
#' @examples
#' \dontrun{
#' state <- list(
#'   data_raw = iris,
#'   target_variable = "Species"
#' )
#' agent <- build_feature_engineering_agent(model = call_llm)
#' agent(state)
#' str(state$data_engineered)
#' }
#' @export
NULL


build_feature_engineering_agent <- function(
    model,
    human_validation = FALSE,
    bypass_recommended_steps = FALSE,
    bypass_explain_code = FALSE) {

  # Define node functions list
  node_functions <- list(
    recommend_feature_engineering_steps = node_recommend_feature_engineering_steps(model),
    human_review = node_func_human_review(
      prompt_text = "Are the following feature engineering instructions correct# (Answer 'yes' or provide modifications)\n%s",
      yes_goto = if (!bypass_explain_code) "explain_feature_engineering_code" else "__end__",
      no_goto = "recommend_feature_engineering_steps",
      user_instructions_key = "user_instructions",
      recommended_steps_key = "recommended_steps"
    ),
    create_feature_engineering_code = node_create_feature_engineering_code(
      model = model,
      bypass_recommended_steps = bypass_recommended_steps
    ),
    execute_feature_engineering_code = node_execute_feature_engineering_code,
    fix_feature_engineering_code = node_fix_feature_engineering_code(model),
    explain_feature_engineering_code = node_explain_feature_engineering_code(model)
  )

  # Create the agent graph
  app <- create_coding_agent_graph(
    node_functions = node_functions,
    recommended_steps_node_name = "recommend_feature_engineering_steps",
    create_code_node_name = "create_feature_engineering_code",
    execute_code_node_name = "execute_feature_engineering_code",
    fix_code_node_name = "fix_feature_engineering_code",
    explain_code_node_name = "explain_feature_engineering_code",
    error_key = "feature_engineer_error",
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
