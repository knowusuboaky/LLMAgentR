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
  message("\n", value, "\n")
  readline("Enter your response: ")
}

make_command <- function(goto = NULL, update = list()) {
  list(goto = goto, update = update)
}

###############################################################################
## 3) NODE FUNCTIONS
###############################################################################

# Generate Data Frame Summary
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
## GENERIC GRAPH BUILDER
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

# Node: Recommend Visualization Steps
node_recommend_visualization_steps <- function(model, verbose = TRUE) {
  function(state) {
    if (verbose) {
      message("---DATA VISUALIZATION AGENT----")
      message("    * RECOMMEND VISUALIZATION STEPS")
    }

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


    # Convert the raw data into a data frame
    n_rows <- nrow(df)
    n_cols <- ncol(df)

    # Overall dataset counts
    dataset_info <- sprintf(
      "Dataset dimensions:\n* Rows: %d\n* Columns: %d\n\n",
      n_rows, n_cols
    )

    #  Build a super-detailed column-by-column summary
    col_summaries <- lapply(names(df), function(col) {
      vec           <- df[[col]]
      type          <- class(vec)[1]
      n_missing     <- sum(is.na(vec))
      pct_missing   <- round(100 * n_missing / n_rows, 2)
      distinct_vals <- unique(vec)
      n_distinct    <- length(distinct_vals)
      pct_unique    <- round(100 * n_distinct / n_rows, 2)

      base_info <- paste0(
        "* **", col, "** (", type, ")\n",
        "  - Missing: ", n_missing, " (", pct_missing, "%)\n",
        "  - Distinct: ", n_distinct, " (", pct_unique, "%)\n"
      )

      #  Numeric columns
      if (is.numeric(vec)) {
        qs      <- quantile(vec, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
        mn      <- mean(vec, na.rm = TRUE)
        sdv     <- sd(vec, na.rm = TRUE)
        iqr     <- IQR(vec, na.rm = TRUE)
        med_val <- qs[3]

        # zero/negative
        n_zero   <- sum(vec == 0, na.rm = TRUE)
        pct_zero <- round(100 * n_zero / n_rows, 2)
        n_neg    <- sum(vec < 0, na.rm = TRUE)
        pct_neg  <- round(100 * n_neg / n_rows, 2)

        # extreme outliers (3 * IQR around median)
        out_thresh  <- 3 * iqr
        n_extreme   <- sum((vec < (med_val - out_thresh) | vec > (med_val + out_thresh)), na.rm = TRUE)
        pct_extreme <- round(100 * n_extreme / n_rows, 2)

        # skewness & excess kurtosis
        skewness <- if (sdv > 0) mean((vec - mn)^3, na.rm = TRUE) / sdv^3 else NA
        kurtosis <- if (sdv > 0) mean((vec - mn)^4, na.rm = TRUE) / sdv^4 - 3 else NA

        num_info <- paste0(
          "  - Min/1st Qu./Median/Mean/3rd Qu./Max:\n",
          sprintf("    %.3f / %.3f / %.3f / %.3f / %.3f / %.3f\n",
                  qs[1], qs[2], med_val, mn, qs[4], qs[5]),
          "  - SD: ", round(sdv, 3), "  IQR: ", round(iqr, 3), "\n",
          "  - Zeros: ", n_zero, " (", pct_zero, "%)  Negatives: ", n_neg, " (", pct_neg, "%)\n",
          "  - Extreme (3 X IQR): ", n_extreme, " (", pct_extreme, "%)\n",
          "  - Skewness: ", round(skewness, 3), "  Excess Kurtosis: ", round(kurtosis, 3), "\n"
        )

        return(paste0(base_info, num_info))
      }

      #  Categorical (factor/character)
      if (is.factor(vec) || is.character(vec)) {
        tab_no_na <- table(vec, useNA = "no")
        freqs     <- prop.table(tab_no_na)
        entropy   <- -sum(freqs * log2(freqs), na.rm = TRUE)

        tab_sorted <- sort(tab_no_na, decreasing = TRUE)
        top_n      <- head(tab_sorted, 3)
        top_info   <- paste(
          sprintf("    %s: %d (%.1f%%)", names(top_n), as.integer(top_n), 100 * as.integer(top_n) / n_rows),
          collapse = "\n"
        )

        cat_info <- paste0(
          "  - Top levels:\n", top_info, "\n",
          "  - Entropy: ", round(entropy, 3), " bits\n"
        )
        return(paste0(base_info, cat_info))
      }

      #  Logical
      if (is.logical(vec)) {
        t_ct      <- sum(vec, na.rm = TRUE)
        f_ct      <- sum(!vec, na.rm = TRUE)
        pct_true  <- round(100 * t_ct / n_rows, 2)
        pct_false <- round(100 * f_ct / n_rows, 2)
        log_info  <- paste0(
          "  - TRUE: ", t_ct, " (", pct_true, "%)  FALSE: ", f_ct, " (", pct_false, "%)  NA: ", n_missing, "\n"
        )
        return(paste0(base_info, log_info))
      }

      #  Fallback for other types
      sample_vals <- if (n_distinct > 10) {
        paste(head(distinct_vals, 10), collapse = ", ")
      } else {
        paste(distinct_vals, collapse = ", ")
      }
      fallback_info <- paste0(
        "  - Sample values: ", sample_vals,
        if (n_distinct > 10) ", ..." else "", "\n"
      )
      paste0(base_info, fallback_info)
    })

    all_datasets_summary <- paste0(
      dataset_info,
      "These are the columns and their detailed statistics:\n\n",
      paste(col_summaries, collapse = "\n")
    )


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

# Node: Create Visualization Code
node_create_visualization_code <- function(model, function_name = "data_visualization",
                                           bypass_recommended_steps = FALSE, verbose = TRUE) {
  function(state) {
    if (bypass_recommended_steps && verbose) {
      message("---DATA VISUALIZATION AGENT----")
    }
    if (verbose) message("    * CREATE VISUALIZATION CODE")

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
      paste0(
        "You are a Chart-Generator Agent who writes production-grade R code that\n",
        "creates interactive visualizations with **plotly** (preferred) or\n",
        "**ggplot2** wrapped in ggplotly().\n\n",

        "========================  CHART INSTRUCTIONS  ========================\n",
        "%s\n\n",
        "===============================  DATA  ===============================\n",
        "%s\n",
        "======================================================================\n\n",

        "OBJECTIVE\n",
        "Write ONE fenced R code block that defines the function  %s(data_raw).\n",
        "The function must:\n",
        "  - Load required packages INSIDE the body (plotly, ggplot2, dplyr).\n",
        "  - Accept either a data.frame *or* a list of data.frames.\n",
        "    If a list is given, return a list of plotly objects (same order).\n",
        "  - Coerce non-data.frame input with as.data.frame().\n",
        "  - Contain stopifnot(is.data.frame(data_raw)) for the single-df path.\n",
        "  - Wrap risky code in tryCatch(); on failure stop('Chart failed: ', msg).\n",
        "  - Use only ASCII identifiers and strings (CRAN portable).\n",
        "  - Perform no file I/O (ggsave, write.csv, etc.).\n",
        "  - Return the plotly object(s) as the final expression.\n",
        "  - Include clear comments for each non-trivial step.\n",
        "  - Be idempotent (two identical runs -> identical plots).\n",
        "  - Everything must be inside ONE fenced R block; no prose outside.\n\n",

        "============================  CODE SKELETON  =========================\n",
        "```r\n",
        "%s <- function(data_raw) {\n",
        "  # 0. Packages ------------------------------------------------------\n",
        "  pkgs <- c('plotly', 'ggplot2', 'dplyr')\n",
        "  sapply(pkgs, function(p) {\n",
        "    if (!requireNamespace(p, quietly = TRUE))\n",
        "      stop(sprintf('Package \"%%s\" is required but not installed', p))\n",
        "    library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)\n",
        "  })\n\n",
        "  # Helper: build one chart for a single data.frame ------------------\n",
        "  build_one <- function(df) {\n",
        "    stopifnot(is.data.frame(df))\n",
        "    # === USER CHART LOGIC STARTS HERE ==============================\n",
        "    # Replace the example below with code that satisfies the\n",
        "    # CHART INSTRUCTIONS section above.  Must return a plotly object.\n",
        "    plt <- ggplot(df) +\n",
        "      geom_bar(aes(x = Churn, y = MonthlyCharges), stat = 'identity',\n",
        "               fill = 'steelblue') +\n",
        "      labs(title = 'Monthly Charges vs Churn') +\n",
        "      theme_minimal()\n",
        "    plotly::ggplotly(plt)\n",
        "    # === USER CHART LOGIC ENDS HERE ================================\n",
        "  }\n\n",
        "  # 1. Dispatch for single df vs list --------------------------------\n",
        "  if (is.list(data_raw) && !is.data.frame(data_raw)) {\n",
        "    return(lapply(data_raw, build_one))\n",
        "  } else {\n",
        "    return(build_one(as.data.frame(data_raw)))\n",
        "  }\n",
        "}\n",
        "```\n\n",

        "===========================  DO NOT DO  ==============================\n",
        "- Do NOT call ggsave(), write.csv(), or any other file I/O.\n",
        "- Do NOT add PCA or unrelated steps."
      ),
      chart_generator_instructions,   # %s  (first)
      all_datasets_summary,           # %s  (second)
      function_name,                  # %s  (third)  -> appears twice more
      function_name                   # %s  (fourth) -> inside code skeleton
    )

    # Print the resulting prompt
    code_raw <- model(prompt)

    # Extract R code from markdown
    regex_pattern <- "```r(.*?)```"
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

# Node: Execute Visualization Code
node_execute_visualization_code <- function(verbose = TRUE) {
  function(state) {
    if (verbose) message("    * EXECUTING VISUALIZATION CODE")

    # 1. Package Management
    required_packages <- c("plotly", "ggplot2", "dplyr")

    # Check for missing packages using requireNamespace
    missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

    if (length(missing_pkgs) > 0) {
      if (verbose) message("Missing packages: ", paste(missing_pkgs, collapse = ", "))
      return(list(visualization_error = paste("Missing required packages:", paste(missing_pkgs, collapse = ", "))))
    }

    # Load packages silently
    suppressPackageStartupMessages({
      invisible(lapply(required_packages, require, character.only = TRUE))
    })

    # 2. Code Extraction
    extract_r_code_block <- function(text) {
      if (is.null(text)) return(NULL)

      # Pattern 1: Triple backtick code blocks
      pattern_r <- "(?s)```(?:r)?\\s*(.*?)\\s*```"
      matches_r <- regmatches(text, regexec(pattern_r, text, perl = TRUE))
      if (length(matches_r) > 0 && length(matches_r[[1]]) >= 2 && nzchar(matches_r[[1]][2])) {
        return(trimws(matches_r[[1]][2]))
      }

      # Pattern 2: Direct function definition
      func_name <- state$visualization_function_name %||% "data_visualization"
      pattern_func <- sprintf("(?s)(%s\\s*<-\\s*function\\s*\\([^\\)]*\\)\\s*\\{.*\\})", func_name)
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
        error = function(e) stop("Execution failed: ", e$message))

    # Validate output
    if (!inherits(res, "plotly") && !inherits(res, "ggplot")) {
      warning("Visualization function did not return a plotly or ggplot object")
    }

    result <- res

    }, error = function(e) {
      agent_error <<- paste("Visualization failed:", e$message)
      if (verbose) message("ERROR:", agent_error)
    })

# 6. Return Results
list(
  visualization_result = result,
  visualization_error = agent_error,
  execution_success = is.null(agent_error),
  timestamp = Sys.time()
)
  }
}

# Node: Fix Visualization Code
node_fix_visualization_code <- function(model, verbose = TRUE) {
  function(state) {
    if (verbose) {
      message("    * FIX VISUALIZATION CODE")
      message("      retry_count:", state$retry_count)
    }

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

    regex_pattern <- "```r(.*?)```"
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

# Node: Explain Visualization Code
node_explain_visualization_code <- function(model, verbose = TRUE) {
  function(state) {
    if (verbose) message("    * EXPLAIN VISUALIZATION CODE")

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

# Node: Human Review
node_func_human_review <- function(
    prompt_text,
    yes_goto,
    no_goto,
    user_instructions_key = "user_instructions",
    recommended_steps_key = "recommended_steps",
    verbose = TRUE) {

  function(state) {
    if (verbose) message(" * HUMAN REVIEW")
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

#' Build Visualization Agent
#'
#' Creates a data visualization agent with configurable workflow steps.
#'
#' @name build_visualization_agent
#' @param model The AI model function to use for code generation
#' @param human_validation Whether to include human validation step (default: FALSE)
#' @param bypass_recommended_steps Skip recommendation step (default: FALSE)
#' @param bypass_explain_code Skip explanation step (default: FALSE)
#' @param function_name Name for generated visualization function (default: "data_visualization")
#' @param verbose Whether to print progress messages (default: TRUE)
#' @return A function that takes state and returns visualization results
#' @export
NULL

build_visualization_agent <- function(
    model,
    human_validation = FALSE,
    bypass_recommended_steps = FALSE,
    bypass_explain_code = FALSE,
    function_name = "data_visualization",
    verbose = TRUE) {

  # Define node functions list
  node_functions <- list(
    recommend_visualization_steps = node_recommend_visualization_steps(model, verbose),
    human_review = node_func_human_review(
      prompt_text = "Are the following visualization instructions correct? (Answer 'yes' or provide modifications)\n%s",
      yes_goto = if (!bypass_explain_code) "explain_visualization_code" else "__end__",
      no_goto = "recommend_visualization_steps",
      user_instructions_key = "user_instructions",
      recommended_steps_key = "recommended_steps",
      verbose = verbose
    ),
    create_visualization_code = node_create_visualization_code(
      model = model,
      function_name = function_name,
      bypass_recommended_steps = bypass_recommended_steps,
      verbose = verbose
    ),
    execute_visualization_code = node_execute_visualization_code(verbose),
    fix_visualization_code = node_fix_visualization_code(model, verbose),
    explain_visualization_code = node_explain_visualization_code(model, verbose)
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
