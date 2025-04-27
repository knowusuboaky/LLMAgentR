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
    if (state$verbose) message("---FEATURE ENGINEERING AGENT----\n")
    if (state$verbose) message("    * RECOMMEND FEATURE ENGINEERING STEPS\n")


    # 1. Packages
    # Ensure all required packages from Suggests are installed
    required_packages <- c(
      "stringr", "dplyr", "tidyr", "purrr", "lubridate", "forcats",
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

        # extreme outliers (3 X IQR around median)
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
      "You are a Feature Engineering Expert. Given the following information about the data,
      recommend a series of numbered steps to take to engineer features.
      The steps should be tailored to the data characteristics and should be helpful
      for a feature engineering agent that will be implemented.

      General Steps:
      Things that should be considered in the feature engineering steps:

      - Convert features to the appropriate data types based on their sample data values
      - Remove string or categorical features with unique values equal to the size of the dataset
      - Remove constant features with the same value in all rows
      - High cardinality categorical features should be encoded by a threshold <= 5 percent of the dataset, by converting infrequent values to \"other\"
      - Encoding categorical variables using OneHotEncoding
      - Numeric features should be left untransformed
      - Create datetime-based features if datetime columns are present
      - If a target variable is provided:
          - If a categorical target variable is provided, encode it using LabelEncoding
          - All other target variables should be converted to numeric and unscaled
      - Convert any Boolean (True/False) values to integer (1/0) values. This should be performed after one-hot encoding.

      Custom Steps:
      - Analyze the data to determine if any additional feature engineering steps are needed.
      - Recommend steps that are specific to the data provided. Include why these steps are necessary or beneficial.
      - If no additional steps are needed, simply state that no additional steps are required.

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


node_create_feature_engineering_code <- function(model,
                                                 bypass_recommended_steps = FALSE) {
  function(state) {
    if (bypass_recommended_steps && isTRUE(state$verbose))
      message("---FEATURE ENGINEERING AGENT----")
    if (isTRUE(state$verbose))
      message("    * CREATE FEATURE ENGINEERING CODE")

    #  Gather state
    recommended_steps    <- state$recommended_steps    %||% ""
    all_datasets_summary <- state$all_datasets_summary %||% "No dataset summary."
    target_variable      <- state$target_variable      %||% "None provided"
    user_instructions    <- state$user_instructions %||% ""

    #  Prompt (built with paste0 to avoid %% issues)
    prompt <- paste0(
      "\nROLE: Principal Feature-Engineering Architect\n\n",

      "=========================== DATA CONTEXT ============================\n",
      "Target variable: ", target_variable, "\n\n",
      all_datasets_summary, "\n",
      "======================== RECOMMENDED STEPS ==========================\n",
      recommended_steps, "\n",
      "=====================================================================\n\n",

      "OBJECTIVE\n",
      "Return ONE fenced R block containing\n",
      "  clean_names(), safe_select(), safe_dummy_cols(), safe_lump_high_card(),\n",
      "  feature_engineer(data_raw) and nothing else.\n\n",

      "==================== NON-NEGOTIABLE REQUIREMENTS ====================\n",
      "1. Pure: exactly one data.frame; no printing or I/O.\n",
      "2. All packages loaded INSIDE feature_engineer().\n",
      "3. Script parses via source(textConnection(<output>)).\n",
      "4. No placeholders; helpers fully implemented.\n",
      "5. tryCatch() around risky blocks; propagate via stop().\n",
      "6. Idempotent output.\n",
      "7. Lint-free under lintr::default_linters.\n",
      "8. Tidyverse style (snake_case, <-, 2-space indent).\n",
      "9. Explicit namespaces.\n",
      "10. At least one stopifnot().\n",
      "11. Memory-safe for nrow > 1e6 (data.table := if needed).\n",
      "12. One-hot encode EVERY factor/character column, always.\n",
      "13. PCA forbidden no PC1/PC2/PC3.\n",
      "14. Column names ->  clean_names()  after all encoding.\n",
      "15. Helpers + main function MUST be in the same code fence.\n\n",

      "=========================== CODE TEMPLATE ===========================\n",
      "```r\n",
      "# --- Helper: clean_names (safe ASCII, keeps words) ---------------\n",
      "clean_names <- function(x) {\n",
      "  x <- make.names(x, unique = TRUE)\n",
      "  x <- iconv(x, to = 'ASCII', sub = '_')\n",
      "  x <- gsub('\\u002E', '_', x, fixed = TRUE)   # literal dot -> underscore\n",
      "  x <- gsub('_+', '_', x)                      # collapse runs of '_'\n",
      "  x <- sub('^_+', '', sub('_+$', '', x))       # trim edges\n",
      "  x\n",
      "}\n\n",
      "# --- Helper: safe_select -----------------------------------------\n",
      "safe_select <- function(df, cols) {\n",
      "  if (!is.data.frame(df)) df <- as.data.frame(df)\n",
      "  cols <- intersect(cols, names(df))\n",
      "  if (length(cols) == 0) return(df)\n",
      "  dplyr::select(df, dplyr::all_of(cols))\n",
      "}\n\n",
      "# --- Helper: safe_dummy_cols -------------------------------------\n",
      "safe_dummy_cols <- function(df, cols, ...) {\n",
      "  cols <- intersect(cols, names(df))\n",
      "  if (length(cols) == 0) return(df)\n",
      "  fastDummies::dummy_cols(df, select_columns = cols, ...)\n",
      "}\n\n",
      "# --- Helper: safe_lump_high_card ---------------------------------\n",
      "safe_lump_high_card <- function(df, thresh = 0.05) {\n",
      "  for (col in names(Filter(is.factor, df))) {\n",
      "    tot <- nrow(df)\n",
      "    freq <- dplyr::count(df, !!rlang::sym(col), name = 'n')\n",
      "    drop <- freq[[1]][ freq[['n']] / tot < thresh ]\n",
      "    if (length(drop) > 0) {\n",
      "      df[[col]] <- forcats::fct_other(df[[col]], keep = setdiff(levels(df[[col]]), drop))\n",
      "    }\n",
      "  }\n",
      "  df\n",
      "}\n\n",
      "# --- Main: feature_engineer --------------------------------------\n",
      "feature_engineer <- function(data_raw) {\n",
      "  ## 0  Packages --------------------------------------------------\n",
      "  pkgs <- c('dplyr','tidyr','stringr','forcats','lubridate',\n",
      "            'fastDummies','tibble','magrittr','purrr','stats','data.table')\n",
      "  sapply(pkgs, function(p) {\n",
      "    if (!requireNamespace(p, quietly = TRUE))\n",
      "      stop(sprintf('Package \"%s\" required but not installed', p))\n",
      "    library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)\n",
      "  })\n\n",
      "  ## 1  Coerce ----------------------------------------------------\n",
      "  if (!is.data.frame(data_raw)) data_raw <- as.data.frame(data_raw)\n\n",
      "  ## 2  Characters -> factors ------------------------------------\n",
      "  chr <- names(Filter(is.character, data_raw))\n",
      "  data_raw[chr] <- lapply(data_raw[chr], forcats::as_factor)\n\n",
      "  ## 3  Lump high-cardinality ------------------------------------\n",
      "  data_raw <- safe_lump_high_card(data_raw, 0.05)\n\n",
      "  ## 4  One-hot encode all factors -------------------------------\n",
      "  fac <- names(Filter(is.factor, data_raw))\n",
      "  data_raw <- safe_dummy_cols(data_raw, fac,\n",
      "                              remove_first_dummy = FALSE,\n",
      "                              remove_selected_columns = TRUE)\n",
      "  data_raw <- as.data.frame(data_raw)\n",
      "  names(data_raw) <- clean_names(names(data_raw))\n\n",
      "  ## 5  Datetime enrichment -------------------------------------\n",
      "  dt <- names(Filter(function(x) inherits(x, c('Date','POSIXct','POSIXlt')), data_raw))\n",
      "  for (col in dt) {\n",
      "    data_raw <- dplyr::mutate(data_raw,\n",
      "      !!paste0(col,'_year')  := lubridate::year(.data[[col]]),\n",
      "      !!paste0(col,'_month') := lubridate::month(.data[[col]]),\n",
      "      !!paste0(col,'_wday')  := lubridate::wday(.data[[col]]))\n",
      "  }\n\n",
      "  ## 6  Drop constants & IDs -------------------------------------\n",
      "  const <- names(Filter(function(x) dplyr::n_distinct(x, na.rm = TRUE) == 1, data_raw))\n",
      "  ids   <- names(Filter(function(x) dplyr::n_distinct(x, na.rm = TRUE) == nrow(data_raw), data_raw))\n",
      "  data_raw <- safe_select(data_raw, setdiff(names(data_raw), c(constant = const, ids = ids)))\n\n",
      "  ## 7  Checks ----------------------------------------------------\n",
      "  stopifnot(is.data.frame(data_raw), nrow(data_raw) > 0, ncol(data_raw) > 0)\n",
      "  if (identical(Sys.getenv('FE_TEST'), '1')) message('Self-test OK')\n",
      "  data_raw\n",
      "}\n",
      "```\n\n",

      "=========================== OUTPUT FORMAT ===========================\n",
      "Return ONE fenced R block (no prose) with all four functions above.\n",
      "Must be ASCII-only; PCA strictly banned; one-hot encoding mandatory.\n"
    )

    #   LLM CALL
    code_raw <- model(prompt)

    # Extract fenced R code
    pat <- "(?s)```\\s*r?\\s*\\n(.*?)\\n```"
    hit <- regexec(pat, code_raw, perl = TRUE)
    cap <- regmatches(code_raw, hit)
    code_extracted <- if (length(cap) && length(cap[[1]]) >= 2) {
      trimws(cap[[1]][2])
    } else {
      trimws(code_raw)
    }
    code_extracted <- gsub("^```.*$", "", code_extracted)
    code_extracted <- gsub("```$",     "", code_extracted)

    list(
      feature_engineer_function      = code_extracted,
      feature_engineer_function_path = NULL,
      feature_engineer_function_name = "feature_engineer"
    )
  }
}

node_execute_feature_engineering_code <- function(state) {
  if (isTRUE(state$verbose))
    message("    * EXECUTING FEATURE ENGINEERING CODE")

  ## 1. Package management
  required_packages <- c(
    "dplyr","tidyr","purrr","stringr","lubridate","forcats","tibble","magrittr",
    "recipes","rsample","yardstick","glue","stats","fastDummies"
  )

  # Check for missing packages using requireNamespace
  missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    if (state$verbose) message("Missing packages: ", paste(missing_pkgs, collapse = ", "))
    return(list(feature_engineer_error = paste("Missing required packages:", paste(missing_pkgs, collapse = ", "))))
  }

  # Load packages silently
  suppressPackageStartupMessages({
    invisible(lapply(required_packages, require, character.only = TRUE))
  })

  ## 2. Helper: pull fenced R code
  extract_r_code_block <- function(txt) {
    pat <- "(?s)```\\s*r?\\s*\\n(.*?)\\n```"
    m   <- regmatches(txt, regexec(pat, txt, perl = TRUE))[[1]]
    if (length(m) >= 2 && nzchar(m[2])) return(trimws(m[2]))
    if (grepl("feature_engineer\\s*<-\\s*function", txt)) return(trimws(txt))
    NULL
  }

  if (is.null(state$feature_engineer_function))
    stop("state$feature_engineer_function is NULL")
  if (is.null(state$data_raw))
    stop("state$data_raw is NULL")

  code_snippet <- extract_r_code_block(state$feature_engineer_function)
  if (is.null(code_snippet))
    stop("Could not locate a fenced code block containing feature_engineer()")
  if (!grepl("feature_engineer\\s*<-\\s*function", code_snippet))
    stop("The extracted block does not define feature_engineer()")

  ## 3. Build sandbox env & preload symbols
  exec_env       <- new.env(parent = baseenv())
  exec_env$`%>%` <- magrittr::`%>%`

  # load every *exported* object from each required package
  for (pkg in required_packages) {
    for (sym in getNamespaceExports(pkg)) {
      # skip S4 methods / weird objects that error on get()
      obj <- tryCatch(getExportedValue(pkg, sym), error = function(e) NULL)
      if (!is.null(obj)) exec_env[[sym]] <- obj
    }
  }

  ## 4. Parse, evaluate, run
  agent_error <- NULL
  engineered  <- NULL

  tryCatch({
    eval(parse(text = code_snippet), envir = exec_env)

    if (!exists("feature_engineer", envir = exec_env) ||
        !is.function(exec_env$feature_engineer))
      stop("feature_engineer() not found or is not a function after evaluation")

    # Coerce input to plain data.frame
    input_df <- state$data_raw
    if (!is.data.frame(input_df)) input_df <- as.data.frame(input_df)

    engineered <- withCallingHandlers(
      exec_env$feature_engineer(input_df),
      error = function(e) {
        stop("Execution failed inside feature_engineer(): ", e$message, call. = FALSE)
      }
    )

    # Sanity-check output
    if (!is.data.frame(engineered))
      stop("feature_engineer() returned an object of class ",
           paste(class(engineered), collapse = "/"), ", expected data.frame")
    if (nrow(engineered) == 0 || ncol(engineered) == 0)
      stop("feature_engineer() returned an empty data.frame")

  }, error = function(e) {
    agent_error <<- paste("Feature engineering failed:", e$message)
    if (isTRUE(state$verbose)) message("ERROR: ", agent_error)
  })

  ## 5. Preview & return
  if (isTRUE(state$verbose) && is.null(agent_error)) {
    message("      feature_engineer() succeeded; preview:")
  }

  list(
    data_engineered        = engineered,
    feature_engineer_error = agent_error,
    execution_success      = is.null(agent_error),
    timestamp              = Sys.time()
  )
}

node_fix_feature_engineering_code <- function(model) {

  # helper
  extract_code_block <- function(txt) {
    # 1. Look for a fenced block ```r ```
    pat  <- "(?s)```\\s*r?\\s*\\n(.*?)\\n```"
    hit  <- regexec(pat, txt, perl = TRUE)
    m    <- regmatches(txt, hit)
    if (length(m) > 0 && length(m[[1]]) >= 2 && nzchar(m[[1]][2]))
      return(trimws(m[[1]][2]))

    # 2. Fallback: first line that defines the function
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    start <- grep("^\\s*feature_engineer\\s*<-\\s*function", lines)
    if (length(start) > 0)
      return(trimws(paste(lines[start:length(lines)], collapse = "\n")))

    NULL
  }

  #  main closure (called by the graph)
  function(state) {
    if (state$verbose) {
      message("    * FIX FEATURE ENGINEERING CODE")
      message("      retry_count:", state$retry_count, "\n")
    }

    code_snippet   <- state$feature_engineer_function
    error_message  <- state$feature_engineer_error
    function_name  <- "feature_engineer"   # kept for consistency

    prompt <- sprintf(
      paste0(
        "You are a Feature-Engineering Fix Agent. The current feature_engineer() ",
        "function failed with the error shown below. Return ONE fenced R code block ",
        "that fully replaces the function and embeds the required helpers.\n\n",

        "===================== BROKEN CODE (please repair) =====================\n",
        "%s\n\n",
        "=========================== LAST ERROR ================================\n",
        "%s\n\n",

        "======================== HARD OUTPUT CONTRACT =========================\n",
        "- Return ONE R code fence only; no prose outside it.\n",
        "- Inside the fence define, IN THIS ORDER:\n",
        "    1. clean_names()\n",
        "    2. safe_select()\n",
        "    3. safe_dummy_cols()\n",
        "    4. safe_lump_high_card()\n",
        "    5. feature_engineer(data_raw)\n",
        "- feature_engineer() MUST:\n",
        "    * Do all package checks and library() calls internally.\n",
        "    * Accept either a single data.frame OR a list of data.frames.\n",
        "      If a list is given, apply itself to each element and return a list.\n",
        "    * Convert character columns to factor, lump levels with share <= 5%%\n",
        "      into 'Other', then ONE-HOT ENCODE EVERY FACTOR (remove_first_dummy = FALSE).\n",
        "    * Call clean_names() after dummying so column names are ASCII, dots\n",
        "      replaced by underscores, runs of '_' collapsed, edges trimmed.\n",
        "    * Drop constant columns (n_distinct == 1) and ID columns\n",
        "      (n_distinct == nrow).\n",
        "    * Wrap risky blocks in tryCatch(); on any error use\n",
        "        stop('Feature engineering failed: ', <msg>).\n",
        "    * Include clear comments for each non-trivial step.\n",
        "    * Contain at least one stopifnot() for invariants.\n",
        "    * Be idempotent (running twice on identical input yields identical output).\n",
        "- ABSOLUTELY NO PCA (no PC1/PC2/PC3, no prcomp()).\n",
        "- All identifiers and strings must be pure ASCII.\n\n",

        "===================== IMPLEMENTATION NOTES (helpful) ===================\n",
        "- clean_names() example skeleton:\n",
        "    clean_names <- function(x) {\n",
        "      x <- make.names(x, unique = TRUE)\n",
        "      x <- iconv(x, to = 'ASCII', sub = '_')\n",
        "      x <- gsub('.', '_', x, fixed = TRUE)\n",
        "      x <- gsub('_+', '_', x)\n",
        "      sub('^_+|_+$', '', x)\n",
        "    }\n",
        "- safe_dummy_cols() must NOT pass sep = '_' (older fastDummies lacks it).\n",
        "- For list input use out <- lapply(data_raw, feature_engineer_single).\n\n",

        "Now produce the single corrected R code block."
      ),
      code_snippet,
      error_message
    )

    # Single LLM call (avoid duplicates)
    llm_reply <- model(prompt)

    # Try to pull out the corrected code
    new_code <- extract_code_block(llm_reply)

    # If extraction failed or doesn't contain the function, keep the old code
    if (is.null(new_code) ||
        !grepl("feature_engineer\\s*<-\\s*function", new_code)) {
      warning("Could not extract a valid 'feature_engineer' definition; keeping previous code.")
      new_code <- code_snippet
    }

    # Strip stray back-ticks / fences, just in case
    new_code <- gsub("^```.*$", "", new_code)
    new_code <- gsub("```$",     "", new_code)
    new_code <- trimws(new_code)

    list(
      feature_engineer_function = new_code,
      feature_engineer_error    = NULL,                      # clear error so Execute retries
      retry_count               = (state$retry_count %||% 0) + 1
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
    if (state$verbose) message(" * HUMAN REVIEW\n")
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
#' @param verbose Logical; whether to print progress messages (default: TRUE)
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
    bypass_explain_code = FALSE,
    verbose = TRUE) {

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
    state$verbose <- if (!is.null(state$verbose)) state$verbose else verbose
    if (is.null(state$retry_count)) state$retry_count <- 0
    if (is.null(state$max_retries)) state$max_retries <- 3
    app(state)
  }
}

