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
## NODE FUNCTIONS FOR DATA WRANGLING (TRANSFORMATION FOCUS)
###############################################################################

node_recommend_wrangling_steps <- function(model) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  function(state) {
    if (isTRUE(state$verbose)) {
      message("---DATA WRANGLING AGENT----\n")
      message("    * RECOMMEND TRANSFORMATION STEPS\n")
    }

    user_instructions      <- state$user_instructions %||% ""
    recommended_steps_prev <- state$recommended_steps   %||% ""

    #  Gather data.frames into a named list
    raw <- state$data_raw
    if (is.data.frame(raw)) {
      data_list <- list(main = raw)
    } else if (is.list(raw)) {
      data_list <- raw
      if (is.null(names(data_list)) || any(names(data_list) == "")) {
        names(data_list) <- paste0("dataset_", seq_along(data_list))
      }
    } else {
      stop("`state$data_raw` must be a data.frame or list of data.frames")
    }

    #  Detect potential join-keys
    all_names <- unlist(lapply(data_list, names))
    name_counts <- sort(table(all_names), decreasing = TRUE)
    join_candidates <- names(name_counts)[name_counts >= 2]
    join_info <- if (length(join_candidates)) {
      paste0(
        "Potential join-keys:\n",
        paste0(
          "* `", join_candidates, "`: in ",
          sapply(join_candidates, function(col) {
            paste(names(Filter(function(df) col %in% names(df), data_list)), collapse = ", ")
          }),
          collapse = "\n"
        ),
        "\n\n"
      )
    } else {
      "No obvious join-keys found (no column in common between datasets).\n\n"
    }

    #  Build per-dataset & per-column summaries
    ds_summaries <- lapply(names(data_list), function(ds_name) {
      df     <- as.data.frame(data_list[[ds_name]])
      n_rows <- nrow(df)
      n_cols <- ncol(df)

      # Header
      header <- sprintf("**%s**  %d rows X %d cols", ds_name, n_rows, n_cols)

      # Column details
      col_lines <- lapply(names(df), function(col) {
        vec         <- df[[col]]
        type        <- class(vec)[1]
        n_miss      <- sum(is.na(vec))
        pct_miss    <- round(100 * n_miss / n_rows, 1)
        n_distinct  <- length(unique(vec))
        pct_unique  <- round(100 * n_distinct / n_rows, 1)

        base <- sprintf(
          "- `%s` (%s): miss%4d (%.1f%%), distinct%4d (%.1f%%)",
          col, type, n_miss, pct_miss, n_distinct, pct_unique
        )

        # Numeric: quartiles
        if (is.numeric(vec)) {
          qs <- quantile(vec, c(0, .25, .5, .75, 1), na.rm=TRUE)
          return(paste0(
            base,
            sprintf(", min/25/50/75/max=%.2f/%.2f/%.2f/%.2f/%.2f", qs[1], qs[2], qs[3], qs[4], qs[5])
          ))
        }

        # Date/time
        if (inherits(vec, c("Date","POSIXt"))) {
          rng <- range(vec, na.rm=TRUE)
          return(paste0(base, sprintf(", range=%s to %s", as.character(rng[1]), as.character(rng[2]))))
        }

        # Factor/char: top-3 levels
        if (is.factor(vec) || is.character(vec)) {
          tl <- sort(table(vec, useNA="no"), decreasing=TRUE)
          top3 <- head(tl,3)
          top_str <- paste(sprintf("%s(%d)", names(top3), as.integer(top3)), collapse=", ")
          return(paste0(base, ", top=", top_str))
        }

        # Logical
        if (is.logical(vec)) {
          t_ct <- sum(vec, na.rm=TRUE)
          f_ct <- sum(!vec, na.rm=TRUE)
          return(paste0(base, sprintf(", TRUE=%d, FALSE=%d", t_ct, f_ct)))
        }

        # Constant?
        if (n_distinct == 1) {
          return(paste0(base, ", CONSTANT"))
        }

        # Fallback
        return(base)
      })

      paste(c(header, col_lines), collapse = "\n")
    })

    all_datasets_summary <- paste0(
      join_info,
      paste(ds_summaries, collapse = "\n\n"),
      "\n"
    )

    #  Build enriched prompt
    prompt <- sprintf(
      "You are a Data Wrangling Expert. Given the data described below, recommend a series of *numbered* transformation steps.
Focus on:
  1. Use the **join-key candidates** to merge/join datasets
  2. Reshaping ideas (pivot_longer, pivot_wider, stacking)
  3. Feature engineering (aggregations, new flags)
  4. Type conversions & date handling
  5. Column renaming & reorganization

User instructions:
%s

Previously recommended steps (if any):
%s

Data structure & stats:
%s

**Return** a numbered list of concise, actionable wrangling steps (no code).",
      user_instructions,
      recommended_steps_prev,
      all_datasets_summary
    )

    #  Invoke LLM & return
    steps <- model(prompt)

    list(
      recommended_steps    = paste0("\nRecommended Wrangling Steps:\n", trimws(steps)),
      all_datasets_summary = all_datasets_summary
    )
  }
}

node_create_data_wrangler_code <- function(model,
                                           bypass_recommended_steps = FALSE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  function(state) {
    if (bypass_recommended_steps && isTRUE(state$verbose)) {
      message("---DATA WRANGLING AGENT----\n")
    }
    if (isTRUE(state$verbose)) {
      message("    * CREATE DATA WRANGLER CODE\n")
    }

    user_instructions    <- state$user_instructions %||% ""
    recommended_steps    <- state$recommended_steps   %||% ""
    all_datasets_summary <- state$all_datasets_summary %||% "No dataset summary available."

    prompt <- paste0(
      "You are a Data Wrangling Coding Agent specialized in transforming and reshaping raw data into an analysis-ready single tibble in R using tidyverse only.\n\n",

      "Please follow these guidelines and answer based on information from Step 1 - Step 3:\n\n",

      "1. **User Instructions for Data Wrangling:**\n",
      user_instructions, "\n\n",

      "2. **Recommended Steps for Data Wrangling:**\n",
      recommended_steps, "\n\n",

      "3. **Dataset Overview:**\n",
      all_datasets_summary, "\n\n",

      "4. **Namespace Requirement:**\n",
      "   - Always prefix functions with their namespace, e.g. `dplyr::filter()`, `tidyr::pivot_longer()`, `purrr::map()`, etc.\n\n",

      "5. **Function Requirements (all-in-one wrangler):**\n",
      "   - **Input validation:** if not a data.frame or list, stop with an error.\n",
      "   - **Normalize to list:** wrap a single data.frame into a list.\n",
      "   - **Ensure tibble** for each element: use `tibble::as_tibble()`.\n",
      "   - **Merge/join datasets** when common keys exist (e.g., `dplyr::left_join()`).\n",
      "   - **Merge columns:** combine related fields using `tidyr::unite()`.\n",
      "   - **Split columns:** separate composite fields with `tidyr::separate()`.\n",
      "   - **Handle duplicate columns:** remove or rename duplicates appropriately.\n\n",

      "6. **Important requirements:**\n",
      "   1. Include all necessary package checks and imports inside the function.\n",
      "   2. Handle both single dataframe and list of dataframes as input.\n",
      "   3. Comment all non-trivial steps clearly.\n",
      "   4. Follow the recommended steps provided.\n",
      "   5. Ensure the output is properly formatted with R code blocks.\n\n",


      "7. **Additional Flexibility & Quality:**\n",
      "   - Feel free to include any extra transformation steps, optimizations, robust error handling, and ensure the generated code is syntactically correct and error-free for production readiness.\n\n",

      "8. **Format:** Wrap the entire function in triple backticks tagged with `r`.\n\n",

      "9. **Expected Output Example:**\n",
      "```r\n",
      "data_wrangler <- function(data_list) {\n",
      "  # 1. Validate input\n",
      "  if (!(is.data.frame(data_list) || is.list(data_list))) stop(\"`data_list` must be a data.frame or list of data.frames.\")\n\n",
      "  # 2. Load packages with namespace checks\n",
      "  for (pkg in c(\"dplyr\",\"tidyr\",\"purrr\",\"magrittr\",\"stringr\",\"lubridate\",\"forcats\",\"janitor\",\"assertr\",\"data.table\",\"dtplyr\",\"recipes\",\"vtreat\")) {\n",
      "    if (!requireNamespace(pkg, quietly=TRUE)) stop(sprintf(\"Package '%s' is required.\", pkg))\n",
      "  }\n",
      "  suppressPackageStartupMessages({\n",
      "    library(dplyr); library(tidyr); library(purrr); library(magrittr)\n",
      "    library(stringr); library(lubridate); library(forcats)\n",
      "    library(janitor); library(assertr); library(data.table)\n",
      "    library(dtplyr); library(recipes); library(vtreat)\n",
      "  })\n\n",
      "  # 3. Normalize input to list of tibbles\n",
      "  if (is.data.frame(data_list)) data_list <- list(data_list)\n",
      "  data_list <- purrr::map(data_list, tibble::as_tibble)\n\n",
      "  # 4. Merge/join datasets by key\n",
      "  if (length(data_list) > 1) {\n",
      "    data_wrangled <- purrr::reduce(data_list, dplyr::left_join, by = \"id\")\n",
      "  } else {\n",
      "    data_wrangled <- data_list[[1]]\n",
      "  }\n\n",
      "  # 5. Merge columns: unite first_name + last_name -> full_name\n",
      "  data_wrangled <- tidyr::unite(data_wrangled, \"full_name\", first_name, last_name, sep = \" \")\n\n",
      "  # 6. Split columns: separate date_time -> date + time\n",
      "  data_wrangled <- tidyr::separate(data_wrangled, \"date_time\", into = c(\"date\",\"time\"), sep = \" \")\n\n",
      "  # 7. Reshape: pivot longer on year_ columns\n",
      "  data_wrangled <- data_wrangled %>% tidyr::pivot_longer(cols = starts_with(\"year_\"), names_to = \"year\", values_to = \"value\")\n\n",
      "  # 8. Feature engineering: compute total & monthly_avg\n",
      "  data_wrangled <- data_wrangled %>% dplyr::mutate(total = quantity * price, monthly_avg = total / 12)\n\n",
      "  # 9. String cleaning: trim & squish description\n",
      "  data_wrangled <- data_wrangled %>% dplyr::mutate(description = stringr::str_squish(stringr::str_trim(description)))\n\n",
      "  # 10. Date parsing: parse date column\n",
      "  data_wrangled <- data_wrangled %>% dplyr::mutate(date = lubridate::ymd(date))\n\n",
      "  # 11. Factor handling: lump infrequent 'category' levels\n",
      "  data_wrangled <- data_wrangled %>% dplyr::mutate(category = forcats::fct_lump(category, n = 5))\n\n",
      "  # 12. Clean names & validate with janitor & assertr\n",
      "  data_wrangled <- janitor::clean_names(data_wrangled)\n",
      "  assertr::assert(data_wrangled, is.numeric, columns = \"price\")\n\n",
      "  # 13. High-performance summary: data.table avg price by region\n",
      "  DT <- data.table::as.data.table(data_wrangled)\n",
      "  dt_summary <- DT[, .(avg_price = mean(price, na.rm = TRUE)), by = region]\n\n",
      "  # 14. Modeling prep: create & juice recipe\n",
      "  rec <- recipes::recipe(total ~ ., data = data_wrangled) %>%\n",
      "    recipes::step_log(total, base = 10) %>%\n",
      "    recipes::step_dummy(all_nominal())\n",
      "  data_wrangled <- recipes::prep(rec, training = data_wrangled) %>% recipes::juice()\n\n",
      "  # 15. Return final tibble\n",
      "  return(data_wrangled)\n",
      "}\n\n",
      "Your output should consist solely of the R function code wrapped in triple backticks as shown in the example.\n",
      "Please generate the function accordingly.",
      "```"
    )
    # -----------------------------------------------------------------
    # Generate the Code Using the Provided Model
    # -----------------------------------------------------------------
    #  LLM CALL
    code_raw <- model(prompt)

    #  Extract fenced R code
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
  if (isTRUE(state$verbose)) message("    * EXECUTING DATA WRANGLER CODE\n")

  # Improved code extraction function
  extract_r_code <- function(text) {
    if (is.null(text)) return(NULL)

    # Handle both ```r and ``` formats
    pattern <- "(?s)```(?:r\\n)?(.*?)```"
    matches <- regmatches(text, regexec(pattern, text, perl = TRUE))

    if (length(matches) > 0 && length(matches[[1]]) > 1) {
      code <- trimws(matches[[1]][2])
      if (nzchar(code)) return(code)
    }

    # Fallback: look for function definition
    if (grepl("data_wrangler\\s*<-\\s*function", text)) {
      return(trimws(text))
    }

    warning("No valid R code block found")
    return(NULL)
  }

  # Input validation
  if (is.null(state$data_wrangler_function)) {
    stop("State is missing data_wrangler_function")
  }

  code_snippet <- extract_r_code(state$data_wrangler_function)
  if (is.null(code_snippet)) {
    stop("No R code could be extracted from the data wrangler function")
  }

  # Package management
  required_packages <- c("dplyr", "tidyr", "purrr", "magrittr")

  # Check for missing packages using requireNamespace
  missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    if (state$verbose) message("Missing packages: ", paste(missing_pkgs, collapse = ", "))
    return(list(data_wrangler_error = paste("Missing required packages:", paste(missing_pkgs, collapse = ", "))))
  }

  # Load packages silently
  suppressPackageStartupMessages({
    invisible(lapply(required_packages, require, character.only = TRUE))
  })


  # Execution environment
  local_env <- new.env(parent = .GlobalEnv)
  local_env$`%>%` <- magrittr::`%>%`

  # Execute with error handling
  agent_error <- NULL
  result <- NULL

  tryCatch({
    # Parse and evaluate the code
    eval(parse(text = code_snippet), envir = local_env)

    if (!exists("data_wrangler", envir = local_env) ||
        !is.function(local_env$data_wrangler)) {
      stop("'data_wrangler' function not found or invalid")
    }

    # Prepare input data
    input_data <- if (!is.null(state$data_list)) {
      if (!is.list(state$data_list)) list(main = state$data_list) else state$data_list
    } else {
      if (is.data.frame(state$data_raw)) list(main = state$data_raw) else as.list(state$data_raw)
    }

    # Execute the function
    result <- local_env$data_wrangler(input_data)

    # Standardize output to data.frame
    if (!is.data.frame(result)) {
      result <- tryCatch(
        as.data.frame(result),
        error = function(e) stop("Output could not be converted to data.frame")
      )
    }

  }, error = function(e) {
    agent_error <<- paste("Data wrangling failed:", e$message)
    if (isTRUE(state$verbose)) message("ERROR:", agent_error, "\n")
  })

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
    if (isTRUE(state$verbose))     message("    * FIX DATA WRANGLER CODE\n")
    if (isTRUE(state$verbose))     message("      retry_count:", state$retry_count, "\n")

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
    if (isTRUE(state$verbose))     message(" * HUMAN REVIEW\n")
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
#' @param verbose Logical; whether to print progress messages (default: TRUE)
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
    bypass_explain_code = FALSE,
    verbose = TRUE) {

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
    state$verbose <- if (!is.null(state$verbose)) state$verbose else verbose
    if (is.null(state$retry_count)) state$retry_count <- 0
    if (is.null(state$max_retries)) state$max_retries <- 3
    app(state)
  }
}


