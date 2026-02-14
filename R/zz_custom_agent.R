#' State Graph Utilities for Custom Agents
#'
#' Lightweight graph primitives used by LLMAgentR's workflow agents.
#' These utilities are exported so users can build custom state-machine agents.
#'
#' @name state_graph_utils
NULL

#' Create a Graph Node Descriptor
#'
#' @rdname state_graph_utils
#' @param func A function that accepts a `state` list and returns either:
#'   1) a named list of state updates, or
#'   2) a command list created by [make_command()].
#' @param name Optional node name label.
#' @return A list with `func` and `name`.
#' @export
make_node <- function(func, name = NULL) {
  if (!is.function(func)) {
    stop("`func` must be a function.", call. = FALSE)
  }
  list(func = func, name = name)
}

#' Create a Graph Edge Descriptor
#'
#' @rdname state_graph_utils
#' @param from Source node name.
#' @param to Destination node name.
#' @param condition Optional function `function(state)` that returns a label used
#'   for conditional routing.
#' @param label Optional label matched against the value returned by `condition`.
#' @return A list with `from`, `to`, `condition`, and `label`.
#' @export
make_edge <- function(from, to, condition = NULL, label = NULL) {
  if (!is.null(condition) && !is.function(condition)) {
    stop("`condition` must be a function or NULL.", call. = FALSE)
  }
  list(from = from, to = to, condition = condition, label = label)
}

#' Create a Command Object
#'
#' @rdname state_graph_utils
#' @param goto Next node name to jump to.
#' @param update Named list of state fields to merge before jumping.
#' @return A command-like list with `goto` and `update`.
#' @export
make_command <- function(goto = NULL, update = list()) {
  if (!is.null(update) && !is.list(update)) {
    stop("`update` must be a list.", call. = FALSE)
  }
  list(goto = goto, update = update)
}

#' Ask for Human Input
#'
#' @rdname state_graph_utils
#' @param value Prompt text shown to the user.
#' @return A character string from `readline()`.
#' @export
interrupt <- function(value) {
  message("\n", value, "\n")
  readline("Enter your response: ")
}

#' Create a Mutable State Graph
#'
#' @rdname state_graph_utils
#' @return A list with methods:
#'   - `add_node(name, func)`
#'   - `add_edge(from, to)`
#'   - `add_conditional_edges(node_name, condition_fun, mapping_list)`
#'   - `set_entry_point(node_name)`
#'   - `compile(checkpointer = NULL)`
#'   - `END_NODE_NAME`
#' @export
StateGraph <- function() {
  graph_env <- new.env(parent = emptyenv())
  graph_env$nodes <- list()
  graph_env$edges <- list()
  graph_env$entry_point <- NULL

  graph_env$add_node <- function(name, func) {
    if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
      stop("Node `name` must be a non-empty string.", call. = FALSE)
    }
    graph_env$nodes[[name]] <- make_node(func, name)
  }

  graph_env$add_edge <- function(from, to) {
    graph_env$edges <- c(graph_env$edges, list(make_edge(from, to)))
  }

  graph_env$add_conditional_edges <- function(node_name, condition_fun, mapping_list) {
    if (!is.function(condition_fun)) {
      stop("`condition_fun` must be a function.", call. = FALSE)
    }
    if (!is.list(mapping_list) || is.null(names(mapping_list)) || any(names(mapping_list) == "")) {
      stop("`mapping_list` must be a named list.", call. = FALSE)
    }
    for (lbl in names(mapping_list)) {
      graph_env$edges <- c(graph_env$edges, list(
        make_edge(
          from = node_name,
          to = mapping_list[[lbl]],
          condition = condition_fun,
          label = lbl
        )
      ))
    }
  }

  graph_env$set_entry_point <- function(node_name) {
    if (!is.character(node_name) || length(node_name) != 1 || !nzchar(node_name)) {
      stop("`node_name` must be a non-empty string.", call. = FALSE)
    }
    graph_env$entry_point <- node_name
  }

  END_NODE_NAME <- "__end__"

  graph_env$compile <- function(checkpointer = NULL) {
    if (!is.null(checkpointer) && !is.function(checkpointer)) {
      stop("`checkpointer` must be a function or NULL.", call. = FALSE)
    }

    function(state) {
      if (is.null(state)) state <- list()
      if (!is.list(state)) {
        stop("`state` must be a list.", call. = FALSE)
      }
      if (is.null(graph_env$entry_point)) {
        stop("No entry point has been set.", call. = FALSE)
      }

      current_node <- if (!is.null(state$current_node)) state$current_node else graph_env$entry_point

      while (!identical(current_node, END_NODE_NAME)) {
        node_obj <- graph_env$nodes[[current_node]]
        if (is.null(node_obj)) {
          stop(sprintf("Node '%s' not found in graph.", current_node), call. = FALSE)
        }

        result <- node_obj$func(state)
        if (is.null(result)) result <- list()
        if (!is.list(result)) {
          stop(sprintf("Node '%s' must return a list or command object.", current_node), call. = FALSE)
        }

        if (length(result) > 0) {
          for (n in names(result)) {
            state[[n]] <- result[[n]]
          }
        }

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
          }

          current_node <- next_node
          if (!is.null(checkpointer)) checkpointer(state, current_node)
          next
        }

        edges_from_node <- Filter(function(e) e$from == current_node, graph_env$edges)
        if (length(edges_from_node) == 0) {
          current_node <- END_NODE_NAME
          break
        }

        if (length(edges_from_node) == 1 && is.null(edges_from_node[[1]]$condition)) {
          current_node <- edges_from_node[[1]]$to
          if (identical(current_node, END_NODE_NAME)) break
          if (!is.null(checkpointer)) checkpointer(state, current_node)
          next
        }

        chosen_label <- edges_from_node[[1]]$condition(state)

        edge_matched <- NULL
        for (e in edges_from_node) {
          if (!is.null(e$label) && identical(e$label, chosen_label)) {
            edge_matched <- e
            break
          }
        }
        if (is.null(edge_matched)) {
          stop(sprintf("No matching conditional edge found from node '%s'.", current_node), call. = FALSE)
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
    add_node = graph_env$add_node,
    add_edge = graph_env$add_edge,
    add_conditional_edges = graph_env$add_conditional_edges,
    set_entry_point = graph_env$set_entry_point,
    compile = graph_env$compile,
    END_NODE_NAME = END_NODE_NAME
  )
}

# Internal helpers -------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

parse_static_edges <- function(edges) {
  out <- list()
  if (length(edges) == 0) return(out)

  for (i in seq_along(edges)) {
    e <- edges[[i]]
    from <- NULL
    to <- NULL

    if (is.character(e) && length(e) == 2) {
      from <- e[[1]]
      to <- e[[2]]
    } else if (is.list(e) && !is.null(e$from) && !is.null(e$to)) {
      from <- e$from
      to <- e$to
    } else {
      stop(sprintf("Invalid edge at index %d. Use c('from','to') or list(from=..., to=...).", i), call. = FALSE)
    }

    out[[length(out) + 1]] <- list(
      from = as.character(from)[1],
      to = as.character(to)[1],
      conditional = FALSE,
      label = NULL
    )
  }

  out
}

parse_conditional_edges <- function(conditional_edges) {
  out <- list()
  if (length(conditional_edges) == 0) return(out)

  for (i in seq_along(conditional_edges)) {
    ce <- conditional_edges[[i]]
    if (!is.list(ce)) {
      stop(sprintf("Conditional edge %d must be a list.", i), call. = FALSE)
    }

    from <- ce$from %||% ce$node_name
    condition_fun <- ce$condition %||% ce$condition_fun
    mapping <- ce$mapping %||% ce$mapping_list

    if (is.null(from) || !nzchar(as.character(from)[1])) {
      stop(sprintf("Conditional edge %d is missing `from`/`node_name`.", i), call. = FALSE)
    }
    if (!is.function(condition_fun)) {
      stop(sprintf("Conditional edge %d has non-function `condition`.", i), call. = FALSE)
    }
    if (!is.list(mapping) || is.null(names(mapping)) || any(names(mapping) == "")) {
      stop(sprintf("Conditional edge %d has invalid `mapping` (must be named list).", i), call. = FALSE)
    }

    out[[length(out) + 1]] <- list(
      from = as.character(from)[1],
      condition_fun = condition_fun,
      mapping = mapping
    )
  }

  out
}

flatten_conditional_edges <- function(conditional_specs) {
  out <- list()
  if (length(conditional_specs) == 0) return(out)

  for (spec in conditional_specs) {
    for (lbl in names(spec$mapping)) {
      out[[length(out) + 1]] <- list(
        from = spec$from,
        to = as.character(spec$mapping[[lbl]])[1],
        conditional = TRUE,
        label = as.character(lbl)[1]
      )
    }
  }

  out
}

validate_referenced_nodes <- function(nodes, entry_point, static_edges, conditional_specs, end_node = "__end__") {
  allowed <- c(nodes, end_node)

  if (!(entry_point %in% nodes)) {
    stop(sprintf("`entry_point` '%s' is not in `node_functions`.", entry_point), call. = FALSE)
  }

  for (e in static_edges) {
    if (!(e$from %in% allowed)) {
      stop(sprintf("Edge source '%s' is not a known node.", e$from), call. = FALSE)
    }
    if (!(e$to %in% allowed)) {
      stop(sprintf("Edge target '%s' is not a known node.", e$to), call. = FALSE)
    }
  }

  for (spec in conditional_specs) {
    if (!(spec$from %in% allowed)) {
      stop(sprintf("Conditional edge source '%s' is not a known node.", spec$from), call. = FALSE)
    }
    for (lbl in names(spec$mapping)) {
      target <- as.character(spec$mapping[[lbl]])[1]
      if (!(target %in% allowed)) {
        stop(sprintf("Conditional edge target '%s' for label '%s' is not a known node.", target, lbl), call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

sanitize_mermaid_id <- function(x) {
  id <- gsub("[^A-Za-z0-9_]", "_", as.character(x), perl = TRUE)
  if (!nzchar(id)) id <- "node"
  if (!grepl("^[A-Za-z_]", id)) id <- paste0("n_", id)
  id
}

unique_ids <- function(values) {
  out <- character(length(values))
  seen <- list()

  for (i in seq_along(values)) {
    base <- sanitize_mermaid_id(values[[i]])
    count <- (seen[[base]] %||% 0L) + 1L
    seen[[base]] <- count
    out[[i]] <- if (count == 1L) base else paste0(base, "_", count)
  }

  out
}

escape_mermaid_label <- function(x) {
  y <- gsub("\"", "\\\\\"", as.character(x), fixed = TRUE)
  gsub("\\|", "/", y, perl = TRUE)
}

build_graph_spec <- function(nodes, entry_point, static_edges, conditional_specs, end_node = "__end__", subgraphs = NULL) {
  list(
    nodes = as.character(nodes),
    entry_point = as.character(entry_point)[1],
    edges = c(static_edges, flatten_conditional_edges(conditional_specs)),
    end_node = end_node,
    subgraphs = subgraphs
  )
}

mermaid_from_spec <- function(graph_spec, direction = "TD", include_start_end = TRUE, style = TRUE) {
  direction <- match.arg(direction, c("TD", "LR"))
  nodes <- graph_spec$nodes
  end_node <- graph_spec$end_node %||% "__end__"

  node_ids <- unique_ids(nodes)
  node_map <- as.list(node_ids)
  names(node_map) <- nodes

  start_id <- "llma_start"
  end_id <- "llma_end"

  lines <- c(sprintf("flowchart %s", direction))

  if (include_start_end) {
    lines <- c(lines, sprintf("  %s([\"__start__\"])", start_id))
    lines <- c(lines, sprintf("  %s([\"__end__\"])", end_id))
  }

  for (i in seq_along(nodes)) {
    lines <- c(lines, sprintf("  %s[\"%s\"]", node_ids[[i]], escape_mermaid_label(nodes[[i]])))
  }

  subgraphs <- graph_spec$subgraphs
  if (!is.null(subgraphs)) {
    if (!is.list(subgraphs) || is.null(names(subgraphs)) || any(names(subgraphs) == "")) {
      stop("`subgraphs` must be a named list of character vectors.", call. = FALSE)
    }

    sg_index <- 0L
    for (sg_name in names(subgraphs)) {
      sg_index <- sg_index + 1L
      sg_id <- paste0("sg_", sg_index)
      lines <- c(lines, sprintf("  subgraph %s[\"%s\"]", sg_id, escape_mermaid_label(sg_name)))
      for (node_name in as.character(subgraphs[[sg_name]])) {
        if (node_name %in% names(node_map)) {
          lines <- c(lines, sprintf("    %s", node_map[[node_name]]))
        }
      }
      lines <- c(lines, "  end")
    }
  }

  if (include_start_end && !is.null(graph_spec$entry_point) && graph_spec$entry_point %in% names(node_map)) {
    lines <- c(lines, sprintf("  %s --> %s", start_id, node_map[[graph_spec$entry_point]]))
  }

  edges <- graph_spec$edges %||% list()
  if (length(edges) > 0) {
    for (e in edges) {
      from_id <- if (include_start_end && identical(e$from, end_node)) end_id else node_map[[e$from]]
      to_id <- if (include_start_end && identical(e$to, end_node)) end_id else node_map[[e$to]]
      if (is.null(from_id) || is.null(to_id)) next

      if (isTRUE(e$conditional) && !is.null(e$label)) {
        lines <- c(lines, sprintf("  %s --|%s|--> %s", from_id, escape_mermaid_label(e$label), to_id))
      } else {
        lines <- c(lines, sprintf("  %s --> %s", from_id, to_id))
      }
    }
  }

  if (isTRUE(style)) {
    lines <- c(
      lines,
      "  classDef llma_start fill:#E8F5E9,stroke:#2E7D32,color:#1B5E20;",
      "  classDef llma_end fill:#FFEBEE,stroke:#C62828,color:#B71C1C;",
      "  classDef llma_node fill:#F8F9FA,stroke:#546E7A,color:#263238;"
    )

    if (include_start_end) {
      lines <- c(lines, sprintf("  class %s llma_start", start_id))
      lines <- c(lines, sprintf("  class %s llma_end", end_id))
    }
    if (length(node_ids) > 0) {
      lines <- c(lines, sprintf("  class %s llma_node", paste(node_ids, collapse = ",")))
    }
  }

  paste(lines, collapse = "\n")
}

#' Convert a Custom Graph Spec to Mermaid
#'
#' @param x Graph spec list (from `build_custom_agent(..., output = "both")$graph`)
#'   or a compiled object returned by `build_custom_agent(..., output = "both")`
#'   or [compile_graph()].
#' @param direction Mermaid direction: `"TD"` (top-down) or `"LR"` (left-right).
#' @param subgraphs Optional named list of subgraph groupings.
#' @param include_start_end Logical; include `__start__` and `__end__` nodes.
#' @param style Logical; include default LangGraph-like Mermaid styling.
#'
#' @return Mermaid flowchart text.
#' @export
as_mermaid <- function(
    x,
    direction = c("TD", "LR"),
    subgraphs = NULL,
    include_start_end = TRUE,
    style = TRUE) {
  direction <- match.arg(direction)

  graph_spec <- x
  if (is.list(x) && !is.null(x$graph)) {
    graph_spec <- x$graph
  }

  if (!is.list(graph_spec) || is.null(graph_spec$nodes) || is.null(graph_spec$edges) || is.null(graph_spec$entry_point)) {
    stop("`x` must be a graph spec list or compiled object with `$graph`.", call. = FALSE)
  }

  if (!is.null(subgraphs)) {
    graph_spec$subgraphs <- subgraphs
  }

  mermaid_from_spec(
    graph_spec = graph_spec,
    direction = direction,
    include_start_end = include_start_end,
    style = style
  )
}

#' Save Mermaid Diagram as PNG
#'
#' Render Mermaid text (or a compiled graph object) to a PNG file using
#' Mermaid CLI (`mmdc`).
#'
#' @param x Mermaid text, graph spec, or compiled object returned by
#'   [build_custom_agent()] with `output = "both"` or [compile_graph()].
#' @param file Output `.png` path.
#' @param mmdc Path to Mermaid CLI executable. Defaults to `Sys.which("mmdc")`.
#' @param direction Mermaid direction used when `x` is not plain Mermaid text.
#' @param subgraphs Optional named list of subgraph groupings.
#' @param include_start_end Logical; include `__start__` and `__end__` nodes.
#' @param style Logical; include default Mermaid class styling.
#' @param width Optional diagram width passed to `mmdc`.
#' @param height Optional diagram height passed to `mmdc`.
#' @param scale Optional diagram scale passed to `mmdc`.
#' @param background Background color for Mermaid rendering.
#' @param theme Mermaid theme (for example `"default"`, `"neutral"`, `"dark"`).
#' @param quiet Logical; suppress Mermaid CLI output when `TRUE`.
#'
#' @return Invisibly returns the output file path.
#' @export
save_mermaid_png <- function(
    x,
    file,
    mmdc = Sys.which("mmdc"),
    direction = c("TD", "LR"),
    subgraphs = NULL,
    include_start_end = TRUE,
    style = TRUE,
    width = NULL,
    height = NULL,
    scale = NULL,
    background = "white",
    theme = "default",
    quiet = TRUE) {

  direction <- match.arg(direction)

  if (!is.character(file) || length(file) != 1 || !nzchar(file)) {
    stop("`file` must be a non-empty file path.", call. = FALSE)
  }
  if (!nzchar(mmdc)) {
    stop(
      "Mermaid CLI (`mmdc`) was not found in PATH. Install it with `npm i -g @mermaid-js/mermaid-cli` ",
      "or provide the `mmdc` path explicitly.",
      call. = FALSE
    )
  }

  mermaid_text <- if (is.character(x)) {
    paste(x, collapse = "\n")
  } else {
    as_mermaid(
      x = x,
      direction = direction,
      subgraphs = subgraphs,
      include_start_end = include_start_end,
      style = style
    )
  }

  out_file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  out_dir <- dirname(out_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  input_file <- tempfile(fileext = ".mmd")
  on.exit(unlink(input_file, force = TRUE), add = TRUE)
  writeLines(mermaid_text, con = input_file, useBytes = TRUE)

  args <- c("-i", shQuote(input_file), "-o", shQuote(out_file))
  if (!is.null(width)) args <- c(args, "--width", shQuote(as.character(width)))
  if (!is.null(height)) args <- c(args, "--height", shQuote(as.character(height)))
  if (!is.null(scale)) args <- c(args, "--scale", shQuote(as.character(scale)))
  if (!is.null(background)) args <- c(args, "--backgroundColor", shQuote(as.character(background)))
  if (!is.null(theme)) args <- c(args, "--theme", shQuote(as.character(theme)))

  cmd_out <- tryCatch(
    system2(mmdc, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) e
  )

  if (inherits(cmd_out, "error")) {
    stop(sprintf("Failed to execute mmdc: %s", cmd_out$message), call. = FALSE)
  }

  if (!quiet && length(cmd_out) > 0) {
    cat(paste(cmd_out, collapse = "\n"), "\n")
  }

  status <- attr(cmd_out, "status")
  if (is.null(status)) status <- 0L

  if (!identical(as.integer(status), 0L) || !file.exists(out_file)) {
    detail <- if (length(cmd_out) > 0) paste(cmd_out, collapse = "\n") else "No CLI output captured."
    stop(sprintf("mmdc failed to render PNG.\n%s", detail), call. = FALSE)
  }

  invisible(out_file)
}

#' Build a Custom Graph-Based Agent
#'
#' Build a reusable agent by wiring user-defined node functions into a state graph.
#' This is the public extension point for creating custom LLMAgentR workflows.
#'
#' @param node_functions Named list of node functions. Each function takes `state`
#'   and returns a named list or [make_command()].
#' @param entry_point Name of the start node (must exist in `node_functions`).
#' @param edges Optional list of static edges. Each item can be:
#'   - a character vector of length 2: `c("from", "to")`, or
#'   - a list with `from` and `to`.
#' @param conditional_edges Optional list where each item contains:
#'   - `from` (or `node_name`),
#'   - `condition` (or `condition_fun`),
#'   - `mapping` (or `mapping_list`) as a named list of label -> node.
#' @param default_state Optional named list merged into `state` for missing keys.
#' @param checkpointer Optional callback `function(state, current_node)` executed
#'   after each transition.
#' @param output Output mode:
#'   - `"agent"` (default): return runnable agent function,
#'   - `"mermaid"`: return Mermaid text only,
#'   - `"both"`: return list with `run`, `graph`, and `mermaid`.
#' @param direction Mermaid direction used when `output` includes Mermaid.
#' @param subgraphs Optional named list of subgraph groups for Mermaid rendering.
#' @param style Logical; include default Mermaid class styling.
#'
#' @return
#' - If `output = "agent"`: a function that accepts `state` and returns final state.
#' - If `output = "mermaid"`: Mermaid flowchart text.
#' - If `output = "both"`: list with `run`, `graph`, and `mermaid`.
#'
#' @examples
#' \dontrun{
#' custom <- build_custom_agent(
#'   node_functions = list(
#'     start = function(state) make_command("classify"),
#'     classify = function(state) {
#'       if (grepl("weather", state$query, ignore.case = TRUE)) {
#'         make_command("weather")
#'       } else {
#'         make_command("general")
#'       }
#'     },
#'     weather = function(state) list(answer = "Routing to weather handler"),
#'     general = function(state) list(answer = "Routing to general handler")
#'   ),
#'   entry_point = "start",
#'   edges = list(c("weather", "__end__"), c("general", "__end__")),
#'   output = "both",
#'   subgraphs = list(
#'     Router = c("start", "classify"),
#'     Handlers = c("weather", "general")
#'   )
#' )
#'
#' cat(custom$mermaid)
#' custom$run(list(query = "weather in Accra"))
#' }
#'
#' @export
build_custom_agent <- function(
    node_functions,
    entry_point,
    edges = list(),
    conditional_edges = list(),
    default_state = list(),
    checkpointer = NULL,
    output = c("agent", "mermaid", "both"),
    direction = c("TD", "LR"),
    subgraphs = NULL,
    style = TRUE) {

  output <- match.arg(output)
  direction <- match.arg(direction)

  if (!is.list(node_functions) || is.null(names(node_functions)) || any(names(node_functions) == "")) {
    stop("`node_functions` must be a named list of functions.", call. = FALSE)
  }
  for (nm in names(node_functions)) {
    if (!is.function(node_functions[[nm]])) {
      stop(sprintf("Node '%s' is not a function.", nm), call. = FALSE)
    }
  }
  if (!is.character(entry_point) || length(entry_point) != 1 || !nzchar(entry_point)) {
    stop("`entry_point` must be a non-empty string.", call. = FALSE)
  }
  if (!is.list(default_state)) {
    stop("`default_state` must be a list.", call. = FALSE)
  }
  if (!is.null(checkpointer) && !is.function(checkpointer)) {
    stop("`checkpointer` must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(subgraphs)) {
    if (!is.list(subgraphs) || is.null(names(subgraphs)) || any(names(subgraphs) == "")) {
      stop("`subgraphs` must be a named list.", call. = FALSE)
    }
  }

  nodes <- names(node_functions)
  static_edges <- parse_static_edges(edges)
  conditional_specs <- parse_conditional_edges(conditional_edges)
  validate_referenced_nodes(nodes, entry_point, static_edges, conditional_specs, end_node = "__end__")

  graph <- StateGraph()
  for (nm in nodes) {
    graph$add_node(nm, node_functions[[nm]])
  }

  if (length(static_edges) > 0) {
    for (e in static_edges) {
      graph$add_edge(e$from, e$to)
    }
  }

  if (length(conditional_specs) > 0) {
    for (spec in conditional_specs) {
      graph$add_conditional_edges(
        node_name = spec$from,
        condition_fun = spec$condition_fun,
        mapping_list = spec$mapping
      )
    }
  }

  graph$set_entry_point(entry_point)
  app <- graph$compile(checkpointer = checkpointer)

  run_agent <- function(state = list()) {
    if (is.null(state)) state <- list()
    if (!is.list(state)) {
      stop("`state` must be a list.", call. = FALSE)
    }

    if (length(default_state) > 0) {
      for (nm in names(default_state)) {
        if (is.null(state[[nm]])) {
          state[[nm]] <- default_state[[nm]]
        }
      }
    }

    app(state)
  }

  graph_spec <- build_graph_spec(
    nodes = nodes,
    entry_point = entry_point,
    static_edges = static_edges,
    conditional_specs = conditional_specs,
    end_node = "__end__",
    subgraphs = subgraphs
  )

  if (identical(output, "agent")) {
    return(run_agent)
  }

  mermaid_text <- as_mermaid(
    graph_spec,
    direction = direction,
    include_start_end = TRUE,
    style = style
  )

  if (identical(output, "mermaid")) {
    return(mermaid_text)
  }

  list(
    run = run_agent,
    graph = graph_spec,
    mermaid = mermaid_text
  )
}

#' Compile a Custom Agent Graph (LangGraph-Style Output)
#'
#' Convenience wrapper around [build_custom_agent()] that returns both runnable
#' agent and Mermaid graph artifacts.
#'
#' @inheritParams build_custom_agent
#' @return A list with `run`, `graph`, and `mermaid`.
#' @export
compile_graph <- function(
    node_functions,
    entry_point,
    edges = list(),
    conditional_edges = list(),
    default_state = list(),
    checkpointer = NULL,
    direction = c("TD", "LR"),
    subgraphs = NULL,
    style = TRUE) {
  build_custom_agent(
    node_functions = node_functions,
    entry_point = entry_point,
    edges = edges,
    conditional_edges = conditional_edges,
    default_state = default_state,
    checkpointer = checkpointer,
    output = "both",
    direction = direction,
    subgraphs = subgraphs,
    style = style
  )
}

#' Build a Custom Multi-Agent Team (Supervisor Style)
#'
#' Build a supervisor-routed multi-agent workflow, similar to LangGraph team
#' orchestration. The supervisor chooses the next worker by setting `state$next`
#' to one of the worker names or `FINISH`.
#'
#' @param supervisor Function that accepts `state` and returns:
#'   - worker name (character scalar), or
#'   - list with `next`, optionally additional updates.
#' @param workers Named list of workers. Each worker can be:
#'   - a function that accepts `state` and returns a named list of updates, or
#'   - a compiled custom-agent object with a callable `$run` function (for
#'     example from [build_custom_agent()] with `output = "both"` or
#'     [compile_graph()]).
#' @param finish_token Character label used by the supervisor to terminate.
#' @param max_turns Maximum worker turns before forcing finish.
#' @param allow_repeat Logical; allow the same worker twice in a row.
#' @param worker_error_policy `"return_to_supervisor"` (default) or `"stop"`.
#' @param default_state Optional defaults merged into incoming `state`.
#' @param checkpointer Optional callback `function(state, current_node)`.
#' @param output Output mode:
#'   - `"agent"` (default): return runnable agent,
#'   - `"mermaid"`: return Mermaid text,
#'   - `"both"`: return list with `run`, `graph`, and `mermaid`.
#' @param direction Mermaid direction when `output` includes Mermaid.
#' @param subgraphs Optional Mermaid subgraph groups. If `NULL`, defaults to
#'   `list(Supervisor = "supervisor", Workers = names(workers))`.
#' @param style Logical; include default Mermaid class styling.
#'
#' @return
#' - If `output = "agent"`: runnable function(state).
#' - If `output = "mermaid"`: Mermaid text.
#' - If `output = "both"`: list with `run`, `graph`, and `mermaid`.
#'
#' @examples
#' \dontrun{
#' supervisor_fn <- function(state) {
#'   if (is.null(state$turn) || state$turn == 0) "Researcher" else "FINISH"
#' }
#'
#' workers <- list(
#'   Researcher = function(state) {
#'     list(result = "Research complete")
#'   },
#'   Writer = function(state) {
#'     list(result = "Draft complete")
#'   }
#' )
#'
#' team <- build_custom_multi_agent(
#'   supervisor = supervisor_fn,
#'   workers = workers,
#'   output = "both"
#' )
#'
#' cat(team$mermaid)
#' team$run(list())
#' }
#'
#' @export
build_custom_multi_agent <- function(
    supervisor,
    workers,
    finish_token = "FINISH",
    max_turns = 10L,
    allow_repeat = FALSE,
    worker_error_policy = c("return_to_supervisor", "stop"),
    default_state = list(),
    checkpointer = NULL,
    output = c("agent", "mermaid", "both"),
    direction = c("TD", "LR"),
    subgraphs = NULL,
    style = TRUE) {

  worker_error_policy <- match.arg(worker_error_policy)
  output <- match.arg(output)
  direction <- match.arg(direction)

  if (!is.function(supervisor)) {
    stop("`supervisor` must be a function.", call. = FALSE)
  }
  if (!is.list(workers) || is.null(names(workers)) || any(names(workers) == "")) {
    stop("`workers` must be a named list.", call. = FALSE)
  }
  worker_runners <- vector("list", length(workers))
  names(worker_runners) <- names(workers)
  for (nm in names(workers)) {
    w <- workers[[nm]]
    if (is.function(w)) {
      worker_runners[[nm]] <- w
      next
    }
    if (is.list(w) && is.function(w$run)) {
      worker_runners[[nm]] <- w$run
      next
    }
    stop(
      sprintf(
        "Worker '%s' must be a function or compiled agent object with a callable `$run`.",
        nm
      ),
      call. = FALSE
    )
  }
  if (!is.character(finish_token) || length(finish_token) != 1 || !nzchar(finish_token)) {
    stop("`finish_token` must be a non-empty string.", call. = FALSE)
  }
  if (!is.numeric(max_turns) || length(max_turns) != 1 || is.na(max_turns) || max_turns < 0) {
    stop("`max_turns` must be a non-negative number.", call. = FALSE)
  }
  if (!is.logical(allow_repeat) || length(allow_repeat) != 1 || is.na(allow_repeat)) {
    stop("`allow_repeat` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.list(default_state)) {
    stop("`default_state` must be a list.", call. = FALSE)
  }
  if (!is.null(checkpointer) && !is.function(checkpointer)) {
    stop("`checkpointer` must be a function or NULL.", call. = FALSE)
  }

  worker_names <- names(workers)
  if ("supervisor" %in% worker_names) {
    stop("`workers` cannot contain a node named 'supervisor'.", call. = FALSE)
  }
  if ("__end__" %in% worker_names) {
    stop("`workers` cannot contain a node named '__end__'.", call. = FALSE)
  }
  if (finish_token %in% worker_names) {
    stop("`finish_token` cannot be one of the worker names.", call. = FALSE)
  }

  supervisor_node <- function(state) {
    turn <- as.integer(state$turn %||% 0L)
    max_t <- as.integer(state$max_turns %||% max_turns)

    if (!is.na(max_t) && turn >= max_t) {
      return(list(`next` = finish_token, supervisor_note = "max_turns reached"))
    }

    raw <- supervisor(state)
    if (is.null(raw)) raw <- list()
    if (is.character(raw) && length(raw) >= 1) {
      raw <- list(`next` = as.character(raw)[1])
    }
    if (!is.list(raw)) {
      stop("`supervisor` must return a character or list.", call. = FALSE)
    }

    if (is.null(raw[["next"]]) && !is.null(raw$goto)) {
      raw[["next"]] <- raw$goto
    }
    if (is.list(raw$update)) {
      for (nm in names(raw$update)) raw[[nm]] <- raw$update[[nm]]
    }
    raw$goto <- NULL
    raw$update <- NULL

    next_worker <- as.character(raw[["next"]] %||% finish_token)[1]
    if (identical(next_worker, "__end__")) {
      next_worker <- finish_token
    }
    if (!(next_worker %in% c(worker_names, finish_token))) {
      raw$supervisor_note <- sprintf(
        "Unknown worker '%s'; using finish token '%s'.",
        next_worker, finish_token
      )
      next_worker <- finish_token
    }

    if (!allow_repeat && length(worker_names) > 1 &&
        next_worker %in% worker_names &&
        identical(as.character(state$last_worker %||% ""), next_worker) &&
        !isTRUE(state$allow_repeat_current)) {
      alt <- setdiff(worker_names, next_worker)
      if (length(alt) > 0) {
        raw$supervisor_note <- sprintf("Reassigning from repeated worker '%s' to '%s'.", next_worker, alt[[1]])
        next_worker <- alt[[1]]
      } else {
        next_worker <- finish_token
      }
    }

    raw[["next"]] <- next_worker
    raw
  }

  worker_nodes <- lapply(worker_names, function(worker_name) {
    worker_fn <- worker_runners[[worker_name]]

    function(state) {
      turn <- as.integer(state$turn %||% 0L)
      next_turn <- turn + 1L

      worker_result <- tryCatch(
        worker_fn(state),
        error = function(e) {
          if (identical(worker_error_policy, "stop")) {
            stop(e)
          }

          errors <- state$errors
          if (is.null(errors) || !is.list(errors)) errors <- list()
          errors[[length(errors) + 1L]] <- list(
            worker = worker_name,
            error = conditionMessage(e),
            turn = turn,
            timestamp = as.character(Sys.time())
          )

          return(list(
            errors = errors,
            last_error = conditionMessage(e)
          ))
        }
      )

      if (is.null(worker_result)) worker_result <- list()
      if (!is.list(worker_result)) {
        stop(sprintf("Worker '%s' must return a list.", worker_name), call. = FALSE)
      }

      worker_result$goto <- NULL
      worker_result$update <- NULL

      if (!is.null(worker_result$response) &&
          is.list(state$messages) &&
          is.null(worker_result$messages) &&
          is.list(worker_result$response)) {
        worker_result$messages <- c(state$messages, worker_result$response)
      }

      worker_result$last_worker <- worker_name
      worker_result$turn <- next_turn
      worker_result
    }
  })
  names(worker_nodes) <- worker_names

  node_functions <- c(list(supervisor = supervisor_node), worker_nodes)

  static_edges <- lapply(worker_names, function(w) c(w, "supervisor"))

  mapping <- as.list(c(worker_names, "__end__"))
  names(mapping) <- c(worker_names, finish_token)

  conditional_edges <- list(
    list(
      from = "supervisor",
      condition = function(state) {
        nxt <- as.character(state[["next"]] %||% finish_token)[1]
        if (identical(nxt, "__end__")) nxt <- finish_token
        if (nxt %in% c(worker_names, finish_token)) nxt else finish_token
      },
      mapping = mapping
    )
  )

  if (is.null(subgraphs)) {
    subgraphs <- list(
      Supervisor = "supervisor",
      Workers = worker_names
    )
  }

  multi_defaults <- c(
    list(
      `next` = NULL,
      last_worker = NULL,
      turn = 0L,
      max_turns = as.integer(max_turns),
      errors = list()
    ),
    default_state
  )

  build_custom_agent(
    node_functions = node_functions,
    entry_point = "supervisor",
    edges = static_edges,
    conditional_edges = conditional_edges,
    default_state = multi_defaults,
    checkpointer = checkpointer,
    output = output,
    direction = direction,
    subgraphs = subgraphs,
    style = style
  )
}
