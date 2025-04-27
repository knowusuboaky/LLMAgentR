#' State Graph Framework
#'
#' A lightweight framework for building state-driven workflows using nodes, edges,
#' and conditionally branching logic. Each node is a function that takes and mutates a shared `state` list.
#'
#' @section Functions:
#' \describe{
#'   \item{\code{make_node()}}{Wraps a function in a graph-compatible node.}
#'   \item{\code{make_edge()}}{Creates a transition between nodes, optionally conditional.}
#'   \item{\code{StateGraph()}}{Builds and compiles the state graph engine.}
#'   \item{\code{interrupt()}}{Prompts the user for input during execution.}
#'   \item{\code{make_command()}}{Signals the next node and state updates.}
#' }
#'
#' @name state_graph
#' @keywords state graph llm workflow
NULL

# Required libraries
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(plotly)

#' @describeIn state_graph Create a Graph Node
#' @param func A function that takes a `state` list and returns updates.
#' @param name An optional name for the node.
#' @return A list containing the function and optional name.
#' @export

make_node <- function(func, name = NULL) {
  list(func = func, name = name)
}

#' @describeIn state_graph Create a Graph Edge
#' @param from The source node name.
#' @param to The destination node name.
#' @param condition An optional function that returns a label based on the `state`.
#' @param label Optional label for conditional branching.
#' @return A list representing the edge.
#' @export

make_edge <- function(from, to, condition = NULL, label = NULL) {
  list(from = from, to = to, condition = condition, label = label)
}

#' @describeIn state_graph Build a State Graph Execution Engine
#' @return A list of graph methods: \code{add_node}, \code{add_edge},
#' \code{add_conditional_edges}, \code{set_entry_point}, \code{compile}, \code{END_NODE_NAME}
#'
#' @examples
#' graph <- StateGraph()
#' graph$add_node("start", function(state) list(goto = "end"))
#' graph$add_node("end", function(state) list())
#' graph$set_entry_point("start")
#' agent <- graph$compile()
#' state <- list()
#' agent(state)
#' @export

StateGraph <- function() {

  # Check for suggested packages
  invisible(lapply(
    c("dplyr", "purrr", "stringr", "ggplot2", "plotly"),
    get_suggested
  ))

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

        result <- node_obj$func(state)

        if (is.list(result)) {
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
          } else {
            current_node <- next_node
            if (!is.null(checkpointer)) {
              checkpointer(state, current_node)
            }
            next
          }
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

#' @describeIn state_graph Pause Execution for User Input
#' @param value A string to print before prompting the user.
#' @return A character response entered by the user.
#' @export

interrupt <- function(value) {
  message("\n", value, "\n")
  readline("Enter your response: ")
}

#' @describeIn state_graph Create a Graph Command Result
#' @param goto Name of the next node to transition to.
#' @param update A list of key-value updates to the `state`.
#' @return A list with `goto` and `update` keys.
#' @export

make_command <- function(goto = NULL, update = list()) {
  list(goto = goto, update = update)
}
