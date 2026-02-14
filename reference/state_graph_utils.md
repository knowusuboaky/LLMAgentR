# State Graph Utilities for Custom Agents

Lightweight graph primitives used by LLMAgentR's workflow agents. These
utilities are exported so users can build custom state-machine agents.

## Usage

``` r
make_node(func, name = NULL)

make_edge(from, to, condition = NULL, label = NULL)

make_command(goto = NULL, update = list())

interrupt(value)

StateGraph()
```

## Arguments

- func:

  A function that accepts a `state` list and returns either: 1) a named
  list of state updates, or 2) a command list created by
  `make_command()`.

- name:

  Optional node name label.

- from:

  Source node name.

- to:

  Destination node name.

- condition:

  Optional function `function(state)` that returns a label used for
  conditional routing.

- label:

  Optional label matched against the value returned by `condition`.

- goto:

  Next node name to jump to.

- update:

  Named list of state fields to merge before jumping.

- value:

  Prompt text shown to the user.

## Value

`make_node()` returns a list with `func` and `name`.

`make_edge()` returns a list with `from`, `to`, `condition`, and
`label`.

`make_command()` returns a command-like list with `goto` and `update`.

`interrupt()` returns a character string from
[`readline()`](https://rdrr.io/r/base/readline.html).

`StateGraph()` returns a list with methods:

- `add_node(name, func)`

- `add_edge(from, to)`

- `add_conditional_edges(node_name, condition_fun, mapping_list)`

- `set_entry_point(node_name)`

- `compile(checkpointer = NULL)`

- `END_NODE_NAME`
