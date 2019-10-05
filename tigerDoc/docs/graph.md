---
id: graph 
title: Liveness Analysis and Interference Graph
sidebar_label: Liveness Analysis and Interference Graph
---

## Liveness Analysis

Two temporaries `a` and `b` can fit into the same register, if `a` and `b` are never "in use" at the same time.

We say a variable is live if it holds a value that may be needed in the future, so this analysis is called liveness analysis. To perform analyses on a program, it is often useful to make a control-flow graph. Each statement in the program is a node in the flow graph; if statement `x` can be followed by statement `y` there is an edge from `x` to `y`. 

In this compiler, flow graph is as represented in [`flowgraph.sml`](https://www.github.com/sourabh2311/btp/tree/master/Compiler/flowgraph.sml).

Similarly [`makegraph.sml`](https://www.github.com/sourabh2311/btp/tree/master/Compiler/flowgraph.sml) constructs this graph.

A variable is live on an edge if there is a directed path from that edge to a use of the variable that does not go through any `def`. A variable is live-in at a node if it is live on any of the in-edges of that node; it is live-out at a node if it is live on any of the out-edges of the node.

Liveness of node is calculated as shown:

$in[n] = use[n] \cup (out[n] \setminus def[n])$

$out[n] = \cup_{s \in succ[n]} in[s]$

A condition that prevents `a` and `b` being allocated to the same register is called an interference.

The most common kind of interference is caused by overlapping live ranges when `a` and `b` are both live at the same program point, then they cannot be put in the same register.

## Interference Graph

Interference graph is created with vertices as temporaries and edges as follows:-

1. At any nonmove instruction that defines a variable $a$, where the live-out variables are $b_1, \dots, b_j$, add interference edges $(a, b_1), \dots, (a, b_j)$.
2. At a move instruction $a \leftarrow c$, where variables $b_1, \dots, b_j$ are live-out, add interference edges $(a, b_1), \dots, (a, b_j)$ for any $b_i$ that is not the same as $c$.

---

All this is handled in [`liveness.sml`](https://www.github.com/sourabh2311/btp/tree/master/Compiler/liveness.sml)