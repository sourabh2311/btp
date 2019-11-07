---
id: reg 
title: Register Allocation 
sidebar_label: Register Allocation 
---

Now that we have made our interference graph, where each edge denotes that its endpoints must go in different registers, thus, our problem reduced into coloring our graph with `K` (# no. of available registers) colors such that no two end points have same colour. 

As of now this is implemented by below mentioned algorithm **but** later I would implement the exact algorithm which is given in book. Algorithm is implemented in the file [`regalloc.sml`](https://www.github.com/sourabh2311/btp/tree/master/Compiler/regalloc.sml) and [`regalloc.sig`](https://www.github.com/sourabh2311/btp/tree/master/Compiler/regalloc.sig)

Note that graph coloring problem is not fixed parameter tractable with respect to number of colors.

## Algorithm 

Recursive algorithm is defined as below, assume that we are given a set of instructions each of which may contain temporaries which might have been already mapped to some register.

1. Construct interference graph given the list of instructions. 
3. Start with vertices (temporaries) which have not been assigned a colour (register).
4. Such vertices whose # Neighbours $<$ `K` can be removed from the graph as we would always have a color available for them. Thus this divides our list of vertices into two viz., (`simplify`, `potentialSpill`).
5. Now we should process these nodes in `simplify`, let "`stack`" denote the set of simplified nodes.
6. Repeat until `potentialSpill` nodes become empty
   1. We "simplify" each node in our `simplify` list by removing it from `simplify` list and adding it to `stack` and decrementing the degree of its neighbors which are not in `stack` (`stack` nodes are already removed) and also which aren't already colored. Note that when we decrement the degree of the neighbors, if degree becomes less then `K` that means we can add this node to `simplify` and remove it from `potentialSpill`.
   2. If `potentialSpill` is empty then done, o/w select a node to spill, choose that which has maximum neighbors and should have accessed temps/memory least number of times. Assign it to `simplify` and remove it from `potentialSpill`.
7. Now process `stack` from top to bottom. For an element, colors available to it are total minus those taken by its neighbors, if colors are available then assign else we add it to actualSpill.
8. If by now actualSpill is empty then we stop else we rewrite the program and repeat. 