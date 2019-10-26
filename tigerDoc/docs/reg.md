---
id: reg 
title: Register Allocation 
sidebar_label: Register Allocation 
---

Now that we have made our interference graph, where each edge denotes that its endpoints must go in different registers, thus, our problem reduced into coloring our graph with K (# no. of available registers) colors such that no two end points have same colour. 

As of now this is implemented by below mentioned algorithm **but** later I would implement the exact algorithm which is given in book.

Note that graph coloring problem is not fixed parameter tractable with respect to number of colors.

## Algorithm 

Recursive algorithm is defined as below, assume that we are given a set of instructions each of which may contain temporaries which might have been already mapped to some register.

1. Construct interference graph given the list of instructions. 
3. Start with vertices (temporaries) which have not been assigned a colour (register).
5. Such vertices whose # Neighbours $<$ K can be removed from the graph as we would always have a color available for them. Thus this divides our list of vertices into two viz., (toRemove, toSpill). 
6. Once we are unable to do above reduction, select a node from the graph to spill such that it has minimum spill cost based on some heuristics. Now repeat above two steps.
7. Finally our graph would be empty. Those registers chosen to be spilled shouldn't be simply put in memory stack yet as we **may** find a color for them.