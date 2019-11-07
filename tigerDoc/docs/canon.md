---
id: canon
title: Canonisation
sidebar_label: Canonisation
header-includes: \usepackage{minted}
---

## Abstract

It's useful to be able to evaluate the sub-expressions of an expression in any  order.  If  tree  expressions  did  not  contain  `ESEQ`  and  `CALL`  nodes, then the order of evaluation would not matter.


### Why `CALL` nodes are an issue?

In actual implementation, `CALL` nodes will return value in the same register (`a0` in case of RISC V). Thus in an expression like `BINOP(PLUS, CALL(...), CALL(...))`; the second call will overwrite the `a0` register before the `PLUS` can be executed.

Remedy is to do the transformation; `CALL(fun, args) -> ESEQ(MOVE(TEMP t, CALL(fun, args)), TEMP t)`

### Why `ESEQ` nodes are an issue?

Clearly in case of simple `ESEQ(s, e)`, statement `s` can have direct or side effects on an expression `e`.

Remedy is as shown in below figure (basically lifting them higher and higher until they become `SEQ` nodes).

![ESEQ Removal](assets/canon1.png)


The transformation is done in three stages: First, a tree is rewritten into a  list  of  canonical  trees  without  `SEQ`  or  `ESEQ`  nodes;  then  this  list  is grouped  into  a  set  of  basic  blocks,  which  contain  no  internal  jumps  or labels;  then  the  basic  blocks  are  ordered  into  a  set  of  traces  in  which every `CJUMP` is immediately followed by its false label. This will become clear when seeing the well documented code.

File: [canon.sml](https://www.github.com/sourabh2311/btp/tree/master/Compiler/canon.sml)