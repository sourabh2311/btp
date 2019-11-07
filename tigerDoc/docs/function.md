---
id: function 
title: Understanding Function Calls
sidebar_label: Understanding Function Calls
---


Below is explained in chronological sequence of what happens and for what reason when function call is executed in this compiler. 

1. When a function is called. The current frame is extended to include outgoing parameters (at the offset already determined by the callee) in case some of them escape; rest of the arguments are put in argument registers.
   1. This step is done by our code generator. 
   2. After moving escaped arguments to their pre-determined location and remaining arguments to argument registers, it will then emit `jal` instruction which will have `src = argTemps` (`argTemps` are chosen argument registers, this is done as these argument registers are used in this time) and `dst = F.getFirstL F.callersaves`; as we know that caller is supposed to save some registers if it was using them and thus setting `dst = F.getFirstL F.callersaves` would enable the garbage collection to know that if function being called **uses** these registers then it would clearly interfere.
   3. Let `fp`, `sp` denote the current frame and stack pointer. 
   4. To extend the current frame, we must subtract it by the amount `escaped-arguments * word-size`. But since we should as well store the old frame pointer (as we will update it with the current stack pointer); we must subtract `sp` by `(escaped-arguments + 1) * word-size`.
   5. Now old value of fp is saved in 0th location of this `sp`, and other arguments are saved respectively at the offsets already determined by the callee. (Callee predetermined these offsets considering the fact that we will save old `fp` at 0th word)
   6. Now as we are moving to the frame of other function `fp` is updated to `sp`, and `sp` value will be deducted by the amount needed by the callee stack. 
   7. Thus local variables allocated will be referred with negative offset wrt to `fp` and escaped argument parameters will be referred with non negative offset wrt `fp`.
   8. Thus it is evident that our code generator would need to know the access list of the callee which wasn't there in the design mentioned in the book, so I added it in `tree.sml` but to avoid cyclic dependency between `tree.sml` and `riscframe.sml`, I had to redefine `access` and had to create `accessConv.sml` to facilitate conversion between access of `tree` and `riscframe`.
   9. Also since code generator must save escaped arguments with respect to the new frame pointer which is not equal to current frame pointer as this whole thing is updated in `procEntryExit3`, but since new frame pointer = current stack pointer - (escape-count + 1) * word-size, I created new function `callexp` in `riscframe` to do this arithmetic.
2. Now inside this called function, we must create new temporary for each passed argument **in argument register** and move that register's value to this new temporary. This might seem unnecessary but consider `function m(x : int, y : int) = (h(y, y); h(x, x))`. If `x` stays in "parameter register 1" throughout `m`, and `y` is passed to `h` in parameter register 1, then there is a problem. The register allocator will eventually choose which machine register should hold the temporary (not implemented as of now). If there is no interference of the type shown in function `m`, then (on the RISC) the allocator will take care to choose register the same register as temporary to hold that register. Then the move instructions will be unnecessary and will be deleted at that time.
   1. This is done in `newFrame` of `riscframe`, named as `shiftInstr` (shift instructions).
3. Called function must save callee-save registers include `ra`, this is done in `procEntryExit1`, similarly restoring them in the end of function body is as well done here.