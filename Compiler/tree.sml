signature TREE = 
sig 
  type label = Temp.label
  type size
  datatype access = InFrame of int | InReg of Temp.temp
  val inReg : access -> bool
  val getAccessTemp : access -> Temp.temp 
  val getAccessOffset : access -> int 
  val genAccessT : Temp.temp -> access
  val genAccessO : int -> access

datatype stm = SEQ of stm * stm
              (* Define the constant value of name n to be the current machine code address. This is like a label definition in assembly language. The value NAME(rc) may be the target of jumps, calls, etc. *)
             | LABEL of label
             | JUMP of exp * label list
             | CJUMP of relop * exp * exp * label * label
	           | MOVE of exp * exp
             | EXP of exp

     and exp = BINOP of binop * exp * exp
             | MEM of exp
             | TEMP of Temp.temp
             | ESEQ of stm * exp
             | NAME of label
             | CONST of int
             | REAL of real
             (* exp = f, exp list = args. *)
	           | CALL of exp * exp list * access list 

     and binop = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
    (* Arithmetic shift preserve sign bit, whereas Logical shift can not preserve sign bit. *)
    (* ARSHIFT -> Arithmetic Right Shift *)
     and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE

end

structure Tree : TREE = 
struct
  type label = Temp.label
  (* wordsize. *)
  type size = int
  datatype access = InFrame of int | InReg of Temp.temp
  fun inReg (acc) = 
  case acc of 
      InReg _ => true 
    | _ => false

  fun genAccessT (temp : Temp.temp) = InReg temp
  fun genAccessO (offset : int) = InFrame offset

  fun getAccessTemp (InReg t) = t 
  fun getAccessOffset (InFrame x) = x

datatype stm = SEQ of stm * stm (* The statement s2 followed by s2. *)
             | LABEL of label (* Define the constant value of name n to be the current machine code address. This is like a label definition in assembly language. *)
             | JUMP of exp * label list (* Transfer control (jump) to address exp. The destination exp may be a literal label, as in NAME(lab), or it may be an address calculated by any other kind of expression. For example, a C-language switch (i) statement may be implemented by doing arithmetic on i. The list of labels labs specifies all the possible locations that the expression exp can evaluate to; this is necessary for dataflow analysis later. The common case of jumping to a known label "l" is written as jump(name l, [l]). *)
             | CJUMP of relop * exp * exp * label * label (* CJUMP(o, e1, e2, t, f): Evaluate e1, e2 in that order, yielding values a, b. Then compare a, b using the relational operator o. If the result is true, jump to t otherwise jump to f. The relational operators *)
	           | MOVE of exp * exp 
             (*
                MOVE(TEMP t, e): Evaluate e and move it into temporary t. 
                MOVE(MEM(e1), e2) Evaluate e1 yielding address a. Then evaluate e2 and store the result into wordSize bytes of memory starting at a.
             *)
             | EXP of exp (* Evaluate exp and discard the result. *)

     and exp = BINOP of binop * exp * exp (* The application of binary operators to operands exp1, exp2. *)
             | MEM of exp (* The contents of wordSize bytes of memory starting at address exp (where wordSize is defined in the Frame module). Note that when MEM is used as the left child of a move, it means "store," but anywhere else it means "fetch." *)
             | TEMP of Temp.temp (* Temporary t. A temporary in the abstract machine is similar to a register in a real machine. However, the abstract machine has an infinite number of temporaries. *)
             | ESEQ of stm * exp (* The statement s is evaluated for side effects, then e is evaluated for result *)
             | NAME of label (* The value NAME(n) may be the target of jumps, calls, etc. *)
             | CONST of int (* The integer constant int. *)
             | REAL of real
	           | CALL of exp * exp list * access list (* A procedure call: the application of function exp1 to argument list exp2 list. The subexpression exp1 is evaluated before the arguments which are evaluated left to right. *)

     and binop = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

     and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE

end

