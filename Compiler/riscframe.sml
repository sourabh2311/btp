structure RiscFrame: FRAME = struct

structure T = Temp
structure Tr = Tree

(* in register or frame? *)
datatype access = InFrame of int | InReg of T.temp
fun getTempString (t as (a, b)) = "(" ^ Int.toString(a) ^ ", " ^ Int.toString(b) ^ ")"

fun inReg (acc) = 
case acc of 
    InReg _ => true 
  | _ => false

fun genAccessT (temp : T.temp) = InReg temp
fun genAccessO (offset : int) = InFrame offset
fun getEscapes ([]) = 0 
  | getEscapes (formal :: formals : access list) = 
    case formal of 
      InFrame _ => 1 + getEscapes(formals)
    | _ => getEscapes(formals)

fun getAccessTemp (InReg t) = t 

fun getAccessOffset (InFrame x) = x

fun dummyFormals (amnt, ls) = if amnt = 0 then ls else dummyFormals(amnt - 1, ls @ [InReg (T.newtemp(0))])   (* remember that here newtemp doesn't matter as in codegen, isRealL will be used to determine stuff, and we don't use InFrame as in this case we have to put in register only *)
(* numLocals: How many variables got called by allocLocal which were put in our stack (i.e. not in register but in Frame) *)
type frame = {name : T.label, formals : access list, numLocals : int ref, shiftInstr : Tr.stm list, escapeCount : int, isRealL : bool list}

type register = string  (* so type of our register list will be "string list", register list is useful to tell available registers *)

(* 
  Given a Tiger function definition comprising a level and an already-translated body expression, the Translate phase should produce a descriptor for the function containing this necessary information: 
    1. frame: The frame descriptor containing machine-specific information about local variables and parameters. 
    2. body: The result returned from procEntryExit1. Call this pair a fragment to be translated to assembly language. 
*)
(* A string literal in the Tiger (or C) language is the constant address of a segment of memory initialized to the proper characters. *)
(* For each string literal "lit", the Translate module makes a new label "lab", and returns the tree "Tree.name (lab)". It also puts the  assembly-language fragment "Frame.STRING (lab, lit) onto a global list of such fragments to be handed to the code emitter. *)
(* All this would be handled in putting it all together phase (chapter 12) *)
datatype frag = PROC of {body: Tree.stm, frame: frame}
              | STRING of T.label * string
              | REAL of T.label * real

(* -----------------CPU Registers----------------------- *)

val zero = T.newtemp(0) (* constant 0 *)
val ra = T.newtemp(0) (* return address *)
val sp = T.newtemp(0) (* stack pointer *)

(* arguments *)
val a0 = T.newtemp(0)
val a1 = T.newtemp(0)
val a2 = T.newtemp(0)
val a3 = T.newtemp(0)
val a4 = T.newtemp(0)
val a5 = T.newtemp(0)
val a6 = T.newtemp(0)
val a7 = T.newtemp(0)

val fa0 = T.newtemp(1)
val fa1 = T.newtemp(1)
val fa2 = T.newtemp(1)
val fa3 = T.newtemp(1)
val fa4 = T.newtemp(1)
val fa5 = T.newtemp(1)
val fa6 = T.newtemp(1)
val fa7 = T.newtemp(1)

val rv = a0
val rrv = fa0

(* temporary *)
val t0 = T.newtemp(0)
val t1 = T.newtemp(0)
val t2 = T.newtemp(0)
val t3 = T.newtemp(0)
val t4 = T.newtemp(0)
val t5 = T.newtemp(0)
val t6 = T.newtemp(0)


val ft0 = T.newtemp(1)
val ft1 = T.newtemp(1)
val ft2 = T.newtemp(1)
val ft3 = T.newtemp(1)
val ft4 = T.newtemp(1)
val ft5 = T.newtemp(1)
val ft6 = T.newtemp(1)
val ft7 = T.newtemp(1)
val ft8 = T.newtemp(1)
val ft9 = T.newtemp(1)
val ft10 = T.newtemp(1)
val ft11 = T.newtemp(1)

(* saved temporary *)
val s0 = T.newtemp(0) (* s0 = fp *)
val s1 = T.newtemp(0)
val s2 = T.newtemp(0)
val s3 = T.newtemp(0)
val s4 = T.newtemp(0)
val s5 = T.newtemp(0)
val s6 = T.newtemp(0)
val s7 = T.newtemp(0)
val s8 = T.newtemp(0)
val s9 = T.newtemp(0)
val s10 = T.newtemp(0)
val s11 = T.newtemp(0)


val fs0 = T.newtemp(1) (* s0 = fp *)
val fs1 = T.newtemp(1)
val fs2 = T.newtemp(1)
val fs3 = T.newtemp(1)
val fs4 = T.newtemp(1)
val fs5 = T.newtemp(1)
val fs6 = T.newtemp(1)
val fs7 = T.newtemp(1)
val fs8 = T.newtemp(1)
val fs9 = T.newtemp(1)
val fs10 = T.newtemp(1)
val fs11 = T.newtemp(1)

val fp = s0 

(* val calldefs = [ra, t0, t1, t2, t3, t4, t5, t6] *)

val specialregs = [(zero, "zero"), (sp, "sp"), (ra, "ra"), (fp, "s0")]

val argregs = [(a0, "a0"), (a1, "a1"), (a2, "a2"), (a3, "a3"), (a4, "a4"), (a5, "a5"), (a6, "a6"), (a7, "a7")]
val rargregs = [(fa0, "fa0"), (fa1, "fa1"), (fa2, "fa2"), (fa3, "fa3"), (fa4, "fa4"), (fa5, "fa5"), (fa6, "fa6"), (fa7, "fa7")]

val savedregs = [(s1, "s1"), (s2, "s2"), (s3, "s3"), (s4, "s4"), (s5, "s5"), (s6, "s6"), (s7, "s7"), (s8, "s8"), (s9, "s9"), (s10, "s10"), (s11, "s11")]
val rsavedregs = [(fs0, "fs0"), (fs1, "fs1"), (fs2, "fs2"), (fs3, "fs3"), (fs4, "fs4"), (fs5, "fs5"), (fs6, "fs6"), (fs7, "fs7"), (fs8, "fs8"), (fs9, "fs9"), (fs10, "fs10"), (fs11, "fs11")]

val temporaries = [(t0, "t0"), (t1, "t1"), (t2, "t2"), (t3, "t3"), (t4, "t4"), (t5, "t5"), (t6, "t6")] 
val rtemporaries = [(ft0, "ft0"), (ft1, "ft1"), (ft2, "ft2"), (ft3, "ft3"), (ft4, "ft4"), (ft5, "ft5"), (ft6, "ft6"), (ft7, "ft7"), (ft8, "ft8"), (ft9, "ft9"), (ft10, "ft10"), (ft11, "ft11")] 

val calleesaves = savedregs @ rsavedregs
val callersaves = temporaries @ rtemporaries
val calldefs = [(rv, "a0"), (ra, "ra"), (rrv, "fa0")] @ callersaves

val wordSize = 4

(* registers allocated for arguments in risc *)
val argregsCount = List.length argregs
val rargregsCount = List.length rargregs

fun getFirstL (ls) = (map (fn (x, y) => x) ls)
fun getSecondL (ls) = (map (fn (x, y) => y) ls)

(* Getting all the registers available for coloring *)
val registers = getSecondL(savedregs @ temporaries) 
val rregisters = getSecondL (rsavedregs @ rtemporaries)
(* when removing sp, it will have to be adjusted *)
val allRegisters = (specialregs @ argregs @ savedregs @ temporaries @ rargregs @ rsavedregs @ rtemporaries)

(* Helper Functions *)

fun incrementNumLocals ({numLocals, ...} : frame) = numLocals := !numLocals + 1
fun getEscapeCount ({escapeCount, ...} : frame) = escapeCount 
fun getOffset ({numLocals, ...} : frame) = !numLocals * (~wordSize) 
fun getFOffset (frame' : frame) = ~(getOffset (frame'))
fun name ({name, ...} : frame) = name
fun formals ({formals, ...} : frame) = formals

(* To generate label for string *)
fun genString (lab, s) = Symbol.name lab ^ ": .string \"" ^ s ^ "\"\n"
fun genReal (lab, r) = Symbol.name lab ^ ": .float " ^ Real.toString (r) ^ "\n"

(* The function Frame.exp is used by Translate to turn a Frame.access into the Tree expression. The Tree.exp argument to Frame.exp is the address of the stack frame that the access lives in. Thus, for an access "a" such as InFrame(k), we have Frame.exp (a) (TEMP(Frame.FP)) = MEM(BINOP(PLUS, TEMP(Frame.FP), CONST(k))). Why bother to pass the tree expression temp (Frame.FP) as an argument? The answer is that the address of the frame is the same as the current frame pointer only when accessing the variable from its own level. When accessing "a" from an inner-nested function, the frame address must be calculated using static links, and the result of this calculation will be the Tree.exp argument to Frame.exp. If "a" is a register access such as InReg(t932) then the frame-address  argument to Frame.exp will be discarded, and the result will be simply TEMP t932. *)
fun exp frameAccess frameAddress isReal = 
  case frameAccess of
      InFrame offset => if isReal then Tr.RMEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST offset)) else Tr.MEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST offset))
    | InReg temp => Tr.TEMP(temp)

fun callexp frameAccess frameAddress escapes isReal = 
  case frameAccess of
      InFrame offset => if isReal then Tr.RMEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST (offset - (escapes + 1) * wordSize))) else Tr.MEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST (offset - (escapes + 1) * wordSize)))
    | InReg temp => Tr.TEMP(temp) (* this line is useless, delete it later *)

(* Given the name of the frame and list of variables mentioned in the format of whether they escape or not, function returns the new frame *)
(* Above comment is sufficient but can look at page 142 *)
fun newFrame {name : T.label, formals : bool list, isRealL : bool list} : frame = 
let
  fun allocFormals(offset, [], allocList, falseCount, rfalseCount, _) = (allocList, falseCount, rfalseCount)
    | allocFormals(offset, curFormal :: l, allocList, falseCount, rfalseCount, isReal :: others) = 
      (
      case curFormal of
          true => allocFormals(offset + wordSize, l, allocList @ [InFrame offset], falseCount, rfalseCount, others)
        | false => 
          if (not isReal) then 
            if (falseCount >= argregsCount) then allocFormals(offset + wordSize, l, allocList @ [InFrame offset], falseCount, rfalseCount, others) else allocFormals(offset, l, allocList @ [InReg(T.newtemp(0))], falseCount + 1, rfalseCount, others)
          else 
            if (rfalseCount >= rargregsCount) then allocFormals(offset + wordSize, l, allocList @ [InFrame offset], falseCount, rfalseCount, others) else 
            allocFormals (offset, l, allocList @ [InReg(T.newtemp(1))], falseCount, rfalseCount + 1, others)
      )
  val (aformals, falseCount, rfalseCount) = allocFormals (wordSize, formals, [], 0, 0, isRealL) (* first word is reserved for fp *)
  val escapeCount = (length formals) - falseCount - rfalseCount
  fun handleShift([], ni, ri, _) = [] 
    | handleShift(acc :: accRem, ni, ri, isReal :: others) = 
      case acc of 
        InReg _ => if isReal then [Tr.RMOVE (exp acc (Tr.TEMP fp) isReal, Tr.TEMP (List.nth(getFirstL(rargregs), ri)))] @ handleShift(accRem, ni, ri + 1, others) else [Tr.MOVE (exp acc (Tr.TEMP fp) false, Tr.TEMP (List.nth(getFirstL(argregs), ni)))] @ handleShift(accRem, ni + 1, ri, others)
      | _ => handleShift(accRem, ni, ri, others)
  val shiftInstr = handleShift(aformals, 0, 0, isRealL)
in
  {name = name, formals = aformals, numLocals = ref 0, shiftInstr = shiftInstr, escapeCount = escapeCount, isRealL = isRealL}
end

fun allocLocal frame' escape isReal = (
  case escape of
      true => (incrementNumLocals frame'; InFrame(getOffset frame'))
    | false => InReg(if isReal then T.newtemp(1) else T.newtemp(0))
)


(* Calling runtime-system functions. To call an external function named initArray with arguments a, b, simply generate a CALL such as CALL(NAME(Temp.namedlabel ( "initArray" )), [a, b]) This refers to an external function initArray which is written in a language such as C or assembly language - it cannot be written in Tiger because Tiger has no mechanism for manipulating raw memory. But on some operating systems, the C compiler puts an underscore at the beginning of each label; and the calling conventions for C functions may differ from those of Tiger functions; and C functions don't expect to receive a static link, and so on. All these target-machine-specific details should be encapsulated into a function provided by the Frame structure. where externalCall takes the name of the external procedure and the  arguments to be passed. *)
(* fun externalCall (s, args) = Tr.CALL(Tr.NAME(T.namedlabel s), args, [], [], false) *)

(* needed as we are going to add new tree instructions *)
fun seq nil = Tr.EXP (Tr.CONST 0)
  | seq [st] = st
  | seq (st :: rest) = Tr.SEQ(st, seq (rest))

(* 
  Each Tiger function is translated into a segment of assembly language with a prologue, a body, and an epilogue. 

  The body of a Tiger function is an expression, and the body of the translation is simply the translation of that expression.

  The prologue, which precedes the body in the assembly-language version of the function, contains:
    1. Pseudo-instructions, as needed in the particular assembly language, to announce the beginning of a function. (Not relevant in our context)
    2. A label definition for the function name. (Done in procEntryExit3)
    3. An instruction to adjust the stack pointer (to allocate a new frame). (Done in procEntryExit3)
    4. Instructions to save "escaping" arguments - including the static link - into the frame, and to move nonescaping arguments into fresh temporary registers. (Done in procEntryExit1)
    5. Store instructions to save any callee-save registers - including the return address register - used within the function. (Done in procEntryExit1)

  (6) The function body.

  The epilogue comes after the body and contains
    7. An instruction to move the return value (result of the function) to the register reserved for that purpose. (Already added in body by translate)
    8. Load instructions to restore the callee-save registers. (Done in procEntryExit1)
    9. An instruction to reset the stack pointer (to deallocate the frame). (Done in procEntryExit3)
    10. A return instruction (JUMP to the return address). (Done in procEntryExit3)
    11. pseudo-instructions, as needed, to announce the end of a function. (Not relevant in our context)

  Some of these items (1, 3, 9 and 11) depend on exact knowledge of the frame size, which will not be known until after the register allocator determines how many local variables need to be kept in the frame because they don't fit in registers. So these instructions should be generated very late, in a frame function called procEntryExit3. 
*)

(* As mentioned in the above comment, it does what is known as "view shift" *)
fun procEntryExit1(frame' as {shiftInstr, ...} : frame, body) = 
let
  val pairs = map (fn reg => (allocLocal frame' false (T.isReal(reg)), reg)) ([ra] @ getFirstL (calleesaves))
  val saves = map (fn (allocLoc, reg) => if (T.isReal(reg)) then Tr.RMOVE (exp allocLoc (Tr.TEMP fp) true, Tr.TEMP reg) else Tr.MOVE (exp allocLoc (Tr.TEMP fp) false, Tr.TEMP reg)) pairs
  val restores = map (fn (allocLoc, reg) => if (T.isReal(reg)) then Tr.RMOVE (Tr.TEMP reg, exp allocLoc (Tr.TEMP fp) true) else Tr.MOVE (Tr.TEMP reg, exp allocLoc (Tr.TEMP fp) false)) (List.rev pairs)
in
  seq(shiftInstr @ saves @ [body] @ restores)
end


(* This function appends a "sink" instruction to the function body to tell the register allocator that certain registers are live at procedure exit. Having zero live at the end means that it is live throughout, which will prevent the register allocator from trying to use it for some other purpose. The same trick works for any other special registers the machine might have. *)
(* The following snippet was given in book, just modified src. *)
fun procEntryExit2(frame, body) = 
        body @
        [Assem.OPER {assem = "",
                (* maybe can remove a0, fp from here *)
                 src = getFirstL (specialregs @ calleesaves),
                 dst = [], jump = SOME[]}
        ]

fun procEntryExit3(frame' : frame, body) =
  {prolog = Symbol.name (name frame') ^ ":\n" ^ 
    "addi sp, sp, -" ^ Int.toString((getEscapeCount(frame') + 1) * wordSize)^ "\n" ^ 
   (* fp -> 0(sp) *)
    "sw s0 0(sp)\n" ^ 
   (* sp -> fp *)
    "mv s0 sp\n" ^ 
   (* sp = sp -  (allocating space in stack) *)
   "addi sp sp -" ^ Int.toString (getFOffset (frame')) ^ "\n",
   body = body,
            (* fp -> sp *)
   epilog = "mv sp s0\n" ^ 
   (* lw Rdest, address			Load Word
Load the 32-bit quantity (word) at address into register Rdest. *)
   "lw s0 0(sp)\n" ^ 
   "addi sp, sp, " ^ Int.toString((getEscapeCount(frame') + 1) * wordSize) ^ "\n" ^ 
   (* jr Rsource				Jump Register
Unconditionally jump to the instruction whose address is in register Rsource.*)
   "jr ra\n"}
end