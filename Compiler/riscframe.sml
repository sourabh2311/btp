structure RiscFrame: FRAME = struct

structure T = Temp
structure Tr = Tree

(* in register or frame? *)
datatype access = InFrame of int | InReg of T.temp

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

(* numLocals: How many variables got called by allocLocal which were put in our stack (i.e. not in register but in Frame) *)
type frame = {name : T.label, formals : access list, numLocals : int ref, shiftInstr : Tr.stm list, escapeCount : int}

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

val zero = T.newtemp() (* constant 0 *)
val ra = T.newtemp() (* return address *)
val sp = T.newtemp() (* stack pointer *)

(* arguments *)
val a0 = T.newtemp()
val a1 = T.newtemp()
val a2 = T.newtemp()
val a3 = T.newtemp()
val a4 = T.newtemp()
val a5 = T.newtemp()
val a6 = T.newtemp()
val a7 = T.newtemp()

val rv = a0

(* temporary *)
val t0 = T.newtemp()
val t1 = T.newtemp()
val t2 = T.newtemp()
val t3 = T.newtemp()
val t4 = T.newtemp()
val t5 = T.newtemp()
val t6 = T.newtemp()

(* saved temporary *)
val s0 = T.newtemp() (* s0 = fp *)
val s1 = T.newtemp()
val s2 = T.newtemp()
val s3 = T.newtemp()
val s4 = T.newtemp()
val s5 = T.newtemp()
val s6 = T.newtemp()
val s7 = T.newtemp()
val s8 = T.newtemp()
val s9 = T.newtemp()
val s10 = T.newtemp()
val s11 = T.newtemp()

val fp = s0 

(* val calldefs = [ra, t0, t1, t2, t3, t4, t5, t6] *)

val specialregs = [(zero, "zero"), (sp, "sp"), (ra, "ra"), (fp, "s0")]

val argregs = [(a0, "a0"), (a1, "a1"), (a2, "a2"), (a3, "a3"), (a4, "a4"), (a5, "a5"), (a6, "a6"), (a7, "a7")]

val savedregs = [(s1, "s1"), (s2, "s2"), (s3, "s3"), (s4, "s4"), (s5, "s5"), (s6, "s6"), (s7, "s7"), (s8, "s8"), (s9, "s9"), (s10, "s10"), (s11, "s11")]

val temporaries = [(t0, "t0"), (t1, "t1"), (t2, "t2"), (t3, "t3"), (t4, "t4"), (t5, "t5"), (t6, "t6")] 

val calleesaves = savedregs
val callersaves = temporaries


val wordSize = 4

(* registers allocated for arguments in risc *)
val argregsCount = List.length argregs

fun getFirstL (ls) = (map (fn (x, y) => x) ls)
fun getSecondL (ls) = (map (fn (x, y) => y) ls)

(* Getting all the registers available for coloring *)
val registers = getSecondL(savedregs @ temporaries) 
val allRegisters = (specialregs @ argregs @ savedregs @ temporaries)

(* tempMap is a table from registers (not all registers but some) to their name *)
(* basically its use is for new temporaries, so that we can assign register to them as well *)
val tempMap = 
let
  fun addToTable ((t, s), table) = T.Table.enter(table, t, s)
  val toAdd = specialregs @ argregs @ savedregs @ temporaries
in
  foldl addToTable T.Table.empty toAdd
end

(* This was useful before register allocation, now it is absolete *)
fun getTempString t = 
  case T.Table.look(tempMap, t) of
      SOME (r) => r
    | NONE => T.makestring t

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
fun exp frameAccess frameAddress = 
  case frameAccess of
      InFrame offset => Tr.MEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST offset))
    | InReg temp => Tr.TEMP(temp)

fun callexp frameAccess frameAddress escapes = 
  case frameAccess of
      InFrame offset => Tr.MEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST (offset - (escapes + 1) * wordSize)))
    | InReg temp => Tr.TEMP(temp) (* this line is useless, delete it later *)

(* Given the name of the frame and list of variables mentioned in the format of whether they escape or not, function returns the new frame *)
(* Above comment is sufficient but can look at page 142 *)
fun newFrame {name : T.label, formals : bool list} : frame = 
let
  fun allocFormals(offset, [], allocList, falseCount) = (allocList, falseCount)
    | allocFormals(offset, curFormal :: l, allocList, falseCount) = 
      (
      case curFormal of
          true => allocFormals(offset + wordSize, l, allocList @ [InFrame offset], falseCount)
        | false => if (falseCount >= argregsCount) then allocFormals(offset + wordSize, l, allocList @ [InFrame offset], falseCount) else allocFormals(offset, l, allocList @ [InReg(T.newtemp())], falseCount + 1)
      )
  val (aformals, falseCount) = allocFormals (wordSize, formals, [], 0) (* first word is reserved for fp *)
  val escapeCount = (length formals) - falseCount
  fun handleShift([], x) = [] 
    | handleShift (acc :: accRem, []) = []
    | handleShift(acc :: accRem, arg :: args) = 
      case acc of 
        InReg _ => [Tr.MOVE (exp acc (Tr.TEMP fp), Tr.TEMP arg)] @ handleShift(accRem, args)
      | _ => handleShift(accRem, arg :: args)
  val shiftInstr = handleShift(aformals, getFirstL (argregs))
in
  {name = name, formals = aformals, numLocals = ref 0, shiftInstr = shiftInstr, escapeCount = escapeCount}
end

fun allocLocal frame' escape = (
  case escape of
      true => (incrementNumLocals frame'; InFrame(getOffset frame'))
    | false => InReg(T.newtemp())
)


(* Calling runtime-system functions. To call an external function named initArray with arguments a, b, simply generate a CALL such as CALL(NAME(Temp.namedlabel ( "initArray" )), [a, b]) This refers to an external function initArray which is written in a language such as C or assembly language - it cannot be written in Tiger because Tiger has no mechanism for manipulating raw memory. But on some operating systems, the C compiler puts an underscore at the beginning of each label; and the calling conventions for C functions may differ from those of Tiger functions; and C functions don't expect to receive a static link, and so on. All these target-machine-specific details should be encapsulated into a function provided by the Frame structure. where externalCall takes the name of the external procedure and the  arguments to be passed. *)
fun externalCall (s, args) = Tr.CALL(Tr.NAME(T.namedlabel s), args, [])

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
  val pairs = map (fn reg => (allocLocal frame' false, reg)) ([ra] @ getFirstL (calleesaves))
  val saves = map (fn (allocLoc, reg) => Tr.MOVE (exp allocLoc (Tr.TEMP fp), Tr.TEMP reg)) pairs
  val restores = map (fn (allocLoc, reg) => Tr.MOVE (Tr.TEMP reg, exp allocLoc (Tr.TEMP fp))) (List.rev pairs)
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