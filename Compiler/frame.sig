signature FRAME =
sig
  (* See riscframe.sml for complete description *)
  type frame
  type access
  val inReg : access -> bool
  val getAccessTemp : access -> Temp.temp 
  val getAccessOffset : access -> int 
  val getEscapes : access list -> int
  val callexp : access -> Tree.exp -> int -> Tree.exp  
  val genAccessT : Temp.temp -> access
  val genAccessO : int -> access
  val newFrame : {name: Temp.label,
                  formals: bool list} -> frame
  val name: frame -> Temp.label
  val formals: frame -> access list
  val allocLocal : frame -> bool -> access
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
                | REAL of Temp.label * real
  val fp : Temp.temp
  val rv : Temp.temp
  (* val calldefs : Temp.temp list *)
  val wordSize : int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall : string * Tree.exp list -> Tree.exp
  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> {prolog : string, body : Assem.instr list, epilog : string}
  type register
  val tempMap : register Temp.Table.table
  val genString : Tree.label * string -> string
  val genReal : Tree.label * real -> string
  val getTempString : Temp.temp -> string
  val argregs : (Temp.temp * string) list
  val argregsCount : int
  val zero : Temp.temp
  val sp : Temp.temp
  val calleesaves : (Temp.temp * string) list
  val callersaves : (Temp.temp * string) list
  val registers : string list
  val allRegisters : (Temp.temp * string) list
  val getFirstL : ('a * 'b) list -> 'a list
end