signature TRANSLATE =
sig
  type level
  type access (* not the same as Frame.access *)
  type exp
  type expty
  type frag (* fragments *)
  val outermost : level
  val newLevel : {parent: level, name: Temp.label,
                  formals: bool list, isRealL : bool list} -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> bool -> access
  val simpleVar : access * level * bool -> exp
  val subscriptVar : exp * exp -> exp
  val fieldVar : exp * Symbol.symbol * (Symbol.symbol * Types.ty) list -> exp
  val intlit : int -> exp
  val strlit : string -> exp
  val reallit : real -> exp
  val relop : Absyn.oper * exp * exp -> exp
  val binop : Absyn.oper * exp * exp * int -> exp
  val ifelse : exp * exp * exp -> exp
  val record : exp list * Types.ty list -> exp
  val classObject : exp list * Types.ty list -> exp
  val array : exp * exp -> exp
  val loop : exp * exp * Temp.label -> exp
  val break : Temp.label -> exp
  val call : level * level * Temp.label * exp list * bool list * bool -> exp
  val assign : exp * exp * bool -> exp
  val sequence : exp list -> exp
  val nilexp : exp
  val letexp : exp list * exp -> exp

  val getResult : unit -> frag list
  val reset : unit -> unit

  val procEntryExit : level * exp * Types.ty -> unit

  val errExp : exp
end