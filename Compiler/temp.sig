signature TEMP =
sig
  eqtype temp
  type ptemp = temp * temp
  val newtemp : int -> temp
  val printTemp : temp -> string
  val isReal : temp -> bool
  structure TempKey : ORD_KEY
  structure TempSet : ORD_SET
  structure TempPSet : ORD_SET
  structure TempMap : ORD_MAP
  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
end
