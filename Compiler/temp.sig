signature TEMP =
sig
  eqtype temp
  type ptemp = temp * temp
  val newtemp : unit -> temp
  structure Table : TABLE sharing type Table.key = temp
  structure TempKey : ORD_KEY
  structure TempSet : ORD_SET
  structure TempPSet : ORD_SET
  structure TempMap : ORD_MAP
  val makestring: temp -> string
  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
end
