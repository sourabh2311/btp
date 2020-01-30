signature REG_ALLOC =
sig
  val alloc : Assem.instr list * RiscFrame.frame -> Assem.instr list * RiscFrame.register Temp.TempMap.map
end
