signature ACCESSCONV = 
sig 
  val frameToTree: RiscFrame.access list -> Tree.access list
  val treeToFrame: Tree.access list -> RiscFrame.access list
end

structure AccessConv : ACCESSCONV = 
struct

structure F = RiscFrame
structure Tr = Tree 

fun frameToTree ([]) = []
  | frameToTree (x :: xs) = 
    case F.inReg x of 
      true => 
      (
        Tr.genAccessT(F.getAccessTemp(x)) :: frameToTree (xs)
      )
    | _ => 
      (
        Tr.genAccessO(F.getAccessOffset (x)) :: frameToTree (xs)
      )

fun treeToFrame ([]) = []
  | treeToFrame (x :: xs) = 
    case Tr.inReg x of 
      true => 
      (
        F.genAccessT(Tr.getAccessTemp(x)) :: treeToFrame (xs)
      )
    | _ => 
      (
        F.genAccessO(Tr.getAccessOffset (x)) :: treeToFrame (xs)
      )
 

end

