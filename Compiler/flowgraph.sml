structure Flow =
struct

  structure TS = Temp.TempSet
(* 
  ismove: could be detected if the def and use are identical. 
*)

  datatype node = FNODE of {index : int, def : TS.set, use : TS.set, ismove : bool, succ : node list ref, succSet : IntRedBlackSet.set ref, liveIn : TS.set ref, liveOut : TS.set ref}
  (* fun ncompare ({id = id1, ...} : FNODE, {id = id2, ...} : FNODE) = Int.compare (id1, id2)
  structure nodeKey : ORD_KEY =
  struct
      type ord_key = node
      fun compare (n1, n2) = ncompare (n1, n2)
  end
  structure nodeSet = RedBlackSetFn (nodeKey) *)
  type flowgraph = node list
end
