(* The MakeGraph module turns a list of Assem instructions into a flow graph. *)

signature MAKE_GRAPH =
sig

  (* The function instrs2graph takes a list of instructions and returns a flow graph, along with a list of nodes that corresponds exactly to the instructions. *)

  val instrs2graph: Assem.instr list -> Flow.flowgraph

end

structure MakeGraph : MAKE_GRAPH =
struct



(* create graph from the instruction list *)
fun instrs2graph instrs =
let
  (* Step 1: Make a node for each instruction! *)
  val nodeC = ref 0
  fun getNode(instr, nodeList) =
  let
    val (def', use', ismove') =
      case instr of
          Assem.OPER{assem, dst, src, jump} => (dst, src, false)
        | Assem.LABEL{assem, lab} => (nil, nil, false)
        | Assem.MOVE{assem, dst, src} => ([dst], [src], true)

  in
    nodeList @ [Flow.FNODE{id = (nodeC := !nodeC + 1; !nodeC), def = def', use = use', ismove = ismove', succ = ref nil, prev = ref nil, liveOut = ref nil}]
  end

  val nodeList = foldl getNode nil instrs

  val instrNodeList = ListPair.zip (instrs, nodeList)
  (* ------------------ Step 1 End ----------------- *)

  (* Step 2: Add edges *)
  (* Function to add an edge *)
  fun addEdge(from as Flow.FNODE{succ, ...}, to as Flow.FNODE{prev, ...}) =
    if List.exists (fn n => n = to) (!succ) then ()
    else (succ := to :: (!succ); prev := from :: (!prev))

  (* When instruction at node is a JUMP instruction, jumping to possibly givenLabel, add an edge between this and corresponding LABEL node  *)
  fun addEdgeToLabel node givenLabel =
    case List.find
      (fn (sintr, nodeNo) =>
        case sintr of
          Assem.LABEL{lab, ...} => givenLabel = lab
          | _ => false
      ) instrNodeList
      of
      SOME ((_, nodeNo)) => addEdge (node, nodeNo)

  fun join nil = ()
    | join [(instr, node)] = 
      (case instr of 
          (* Handle Jump Instructions *)
          Assem.OPER{jump = SOME jumpList, ...} => (map (addEdgeToLabel node) jumpList; ())
        | _ => ())
    | join ((instrA, nodeA) :: ((instrB, nodeB) :: rest)) =
      (case instrA of
          (* Handle Jump Instructions *)
          Assem.OPER{jump = SOME jumpList, ...} => (map (addEdgeToLabel nodeA) jumpList; ())
        | _ => addEdge(nodeA, nodeB); join ((instrB, nodeB) :: rest))

  val _ = join instrNodeList;

  (* ------------------- Step 2 End ------------------ *)
in
  nodeList
end




end