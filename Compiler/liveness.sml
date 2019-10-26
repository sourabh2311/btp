(* Here we will build interference graph (remember that it is undirected (obv.)) *)

signature LIVENESS =
sig

  (* So a node of our interference graph is a (temporary register and its neighbours) *)
  datatype node = NODE of {temp: Temp.temp, adj: node list ref}

  (* So our I(Interference)GRAPH is a node list *)
  type igraph = node list

  val interferenceGraph : Flow.flowgraph -> igraph

end

(* Defining a way to compare two temps as we are using set *)
structure TempKey : ORD_KEY =
struct
  type ord_key = Temp.temp
  fun compare (t1, t2) = String.compare (Temp.makestring t1, Temp.makestring t2)
end

structure Liveness :> LIVENESS =
struct

datatype node = NODE of {temp: Temp.temp, adj: node list ref}

type igraph = node list

structure F = Flow
(* Not a symbol *)
structure S = RedBlackSetFn(TempKey)
structure T = Temp
structure TT = Temp.Table
structure GT = IntMapTable(type key = int fun getInt n = n)
structure Frame = RiscFrame


(* Step 1: Do liveness analysis, compute liveOut for each node. *)
fun liveness flowgraph = 
let 

  type liveSet = S.set
  type liveMap = liveSet GT.table
  fun look(table: liveMap, key) = valOf(GT.look(table, key))

  fun fnodeToEmptySet () =
    foldl (fn (F.FNODE{id, ...}, curTable) => GT.enter(curTable, id, S.empty)) (GT.empty) (flowgraph)
  (* ^-- Make a empty map that maps each node in graph to an empty set *)

  val liveInMap = ref (fnodeToEmptySet())
  val liveOutMap = ref (fnodeToEmptySet())
  val changed = ref false 

  fun updateNodeLiveness (F.FNODE {id, def, use, succ, ...}) = 
  let 
    val oldIn = look(!liveInMap, id) (* Save old livein set *)
    val oldOut = look(!liveOutMap, id) (* Save old liveout set *)
    val usex = S.addList(S.empty, use)
    val defx = S.addList(S.empty, def)
    val newIn = S.union(usex, S.difference(oldOut, defx))
    val newOut = foldl (fn (F.FNODE{id = id', ...}, currSet) => S.union (look (!liveInMap, id'), currSet)) S.empty (!succ)
  in
    liveInMap := GT.enter(!liveInMap, id, newIn); 
    liveOutMap := GT.enter(!liveOutMap, id, newOut);
    if S.equal(newIn, oldIn) andalso S.equal(newOut, oldOut)
    then () else changed := true
  end

  fun updateLiveness () = app updateNodeLiveness flowgraph

  fun LivenessFixPoint () = 
    (changed := false;
    updateLiveness (); 
    if !changed then LivenessFixPoint () else ())

  fun setNodeLiveOut (F.FNODE{id, liveOut, ...}) = 
  let 
    val liveOut' = look(!liveOutMap, id)
  in 
    liveOut := S.listItems(liveOut')
  end 

  fun setLiveOut () = app setNodeLiveOut flowgraph

in 
  LivenessFixPoint (); 
  setLiveOut () 
end

(* Step 1 Finished *)

(* Step 2: Construct interference graph *)
fun interferenceGraph flowgraph =
let
  (* will map temporary to its corresponding node *)
  val tempMap = ref TT.empty 

  (* this dummy-temporary will be need in case an instruction is not a move and we need to compare whether liveout temporary is not equal to this *)
  val dummyTemp = Temp.newtemp()

  fun look(table, key) = valOf(TT.look(table, key))

  (* create node corresponding to temporary if not already created *)
  fun createNode(temp) = 
  case TT.look(!tempMap, temp) of 
      NONE => 
      (
        let 
          val n = NODE{temp = temp, adj = ref nil}
        in 
          tempMap := TT.enter(!tempMap, temp, n);
          n 
        end
      ) 
    | SOME n => n

  fun createEdges([]) = ()
    | createEdges(node as F.FNODE{def, use, liveOut, ismove, ...} :: nodes) = 
      let 
        fun notPresent(temp, adj) = (List.all (fn (neigh as NODE{temp = temp', ...}) => temp <> temp') adj)

        fun traverseLiveOut(tempn, [], mvtemp) = ()
          | traverseLiveOut(tempn as NODE{temp = temp', adj = adj'}, liveout :: liveouts, mvtemp) = 
            (* no self loops and no repeatation of an edge *)
            if temp' <> liveout andalso liveout <> mvtemp andalso notPresent(temp', !adj') then 
            case look(!tempMap, liveout) of 
                (n as NODE{temp = temp'', adj = adj''}) => (adj' := n :: !adj'; adj'' := tempn :: !adj''; traverseLiveOut(tempn, liveouts, mvtemp))
            else traverseLiveOut(tempn, liveouts, mvtemp)
  
        fun traverseDef([], mvtemp) = ()
          | traverseDef(temp :: temps, mvtemp) = (traverseLiveOut(temp, !liveOut, mvtemp); traverseDef(temps, mvtemp))
      in 
        map createNode (!liveOut); (* just so that those nodes exist *)
        if ismove then traverseDef(map createNode def, List.hd(use)) else traverseDef(map createNode def, dummyTemp);
        map createNode use; (* just so that we don't miss any node *)
        createEdges(nodes)
      end
in
  liveness flowgraph; 
  createEdges flowgraph;
  TT.listItems(!tempMap)
end
end