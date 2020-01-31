(* can uncomment those live lines after checking *)
(* Mention that it took many hours to get to that pivotal line *)
(* Seemingly the possible errors could be only with codegen, check tomorrow  *)
(* Tell them that it took time to figure out that actually there was no fault with your register allocator but had to just include ra in codegen and these things take some time *)
(* Add in report, static link <> old fp *)
(* frozenMoves, constrainedMoves was nowhere 'used' in his algorithm, so not keeping it as of now
    Additionally coalescedMoves is also not used anywhere but have included here *)
structure RegAlloc : REG_ALLOC =
struct
(* -- Structure Begin -- *)
structure A = Assem
structure Frame = RiscFrame
structure T = Temp
structure TT = T.Table
structure Tr = Tree
structure TS = Temp.TempSet
structure TPS = Temp.TempPSet
structure TM = Temp.TempMap

(* Colors *)
structure colors = RedBlackSetFn(
    type ord_key = Frame.register
    fun compare (r1, r2) = String.compare(r1, r2)
)


(*****************************************************************************************)
(********************-- Important Data Structures --**************************************)
(********************-- Their emptied in respected procedures --**************************)
(*****************************************************************************************)

val instrs : (Assem.instr list ref) = ref nil
val frame : ((Frame.frame option) ref) = ref NONE
val fgraph : (Flow.node list ref) = ref nil
val simplifyWorklist = ref TS.empty
val spillWorklist = ref TS.empty 
val freezeWorklist = ref TS.empty
val worklistMoves = ref TPS.empty
val activeMoves = ref TPS.empty
val precolored = TS.fromList (Frame.getFirstL(Frame.allRegisters))
val initial = ref TS.empty (* initialised in alloc function *)
val d = Temp.newtemp()
val selectStack = ref [d]
val coalescedNodes = ref TS.empty
val coloredNodes = ref TS.empty  (* this is not expected to include precolored nodes, only the newly colored ones *) 
val spilledNodes = ref TS.empty
val totalColors = colors.fromList(Frame.registers)
val K = List.length (Frame.registers)
(********************-- Graph Data Structures --******************************************)
(********************-- These would be required to be cleared in repeated Main's call --**)
(* This would be required to clear in repeated Main's call *)
val moveList = ref (TM.insert(TM.empty, d, TPS.singleton ((d, d))))
val coalescedMoves = ref TPS.empty
val adjSet = ref TPS.empty 
val adjList = ref (TM.insert (TM.empty, d, TS.singleton(d)))
val degree = ref (TM.insert(TM.empty, d, 1))
val alias = ref (TM.insert(TM.empty, d, d))
val color : (RiscFrame.register TM.map ref) = ref TM.empty

fun getTSElem (set) = List.hd(TS.listItems(set))  (* structures are not values and thus cannot be passed to functions *)
fun getTPSElem (set) = List.hd(TPS.listItems(set)) 
fun getTMSet (mp, item) = case TM.find (mp, item) of 
  SOME s => s 
| NONE => TS.empty

fun getTMPSet (mp, item) = case TM.find (mp, item) of 
  SOME s => s 
| NONE => TPS.empty

fun getTMInt (mp, item) = case TM.find (mp, item) of 
  SOME s => s 
| NONE => 0

fun getUseDef (Assem.OPER{assem, dst, src, jump}) = dst @ src
  | getUseDef (Assem.LABEL{assem, lab}) = []
  | getUseDef (Assem.MOVE{assem, dst, src}) = [dst, src]

(* remove redundant moves after coalescing *)
fun isRedundant instr =
    case instr of
        A.MOVE{assem, dst, src} => TM.lookup(!color, dst) = TM.lookup(!color, src)
        | _ => false

fun alloc (ginstrs, gframe) = (* g = given *)
(
    instrs := ginstrs;
    frame := SOME (gframe);
    initial := (
        let 
            val allTemps = foldl (fn (instr, currentSet) => TS.addList (currentSet, getUseDef (instr))) TS.empty (!instrs)
        in 
            TS.difference (allTemps, precolored)
        end);
    Main ()
    (* We need not initialise any datastructure for repeated call of mains as it is handled in various procedures *)
) 
    and

clearGraph () = (activeMoves := TPS.empty; spilledNodes := TS.empty; coloredNodes := TS.empty; coalescedNodes := TS.empty; moveList := TM.empty; adjList := TM.empty; degree := TM.empty; adjSet := TPS.empty; alias := TM.empty; color := List.foldl (fn ((t, c), m) => TM.insert(m, t, c)) (TM.empty) (Frame.allRegisters); selectStack := []; coalescedMoves := TPS.empty)

    and 
Main () = 
(
    clearGraph ();
    Build (); (* does liveness analysis *)
    MakeWorklist ();
    let 
        fun mainRepeat () = 
            if TS.isEmpty (!simplifyWorklist) andalso TPS.isEmpty (!worklistMoves) andalso TS.isEmpty (!freezeWorklist) andalso TS.isEmpty (!spillWorklist) then () 
            else 
            (
                if TS.isEmpty (!simplifyWorklist) = false then Simplify ()
                else if TPS.isEmpty (!worklistMoves) = false then Coalesce ()
                else if TS.isEmpty (!freezeWorklist) = false then Freeze ()
                else if TS.isEmpty (!spillWorklist) = false then SelectSpill () else ();
                mainRepeat ()
            )
    in 
        mainRepeat () 
    end;
    AssignColors ();
    if TS.isEmpty (!spilledNodes) = false then 
        (RewriteProgram(); Main ())
    else (
        (* instrs := List.filter (fn instr => not (isRedundant instr)) (!instrs); *)
        (* Need to return something *)
        (!instrs, !color)
    )
)
    and

Build () = 
let 
    val _ = fgraph := MakeGraph.instrs2graph (!instrs)
    val _ = Liveness.liveness (!fgraph)
    fun forEachInstr (fgraphNode as Flow.FNODE {def, use, ismove, liveOut, ...}) = 
    (* val something = ref (!somethingElse) - This will work as expected, but in general copying of ref variable will always change at both places *)
    let 
        val live = ref (!liveOut) 
    in 
        if ismove then 
        (
            (* Thing to note: Move instructions have only one dst (def) and src (use). *)
            let 
                val [d, u] = TS.listItems(def) @ TS.listItems (use)
            in 
            (
                live := TS.subtract (!live, u);
                moveList := TM.insert (!moveList, d, TPS.add (getTMPSet (!moveList, d), (d, u)));
                moveList := TM.insert (!moveList, u, TPS.add (getTMPSet (!moveList, u), (d, u))); 
                worklistMoves := TPS.add (!worklistMoves, (d, u))  
            )
            end
        ) else ();
        (* live := TS.union (!live, def); *)
        TS.app (
            fn d => 
                TS.app (
                    fn l => AddEdge (l, d)
                ) (!live)
        ) def
        (* live := TS.union (use, TS.difference (!live, def)) This line is not needed as my blocks are simply instructions *)
    end
in 
    app forEachInstr (!fgraph)
end
    and

AddEdge (u, v) = 
if (TPS.member ((!adjSet), (u, v)) = false andalso u <> v) then (
    adjSet := TPS.addList (!adjSet, [(u, v), (v, u)]);
    if TS.member (precolored, u) = false then (
        adjList := TM.insert (!adjList, u, TS.add (getTMSet (!adjList, u), v));
        degree := TM.insert (!degree, u, getTMInt (!degree, u) + 1)
    )
    else ();
    if TS.member (precolored, v) = false then (
        adjList := TM.insert (!adjList, v, TS.add (getTMSet (!adjList, v), u));
        degree := TM.insert (!degree, v, getTMInt (!degree, v) + 1)
    )
    else ()
) else ()
    and
MakeWorklist () = 
(
    (* spillWorklist := TS.empty; (* these initial statements were as such not needed *)
    freezeWorklist := TS.empty;
    simplifyWorklist := TS.empty; *)
    TS.app (
        fn n =>
        ( 
            initial := TS.subtract (!initial, n);
            if getTMInt (!degree, n) >= K then spillWorklist := TS.add (!spillWorklist, n) 
            else if (MoveRelated (n)) then freezeWorklist := TS.add (!freezeWorklist, n)
            else simplifyWorklist := TS.add (!simplifyWorklist, n)
        )
        
    ) (!initial)
)
    and
Adjacent (n) = TS.difference (getTMSet (!adjList, n), TS.union (TS.fromList (!selectStack), !coalescedNodes))
    and
NodeMoves (n) = TPS.intersection (getTMPSet (!moveList, n), TPS.union (!activeMoves, !worklistMoves))
    and
MoveRelated (n) = TPS.isEmpty (NodeMoves (n)) = false
    and
Simplify () = 
let 
    val n = getTSElem(!simplifyWorklist)
in 
(
    simplifyWorklist := TS.subtract (!simplifyWorklist, n);
    selectStack := [n] @ (!selectStack);
    TS.app (fn m => DecrementDegree(m)) (Adjacent (n))
)
end
    and
DecrementDegree (m) = 
let 
    val d = getTMInt(!degree, m) 
in 
    degree := TM.insert(!degree, m, d - 1);
    if d = K then (
        EnableMoves(TS.add (Adjacent(m), m));
        spillWorklist := TS.subtract(!spillWorklist, m);
        if MoveRelated (m) then 
            freezeWorklist := TS.add(!freezeWorklist, m)
        else 
            simplifyWorklist := TS.add (!simplifyWorklist, m)
    ) else ()
end
    and
EnableMoves (nodes) = 
    TS.app (
      fn n => TPS.app (
          fn m => if TPS.member(!activeMoves, m) then (
              activeMoves := TPS.subtract (!activeMoves, m);
              worklistMoves := TPS.add(!worklistMoves, m)
          ) else ()
        ) (NodeMoves (n))
    ) nodes
    and
Coalesce () = 
let 
    val m as (a, b) = getTPSElem(!worklistMoves) 
    val x = GetAlias (a)
    val y = GetAlias (b)
    val u = ref 0
    val v = ref 0 
in 
(
    if TS.member(precolored, y) then 
        (u := y; v := x)
    else 
        (u := x; v := y);
    worklistMoves := TPS.subtract(!worklistMoves, m);
    if ((!u) = (!v)) then 
    (
        coalescedMoves := TPS.add (!coalescedMoves, m);
        AddWorkList(!u)
    ) else if (TS.member(precolored, (!v)) orelse TPS.member(!adjSet, (!u, !v))) then (
        AddWorkList(!u);
        AddWorkList(!v)
    ) else if ((TS.member(precolored, !u) andalso TS.all (fn t => OK(t, !u)) (Adjacent(!v))) orelse (TS.member(precolored, !u) = false andalso Conservative(TS.union(Adjacent(!u), Adjacent(!v))))) then 
    (
        coalescedMoves := TPS.add(!coalescedMoves, m);
        Combine(!u, !v);
        AddWorkList(!u)
    ) else (
        activeMoves := TPS.add(!activeMoves, m)
    )
)
end
    and
AddWorkList(u) = 
if (TS.member(precolored, u) = false andalso not (MoveRelated (u)) andalso getTMInt(!degree, u) < K) then (
    freezeWorklist := TS.subtract(!freezeWorklist, u);
    simplifyWorklist := TS.add (!simplifyWorklist, u)
) else ()
    and
OK (t, r) = getTMInt(!degree, t) < K orelse TS.member(precolored, t) orelse TPS.member(!adjSet, (t, r))
    and
Conservative (nodes) = 
let 
    val k = ref 0
in 
(
    TS.app (
        fn n => if (getTMInt (!degree, n) >= K) then k := (!k) + 1 else ()
    ) nodes;
    (!k) < K
)
end
    and
GetAlias (n) = 
    if (TS.member(!coalescedNodes, n)) then GetAlias(TM.lookup(!alias, n))
    else n
    and
Combine (u, v) = (
    if (TS.member(!freezeWorklist, v)) then 
        freezeWorklist := TS.subtract (!freezeWorklist, v)
    else 
        spillWorklist := TS.subtract(!spillWorklist, v);
    coalescedNodes := TS.add (!coalescedNodes, v);
    alias := TM.insert (!alias, v, u);
    moveList := TM.insert (!moveList, u, TPS.union(getTMPSet(!moveList, u), getTMPSet(!moveList, v)));
    EnableMoves(TS.singleton(v));
    TS.app (
        fn t => (AddEdge (t, u); DecrementDegree (t))
    ) (Adjacent(v));
    if getTMInt(!degree, u) >= K andalso TS.member(!freezeWorklist, u) then 
    (
        freezeWorklist := TS.subtract(!freezeWorklist, u);
        spillWorklist := TS.add(!spillWorklist, u)
    )
    else ()
)
    and
Freeze () = 
let 
    val u = getTSElem(!freezeWorklist)
in 
(
    freezeWorklist := TS.subtract(!freezeWorklist, u);
    simplifyWorklist := TS.add (!simplifyWorklist, u);
    FreezeMoves (u)
)
end
    and
FreezeMoves(u) = 
TPS.app (
    fn (m as (x, y)) =>
    let 
        val v = ref 0 
    in 
    (
        if (GetAlias (y) = GetAlias (u)) then 
            v := GetAlias (x)
        else 
            v := GetAlias (y);
        activeMoves := TPS.subtract (!activeMoves, m);
        if (TPS.isEmpty(NodeMoves (!v)) andalso getTMInt(!degree, !v) < K andalso TS.member(precolored, !v) = false) then (  (* adding v check in precolored was pivotal *)
            freezeWorklist := TS.subtract(!freezeWorklist, !v);
            simplifyWorklist := TS.add(!simplifyWorklist, !v)
        ) else ()
    )
    end
) (NodeMoves(u))
    and
spillCost temp =
let
    (* our target is to minimize spill cost, i.e., the chosen node should have maximum neighbours and should have accessed regs/memory least number of times *)
    fun countAccess(Flow.FNODE{def, use, ...}, accessCount) = accessCount + (if TS.member(def, temp) then 1 else 0) + (if TS.member(use, temp) then 1 else 0)
    val occurenceCnt = foldl countAccess 0 (!fgraph)
in
    (Real.fromInt(occurenceCnt) / Real.fromInt (TS.numItems(Adjacent(temp))))
end
    and
SelectSpill() = 
let  
    (* Select potential temporary to spill *)
    val m = TS.foldl (
        fn (it, currentMin) => if (Real.<= (spillCost it, spillCost currentMin)) then it else currentMin
    ) (getTSElem(!spillWorklist)) (!spillWorklist) 
in  
(
    spillWorklist := TS.subtract (!spillWorklist, m);
    simplifyWorklist := TS.add (!simplifyWorklist, m);
    FreezeMoves (m)
)
end
    and
AssignColors () =
case (!selectStack) of
    n :: ns =>
    let
        val okColors = ref totalColors
    in
    (
        selectStack := ns;
        TS.app (
            fn w => if TS.member(TS.union(!coloredNodes, precolored), GetAlias(w)) then (
                okColors := colors.subtract (!okColors, TM.lookup(!color, GetAlias(w)))
            )
            else ()
        ) (getTMSet(!adjList, n));
        if colors.isEmpty(!okColors) then 
            spilledNodes := TS.add(!spilledNodes, n)
        else (
            coloredNodes := TS.add(!coloredNodes, n);
            color := TM.insert(!color, n, List.hd (colors.listItems(!okColors)))
        );
        AssignColors ()
    )
    end
  | nil => (
        TS.app (
        fn n =>
            color := TM.insert(!color, n, TM.lookup(!color, GetAlias(n)))
        ) (!coalescedNodes)
    )
    and

(* 
  procedure RewriteProgram()
    1. Allocate memory locations for each v ∈ spilledNodes,
    2. Create a new temporary vᵢ for each definition and each use,
    3. In the program (instructions), insert a store after each definition of a vᵢ, a fetch before each use of vᵢ.
    4. Put all the vᵢ into a set newTemps. 
    5. spilledNodes <- {}
    6. initial <- coloredNodes U coalescedNodes U newTemps
    7. coloredNodes <- {}
    8. coalescedNodes <- {}
*)
RewriteProgram () =
let
    val frame = valOf (!frame)
    val newTemps = ref TS.empty
    fun rewriteProgramForEachSpill (instrs, spill) =
    let
        val spilledLocation = Frame.exp (Frame.allocLocal(frame) true) (Tr.TEMP Frame.fp)

        (* Replace occurance of spill in list ls by newTemp *)
        fun replace (ls, newTemp) = List.map (fn elem => if elem = spill then newTemp else elem) ls 

        (* get count of how many times an element elem is in the list *)
        fun getCount([], elem, currentCount) = currentCount
        | getCount(l :: ls, elem, currentCount) = if l = elem then getCount(ls, elem, currentCount + 1) else getCount(ls, elem, currentCount)

        fun rewriteDef(def) = 
            if (getCount(def, spill, 0) <> 0) then (
                let 
                    val newTemp = T.newtemp()
                in 
                (
                    newTemps := TS.add (!newTemps, newTemp);
                    (Risc.codegen (frame) (Tr.MOVE(spilledLocation, Tr.TEMP newTemp)), replace (def, newTemp))
                )
            end
            ) else ([], def) 

        fun rewriteUse(use) = 
            if (getCount(use, spill, 0) <> 0) then (
                let 
                    val newTemp = T.newtemp()
                in 
                (
                    newTemps := TS.add (!newTemps, newTemp);
                    (Risc.codegen (frame) (Tr.MOVE(Tr.TEMP newTemp, spilledLocation)), replace (use, newTemp))
                )
                end
            ) else ([], use)
        (* rewrite one instruction for one spilled temp *)
        fun rewriteInstruction instr =
        case instr of
            A.OPER {assem, dst, src, jump} =>
            let 
                val (store, dst') = rewriteDef dst
                val (fetch, src') = rewriteUse src
            in 
                (fetch @ [A.OPER {assem = assem, dst = dst', src = src', jump = jump}] @ store)
            end
            | A.MOVE {assem, dst, src} =>
            let 
                val (store, [dst']) = rewriteDef [dst]
                val (fetch, [src']) = rewriteUse [src]
            in 
                (fetch @ [A.MOVE {assem = assem, dst = dst', src = src'}] @ store)
            end
            | instr => [instr]
    in
        List.foldl (fn (currentInstr, modifiedInstructionList) => modifiedInstructionList @ rewriteInstruction currentInstr) nil instrs
    end

in
(
    instrs := List.foldl (fn (spillTemp, currentInstrList) => rewriteProgramForEachSpill (currentInstrList, spillTemp)) (!instrs) (TS.listItems(!spilledNodes));
    spilledNodes := TS.empty;
    initial := TS.union (!coloredNodes, TS.union (!coalescedNodes, !newTemps));
    coloredNodes := TS.empty;
    coalescedNodes := TS.empty
)
end

(* -- Structure End -- *)
end