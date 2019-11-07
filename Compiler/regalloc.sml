structure RegAlloc : REG_ALLOC =
struct
structure A = Assem
structure Frame = RiscFrame
structure T = Temp
structure TT = T.Table
structure Tr = Tree
structure L = Liveness
type allocation = Frame.register Temp.Table.table

(* get count of how many times an element elem is in the list *)
fun getCount([], elem, currentCount) = currentCount
  | getCount(l :: ls, elem, currentCount) = if l = elem then getCount(ls, elem, currentCount + 1) else getCount(ls, elem, currentCount)

(* Colors *)
structure colors = RedBlackSetFn(
    type ord_key = Frame.register
    fun compare (r1, r2) = String.compare(r1, r2)
)
(* color our interference graph given spill cost of each node, available registers for coloring and map of initially colored nodes  *)
fun color {interference, initAlloc, spillCost, registers} =
let
    (* is spill cost of a <= b? *)
    fun less(L.NODE{temp = temp1, adj = adj1}, L.NODE{temp = temp2, adj = adj2}) = if (Real.<= (spillCost temp1 (List.length(!adj1)), spillCost temp2 (List.length(!adj2)))) then true else false
    (* Get minimum in list *)
    fun getMinimum(min, []) = min
      | getMinimum(min, l :: ls) = if (less(l, min)) then getMinimum(l, ls) else getMinimum(min, ls)
    (* Nodes which we have to remove from graph, for them we will always be able to find colors *)
    val simplify = ref nil 
    (* Nodes which we may potentially spill *)
    val potentialSpill = ref nil 
    (* Actual spilled nodes *)
    val actualSpill = ref nil 
    (* Nodes which have been simplified *)
    val stack = ref nil
    (* Mapping each element to its degree, which will change as we psuedo remove the nodes from graph *)
    val degreeMap = ref TT.empty 
    (* We will return this which will contain new color map *)
    val colorTable = ref TT.empty
    (* Total colors available for coloring *)
    val totalColors = colors.addList(colors.empty, registers)
    (* Number of total colors available for coloring *)
    val K = List.length registers

    (* Delete an item (toDelete) from the list (givenList) *)
    fun deleteFromList givenList toDelete = List.filter (fn (item) => item <> toDelete) givenList

    (* Select potential temporary to spill *)
    fun selectSpill() = 
    let 
      val min = getMinimum(List.hd(!potentialSpill), List.tl(!potentialSpill))
    in  
      (simplify := [min], potentialSpill := deleteFromList (!potentialSpill) min)
    end

    fun look(givenMap, L.NODE{temp, ...}) = valOf(TT.look(givenMap, temp))

    fun insertNodeDegree(L.NODE{temp, adj, ...}) = degreeMap := TT.enter(!degreeMap, temp, List.length (!adj))

    fun mapNodesDegree() = app insertNodeDegree interference 


    fun copyInitAlloc () = app (fn (temp, color) => (colorTable := TT.enter(!colorTable, temp, color))) (TT.listItemsi(initAlloc))

    fun findUncolored () = List.filter (fn L.NODE{temp, adj} => not (TT.inDomain(!colorTable, temp))) interference 


    fun getStartWorkingList () = 
    let 
        val (simplify', potentialSpill') = List.partition (fn node => look(!degreeMap, node) < K) (copyInitAlloc(); findUncolored())
    in 
        (simplify := simplify'; potentialSpill := potentialSpill')
    end 
    
    (* Remember that we shouldn't recolor already colored nodes *)
    fun decrementDegree (node as L.NODE{temp, adj}) = if TT.inDomain(initAlloc, temp) then () else (
        degreeMap := TT.enter(!degreeMap, temp, look(!degreeMap, node) - 1);
        if (look(!degreeMap, node) + 1 = K) then (simplify := !simplify @ [node]; potentialSpill := (deleteFromList (!potentialSpill) node)) else ()
    )
    (* Check whether this neighbour is in stack *)
    fun neighbourNotInStack (neighbour) = List.all (fn stackNode => stackNode <> neighbour) (!stack)
    (* Find neighbours which are not in stack *)
    fun neighboursNotInStack (L.NODE{adj, ...}) = List.filter neighbourNotInStack (!adj)
    (* fun neighboursNotInStack (L.NODE{adj, ...}) = colors.listItems(colors.difference(colors.addList(colors.empty, (!adj)), colors.addList(colors.empty, (!stack)))) *)

    fun simplifyGraph () =
    case (!simplify) of
        nil => ()
        | l :: ls => (simplify := ls; List.app (fn n => decrementDegree n) (neighboursNotInStack(l)); stack := l :: (!stack); simplifyGraph ())

    fun neighboursColor(nil, colorSet) = colorSet 
      | neighboursColor((neighbour as L.NODE{temp, ...}) :: neighbours, colorSet) = if TT.inDomain(!colorTable, temp) then neighboursColor(neighbours, colors.add(colorSet, look(!colorTable, neighbour))) else neighboursColor(neighbours, colorSet) 

    fun assignColors () =
    case (!stack) of
        nil => ()
        | L.NODE {temp = n, adj, ...} :: ns =>
        let
            val availableColors = colors.difference(totalColors, neighboursColor(!adj, colors.empty))
        in
            stack := ns;
            if colors.isEmpty(availableColors) then (actualSpill := n :: (!actualSpill); assignColors())
            else
                (colorTable := TT.enter(!colorTable, n, List.hd(colors.listItems(availableColors))); assignColors())
        end

    (* iterate till there are no potential spills *)
    fun iter () = (
        simplifyGraph ();
        if List.length(!potentialSpill) = 0 then ()
        else (selectSpill(); iter ())
    )
in
    mapNodesDegree();
    getStartWorkingList();
    iter();
    assignColors();
    (!colorTable, !actualSpill)
end

(* 
  procedure RewriteProgram()
    1. Allocate memory locations for each v ∈ spilledNodes,
    2. Create a new temporary vᵢ for each definition and each use,
    3. In the program (instructions), insert a store after each definition of a vᵢ, a fetch before each use of vᵢ.
    4. Put all the vᵢ into a set newTemps. 
*)
fun rewriteProgram (instrs, frame, spills) =
let
    fun rewriteProgramForEachSpill (instrs, spill) =
    let
        val spilledLocation = Frame.exp (Frame.allocLocal(frame) true) (Tr.TEMP Frame.fp)

        (* Replace occurance of spill in list ls by newTemp *)
        fun replace (ls, newTemp) = List.map (fn elem => if elem = spill then newTemp else elem) ls 

        fun rewriteDef(def) = 
            if (getCount(def, spill, 0) <> 0) then (
            let 
                val newTemp = T.newtemp()
            in 
                (Risc.codegen (frame) (Tr.MOVE(spilledLocation, Tr.TEMP newTemp)), replace (def, newTemp))
            end
            ) else ([], def) 

        fun rewriteUse(use) = 
            if (getCount(use, spill, 0) <> 0) then (
            let 
                val newTemp = T.newtemp()
            in 
                (Risc.codegen (frame) (Tr.MOVE(Tr.TEMP newTemp, spilledLocation)), replace (use, newTemp))
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
    List.foldl (fn (spillTemp, currentInstrList) => rewriteProgramForEachSpill (currentInstrList, spillTemp)) instrs spills
end

fun alloc (instrs, frame) =
let

    val graph = MakeGraph.instrs2graph instrs
    val igraph = Liveness.interferenceGraph graph

    (* our target is to minimize spill cost, i.e., the chosen node should have maximum neighbours and should have accessed regs/memory least number of times *)
    fun spillCost temp neighbours =
    let
        fun countAccess(Flow.FNODE{def, use, ...}, accessCount) = accessCount + getCount(use, temp, 0) + getCount(def, temp, 0)
        val occurenceCnt = foldl countAccess 0 graph
    in
        (Real.fromInt(occurenceCnt) / Real.fromInt(neighbours))
    end

    val (allocationTable, spills) = color {interference = igraph, initAlloc = Frame.tempMap, spillCost = spillCost, registers = Frame.registers}

    (* Some moves may become redundant after allocation *)
    fun isRedundant instr =
        case instr of
            A.MOVE{assem, dst, src} => valOf(TT.look(allocationTable, dst)) = valOf(TT.look(allocationTable, src))
            | _ => false

in
    if List.length spills = 0 then (List.filter (fn instr => not (isRedundant instr)) instrs, allocationTable)
    (* Need to again do coloring *)
    else alloc (rewriteProgram(instrs, frame, spills), frame)
end


end