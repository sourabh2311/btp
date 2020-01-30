signature LIVENESS =
sig
  val liveness : Flow.flowgraph -> unit
end

structure Liveness :> LIVENESS =
struct
(* ---- Structure Begin ---- *)
structure F = Flow
structure TS = Temp.TempSet

fun liveness flowgraph = 
let 
  val changed = ref false 

  fun updateNodeLiveness (F.FNODE {def, use, succ, liveIn, liveOut, ...}) = 
  let 
    val oldInCnt = TS.numItems (!liveIn)
    val oldOutCnt = TS.numItems (!liveOut)
  in
  (
    liveIn := TS.union(use, TS.difference(!liveOut, def));
    liveOut := foldl (fn (F.FNODE{liveIn, ...}, currSet) => TS.union (!liveIn, currSet)) TS.empty (!succ);
    if oldInCnt = TS.numItems (!liveIn) andalso oldOutCnt = TS.numItems (!liveOut)
    then () else changed := true
  )
  end

  fun updateLiveness () = app updateNodeLiveness flowgraph

  fun LivenessFixPoint () = 
    (changed := false;
    updateLiveness (); 
    if !changed then LivenessFixPoint () else ())
in 
  LivenessFixPoint () 
end

(* ---- Structure End ---- *)
end