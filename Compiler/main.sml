structure Main = struct
(* -------------- Compile Time Flags ----------------- *)
    exception InvalidFlag
    val stringAlgo = ref 0
    val ir = ref TextIO.stdOut
    val bacode = ref TextIO.stdOut 
(* --------------  ----------------- *)
    structure Tr = Translate
    structure F = RiscFrame
    structure R = RegAlloc
    structure A = Assem
    structure TM = Temp.TempMap
    fun getsome (SOME x) = x
      | getsome (_) = ErrorMsg.impossible "Compiler Bug! Error during getSome in MainGiven..."

    (* Get a name of the temporary from the allocationTable received from register allocator *)
    fun tempName allocationTable temp =
    case TM.find(allocationTable, temp) of
        SOME(r) => r
      | NONE => ErrorMsg.impossible "there is a register unallocated!"

    fun indent instrs =
    map (fn (i) =>
            case i of
              l as A.LABEL _ => l
            | A.OPER{assem, src, dst, jump} => A.OPER{assem = "\t" ^ assem, src = src, dst = dst,jump = jump}
            | A.MOVE{assem, dst, src} => A.MOVE{assem = "\t" ^ assem, src = src, dst = dst}
        ) instrs

    fun emitproc out (F.PROC{body, frame}) =
    let 
        val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")
        val _ = TextIO.output(!ir, "---------- IR Tree ---------\n")
        val _ = Printtree.printtree(!ir, body);
        val stms = Canon.linearize body
        val stms' = Canon.traceSchedule(Canon.optimizeBasicBlocks(Canon.basicBlocks stms))
        val _ = TextIO.output(!ir, "---------- IR Tree (After Canon) ---------\n")
        val _ = app (fn s => Printtree.printtree(!ir, s)) stms;
        val instrs = List.concat(map (Risc.codegen frame) stms') 
        val instrs2 = F.procEntryExit2 (frame, instrs)
        val _ = TextIO.output(!bacode, "---------- Instr before allocation ---------\n")
        val format1 = Assem.format(F.getTempString)
        val _ = app (fn i => TextIO.output(!bacode, format1 i)) instrs2
        val (instrs2', allocationTable) = R.alloc(instrs2, frame)
        val {prolog, body, epilog} = F.procEntryExit3(frame, instrs2')
        val body' = indent body
        val format0 = Assem.format(tempName allocationTable)
    in  
        TextIO.output(out, prolog);
        (* TextIO.output(out, "\nsourabh\n"); *)
        app (fn i => TextIO.output(out, (format0 i))) body';
        TextIO.output(out, epilog)
        (* app (fn i => TextIO.output(out,format0 i)) instrs *)
    end

    fun emitothers out (F.STRING (lab, str)) = TextIO.output(out, F.genString(lab, str))
    | emitothers out (F.REAL(lab, r)) = TextIO.output(out, F.genReal(lab, r)) 

    fun withOpenFile fname f = 
    let 
        val out = TextIO.openOut fname
    in 
        (f out before TextIO.closeOut out) 
        handle e => (TextIO.closeOut out; raise e)
    end 
    fun appendRuntime fname = 
    let 
        val rin = TextIO.openIn "runtime.s" 
        val out = TextIO.openAppend fname
        fun loop rin = 
        case TextIO.inputLine rin of 
            SOME line => (TextIO.output(out, line); loop rin)
          | NONE      => ()
    in 
        loop rin before (TextIO.closeIn rin; TextIO.closeOut out) 
    end
    fun compile (filename, flags) = 
    let 
        
        fun getFlags (l :: ls) = 
        (
          if String.isPrefix "sa=" l then stringAlgo := valOf(Int.fromString(String.extract(l, 3, NONE)))
          else if String.isPrefix "ir=" l then ir := TextIO.openOut (String.extract(l, 3, NONE)) 
          else if String.isPrefix "ba=" l then bacode := TextIO.openOut (String.extract(l, 3, NONE))
          else raise InvalidFlag;
          getFlags (ls)
        )
          | getFlags ([]) = ()
        val _ = getFlags (flags)
        val _ = if (!stringAlgo < 0 orelse !stringAlgo > 1) then OS.Process.exit (OS.Process.failure) else ()
        val _ = (Semant.stringAlgorithm := !stringAlgo)
        val absyn = Parse.parse filename
        val _ = ClassOffsets.classOffsets(absyn)
        val _ = if (!ErrorMsg.anyErrors) then (print ("Unsuccessful compilation due to above errors\n"); OS.Process.exit (OS.Process.failure)) else ()
        (* FindEscape may fail in case there are some errors, in which case it is better to first make sure that we don't have these semantic errors *)
        val frags = Semant.transProg absyn 
        val _ = if (!ErrorMsg.anyErrors) then (print ("Unsuccessful compilation due to above errors\n"); OS.Process.exit (OS.Process.failure)) else ()
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        (* val frags = (FindEscape.findEscape absyn; Semant.transProg absyn) *)
        val (proc, others) = List.partition 
                              (
                                fn (x) => 
                                  case x of 
                                      F.PROC (_) => true
                                    | _ => false
                              ) frags
    in 
      (
        withOpenFile (filename ^ ".s") 
        (fn out => (
                    TextIO.output(out, "\n.globl main\n");
                    TextIO.output(out, ".data\n");
                    app (emitothers out) others;
                    TextIO.output(out, "\n.text\n");
                    app (emitproc out) proc
                   )
        );
        appendRuntime (filename ^ ".s")
      )
    end

end
