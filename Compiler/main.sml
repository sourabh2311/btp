structure Main = struct

    structure Tr = Translate
    structure F = RiscFrame
    structure R = RegAlloc
    structure A = Assem
    fun getsome (SOME x) = x
      | getsome (_) = ErrorMsg.impossible "Compiler Bug! Error during getSome in MainGiven..."

    (* Get a name of the temporary from the allocationTable received from register allocator *)
    fun tempName allocationTable temp =
    case Temp.Table.look(allocationTable, temp) of
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
        (* val _ = Printtree.printtree(out,body); *)
        val stms = Canon.linearize body
        (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule(Canon.optimizeBasicBlocks(Canon.basicBlocks stms))
        val instrs = List.concat(map (Risc.codegen frame) stms') 
        val instrs2 = F.procEntryExit2 (frame, instrs)
        (* val format1 = Assem.format(F.getTempString) *)
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
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out, (Symbol.name lab) ^ ": " ^ s ^ "\n")


    fun emitstr out (F.STRING(lab, str)) = TextIO.output(out, F.genString(lab, str))

    fun withOpenFile fname f = 
    let 
        val out = TextIO.openOut fname
    in 
        (f out before TextIO.closeOut out) 
        handle e => (TextIO.closeOut out; raise e)
    end 

    fun compile filename = 
    let 
        val absyn = Parse.parse filename
        (* FindEscape may fail in case there are some errors, in which case it is better to first make sure that we don't have these semantic errors *)
        val frags = Semant.transProg absyn 
        val frags = if (!ErrorMsg.anyErrors) then frags else (FindEscape.findEscape absyn; Semant.transProg absyn)
        (* val frags = (FindEscape.findEscape absyn; Semant.transProg absyn) *)
        val (proc, strs) = List.partition 
                          (
                            fn (x) => 
                              case x of 
                                  F.PROC (_) => true
                                | _ => false
                          ) frags

    in 
        withOpenFile (filename ^ ".s") 
        (fn out => (
                    TextIO.output(out, "\n.globl main\n");
                    TextIO.output(out, ".data\n");
                    app (emitstr out) strs;
                    TextIO.output(out, "\n.text\n");
                    app (emitproc out) proc
                   )
        )
    end

end
