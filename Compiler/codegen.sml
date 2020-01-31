signature CODEGEN = 
sig
  structure F : FRAME
  val codegen : F.frame -> Tree.stm -> Assem.instr list
end


structure Risc : CODEGEN = 
struct
  structure F = RiscFrame
  structure T = Tree
  structure A = Assem
  structure S = Symbol
  exception ArgExceed of string
  fun codegen (frame) (stm: Tree.stm) : A.instr list = 
  let 
    val ilist = ref [] : A.instr list ref
    (* Each time we will be emitting, and in the end we will return reverse of this ilist *)
    fun emit x = ilist := x :: !ilist
    (* When we will be doing munchExp, we would want to return the temporary having that value (and ofcourse emitting our instruction), but such a temporary doesn't exist, and therefore this "gen" function will take the newly created temporary, pass it to "gen" (which will emit the instruction) and then we will simply return this temporary as this is used in filling up src list aswell as destination list. *)
    fun result (gen) = let val t = Temp.newtemp() in gen t; t end

    (* Self explanatory *)
    fun origBranchRelopStr(relop) = 
    (case relop of 
      T.EQ => "beq"
    | T.NE => "bne"
    | T.LT => "blt"
    | T.GT => "bgt"
    | T.LE => "ble"
    | T.GE => "bge"
    | T.ULT => "bltu"
    | T.ULE => "bgtu"
    | T.UGT => "bleu"
    | T.UGE => "bgeu")

    (* Self explanatory *)
    fun negBranchRelopStr(relop) = 
    (case relop of 
      T.EQ => "bne"
    | T.NE => "beq"
    | T.LT => "bge"
    | T.GT => "ble"
    | T.LE => "bgt"
    | T.GE => "blt"
    | T.ULT => "bgeu"
    | T.ULE => "bgtu"
    | T.UGT => "bleu"
    | T.UGE => "bltu")

    (* Self explanatory *)
    fun intToString i = if i >= 0 then Int.toString i else "-" ^ Int.toString(~i)

    (* Self explanatory *)
    fun getBinopString T.PLUS = "add"
    | 	getBinopString T.MINUS = "sub"
    | 	getBinopString T.MUL = "mul"
    | 	getBinopString T.DIV = "div"
    | 	getBinopString T.AND = "and"
    | 	getBinopString T.OR = "or"
    |   getBinopString T.LSHIFT = "sll"
    |   getBinopString T.RSHIFT = "srl"
    |   getBinopString T.ARSHIFT = "sra"
    |   getBinopString T.XOR = "xor"

    (* Note: Everything is always regisiter *)
    fun emitMoveInstr(srcTemp, dstTemp) =  
      emit(A.MOVE {assem = "mv `d0, `s0\n",
                  src = srcTemp,
                  dst = dstTemp})

        (* No SEQ *)
        (* T.LABEL *)
	  fun munchStm(T.LABEL(label)) = emit(A.LABEL {assem = (S.name label ^ ":\n"), lab = label})

        (* T.JUMP *)
        (* Unconditionally jump to the instruction whose address is in register s0. *)
      | munchStm(T.JUMP(T.TEMP ra, _)) = 
        emit(A.OPER {assem = "jr `s0\n",
                    src = [ra],
                    dst = [],
                    jump = NONE})

        (* Unconditionally jump to the instruction at the label. *)
      | munchStm(T.JUMP(T.NAME labelName, l :: rest)) = 
        emit(A.OPER {assem = "j `j0\n", 
                    src = [],
                    dst = [],
                    jump = SOME(l :: rest)})

        (* T.CJUMP *)
      | munchStm(T.CJUMP(relop, exp1, exp2, tlabel, flabel)) = 
          emit(A.OPER {assem = origBranchRelopStr(relop) ^ " `s0, `s1, `j0\n\t" ^ negBranchRelopStr(relop) ^ " `s0, `s1, `j1\n",
          src = [munchExp(exp1), munchExp(exp2)],
          dst = [],
          jump = SOME([tlabel, flabel])})

        (* T.MOVE *)
        (* dst and src are both registers *)
      | munchStm(T.MOVE(T.TEMP r1, T.TEMP r2)) = emitMoveInstr(r2, r1)

        (* li Rdest, imm		Load Immediate†
           Move the immediate into register Rdest. *)
        (* li instructions *)
      | munchStm(T.MOVE(T.TEMP r, T.CONST i)) = emit(A.OPER {assem = "li `d0, " ^ intToString i ^ "\n",
                                                      src = [],
                                                      dst = [r],
                                                      jump = NONE})
        (* dst is a register *)
      | munchStm(T.MOVE(T.TEMP r , exp)) = emitMoveInstr(munchExp(exp), r)

        (* sw Rsource, address		Store Word
           Store the word from register Rsource at address. *)
      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), exp)) =
          emit(A.OPER {assem = "sw `s0, " ^ intToString i ^ "(`s1)\n",
                      src = [munchExp exp, munchExp e1],
                      dst = [],
                      jump = NONE})
          
        (* Same as above *)
      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), exp)) =
          emit(A.OPER {assem = "sw `s0, " ^ intToString i ^ "(`s1)\n",
                      src = [munchExp exp, munchExp e1],
                      dst = [],
                      jump = NONE})

      | munchStm(T.MOVE(T.MEM(e1), e2)) =
          emit(A.OPER {assem = "sw `s0, 0(`s1)\n",
                      src = [munchExp e2, munchExp e1],
                      dst = [],
                      jump = NONE})

      | munchStm(T.MOVE(exp1, exp2)) = emitMoveInstr(munchExp(exp2), munchExp(exp1))

        (* T.EXP *)
      | munchStm(T.EXP exp) = (munchExp(exp); ())


        (* T.BINOP start *)
        (* addi *)
	  and munchExp(T.BINOP(T.PLUS, T.CONST 0, T.CONST i)) =
        result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [F.zero], 
                                    dst = [r], 
                                    jump = NONE}))

    |   munchExp(T.BINOP(T.PLUS, T.CONST i, T.CONST 0)) =
        result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [F.zero], 
                                    dst = [r], 
                                    jump = NONE}))

    |   munchExp(T.BINOP(T.MINUS, T.CONST 0, T.CONST i)) =
        result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString (~i) ^ "\n",
                                    src = [F.zero], 
                                    dst = [r], 
                                    jump = NONE}))

    |	  munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
        result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [munchExp e1], 
                                    dst = [r], 
                                    jump = NONE}))
    | 	munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
        result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [munchExp e1],
                                    dst = [r],
                                    jump = NONE}))
        (* There is nothing like subi, only addi *)
    | 	munchExp(T.BINOP(T.MINUS, e1, T.CONST i)) =
                result(fn r => emit(A.OPER {assem = "addi `d0, `s0, " ^ intToString (~i) ^ "\n",
                                            src = [munchExp e1], 
                                            dst = [r], 
                                            jump = NONE}))
        (* andi *)
    | 	munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
        result(fn r => emit(A.OPER {assem = "andi `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [munchExp e1], 
                                    dst = [r], 
                                    jump = NONE}))
    | 	munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
            result(fn r => emit(A.OPER {assem = "andi `d0, `s0, " ^ intToString i ^ "\n",
                                        src = [munchExp e1], 
                                        dst = [r], 
                                        jump = NONE}))
        (* ori *)
    | 	munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
            result(fn r => emit(A.OPER {assem = "ori `d0, `s0, " ^ intToString i ^ "\n",
                                        src = [munchExp e1], 
                                        dst = [r], 
                                        jump = NONE}))
    | 	munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
        result(fn r => emit(A.OPER {assem="ori `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [munchExp e1], 
                                    dst = [r], 
                                    jump = NONE}))
        (* left shift *)
    | 	munchExp(T.BINOP(T.LSHIFT, e1, T.CONST i)) =
        result(fn r => emit(A.OPER {assem = "slli `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [munchExp e1], 
                                    dst = [r], 
                                    jump = NONE}))
        (* right shift *)
    | 	munchExp(T.BINOP(T.RSHIFT, e1, T.CONST i)) =
        result(fn r => emit(A.OPER {assem = "srli `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [munchExp e1], 
                                    dst = [r], 
                                    jump = NONE}))

        (* right arithmetic shift *)
    | 	munchExp(T.BINOP(T.ARSHIFT, e1, T.CONST i)) =
        result(fn r => emit(A.OPER {assem = "srai `d0, `s0, " ^ intToString i ^ "\n",
                                    src = [munchExp e1], 
                                    dst = [r], 
                                    jump = NONE}))

        (* rest of binops *)
    | 	munchExp(T.BINOP(binop, e1, e2)) =
        result(fn r => emit(A.OPER {assem = getBinopString(binop) ^ " `d0, `s0, `s1\n",
                                            src = [munchExp e1, munchExp e2], 
                                            dst = [r], 
                                            jump = NONE}))
        (* T.MEM start *)
    |   munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
		 		result(fn r => emit(A.OPER {assem = "lw `d0, " ^ intToString i ^ "(`s0)\n",
		 		 							src = [munchExp e1], 
		 		 							dst = [r], 
		 		 							jump = NONE}))
    | 	munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = 
            result(fn r => emit(A.OPER {assem = "lw `d0, " ^ intToString i ^ "(`s0)\n",
                                        src = [munchExp e1], 
                                        dst = [r], 
                                        jump = NONE}))
        (* T.MEM *)
    | 	munchExp (T.MEM e1) = 
        result(fn r => emit(A.OPER {assem = "lw `d0, 0(`s0)\n",
                                        src = [munchExp e1], 
                                        dst = [r], 
                                        jump = NONE}))
        (* T.TEMP *)
    | 	munchExp (T.TEMP t) = t

        (* No ESEQ *)
        (* T.NAME *)
        (* la Rdest, address			Load Address
           Load computed address, not the contents of the location; into register Rdest. *)
    |	  munchExp (T.NAME l) =
            result(fn r => emit(A.OPER {assem = ("la `d0, " ^ S.name(l) ^ "\n"),
                                        src = [], 
                                        dst = [r], 
                                        jump = NONE}))
        (* T.CONST *)
    | 	munchExp (T.CONST c) = 
            result(fn r => emit(A.OPER {assem = "li `d0, " ^ intToString c ^ "\n",
                                        src = [],
                                        dst = [r],
                                        jump = NONE}))
        (* T.CALL *)
        (* 
          The following steps are necessary to effect a call:

          1. Pass the arguments. By convention, the first eight arguments are passed in registers $a0--$a7 (though simpler compilers may choose to ignore this convention and pass all arguments via the stack). The remaining arguments are pushed on the stack.
          2. Save the caller-saved registers. This includes registers $t0--$t7, if they contain live values at the call site.
          3. Execute a jal instruction. 
        *)
    | 	munchExp (T.CALL (lexp, args, formals)) = 
        let
            val label = (case lexp of
                        T.NAME l => l
                        | _ => ErrorMsg.impossible "Compiler Bug! Calling expression is not NAME")
            (* To check whether function being called is external or not and also given frame needn't be the frame of the called function *)
            val formals' = AccessConv.treeToFrame(formals)
            val escapes = F.getEscapes(formals')
            val argTemps = if (length formals' = 0) then 
            (
              ListPair.map (fn (reg, arg) => (munchStm(T.MOVE(T.TEMP reg, arg)); reg)) (List.take(F.getFirstL F.argregs, length args), args)
            ) else munchArgs(0, args, formals', escapes)

        in
            (emit(A.OPER {
                assem = "jal " ^ S.name(label) ^ "\n",
                src = argTemps,	
                (* All these dst registers are named, i.e. they are not arbitrary temporaries but are already mapped to actual machine regs, thus if the function being called wants to use them, our register allocation will handle it. Also see highlighted portion of page 237. *)
                dst = F.getFirstL (F.calldefs),	
                jump = NONE});
            F.rv)
        end
    |   munchExp (T.ESEQ(_, _)) = ErrorMsg.impossible "Compiler Bug! There shouldn't be any ESEQ's..."

	  and munchArgs (i, [], [], escapes) = []
      (* | munchArgs (i, x, []) = (print("IMPOSSIBLE\n"); []) *)
      (* | munchArgs (i, [], x) = (print("IMPOSSBILE\n"); []) *)
	    | munchArgs(i, a :: l, frameFormal :: frameFormals, escapes) = 
        if F.inReg(frameFormal) then 
          (
            let 
              val argDst = List.nth((F.getFirstL F.argregs), i)
            in 
              (munchStm(T.MOVE(T.TEMP argDst, a));
              argDst :: munchArgs(i + 1, l, frameFormals, escapes))
            end
          )
          else
          (
            let 
              val dst = F.callexp (frameFormal) (T.TEMP F.sp) escapes
            in 
              munchStm(T.MOVE(dst, a));
              munchArgs(i, l, frameFormals, escapes)
            end
          )
	in 
		munchStm stm; rev(!ilist)
	end
end