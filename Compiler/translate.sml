structure Translate : TRANSLATE =
struct

structure Tr = Tree
structure T = Temp
structure F = RiscFrame
structure A = Absyn

(* Basic Definitions *)
datatype level =  Top
								| Lev of {parent: level, frame: F.frame} * unit ref

datatype exp =  Ex of Tr.exp
							| Nx of Tr.stm
							| Cx of Temp.label * Temp.label -> Tr.stm

type expty = {exp : exp, ty : Types.ty}

type access = level * F.access

type frag = F.frag

val outermost = Top  

val fragments : frag list ref = ref nil

val errExp = Ex(Tr.CONST 0)

(* Helper Functions *)

fun getResult () = !fragments

fun reset () = fragments := nil

fun memPlus (e1, e2) = Tr.MEM(Tr.BINOP(Tr.PLUS, e1, e2))

fun rmemPlus (e1, e2) = Tr.RMEM(Tr.BINOP(Tr.PLUS, e1, e2))

(* Remember that we should pass the static link for which we have added its escape *)
fun newLevel {parent, name, formals, isRealL} = Lev({parent = parent, frame = F.newFrame {name = name, formals = true :: formals, isRealL = false :: isRealL}}, ref ())

fun allocLocal (lev as Lev({parent, frame}, _)) escape isReal = (lev, F.allocLocal frame escape isReal)

fun getBinOper(oper) = 
case oper of 
		A.PlusOp => Tr.PLUS
	| A.MinusOp => Tr.MINUS
	| A.TimesOp => Tr.MUL
	| A.DivideOp => Tr.DIV
	| A.LShift => Tr.LSHIFT
	| A.RShift => Tr.RSHIFT

fun getRBinOper(oper) = 
case oper of 
		A.PlusOp => Tr.RPLUS
	| A.MinusOp => Tr.RMINUS
	| A.TimesOp => Tr.RMUL
	| A.DivideOp => Tr.RDIV

fun getRelOper(oper) = 
case oper of 
	  A.EqOp => Tr.EQ
	| A.NeqOp => Tr.NE
	| A.LtOp => Tr.LT
	| A.LeOp => Tr.LE
	| A.GtOp => Tr.GT
	| A.GeOp => Tr.GE
	
fun formals Top = nil 
	| formals (lev as Lev({parent, frame}, _)) = map (fn formal => (lev, formal)) (List.tl (F.formals frame))

fun seq stmlist =
	case stmlist of
		[s] => s
	| [s1, s2] => Tr.SEQ(s1, s2)
	| stm :: rest => Tr.SEQ(stm, seq(rest))


fun unEx (Ex e) = e 
	| unEx (Cx genstm) = 
let 
	val r = Temp.newtemp(0) 
	val t = Temp.newlabel() and f = Temp.newlabel() 
in
	Tr.ESEQ(seq[Tr.MOVE(Tr.TEMP r, Tr.CONST 1), 
				genstm(t, f), 
				Tr.LABEL f, 
				Tr.MOVE(Tr.TEMP r, Tr.CONST 0), 
				Tr.LABEL t], 
			Tr.TEMP r) 
end 
	| unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0)

fun unCx (Cx c) = c
	| unCx (Ex (Tr.CONST 0)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(flabel), [flabel]))
	| unCx (Ex (Tr.CONST 1)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(tlabel), [tlabel]))
	| unCx (Ex e) = (fn(tlabel, flabel) => Tr.CJUMP(Tr.EQ, Tr.CONST 0, e, flabel, tlabel))
	| unCx (Nx _) = (ErrorMsg.impossible "Compiler bug! unCx a Nx"; fn (a, b) => Tr.LABEL(Temp.newlabel()))

fun unNx (Ex e) = Tr.EXP(e)
	| unNx (Nx n) = n
	| unNx (c) = unNx(Ex(unEx(c)))


val nilexp = Ex(Tr.CONST(0))
fun intlit (n) = Ex(Tr.CONST (n))
fun reallit (r : real) : exp = 
let 
	val there = List.find
		(fn (fragment) =>
			case fragment of
				F.PROC _ => false
			| F.STRING _ => false
			| F.REAL(_, r') => Real.== (r, r')) (!fragments)
in 
	case there of
			NONE =>
			let 
				val l = Temp.newlabel() 
			in
				(fragments := F.REAL(l, r) :: !fragments; Ex(Tr.RMEM (Tr.NAME(l))))
			end
		| SOME(F.REAL(lab, _)) => Ex(Tr.RMEM (Tr.NAME(lab)))
end
fun strlit (s: string) : exp =
let 
	val there = List.find
		(fn (fragment) =>
			case fragment of
				F.PROC _ => false
			| F.REAL _ => false
			| F.STRING(_, s') => s = s') (!fragments)
in 
	case there of
			NONE =>
			let 
				val l = Temp.newlabel() 
			in
				(fragments := F.STRING(l, s) :: !fragments; Ex(Tr.NAME(l))) 
			end
		| SOME(F.STRING(lab, _)) => Ex(Tr.NAME(lab))
end

fun getLevelsFPUsingStaticLink (Top, _, frameAddress) = ErrorMsg.impossible "You have issue with Static Links"
  | getLevelsFPUsingStaticLink (_, Top, frameAddress) = ErrorMsg.impossible "You have issue with Static Links"
	| getLevelsFPUsingStaticLink (Lev (ignore, targetLevelRef), Lev ({parent, frame}, curLevelRef), frameAddress) = 
		if (curLevelRef = targetLevelRef) then frameAddress
		else getLevelsFPUsingStaticLink (Lev (ignore, targetLevelRef), parent, (F.exp (List.hd(F.formals frame)) frameAddress false))

fun simpleVar (targetLevelAccess, curLevel, isReal) =
let 
	val (targetLevel, frameAccess) = targetLevelAccess  (* This is our target to reach *)
	(* The passed level need not be our target level *)
	val targetLevelsFP = getLevelsFPUsingStaticLink (targetLevel, curLevel, Tr.TEMP (F.fp))
in 
	Ex(F.exp frameAccess targetLevelsFP isReal)
end

(* Just the way given in text. *)
fun subscriptVar (base, offset) = Ex(memPlus(unEx(base), Tr.BINOP(Tr.MUL, unEx(offset), Tr.CONST(F.wordSize))))

(* Just the way given in text. *)
fun fieldVar (base, id, SL) =
let 
	fun findElem (index : int, elem : Symbol.symbol, l :: ls : ((Symbol.symbol * Types.ty) list)) =
		if elem = (#1 l) then (index, (#2 l))
		else findElem(index + 1, elem, ls) 
	val (index, ty) = findElem (0, id, SL)
in
	if Types.isReal(ty) then 
		Ex(rmemPlus(unEx(base), Tr.BINOP(Tr.MUL, Tr.CONST(index), Tr.CONST(F.wordSize))))
	else 
		Ex(memPlus(unEx(base), Tr.BINOP(Tr.MUL, Tr.CONST(index), Tr.CONST(F.wordSize))))
end

fun binop (oper, e1, e2, flag') =
let
	val left = unEx(e1)
	val right = unEx(e2)
	val treeop = if flag' = 1 then getRBinOper(oper) else getBinOper(oper)
in 
	Ex(Tr.BINOP(treeop, left, right))
end

fun relop (oper, e1, e2) =
let
	val left = unEx(e1)
	val right = unEx(e2)
	val treeop = getRelOper(oper)
in 
	Cx((fn (t, f) => Tr.CJUMP(treeop, left, right, t, f))) 
end


fun ifelse (testexp, thenexp, elseexp) =
let
	val neotest  = unCx(testexp) 
	val neothen = unEx (thenexp)
	val neoelseexp = unEx (elseexp)
	val result = Temp.newtemp(0) 
	val tlabel = Temp.newlabel()
	val flabel = Temp.newlabel()
	val finish = Temp.newlabel()
in
	Ex(Tr.ESEQ(seq[neotest(tlabel, flabel),
	Tr.LABEL tlabel, Tr.MOVE(Tr.TEMP result, neothen),
	Tr.JUMP (Tr.NAME finish, [finish]),
	Tr.LABEL flabel, Tr.MOVE(Tr.TEMP result, neoelseexp),
	Tr.JUMP (Tr.NAME finish, [finish]),
	Tr.LABEL finish],
	Tr.TEMP result))
end

fun record (fieldsE, fieldsT) =
let
	val r = Temp.newtemp(0)
	val init =
			Tr.MOVE(
			  Tr.TEMP r, 
				Tr.CALL(Tr.NAME (T.namedlabel "allocRecord"), [Tr.CONST(length(fieldsE) * F.wordSize)], AccessConv.frameToTree(F.dummyFormals(1, [])), [false], false)
			)
	fun loop ([], [], index) = nil 
		|	loop (fieldE :: fieldsE, fieldT :: fieldsT, index) = 
				if Types.isReal(fieldT) then 
					Tr.RMOVE(memPlus(Tr.TEMP r, Tr.CONST(index * F.wordSize)), unEx(fieldE)) :: loop(fieldsE, fieldsT, index + 1)
				else 	
					Tr.MOVE(memPlus(Tr.TEMP r, Tr.CONST(index * F.wordSize)), unEx(fieldE)) :: loop(fieldsE, fieldsT, index + 1)
in 
	Ex(Tr.ESEQ(seq(init :: loop(fieldsE, fieldsT, 0)), Tr.TEMP r))
end

fun array (size, init) = Ex(Tr.CALL(Tr.NAME (T.namedlabel "initArray"), [unEx(size), unEx(init)], AccessConv.frameToTree(F.dummyFormals(2, [])), [false, false], false)) 

fun assign (left, right, isReal) = if isReal then Nx(Tr.RMOVE(unEx(left), unEx(right))) else Nx(Tr.MOVE(unEx(left), unEx(right)))

fun letexp (decs, body) =
let 
	val len = List.length decs 
in
	if len = 0 then body
	else if len = 1 then Ex(Tr.ESEQ(unNx(hd(decs)), unEx(body)))
	else 
	let 
		val seqDecs = map unNx decs 
	in 
		Ex(Tr.ESEQ(seq seqDecs, unEx(body))) 
	end
end


(* Just the book instructions *)
fun loop (test, body, doneLabel) =
let 
	val testLabel = Temp.newlabel()
	val bodyLabel = Temp.newlabel() 
in
	Nx(seq[
	Tr.LABEL testLabel,
	Tr.CJUMP(Tr.EQ, unEx(test), Tr.CONST 0, doneLabel, bodyLabel),
	Tr.LABEL bodyLabel,
	unNx(body),
	Tr.JUMP(Tr.NAME testLabel, [testLabel]),
	Tr.LABEL doneLabel])
end

fun break (label) = Nx(Tr.JUMP(Tr.NAME label, [label]))

(* first argument is our level and second one is function level *)
fun call (_, Lev({parent = Top, ...}, _), label, exps, isRealL, isRtyReal) = Ex(Tr.CALL(Tr.NAME label, map unEx exps, AccessConv.frameToTree(F.dummyFormals(List.length(isRealL), [])), isRealL, isRtyReal)) (*(F.externalCall(Symbol.name label, map unEx exps))*)
  | call (callLevel, funLevel as Lev({parent, frame = frame'}, _), label, exps, isRealL, isRtyReal) =
    let
			val Lev({parent, frame}, _) = funLevel
			val traceLevel = getLevelsFPUsingStaticLink (parent, callLevel, Tr.TEMP (F.fp))
		in 
			Ex (Tr.CALL (Tr.NAME label, traceLevel :: (map unEx exps), AccessConv.frameToTree(F.formals frame'), false :: isRealL, isRtyReal))
    end

fun sequence (exps: exp list) =
let 
	val len = length exps 
in
	if len = 0 then (Ex(Tr.CONST 0)) (* for "()" *)
	else if len = 1 then Ex(unEx (hd(exps)))
	else
		Ex(Tr.ESEQ (seq(map unNx (List.take(exps, length(exps) - 1))), unEx (List.last(exps))))
end


fun procEntryExit (Lev({frame, ...}, _), body, rty) =
case rty of 
		Types.REAL => fragments := F.PROC{frame = frame, body = F.procEntryExit1(frame, Tr.RMOVE(Tr.TEMP F.rrv, unEx(body)))} :: (!fragments) 
	| _ => fragments := F.PROC{frame = frame, body = F.procEntryExit1(frame, Tr.MOVE(Tr.TEMP F.rv, unEx(body)))} :: (!fragments)

end