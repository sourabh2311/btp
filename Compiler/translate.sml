structure Translate : TRANSLATE =
struct

structure Tr = Tree
structure F = RiscFrame
structure A = Absyn

(* Basic Definitions *)
datatype level =  Top
								| Lev of {parent: level, frame: F.frame} * unit ref

datatype exp =  Ex of Tr.exp
							| Nx of Tr.stm
							| Cx of Temp.label * Temp.label -> Tr.stm

type access = level * F.access

type frag = F.frag

val outermost = Top  

val fragments : frag list ref = ref nil

val errExp = Ex(Tr.CONST 0)

(* Helper Functions *)

fun getResult () = !fragments

fun reset () = fragments := nil

fun memPlus (e1, e2) = Tr.MEM(Tr.BINOP(Tr.PLUS, e1, e2))

(* Remember that we should pass the static link for which we have added its escape *)
fun newLevel {parent, name, formals} = Lev({parent = parent, frame = F.newFrame {name = name, formals = true :: formals}}, ref ())

(* Return formals associated with the frame in this level, excluding the static link (first element of the list) *)
fun formals lev =
    case lev of
      Top => nil
    | Lev({parent = parent, frame = frame}, _) =>
      let 
				val formals = tl (F.formals frame) 
			in
        (map (fn (x) => (lev, x)) formals) 
			end

(* Allocate a new local variable either on frame or in register (depending on whether it escapes) *)
fun allocLocal lev escape =
	case lev of
		Lev({parent = _, frame = frame}, _) => (lev, F.allocLocal frame escape)


fun seq stmlist =
	case stmlist of
		[s] => s
	| [s1, s2] => Tr.SEQ(s1, s2)
	| stm :: rest => Tr.SEQ(stm, seq(rest))


fun unEx (Ex e) = e 
	| unEx (Cx genstm) = 
let 
	val r = Temp.newtemp() 
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
	| unCx (Nx _) = (ErrorMsg.error 0 "Compiler error: unCx an Nx"; fn (a, b) => Tr.LABEL(Temp.newlabel()))

fun unNx (Ex e) = Tr.EXP(e)
	| unNx (Nx n) = n
	| unNx (c) = unNx(Ex(unEx(c)))


val nilexp = Ex(Tr.CONST(0))
fun intlit (n) = Ex(Tr.CONST (n))
fun strlit (s: string) : exp =
let 
	val there = List.find
		(fn (x) =>
			case x of
				F.PROC _ => false
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

fun getLevelUsingStaticLink (Top, _, frameAddress) = ErrorMsg.impossible "You have issue with Static Links"
  | getLevelUsingStaticLink (_, Top, frameAddress) = ErrorMsg.impossible "You have issue with Static Links"
	| getLevelUsingStaticLink (Lev (ignore, targetLevelRef), Lev ({parent, frame}, curLevelRef), frameAddress) = 
		if (curLevelRef = targetLevelRef) then frameAddress
		else getLevelUsingStaticLink (Lev (ignore, targetLevelRef), parent, (F.exp (List.hd(F.formals frame)) frameAddress))

fun simpleVar (targetLevelAccess, curLevel) =
let 
	val (targetLevel, frameAccess) = targetLevelAccess  (* This is our target to reach *)
	(* The passed level need not be our target level *)
	val traceLevel = getLevelUsingStaticLink (targetLevel, curLevel, Tr.TEMP (F.fp))
in 
	Ex(F.exp frameAccess traceLevel)
end

(* Just the way given in text. *)
fun subscriptVar (base, offset) = Ex(memPlus(unEx(base), Tr.BINOP(Tr.MUL, unEx(offset), Tr.CONST(F.wordSize))))

(* Just the way given in text. *)
fun fieldVar (base, id, SL) =
let 
	fun findindex (index, elem, l :: ls) =
		if elem = l then index
		else findindex(index + 1, elem, ls) 
in
	Ex(memPlus(unEx(base), Tr.BINOP(Tr.MUL, Tr.CONST(findindex(0, id, SL)), Tr.CONST(F.wordSize))))
end

fun binop (oper, e1, e2) =
let
	val left = unEx(e1)
	val right = unEx(e2)
	val treeop =
		case oper of
			A.PlusOp => Tr.PLUS
		| A.MinusOp => Tr.MINUS
		| A.TimesOp => Tr.MUL
		| A.DivideOp => Tr.DIV
in 
	Ex(Tr.BINOP(treeop, left, right))
end

fun relop (oper, e1, e2) =
let
	val left = unEx(e1)
	val right = unEx(e2)
	val treeop =
			case oper of
				A.EqOp => Tr.EQ
			| A.NeqOp => Tr.NE
			| A.LtOp => Tr.LT
			| A.LeOp => Tr.LE
			| A.GtOp => Tr.GT
			| A.GeOp => Tr.GE
in 
	Cx((fn (t, f) => Tr.CJUMP(treeop, left, right, t, f))) 
end


(* The elseexp could be NONE, in which case the
 * result must be unit, and thus we return CONST(0) *)
fun ifelse (testexp, thenexp, elseexp) =
let
	val r = Temp.newtemp() (* hold result *)
	val t = Temp.newlabel()
	val f = Temp.newlabel()
	val finish = Temp.newlabel()
	val neotest  = unCx(testexp) 
	val neothen = unEx (thenexp)
	val neoelseexp = unEx (elseexp)
in
	Ex(Tr.ESEQ(seq[neotest(t, f),
	Tr.LABEL t, Tr.MOVE(Tr.TEMP r, neothen),
	Tr.JUMP (Tr.NAME finish, [finish]),
	Tr.LABEL f, Tr.MOVE(Tr.TEMP r, neoelseexp),
	Tr.JUMP (Tr.NAME finish, [finish]),
	Tr.LABEL finish],
	Tr.TEMP r))
end

(* creation of an array and record (see page 164 for more info) *)
fun record (fields) =
let
	val r = Temp.newtemp()
	val init =
			Tr.MOVE(
			Tr.TEMP r,
			F.externalCall(
			"allocRecord", [Tr.CONST(length(fields) * F.wordSize)]))

	fun loop (fields, index) =
			case fields of
				nil => nil
			| e :: rest =>
				Tr.MOVE(
				memPlus(Tr.TEMP r, Tr.CONST(index * F.wordSize)),
				unEx(e)) :: loop(rest, index + 1)
in 
	Ex(Tr.ESEQ(seq(init :: loop(fields, 0)), Tr.TEMP r))
end


(* Believe in text *)
fun array (size, init) = Ex(F.externalCall("initArray", [unEx(size), unEx(init)]))

fun assign (left, right) = Nx(Tr.MOVE(unEx(left), unEx(right)))

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
fun call (_, Lev({parent = Top, ...}, _), label, exps) = Ex(F.externalCall(Symbol.name label, map unEx exps))
  | call (callLevel, funLevel as Lev({parent, frame = frame'}, _), label, exps) =
    let
			val Lev({parent, frame}, _) = funLevel
			val traceLevel = getLevelUsingStaticLink (parent, callLevel, Tr.TEMP (F.fp))
		in 
			Ex (Tr.CALL (Tr.NAME label, traceLevel :: (map unEx exps), AccessConv.frameToTree(F.formals frame')))
    end

fun sequence (exps: exp list) =
let 
	val len = length exps 
in
	if len = 0 then (Ex(Tr.CONST 0)) (* for "()" *)
	else if len = 1 then Ex(unEx (hd(exps)))
	else
	let 
		(* take (l, i) returns the first i elements of the list l. Note: take (l, length l) = l *)
		val first = seq(map unNx (List.take(exps, length(exps) - 1)))
		val last = List.last(exps) 
	in
		Ex(Tr.ESEQ (first, unEx (last)))
	end
end

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


fun procEntryExit (Lev({frame, ...}, _), body) =
let 
	val body' = F.procEntryExit1(frame, Tr.MOVE(Tr.TEMP F.rv, unEx(body)))
	val _ = fragments := F.PROC{frame = frame, body = body'} :: !fragments
in 
	()
end




end