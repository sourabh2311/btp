(* There is no SOME without NONE and all cases are exhaustive, so this can be run before semantic analysis :D *)
(* Only considering pos for vars as pos for any two var with even same name, etc is different (hack) *)
structure ClassOffsets: sig val classOffsets: Absyn.exp -> unit end =
struct

structure S = Symbol
structure A = Absyn
structure Err = ErrorMsg

fun partitionClassFields ([], a, b) = (a, b)
  | partitionClassFields (cf :: cfs, a, b) = 
    case cf of 
      A.VarDec (_) => partitionClassFields (cfs, a @ [cf], b)
    | _ => partitionClassFields (cfs, a, b @ [cf])

fun printOffsets([]) = print("\n")
  | printOffsets(l :: ls) = (print (" " ^ Int.toString(l)); printOffsets(ls))


structure I = IntBinaryMap

(* Contains all vars declared till and including that class *)
val classAllVars = ref (S.enter(S.empty, S.symbol "Object", [] : A.dec list))
val explore = ref 1 (* By default it is one and will be made zero after computing the sets *)

val rank = ref (I.empty : int I.map) 
val p = ref (I.empty : int I.map)
val setSize = ref (I.empty : int I.map)
val setTaken = ref (I.empty : int I.map)
val elements = ref 0
val varPosToInt = ref (I.empty : int I.map)
val color = ref (I.empty : int I.map)  (* Will map the pos of a var to its color which is nothing but an offset *)

fun createSet (A.VarDec{pos, ...}) = 
(
  elements := (!elements) + 1;
  varPosToInt := I.insert(!varPosToInt, pos, !elements);
  p := I.insert(!p, !elements, !elements);
  setSize := I.insert(!setSize, !elements, 1);
  rank := I.insert(!rank, !elements, 0)
)

fun findSet (i : int) = 
  if (I.lookup(!p, i) = i) then i else (
    p := I.insert(!p, i, findSet(I.lookup(!p, i)));
    I.lookup(!p, i)
  )

fun isSameSet(i : int, j : int) = findSet(i) = findSet(j)

fun unionSet(i : int, j : int) = 
  if (not (isSameSet(i, j))) then (
    let 
      val x = findSet(i)
      val y = findSet(j)
      val setSizeX = I.lookup(!setSize, x)
      val setSizeY = I.lookup(!setSize, y)
    in 
      if (I.lookup(!rank, x) > I.lookup(!rank, y)) then 
      (
        setSize := I.insert(!setSize, x, setSizeX + setSizeY);
        p := I.insert(!p, y, x)
      )
      else (
        setSize := I.insert(!setSize, y, setSizeY + setSizeX);
        p := I.insert(!p, x, y);
        if (I.lookup(!rank, x) = I.lookup(!rank, y)) then (
          rank := I.insert(!rank, y, I.lookup(!rank, y) + 1)
        ) else ()
      )
    end
  ) else ()

fun getPosInt(pos) = I.lookup(!varPosToInt, pos)

fun join ([]) = ()
  | join ([A.VarDec{name, pos, ...}]) = ()
  | join ((l as A.VarDec{name = ln, pos = lp, ...}) :: (m as A.VarDec{name = mn, pos = mp, ...}) :: ls) = (unionSet(getPosInt(lp), getPosInt(mp)); join(m :: ls))

fun varDuplicate ([], seenMap) = false
  | varDuplicate ((l as A.VarDec {name, ...}) :: ls, seenMap) = 
      if (S.inDomain(seenMap, name)) then true else varDuplicate(ls, S.enter(seenMap, name, true))


fun traverseVar(s : A.var): unit =
case s of
    A.SimpleVar(sym, pos) => ()
  | A.FieldVar(var, sym, pos) => traverseVar(var)
  | A.SubscriptVar(var, exp, pos) => (traverseVar(var); traverseExp(exp))

and traverseExp(s : A.exp): unit =  (* traverseExp is done *)
case s of
    A.NilExp => ()
  | A.IntExp(_) => ()
  | A.RealExp(_) => ()
  | A.StringExp(_, _) => ()
  | A.ClassObject(_) => ()
  | A.ClassCallExp({lvalue, args, ...}) => (traverseVar(lvalue); app (fn arg => traverseExp(arg)) args)
  | A.CallExp({args, ...}) => app (fn (arg) => traverseExp(arg)) args
  | A.ArrayExp({size, init, ...}) => (traverseExp(size); traverseExp(init))
  | A.RecordExp({fields, ...}) => app (fn (_, exp, _) => traverseExp(exp)) fields
  | A.OpExp({left, right, ...}) => (traverseExp(left); traverseExp(right))
  | A.SeqExp(exps) => app (fn (exp, _) => traverseExp(exp)) exps
  | A.IfExp({test, then', else', ...}) =>
    (
      traverseExp(test); traverseExp(then');
      (case else' of
          SOME(else'') => traverseExp(else'')
        | NONE => ()
      )
    )
  | A.WhileExp({test, body, ...}) => (traverseExp(test); traverseExp(body))
  | A.LetExp({decs, body, ...}) =>
    (
      traverseDecs(decs);
      traverseExp(body)
    )
  | A.ForExp({var, escape, lo, hi, body, pos}) =>
    (
      traverseExp(lo);
      traverseExp(hi);
      traverseExp(body)
    )
  | A.BreakExp(_) => ()
  | A.AssignExp({var, exp, ...}) => (traverseVar(var); traverseExp(exp))
  | A.VarExp(var) => traverseVar(var)

and traverseDecs(s : A.dec list) : unit =
let 
  fun parseDec (dec) =
  case dec of
      A.FunctionDec(fundecs) => app (fn {body, ...} => traverseExp(body)) fundecs
    | A.VarDec({init, ...}) => traverseExp(init)
    | A.TypeDec(_) => ()
    (* It feels risky to store classes, so use global variable switch here *)
    | A.ClassDec({name, extends, classFields, varOffsets, pos}) => if ((!explore) = 1 andalso S.inDomain(!classAllVars, name)) then (Err.error pos "This Class has already been declared before!") 
      else 
      (
        let 
          (* Check whether this class has been declared before! I support only unique classes! *)
          val (newVars, _) = partitionClassFields(classFields, [], [])
          val _ = traverseDecs(classFields)
          fun ExtendVars([], vars) = vars 
            | ExtendVars(l :: ls, vars) = ExtendVars (ls, vars @ (if S.inDomain (!classAllVars, l) then S.lookup (!classAllVars, l) else (Err.error pos ("No such extend class exist!"); [])))
          val extendVars = ExtendVars(extends, [])
          val seenMap = ref S.empty
          fun seeVars([]) = ()
            | seeVars((l as A.VarDec{name, ...} :: ls)) = (seenMap := S.enter(!seenMap, name, true); seeVars(ls))
          val _ = seeVars(extendVars)
          val newVars' = List.filter (fn (A.VarDec{name, ...}) => if (S.inDomain(!seenMap, name)) then false else true) newVars
          val completeVars = extendVars @ newVars'
          (* This function will be called only if explore is set to 0 *)
          fun appendOffset ([]) = ()
            | appendOffset ((l as A.VarDec{name, pos, ...}) :: ls) = (
              let 
                val vset = findSet(getPosInt(pos))
                val vsetTaken = if I.inDomain(!setTaken, vset) then I.lookup(!setTaken, vset) else (setTaken := I.insert(!setTaken, vset, 1); 1)
              in 
                if I.inDomain(!color, pos) then (varOffsets := (!varOffsets) @ [I.lookup(!color, pos)]; appendOffset(ls))
                else (  (* Need to assign this a colour *)
                  color := I.insert(!color, pos, I.lookup(!setSize, vset) - vsetTaken);
                  setTaken := I.insert(!setTaken, vset, vsetTaken + 1);
                  varOffsets := (!varOffsets) @ [I.lookup(!color, pos)];
                  appendOffset(ls)
                )
              end
            )
        in 
        (
          if (!explore = 1) then (
            app createSet newVars';
            if varDuplicate(extendVars, S.empty) orelse varDuplicate(newVars, S.empty) then (Err.error pos ("There duplicate var declarations in the extends or current class var declarations")) 
            else (
              classAllVars := S.enter(!classAllVars, name, completeVars);
              join(completeVars)
            )
          )
          else ( (* Need to fill the info *)
            appendOffset (completeVars);
            print("For class: " ^ S.name(name) ^ " offsets computed are:");
            printOffsets(!varOffsets)
          )
        )
        end
      )
in 
  app parseDec s 
end

fun classOffsets(prog : A.exp): unit = 
(
  traverseExp(prog);
  explore := 0; 
  traverseExp(prog) (* Now one should fill in the details *)
)

(* end *)
end