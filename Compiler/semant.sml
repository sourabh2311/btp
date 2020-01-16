structure Semant =
(* This structure does semantic analysis - type checking of abstract syntax and also translating to IR. Note: Semant module should not contain any direct reference to the Tree or Frame module. Any manipulation of IR trees should be done by Translate *)

struct
    (* Basic Structures *)
    structure A = Absyn
    structure T = Types
    structure S = Symbol
    structure Err = ErrorMsg
    structure E = Env
    structure L = Translate
    type venv = E.enventry Symbol.table
    type tenv = T.ty Symbol.table
    (* Translate.exp is the translation of the expression into intermediate code and ty is the type of the expression *)
    type expty = {exp: Translate.exp, ty: T.ty}
    (* Helper types/vals *)
    val errResult = {exp = L.errExp, ty = T.NIL}
    val curDepth = ref 0

    (* Helper functions *)
    fun typeNotFound(tenv, pos, targetType, sym) = (Err.error pos ("following " ^ targetType ^ " is not defined: " ^ S.name(sym)))
    fun variableNotFound(venv, pos, targetVar, sym) = (Err.error pos ("following " ^ targetVar ^ " is not defined: " ^ S.name(sym)))
    (* The type in the VarEntry will sometimes be a "NAME type", and all the types returned from transExp should be "actual" types (with the names traced through to their underlying definitions). So, "actual_ty" is used to skip past all the names. The result will be a Types.ty that is not a NAME, though if it is a record or array type it might contain NAME types to describe its components. *)
    fun actual_ty (tenv, ty, pos) =
        case ty of
            T.NAME(sym, tyref) =>
            (case (!tyref) of
              NONE => (typeNotFound(tenv, pos, "type", sym); T.NIL)
            | SOME(ty) => actual_ty (tenv, ty, pos))
            (* For type checking of array with "init", we must trace the type of 'ty'' *)
            | T.ARRAY(ty', ref') => T.ARRAY(actual_ty (tenv, ty', pos), ref')
            | _ => ty

    (* augment ty to auxiliary record *)
    fun augmentR (t : T.ty) = {exp = L.errExp, ty = t}
    fun getexp (ety : expty) = (#exp ety)
    fun incDepth () = curDepth := !curDepth + 1
    fun decDepth () = curDepth := !curDepth - 1
    fun setDepth (newDepth) = curDepth := newDepth
    (* Simply a function to check if all names are unique *)
    fun checkDup (nil, nil) = ()
      | checkDup (name :: names, pos :: poss) = 
        if (List.all (fn (x) => (name <> x)) names) then checkDup (names, poss)
        else (Err.error pos ("Duplicate definition " ^ (S.name name)))
    fun checkInt ({exp = _, ty = T.INT}, pos) = ()
      | checkInt ({exp = _, ty = _ }, pos) = Err.error pos "error : integer required"
    
    fun checkType (tenv, {exp = _, ty = T.INT}, {exp = _, ty = T.INT}, pos) = ()
      | checkType (tenv, {exp = _, ty = T.STRING}, {exp = _, ty = T.STRING}, pos) = ()
      | checkType (tenv, {exp = _, ty = T.REAL}, {exp = _, ty = T.REAL}, pos) = ()
      (* Just need to match unit ref as said before *)
      | checkType (tenv, {exp = _, ty = T.RECORD(_, ref1)}, {exp = _, ty = T.RECORD(_, ref2)}, pos) = if ref1 = ref2 then () else Err.error pos "can't compare different record types"
      (* As said before nil belongs to every record *)
      | checkType (tenv, {exp = _, ty = T.NIL}, {exp = _, ty = T.RECORD(_, _)}, pos) = ()
      | checkType (tenv, {exp = _, ty = T.RECORD(_, _)}, {exp = _, ty = T.NIL}, pos) = ()
      | checkType (tenv, {exp = _, ty = T.ARRAY(_, ref1)}, {exp = _, ty = T.ARRAY(_, ref2)}, pos) = if ref1 = ref2 then () else Err.error pos "can't compare different array types"
      | checkType (tenv, {exp = _, ty = T.UNIT}, {exp = _, ty = T.UNIT}, pos) = ()
      | checkType (tenv, {exp = e1, ty = T.NAME (ll, lm)}, {exp = e2, ty = t2}, pos) = checkType (tenv, augmentR (actual_ty (tenv, T.NAME (ll, lm), pos)), {exp = e2, ty = t2}, pos)
      | checkType (tenv, {exp = e1, ty = t1}, {exp = e2, ty = T.NAME (ll, lm)}, pos) = checkType (tenv, augmentR (actual_ty (tenv, T.NAME (ll, lm), pos)), {exp = e1, ty = t1}, pos)
      | checkType (tenv, {exp = _, ty = _}, {exp = _, ty = _}, pos) = Err.error pos "type mismatch"

    (* So that transExp can translate break statements, it has a formal parameter break that is the done label of the nearest enclosing loop. In translating a while loop, transExp is called upon body with the done label passed as the break parameter. When transExp is recursively calling itself in nonloop contexts, it can simply pass down the same break parameter that was passed to it. The break argument must also be added to the transDec function. *)
    fun transExp (venv, tenv, exp, level, break) = 
    let 
    fun
      (* Will replace string comparison by corresponsing function call *)
      callStringFunction (left, oper, right, pos) = 
      case oper of
        (* A.PlusOp => trexp(A.CallExp({func = Symbol.symbol "stringEqual", args = [left, right], pos = pos})) *)
        A.EqOp => trexp(A.CallExp({func = Symbol.symbol "stringEqual", args = [left, right], pos = pos}))
      | A.NeqOp => trexp(A.IfExp({test = A.CallExp({func = Symbol.symbol "stringEqual", args = [left, right], pos = pos}), then' = A.IntExp(0), else' = SOME (A.IntExp(1)), pos = pos}))
      | A.LtOp => trexp(A.CallExp({func = Symbol.symbol "stringLess", args = [left, right], pos = pos}))
      | A.LeOp => trexp(A.IfExp({test = A.CallExp({func = Symbol.symbol "stringLess", args = [left, right], pos = pos}), then' = A.IntExp(1), else' = SOME (A.CallExp({func = Symbol.symbol "stringEqual", args = [left, right], pos = pos})), pos = pos}))
      | A.GtOp => trexp(A.CallExp({func = Symbol.symbol "stringGreat", args = [left, right], pos = pos}))
      | A.GeOp => trexp(A.IfExp({test = A.CallExp({func = Symbol.symbol "stringGreat", args = [left, right], pos = pos}), then' = A.IntExp(1), else' = SOME (A.CallExp({func = Symbol.symbol "stringEqual", args = [left, right], pos = pos})), pos = pos}))
    and 
      callRealFunction (left, oper, right, pos) = 
      case oper of
        A.EqOp => trexp(A.CallExp({func = Symbol.symbol "realEqual", args = [left, right], pos = pos}))
      | A.NeqOp => trexp(A.IfExp({test = A.CallExp({func = Symbol.symbol "realEqual", args = [left, right], pos = pos}), then' = A.IntExp(0), else' = SOME (A.IntExp(1)), pos = pos}))
      | A.LtOp => trexp(A.CallExp({func = Symbol.symbol "realLess", args = [left, right], pos = pos}))
      | A.LeOp => trexp(A.IfExp({test = A.CallExp({func = Symbol.symbol "realLess", args = [left, right], pos = pos}), then' = A.IntExp(1), else' = SOME (A.CallExp({func = Symbol.symbol "realEqual", args = [left, right], pos = pos})), pos = pos}))
      | A.GtOp => trexp(A.CallExp({func = Symbol.symbol "realGreat", args = [left, right], pos = pos}))
      | A.GeOp => trexp(A.IfExp({test = A.CallExp({func = Symbol.symbol "realGreat", args = [left, right], pos = pos}), then' = A.IntExp(1), else' = SOME (A.CallExp({func = Symbol.symbol "realEqual", args = [left, right], pos = pos})), pos = pos}))
      | A.PlusOp => trexp(A.CallExp({func = Symbol.symbol "radd", args = [left, right], pos = pos}))
      | A.MinusOp => trexp(A.CallExp({func = Symbol.symbol "rsub", args = [left, right], pos = pos}))
      | A.TimesOp => trexp(A.CallExp({func = Symbol.symbol "rmul", args = [left, right], pos = pos}))
      | A.DivideOp => trexp(A.CallExp({func = Symbol.symbol "rdiv", args = [left, right], pos = pos}))
    and
        (* trexp recurs over Absyn.exp and trvar recurs over Absyn.var *)
        (* In rare cases when trexp wants to change venv, it must call transExp instead of just trexp *)
        trexp (A.VarExp(var)) = trvar var
      | trexp (A.NilExp) = {exp = L.nilexp, ty = T.NIL}
      | trexp (A.IntExp(intvalue)) = {exp = L.intlit(intvalue), ty = T.INT}
      | trexp (A.StringExp(stringvalue, pos)) = {exp = L.strlit(stringvalue), ty = T.STRING}
      | trexp (A.RealExp(realvalue)) = {exp = L.reallit(realvalue), ty = T.REAL}
      | trexp (A.CallExp({func, args, pos})) = 
        let
            val argET = map trexp args
            fun checkFormals(formals, pos) = if (List.length (formals) <> List.length (argET)) then (Err.error pos "Number of arguments don't match corresponding to type") else (List.app (fn (t, e) => checkType (tenv, augmentR (actual_ty (tenv, t, pos)), e, pos)) (ListPair.zip (formals, argET)))
        in
            case S.look(venv, func) of
                SOME(E.FunEntry({level = funlevel, label, formals, result})) => (checkFormals(formals, pos); {exp = L.call(level, funlevel, label, map #exp argET), ty = actual_ty (tenv, result, pos)})
              | SOME(_) => (Err.error pos ("symbol not a function " ^ S.name func); errResult)
              | NONE => (variableNotFound(venv, pos, "function", func); errResult)
        end
      | trexp (A.OpExp{left, oper, right, pos}) = 
        let 
          fun checkArith () = (* for both int, real valid operations. *)
          let
            val exptyleft = trexp left
            val exptyright = trexp right
          in 
            (case exptyleft of 
              {exp = _, ty = T.REAL} => (checkType (tenv, exptyleft, exptyright, pos); callRealFunction(left, oper, right, pos))
            | {exp = _, ty = T.INT} => (checkType (tenv, exptyleft, exptyright, pos); {exp = L.binop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
            | _ => ((Err.error pos "Arithmetic operations supported only upon ints and reals"); {exp = L.binop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
            )
          end
          fun checkIArith () = (* for lshift, rshift which require strictly integers *)
          let
            val exptyleft = trexp left
            val exptyright = trexp right
          in 
            (checkInt(exptyleft, pos); checkInt (exptyright, pos); {exp = L.binop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
          end
          fun checkEq () = 
          let
            val exptyleft = trexp left
            val exptyright = trexp right
          in 
            (
            case exptyleft of 
            {exp = _, ty = T.RECORD(_, _)} => (checkType (tenv, exptyleft, exptyright, pos); {exp = L.relop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
            | {exp = _, ty = T.ARRAY(_, _)} => (checkType (tenv, exptyleft, exptyright, pos); {exp = L.relop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
            | {exp = _, ty = T.STRING} => (checkType (tenv, exptyleft, exptyright, pos); callStringFunction(left, oper, right, pos))
            | {exp = _, ty = T.REAL} => (checkType (tenv, exptyleft, exptyright, pos); callRealFunction(left, oper, right, pos))
            | {exp = _, ty = T.INT} => (checkType (tenv, exptyleft, exptyright, pos); {exp = L.relop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
            | _ => ((Err.error pos "Can compare only two ints, strings, arrays, records and reals for eq/neq"); {exp = L.relop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
            ) 
          end
          fun checkComp () = 
          let 
            val exptyleft = trexp left
            val exptyright = trexp right
          in 
            
            (
            case (#ty exptyleft) of
              T.INT => (checkType (tenv, exptyleft, exptyright, pos); {exp = L.relop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
              | T.STRING => (checkType (tenv, exptyleft, exptyright, pos); callStringFunction(left, oper, right, pos))
              | T.REAL => (checkType (tenv, exptyleft, exptyright, pos); callRealFunction(left, oper, right, pos))
              | _ => ((Err.error pos "Comparison can be checked only on ints, strings and reals"); {exp = L.relop(oper, getexp(exptyleft), getexp(exptyright)), ty = T.INT})
            )
            
          end
        in 
          (case oper of
              A.PlusOp => (checkArith ())
            | A.MinusOp => (checkArith ())
            | A.TimesOp => (checkArith ())
            | A.DivideOp => (checkArith ())
            | A.LShift => (checkIArith ())
            | A.RShift => (checkIArith ())
            | A.EqOp => (checkEq ())
            | A.NeqOp => (checkEq ())
            | A.LtOp => (checkComp ())
            | A.LeOp => (checkComp ())
            | A.GtOp => (checkComp ())
            | A.GeOp => (checkComp ())
          ) 
        end
        
      | trexp (A.RecordExp({fields, typ, pos})) = 
        (case S.look(tenv, typ) of
            NONE => (typeNotFound(tenv, pos, "record", typ); errResult)
          | SOME(t) =>
            case actual_ty (tenv, t, pos) of
                (* Recall: it is (symbol * ty) list * unique *)
                T.RECORD (StyL, u) =>
                let
                    val fieldsET = map (fn (_, e, pos) => (trexp e, pos)) fields
                    val fieldsE = map (fn ({exp, ty}, _) => exp) fieldsET 
                    fun checkRecord () = if (List.length (StyL) <> List.length (fieldsET)) then (Err.error pos "Number of fields in a record doesn't match with its corresponding type declaration fields") else (List.app (fn (Sty, (t, pos)) => checkType (tenv, augmentR(#2Sty), t, pos)) (ListPair.zip (StyL, fieldsET)))
                in
                    (checkRecord (); {exp = L.record(fieldsE), ty = T.RECORD (StyL, u)})
                end
              | t => (Err.error pos "Type Mismatch"; errResult))
            
      | trexp (A.SeqExp(expList)) = 
        let
            val aexpL = map (fn (exp, _) => getexp(trexp exp)) expList
            val ty = if List.null expList then T.UNIT else (#ty (trexp (#1 (List.last expList))))
        in
            {exp = L.sequence (aexpL), ty = ty}
        end
      | trexp (A.AssignExp({var, exp, pos})) = 
        let 
          val exptyvar = trvar var
          val exptyexp = trexp exp
        in 
          (checkType (tenv, trvar var, trexp exp, pos); {exp = L.assign(getexp(exptyvar), getexp(exptyexp)), ty = T.UNIT})
        end
      | trexp (A.IfExp({test, then', else', pos})) = 
        let 
          val exptytest = trexp test
          val exptythen = trexp then'
          val _ = checkType (tenv, exptytest, augmentR(T.INT), pos)
          val exptyelse = case else' of 
                    NONE => ((checkType (tenv, augmentR(T.UNIT), exptythen, pos)); {exp = L.errExp, ty = T.UNIT})
                  | SOME(e) => (
                    let val exptyelse = trexp e in (checkType (tenv, exptyelse, exptythen, pos); {exp = getexp(exptyelse), ty = (#ty exptythen)}) end)
        in 
          {exp = L.ifelse (getexp(exptytest), getexp(exptythen), getexp(exptyelse)), ty = (#ty exptythen)}
        end
      | trexp (A.WhileExp({test, body, pos})) = 
        let 
          val exptytest = trexp test
          val _ = checkType (tenv, exptytest, augmentR(T.INT), pos)
          val _ = incDepth ()
          val doneLabel = Temp.newlabel()
          val exptybody = transExp (venv, tenv, body, level, doneLabel)
          val _ = checkType (tenv, exptybody, augmentR(T.UNIT), pos)
          val _ = decDepth ()
        in 
            {exp = L.loop(getexp(exptytest), getexp(exptybody), doneLabel), ty = T.UNIT}
        end
        (* Need to convert for in terms of let and while *)
        (* No need to do type checking, it will be done on new expression *)
        (* For better understanding, just see page 166 of book. *)
      | trexp (A.ForExp({var, escape, lo, hi, body, pos})) = 
        let
          val limit = S.symbol "limit"
          val ivar = A.SimpleVar(var, pos)
          val limitvar = A.SimpleVar(limit, pos)
          val letdecs =
            [
              A.VarDec {
                name = var,
                escape = escape,
                typ = NONE,
                init = lo,
                pos = pos
              },
              A.VarDec {
                name = limit,
                escape = ref false,
                typ = NONE,
                init = hi,
                pos = pos
              }
            ]

          val loop =
            A.WhileExp {
              test = A.OpExp {
                left = A.VarExp(ivar),
                oper = A.LeOp,
                right = A.VarExp(limitvar),
                pos = pos
              },
              body = A.SeqExp[
                (body, pos),
                (A.AssignExp {
                  var = ivar,
                  exp = A.OpExp {
                    left = A.VarExp(ivar),
                    oper = A.PlusOp,
                    right = A.IntExp(1),
                    pos = pos
                  }, 
                  pos = pos
                }, pos)
              ],
              pos = pos
            }
        in
          trexp (A.LetExp {decs = letdecs, body = loop, pos = pos})
        end
      | trexp (A.BreakExp(pos)) = 
        let
          val _ = if (!curDepth = 0) then (Err.error pos "Incorrect break insertion") else ()
        in 
          {exp = L.break(break), ty = T.UNIT}
        end
      | trexp (A.LetExp({decs, body, pos})) = 
        let
          val backupDepth = !curDepth
          val _ = setDepth (0)
          val {venv = venv', tenv = tenv', expList = expList'} = 
            foldl (
              fn (dec, {venv, tenv, expList}) => 
              let
                val {venv = venv', tenv = tenv', expList = expList'} = transDec (venv, tenv, dec, level, break)
              in
                {venv = venv', tenv = tenv', expList = expList @ expList'}
              end) {venv = venv, tenv = tenv, expList = []} decs
          val _ = setDepth (backupDepth)
          val exptyBody = transExp (venv', tenv', body, level, break)
        in
          {exp = L.letexp (expList', getexp(exptyBody)), ty = (#ty exptyBody)}
        end
        (* type of size should be int, type of array should be same as init *)
      | trexp (A.ArrayExp({typ, size, init, pos})) = 
        case S.look (tenv, typ) of 
          NONE => (typeNotFound(tenv, pos, "array", typ); errResult)
        | SOME (arrayty) =>
          let 
            val acArrayty = actual_ty (tenv, arrayty, pos)
          in 
            case acArrayty of 
              T.ARRAY(realthing, uniq) => 
              let
                val exptysize = trexp size
                val exptyinit = trexp init
                val _ = checkType (tenv, augmentR(T.INT), exptysize, pos)
                val _ = checkType (tenv, augmentR(realthing), exptyinit, pos)
              in 
                {exp = L.array (getexp(exptysize), getexp(exptyinit)), ty = arrayty}
              end
              | _ => ((Err.error pos "Identifier not corresponding to an array"); errResult)

          end
    and trvar (A.SimpleVar(id, pos)) = 
        (case S.look(venv, id) of
            SOME(E.VarEntry({access, ty})) => {exp = L.simpleVar (access, level), ty = actual_ty (tenv, ty, pos)}
          | SOME(_) => (Err.error pos ("variable was expected but instead function is given"); errResult)
          | NONE => (variableNotFound(venv, pos, "variable", id); errResult)
        )
      | trvar (A.FieldVar(lval, id, pos)) =
        let 
          val exptylval = trvar lval
        in
          (case exptylval of
            {exp = _, ty = T.RECORD(StyL, uniq)} => 
            (case List.find (fn elem => (#1elem) = id) StyL of
                NONE => (typeNotFound(tenv, pos, "record symbol", id); errResult)
              | SOME (elem) => {exp = L.fieldVar(getexp (exptylval), id, map #1 StyL), ty = actual_ty (tenv, #2elem, pos)}) 
          | _ => (Err.error pos ("record was expected but something else is given"); errResult)
          )
        end
      | trvar (A.SubscriptVar(avar, index, pos)) = 
        let
          val exptyavar = trvar avar        
        in
          case actual_ty(tenv, #ty exptyavar, pos) of 
            T.ARRAY(t, _) => (
              let
                val exptyindex = trexp index
              in 
                (checkType (tenv, exptyindex, augmentR(T.INT), pos); {exp = L.subscriptVar (getexp (exptyavar), getexp (exptyindex)), ty = t})
              end)
          | _ => ((Err.error pos "array was expected but something else is given"); errResult)
        end
    in
      trexp exp
    end
    and transDec(venv, tenv, A.VarDec {name, escape, typ = NONE, init, pos}, level, break) =
    let
      val {exp, ty} = transExp (venv, tenv, init, level, break)
      val access' = L.allocLocal (level) (!escape)
      val varexp = L.simpleVar (access', level)
    in
      {tenv = tenv, venv = S.enter (venv, name, E.VarEntry {access = access', ty = ty}), expList = [L.assign (varexp, exp)]}
    end
    | transDec(venv, tenv, A.VarDec {name, escape, typ = SOME (tname, tpos), init, pos}, level, break) =
    let
      val {exp, ty} = transExp (venv, tenv, init, level, break)
      val access' = L.allocLocal level (!escape)
      val varexp = L.simpleVar (access', level)
    in
      case S.look (tenv, tname) of 
        NONE => (typeNotFound(tenv, pos, "type", tname); {tenv = tenv, venv = S.enter(venv, name, E.VarEntry{access = access', ty = ty}), expList = []})
      | SOME(dty) =>
          let
            val at = actual_ty(tenv, dty, pos) 
          in
            (checkType(tenv, augmentR(at), augmentR(ty), pos);
            {tenv = tenv, venv = S.enter(venv, name, E.VarEntry{access = access', ty = at}), expList = [L.assign (varexp, exp)]})
          end
    end
    (*
      The solution for a set of mutually recursive things (types or functions) t1,...,tn is to put all the "headers" in the environment first, resulting in an environment e1. Then process all the "bodies" in the environment e1. During processing of the bodies it will be necessary to look up some of the newly defined names, but they will in fact be there - though some of them may be empty headers without bodies. 

      What is a header? For a type declaration such as
      type list = {first: int, rest: list}
      the header is approximately type list =.
      To enter this header into an environment tenv we can use a NAME type with an empty binding (ty option):
      tenv' = S.enter(tenv, name, Types.NAME(name, ref NONE))
      Now, we can call transTy on the "body" of the type declaration, that is, on the record expression {first: int, rest: list}. The environment we give to transTy will be tenv'.
      It's important that transTy stop as soon as it gets to any NAME type. If, for example, transTy behaved like actual_ty and tried to look "through" the NAME type bound to the identifier list, all it would find (in this case) would be NONE - which it is certainly not prepared for. This none can be replaced only by a valid type after the entire {first: int, rest: list} is translated. For more info, refer pg 120. 
    *)
    | transDec (venv, tenv, A.TypeDec (tdecs), level, break) =
      let
        fun processHeaders() = foldl (fn ({name, ...}, ienv) => (S.enter(ienv, name, T.NAME(name, ref NONE)))) tenv tdecs 

        val tenv' = processHeaders()

        fun processBody({name, ty, pos}) = 
        case S.look(tenv', name) of 
          SOME(T.NAME(name', ref')) => ref' := SOME(transTy(tenv', ty))
        
        fun processBodies() = app processBody tdecs 
        (* Used to detect cycles *)
        val seen = ref S.empty
        (* Returns true if it detects the cycle *)
        fun cycleExists(SOME(t)) =
          case t of
            T.NAME(sym, r) =>
            if (S.look(!seen, sym) = NONE) then cycleExists(!r) else true
          | _ => false

        fun traverse(nil) = ()
          | traverse({name, ty, pos} :: ls) =
            case S.look(tenv', name) of
              SOME(T.NAME(_, r)) => (
                seen := S.enter(!seen, name, 1); 
                if (cycleExists(!r)) then
                  (Err.error pos ("Cyclic definition starting from " ^ S.name(name)))
                else traverse(ls)
              )
      in 
        (* 
          Every cycle in a set of mutually recursive type declarations must pass through a record or array declaration; the declaration
          type a = b
          type b = d
          type c = a
          type d = a 
          contains an illegal cycle a —► b —► d —► a. Illegal cycles should be detected by the type-checker.
        *)
        (
        processBodies();
        traverse(tdecs);
        checkDup(map #name tdecs, map #pos tdecs);
        {venv = venv, tenv = tenv', expList = []}
        )
      end    
    | transDec (venv, tenv, A.FunctionDec(fundecs), level, break) =
      let
        (* The first pass gathers information about the header of each function (function name, formal  parameter list, return type) but leaves the bodies of the functions untouched. In this pass, the types of the formal parameters are needed, but not their names *)
        (* The second pass processes the bodies of all functions in the mutually recursive declaration, taking advantage of the environment augmented with all the function headers. For each body, the formal parameter list is processed again, this time entering the parameters as VarEntrys in the value environment. *)
        (* first pass on a fundec: check formal types, and store header info in the venv. *)
        fun transfun ({name, params, result, body, pos}, env) =
        let 
          val rt =
            case result of
              NONE => T.UNIT (* procedure - should return unit *)
            | SOME(t, pos) =>
              (case S.look(tenv, t) of
                SOME ty => ty
              | NONE =>
                (typeNotFound(tenv, pos, "type", t);
                  T.UNIT)
              )
          (* Transparam *)
          val formalsTy =
            map (fn ({typ, name, ...}: A.field) =>
                case S.look(tenv, typ) of
                  SOME t => t
                | NONE =>
                  (typeNotFound(tenv, pos, "function's parameter for function " ^ S.name(name), typ); 
                    T.UNIT)
                ) params
          val escapeL = map (fn {escape, ...} => !escape) params
        in
          checkDup(map #name params, map #pos params);
          S.enter(env, name, E.FunEntry{level = L.newLevel {parent = level, name = name, formals = escapeL}, label = name, formals = formalsTy, result = rt})
        end
        val venv' = foldl transfun venv fundecs
        (* First Part Done! *)
        (* second pass on a fundec: do type checking, put VarEntry on venv, and check body *)
        fun transbody ({name, params, result, body, pos}) =
        let
          (* Must Match *)
          val SOME(E.FunEntry{result, level = newlevel, ...}) =
              S.look(venv', name)

          fun enterparam ({name, escape, typ, pos}, access) =
              case S.look(tenv, typ) of
                  SOME t => {access = access, name = name, ty = t}
                | NONE =>
                  (typeNotFound(tenv, pos, "function's parameter", typ); {access = access, name = name, ty = T.UNIT})
          val params' = ListPair.map enterparam (params, L.formals newlevel)
          val venv'' =
              (foldl (fn ({access, name, ty}, env) =>
                      S.enter(env, name, E.VarEntry{access = access, ty = ty})
                      )
                    venv' params')
          val {exp, ty} = transExp(venv'', tenv, body, newlevel, break)
        in 
          checkType(tenv, augmentR(result), augmentR(ty), pos);
          L.procEntryExit (newlevel, exp);
          ()
        end
      in
        checkDup(map #name fundecs, map #pos fundecs);
        (app transbody fundecs);
        {venv = venv', tenv = tenv, expList = []} 
      end
    (* transTy translates type expressions as found in the abstract syntax to the corresponding digested type in Types(T).ty *)
    and transTy (tenv, A.NameTy (sym, pos)) = (case S.look(tenv, sym) of SOME(t) => t)
      | transTy (tenv, A.RecordTy (fields)) =
        (checkDup(map #name fields, map #pos fields);
        T.RECORD (
          (map (fn {name, escape, typ, pos} =>
                    case S.look(tenv, typ) of
                      SOME(t) => (name, t)
                    | NONE => (typeNotFound(tenv, pos, "type", typ); (name, T.UNIT))
                ) fields
          ), ref ()))

      | transTy (tenv, A.ArrayTy (sym, pos)) =
        case S.look(tenv, sym) of
          SOME(t) => T.ARRAY(t, ref ())
        | NONE => (typeNotFound(tenv, pos, "type", sym); T.ARRAY(T.NIL, ref ()))
    fun transProg (my_exp : A.exp) = 
    let
      (* Clear fragment list *)
      val _ = L.reset ()  
      (* With every call to newLevel, Semant must pass the enclosing level value. When creating the level for the "main" Tiger program (one not within any Tiger function), Semant should pass a special level value: Translate.outermost. This is not the level of the Tiger main program, it is the level within which that program is nested. All "library" functions are declared (as described at the end of Section 5.2) at this outermost level, which does not contain a frame or formal parameter list. *)
      val mainLabel = Temp.namedlabel "main"
      val mainLevel = L.newLevel {parent = L.outermost, name = mainLabel, formals = []}
      val {exp, ty} = transExp (E.base_venv, E.base_tenv, my_exp, mainLevel, mainLabel)
    in
      (* The semantic analysis phase calls upon Translate.newLevel in  processing a function header. Later it calls other interface fields of Translate to translate the body of the Tiger function; this has the side effect of remembering string fragments for any string literals encountered, Finally the semantic analyzer calls procEntryExit, which has the side effect of remembering a PROC fragment.  *)
      L.procEntryExit (mainLevel, exp);
      L.getResult()
    end
end
