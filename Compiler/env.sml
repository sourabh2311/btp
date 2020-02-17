structure Env : ENV =
struct

    structure S = Symbol
    structure T = Types
    type access = unit
    type ty = Types.ty
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result : ty}

    (* predefined types *)
    val base_tenv = 
    let
        fun addtotable ((s, t), table) = S.enter(table, S.symbol s, t)
        val toadd = [("int", T.INT), ("real", T.REAL), ("string", T.STRING)]
    in
        foldr addtotable S.empty toadd
    end

    val base_funs =
    (* function name, input parameters (formals), return type *)
    [
        ("exit", [T.INT], T.UNIT),
        ("not", [T.INT], T.INT),
        ("size", [T.STRING], T.INT),
        ("concat", [T.STRING, T.STRING], T.STRING),
        ("substring", [T.STRING, T.INT, T.INT], T.STRING),
        ("stringGreat", [T.STRING, T.STRING], T.INT),
        ("stringLess", [T.STRING, T.STRING], T.INT),
        ("stringEqual", [T.STRING, T.STRING], T.INT),
        ("chr", [T.INT], T.STRING),
        ("ord", [T.STRING], T.INT),
        ("getchar", [], T.STRING),
        ("flush", [], T.UNIT),
        ("print", [T.STRING], T.UNIT),
        ("printI", [T.INT], T.UNIT),
        ("printR", [T.REAL], T.UNIT),
        ("radd", [T.REAL, T.REAL], T.REAL),
        ("rsub", [T.REAL, T.REAL], T.REAL),
        ("rmul", [T.REAL, T.REAL], T.REAL),
        ("rdiv", [T.REAL, T.REAL], T.REAL),
        ("realEqual", [T.REAL, T.REAL], T.INT),
        ("realLess", [T.REAL, T.REAL], T.INT),
        ("realGreat", [T.REAL, T.REAL], T.INT)
        (* initArray, allocRecord are external calls *)
    ]

    (* predefined functions *)
    (* With every call to newLevel, Semant must pass the enclosing level value. When creating the level for the "main" Tiger program (one not within any Tiger function), Semant should pass a special level value: Translate. outermost. This is not the level of the Tiger main program, it is the level within which that program is nested. All "library" functions are declared (as described at the end of Section 5.2) at this outermost level, which does not contain a frame or formal parameter list. *)
    val base_venv =
        List.foldr
            (fn ((name, formals, result), env) =>
            let 
                val label = Temp.namedlabel name 
            in
                S.enter (env, S.symbol(name), FunEntry{level = Translate.newLevel {parent = Translate.outermost, name = label, formals = map (fn _ => false) formals, isRealL = map (fn f => case f of T.REAL => true | _ => false) formals}, label = label, formals = formals, result = result})
            end)
            S.empty base_funs

end