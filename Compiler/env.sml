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
        val toadd = [("int", T.INT), ("string", T.STRING)]
    in
        foldr addtotable S.empty toadd
    end

    val base_funs =
    (* function name, input parameters (formals), return type *)
    [("print", [T.STRING], T.UNIT),
     (* ("printi", [T.INT], T.UNIT), *)
     ("flush", [], T.UNIT),
     ("getchar", [], T.STRING),
     ("ord", [T.STRING], T.INT),
     ("chr", [T.INT], T.STRING),
     ("size", [T.STRING], T.INT),
     ("substring", [T.STRING, T.INT, T.INT], T.INT),
     ("concat", [T.STRING, T.STRING], T.STRING),
     ("not", [T.INT], T.INT),
     ("strcmp", [T.STRING, T.STRING], T.INT),
     ("exit", [T.INT],  T.UNIT)]

    (* predefined functions *)
    (* With every call to newLevel, Semant must pass the enclosing level value. When creating the level for the "main" Tiger program (one not within any Tiger function), Semant should pass a special level value: Translate. outermost. This is not the level of the Tiger main program, it is the level within which that program is nested. All "library" functions are declared (as described at the end of Section 5.2) at this outermost level, which does not contain a frame or formal parameter list. *)
    val base_venv =
        List.foldr
            (fn ((name, formals, result), env) =>
            let 
                val label = Temp.namedlabel name 
            in
                (* making no variable escape now, will handle later in newFrame method of mipsframe.sml *)
                S.enter (env, S.symbol(name), FunEntry{level = Translate.newLevel {parent = Translate.outermost, name = label, formals = map (fn _ => false) formals}, label = label, formals = formals, result = result})
            end)
            S.empty base_funs

end