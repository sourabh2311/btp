signature ENV = 
sig
    type access
    type ty = Types.ty
    (* if a variable, what is its type; if a function, what are its parameter and result types, and so on. *)
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result : ty}
    val base_tenv : ty Symbol.table (* predefined types *)
    val base_venv : enventry Symbol.table (* predefined functions *)
    (* like flush, ord, chr, size etc defined in Appendix A *)
end