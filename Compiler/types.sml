structure Types =
struct

  type unique = unit ref

  datatype ty = 
  (* Since every "record type expression" creates a new (and different) record type, even if the fields are similar, we have a "unique" value to distinguish it. (The only interesting thing you can do with a unit ref is to test it for equality with another one; each ref is unique.)  Arrays work just like records: the ARRAY constructor carries the type of the array elements, and also a "unique" value to distinguish this array type from all others. If we were compiling some other language, we might have the following as a legal program:
  let 
    type a = {x: int, y: int}
    type b = {x: int, y: int}
    var i : a := ...
    var j : b := ..
  in 
    i := j
  end 
  This is illegal in Tiger, but would be legal in a language where structurally equivalent types are interchangeable. To test type equality in a compiler for such a language, we would need to examine record types field by field,  recursively. However, the following Tiger program is legal, since type c is the same as type a:
  let type a = {x: int, y: int}
      type c = a
      var i : a := ...
      var j : c := ...
  in i := j
  end
  It is not the type declaration that causes a new and distinct type to be made, but the type expression {x : int, y : int}.
  In Tiger, the expression nil belongs to any record type. We handle this exceptional case by inventing a special "nil" type. There are also expressions that return "no value," so we invent a type unit.
  When processing mutually recursive types, we will need a place-holder for types whose name we know but whose definition we have not yet seen. The type NAME(sym, ref(SOME(t))) is equivalent to type t; but NAME(sym, ref(NONE)) is just the place-holder.
*)
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of Symbol.symbol * ty option ref
          | UNIT

end

