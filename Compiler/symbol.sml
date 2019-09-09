signature SYMBOL =
sig
  eqtype symbol  (* types for which equality is defined, like int has equality defined unlike float *)
  val symbol : string -> symbol (* map string to symbol *)
  val name : symbol -> string (* reverse to above *)
  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val look  : 'a table * symbol -> 'a option
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string, int) H.hash_table = H.mkTable(HashString.hashString, op = ) (sizeHint, Symbol)  (* so for our hash table, key is string and value is int *)
  
  fun symbol name =
  case H.find hashtable name
    of SOME i => (name, i)
    | NONE => 
    let 
      val i = !nextsym
    in 
      nextsym := i + 1;
      H.insert hashtable (name, i);
      (name, i)
    end

  fun name(s, n) = s

(* Till now we hashed string to int. Now, we will hash that int to 'a *)
(* "Tables" map symbols to bindings *)
(* 
  We want different notions of binding for 
  different purposes in the compiler - type bindings for types, value bindings for variables and functions so we let table be a polymorphic type 
*)
(* 
  We will have a type environment and a value environment. The
  following Tiger program demonstrates that one environment will not suffice:
  let 
    type a = int
    var a : a := 5
    var b : a := a
  in 
    b + a
  end
  The symbol "a" denotes the type "a" in syntactic contexts where type identifiers
  are expected, and the variable "a" in syntactic contexts where variables are expected
  For a type identifier, we need to remember only the type that it stands for.
  Thus a type environment is a mapping from symbol to Types.ty 
*)

  structure Table = IntMapTable(type key = symbol fun getInt(s, n) = n)

  type 'a table = 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val look = Table.look
end
