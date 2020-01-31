(* temp -> register, label -> label *)
structure Temp : TEMP =
struct
    type temp = int
    type ptemp = temp * temp
    val temps = ref 100
    (* Defining a way to compare temp pairs *)
    fun pcompare ((t1a, t1b), (t2a, t2b)) = if (t1a < t2a orelse (t1a = t2a andalso t1b < t2b)) then LESS else if (t1a = t2a andalso t1b = t2b) then EQUAL else GREATER
    structure TempKey : ORD_KEY =
    struct
        type ord_key = temp
        fun compare (t1, t2) = Int.compare(t1, t2)
    end
    structure TempPKey : ORD_KEY =
    struct
        type ord_key = temp * temp
        fun compare (t1, t2) = pcompare (t1, t2)
    end
    structure TempSet = RedBlackSetFn (TempKey)
    structure TempPSet = RedBlackSetFn (TempPKey)
    structure TempMap = RedBlackMapFn (TempKey)
    fun newtemp() = let val t = !temps in temps := t + 1; t end

    structure Table = IntMapTable(type key = int fun getInt n = n)

    fun makestring t = "t" ^ Int.toString t

    type label = Symbol.symbol

    local 
        (* Can be seen, Format is like printf *)
        structure F = Format
        fun postinc x = let val i = !x in x := i + 1; i end
        val labs = ref 0
    in
        fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
        val namedlabel = Symbol.symbol
    end


end
