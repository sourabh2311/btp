(* temp -> register, label -> label *)
structure Temp : TEMP =
struct
    type temp = int * int
    type ptemp = temp * temp
    val temps = ref 100
    (* Defining a way to compare temp pairs *)
    fun tcompare (t1 as (t1a, t1b), t2 as (t2a, t2b)) = if Int.compare(t1a, t2a) = EQUAL then Int.compare(t1b, t2b) else Int.compare(t1a, t2a)
    fun pcompare ((t1a, t1b), (t2a, t2b)) = if tcompare(t1a, t2a) = EQUAL then tcompare(t1b, t2b) else tcompare(t1a, t2a)
    structure TempKey : ORD_KEY =
    struct
        type ord_key = temp
        fun compare (t1, t2) = tcompare (t1, t2)
    end
    structure TempPKey : ORD_KEY =
    struct
        type ord_key = temp * temp
        fun compare (t1, t2) = pcompare (t1, t2)
    end
    structure TempSet = RedBlackSetFn (TempKey)
    structure TempPSet = RedBlackSetFn (TempPKey)
    structure TempMap = RedBlackMapFn (TempKey)
    fun newtemp(flag') = let val t = !temps in temps := t + 1; (t, flag') end
    fun isReal (temp as (a, b)) = if b = 1 then true else false
    fun printTemp (t as (a, b)) = "(" ^ Int.toString(a) ^ ", " ^ Int.toString(b) ^ ")"
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
