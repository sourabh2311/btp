(* In our lexer, each line will occur at a particular "pos". "pos" is incremented for each character. Our goal is that given "pos", we have to determine the line number and column number. Reason of doing this in such an odd way is because... *)

signature ERRORMSG =
sig
    val anyErrors : bool ref  (* has their been an occurance of any error? *)
    val fileName : string ref
    val lineNum : int ref (* Number of lines in the read file *)
    val linePos : int list ref (* as defined in the top most comment, it is the list containing value of "pos" at each line *)
    val sourceStream : TextIO.instream ref
    val error : int -> string -> unit (* it should take "pos" and error message to print, from "pos" it will determine the line number and the column number, and will print abstractly "filename:lineNo.ColNo:Message". *)
    exception Error
    val impossible : string -> 'a   (* raises Error, behaviour we didn't expect *)
    val reset : unit -> unit  (* reset the parameters, so that we can move on to read new file *)
end

structure ErrorMsg : ERRORMSG =
struct

    val anyErrors = ref false
    val fileName = ref ""
    val lineNum = ref 1
    val linePos = ref [1]
    val sourceStream = ref TextIO.stdIn

    fun reset() = 
    (
        anyErrors := false;
        fileName := "";
        lineNum := 1;
        linePos := [1];
        sourceStream := TextIO.stdIn
    )

    exception Error

    fun error pos (msg : string) =
    let 
        fun look (a :: rest, n) =
            if a < pos then [", error at line: ", Int.toString n, " and column: ", Int.toString (pos - a), "."]
            else look(rest, n - 1)
        | look _ = [", error at line: 0 and column: 0."]
    in 
        anyErrors := true;
        app print (["Filename: ", !fileName] @ look(!linePos, !lineNum) @ [" Message: ", msg, "\n"])
    end

    fun impossible msg =
    (
        app print ["Error: Compiler bug: ", msg, "\n"];
        TextIO.flushOut TextIO.stdOut;
        raise Error
    )

end  