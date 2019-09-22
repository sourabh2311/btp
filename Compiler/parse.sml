structure Parse : sig val parse : string -> Absyn.exp end = 
struct

(* 
 * We apply the functors generated from tiger.lex and tiger.grm to produce
 * the TigerParser structure.
 *)

  structure TigerLrVals =
    TigerLrValsFun(structure Token = LrParser.Token)

  structure TigerLex =
    TigerLexFun(structure Tokens = TigerLrVals.Tokens)

  structure TigerParser =
    Join(
      structure LrParser = LrParser
	    structure ParserData = TigerLrVals.ParserData
	    structure Lex = TigerLex)

  fun parse filename =
  let 
    val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  fun parseerror(s, p1, p2) = ErrorMsg.error p1 s
	  val lexer = LrParser.Stream.streamify (TigerLex.makeLexer get)
	  val (absyn, _) = TigerParser.parse(30, lexer, parseerror,())
  in 
    TextIO.closeIn file;
	  absyn
  end 
  (* exceptions can be handled cleanly instead of simply causing the interpreter to print an error. *)
  handle LrParser.ParseError => raise ErrorMsg.Error

end



