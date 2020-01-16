structure Tokens = Tokens

(* Standard Glue *)
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

(* Second parameter (yypos + size yytext) never gets used. *)
(* Regarding String matching, we just want to match string inside double quotes, but we need to be careful with when double quote is part of the string or when it is string terminator *)
(* Remember that we cannot put comments inside rules *)
val lineNo = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val colNo = ref 1
val givenString = ref ""
val stringOpen = ref 0
val commentDepth = ref 0

fun printError (errorCode, yytext) = 
(
  print ("LEXING ERROR: Error is at line no: " ^ Int.toString (!lineNo) ^ " and column no is: " ^ Int.toString (!colNo) ^ ". Message: ");
  if (errorCode = 1) then
    print ("Reached EOF with comment not terminated\n")
  else if (errorCode = 2) then  
    print ("Reached EOF with string not terminated\n")
  else if (errorCode = 3) then
    print ("Found comment terminator without corresponding comment beginner\n")
  else if (errorCode = 4) then
    print ("Newline without terminating string\n")
  else 
    print ("Symbol: " ^ yytext ^ " not valid, remember that identifier should begin with a lower case character\n")
  ; 
  OS.Process.exit (OS.Process.success)
)

fun eof() = 
let 
    val CStatus = !commentDepth
    val SStatus = !stringOpen
    val _ = if (CStatus = 1) then printError (1, "") else ()
    val _ = if (SStatus = 1) then printError (2, "") else ()
in  
    Tokens.EOF(!lineNo, !colNo)
end

%% 
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s COMMENT STRING;

%%

<INITIAL, COMMENT>\n	=>                     (colNo := 1; lineNo := !lineNo + 1; linePos := yypos :: (!linePos); continue());
<INITIAL, COMMENT>[\ \t]+ =>                 (colNo := !colNo + size yytext; continue());
<INITIAL>"type" =>                           (colNo := !colNo + size yytext; Tokens.TYPE(yypos, yypos + size yytext));
<INITIAL>"var" =>                            (colNo := !colNo + size yytext; Tokens.VAR(yypos, yypos + size yytext));
<INITIAL>"function" =>                       (colNo := !colNo + size yytext; Tokens.FUNCTION(yypos, yypos + size yytext));
<INITIAL>"break" =>                          (colNo := !colNo + size yytext; Tokens.BREAK(yypos, yypos + size yytext));
<INITIAL>"of" =>                             (colNo := !colNo + size yytext; Tokens.OF(yypos, yypos + size yytext));
<INITIAL>"end" =>                            (colNo := !colNo + size yytext; Tokens.END(yypos, yypos + size yytext));
<INITIAL>"in" =>                             (colNo := !colNo + size yytext; Tokens.IN(yypos, yypos + size yytext));
<INITIAL>"nil" =>                            (colNo := !colNo + size yytext; Tokens.NIL(yypos, yypos + size yytext));
<INITIAL>"let" =>                            (colNo := !colNo + size yytext; Tokens.LET(yypos, yypos + size yytext));
<INITIAL>"do" =>                             (colNo := !colNo + size yytext; Tokens.DO(yypos, yypos + size yytext));
<INITIAL>"to" =>                             (colNo := !colNo + size yytext; Tokens.TO(yypos, yypos + size yytext));
<INITIAL>"for" =>                            (colNo := !colNo + size yytext; Tokens.FOR(yypos, yypos + size yytext));
<INITIAL>"while" =>                          (colNo := !colNo + size yytext; Tokens.WHILE(yypos, yypos + size yytext));
<INITIAL>"else" =>                           (colNo := !colNo + size yytext; Tokens.ELSE(yypos, yypos + size yytext));
<INITIAL>"then" =>                           (colNo := !colNo + size yytext; Tokens.THEN(yypos, yypos + size yytext));
<INITIAL>"if" =>                             (colNo := !colNo + size yytext; Tokens.IF(yypos, yypos + size yytext));
<INITIAL>"array" =>                          (colNo := !colNo + size yytext; Tokens.ARRAY(yypos, yypos + size yytext));
<INITIAL>":=" =>                             (colNo := !colNo + size yytext; Tokens.ASSIGN(yypos, yypos + size yytext));
<INITIAL>"|" =>                              (colNo := !colNo + size yytext; Tokens.OR(yypos, yypos + size yytext));
<INITIAL>"&" =>                              (colNo := !colNo + size yytext; Tokens.AND(yypos, yypos + size yytext));
<INITIAL>">=" =>                             (colNo := !colNo + size yytext; Tokens.GE(yypos, yypos + size yytext));
<INITIAL>">" =>                              (colNo := !colNo + size yytext; Tokens.GT(yypos, yypos + size yytext));
<INITIAL>"<=" =>                             (colNo := !colNo + size yytext; Tokens.LE(yypos, yypos + size yytext));
<INITIAL>"<" =>                              (colNo := !colNo + size yytext; Tokens.LT(yypos, yypos + size yytext));
<INITIAL>"<>" =>                             (colNo := !colNo + size yytext; Tokens.NEQ(yypos, yypos + size yytext));
<INITIAL>"=" =>                              (colNo := !colNo + size yytext; Tokens.EQ(yypos, yypos + size yytext));
<INITIAL>"/" =>                              (colNo := !colNo + size yytext; Tokens.DIVIDE(yypos, yypos + size yytext));
<INITIAL>"*" =>                              (colNo := !colNo + size yytext; Tokens.TIMES(yypos, yypos + size yytext));
<INITIAL>"-" =>                              (colNo := !colNo + size yytext; Tokens.MINUS(yypos, yypos + size yytext));
<INITIAL>"+" =>                              (colNo := !colNo + size yytext; Tokens.PLUS(yypos, yypos + size yytext));
<INITIAL>">>" =>                              (colNo := !colNo + size yytext; Tokens.RSHIFT(yypos, yypos + size yytext));
<INITIAL>"<<" =>                              (colNo := !colNo + size yytext; Tokens.LSHIFT(yypos, yypos + size yytext));
<INITIAL>"." =>                              (colNo := !colNo + size yytext; Tokens.DOT(yypos, yypos + size yytext));
<INITIAL>"}" =>                              (colNo := !colNo + size yytext; Tokens.RBRACE(yypos, yypos + size yytext));
<INITIAL>"{" =>                              (colNo := !colNo + size yytext; Tokens.LBRACE(yypos, yypos + size yytext));
<INITIAL>"]" =>                              (colNo := !colNo + size yytext; Tokens.RBRACK(yypos, yypos + size yytext));
<INITIAL>"[" =>                              (colNo := !colNo + size yytext; Tokens.LBRACK(yypos, yypos + size yytext));
<INITIAL>")" =>                              (colNo := !colNo + size yytext; Tokens.RPAREN(yypos, yypos + size yytext));
<INITIAL>"(" =>                              (colNo := !colNo + size yytext; Tokens.LPAREN(yypos, yypos + size yytext));
<INITIAL>";" =>                              (colNo := !colNo + size yytext; Tokens.SEMICOLON(yypos, yypos + size yytext));
<INITIAL>":" =>                              (colNo := !colNo + size yytext; Tokens.COLON(yypos, yypos + size yytext));
<INITIAL>"," =>                              (colNo := !colNo + size yytext; Tokens.COMMA(yypos, yypos + size yytext));
<INITIAL>[0-9]* =>                           (colNo := !colNo + size yytext; Tokens.INT(valOf (Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL>[0-9]*"."[0-9]* =>                  (colNo := !colNo + size yytext; Tokens.REAL(valOf (Real.fromString yytext), yypos, yypos + size yytext));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]* =>            (colNo := !colNo + size yytext; Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>"/*" =>                             (colNo := !colNo + size yytext; commentDepth := 1; YYBEGIN COMMENT; continue());
<INITIAL>"*/" =>                             (printError (3, yytext); continue());
<COMMENT>"/*" =>                             (colNo := !colNo + size yytext; commentDepth := (!commentDepth + 1); continue());
<COMMENT>"*/" =>                             (colNo := !colNo + size yytext; commentDepth := (!commentDepth - 1); if (!commentDepth = 0) then YYBEGIN INITIAL else (); continue());
<COMMENT>. =>                                (colNo := !colNo + size yytext; continue());
<INITIAL>\" =>                               (colNo := !colNo + size yytext; YYBEGIN STRING; stringOpen := 1; givenString := ""; continue ());
<STRING>[^\n\"]+ =>                          (colNo := !colNo + size yytext; givenString := !givenString ^ yytext; continue());
<STRING>\\\" =>                              (colNo := !colNo + size yytext; givenString := !givenString ^ yytext; continue());
<STRING>\" =>                                (colNo := !colNo + size yytext; stringOpen := 0; YYBEGIN INITIAL; Tokens.STRING(!givenString, yypos, yypos + size yytext));
<STRING>\n =>                                (printError (4, yytext); continue());
<INITIAL>. =>                                (printError (5, yytext); continue());
