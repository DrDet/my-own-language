{
open Parser

(* let parse_buf_exn lexbuf =
  try
    T.input T.rule lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Sql_lexer.ruleTail "" lexbuf in
      raise (Error (exn,(line,cnum,tok,tail)))
    end *)
}

let str_name = ['a' - 'z' 'A' - 'Z'] + ['a' - 'z' 'A' - 'Z' '0' - '9']*
let numeric = ['0' - '9']+
let white_space = [' ' '\t' '\n' '\r' '\b']+

rule main = parse
        | white_space   { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; main lexbuf }
        | "true"        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; TRUE }
        | "false"       { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; FALSE }
        | "int"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  INT }
        | "long"            { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; LONG }
        | "float"           { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; FLOAT }
        | "bool"            { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; BOOL }
        | "string"          { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; STRING }
        | "void"            { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; VOID }
        | "fun"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  FUN }
        | "ret"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; RET }
        | "for"             { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; FOR }
        | "while"           { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; WHILE }
        | "if"              { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; IF }
        | "else if"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; ELSE_IF }
        | "else"            { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; ELSE }
        | "+="        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; PLUS_ASSIGN }
        | "-="        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; MINUS_ASSIGN }
        | "=="        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; EQ }
        | "<="        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; LOWER_EQ }
        | ">="        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; GREATER_EQ }
        | "!="        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; NON_EQ }
        | "+"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; PLUS }
        | "-"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; MINUS }
        | "*"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; MUL }
        | "/"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; DIV }
        | "mod"       { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; MOD }
        | "and"       { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; AND }
        | "or"        { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; OR }
        | "not"       { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; NOT }
        | "<"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; LOWER }
        | ">"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; GREATER }
        | "="         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; ASSIGN }
        | "("         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  ROUND_BR_OPEN }
        | ")"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;   ROUND_BR_CLOSE }
        | "{"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  CURLY_BR_OPEN }
        | "}"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  CURLY_BR_CLOSE }
        | "["         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  SQUARE_BR_OPEN }
        | "]"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  SQUARE_BR_CLOSE }
        | ":"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; COLON }
        | ";"         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  SEMICOLON }
        | ","         { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ;  COMMA }
        | str_name as str { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; STR_NAME(str) }
        | numeric as str_n  { let x = Lexing.lexeme lexbuf in Printf.printf "lexeme:~%s~\n" x ; NUM(str_n) }
        | eof         { Printf.printf "lexeme: EOF\n";  EOF }
