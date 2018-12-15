{
open Parser
}

let str_name = ['a' - 'z' 'A' - 'Z' '_']+ ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*
let numeric = ['0' - '9']+ | ['0' - '9']* '.' ['0' - '9']+
let str_literal = '"' [^'"']* '"'
let white_space = [' ' '\t' '\n' '\r' '\b']+

rule main = parse
        | white_space   { main lexbuf }
        | "true"        { TRUE }
        | "false"       { FALSE }
        | "int"         {  INT }
        | "long"            { LONG }
        | "float"           { FLOAT }
        | "bool"            { BOOL }
        | "string"          { STRING }
        | "void"            { VOID }
        | "fun"         { FUN }
        | "ret"         { RET }
        | "for"             { FOR }
        | "while"           { WHILE }
        | "if"              { IF }
        | "else if"         { ELSE_IF }
        | "else"            { ELSE }
        | "+="        { PLUS_ASSIGN }
        | "-="        { MINUS_ASSIGN }
        | "=="        { EQ }
        | "<="        { LOWER_EQ }
        | ">="        { GREATER_EQ }
        | "!="        { NON_EQ }
        | "+"         { PLUS }
        | "-"         { MINUS }
        | "*"         { MUL }
        | "/"         { DIV }
        | "mod"       { MOD }
        | "and"       { AND }
        | "or"        { OR }
        | "not"       { NOT }
        | "<"         { LOWER }
        | ">"         { GREATER }
        | "="         { ASSIGN }
        | "("         { ROUND_BR_OPEN }
        | ")"         { ROUND_BR_CLOSE }
        | "{"         { CURLY_BR_OPEN }
        | "}"         { CURLY_BR_CLOSE }
        | "["         { SQUARE_BR_OPEN }
        | "]"         { SQUARE_BR_CLOSE }
        | ":"         { COLON }
        | ";"         { SEMICOLON }
        | ","         { COMMA }
        | str_name as str               { STR_NAME(str) }
        | numeric as str_n              { NUM(str_n) }
        | str_literal as str_literal    { STR_LITERAL(str_literal) }
        | eof                           { EOF }
