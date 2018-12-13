{
open Parser
}

let str_name = ['a' - 'z' 'A' - 'Z'] + ['a' - 'z' 'A' - 'Z' '0' - '9']*
let numeric = ['0' - '9']+
let white_space = [' ' '\t' '\n' '\r' '\b']+

rule main = parse
        | white_space   { main lexbuf }
        | str_name as str { STR_NAME(str) }
        | numeric as str_n  { NUM(str_n) }
        | "true"        { TRUE }
        | "false"       { FALSE }
        | "int"         { INT }
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
        | "else"            { ELSE }
        | "else if"         { ELSE_IF }
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
        | "<="        { LOWER_EQ }
        | ">="        { GREATER_EQ }
        | "=="        { EQ }
        | "!="        { NON_EQ }
        | "="         { ASSIGN }
        | "+="        { PLUS_ASSIGN }
        | "-="        { MINUS_ASSIGN }
        | "("         { ROUND_BR_OPEN }
        | ")"         { ROUND_BR_CLOSE }
        | "{"         { CURLY_BR_OPEN }
        | "}"         { CURLY_BR_CLOSE }
        | "["         { SQUARE_BR_OPEN }
        | "]"         { SQUARE_BR_CLOSE }
        | ":"         { COLON }
        | ";"         { SEMICOLON }
        | ","         { COMMA }
        | eof         { EOF }
