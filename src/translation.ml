open Tree;;

let type_basic_to_c = function
	| Int -> 	"int"
	| Long -> 	"long long"
	| Float -> 	"double"
	| Bool -> 	"char"
	| String -> "char*"
	| Void 	-> 	"void"
;;

let rec type_array_to_c = function
	| Eps -> ""
	| TypeArray (n, type_array) -> "[" ^ n ^ "]" ^ (type_array_to_c type_array);;
;;

let language_type_to_c = function
 	| Type (type_basic, type_array) -> (type_basic_to_c type_basic) ^ (type_array_to_c type_array)
;;

let arg_to_c = function
	| Arg (name, language_type) -> (language_type_to_c language_type) ^ " " ^ name
;;

let rec arg_list_to_c = function
	| Nil -> ""
	| ArgList (arg, Nil) -> arg_to_c arg
	| ArgList (arg, arg_list) -> (arg_to_c arg) ^ ", " (arg_list_to_c arg_list)
;;

let func_call_to_c = function
	| FuncCall (name, exp_list) -> name ^ "(" ^ (exp_list_to_c exp_list) ^ ")"
and
exp_to_c = function
	| BinaryOp (op, a, b) -> (exp_to_c a) ^ " " ^ op ^ " " ^ (exp_to_c b)
	| UnaryOp (op, a) -> op ^ " " ^ (exp_to_c a)
	| ExpFuncCall (func_call) -> func_call_to_c func_call
	| Str_name (str) -> str
	| Num (str) -> str
	| True -> "\'\\1\'"
	| False -> "\'\\0\'"
and
exp_list = function
	| Nil -> ""
	| ExpList (exp, Nil) ->	exp_to_c exp
	| ExpList (exp, exp_list) -> (exp_to_c exp) ^ ", " ^ (exp_list_to_c exp_list)
;;

let var_decl_to_c = function
	| VarDecl (name, language_type) -> (language_type_to_c language_type) ^ " " ^ name
;;

let local_statement_to_c = function
	| LocalVarDecl (var_decl) -> (var_decl_to_c var_decl) ^ ";"
	| For (var_decl, init, cond, post_action, body) -> "for (" ^ (var_decl_to_c var_decl) ^ " = " ^ (exp_to_c init) ^ "; " ^
														(exp_to_c cond) ^ "; " ^ (exp_to_c post_action) ^ ") {\n" ^ (body_to_c body) ^ "}\n"
	| While (cond, body) -> "while (" ^ (exp_to_c cond) ^ ") {\n" ^ (body_to_c body) ^ "}\n"
	| If (cond, body, if_cont) -> "if (" ^ (exp_to_c cond) ^ ") {\n" ^ (body_to_c body) ^ "}\n" ^ (if_cont_to_c if_cont)
	| Ret (exp) -> "return " ^ (exp_to_c exp) ^ ";"
	| RetEmpty -> "return;"
and
body_to_c = function
	| Eps -> ""
	| LocalStatementsList (local_statement, body) -> "\t" ^ (local_statement_to_c local_statement) ^ "\n" ^ (body_to_c body)
and
if_cont_to_c = function
	| Eps -> ""
	| ElseIf (cond, body, if_cont) -> "else if (" ^ (exp_to_c cond) ^ ") {\n" ^ (body_to_c body) ^ "}\n" ^ (if_cont_to_c if_cont)
	| Else (body) -> "else {\n" ^ (body_to_c body) ^ "}\n"
;;

let rec global_statement_to_c = function
    | GlobalVarDecl (var_decl) -> (var_decl_to_c var_decl) ^ ";"
    | Function (name, arg_list, language_type, body) -> (language_type_to_c language_type) ^ " " ^ name ^ "(" ^ (arg_list_to_c arg) ^ ")" ^
														" {\n" ^ (body_to_c body) ^ "}\n"
    | GlobFuncCall (func_call) -> (func_call_to_c func_call) ^ ";"
;;

let rec tree_to_c = function
	| Eps -> ""
	| GlobalStatementsList (global_statement, tree) -> (global_statement_to_c global_statement) ^ "\n" ^ (tree_to_c tree)
;;
