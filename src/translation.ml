open Tree;;

let rec indent = function
 	| 0 -> ""
	| n -> "\t" ^ indent (n-1)
;;

let type_basic_to_c = function
	| Int -> 	"int"
	| Long -> 	"long long"
	| Float -> 	"double"
	| Bool -> 	"char"
	| String -> "char*"
	| Void 	-> 	"void"
;;

let rec type_array_to_c = function
	| EpsTArr -> ""
	| TypeArray (n, type_array) -> "[" ^ n ^ "]" ^ (type_array_to_c type_array);;
;;

let language_type_to_c = function
 	| Type (type_basic, type_array) -> (type_basic_to_c type_basic) ^ (type_array_to_c type_array)
;;

let arg_to_c = function
	| Arg (name, language_type) -> (language_type_to_c language_type) ^ " " ^ name
;;

let rec arg_list_to_c = function
	| NilArgList -> ""
	| ArgList (arg, NilArgList) -> arg_to_c arg
	| ArgList (arg, arg_list) -> (arg_to_c arg) ^ ", " ^ (arg_list_to_c arg_list)
;;

let rec exp_list_to_c = function
	| NilExpList -> ""
	| ExpList (exp, NilExpList) ->	exp_to_c exp
	| ExpList (exp, exp_list) -> (exp_to_c exp) ^ ", " ^ (exp_list_to_c exp_list)
and
exp_to_c = function
	| BinaryOp (op, a, b) -> (exp_to_c a) ^ " " ^ op ^ " " ^ (exp_to_c b)
	| UnaryOp (op, a) -> op ^ " " ^ (exp_to_c a)
	| ExpFuncCall (func_call) -> func_call_to_c func_call
	| Str_name (str) -> str
	| Num (str) -> str
	| True -> "\'\\1\'"
	| False -> "\'\\0\'"
	| ArrayAccess (name, type_array_access) -> name ^ (type_array_access_to_c type_array_access)
	| StrLiteral (str_literal) -> str_literal
and
func_call_to_c = function
	| FuncCall (name, exp_list) -> name ^ "(" ^ (exp_list_to_c exp_list) ^ ")"
and
type_array_access_to_c = function
	| EpsTArrAccess -> ""
	| TypeArrayAccess (exp, type_array_access) -> "[" ^ (exp_to_c exp) ^ "]" ^ (type_array_access_to_c type_array_access);;
;;

let var_decl_to_c = function
	| VarDecl (name, Type(type_basic, EpsTArr)) -> (type_basic_to_c type_basic) ^ " " ^ name
	| VarDecl (name, Type(type_basic, type_array)) -> (type_basic_to_c type_basic) ^ " " ^ name ^ (type_array_to_c type_array)
;;

let var_def_to_c = function
	| VarDef (name, language_type, exp) -> (language_type_to_c language_type) ^ " " ^ name ^ " = " ^ (exp_to_c exp)
;;

let prepare_cs_to_c rhs =
    let rec impl_rec l idx =
    match l with
        | NilExpList -> ""
        | ExpList(exp, rest) -> "__buf" ^ (string_of_int idx) ^ " = " ^ (exp_to_c exp) ^ "; " ^ (impl_rec rest (idx + 1))
    in impl_rec rhs 0
;;

let assign_cs_to_c lhs =
    let rec impl_rec l idx =
    match l with
        | NilExpList -> ""
        | ExpList(exp, rest) -> (exp_to_c exp) ^ " = __buf" ^ (string_of_int idx) ^ "; " ^ (impl_rec rest (idx + 1))
    in impl_rec lhs 0
;;

let rec local_statement_to_c t lvl = match t with
	| LocalVarDecl (var_decl) -> (indent lvl) ^ (var_decl_to_c var_decl) ^ ";"
	| LocalVarDef (var_def) -> (indent lvl) ^ (var_def_to_c var_def) ^ ";"
	| LocalExp(local_exp) -> (indent lvl) ^ (exp_to_c local_exp) ^ ";"
	| For (var_def, cond, post_action, body) -> (indent lvl) ^ "for (" ^ (var_def_to_c var_def) ^ "; " ^
														(exp_to_c cond) ^ "; " ^ (exp_to_c post_action) ^ ") {\n" ^ (body_to_c body (lvl + 1)) ^ (indent lvl) ^ "}"
	| While (cond, body) -> (indent lvl) ^ "while (" ^ (exp_to_c cond) ^ ") {\n" ^ (body_to_c body (lvl+1)) ^ (indent lvl) ^ "}"
	| If (cond, body, if_cont) -> (indent lvl) ^ "if (" ^ (exp_to_c cond) ^ ") {\n" ^ (body_to_c body (lvl+1)) ^ (indent lvl) ^ "}" ^ (if_cont_to_c if_cont lvl)
	| Ret (exp) -> (indent lvl) ^ "return " ^ (exp_to_c exp) ^ ";"
	| RetEmpty -> (indent lvl) ^ "return;"
    | CSAssignment (lhs, rhs) -> (indent lvl) ^ (prepare_cs_to_c rhs) ^ "\n" ^ (indent lvl) ^ (assign_cs_to_c lhs)
and
if_cont_to_c t lvl = match t with
	| EpsIfCont -> ""
	| ElseIf (cond, body, if_cont) -> " else if (" ^ (exp_to_c cond) ^ ") {\n" ^ (body_to_c body (lvl + 1)) ^ (indent lvl) ^ "}" ^ (if_cont_to_c if_cont lvl)
	| Else (body) -> " else {\n" ^ (body_to_c body (lvl + 1)) ^ (indent lvl) ^ "}"
and
body_to_c t lvl = match t with
	| EpsBody -> ""
	| LocalStatementsList (local_statement, body) -> (local_statement_to_c local_statement lvl) ^ "\n" ^ (body_to_c body lvl)
;;

let rec global_statement_to_c = function
    | GlobalVarDecl (var_decl) -> (var_decl_to_c var_decl) ^ ";"
	| GlobalVarDef (var_def) -> (var_def_to_c var_def) ^ ";"
    | Function (name, arg_list, language_type, body) -> (language_type_to_c language_type) ^ " " ^ name ^ "(" ^ (arg_list_to_c arg_list) ^ ") {\n" ^
														(body_to_c body 1) ^ "}"
    | GlobalFuncCall (func_call) -> (func_call_to_c func_call) ^ ";"
;;

let rec collect_globals t (decls, defs, funs, fun_calls) = match t with
	| Eps -> (List.rev decls, List.rev defs, List.rev funs, List.rev fun_calls)
	| GlobalStatementsList (global_statement, tree) -> let s = (global_statement_to_c global_statement) in
													   match global_statement with
													   | GlobalVarDecl(x) -> collect_globals tree (s::decls, defs, funs, fun_calls)
													   | GlobalVarDef(x) -> collect_globals tree (decls, s::defs, funs, fun_calls)
													   | Function(a, b, c, d) -> collect_globals tree (decls, defs, s::funs, fun_calls)
													   | GlobalFuncCall(x) -> collect_globals tree (decls, defs, funs, s::fun_calls)
;;
