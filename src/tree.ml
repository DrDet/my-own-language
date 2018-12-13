type type_basic = Int | Long | Float | Bool | String | Void;;
type type_array = Eps | TypeArray of string * type_array;;
type language_type =  Type of type_basic * type_array;;

type arg = Arg of string * language_type;;
type arg_list = Nil | ArgList of arg * arg_list;;

type func_call = FuncCall of string * exp_list
and
exp =
| BinaryOp of string * exp * exp
| UnaryOp of string * exp
| ExpFuncCall of func_call
| Str_name of string
| Num of string
| True
| False
and
exp_list = Nil | ExpList of exp * exp_list;;

type var_decl = VarDecl of string * language_type;;

type local_statement =
| LocalVarDecl of var_decl
| For of var_decl * exp * exp * exp * body
| While of exp * body
| If of exp * body * if_cont
| Ret of exp
| RetEmpty
and
body = Eps | LocalStatementsList of local_statement * body
and
if_cont = Eps | ElseIf of exp * body * if_cont | Else of body;;

type global_statement =
    GlobalVarDecl of var_decl
    | Function of string * arg_list * language_type * body
    | GlobFuncCall of func_call
;;

type tree = Eps | GlobalStatementsList of global_statement * tree;;
