type type_basic = Int | Long | Float | Bool | String | Void;;
type type_array = EpsTArr | TypeArray of string * type_array;;
type language_type = Type of type_basic * type_array;;

type arg = Arg of string * language_type;;
type arg_list = NilArgList | ArgList of arg * arg_list;;

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
| ArrayAccess of string * type_array_access
| StrLiteral of string
and
exp_list = NilExpList | ExpList of exp * exp_list
and
type_array_access = EpsTArrAccess | TypeArrayAccess of exp * type_array_access
;;

type var_decl = VarDecl of string * language_type;;
type var_def = VarDef of string * language_type * exp;;

type local_statement =
| LocalVarDecl of var_decl
| LocalVarDef of var_def
| LocalExp of exp
| For of var_def * exp * exp * body
| While of exp * body
| If of exp * body * if_cont
| Ret of exp
| RetEmpty
| CSAssignment of exp_list * exp_list
and
body = EpsBody | LocalStatementsList of local_statement * body
and
if_cont = EpsIfCont | ElseIf of exp * body * if_cont | Else of body;;

type global_statement =
    GlobalVarDecl of var_decl
    | GlobalVarDef of var_def
    | Function of string * arg_list * language_type * body
    | GlobalFuncCall of func_call
;;

type tree = Eps | GlobalStatementsList of global_statement * tree;;
