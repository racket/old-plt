type exp =
IntConst of int
| BoolConst of bool
| OpExp of exp * string * exp;;

let test1 =
OpExp(OpExp(IntConst(2), "+", IntConst(3)),
"<",
OpExp(IntConst(4),
"*",
OpExp(IntConst(5), "-", IntConst(6))));;

OpExp(IntConst(1),
"+",
OpExp(OpExp(OpExp(IntConst(2),"-",IntConst(3)),
"*",
IntConst(4)),
"+",
IntConst(5)));;

type value =
IntVal of int
| BoolVal of bool;;

let rec eval e =
match e with
IntConst(i) -> IntVal(i)
| BoolConst(b) -> BoolVal(b)
| OpExp(e1,opname,e2) ->
let v1 = eval e1 in
let v2 = eval e2 in
(match (opname,v1,v2) with
("+",IntVal(i1),IntVal(i2)) -> IntVal(i1+i2)
| ("-",IntVal(i1),IntVal(i2)) -> IntVal(i1-i2)
| ("*",IntVal(i1),IntVal(i2)) -> IntVal(i1*i2)
| ("<",IntVal(i1),IntVal(i2)) -> BoolVal(i1<i2));;

eval (OpExp(IntConst(77),"+",IntConst(88)));;
(* - : value = IntVal 165 *)

eval (IntConst(77));;
(* - : value = IntVal 77 *)
eval test1;;
(* - : value = BoolVal false *)

exception LastEx;;
let rec last l =
match l with
[] -> raise LastEx
| [x] -> x
| _ :: t -> last t;;
last [];;
(* Uncaught exception: LastEx. *)

exception FactEx of int;;
let rec fact n =
if n = 0 then 1
else if n > 0 then n * fact (n-1)
else raise (FactEx n);;
fact (17 - 64);;

exception EvaluationError;;
let rec eval e =
match e with
IntConst(i) -> IntVal(i)
| BoolConst(b) -> BoolVal(b)
| OpExp(e1,opname,e2) ->
let v1 = eval e1 in
let v2 = eval e2 in
(match (opname,v1,v2) with
("+",IntVal(i1),IntVal(i2)) -> IntVal(i1+i2)
| ("-",IntVal(i1),IntVal(i2)) -> IntVal(i1-i2)
| ("*",IntVal(i1),IntVal(i2)) -> IntVal(i1*i2)
| ("<",IntVal(i1),IntVal(i2)) -> BoolVal(i1<i2)
| _ -> raise EvaluationError);;

let test1 =
OpExp(OpExp(IntConst(2), "+", IntConst(3)),
"<",
OpExp(IntConst(4),
"*",
OpExp(IntConst(5), "-", IntConst(6))));;

type exp =
IntConst of int
| BoolConst of bool
| OpExp of exp * string * exp
| Var of string;;

OpExp(Var("x"), "+", IntConst(2));;

type binding = ValBinding of string * value;;
let env_xy = [ValBinding("y", IntVal(7));
ValBinding("x", IntVal(4))];;

exception UnboundVariable;;




let rec lookup env x =
match env with
[] -> raise UnboundVariable
| ValBinding(y,v) :: rest ->
if x=y then v
else lookup rest x;;

let addbinding b env = b :: env;;

let rec eval env e =
match e with
IntConst(i) -> IntVal(i)
| BoolConst(b) -> BoolVal(b)
| OpExp(e1,opname,e2) ->
let v1 = eval env e1 in
let v2 = eval env e2 in
(match (opname,v1,v2) with
("+",IntVal(i1),IntVal(i2)) -> IntVal(i1+i2)
| ("-",IntVal(i1),IntVal(i2)) -> IntVal(i1-i2)
| ("*",IntVal(i1),IntVal(i2)) -> IntVal(i1*i2)
| ("<",IntVal(i1),IntVal(i2)) -> BoolVal(i1<i2)
| _ -> raise EvaluationError)
| Var(x) -> lookup env x;;
eval env_xy (OpExp(Var("x"), "+", IntConst(2)));;
(* - : value = IntVal 6 *)

type exp =
IntConst of int
| BoolConst of bool
| OpExp of exp * string * exp
| Var of string
| LetValExp of string * exp * exp;;

let a_let_exp =
LetValExp("x",
IntConst(99),
OpExp(Var("x"), "+", IntConst(2)));;

let rec eval env e =
match e with
IntConst(i) -> IntVal(i)
| BoolConst(b) -> BoolVal(b)
| OpExp(e1,opname,e2) ->
let v1 = eval env e1 in
let v2 = eval env e2 in
(match (opname,v1,v2) with
("+",IntVal(i1),IntVal(i2)) -> IntVal(i1+i2)
| ("-",IntVal(i1),IntVal(i2)) -> IntVal(i1-i2)
| ("*",IntVal(i1),IntVal(i2)) -> IntVal(i1*i2)
| ("<",IntVal(i1),IntVal(i2)) -> BoolVal(i1<i2)
| _ -> raise EvaluationError)
| Var(x) -> lookup env x
| LetValExp(x,e1,e2) ->
let v1 = eval env e1 in
let newenv = addbinding (ValBinding(x,v1)) env in
eval newenv e2;;

eval env_xy a_let_exp;;
(* - : value = IntVal 101 *)

type exp =
IntConst of int
| BoolConst of bool
| OpExp of exp * string * exp
| Var of string
| LetValExp of string * exp * exp
| IfExp of exp * exp * exp
| LetRecFunExp of string * string * exp * exp
| AppExp of string * exp;;

let factprog =
LetRecFunExp("f", "a",
IfExp(OpExp(Var("a"), "<", IntConst(1)),
IntConst(1),
OpExp(Var("a"),
"*",
AppExp("f",
OpExp(Var("a"), "-", IntConst(1))))),
AppExp("f", IntConst(5)));;

type binding =
ValBinding of string * value
| RecFunBinding of string * string * exp;;

let rec eval env e =
match e with
IntConst(i) -> IntVal(i)
| BoolConst(b) -> BoolVal(b)
| OpExp(e1,opname,e2) -> 
let v1 = eval env e1 in
let v2 = eval env e2 in
(match (opname,v1,v2) with
("+",IntVal(i1),IntVal(i2)) -> IntVal(i1+i2)
| ("-",IntVal(i1),IntVal(i2)) -> IntVal(i1-i2)
| ("*",IntVal(i1),IntVal(i2)) -> IntVal(i1*i2)
| ("<",IntVal(i1),IntVal(i2)) -> BoolVal(i1<i2)
| _ -> raise EvaluationError)
| Var(x) -> lookup env x
| IfExp(e1,e2,e3) ->
(match eval env e1 with BoolVal(true) -> eval env e2
| BoolVal(false) -> eval env e3
| _ -> raise EvaluationError)
| LetValExp(x,e1,e2) ->
let v1 = eval env e1 in
let newenv = addbinding (ValBinding(x, v1)) env in
eval newenv e2
| LetRecFunExp(f,a,e1,e2) ->
let newenv = addbinding (RecFunBinding (f, a, e1)) env in
eval newenv e2;;

type value =
IntVal of int
| BoolVal of bool
| FunVal of string * exp * binding list;;

let rec lookup env x =
match env with
[] -> raise UnboundVariable
| ValBinding(y,v) :: rest ->
if x=y then v
else lookup rest x
| RecFunBinding(f,a,e1) :: rest ->
if x=f then FunVal(a,e1,env)
else lookup rest x;;

type value =
IntVal of int
| BoolVal of bool
| FunVal of string * exp * binding list
and binding =
ValBinding of string * value
| RecFunBinding of string * string * exp;;

let rec eval env e =
match e with
IntConst(i) -> IntVal(i)
| BoolConst(b) -> BoolVal(b)
| OpExp(e1,opname,e2) -> 
let v1 = eval env e1 in
let v2 = eval env e2 in
(match (opname,v1,v2) with
("+",IntVal(i1),IntVal(i2)) -> IntVal(i1+i2)
| ("-",IntVal(i1),IntVal(i2)) -> IntVal(i1-i2)
| ("*",IntVal(i1),IntVal(i2)) -> IntVal(i1*i2)
| ("<",IntVal(i1),IntVal(i2)) -> BoolVal(i1<i2)
| _ -> raise EvaluationError)
| Var(x) -> lookup env x
| IfExp(e1,e2,e3) ->
(match eval env e1 with BoolVal(true) -> eval env e2
| BoolVal(false) -> eval env e3
| _ -> raise EvaluationError)
| LetValExp(x,e1,e2) ->
let v1 = eval env e1 in
let newenv = addbinding (ValBinding(x, v1)) env in
eval newenv e2
| LetRecFunExp(f,a,e1,e2) ->
let newenv = addbinding (RecFunBinding (f, a, e1)) env in
eval newenv e2
| AppExp(f,e) ->
let v = eval env e in
(match lookup env f with
FunVal(x,e1,defenv) ->
let newenv = addbinding (ValBinding (x, v)) defenv in
eval newenv e1
| _ -> raise EvaluationError);;

let funprog =
LetRecFunExp("f", "x",
OpExp(Var("x"), "+", Var("x")),
AppExp("f", IntConst(5)));;

eval [] funprog;;
(* - : value = IntVal 10 *)


let factprog =
LetRecFunExp("f", "x",
IfExp(OpExp(Var("x"), "<", IntConst(1)),
IntConst(1),
OpExp(Var("x"),
"*",
AppExp("f",
OpExp(Var("x"), "-", IntConst(1))))),
AppExp("f", IntConst(5)));;

eval [] factprog;;
(* - : value = IntVal 120 *)

