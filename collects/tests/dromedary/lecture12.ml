let x = 5;;
let square (n:int) = n*n;;
let rec fact (n:int) =
if n=0 then 0 else n*fact (n-1);;
let hyp x y =
let sq u = u *. u
in sqrt (sq x +. sq y);;
type d = A of int | B of int*bool;;

5;; "five";; true;; 0.25;;

let rec maxsum l =
match l with
[] -> (0, 0)
| h::t ->
let (m, s) = maxsum t in
((if h > m then h else m), h + s);;
(* val maxsum : int list -> int * int = <fun> *)
maxsum [1;2;3];;
(* - : int * int = (3, 6) *)

let rec dmap f l =
if l = [] then []
else f (List.hd l) :: dmap f (List.tl l);;
(* val dmap : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

List.map ((+) 1) [1;2;3];;
(* - : int list = [2; 3; 4] *)
List.map (List.map ((+) 1)) [[1; 2]; [5]; [4; 6]];;
(* - : int list list = [[2; 3]; [6]; [5; 7]] *)

let consAll x ll =
List.map (function l -> x::l) ll;;
(* val consAll : 'a -> 'a list list -> 'a list list = <fun> *)
consAll 1 [[2; 3]; [4]; [5; 6]];;
(* - : int list list = [[1; 2; 3]; [1; 4]; [1; 5; 6]] *)

(* Define a datatype d, with constructors A and B *)
type d = A of int | B of int*bool;;
A(3);;
(* - : d = A 3 *)
B(7,true);;
(* - : d = B (7, true) *)

let v = A(9);;
(* val v : d = A 9 *)
let w = B(7, true);;
(* val w : d = B (7, true) *)
match v with
A(i) -> i
| B(i,b) -> i + (if b then 1 else 3);;
(* - : int = 9 *)
match w with
A(i) -> i
| B(i,b) -> i + (if b then 1 else 3);;
(* - : int = 8 *)
