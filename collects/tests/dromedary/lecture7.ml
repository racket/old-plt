let rec sum l =
match l with
[] -> 0
| h::t -> h + sum t;;
(* val sum : int list -> int = <fun> *)

let rotate l =
match l with h :: t -> t @ [h] ;;
(* Warning: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val rotate : 'a list -> 'a list = <fun> *)

let rotate l =
match l with
[] -> []
| h :: t -> t @ [h] ;;
(* val rotate : 'a list -> 'a list = <fun> *)

let rec repeat l =
match l with
[] -> []
| h::t -> h :: h :: repeat t;;
repeat [1; 2; 3];;
(* int list = [1; 1; 2; 2; 3; 3] *)

let rec reverse l =
match l with
[] -> []
| h :: t -> reverse t @ [h];;
let rec sum l =
match l with
[] -> 0
| h :: t -> h + sum t;;

let rec count l =
match l with
[] -> 0
| _ :: t -> 1 + count t;;

let rec last l =
match l with
[x] -> x
| y -> last (List.tl y);;

exception DoesNotExist;;

let rec last l =
match l with
[] -> raise DoesNotExist (* generate informative error *)
| [x] -> x
| _ :: t -> last t;;

let rec sorted l =
match l with
[] -> true
| [_] -> true
| h1 :: h2 :: t -> h1 <= h2 && sorted (h2 :: t);;

1, 2, 3 ;;
(* int * int * int = 1, 2, 3 *)
2, "cat";;
(* int * string = 2, "cat" *)
1, ("cat",true), "dog";;
(* int * (string * bool) * string = 1, ("cat", true), "dog" *)

let p = (2, "bats") ;;
(* val p : int * string = 2, "bats" *)
match p with (x, _) -> x;;
(* int = 2 *)
match p with (_ , y) -> y;;
(* string = "bats" *)
let order (x:int) (y:int) = if x < y then x,y else y,x;;
(* val order : int -> int -> int * int = <fun> *)
order 2 1;;
(* int * int = (1, 2) *)

let rec split (x: int) (l: int list) =
match l with
[] -> ([], [])
| y :: t -> match split x t with (l, h) ->
if y < x then (y::l,h) else (l, y::h);;
(* val split : int -> int list -> int list * int list = <fun> *)
split 3 [1;2;4];;
(* int list * int list = ([1; 2], [4]) *)
