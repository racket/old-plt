"hello";; (* string = "hello" *)
"hello" ^ "there";; (* string = "hellothere" *)
"hello" ^ "      " ^ "there";; (* string = "hello      there" *)

let oneMove (s1:string) (s2:string) =
s1 ^ "->" ^ s2 ^ "; ";;
(* val oneMove : string -> string -> string = <fun> *)
oneMove "A" "B";;
(* string = "A->B; " *)

let rec toh (numdisks:int)
(fromP:string) (toP:string) (spareP:string) =
if numdisks = 0 then ""
else (toh (numdisks-1) fromP spareP toP) (* (2) *)
^ (oneMove fromP toP) (* (3) *)
^ (toh (numdisks-1) spareP toP fromP) (* (4) *);;
(* val toh : int -> string -> string -> string -> string = <fun> *)

toh 4 "A" "B" "C";;
(* string =
"A->C; A->B; C->B; A->C; B->A; B->C; A->C; A->B;
C->B; C->A; B->A; C->B; A->C; A->B; C->B; " *)

[1; 3; 2; 5];;
(* int list = [1; 3; 2; 5] *)

["cat"; "dog"; "gnu"];;
(* string list = ["cat"; "dog"; "gnu"] *)
[true; true; false];;
(* bool list = [true; true; false] *)
[[1; 2]; [2; 3; 4]; [5]];;
(* int list list = [[1; 2]; [2; 3; 4]; [5]] *)

(* [1; 2; "dog"];; This expression has type string but is here used with type int *)

1 :: [2; 3];;
(* int list = [1; 2; 3] *)
let add123 (l: int list) = 1 :: 2 :: 3 :: l;;
(* val add123 : int list -> int list = <fun> *)
add123 [5; 6; 7];;
(* int list = [1; 2; 3; 5; 6; 7] *)
add123 [];;
(* int list = [1; 2; 3] *)

let rec repeat (k:int) (n:int) = (* A list of n copies of k *)
if n = 0 then []
else k :: repeat k (n-1);;
(* val repeat : int -> int -> int list = <fun> *)
repeat 7 12;;
(* int list = [7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7] *)
let rec fromTo (m:int) (n:int) = (* The numbers from m to n *)
if n < m then []
else m :: fromTo (m+1) n;;
(* val fromTo : int -> int -> int list = <fun> *)
fromTo 9 18;;
(* int list = [9; 10; 11; 12; 13; 14; 15; 16; 17; 18] *)
