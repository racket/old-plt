let rec last (l: int list) =
if List.tl l = [] then List.hd l
else last (List.tl l);;
(* val last : int list -> int = <fun> *)
last [1; 4; 5; 2; 7];;
(* int = 7 *)
let rec last (l: string list) =
if List.tl l = [] then List.hd l
else last (List.tl l);;
(* val last : string list -> string = <fun> *)
last ["The"; "fat"; "cat"];;
(* string = "cat" *)
let rec last (l: bool list) =
if List.tl l = [] then List.hd l
else last (List.tl l);;
(* val last : string list -> string = <fun> *)
last [true; true; false];;
(* bool = false *)
let rec last (l: 'a list) =
if List.tl l = [] then List.hd l
else last (List.tl l);;
(* val last : 'a list -> 'a = <fun> *)
last [1; 4; 5; 2; 7];;
(* int = 7 *)
last ["The"; "fat"; "cat"];;
(* string = "cat" *)
last [true; true; false];;
(* bool = false *)
let rec append (l1: 'a list) (l2: 'a list) =
if l1 = [] then l2
else List.hd l1 :: append (List.tl l1) l2;;
(* val append : 'a list -> 'a list -> 'a list = <fun> *)
append [4; 3; 2] [6; 6; 7];;
(* int list = [4; 3; 2; 6; 6; 7] *)
append ["cat"; "in"] ["the"; "hat"];;
(* string list = ["cat"; "in"; "the"; "hat"] *)
let rec revaux (l: 'a list) (res: 'a list) =
if l = [] then res
else revaux (List.tl l) (List.hd l :: res);;
(* val revaux : 'a list -> 'a list -> 'a list = <fun> *)
let rev (l: 'a list) = revaux l [];;
(* val rev : 'a list -> 'a list = <fun> *)
rev ["cat"; "in"; "the"; "hat"];;
(* string list = ["hat"; "the"; "in"; "cat"] *)
rev [false; true];;
(* bool list = [true; false] *)
let rec repeat (k:'a) (n:int) = (* A list of n copies of k *)
if n = 0 then []
else k :: repeat k (n-1);;
(* val repeat : 'a -> int -> 'a list = <fun> *)
repeat 7 12;;
(* int list = [7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7] *)
repeat true 3;;
(* bool list = [true; true; true] *)
repeat [6;7] 4;;
(* int list list = [[6; 7]; [6; 7]; [6; 7]; [6; 7]] *)
let palindrome (l: 'a list) =
l = (rev l);;
(* val palindrome : 'a list -> bool = <fun> *)
palindrome [1; 2; 4; 2; 1];;
(* bool = true *)
palindrome [true; true; false];;
(*  bool = false *)
palindrome ["a";"b";"l";"e"; "w";"a";"s"; "I"; "e";"r";"e"; "I";
"s";"a";"w"; "e";"l";"b";"a"];;
(* bool = true *)
let square x = x * x;;
List.map square [1; 3; 5; 9; 2; 21];;
(* int list = [1; 9; 25; 81; 4; 441] *)
List.map not [false; false; true];;
(* bool list = [true; true; false] *)
let even (n:int) = n mod 2 = 0;;
(* val even : int -> bool = <fun> *)
List.filter even [1; 2; 3; 4; 5; 6; 7; 8; 9];;
(* int list = [2; 4; 6; 8] *)
List.filter palindrome [[1]; [1; 2; 3]; [1; 2; 1]; []];;
(* int list list = [[1]; [1; 2; 1]; []] *)
let rec map (f: 'a->'b) (l: 'a list) =
if l = [] then []
else f (List.hd l) :: map f (List.tl l);;
(* val map : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
map String.length ["The"; "quick"; "brown"; "fox"];;
(* int list = [3; 5; 5; 3] *)
let rec filter (p: 'a->bool) (l: 'a list) =
if l = [] then []
else if p (List.hd l) then List.hd l :: filter p (List.tl l)
else filter p (List.tl l)
(* val filter : ('a -> bool) -> 'a list -> 'a list = <fun> *)
