let rec insert x l =
match l with
[] -> [x]
| h::t -> if x <= h then x :: h :: t
else h :: insert x t;;
(* val insert : 'a -> 'a list -> 'a list = <fun> *)
insert 4 [1; 2; 5; 6];;
(* int list = [1; 2; 4; 5; 6] *)

let rec sort l =
match l with
[] -> []
| h::t -> insert h (sort t);;
(* val sort : 'a list -> 'a list = <fun> *)
sort [1; 9; 2; 8; 3; 7; 1; 6];;
(* int list = [1; 1; 2; 3; 6; 7; 8; 9] *)

let rec lessLst x l =
match l with
[] -> []
| h::t -> if h < x then h :: lessLst x t
else lessLst x t;;

let rec greaterLst x l = 
match l with
[] -> []
| h::t -> if h > x then h :: greaterLst x t
else greaterLst x t;;

let rec eqLst x l =
match l with
[] -> []
| h::t -> if h = x then h :: eqLst x t
else eqLst x t;;



let rec quickSort l =
match l with
[] -> []
| [x] -> [x]
| h::t -> quickSort (lessLst h t)
@ h::eqLst h t
@ quickSort (greaterLst h t) ;;

let rec split3 x l =
match l with
[] -> [], [], []
| h::t -> match split3 x t with (s, e, g) ->
if h < x then (h::s, e,g)
else if h = x then s, h::e, g
else s, e, h::g ;;
(* val split3 : 'a -> 'a list -> 'a list * 'a list * 'a list = <fun> *)

split3 5 [1; 9; 2; 7; 3; 8; 2; 5; 6];;
(* int list * int list * int list = [1; 2; 3; 2], [5], [9; 7; 8; 6] *)

let rec quickSort l =
match l with
[] -> []
| [x] -> [x]
| h::t ->
match split3 h t with (l, e, g) ->
quickSort l @ h::e @ quickSort g ;;

let rec merge p =
match p with
l1, [] -> l1
| [], l2 -> l2
| (h1::t1), (h2::t2) ->
if h1 <= h2 then h1 :: merge (t1, h2::t2)
else h2::merge (h1::t1, t2) ;;
(* val merge : 'a list * 'a list -> 'a list = <fun> *)

merge (["cat"; "dog"], ["bat"; "cow"; "emu"]);;
(* string list = ["bat"; "cat"; "cow"; "dog"; "emu"] *)

let rec split l =
match l with
[] -> [], []
| [x] -> [x], []
| h1 :: h2 :: t ->
match split t with (l1, l2) -> h1::l1, h2 ::l2 ;;
(* val split : 'a list -> 'a list * 'a list = <fun> *)
split [1; 2; 3; 4; 5; 6];;
(* int list * int list = [1; 3; 5], [2; 4; 6] *)

let rec mergesort l =
match l with
[] -> []
| [x] -> [x]
| l -> match split l with (l1, l2) ->
merge(mergesort l1, mergesort l2);;
mergesort [1; 2; 3; 4; 6; 0; 9; 7; 8; 5; 6; 4];;
(* int list = [0; 1; 2; 3; 4; 4; 5; 6; 6; 7; 8; 9] *)

let rec merge comp p =
match p with
l1, [] -> l1
| [], l2 -> l2
| (h1::t1), (h2::t2) ->
if comp h1 h2 then h1 :: merge comp (t1, h2::t2)
else h2::merge comp (h1::t1, t2);;
let rec mergesort comp l =
match l with
[] -> []
| [x] -> [x]
| l -> match split l with (l1, l2) ->
merge comp (mergesort comp l1, mergesort comp l2);;
(* val mergesort : ('a -> 'a -> bool) -> 'a list -> 'a list = <fun> *)

mergesort ( <= ) [1; 9; 3; 8; 4; 7; 3] ;;
(* int list = [1; 3; 3; 4; 7; 8; 9] *)
mergesort ( >= ) [1; 9; 3; 8; 4; 7; 3] ;;
(* int list = [9; 8; 7; 4; 3; 3; 1] *)
mergesort ( <= ) ["Jack"; "havoc"; "Newt"; "Alan"; "cat"];;
(* string list = ["Alan"; "Jack"; "Newt"; "cat"; "havoc"] *)
let strComp s1 s2 = String.uppercase s1 <= String.uppercase s2;;
mergesort strComp ["Jack"; "havoc"; "Newt"; "Alan"; "cat"] ;;
(* string list = ["Alan"; "cat"; "havoc"; "Jack"; "Newt"] *)
