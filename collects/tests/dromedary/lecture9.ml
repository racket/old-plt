let rec merge comp p =
match p with
l1, [] -> l1
| [], l2 -> l2
| (h1::t1), (h2::t2) ->
if comp h1 h2 then h1 :: merge comp (t1, h2::t2)
else h2::merge comp (h1::t1, t2);;
(* val merge : ('a -> 'a -> bool) -> 'a list * 'a list -> 'a list = <fun> *)
merge (<) ([1;4;7], [2;3;5]);;
(* int list = [1; 2; 3; 4; 5; 7] *)

let rec singlist l =
match l with
[] -> []
| h::t -> [h] :: singlist t;;
(* val singlist : 'a list -> 'a list list = <fun> *)
singlist [3; 2; 1; 4];;
(* int list list = [[3]; [2]; [1]; [4]] *)

let rec sweep comp l =
match l with
[] -> []
| [x] -> [x];
| l1::l2::t -> merge comp (l1,l2) :: sweep comp t;;
(* val sweep : ('a -> 'a -> bool) -> 'a list list -> 'a list list = <fun> *)
sweep (<) [[3]; [2]; [1]; [4]];;
(* int list list = [[2; 3]; [1; 4]] *)

let ll1 = singlist [1; 2; 3; 4; 6; 0; 9; 7; 8; 5; 6; 4];;
(* val ll1 : int list list = 
[[1]; [2]; [3]; [4]; [6]; [0]; [9]; [7]; [8]; [5]; [6]; [4]] *)
let ll2 = sweep (<) ll1;;
(* val ll2 : int list list =
[[1; 2]; [3; 4]; [0; 6]; [7; 9]; [5; 8]; [4; 6]] *)
let ll3 = sweep (<) ll2;;
(* val ll3 : int list list = [[1; 2; 3; 4]; [0; 6; 7; 9]; [4; 5; 6; 8]] *)
let ll4 = sweep (<) ll3;;
(* val ll4 : int list list = [[0; 1; 2; 3; 4; 6; 7; 9]; [4; 5; 6; 8]] *)
let ll5 = sweep (<) ll4;;
(* val ll5 : int list list = [[0; 1; 2; 3; 4; 4; 5; 6; 6; 7; 8; 9]] *)

let mergesort comp l =
let rec iterSweep ll =
match ll with
[] -> []
| [s] ->s
| ls -> iterSweep(sweep comp ls)
in iterSweep (singlist l);;
(* val mergesort : ('a -> 'a -> bool) -> 'a list -> 'a list = <fun> *)
mergesort (<) [3;2;1;4];;
(* int list = [1; 2; 3; 4] *)

let db = [
("Smith", "Carol", 43, 5551234); ("Upper", "Beth", 31, 5678900);
("Turner", "Drew", 22, 5524321); ("Smith", "Adam", 42, 5551234) ];;

let lastname ( x , _ , _ , _ ) = x;;
let firstname ( _ , x , _ , _ ) = x;;
let age ( _ , _ , x , _ ) = x;;
let phone ( _ , _ , _ , x ) = x;;
let complast p1 p2 = (lastname p1) <= (lastname p2);;
let compfirst p1 p2 = (firstname p1) <= (firstname p2);;
let compage p1 p2 = (age p1) <= (age p2);;

mergesort complast db;;
(* (string * string * int * int) list =
["Smith", "Carol", 43, 5551234; "Smith", "Adam", 42, 5551234;
"Turner", "Drew", 22, 5524321; "Upper", "Beth", 31, 5678900] *)
mergesort compfirst db;;
(* (string * string * int * int) list =
["Smith", "Adam", 42, 5551234; "Upper", "Beth", 31, 5678900;
"Smith", "Carol", 43, 5551234; "Turner", "Drew", 22, 5524321] *)
mergesort compage db;;
(* (string * string * int * int) list =
["Turner", "Drew", 22, 5524321; "Upper", "Beth", 31, 5678900;
"Smith", "Adam", 42, 5551234; "Smith", "Carol", 43, 5551234] *)

mergesort (function p1 -> function p2 -> lastname p1 <= lastname p2) db;;
(* (string * string * int * int) list =
[("Smith", "Carol", 43, 5551234); ("Smith", "Adam", 42, 5551234);
("Turner", "Drew", 22, 5524321); ("Upper", "Beth", 31, 5678900)] *)

mergesort (function p1 -> function p2 ->
lastname p1 < lastname p2 ||
lastname p1 = lastname p2 &&
firstname p1 < firstname p2) db;;
(* (string * string * int * int) list = 
[("Smith", "Adam", 42, 5551234); ("Smith", "Carol", 43, 5551234);
("Turner", "Drew", 22, 5524321); ("Upper", "Beth", 31, 5678900)] *)

let alphasort l =
mergesort complast (mergesort compfirst l);;
alphasort db;;
(* (string * string * int * int) list =
["Smith", "Adam", 42, 5551234; "Smith", "Carol", 43, 5551234;
"Turner", "Drew", 22, 5524321; "Upper", "Beth", 31, 5678900] *)

let db1 = [
("X", "A", 0, 0);
("X", "B", 0, 0);
("X", "C", 0, 0);
("X", "D", 0, 0);
("Y", "A", 0, 0);
("Y", "B", 0, 0);
("Y", "C", 0, 0);
("Y", "D", 0, 0) ];;
alphasort db1;;
(* (string * string * int * int) list =
["X", "A", 0, 0; "X", "C", 0, 0; "X", "B", 0, 0; "X", "D", 0, 0;
"Y", "A", 0, 0; "Y", "C", 0, 0; "Y", "B", 0, 0; "Y", "D", 0, 0] *)

let rec split l =
match l with
[] -> [], []
| [x] -> [x], []
| h1 :: h2 :: t ->
match split t with (l1, l2) -> h1::l1, h2 ::l2 ;;
split [1; 2; 3; 4; 5; 6];;
(* int list * int list = [1; 3; 5], [2; 4; 6] *)
split db1;;
(* ["X", "A", 0, 0; "X", "C", 0, 0; "Y", "A", 0, 0; "Y", "C", 0, 0],
["X", "B", 0, 0; "X", "D", 0, 0; "Y", "B", 0, 0; "Y", "D", 0, 0] *)

let rec prefix n l =
if (n > 0) then List.hd l :: (prefix (n - 1) (List.tl l))
else [];;
let rec suffix n l = 
if ((List.length l) > n) then (suffix n (List.tl l))
else l;;

let rec split l =
let n = (List.length l) / 2 in
(prefix n l, suffix n l);;
split [1; 2; 3; 4; 5; 6];;
(* int list * int list = [1; 2; 3], [4; 5; 6] *)

alphasort db1;;
(* (string * string * int * int) list =
["X", "A", 0, 0; "X", "B", 0, 0; "X", "C", 0, 0; "X", "D", 0, 0;
"Y", "A", 0, 0; "Y", "B", 0, 0; "Y", "C", 0, 0; "Y", "D", 0, 0] *)
