1 :: 2 :: 3 :: 2 :: 1 :: [] ;;(* int list = [1; 2; 3; 2; 1] *)
let rec fromTo (m:int) (n:int) = (* The numbers from m to n *)
if n < m then []
else m :: fromTo (m+1) n;;
fromTo 9 18;;
(* int list = [9; 10; 11; 12; 13; 14; 15; 16; 17; 18] *)
List.hd [1; 2; 3];;
(* int = 1 *)
List.tl [1; 2; 3];;
(* int list = [2; 3] *)
List.tl (List.tl [1; 2; 3]);;
(* int list = [3] *)
List.tl (List.tl (List.tl [1; 2; 3]));;
(* int list = [] *)
List.hd (List.tl (List.tl [1; 2; 3]));;
(* int = 3 *)
List.hd [[5; 4]; [3; 2]];;
(* int list = [5; 4] *)
List.hd (List.hd [[5; 4]; [3; 2]]);;
(* int = 5 *)
List.tl (List.hd [[5; 4]; [3; 2]]);;
(* int list = [4] *)
let hd (x:int) = x * 10 + 3;;
(* val hd : int -> int = <fun> *)
hd 5;;
(* int = 53 *)
List.hd [1; 2; 3];;
(* int = 1 *)
let rec listSum (l:int list) =
if l = [] then 0
else List.hd l + listSum (List.tl l);;
(* val listSum : int -> int -> int = <fun> *)
listSum [5; 4; 3; 2; 1];;
(* int = 15 *)
let rec length (l: int list) =
if l = [] then 0
else 1 + length (List.tl l);;
let rec listProd (l: int list) =
if l = [] then 1
else List.hd l * listProd (List.tl l);;
let fact (n:int) = listProd (fromTo 1 n);;
fact 6;;
(* int = 720 *)
let rec append (l1: int list) (l2: int list) =
if l1 = [] then l2
else List.hd l1 :: append (List.tl l1) l2;;
(* val append : int list -> int list -> int list = <fun> *)
append [4; 3; 2] [6; 6; 7];;
(* int list = [4; 3; 2; 6; 6; 7] *)
append [] [6; 6; 7];;
(* int list = [6; 6; 7] *)
append [4; 3; 2] [];;
(* int list = [4; 3; 2] *)
let rec snoc (l: int list) (x: int) = (* put x on the end of l *)
if l = [] then x::[]
else List.hd l :: snoc(List.tl l) x;;
(* val snoc : int list -> int -> int list = <fun> *)
snoc [5; 4; 3; 2] 1;;
(* int list = [5; 4; 3; 2; 1] *)
let rec nrev (l: int list) = (* Reverses l -- inefficiently *)
if l = [] then []
else snoc (nrev (List.tl l)) (List.hd l);;
(* val nrev : int list -> int list = <fun> *)
nrev [1; 2; 3];;
(* int list = [3; 2; 1] *)
let rec revaux (l: int list) (res: int list) =
if l = [] then res
else revaux (List.tl l) (List.hd l :: res);;
(* val revaux : int list -> int list -> int list = <fun> *)
revaux [1; 2; 3] [4; 5; 6];;
(* int list = [3; 2; 1; 4; 5; 6] *)
let rev (l: int list) = revaux l [];;
(* val rev : int list -> int list = <fun> *)
let rec flatten (l: int list list) =
if l = [] then []
else append (List.hd l) (flatten (List.tl l));;
(* val flatten : int list list -> int list = <fun> *)
flatten [[5; 6]; []; [1; 2]; []];;
(* int list = [5; 6; 1; 2] *)
let rec insertOne (item:int) (left:int list) (right:int list) =
if right = [] then [snoc left item]
else append (snoc left item) right ::
insertOne item (snoc left (List.hd right)) (List.tl right);;
(* val insertOne : int -> int list -> int list -> int list list = <fun> *)
insertOne 1 [] [2;3];;
(* int list list = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]] *)
let rec insert (item:int) (lists:int list list) =
if lists = [] then []
else
List.append (insertOne item [] (List.hd lists))
(insert item (List.tl lists));;
(* val insert : int -> int list list -> int list list = <fun> *)
let rec permute (list:int list) =
if list = [] then [[]]
else insert (List.hd list) (permute (List.tl list));;
(* val permute : int list -> int list list = <fun> *)
permute [1;2;3];;
(* int list list =
[[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] *)
