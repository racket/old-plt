let plus = ( + );;
(* val plus : int -> int -> int = <fun> *)
plus 2 3;;
(* int = 5 *)
let times = ( * );;
(* val times : int -> int -> int = <fun> *)
times 2 3;;
(* int = 6 *)
let rec sum (l: int list) =
if l = [] then 0
else List.hd l + sum (List.tl l);;
(* val sum : int list -> int = <fun> *)
sum [1;2;3];;
(* int = 6 *)
let rec prod (l: int list) =
if l = [] then 1
else List.hd l * prod (List.tl l);;
(* val prod : int list -> int = <fun> *)
prod [2;3;4];;
(* int = 24 *)
let rec paste (l :string list) =
if l = [] then ""
else List.hd l ^ paste (List.tl l);;
(* val paste : string list -> string = <fun> *)
paste ["Abstraction"; " "; "rules!"];;
(* string = "Abstraction rules!" *)
let rec concat (l: 'a list list) =
if l = [] then []
else List.append (List.hd l) (concat (List.tl l));;
(* val concat : 'a list list -> 'a list = <fun> *)
concat [[1;2];[3;4]];;
(*  int list = [1; 2; 3; 4] *)
let rec fold_right (f: 'a -> 'b -> 'b) (l: 'a list) (b: 'b) =
if l = [] then b
else f (List.hd l) (fold_right f (List.tl l) b);;
(* val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun> *)
fold_right (+) [1;2;3] 0;;
(* int = 6 *)
fold_right ( * ) [2;3;4] 1;;
(* int = 24 *)
fold_right (^) ["Abstraction"; " "; "rules!"] "";;
(* string = "Abstraction rules!" *)
fold_right List.append [[1;2];[3;4]] [];;
(* int list = [1; 2; 3; 4] *)
let addlength (s: string) (b: int) = String.length s + b;;
(* val addlength : string -> int -> int = <fun> *)
fold_right addlength ["Abstraction"; " "; "rules!"] 0;;
(* int = 18 *)
fold_right (+) (List.map String.length ["Abstraction"; " "; "rules!"]) 0;;
(* int = 18 *)
let twice (f: 'a->'a) (x:'a) = f (f x);;
(* val twice : ('a -> 'a) -> 'a -> 'a = <fun> *)
let double (x:int) = 2*x;;
(* val double : int -> int = <fun> *)
twice double 3;;
(* int = 12 *)
let quadruple = twice double;;
(* val quadruple : int -> int = <fun> *)
quadruple 3;;
(* int = 12 *)
let add (x:int) (y:int) = x + y;;
(* val add : int -> int -> int = <fun> *)
let add1 = add 1;;
(* val add1 : int -> int = <fun> *)
add1 3;;
(* int = 4 *)
List.map (( * ) 2) [1;2;3];;
(* int list = [2; 4; 6] *)
let funcons (f: 'a -> 'b) (x: 'a) (l: 'b list) = f x :: l;;
(* val funcons : ('a -> 'b) -> 'a -> 'b list -> 'b list = <fun> *)
let mymap (f: 'a -> 'b) (l: 'a list) = fold_right (funcons f) l [];;
(* val mymap : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
mymap (( * ) 2) [1;2;3];;
(* int list = [2; 4; 6] *)
twice (List.map (( * ) 2)) [1;2;3];;
(* int list = [4; 8; 12] *)
List.map (twice (( * ) 2)) [1;2;3];;
(* int list = [4; 8; 12] *)
List.map (function (x:int) -> x*x) [1;2;3];;
(* int list = [1; 4; 9] *)
let mymap (f: 'a -> 'b) (l: 'a list) =
fold_right (function (x:'a) -> function (l:'b list) -> f x :: l) l [];;
(* val mymap : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
mymap (function (x:int) -> x*x) [1;2;3];;
(* int list = [1; 4; 9] *)
let compose (f: 'a -> 'b) (g: 'c -> 'a) = function (x: 'c) -> f (g x);;
(* val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)
let totalLength =
compose (function l -> fold_right (+) l 0) (List.map String.length);;
(* val totalLength : string list -> int = <fun> *)
totalLength ["Abstraction"; " "; "rules!"];;
(* int = 18 *)
let filter p l = fold_right
(function x -> function r -> if p x then x::r else r) l [];;
(* val filter : ('a -> bool) -> 'a list -> 'a list = <fun> *)
let longwords = filter (function w -> String.length w > 6);;
(* val longwords : string list -> string list = <fun> *)
longwords ["abstraction"; "is"; "mind"; "boggling"];;
(* string list = ["abstraction"; "boggling"] *)
