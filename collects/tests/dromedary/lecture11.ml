type num = Int of int | Float of float;;
let add r1 r2 =
match (r1,r2) with
(Int i1, Int i2) -> Int (i1 + i2)
| (Float r1, Int i2) -> Float (r1 +. float i2)
| (Int i1, Float r2) -> Float (float i1 +. r2)
| (Float r1, Float r2) -> Float (r1 +. r2);;
add (Int 3) (Float 4.5);;
(* num = Float 7.5 *)


let mult r1 r2 =
match (r1,r2) with
(Int i1, Int i2) -> Int (i1 * i2)
| (Float r1, Int i2) -> Float (r1 *. float i2)
| (Int i1, Float r2) -> Float (float i1 *. r2)
| (Float r1, Float r2) -> Float (r1 *. r2);;

let unaryMinus n =
match n with Int i -> Int (- i) | Float r -> Float (-. r);;
let minus n1 n2 = add n1 (unaryMinus n2);;
let squareRoot n =
match n with
Int i -> Float (sqrt (float i))
| Float r -> Float(sqrt r);;

let rec fact n =
match n with
Int 0 -> Int 1
| Float 0. -> Float 1.
| _ -> mult n (fact (minus n (Int 1)));;

fact (Int 6);;
(* num = Int 720 *)
fact (mult (Float 1.) (Int 6));;

type maybe = Absent | Present of int;;

let directory = [("Joe", 1234); ("Martha", 5672);
("Jane", 3456); ("Ed", 7623)];;
let rec lookup s l =
match l with
[] -> Absent
| (k,i)::t -> if k = s then Present i
else lookup s t;;
lookup "Jane" directory;;
(* maybe = Present 3456 *)
lookup "Karen" directory;;
(* maybe = Absent *)

type color = Red | Yellow | Green;;
let next c =
match c with Green -> Yellow | Yellow -> Red | Red -> Green;;

type day = Sunday | Monday | Tuesday | Wednesday
| Thursday | Friday | Saturday;;
let weekend d =
match d with
Saturday -> true
| Sunday -> true
| _ -> false;;

let roots a b c =
let v = b *. b -. 4.0 *. a *. c in
if v < 0.0 then None
else let s = sqrt v in
Some((-.b-.s)/.(2.0*.a), (-.b +.s)/.(2.0*.a))
(* val roots : float -> float -> float -> (float * float) option = <fun> *)

type tree = Leaf
| Node of (tree * int * tree);;

let n3 = Node(Leaf, 3, Leaf);;
let n8 = Node(Leaf, 8, Leaf);;
let n7 = Node(n3, 7, n8);;
let n9 = Node(n7, 9, Leaf);;
let n19 = Node(Leaf, 19, Leaf);;
let n17 = Node(Leaf, 17, n19);;
let n22 = Node(Leaf, 22, Leaf);;
let n20 = Node(n17, 20, n22);;
let n14 = Node(n9, 14, n20);;

let rec lookup x n =
match n with
Leaf -> false
| Node(l,v,r) ->
if x = v then true
else if x < v then lookup x l
else lookup x r;;
lookup 3 n14;;
(* bool = true *)
lookup 2 n14;;
(* bool = false *)

let rec insert x n =
match n with
Leaf -> Node(Leaf,x,Leaf)
| Node(l,v,r) ->
if x = v then
Node(l,v,r) (* already present *)
else if x < v then
Node(insert x l, v, r) (* put in left subtree *)
else
Node(l, v, insert x r) (* put in right subtree *);;
lookup 5 (insert 88 (insert 5 n14));;
(* bool = true *)

let rec treeToList t =
match t with
Leaf -> []
| Node(l, v, r) ->
treeToList l @ v::treeToList r;;
(* val treeToList : tree -> int list = <fun> *)
treeToList n14;;
(* int list = [3; 7; 8; 9; 14; 17; 19; 20; 22] *)

let rec maxVal n =
match n with
Node(_, v, Leaf) -> v
| Node(_, _, r) -> maxVal r;;
maxVal n14;;
(* int = 22 *)

let rec deleteMax n =
match n with
Node(l, v, Leaf) -> l
| Node(l, v, r) -> Node(l, v, deleteMax r);;

let isLeaf n = match n with Leaf -> true | _ -> false;;

let rec delete x n =
match n with
Leaf -> Leaf (* it isn't there !*)
| Node(l,v,r) ->
if x < v then Node(delete x l, v, r) (* first easy case *)
else if x > v then Node(l, v, delete x r) (* second easy case *)
else if isLeaf l then r (* v = x -- delete v*)
else if isLeaf r then l
else Node(deleteMax l, maxVal l, r);;

let rec findAndDeleteMax n =
match n with
Node(l, v, Leaf) -> (v, l)
| Node(l, v, r) ->
let (v', r') = findAndDeleteMax r in
(v', Node(l, v, r'));;

(* val findAndDeleteMax : tree -> int * tree = <fun> *)
findAndDeleteMax n9;;
(* int * tree = (9, Node (Node (Leaf, 3, Leaf), 7, Node (Leaf, 8, Leaf))) *)

let rec fastDelete x n =
match n with
Leaf -> Leaf
| Node(l,v,r) ->
if x < v then Node(fastDelete x l, v, r)
else if x > v then Node(l, v, fastDelete x r)
else if isLeaf l then r (* v = x -- delete v*)
else if isLeaf r then l
else
let (v', l') = findAndDeleteMax l in
Node(l', v', r);;
(* val fastDelete : int -> tree -> tree = <fun> *)
let rec listToTree l =
match l with
[] -> Leaf
| x::y -> insert x (listToTree y);;

listToTree [1; 2; 3; 4; 5; 6];;

type ('a,'b) tree =
Leaf
| Node of ('a,'b) tree * ('a * 'b) * ('a,'b) tree;;

let rec lookup k t =
match t with
Leaf -> None
| Node(l, (k',v), r) ->
if k < k' then lookup k l
else if k > k' then lookup k r
else Some v;;
(* val lookup : 'a -> ('a, 'b) tree -> 'b option = <fun> *)

let rec insert x t =
let (k, v) = x in
match t with
Leaf -> Node(Leaf, x, Leaf)
| Node(l, y, r) ->
let (k', v') = y in
if k < k' then
Node(insert x l, y, r)
else if k > k' then
Node(l, y, insert x r)
else Node(l, x, r);;
(* val insert : 'a * 'b -> ('a, 'b) tree -> ('a, 'b) tree = <fun> *)


let d = insert ("Alice", 1234)
(insert ("Clare", 7777)
(insert ("Bob", 6789) Leaf));;
(* val d : (string, int) tree =
Node (Node (Leaf, ("Alice", 1234), Leaf), ("Bob", 6789),
Node (Leaf, ("Clare", 7777), Leaf)) *)
lookup "Bob" d;;
(* int option = Some 6789 *)
lookup "Doug" d;;
(* int option = None *)
