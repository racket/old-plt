let x = ref 1;;
(* val x : int ref = {contents = 1} *)
!x;;
(* - : int = 1 *)

x := 2;;
(* - : unit = () *)
!x;;
(* - : int = 2 *)

!x + 1;;
(* - : int = 3 *)

x := 2;;
(* - : unit = () *)
x;;
(* - : int ref = {contents = 2} *)

let x = ref 0;;
(* val x : int ref = {contents = 0} *)
let f z = z + !x;;
(* val f : int -> int = <fun> *)
f 1;;
(* - : int = 1 *)
x := 3;;
(* - : unit = () *)
f 1;;
(* - : int = 4 *)

type btree = Leaf | Node of btree ref * int * btree ref;;
let rec insert (x: int) (n: btree ref) =
match !n with
Leaf -> n := Node(ref Leaf, x, ref Leaf)
| Node(l, v, r) ->
if x = v then () (* already present *)
else if x < v then insert x l (* put in left subtree *)
else insert x r;; (* put in right subtree *)
(* val insert : int -> btree ref -> unit = <fun> *)

let t = ref Leaf;;
(* val t : btree ref = {contents = Leaf} *)
insert 3 t;;
(* - : unit = () *)
t;;
(* - : btree ref =
{contents = Node ({contents = Leaf}, 3, {contents = Leaf})} *)
insert 1 t;;
(* - : unit = () *)
t;;
(* - : btree ref =
{contents =
Node ({contents = Node ({contents = Leaf}, 1, {contents = Leaf})}, 3,
{contents = Leaf})} *)

let a = [|1; 5; 7; 2|];;
(* val a : int array = [|1; 5; 7; 2|] *)

a.(1);;
(* - : int = 5 *)

a.(1) <- 9;;
(* - : unit = () *)
a;;
(* - : int array = [|1; 9; 7; 2|] *)

let x = ref 1;;
(* val x : int ref = {contents=1} *)
let y = x;;
(* val y : int ref = {contents=1} *)
y := 3;;
(* - : unit = () *)
  !y;;
(* - : int = 3 *)
!x;;
(* - : int = 3 *)

let a = [|1; 3; 5|];;
(* val a : int array = [|1; 3; 5|] *)
let b = a ;;
(* val b : int array = [|1; 3; 5|] *)
a.(0) <- 4;;
(* - : unit = () *)
a;;
(* - : int array = [|4; 3; 5|] *)
b;;
(* - : int array = [|4; 3; 5|] *)
