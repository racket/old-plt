1 + + 1;;

(* type declaration errors *)
type foo = 'a list;;

type tree = Leaf | Node of 'a tree * int;;

type 'a tree = Leaf | Node of tree * 'a;;

type ('a, 'b) tree = Leaf | Node of 'a tree;;

type foo = list;;

type ('a, 'b) foo = ('a, 'b) ref;;

type ('a, 'b, 'c) foo = ('a, 'b, 'c) array;;

type foo = 'a int;;

type foo = moogly;;

(* identifier errors *)
foo 2;;

Foo.bar 2;;

String.foo 2;;

(* constructor errors *)
Foo 3;;

type foo = A;;
A 2;;

type foo = A;;
A (1, 2);;

(* let rec errors *)
let rec x = 3;;

let rec (x, y) = (4, 5);;

(* function application errors *)
(+) 1 2 3;;

(* match errors *)
match (3, 4) with (x, x) -> x + x | (x, y) -> x + y;;

type foo = A of int;;
match (A 2) with A -> 3;;

type foo = A;;
match A with (A 2) ->3;;

match 3 with A -> 3;;

(* function argument errors *)
type goo = B of int;;
let f B = 2;;

type goo = A;;
let f (A x) = x;;

let f (A x) = x;;

[| 1; true; 3 |];;
