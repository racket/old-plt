let swap x y =
let u = !x
and v = !y in
x := v; y := u;;
(* val swap : 'a ref -> 'a ref -> unit = <fun> *)
let a = ref 1;;
(* val a : int ref = {contents = 1} *)
let b = ref 2;;
(* val b : int ref = {contents = 2} *)
swap a b;;
(* - : unit = () *)

let rotate a =
let len = Array.length a in
let last = a.(len-1)
and i = ref (len-1) in
while !i > 0 do
a.(!i) <- a.(!i-1);
i := !i - 1
done;
a.(0) <- last;;
(* val rotate : 'a array -> unit = <fun> *)
let a = [|1;2;3|];;
(* val a : int array = [|1; 2; 3|] *)
rotate a;;
(* - : unit = () *)
a;;
(* - : int array = [|3; 1; 2|] *)

let add_vect v1 v2 =
let len = min (Array.length v1) (Array.length v2) in
let res = Array.make len 0.0 in (* create array of len 0.0's *)
for i = 0 to (len - 1) do
res.(i) <- v1.(i) +. v2.(i)
done;
res;; (* return the new array *)
(* val add_vect : float array -> float array -> float array = <fun> *)
add_vect [|1.0; 2.0|] [|3.0; 4.0|];;
(* - : float array = [|4.; 6.|] *)

let index (x: 'a) (a: 'a array) =
let rec from i =
if i < Array.length a then
if a.(i) = x then Some i
else from (i+1)
else None
in from 0;;
(* val index : 'a -> 'a array -> int option = <fun> *)
index 3 [|5; 2; 3; 4; 3|];;
(* - : int option = Some 2 *)

let search (x: 'a) (a: 'a array) =
(* look for x from lo to hi-1 *)
let rec fromto lo hi =
if (lo >= hi) then None
else
let mid = (lo + hi - 1)/2 in
if a.(mid) = x then Some mid
else if x < a.(mid) then fromto lo mid
else fromto (mid+1) hi
in
fromto 0 (Array.length a);;
(* val search : 'a -> 'a array -> int option = <fun> *)
search 3 [|2; 3; 4; 7; 9|];;
(* - : int option = Some 1 *)
let split (a: 'a array) (bot: int) (top: int) =
let swap p q = (* swap the array elts at positions p and q *)
let x = a.(p) in
a.(p) <- a.(q); a.(q) <- x
and pivot = a.(bot) (* the element to split on *)
and lo = ref (bot+1) (* lowest index to consider *)
and hi = ref (top-1) (* highest index *)
in while (!lo < !hi) do
while ((!lo < !hi) & (a.(!lo) <= pivot)) do lo := !lo+1 done;
while (a.(!hi) > pivot) do hi := !hi-1 done;
if (!lo < !hi) then swap !lo !hi; (* elts out of order *)
done;
if bot < !hi then swap bot !hi; (* swap pivot into split point *)
!hi;; (* return split point *)

let quicksort (a: 'a array) =
let rec qs bot top =
if top-bot <= 1 then ()
else
let mid = split a bot top
in
qs bot mid;
qs (mid+1) top
in qs 0 (Array.length a);;

let a = [|4; 1; 9; 7; 8; 3; 2; 5; 6|];;
split a 0 9;; (* 3 *)
let a = [|3; 1; 2; 4; 8; 7; 9; 5; 6|];;
split a 0 3;; (* 2 *)
let a = [|2; 1; 3; 4; 8; 7; 9; 5; 6|];;
split a 0 2;; (* 1 *)
let a = [|1; 2; 3; 4; 8; 7; 9; 5; 6|];;
split a 4 9;; (* 7 *)
let a = [|1; 2; 3; 4; 5; 7; 6; 8; 9|];;
split a 4 7;; (* 4 *)
let a = [|1; 2; 3; 4; 5; 7; 6; 8; 9|];;
split a 5 7;; (* 6 *)
let a = [|1; 2; 3; 4; 5; 6; 7; 8; 9|];;


let rec f (x:int array) =
x.(1) <- 5;;
let a = [|0;1;2;3|];;
f a;;
a.(1);;

let rec f y =
let x = [|0;1|] in
x.(0) <- 5;
x.(1) <- y;
x;;
let a = f 2;;
a.(0);;

let a = [|0;1|];;
let b = [|2;3|];;
b.(1) <- 99;;
let a = b;;
a.(1);;

let f a b =
a.(1) <- 0;
b.(1) <- 5;
a.(1);;

let f a b =
a.(1) <- 0;
b.(1) <- 5;
a.(1);;
let a = [|0;1;2;3|];;
f a a;;

