let abs(x:int) = if x>=0 then x else -x;; (* val abs : int -> int = <fun> *)
abs (5-3);; (* int = 2 *)
abs (3-4);; (* int = 1 *)
let whichIsSmallest (a:int) (b:int) (c:int) =
  if a <= b then
    if a <= c then 1
	else 3
  else
    if b <= c then 2
    else 3;;
(* val whichIsSmallest : int -> int -> int -> int = <fun> *)
whichIsSmallest 13 7 25;; (* int = 2 *)
not (2 < 1);; (* bool = true *)
not (2 = 2);; (* bool = false *)
1 < 2 || 2 >= 1;; (* bool = true *)
1 < 2 && 1 > 2;; (* bool = false *)
1 < 2 && 2 < 3;; (* bool = true *)
let square (x:int) = x * x;; (* val square : int -> int = <fun> *)
let sumsq (x:int) (y:int) = square x + square y;; (* val sumsq : int -> int -> int = <fun> *)
sumsq 3 4;;  (* int = 25 *)
let rec sum (n:int) = if n = 0 then 0 else n + sum (n-1);;
(* val sum : int -> int = <fun> *)
sum 6;; (* int = 21 *)
let rec fact (n:int) = if n = 0 then 1 else n * fact (n-1);;
(* val fact : int -> int = <fun> *)
fact 6;;(* int = 720 *)
let rec power (n:int) (m:int) =
if m = 0 then 1
else n * power n (m-1);; (* val power : int -> int -> int = <fun> *)
let rec gcd (x:int) (y:int) =
if x = y then y
else if x > y then gcd (x-y) y
else gcd x (y-x);; (* val gcd : int -> int -> int = <fun> *)
let rec choose (k:int) (n:int) =
if k = n then 1
else if k = 0 then 1
else choose (k-1) (n-1) + choose k (n-1);; (* val choose : int -> int -> int = <fun> *)
choose 3 10;; (* int = 120 *)
let changeP (a:int) = 1;; (* val changeP : int -> int = <fun> *)
let rec changePN (a:int) =
if a < 5 then changeP a
else changeP a + changePN (a-5);; (* varl changePN : int -> int = <fun> *)
let rec changePND (a:int) =
if a < 10 then changePN a
else changePN a + changePND (a-10);; (* val changePND : int -> int = <fun> *)
let rec changePNDQ (a:int) =
if a < 25 then changePND a
else changePND a + changePNDQ (a-25);; (* val changePNDQ : int -> int = <fun> *)
let rec changePNDQS (a:int) =
if a < 100 then changePNDQ a
else changePNDQ a + changePNDQS (a-100);; (* val changePNDQS : int -> int = <fun> *)
changePNDQS 5;;
(* int = 2 *)
changePNDQS 9;;
(* int = 2 *)
changePNDQS 10;;
(* int = 4 *)
changePNDQS 29;;
(* int = 13 *)
 changePNDQS 30;;
(* int = 18 *)
changePNDQS 100;;
(* int = 243 *)
changePNDQS 499;;
(* int = 33995 *)
