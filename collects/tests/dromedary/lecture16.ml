type re =
Empty (* Always matches consuming no input *)
| Char of char (* Matches the given character *)
| Seq of re * re (* An expression and then the other *)
| Alt of re * re (* One expression or another *)
| Rep of re;; (* 0 or more repetitions of an expression *)

Seq(Char 'A',
Seq(Alt(Empty, Char 'T'),
Char 'G'));;

Seq(Seq(Char 'T', Char 'A'),
Seq(Rep(Seq(Char 'G', Char 'C')),
Seq(Char 'T', Char 'A')));;

let rec skippable (e: re) =
match e with
Empty -> true
| Char _ -> false
| Seq(e1, e2) -> skippable e1 && skippable e2
| Alt(e1, e2) -> skippable e1 || skippable e2
| Rep _ -> true;;

let multimap (f: 'a -> 'b list) (l: 'a list) =
List.flatten (List.map f l);;
(* e1 followed by e2 *)

let followedBy (e2: re) (e1: re) =
match e1 with
Empty -> e2
| _ -> Seq(e1, e2);;

(* List of expressions that e transforms into after consuming c *)
let rec advance (c: char) (e: re) =
match e with
Empty -> []
| Char d ->
if c = d then [Empty] else []
| Seq(e1, e2) ->
let d1 = List.map (followedBy e2) (advance c e1)
in if skippable e1 then
d1 @ (advance c e2)
else d1
| Alt(e1, e2) -> (advance c e1) @ (advance c e2)
| Rep f ->
List.map (followedBy e) (advance c f);;

(* The list of expressions that the elements of es
transform into after consuming s from position i *)
let rec matches (s: string) (i:int) (es:re list) =
if i >= String.length s then
es
else
matches s (i+1)
(multimap (advance (String.get s i)) es);;
(* Does e match s? *)
let rec accepts (e: re) (s: string) =
List.length
(List.filter skippable (matches s 0 [e])) > 0;;

let es0 = [Seq(Seq(Char 'T', Char 'A'),
Seq(Rep(Seq(Char 'G', Char 'C')),
Seq(Char 'T', Char 'A')))];;
(* val es0 : re list =
[Seq (Seq (Char 'T', Char 'A'),
Seq (Rep (Seq (Char 'G', Char 'C')), Seq (Char 'T', Char 'A')))] *)

let es1 = multimap (advance 'T') es0;;
(* val es1 : re list =
[Seq (Char 'A',
Seq (Rep (Seq (Char 'G', Char 'C')), Seq (Char 'T', Char 'A')))] *)

let es2 = multimap (advance 'A') es1;;
(* val es2 : re list =
[Seq (Rep (Seq (Char 'G', Char 'C')), Seq (Char 'T', Char 'A'))] *)

let es3 = multimap (advance 'G') es2;;
(* val es3 : re list =
[Seq (Seq (Char 'C', Rep (Seq (Char 'G', Char 'C'))),
Seq (Char 'T', Char 'A'))] *)
let es4 = multimap (advance 'C') es3;;
(* val es4 : re list =
[Seq (Rep (Seq (Char 'G', Char 'C')), Seq (Char 'T', Char 'A'))] *)
let es5 = multimap (advance 'T') es4;;
(* val es5 : re list = [Char 'A'] *)
let es6 = multimap (advance 'A') es5;;
(* val es6 : re list = [Empty] *)

