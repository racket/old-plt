type xml = Text of string | Element of string * xml list;;
type step =
Tag of string (* Any child node with the given tag *)
| Pred of pred (* Node must satisfy predicate *)
| Any (* Any child node *)
| Desc (* Any descendant or self *)
and pred =
AtLeast of path*int (* At least this many nodes satisfy path *)
| Has of path*int (* Exactly this many nodes satisfy path *)
| And of pred*pred
| Or of pred*pred
| Not of pred
and path = step list;;

let hasTag tag xnode =
match xnode with
Element(t, _) -> t = tag
| _ -> false;;

let multimap f l =
List.flatten (List.map f l);;
(* val multimap : ('a -> 'b list) -> 'a list -> 'b list = <fun> *)

let any xl =
let anyn x =
match x with
Text _ -> []
| Element(_, c) -> c
in
multimap anyn xl;;
(* val any : xml list -> xml list = <fun> *)

let rec tag t xl =
let tagn x =
match x with
Text _ -> []
| Element(_, c) ->
List.filter (hasTag t) c
in multimap tagn xl;;
(* val tag : string -> xml list -> xml list = <fun> *)

let desc xl =
let rec descn x =
match x with
Text _ -> [x]
| Element(_, c) ->
x :: multimap descn c
in multimap descn xl;;
(* val desc : xml list -> xml list = <fun> *)

let rec eval (p:path) (xl:xml list) =
let rec test (pr:pred) (x:xml) =
match pr with
AtLeast(pa, n) -> List.length (eval pa [x]) >= n
| Has(pa, n) -> List.length (eval pa [x]) = n
| And(r, s) -> test r x && test s x
| Or(r, s) -> test r x || test s x
| Not r -> not (test r x)
and apply (l:xml list) (s:step) =
match s with
Tag t -> tag t l
| Any -> any l
| Desc -> desc l
| Pred p -> List.filter (test p) l
in List.fold_left apply xl p;;

let addrs = Element("addressbook",
[Element("person",
[Element("name", [Text "Steven Bird"]);
Element("address", [Text "Philadelphia"]);
Element("tel", [Text "215 898 1234"]);
Element("tel", [Text "610 678 4321"])]);
Element("person",
[Element("name", [Text "Chris Brew"]);
Element("address", [Text "Ohio"]);
Element("tel", [Text "123 456 9876"]);
Element("email", [Text "brew@abc.com"])]);
Element("person",
[Element("name", [Text "Jim Bean"]);
Element("fulladdress",
[Element("street", [Text "121 Park Place"]);
Element("city", [Text "Springfield"]);
Element("state", [Text "OH"]);
Element("zip", [Text "98756"])])])]);;

eval [Desc; Tag "person"; Pred (Or(Has([Tag "tel"], 1),Has([Tag "email"], 1))); Desc; Tag "name"] [addrs];;
(* - : xml list = [Element ("name", [Text "Chris Brew"])] *)


eval [Tag "person"; Desc; Tag "city"] [addrs];;
(* - : xml list = [Element ("city", [Text "Springfield"])] *)

eval [Desc; Pred (AtLeast([Tag "tel"], 1))] [addrs];;
(* - : xml list =
[Element ("person",
[Element ("name", [Text "Steven Bird"]);
Element ("address", [Text "Philadelphia"]);
Element ("tel", [Text "215 898 1234"]);
Element ("tel", [Text "610 678 4321"])]);
Element ("person",
[Element ("name", [Text "Chris Brew"]);
Element ("address", [Text "Ohio"]);
Element ("tel", [Text "123 456 9876"]);
Element ("email", [Text "brew@abc.com"])])] *)
