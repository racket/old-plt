type xml = Text of string | Element of string * xml list;;


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

let isElement xnode =
match xnode with
Element(t, _) -> true
| Text _ -> false;;
(* val isElement : xml -> bool = <fun> *)
isElement addrs;;
(* - : bool = true *)
isElement (Text "Philadelphia");;
(* - : bool = false *)

let getTag xnode =
match xnode with
Element(t, _) -> Some t
| Text _ -> None;;
(* val getTag : xml -> string option = <fun> *)
getTag addrs;;
(* - : string option = Some "addressbook" *)
getTag (Text "Ohio");;
(* - : string option = None *)

let isSimple xnode =
match xnode with
Element(_, [Text _]) -> true
| _ -> false;;
(* val isSimple : xml -> bool = <fun> *)
isSimple addrs;;
(* - : bool = false *)
isSimple (Element("city",[Text "Springfield"]));;
(* - : bool = true *)

let getValue xnode =
match xnode with
Element(_, [Text value]) -> Some value
| _ -> None;;
(* val getValue : xml -> string option = <fun> *)
getValue (Element("address", [Text "Philadelphia"]));;
(* - : string option = Some "Philadelphia" *)

let hasTag tag xnode =
match xnode with
Element(t, _) -> t = tag
| _ -> false;;
(* val hasTag : string -> xml -> bool = <fun> *)
hasTag "addbook" addrs;;
(* - : bool = false *)
hasTag "addressbook" addrs;;
(* - : bool = true *)

let rec size xnode =
match xnode with
Text _ -> 1
| Element(tag, xlst) ->
1 + List.fold_right (+) (List.map size xlst) 0;;
(* val size : xml -> int = <fun> *)
size addrs;;
(* - : int = 31 *)

let rec allDescendants x =
match x with
Text _ -> [x]
| Element(_, ch) ->
x :: (List.flatten
(List.map allDescendants ch));;
(* val allDescendants : xml -> xml list = <fun> *)


let filterDescendants p x =
List.filter p (allDescendants x);;
(* val filterDescendants : (xml -> bool) -> xml -> xml list = <fun> *)
filterDescendants (hasTag "address") addrs;;
(* - : xml list =
[Element ("address", [Text "Philadelphia"]);
Element ("address", [Text "Ohio"])] *)


