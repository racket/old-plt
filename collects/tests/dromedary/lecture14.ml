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
     
let hasTag tag xnode =
match xnode with
Element(t, _) -> t = tag
| _ -> false;;
(* val hasTag : string -> xml -> bool = <fun> *)
hasTag "addbook" addrs;;
(* - : bool = false *)
hasTag "addressbook" addrs;;
(* - : bool = true *)

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

let allDescendantsWithTag tag x =
filterDescendants (hasTag tag) x;;
(* val allDescendantsWithTag : string -> xml -> xml list = <fun> *)
allDescendantsWithTag "address" addrs;;
(* - : xml list =
[Element ("address", [Text "Philadelphia"]);
Element ("address", [Text "Ohio"])] *)





let getPersons x = allDescendantsWithTag "person" x;;
(* val getPersons : xml -> xml list = <fun> *)


getPersons addrs;;
(* - : xml list = [Element ("person",
[Element ("name", [Text "Steven Bird"]);
Element ("address", [Text "Philadelphia"]);
Element ("tel", [Text "215 898 1234"]);
Element ("tel", [Text "610 678 4321"])]);
Element ("person",
[Element ("name", [Text "Chris Brew"]);
Element ("address", [Text "Ohio"]);
Element ("tel", [Text "123 456 9876"]);
Element ("email", [Text "brew@abc.com"])]);
Element ("person",
[Element ("name", [Text "Jim Bean"]);
Element ("fulladdress",
[Element ("street", [Text "121 Park Place"]);
Element ("city", [Text "Springfield"]);
Element ("state", [Text "OH"]);
Element ("zip", [Text "98756"])])])] *)

let isSimple x =
match x with
Element(_, [Text _]) -> true
| _ -> false;;
(* val isSimple : xml -> bool = <fun> *)
let getValue x =
match x with
Element(_, [Text value]) -> Some value
| _ -> None;;
(* val getValue : xml -> string option = <fun> *)

let rec optionValues ol =
match ol with
[] -> []
| None::r -> optionValues r
| Some(v)::r -> v::optionValues r;;
(* val optionValues : 'a option list -> 'a list = <fun> *)
optionValues [Some "fie"; None; Some "foe"; None];;
(* - : string list = ["fie"; "foe"] *)

let getSimpleValues tag x =
optionValues
(List.map getValue
(allDescendantsWithTag tag x));;
(* val getSimpleValues : string -> xml -> string list = <fun> *)
getSimpleValues "address" addrs;;
(* - : string list = ["Philadelphia"; "Ohio"] *)
getSimpleValues "name" addrs;;
(* - : string list = ["Steven Bird"; "Chris Brew"; "Jim Bean"] *)

  let getName x =
match (getSimpleValues "name" x) with
[n] -> Some n
| _ -> None;;
(* val getName : xml -> string option = <fun> *)

let getNames xl =
optionValues (List.map getName xl);;
(* val getNames : xml list -> string list = <fun> *)
getNames (getPersons addrs);;
(* - : string list = ["Steven Bird"; "Chris Brew"; "Jim Bean"] *)
                   
let hasTels x =
(List.length (allDescendantsWithTag "tel" x)) > 0;;
(* val hasTels : xml -> bool = <fun> *)

let namesOfPeopleWithTels x =
getNames (List.filter hasTels (getPersons x));;
(* val namesOfPeopleWithTels : xml -> string list = <fun> *)
namesOfPeopleWithTels addrs;;
(* - : string list = ["Steven Bird"; "Chris Brew"] *)

let easyToReach x =
List.length (allDescendantsWithTag "tel" x) = 1 ||
List.length (allDescendantsWithTag "email" x) = 1;;
(* val easyToReach : xml -> bool = <fun> *)
let hardToReach x = not (easyToReach x);;
(* val hardToReach : xml -> bool = <fun> *)

let getHardToReach x =
getNames (List.filter hardToReach (getPersons x));;
(* val getHardToReach : xml -> string list = <fun> *)
getHardToReach addrs;;
(* - : string list = ["Steven Bird"; "Jim Bean"] *)

let easyAddrs x =
Element("addressbook",
List.filter easyToReach (getPersons x));;
(* val easyAddrs : xml -> xml = <fun> *)
let goodFriends = easyAddrs addrs;;
(* val goodFriends : xml =
Element ("addressbook",
[Element ("person",
[Element ("name", [Text "Chris Brew"]);
Element ("address", [Text "Ohio"]);
Element ("tel", [Text "123 456 9876"]);
Element ("email", [Text "brew@abc.com"])])]) *)

