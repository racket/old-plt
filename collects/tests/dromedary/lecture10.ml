2.1 *. 3.4;;
(* float = 7.14 *)
1.3 +. 4.7;;
(* float = 6 *)
1.3 -. 4.7;;
(* float = -3.4 *)
float 3 +. 4.5;;
(* float = 7.5 *)
type square = Square of float * float * float;;
Square(1.1,2.2,3.3);;
(* square = Square (1.1, 2.2, 3.3) *)
let areaOfSquare s =
match s with
Square(_, _, d) -> d *. d;;
(* val areaOfSquare: square -> float = <fun> *)
let positionOfSquare s =
match s with
Square(x, y, _) -> (x,y);;
(* val positionOfSquare : square -> float * float = <fun> *)
let areaOfSquare (Square(_, _, d)) = d *. d;;
let positionOfSquare (Square(x, y, _)) = (x,y);;

type circle = Circle of float * float * float;;
let c = Circle (1.0, 2.0, 2.0);;
let areaOfCircle (Circle(_, _, r)) = 3.14159 *. r *. r;;
let positionOfCircle (Circle(x, y, _)) = (x,y);;
areaOfCircle c;;
(* float = 12.56636 *)

type shape = Circle of float * float * float
| Square of float * float * float;;
Square(1.,2.,3.);;
(* shape = Square (1., 2., 3.) *)
Circle(2.,2.,1.);;
(* shape = Circle (2., 2., 1.) *)
let area s =
match s with
Circle (_, _, r) -> 3.14159 *. r *. r
| Square (_, _, d) -> d *. d;;
(* val area : shape -> float = <fun> *)
area (Square(1.,2.,3.));;
(* float = 9. *)
area (Circle(2.,2.,1.));;
(* float = 3.14159 *)
let l = [Circle (0.0, 0.0, 1.5); Square (1.0, 2.0, 1.0);
Circle (2.0, 0.0, 1.5); Circle (5.0, 0.0, 2.5)];;
List.map area l;;
(* float list = [7.0685775; 1; 7.0685775; 19.6349375] *)

let boundingBox s =
match s with
Circle (x, y, r) -> Square(x -. r, y -. r, 2.0 *. r)
| s -> s;;

type shape = Circle of float * float * float
| Square of float * float * float
| TwoShapes of shape * shape;;
let c1 = Circle (2.0, 2.0, 4.0);;
(* val c1 : shape = Circle (2., 2., 4.) *)
let s1 = Square (1.0, 1.0, 3.0);;
(* val s1 : shape = Square (1., 1., 3.) *)
let ss = TwoShapes(c1,s1);;
(* val ss : shape = TwoShapes (Circle (2., 2., 4.), Square (1., 1., 3.)) *)

let sss = TwoShapes(c1, ss);;
(* val sss : shape =
TwoShapes (Circle (2., 2., 4.),
TwoShapes (Circle (2., 2., 4.), Square (1., 1., 3.))) *)

let x (Square(x1,_,_)) = x1;;
let y (Square(_,y1,_)) = y1;;
let w (Square(_,_,z1)) = z1;;
let rec boundingBox s =
match s with
Circle (x, y, r) -> Square(x -. r, y -. r, 2.0 *. r)
| Square (x, y, w) -> Square(x, y, w)
| TwoShapes (shape1,shape2) ->
let bb1 = boundingBox shape1
and bb2 = boundingBox shape2
in
let lx = min (x bb1) (x bb2)
and ly = min (y bb1) (y bb2)
and hx = max (x bb1 +. w bb1) (x bb2 +. w bb2)
and hy = max (y bb1 +. w bb1) (y bb2 +. w bb2)
in Square(lx, ly, max (hx -. lx) (hy -. ly));;
boundingBox(Square(1.0, 2.0, 3.0));;
boundingBox(Circle(2.0, 2.0, 4.0));;
boundingBox(TwoShapes(Square(1.0, 2.0, 3.0), Circle(2.0, 2.0, 4.0)));;
