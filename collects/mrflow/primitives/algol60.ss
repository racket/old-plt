; Algol 60 primitives and runtime support
(
 ; primitives
 
 (!= (number number -> boolean))
 (! (boolean -> boolean))
 (& (boolean boolean -> boolean))
 (\| (boolean boolean -> boolean))
 (=> (boolean boolean -> boolean))
 (== (boolean boolean -> boolean))
 
 (sign (forall ([a top])
               ((real -> a) (-> real) -> a)))
 (entier (forall ([a top])
                 ((real -> a) (-> real) -> a)))
 
 (a60:sin (forall ([a top])
                  ((complex -> a) (-> complex) -> a)))
 (a60:cos (forall ([a top])
                  ((complex -> a) (-> complex) -> a)))
 (a60:arctan (forall ([a top])
                     ((complex -> a) (-> complex) -> a)))
 (a60:sqrt (forall ([a top])
                   ((complex -> a) (-> complex) -> a)))
 (a60:abs (forall ([a top])
                  ((real -> a) (-> real) -> a)))
 (a60:ln (forall ([a top])
                 ((complex -> a) (-> complex) -> a)))
 (a60:exp (forall ([a top])
                  ((complex -> a) (-> complex) -> a)))

 (prints (forall ([a top])
                 ((void -> a) (-> top) -> a)))
 (printn (forall ([a top])
                 ((void -> a) (-> top) -> a)))
 (printsln (forall ([a top])
                   ((void -> a) (-> top) -> a)))
 (printnln (forall ([a top])
                   ((void -> a) (-> top) -> a)))
 
 ; runtime support
 
 ;(a60:array (struct a60:array (dependant type)))
 ;(a60:switch (struct a60:switch (choices))
 
 (undefined undefined)
 
 (check-boolean (forall ([a top]) (a -> a)))
 (goto (forall ([a top]) ((-> a) -> a)))
 (get-value (forall ([a top]) ((-> a) -> a)))
 (set-target! (forall ([a top][b top])
                      ((a -> b) a -> b)))
 ;make-array
 ;array-ref
 ;array-set!
 ;make-switch
 ;switch-ref
 )