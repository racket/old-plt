(define-signature turtle^ 
  (turtles

   Turtles 
   Cache 

   clear

   turn turn/radians
   move move-offset
   draw draw-offset
   erase erase-offset

   save-turtle-bitmap

   splitfn split*fn 
   turtle-window-size pi))

(define-signature turtle:create-window^ 
  (create-turtle-window))


