(require-library "functios.ss")

(define-signature turtle^ 
  (turtles

   flip-icons

   clear

   turn turn/radians
   move move-offset
   draw draw-offset
   erase erase-offset

   save-turtle-bitmap

   splitfn split*fn tpromptfn
   turtle-window-size))


