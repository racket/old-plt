
(load-relative "boardsig.ss")

(define-signature utils^
  (
   n-times      ; call a procedure with each number in [0, n]
   n-map        ; call a procedure with each number in [0, n] to make a list
   print-board  ; pretty-prints a board
   find-winner  ; checks a board to see if there's a winner
   other-player ; x -> o   or  o -> x

   quicksort    ; Quicksort

   pick-best    ; takes a list of (cons <num> <val>) and returns pair
                ;  with the biggest <num>, randomly chossing among
                ;  equals
   ))
