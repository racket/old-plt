
(define-signature board^
  (

           x o none     ; cell values
	   new-board    ; n => board
	                ;  where n is the board size
                        ;  the board is empty
	   board-cell   ; board col row => cell-value
	                ;   cols/rows are numbered from 0
	   push         ; board side index value => board
	                ;   where side is one of
	                ;        'top 'bottom 'left 'right
	                ;   index is a number in [0, n-1]
                        ;   values is x or o
	   rotate-cw    ; board turns => board
	                ;   rotates 90 degrees x turns
	                ;   rotation affects board-cell and
                        ;    push, but not find-board-in-history

	   new-history             ; => history
                                   ;  the history is empty
	   find-board-in-history   ; board history => board or #f
	   extend-history          ; board history => history
	   extend-history!         ; board history => history
                                   ;   maybe mutates the input history
  ))
