
; Images don't work at the moment...
(set! argv (list->vector
	    (list* "x" "x" "n" 
		   (vector->list argv))))

(load-relative "mzrice.ss")
(go)
(read-eval-print-loop)
