(require-library "slatex.ss" "slatex")
(define slatex::*texinputs* #f)
(define slatex::*texinputs-list* #f)
(current-load 
 (lambda (fn) 
   (let-values ([(base name dir?)
		 (split-path fn)])
     (when (string? base) (current-directory base))
     (slatex::process-main-tex-file fn))))
