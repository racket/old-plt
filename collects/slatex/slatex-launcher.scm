(require-library "slatex.ss" "slatex")
(define slatex::*texinputs* #f)
(define slatex::*texinputs-list* #f)
(current-load (lambda (fn) (slatex::process-main-tex-file fn)))
