(require-library "slatex.ss" "slatex")
(define slatex::*texinputs* #f)
(define slatex::*texinputs-list* #f)
(let ([slatex-process-file
       (lambda (fn)
	 (let-values ([(base name dir?)
		       (split-path fn)])
	   (when (string? base) (current-directory base))
	   (slatex::process-main-tex-file fn)))])
  (case (system-type)
    [(macos)

     ;; set up drag and drop
     (current-load slatex-process-file)

     (for-each slatex-process-file (vector->list argv))]
    [(windows unix)
     (when (eq? (vector) argv)
       (error 'slatex "expected a file on the command line~n"))
     (slatex-process-file (vector-ref argv 0))
     (system (format "latex ~a" (vector-ref argv 0)))
     (exit)]))
