(require-library "file.ss")

(define slatex
  (let ([ns (make-namespace)])
    (parameterize ([current-namespace ns])
      (require-library "slatexsrc.scm" "slatex" "slatex-code")
      (global-defined-value 'slatex::*texinputs* #f)
      (global-defined-value 'slatex::*texinputs-list* #f))
    (lambda (file)
      (unless (file-exists? file)
	(error 'slatex "~e does not exist" file))
      (let ([file (normalize-path file)])
	(let-values ([(base name dir?) (split-path file)])
	  (parameterize ([current-namespace ns]
			 [current-directory
			  (if (string? base)
			      base
			      (current-directory))])
	    (eval `(slatex::process-main-tex-file ,name))))
	(case (system-type)
	  [(macos)
	   (system "OTEX")
	   (send-event "OTEX" "aevt" "odoc" (vector 'file file))]
	   [(windows unix)
	    (system (format "latex ~a" file))])))))
