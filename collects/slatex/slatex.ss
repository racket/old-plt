(require-library "file.ss")

(define slatex
  (let ([ns (make-namespace)])
    (parameterize ([current-namespace ns])
      (require-library "slatexsrc.ss" "slatex")
      (global-defined-value 'slatex::*texinputs* #f)
      (global-defined-value 'slatex::*texinputs-list* #f))
    (lambda (input-file)
      (let* ([fixed-file (cond
			  [(file-exists? input-file) input-file]
			  [(file-exists? (string-append input-file ".tex"))
			   (string-append input-file ".tex")]
			  [else
			   (error 'slatex "~e does not exist" input-file)])]
	     [file (normalize-path fixed-file)])
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

           ;; boy, wouldn't it be great if the "actv" appleevent worked for OTEX?
	   ;(send-event "OTEX" "misc" "acvt")
           (let ([oztex-location (build-path (car (filesystem-root-list))
                                             "Applications"
                                             "OzTeX"
                                             "OzTeX")])
             (when (file-exists? oztex-location)
               (with-handlers ([void void]) ;; mzscheme cannot handle result
                 (send-event "MACS" "aevt" "odoc" (vector 'file oztex-location)))))
             
	   (send-event "OTEX" "aevt" "odoc" (vector 'file file))]
	   [(windows unix)
	    (system (format "latex ~a" file))])))))
