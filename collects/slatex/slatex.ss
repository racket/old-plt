
(module slatex mzscheme
  (require (lib "file.ss")
	   (lib "process.ss"))

  (provide slatex latex slatex/no-latex)

  (define (filename->latex-filename input-file)
    (cond
     [(file-exists? input-file) input-file]
     [(file-exists? (string-append input-file ".tex"))
      (string-append input-file ".tex")]
     [else
      (error 'filename->latex-filename "~e does not exist" input-file)]))

  (define (latex input-file)
    (let ([file (filename->latex-filename (normalize-path input-file))])
      (case (system-type)
	[(macos)
	 (system "OTEX")

	 ;; boy, wouldn't it be great if the "actv" appleevent worked for OTEX?
	 ;;(send-event "OTEX" "misc" "acvt")
	 (let* ([build-oztex-locations
		 (list
		  (lambda (x)
		    (build-path x
				"Applications"
				"OzTeX"
				"OzTeX"))
		  (lambda (x)
		    (build-path x
				"Applications (Mac OS 9)"
				"OzTeX"
				"OzTeX")))]
		[oztex-locations
		 (apply
		  append
		  (map (lambda (f) (map f (filesystem-root-list))) build-oztex-locations))]
		[oztex-location (ormap (lambda (x) (if (file-exists? x) x #f)) oztex-locations)])
           (when oztex-location
	     (with-handlers ([void void]) ;; mzscheme cannot handle result
	       (send-event "MACS" "aevt" "odoc" (vector 'file oztex-location)))))
	 (send-event "OTEX" "aevt" "odoc" (vector 'file file))]
	[(windows unix macosx) ;; is this also okay for beos?
	 (system (format "latex ~a" file))]
	[else
	 (error 'latex "do not know how to run latex on ~s" (system-type))])))

  (define (slatex filename)
    (slatex/no-latex filename)
    (latex filename))

  (define slatex/no-latex
    (let ([ns (make-namespace)])
      (parameterize ([current-namespace ns])
	(load/use-compiled (build-path (collection-path "slatex") "slatexsrc.ss"))
	(namespace-variable-binding 'slatex::*texinputs* #f)
	(namespace-variable-binding 'slatex::*texinputs-list* #f))
      (lambda (input-file)
	(let* ([fixed-file (filename->latex-filename input-file)]
	       [file (normalize-path fixed-file)])
	  (let-values ([(base name dir?) (split-path file)])
	    (parameterize ([current-namespace ns]
			   [current-directory
			    (if (string? base)
				base
				(current-directory))])
	      (eval `(slatex::process-main-tex-file ,name)))))))))

