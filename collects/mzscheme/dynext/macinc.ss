  (define (cw-event . args)
    (apply send-event "CWIE" args))
  (define (cw . args)
    (apply cw-event "MMPR" args))

  (define (ping-cw?)
    (with-handlers ([void (lambda (x) #f)])
      (cw "GDoc")
      #t))
  
 (define (macos-make name base-project tmp-suffix quiet? input-files output-file includes)
    (define src-files (map path->complete-path input-files))
    (define dest-file (path->complete-path output-file))
    (define proj-dir (build-path (collection-path "mzscheme" "dynext")))
    (define tmp-proj (build-path proj-dir "building-extension"))
    (define ext-out (build-path proj-dir (format "extension.~a" tmp-suffix)))
    (define debug-out (string-append ext-out ".xSYM"))
    
    (delete-file dest-file)
    (delete-file tmp-proj)
    (unless (copy-file (build-path proj-dir base-project) 
                       tmp-proj)
         (error name "couldn't create the CodeWarrior project"))
    
    (with-handlers ([void (lambda (exn)
                             (error name "~a" (exn-message exn)))])
     (let ([started? (ping-cw?)])
      (unless started?
        ; Start CW
        (system "CWIE")
        (unless (ping-cw?)
          (error name "couldn't start CodeWarrior")))
	    ; Open the project
        (cw-event "aevt" "odoc" `#(file ,tmp-proj))
    	(for-each (lambda (f) (cw "AddF" f)) src-files)
    	(cw "Make")
    	; Clean up
    	(cw "ClsP")
    	(unless started?
          (cw-event "aevt" "quit"))))
          
    (delete-file tmp-proj)
    (delete-file debug-out)

    (unless (rename-file ext-out dest-file)
      (unless (copy-file ext-out dest-file)
      (error name "couldn't move output to destination: ~a" 
      	     output-file))))