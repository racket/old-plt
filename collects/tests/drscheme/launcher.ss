(define tmp-filename
  (build-path (collection-path "tests" "drscheme")
	      "launcher-test-tmp.ss"))
(define tmp-launcher
  (build-path (collection-path "tests" "drscheme")
	      (case (system-type)
		[(unix) "launcher-test-tmp"]
		[(windows) "launcher-test-tmp.exe"]
		[else (error 'launcher.ss "cannot run this test under ~s" (system-type))])))

(define-values (port-num listener)
  (let loop ([n 100])
    (unless (zero? n)
      (with-handlers ([(lambda (x) #t)
		       (lambda (x)
			 (loop (- n 1)))])
	(let ([tcp-port (+ 51700 n)])
	  (values tcp-port
		  (tcp-listen tcp-port)))))))

(define drs (wait-for-drscheme-frame))
(define definitions-canvas (ivar drs definitions-canvas))
(send definitions-canvas focus)
(fw:test:menu-select "Edit" "Select All")
(for-each
 fw:test:keystroke
 (let ([port (open-output-string)])
   (parameterize ([current-output-port port])
     (write
      `(let-values ([(in out) (tcp-connect "localhost" ,port-num)])
	 (write '() out))))
   (string->list (get-output-string port))))
(when (file-exists? tmp-filename)
  (delete-file tmp-filename))
(save-drscheme-window-as tmp-filename)

(define (create-launcher language teachpack)
  (set-language-level! language)
  (when (file-exists? tmp-launcher)
    (delete-file tmp-launcher))
  (use-open/close-dialog
   (lambda ()
     (fw:test:menu-select "Scheme" "Create Launcher..."))
   tmp-launcher)
  (let-values ([(l-in l-out l-pid l-err l-proc) (apply values (process* tmp-launcher))]
	       [(in out) (tcp-accept listener)])
    (printf "about to read~n")
    (unless (null? (read in))
      (error))))

(create-launcher "Graphical without Debugging (MrEd)" #f)