(define tmp-filename
  (build-path (collection-path "tests" "drscheme")
	      "launcher-test-tmp.ss"))
(define tmp-launcher
  (build-path (collection-path "tests" "drscheme")
	      (case (system-type)
		[(unix) "launcher-test-tmp"]
		[(windows) "launcher-test-tmp.exe"]
		[else (error 'launcher.ss "cannot run this test under ~s" (system-type))])))
(define tmp-teachpack
  (build-path (collection-path "tests" "drscheme")
	      "launcher-test-teachpack.ss"))

(define (get-port)
  (let loop ([n 100])
    (unless (zero? n)
      (with-handlers ([(lambda (x) #t)
		       (lambda (x)
			 (loop (- n 1)))])
	(let ([tcp-port (+ 51700 n)])
	  (values tcp-port
		  (tcp-listen tcp-port)))))))

(define (run-launcher/no-teachpack listener test expected)
  (when (file-exists? tmp-launcher)
    (delete-file tmp-launcher))
  (use-get/put-dialog
   (lambda ()
     (fw:test:menu-select "Scheme" "Create Launcher..."))
   tmp-launcher)
  (let-values ([(l-in l-out l-pid l-err l-proc) (apply values (process* tmp-launcher))]
	       [(in out) (tcp-accept listener)])
    (let ([got (read in)])
      (unless (equal? expected got)
	(error test "expected ~s, got ~s" expected got)))))

(define (teachpackless-test)
  (define-values (port-num listener) (get-port))
  (define drs (wait-for-drscheme-frame))
  (clear-definitions drs)
  (type-in-definitions
   drs
   `(let-values ([(in out) (tcp-connect "localhost" ,port-num)])
      (write 'the-correct-answer out)
      (newline out)))
  (when (file-exists? tmp-filename)
    (delete-file tmp-filename))
  (save-drscheme-window-as tmp-filename)
  (set-language-level! "Graphical without Debugging (MrEd)")
  (run-launcher/no-teachpack listener 'no-teachpack 'the-correct-answer))

(define (teachpack-test)
  (define-values (port-num listener) (get-port))
  (define drs (wait-for-drscheme-frame))
  (call-with-output-file tmp-teachpack
    (lambda (port)
      (write
       `(unit/sig (send-back)
	  (import plt:userspace^)
	  (define (send-back sexp)
	    (let-values ([(in out) (tcp-connect "localhost" ,port-num)])
	      (write sexp out)
	      (newline out)
	      (close-output-port out)
	      (close-input-port in))))
       port))
    'truncate)
  (clear-definitions drs)
  (type-in-definitions drs `(send-back 'the-correct-answer))
  (fw:test:menu-select "File" "Save")
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  (use-get/put-dialog
   (lambda ()
     (fw:test:menu-select "Language" "Add Teachpack..."))
   tmp-teachpack)
  (run-launcher/no-teachpack listener 'teachpack-beginner 'the-correct-answer))

(teachpackless-test)
(teachpack-test)