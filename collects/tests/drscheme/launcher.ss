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
    (let* ([sema (make-semaphore 0)]
	   [skip-break? #f]
	   [to-be-broken (current-thread)]
	   [thrd-timeout
	    (thread
	     (lambda ()
	       (sleep 60)
	       (semaphore-wait sema)
	       (break-thread to-be-broken)
	       (semaphore-post sema)))]
	   [make-thrd-port
	    (lambda (tag port)
	      (thread
	       (lambda ()
		 (let loop ()
		   (let ([l (with-handlers ([(lambda (x) #t)
					     (lambda (x) eof)])
			      (read-line port))])
		     (unless (eof-object? l)
		       (printf "~a>> ~a~n" tag l)
		       (loop)))))))]
	   [_ (begin (make-thrd-port "stdout" l-in)
		     (make-thrd-port "stderr" l-err))]
	   [got (with-handlers ([(lambda (x) #f)
				 (lambda (x)
				   (exn-message x))])
		  (semaphore-post sema)
		  (begin0
		   (read in)
		   (semaphore-wait sema)))])
      (break-thread thrd-timeout)
      (close-input-port l-in)
      (close-input-port l-err)
      (close-output-port l-out)
      (unless (equal? expected got)
	(error test "expected ~s, got ~s" expected got)))))

(define (teachpackless-test)
  (define-values (port-num listener) (get-port))
  (define drs (wait-for-drscheme-frame))
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  (clear-definitions drs)
  (type-in-definitions
   drs
   `(let-values ([(in out) (tcp-connect "localhost" ,port-num)])
      (write 'the-correct-answer out)
      (newline out)))
  (when (file-exists? tmp-filename)
    (delete-file tmp-filename))
  (save-drscheme-window-as tmp-filename)
  (let ([run-one-language
	 (lambda (language)
	   (printf "    teachpackless ~a~n" language)
	   (set-language-level! language)
	   (run-launcher/no-teachpack listener 'no-teachpack 'the-correct-answer))])
    (run-one-language "Graphical without Debugging (MrEd)")
    (run-one-language "Textual without Debugging (MzScheme)")
    (run-one-language "Graphical (MrEd)")
    (run-one-language "Textual (MzScheme)")))

(define (teachpack-test language insert-junk)
  (define-values (port-num listener) (get-port))
  (define drs (wait-for-drscheme-frame))

  (printf "    teachpack ~a (~a)~n" language insert-junk)

  (set-language-level! language)
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
  (insert-junk)
  (type-in-definitions drs `(send-back 'the-correct-answer))
  (fw:test:menu-select "File" "Save Definitions")
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  (use-get/put-dialog
   (lambda ()
     (fw:test:menu-select "Language" "Add Teachpack..."))
   tmp-teachpack)
  (run-launcher/no-teachpack listener 'teachpack-beginner 'the-correct-answer))

(teachpackless-test)

(teachpack-test "Graphical (MrEd)" void)
(teachpack-test "Textual (MzScheme)" void)
(teachpack-test "Textual without Debugging (MzScheme)" void)
(teachpack-test "Graphical without Debugging (MrEd)" void)
(teachpack-test "Beginning Student" void)
(teachpack-test "Intermediate Student" void)
(teachpack-test "Advanced Student" void)

(teachpack-test "Beginning Student"
		(rec
		 insert-text-box
		 (lambda ()
		   (let ([drs (wait-for-drscheme-frame)])
		     (fw:test:menu-select "Edit" "Insert Text Box")
		     (fw:test:keystroke #\a)
		     (fw:test:keystroke #\b)
		     (fw:test:keystroke #\c)))))