(require-library "errortrace.ss" "errortrace")
(load-relative "start-drs.ss")
(start-drscheme)
(yield (make-semaphore))

#|
(require-library "errortrace.ss" "errortrace")
(error-print-width 80)

(require-library "refer.ss")
(define file-stack null)
(define file-ht (make-hash-table))
(define value-ht (make-hash-table))
(define mods-ht (make-hash-table))

(define (graphical-read-eval-print-loop)
  ;; The REPL buffer class
  (define esq:text%
    (class text% ()
      (inherit insert last-position get-text erase change-style clear-undos
	       begin-edit-sequence end-edit-sequence get-start-position)
      (rename [super-on-char on-char])
      (private [prompt-pos 0]
	       [locked? #f])
      (override
	[can-insert? (lambda (start end) (and (>= start prompt-pos)
					      (not locked?)))]
	[can-delete? (lambda (start end) (and (>= start prompt-pos)
					      (not locked?)))]
	[on-char (lambda (c)
		   (super-on-char c)
		   (when (and (memq (send c get-key-code)
				    '(#\return #\newline #\003))
			      (not locked?))
		     (set! locked? #t)
		     (evaluate (get-text prompt-pos (last-position)))))])
      (private
	[plain-style (make-object style-delta% 'change-normal)])
      (inherit delete)
      (public
	[set-input
	 (lambda (string)
	   (begin-edit-sequence)
	   (delete prompt-pos (last-position))
	   (insert string (last-position) (last-position))
	   (end-edit-sequence))]
	[new-prompt (lambda ()
		      (output "> " #f)
		      (set! prompt-pos (last-position))
		      (set! locked? #f)
		      (clear-undos))]
	[output (lambda (str style-delta)
		  (let ([l? locked?])
		    (set! locked? #f)
		    (begin-edit-sequence)
		    (let ([pos (last-position)])
		      (insert str pos pos)
		      (change-style (or style-delta plain-style)
				    pos
				    (last-position)))
		    (end-edit-sequence)
		    (set! locked? l?)))]
	[reset (lambda ()
		 (set! locked? #f)
		 (set! prompt-pos 0)
		 (erase)
		 (new-prompt))])
      (sequence 
	(super-init)
	(let ([s (last-position)])
	  (insert (format "Welcome to MrEd version ~a." (version)))
	  (let ([e (last-position)])
	    (insert #\newline)
	    (change-style (send (make-object style-delta% 'change-bold) set-delta-foreground "BLUE") s e)))
	(output (format "Copyright (c) 1995-98 PLT (Matthew Flatt and Robby Findler)~n") #f)
	(insert "This is a simple window for evaluating MrEd Scheme expressions.") (insert #\newline)
	(let ([s (last-position)])
	  (insert "Quit now and run DrScheme to get a better window.")
	  (let ([e (last-position)])
	    (insert #\newline)
	    (change-style
	     (send (make-object style-delta% 'change-style 'slant) set-delta-foreground "RED")
	     s e)))
	(insert "The current input port always returns eof.") (insert #\newline)
	(new-prompt))))

  (define frame-section "MrEd-REPL-size")

  (define-values (frame-width frame-height)
    (let* ([default (lambda (reason)
		      ;(printf "using default console size: ~a~n" reason)
		      (values 500 400))]
	   [b (box "")]
	   [s (get-resource "mred" frame-section b)])
      (with-handlers ([(lambda (x) #t)
		       (lambda (x)
			 (default (if (exn? x) (exn-message x) x)))])
	(if s
	    (let* ([p (open-input-string (unbox b))]
		   [l (read p)])
	      (if (and (list? l)
		       (= 2 (length l))
		       (andmap number? l))
		  (values (car l) (cadr l))
		  (default "not alist of length two of numbers")))
	    (default "no resource returned")))))

  ;; GUI creation
  (define frame (make-object (class frame% args
			       (inherit accept-drop-files)
			       (override
				[on-size
				 (lambda (w h)
				   (write-resource
				    "mred"
				    frame-section
				    (format "~s" (list w h))
				    (find-graphical-system-path 'setup-file)))]
				 [on-close (lambda () 
					     (custodian-shutdown-all user-custodian)
					     (semaphore-post waiting))]
				 [on-drop-file (lambda (f) (evaluate (format "(load ~s)" f)))])
			       (sequence (apply super-init args) (accept-drop-files #t)))
			     "Color MrEd REPL"
			     #f
			     frame-width
			     frame-height))
  (define repl-buffer (make-object esq:text%))
  (define repl-display-canvas (make-object editor-canvas% frame))

  ;; User space initialization
  (define user-custodian (make-custodian))
  
  (define user-eventspace
    (parameterize ([current-custodian user-custodian])
      (make-eventspace)))
  (define user-parameterization (eventspace-parameterization user-eventspace))
  
  (define make-user-output-port
    (let ([semaphore (make-semaphore 1)])
      (lambda (style-delta)
	(make-output-port (lambda (s)
			    (semaphore-wait semaphore)
			    (send repl-buffer output s style-delta)
			    (semaphore-post semaphore))
			  (lambda () 'nothing-to-do)))))

  (define user-output-port (make-user-output-port
			    (send (make-object style-delta%)
				  set-delta-foreground "PURPLE")))
  (define user-error-port (make-user-output-port
			    (send (make-object style-delta% 'change-style 'slant)
				  set-delta-foreground "RED")))
  (define user-value-port (make-user-output-port
			   (send (make-object style-delta% 'change-bold)
				 set-delta-foreground "BLUE")))
  
  
  ;; Evaluation and resetting
  
  (define previous-expressions null)
  (define (previous-input)
    (unless (null? previous-expressions)
      (let ([exp (car previous-expressions)])
	(send repl-buffer set-input exp)
	(set! previous-expressions
	      (let loop ([l (cdr previous-expressions)])
		(cond
		  [(null? l) (list exp)]
		  [else (cons (car l) (loop (cdr l)))]))))))
  (define (next-input)
    (unless (null? previous-expressions)
      (let loop ([l previous-expressions])
	(cond
	 [(null? (cdr l)) (send repl-buffer set-input (car l))
			  (set-cdr! l null)]
	 [else (loop (cdr l))]))))
  (define (remember exp)
    (set! previous-expressions
	  (let loop ([n 50]
		     [l (cons (substring exp 0 (- (string-length exp) 1))
			      previous-expressions)])
	    (cond
	     [(or (zero? n) (null? l)) previous-expressions]
	     [else (cons (car l) (loop (- n 1) (cdr l)))]))))

  (define (evaluate expr-str)
    (remember expr-str)
    (parameterize ([current-eventspace user-eventspace])
      (queue-callback
       (lambda ()
	 (current-parameterization user-parameterization)
	 (dynamic-wind
	  void
	  (lambda () 
	    (call-with-values
	     (lambda () (eval (read (open-input-string expr-str))))
	     (lambda results
	       (for-each 
		(lambda (v) (print v user-value-port) (newline))
		results))))
	  (lambda ()
	    (send repl-buffer new-prompt)))))))

  (define waiting (make-semaphore 0))

  (let ([mb (make-object menu-bar% frame)])
    (let ([m (make-object menu% "&File" mb)])
      (make-object menu-item% "Load File..." m (lambda (i e) (let ([f (get-file)]) (and f (evaluate (format "(load ~s)" f))))))
      (make-object menu-item% 
		   (if (eq? (system-type) 'windows)
		       "E&xit"
		       "&Quit")
		   m (lambda (i e) (send frame on-close) (send frame show #f)) #\q))
    (let ([m (make-object menu% "&Edit" mb)])
      (append-editor-operation-menu-items m #f)))

  ;; Just a few extra key bindings:
  (install-standard-text-bindings repl-buffer)
  (let ([console-keymap (make-object keymap%)])
    (send console-keymap add-key-function
	  "previous-input"
	  (lambda (value key-event) (previous-input)))
    (send console-keymap add-key-function
	  "next-input"
	  (lambda (value key-event) (next-input)))
    (send console-keymap map-function "m:p" "previous-input")
    (send console-keymap map-function "m:n" "next-input")
    (send console-keymap map-function "c:p" "previous-input")
    (send console-keymap map-function "c:n" "next-input")
    (send console-keymap map-function "a:p" "previous-input")
    (send console-keymap map-function "a:n" "next-input")
    (send (send repl-buffer get-keymap) chain-to-keymap console-keymap #t))




  (send repl-buffer auto-wrap #t)

  ;; Go
  ((in-parameterization user-parameterization current-output-port) user-output-port)
  ((in-parameterization user-parameterization current-error-port) user-error-port)
  ((in-parameterization user-parameterization current-input-port) (make-input-port (lambda () eof) void void))
  ((in-parameterization user-parameterization current-custodian) user-custodian)
  ((in-parameterization user-parameterization current-will-executor) (make-will-executor))
  (send repl-display-canvas set-editor repl-buffer)
  (send frame show #t)

  (send repl-display-canvas focus)

  (yield waiting))

(current-load
 (let ([ol (current-load)])
   (lambda (fn)
     (unless (file-exists? fn)
       (error 'load-handler "file ~a does not exist" fn))
     (let ([sym (string->symbol fn)])
       (dynamic-wind
	(lambda ()
	  (for-each (lambda (stack-fn)
		      (let ([old (hash-table-get file-ht stack-fn (lambda () null))])
			(unless (member fn old)
			  (hash-table-put! file-ht stack-fn (cons fn old)))))
		    file-stack)
	  (hash-table-put! mods-ht sym (file-or-directory-modify-seconds fn))
	  (set! file-stack (cons sym file-stack)))
	(lambda () (ol fn))
	(lambda ()
	  (set! file-stack (cdr file-stack))))))))

(define check-require/proc
  (lambda (filename)
    (unless (file-exists? filename)
      (error 'check-require/proc "file does not exist: ~a~n" filename))

    (let* ([sym (string->symbol filename)]
	   [load/save
	    (lambda (filename reason)
	      (printf "loading: ~a because ~a~n" filename reason)
	      (let ([ans (load filename)])
		(hash-table-put! value-ht sym ans)
		ans))]
	   [hash-table-maps?
	    (lambda (ht value)
	      (let/ec k
		(hash-table-get ht value (lambda () (k #f)))
		#t))])
      (if (hash-table-maps? value-ht sym)
	  (let* ([secs (hash-table-get mods-ht sym)]
		 [reason (ormap (lambda (fn)
				  (if (< (hash-table-get mods-ht (string->symbol fn))
					 (file-or-directory-modify-seconds fn))
				      fn
				      #f))
				(cons filename (hash-table-get file-ht sym (lambda () null))))])
	    (if reason
		(load/save filename (format "~a was modified" reason))
		(hash-table-get value-ht sym)))
	  (load/save filename "never before loaded")))))

(define-macro require-relative-library
  (lambda (filename)
    `(let ([require-relative-collection (current-require-relative-collection)])
       (unless require-relative-collection
	 (error 'require-relative-library "no collection~n"))
       ((global-defined-value 'check-require/proc)
	(build-path
	 (apply collection-path require-relative-collection)
	 ,filename)))))

(define-macro require-library
  (lambda (filename . collections)
    `(let ([g (list ,@collections)]
	   [f ,filename])
       (let ([h (if (null? g)
		    (list "mzlib")
		    g)])
	 (parameterize ([current-require-relative-collection h])
	   ((global-defined-value 'check-require/proc)
	    (build-path
	     (apply collection-path h)
	     f)))))))


(define debug? #t)

(define drscheme-custodian #f)

(load-relative "start-drs.ss")

(define (T)
  (when drscheme-custodian (custodian-shutdown-all drscheme-custodian))
  (set! drscheme-custodian (make-custodian))
  (parameterize ([current-custodian drscheme-custodian]
		 [current-eventspace (make-eventspace)])
    (start-drscheme)))

(define start-drscheme-expression '(T))

(if debug?
    (begin
      (thread
       (lambda ()
	 (let* ([f (let loop ([n 10])
		     (cond
		      [(get-top-level-focus-window) => (lambda (x) x)]
		      [(zero? n) (error 'drscheme.ss "didn't find frame after 5 seconds")]
		      [else
		       (sleep/yield 1/2)
		       (loop (- n 1))]))]
		[canvas
		 (let loop ([f f])
		   (cond
		    [(is-a? f editor-canvas%) f]
		    [(is-a? f area-container<%>) (ormap loop (send f get-children))]
		    [else (error 'drscheme.ss "couldn't find editor")]))]
		[text (send canvas get-editor)]
		[send-sexp
		 (lambda (sexp)
		   (let ([port (open-output-string)]
			 [event (make-object key-event%)])
		     (write sexp port)
		     (send text insert (get-output-string port))
		     (send event set-key-code #\return)
		     (send text on-char event)
		     (sleep 1/2)))])
	   (send-sexp start-drscheme-expression))))

      (graphical-read-eval-print-loop))
    (eval start-drscheme-expression))
|#