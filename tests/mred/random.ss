
(require-library "core.ss")

(define example-list%
  (class '() (name-in parents [filter (lambda (x) (not (void? x)))])
      (public
       [name name-in]
       [items '()]
       [num-items 0]
       
       [parents-count 
	(if parents
	    (map (lambda (parent)
		   (ivar parent count))
		 parents)
	    '())]
       [parents-choose
	(if parents
	    (map (lambda (parent)
		   (ivar parent choose-example))
		 parents)
	    '())]
       [choose-parent-example
	(lambda (which)
	  (let loop ([pos which][counts parents-count][chooses parents-choose])
	    (if (null? counts)
		(void)
		(let ([c ((car counts))])
		  (if (< pos c)
		      ((car chooses) pos)
		      (loop (- pos c) (cdr counts) (cdr chooses)))))))]

       [count 
	(lambda () (+ num-items (apply + (map (lambda (x) (x)) parents-count))))]
       [set-filter
	(lambda (f)
	  (set! filter f))]
       [add
	(lambda (x)
	  (when (filter x)
		(set! num-items (add1 num-items))
		(set! items (cons x items))))]
       [all-examples
	(lambda ()
	  (apply append items (map (lambda (p) (send p all-examples)) parents)))]
       [choose-example
	(opt-lambda ([which #f])
	  (let ([n (if which 
		       which
		       (let ([c (count)])
			 (if (zero? c)
			     0
			     (random c))))])
	    (if (< n num-items)
		(list-ref items n)
		(choose-parent-example (- n num-items)))))])))

(define boxed-example-list%
  (class () (parent)
    (public
     [name `(boxed ,(ivar parent name))]
     [all-examples
      (lambda ()
	(let ([l (map box (send parent all-examples))])
	  l))]
     [choose-example
      (opt-lambda ([which #f])
	(let ([ex (send parent choose-example)])
	  (if (void? ex)
	      (void)
	      (box ex))))])))

(define optional-example-list%
  (class () (parent)
    (public
     [name `(optional ,(ivar parent name))]
     [all-examples
      (lambda ()
	(let ([l (map box (send parent all-examples))])
	  (cons #f l)))]
     [add
      (lambda (x)
	(and x (send parent add x)))]
     [choose-example
      (opt-lambda ([which #f])
	(if (zero? (random 2))
	    #f
	    (send parent choose-example)))])))

(define choose-example-list%
  (class () (parents)
    (public
     [name `(choose ,(map (lambda (p) (ivar p name)) parents))]
     [all-examples
      (lambda ()
	(apply append (map (lambda (p) (send p all-examples)) parents)))]
     [add void]
     [choose-example
      (opt-lambda ([which #f])
	(send (list-ref parents (random (add1 (length parents))))
	      choose-example which))])))

(define unknown-example-list%
  (class () ()
    (public
     [name 'unknown]
     [all-examples (lambda () null)]
     [add void]
     [choose-example
      (opt-lambda ([which #f])
	(error 'choose-example "can't choose unknown example"))])))

(define discrete-example-list%
  (class () (vals)
    (public
     [name `(one-of ,@vals)]
     [all-examples (lambda () vals)]
     [add (lambda (x) (unless (member x vals)
			(error '|add in discrete-example-list|
				 "no good: ~a" x)))]
     [choose-example
      (opt-lambda ([which #f])
	(list-ref vals (random (add1 (length vals)))))])))

(define-struct (fatal-exn struct:exn) ())

(define (fatal-error name str . args)
  (raise (make-fatal-exn (apply format (string-append "~a: " str) name args)
			 ((debug-info-handler)))))

(define trying-class #f)
(define trying-method #f)

(define null-results null)

(define-macro define-main 
  (lambda list
    (let loop ([l list][rest '()])
      (if (null? l)
	  (cons 'begin rest)
	  (loop (cdr l)
		(let* ([first (car l)]
		       [name (if (symbol? first)
				 first
				 (car first))]
		       [strname (symbol->string name)]
		       [bases (if (symbol? first)
				  ()
				  (cdr first))]
		       [el-name (lambda (s)
				  (if s
				      (string->symbol
				       (string-append
					(symbol->string s)
					"-example-list"))
				      #f))])
		  (cons
		   `(define ,(el-name name)
		      (make-object example-list% 
				   ',name
				   (list ,@(map el-name bases))
				   (lambda (v) (when (null? v)
						 (set! null-results (cons (list trying-class trying-method ',name)
									  null-results))
						 (error ',name "got null")))))
		   rest)))))))

(define-main
  void
  char
  ubyte
  integer
  nonnegative-integer
  symbol
  real
  string
  string-list
  boolean
  procedure
  value
  eventspace
  0-to-255
  0-to-10000
  -10000-to-10000

  void*
  istream%
  ostream%

  (area<%> window<%> subarea<%> area-container<%>)

  (subarea<%> subwindow<%> pane%)

  (window<%> subwindow<%> area-container-window<%>)

  (area-container<%> area-container-window<%> pane%)

  (subwindow<%> control<%> canvas<%> panel%)

  (area-container-window<%> top-level-window<%> panel%)

  (control<%> message% button% check-box% slider% gauge% text-field% radio-box% list-control<%>)

  (list-control<%> choice% list-box%)

  (top-level-window<%> frame% dialog%)

  (pane% horizontal-pane% vertical-pane%)
  
  (panel% horizontal-panel% vertical-panel%)

  (canvas<%> canvas% editor-canvas%)

  message%
  button%
  check-box%
  slider%
  gauge%
  text-field%
  radio-box%

  choice%
  list-box%

  canvas%
  editor-canvas%

  horizontal-pane%
  vertical-pane%

  horizontal-panel%
  vertical-panel%

  frame%
  dialog%

  point%

  ps-setup%

  color%
  font%
  brush%
  pen%

  font-list%
  pen-list%
  brush-list%
  color-database%
  font-name-directory%

  color-map%
  cursor%
  icon%
  bitmap%

  (event% control-event% scroll-event% mouse-event% key-event%)
  control-event%
  scroll-event%
  mouse-event%
  key-event%

  (dc<%> canvas-dc% post-script-dc%)
  (canvas-dc%  memory-dc%)
  memory-dc%
  post-script-dc%

  printer-dc%
  meta-file-dc%
  
  meta-file%


  (item% button% checkbox% choice%
	   listbox% slider% sgauge% text% multitext% 
	   radiobox% message% groupbox%)
  button%
  checkbox%
  choice%
  listbox%
  slider%
  sgauge%
  text%
  multitext%
  message%
  radiobox%
  groupbox%

  (menu-item-container<%> menu% menu-bar% popup-menu%)

  menu%
  popup-menu%
  menu-bar%
  
  (menu-item<%> separator-menu-item%  labelled-menu-item<%>)
  (labelled-menu-item<%> shortcut-menu-item<%> submenu-item<%>)
  (shortcut-menu-item<%> menu-item% checkable-menu-item%)
  submenu-item<%>
  separator-menu-item%
  menu-item%
  checkable-menu-item%

  timer%

  add-color%
  mult-color%
  style-delta%
  style%
  style-list%

  (editor-admin%  editor-snip-editor-admin%)
  editor-snip-editor-admin%
  snip-admin%
  
  (editor<%> text% pasteboard%)
  text%
  pasteboard%

  (snip% text-snip% image-snip% editor-snip%)
  (text-snip% tab-snip%)
  tab-snip%
  image-snip%
  editor-snip%

  snip-class%
  snip-class-list%

  editor-data%
  editor-data-class%
  editor-data-class-list%

  keymap%
  editor-wordbreak-map%

  (editor-stream-in-base% editor-stream-in-string-base%)
  (editor-stream-out-base% editor-stream-out-string-base%)

  editor-stream-in-string-base%
  editor-stream-out-string-base%

  editor-stream-in%
  editor-stream-out%
  
  clipboard%
  clipboard-client%)

(send bitmap%-example-list set-filter (lambda (bm) (send bm ok?)))

(define false-example-list (make-object example-list% 'false '()))
(send false-example-list add #f)

(define empty-list-example-list (make-object example-list% 'empty-list '()))
(send empty-list-example-list add null)

(send* boolean-example-list
       (add #t)
       (add #f))

(send* integer-example-list
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0) (add 0)
       (add -1)
       (add -2)
       (add -3)
       (add -1000)
       (add 1)
       (add 2)
       (add 3)
       (add 4)
       (add 5)
       (add 6)
       (add 7)
       (add 8)
       (add 9)
       (add 10)
       (add 16)
       (add 32)
       (add 64)
       (add 128)
       (add 256)
       (add 255)
       (add 1023)
       (add 1000)
       (add 5.0))

(send* 0-to-255-example-list
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 0) (add 0) (add 0)
       (add 1)
       (add 2)
       (add 3)
       (add 4)
       (add 5)
       (add 6)
       (add 7)
       (add 8)
       (add 9)
       (add 10)
       (add 16)
       (add 32)
       (add 64)
       (add 128)
       (add 255)
       (add 5))

(send* char-example-list
       (add #\nul)
       (add #\a)
       (add #\1)
       (add #\newline)
       (add #\tab)
       (add #\z)
       (add #\C))

(send* real-example-list
       (add 0.)
       (add 0.)
       (add 0.)
       (add -1.)
       (add -2.)
       (add -3.)
       (add -1000.)
       (add 1.)
       (add 2.)
       (add 3.)
       (add 1000.)
       (add 5))

(send* string-example-list
       (add "")
       (add "hello")
       (add "system/mred.xbm")
       (add "system/mred.bmp")
       (add "mred.gif")
       (add "goodbye adious see you later zai jian seeya bye-bye"))

(send procedure-example-list add void)

(define classinfo (make-hash-table))

(define (add-all-combinations example-list items)
  (for-each
   (lambda (i) (send example-list add i))
   (let loop ([items items])
     (cond
      [(null? (cdr items)) items]
      [else (let ([l (loop (cdr items))])
	      (append
	       (map (lambda (x) (bitwise-ior (car items) x)) l)
	       l))]))))

(define unknown-example-list (make-object unknown-example-list%))

(define (optional l) (make-object optional-example-list% l))
(define (boxed l) (make-object boxed-example-list% l))
(define (unknown s) unknown-example-list)
(define (choice . l) (make-object choose-example-list% l))
(define (style-list . l) (make-object discrete-example-list% l))

(load-relative "windowing-classes.ss")
(load-relative "drawing-classes.ss")
(load-relative "editor-classes.ss")

(define (get-args l)
  (let/ec bad
	  (let loop ([l l])
	    (if (null? l)
		'()
		(let* ([source (car l)]
		       [value (send source choose-example #f)])
		  (if (void? value)
		      (bad #f)
		      (cons value (loop (cdr l)))))))))

(define (get-all-args l)
  (let loop ([l l])
    (if (null? l)
	'()
	(let* ([source (car l)]
	       [values (send source all-examples)]
	       [rest (loop (cdr l))])
	  (if (null? (cdr l))
	      (list values)
	      (apply append
		     (map (lambda (other)
			    (map (lambda (v) (cons v other)) values))
			  rest)))))))

(define thread-output-port current-output-port)

(define (apply-args v dest name k)
  (if v
      (begin
	(fprintf (thread-output-port) "~a: ~a" name v)
	(flush-output (thread-output-port))
	(with-handlers (((lambda (x) (not (fatal-exn? x)))
			 (lambda (x)
			   (fprintf (thread-output-port)
				    ": error: ~a~n"
				    (exn-message x)))))
	  (send dest add (k v))
	  (:flush-display)
	  (fprintf (thread-output-port) ": success~n")))
      (fprintf (thread-output-port) "~a: failure~n" name)))

(define (try-args arg-types dest name k)
  (apply-args (get-args arg-types) dest name k))

(define (try-all-args arg-types dest name k)
  (let ([vs (get-all-args arg-types)])
    (map (lambda (v)
	   (apply-args v dest name k))
	 vs)))

(define (create-some cls try)
  (when (class? cls)
	(let* ([v (hash-table-get classinfo cls)]
	       [dest (car v)]
	       [name (cadr v)]
	       [creators (caddr v)])
	  (let loop ([l creators])
	    (unless (null? l)
		    (try (car l) dest name
			 (lambda (v)
			   (apply make-object cls v)))
		    (loop (cdr l)))))))

(define (create-all-random)
  (fprintf (thread-output-port) "creating all randomly...~n")
  (hash-table-for-each classinfo (lambda (k v)
				   (create-some k try-args))))
(define (create-all-exhaust)
  (fprintf (thread-output-port) "creating all exhaustively...~n")
  (hash-table-for-each classinfo (lambda (k v)
				   (create-some k try-all-args))))

(define (try-methods cls try)
  (let* ([v (hash-table-get classinfo cls)]
	 [source (car v)]
	 [use (if source (send source choose-example) #f)]
	 [name (cadr v)]
	 [methods (cdddr v)])
    (if (void? use)
	(fprintf (thread-output-port) "~s: no examples~n" name)
	(let loop ([l methods])
	  (unless (null? l)
		  (let* ([method (car l)]
			 [iv (car method)]
			 [resulttype (cadr method)]
			 [argtypes (cddr method)])
		    (set! trying-class (and source (ivar source name)))
		    (set! trying-method iv)
		    (try argtypes resulttype (list name iv use)
			 (lambda (args)
			   (if use
			       (apply (ivar/proc use iv) args)
			       (apply (global-defined-value iv) args)))))
		  (loop (cdr l)))))))

(define (call-random except)
  (fprintf (thread-output-port) "calling all except ~a randomly...~n" except)
  (hash-table-for-each classinfo (lambda (k v)
				   (unless (member k except)
					   (try-methods k try-args)))))

(define (call-all-random)
  (call-random null))

(define (call-all-non-editor)
  (call-random (list :editor-buffer% :editor-edit% :editor-snip% :editor-pasteboard% 'EditorGlobal)))

(define (init)
  (create-all-random)
  (create-all-random)
  (create-all-random)
  (create-all-random))

(with-handlers ([void void])
  (load-relative-extension "classhack.so"))

(printf "Random loaded~n")

(printf " Checking all methods~n")
(define in-top-level null)
(hash-table-for-each classinfo 
		     (lambda (key v)
		       (let* ([methods (cdddr v)]
			      [names (map car methods)])
			 (if (string? key)
			     ;; Check global procs
			     (for-each
			      (lambda (name)
				(unless (procedure? (with-handlers ([void void])
						      (global-defined-value name)))
				  (printf "No such procedure: ~a~n" name))
				(set! in-top-level (cons name in-top-level)))
			      names)
			     ;; Check intf/class methods
			     (begin
			       (set! in-top-level (cons (cadr v) in-top-level))
			       ; Check printed form:
			       (let ([p (open-output-string)])
				 (display key p)
				 (let ([sp (get-output-string p)]
				       [ss (let ([s (symbol->string (cadr v))])
					     (format "#<~a:~a>"
						     (if (interface? key) "interface" "class")
						     s))])
				   (unless (string=? sp ss)
				     (printf "bad printed form: ~a != ~a~n" sp ss))))
			       ; Check documented are right
			       (for-each
				(lambda (name)
				  (unless (or (and (interface? key)
						   (ivar-in-interface? name key))
					      (and (class? key)
						   (ivar-in-class? name key)))
				    (printf "No such method: ~a in ~a~n" name key)))
				names)
			       ; Check everything is documented
			       (when (procedure? (with-handlers ([void void]) (global-defined-value 'class->names)))
				 (for-each
				  (lambda (n)
				    (unless (memq n names)
				      (printf "Undocumented method: ~a in ~a~n" n key)))
				  ((if (interface? key) interface->names class->names) key))))))))
(printf " Method-checking done~n")

(let* ([get-all (lambda (n)
		  (parameterize ([current-namespace n])
		    (map car (make-global-value-list))))]
       [expect-n (list* 'mred@ 'mred^ (append (get-all (make-namespace)) in-top-level))]
       [actual-n (get-all (make-namespace 'mred))])
  (for-each
   (lambda (i)
     (unless (memq i expect-n)
       (printf "Undocumented global: ~a~n" i)))
   actual-n))

  