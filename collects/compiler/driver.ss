;; Compiler driver routines 
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

;; Scheme->C compilation Overview
;; ------------------------------
;;
;; Compilation is performed in a number of phases,
;;  each of which is implemented in its own unit:
;;
;;   1) Reading/parsing - Zodiac collection
;;   2) SBA analysis (optional) - MrSpidey collection
;;   3) Prephase - prephase.ss
;;   4) A-normalization - anorm.ss
;;   5) Known-value analysis - known.ss
;;   6) Lexical analysis and inlining - analyze.ss
;;   7) Closure conversion - closure.ss
;;   8) Closure vechicle assignment - vehicle.ss
;;   9) Representation choosing - rep.ss
;;  10) Scheme to virtual machine translation - vmphase.ss
;;  11) Optimizations on VM code - vmopt.ss
;;  12) VM to C translation - vm2c.ss
;;
;; For more information about a phase, see the file
;;  implementing that phase.
;;
;; All steps up to 10 work on a Scheme program, representated
;;  as a zodiac AST. The AST produced by zodiac is destructively
;;  modified by each phase (usually); mzc-specific information is
;;  stored in the AST as ``annotations''. At the implementation file
;;  for each phase, the annotations installed or changed by the phase
;;  are listed.
;;
;; C code is compiled and linked via procedures provided by the
;;  dynext collection.
;;
;; In this implementation, `var' is used for variable names in
;;  confusing and inconsistent ways. There are two different
;;  AST entities that could be called "var":
;;     1) binding instances of variables, e.g., the formal
;;        arguments of a lambda; these are always called
;;        `bindings' in Zodiac terminology
;;     2) bound instances of variables, e.g., a free variable 
;;        in a sub-expression; these are always called
;;        `varrefs' in Zodiac terminology.
;;  Almost all information about a variable is stored with a
;;   zodiac:binding AST node, and very little information is
;;   stored with a zodiac:varref AST node.
;;  To make matters worse, the name `binding' is overloaded.
;;    `zodiac:binding' is the name of a Zodiac structure
;;    type, and `binding' is also the name of the structure
;;    type for annotations attached to zodiac:binding objects.


(unit/sig compiler:driver^
  (import (compiler:option : compiler:option^)
	  compiler:library^
	  compiler:cstructs^
	  (zodiac : zodiac:system^)
	  compiler:zlayer^
	  compiler:prephase^
	  compiler:anorm^
	  compiler:known^
	  compiler:analyze^
	  compiler:const^
	  compiler:closure^
	  compiler:vehicle^
	  compiler:rep^
	  compiler:vmstructs^
	  compiler:vmphase^
	  compiler:vmopt^
	  compiler:vm2c^
	  compiler:top-level^
	  dynext:compile^
	  dynext:link^
	  dynext:file^
	  mzlib:function^
	  mzlib:pretty-print^
	  (mrspidey : compiler:mrspidey^))
  (rename (compile-extension* compile-extension))
  
  (define debug:file "dump.txt")
  (define debug:port #f)
  (define debug
    (lambda x
      (when (and (compiler:option:debug) debug:port)
	(apply fprintf (cons debug:port x))
	(flush-output debug:port))))
  
  ;;----------------------------------------------------------------------------
  ;; FILE PROCESSING FUNCTIONS
  ;;
  
  ; takes an input-name from the compile command and returns 4 values
  ; 1) an input path
  ; 2) a C output path
  ; 3) a constant pool output path
  ; 4) an obj output path
  ; 5) a dll output path
  ; 6) a scheme_setup suffix
  (define s:process-filenames
    (lambda (input-name dest-dir c?)
      (let-values ([(basedir file dir?) (split-path input-name)])
	(let* ([sbase (extract-base-filename/ss file (if c? #f 'mzc))]
	       [cbase (extract-base-filename/c file (if c? 'mzc #f))]
	       [base (or sbase cbase)])
	  (unless base
	    (error 'mzc "not a Scheme or C file: ~a" input-name))
	  (values (if sbase input-name #f)
		  (if cbase input-name (build-path dest-dir (append-c-suffix base)))
		  (build-path dest-dir (append-constant-pool-suffix base))
		  (build-path dest-dir (append-object-suffix base))
		  (build-path dest-dir (append-extension-suffix base))
		  (string-append (compiler:clean-string (compiler:option:setup-prefix)) "_" (compiler:clean-string base)))))))

  (define elaboration-exn-handler
    (lambda (exn)
      (compiler:fatal-error 
       #f
       (format "Error at elaboration time: ~a" 
	       (if (exn? exn)
		   (exn-message exn)
		   exn)))
      (raise exn)))

  (define top-level-exn-handler
    (lambda (exn)
      (set! compiler:messages (reverse! compiler:messages))
      (compiler:report-messages! #t)
      (exit -1)))

  (define (is-mzlib-library? path lib)
    (and (regexp-match lib path)
	 (string=? (build-path (collection-path "mzlib") lib) path)))

  (define load-prefix-file
    (lambda (prefix)
      (if (or (is-mzlib-library? prefix "refer.ss")
	      (is-mzlib-library? prefix "macrox.ss"))
	  (begin
	    (printf " Prefix: skipping \"~a\"~n" prefix)
	    (void))
	  (begin
	    (printf " Prefix: loading \"~a\"~n" prefix)
	    (let-values ([(base file dir?) (split-path prefix)]
			 ; [(start) (current-process-milliseconds)]
			 [(p) (open-input-file prefix 'text)])
	      (let* ([results
		      (s:expand-top-level-expressions! 
		       (path->complete-path
			(if (eq? 'relative base)
			    (build-path 'same)
			    base))
		       (zodiac:read p (zodiac:make-location 1 1 0 prefix))
		       #f
		       #f
		       (lambda (expr)
			 (if (void? expr)
			     expr
			     (let ([r (zodiac:parsed->raw expr)])
			       (call-with-values
				(lambda () 
				  (eval r))
				list)))))])
		(close-input-port p)
		; (printf "~a~n" (- (current-process-milliseconds) start))
		(apply values (car (last-pair results)))))))))

  (define s:expand-top-level-expressions!
    (lambda (input-directory reader vocab verbose? r-eval)
      (when verbose? (printf "~n Zodiac: reading... ") (flush-output))
      ; During reads, syntax errors are truly fatal
      (let ([exprs (begin ; time
		    (let loop ([n 1])
		      (let ([sexp (let loop () 
				    ; This loop handles the case where
				    ; the reader is really unhappy after
				    ; it encounters an error
				    (with-handlers ([void (lambda (x) (loop))])
				      (reader)))])
			(if (zodiac:eof? sexp)
			    null
			    (begin 
			      (when (compiler:option:debug)
				(debug "~a[~a.~a]_"
				       n
				       (zodiac:location-line
					(zodiac:zodiac-start sexp))
				       (zodiac:location-column
					(zodiac:zodiac-start sexp))))
			      (cons sexp (loop (+ n 1))))))))])
	(unless (null? compiler:messages) (when (compiler:option:verbose) (newline)))
	(compiler:report-messages! #t)
	(when verbose? (printf " expanding...~n"))
	(parameterize ([current-load-relative-directory input-directory]
		       [current-load load-prefix-file])
	  (begin ; time
	   (map r-eval (call/nal zodiac:scheme-expand-program/nal
				 zodiac:scheme-expand-program
				 (vocabulary:  vocab)
				 (expressions: exprs)
				 (elaboration-evaluator: (lambda (expr p->r phase)
							   (if (void? expr)
							       (void)
							       (eval (p->r expr)))))
				 (user-macro-body-evaluator: 
				  (lambda (f . args) (apply f args))))))))))

  (define elaborate-namespace (make-namespace))

  (define (eval-compile-prefix prefix)
    (with-handlers ([void top-level-exn-handler])
      (with-handlers ([void elaboration-exn-handler])
	(parameterize ([current-namespace elaborate-namespace]
		       [require-library-use-compiled #f]
		       [compiler:escape-on-error #t]
		       [current-load load-prefix-file])
	  (eval prefix))
	(compiler:report-messages! #t))))

  ;;----------------------------------------------------------------------
  ;; Misc utils

  ; takes a list of a-normalized expressions and analyzes them
  ; returns the analyzed code, a list of local variable lists, 
  ; used variable lists, and captured variable lists
  (define s:analyze-source-list
    (lambda (source)
      (let loop ([sexps source] [source-acc null] 
				[locals-acc null] [globals-acc null] [used-acc null] [captured-acc null]
				[children-acc null]
				[max-arity 0])
	(if (null? sexps)
	    (values (reverse! source-acc)
		    (map (lambda (loc glob used cap children)
			   (make-code empty-set
				      loc
				      glob
				      used
				      cap
				      #f
				      #f
				      children))
			 (reverse! locals-acc)
			 (reverse! globals-acc)
			 (reverse! used-acc)
			 (reverse! captured-acc)
			 (reverse! children-acc))
		    max-arity)
	    (let-values ([(exp free-vars local-vars global-vars used-vars captured-vars children new-max-arity multi)
			  (analyze-expression! (car sexps) empty-set (null? (cdr sexps)))])
	      (loop (cdr sexps) 
		    (cons exp source-acc) 
		    (cons local-vars locals-acc)
		    (cons global-vars globals-acc)
		    (cons used-vars used-acc)
		    (cons captured-vars captured-acc)
		    (cons children children-acc)
		    (max max-arity new-max-arity)))))))

  (define s:append-block-sources!
    (lambda (file-block l)
      (set-block-codes!
       file-block
       (append!
	(map
	 (lambda (glob)
	   (make-code empty-set
		      empty-set
		      empty-set
		      empty-set
		      empty-set
		      #f #f null))
	 l)
	(block-codes file-block)))
      (set-block-source! 
       file-block
       (append! l (block-source file-block)))))

  ;;-------------------------------------------------------------------------------
  ;; MrSpidey Per-unit analysis
  (define (wrap-unit-for-mrspidey expr)
    ;; Wrap the unit in an expression that drives to unknown all of the imports
    ;;  and arguments to exported functions.
    (let* ([imports (zodiac:unit-form-imports expr)]
	   [exports (map zodiac:read-object (map cdr (zodiac:unit-form-exports expr)))]
	   [dummies (map (lambda (x) (gensym)) imports)]	   
	   [my-expr `(let ([u u] 
			   ,@(map (lambda (d) `(,d (#%read))) dummies))
		       (invoke-unit
			(compound-unit 
			 (import ,@dummies)
			 (link [U (u ,@dummies)]
			       [O ((unit (import ,@exports)
					 (export)
					 ,@(map (lambda (x) `(#%apply ,x (#%read))) exports))
				   (U ,@exports))])
			 (export))
			,@dummies))]
	   [wrapper (let-values ([(in out) (make-pipe)])
		      (write my-expr out)
		      (close-output-port out)
		      (car (s:expand-top-level-expressions!
			    (current-directory)
			    (zodiac:read in (zodiac:make-location 1 1 0 "wrapper"))
			    #f
			    #f
			    identity)))])
      (set-car!
       (zodiac:let-values-form-vals wrapper)
       expr)
      wrapper))

  (define (unwrap-unit-from-mrspidey expr)
    (car (zodiac:let-values-form-vals expr)))

  (define (analyze-units expr)
    (cond
     [(zodiac:unit-form? expr)
      (unwrap-unit-from-mrspidey
       (car (mrspidey:analyze-program-sexps 
	     (list (wrap-unit-for-mrspidey expr))
	     (current-directory))))]
     [(zodiac:app? expr)
      (let* ([exprs (cons (zodiac:app-fun expr) (zodiac:app-args expr))]
	     [exprs2 (map analyze-units exprs)])
	(unless (andmap eq? exprs exprs2)
	  (zodiac:set-app-fun! expr (car exprs2))
	  (zodiac:set-app-args! expr (cdr exprs2)))
	expr)]
     [else expr]))

  ;;-------------------------------------------------------------------------------
  ;; ERROR/WARNING REPORTING/HANDLING ROUTINES
  ;;
  (define compiler:messages null)
  (define compiler:make-message
    (lambda (constructor)
      (lambda (ast message)
	(set! compiler:messages (cons (constructor ast message)
				      compiler:messages)))))
  (define compiler:error (compiler:make-message make-compiler:fatal-error))
  (define compiler:fatal-error compiler:error)
  (define compiler:internal-error
    (case-lambda
     [(ast message)
      (set! compiler:messages 
	    (reverse! (cons (make-compiler:internal-error ast message)
			    compiler:messages)))
      (compiler:report-messages! #t)]
     [(ast fmt . args)
      (compiler:internal-error ast (apply format fmt args))]))

  (define compiler:warning (compiler:make-message make-compiler:warning))

  (define compiler:report-messages!
    (lambda (stop-on-errors?)
      (let ([error-count 0]
	    [fatal-error-count 0]
	    [msgs (reverse! compiler:messages)])
	(set! compiler:messages null)
	(for-each (lambda (message)
		    (when (compiler:error? message) 
		      (set! error-count (add1 error-count)))
		    (when (or (compiler:fatal-error? message) 
			      (compiler:internal-error? message))
		      (set! fatal-error-count (add1 fatal-error-count)))

		    (let* ([ast (compiler:message-ast message)]
			   [string (compiler:message-message message)])
		      (zodiac:print-start! (current-output-port) ast)
		      (printf 
		       "~a: ~a~n"
		       (cond
			[(compiler:error? message) "Error"]
			[(compiler:warning? message) "Warning"]
			[(compiler:fatal-error? message) "Error"]
			[(compiler:internal-error? message) "INTERNAL ERROR"]
			[else (error 'report-messages "internal error")])
		       string)
		      (when (compiler:internal-error? message)
			(printf 
			 (string-append
			  " please report the bug at http://www.cs.rice.edu/CS/PLT/Bugs/~n"
			  "  (or, as a last resort, send mail to plt-bugs@cs.rice.edu)~n"
			  "  and include a transcript in verbose mode~n")))))
		  
		  msgs)
	(when (and stop-on-errors?
		   (or (positive? error-count)
		       (positive? fatal-error-count)))
	  (error "Errors encountered.  Compilation aborted.")))))

  (define total-cpu-time 0)
  (define total-real-time 0)
  (define verbose-time
    (lambda (thunk)
      (let-values ([(vals cpu real) (time-apply thunk)])
	(set! total-cpu-time (+ total-cpu-time cpu))
	(set! total-real-time (+ total-real-time real))
	(when (compiler:option:verbose)
	  (printf "      [cpu: ~ams, real: ~ams]~n" cpu real))
	(apply values vals))))

  ;;-----------------------------------------------------------------------------
  ;; File-level Block information

  (define s:file-block (make-empty-block))
  (define s:max-arity 0) ; compilation-wide max
  (define s:register-max-arity!
    (lambda (n) (set! s:max-arity (max s:max-arity n))))

  (define number-of-true-constants 0)

  (define s:unit-list null) ; list of units in the code

  (define compiler:setup-suffix "")

  ;;-----------------------------------------------------------------------------
  ;; THE MAIN DRIVING ROUTINE

  (define (compile-extension* input-name dest-directory)
    (s:compile #f #f #f input-name dest-directory))
  (define (compile-extension-to-c input-name dest-directory)
    (s:compile #t #f #f input-name dest-directory))
  (define (compile-c-extension input-name dest-directory)
    (s:compile #f #f #t input-name dest-directory))

  (define (compile-extension-part input-name dest-directory)
    (s:compile #f #t #f input-name dest-directory))
  (define (compile-extension-part-to-c input-name dest-directory)
    (s:compile #t #t #f input-name dest-directory))
  (define (compile-c-extension-part input-name dest-directory)
    (s:compile #f #t #t input-name dest-directory))

  (define compiler:multi-o-constant-pool? #f)

  (define s:compile
    (lambda (c-only? multi-o? c? input-name dest-directory)
      (define input-directory 
	(let-values ([(base file dir?)
		      (split-path (path->complete-path input-name))])
	  base))
      (set! compiler:multi-o-constant-pool? multi-o?)
      (set! s:file-block (make-empty-block))
      (set! s:max-arity 0)
      (set! total-cpu-time 0)
      (set! total-real-time 0)
      (random-seed (compiler:option:seed))
      (set! compiler:messages null)
      (const:init-tables!)
      ; process the input string - try to open the input file
      (let-values ([(input-path c-output-path constant-pool-output-path obj-output-path dll-output-path setup-suffix)
		    (s:process-filenames input-name dest-directory c?)])
	(unless (or (not input-path) (file-exists? input-path))
	  (error 's:compile "could not open ~a for input" input-path))
	(set! compiler:setup-suffix
	      (if multi-o?
		  setup-suffix
		  ""))

	(for-each (lambda (path)
		    (when (file-exists? path) (delete-file path)))
		  (list (if input-path c-output-path obj-output-path) 
			(if input-path constant-pool-output-path obj-output-path) 
			obj-output-path dll-output-path))
	
	(when (compiler:option:debug)
	  (when (file-exists? debug:file) (delete-file debug:file))
	  (set! debug:port (open-output-file debug:file 'text)))
	
	(when input-path 
	  (parameterize ([main-source-file input-path])
	    (let ([input-port (open-input-file input-path 'text)])
	      
	      ;;-----------------------------------------------------------------------
	      ;; read all top-level s-expressions
	      ;;
	      
	      (printf "\"~a\": " input-path)
	      (unless (compiler:option:verbose) (newline))
	      (let ([read-thunk
		     (lambda ()
		       (with-handlers ([void top-level-exn-handler])
			 (with-handlers ([void elaboration-exn-handler])
			   (parameterize ([current-namespace elaborate-namespace]
					  [require-library-use-compiled #f]
					  [compiler:escape-on-error #t])
			     (set-block-source! 
			      s:file-block
			      (s:expand-top-level-expressions! 
			       input-directory
			       (zodiac:read input-port 
					    (zodiac:make-location 1 1 0 input-path))
			       (if (compiler:option:use-mrspidey)
				   zodiac:mrspidey-vocabulary
				   #f)
			       (compiler:option:verbose)
			       identity))))))])
		(verbose-time read-thunk)	
		(close-input-port input-port)
		(set! input-port #f)
		
		(compiler:report-messages! #t))
	
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; MrSpidey analyze
	      
	      ; -- whole-program analysis? ----------------------------------------
	      (when (compiler:option:use-mrspidey)
		(when (compiler:option:verbose) (printf " MrSpidey: analyzing~n"))

		(set-block-source! 
		 s:file-block
		 (with-handlers ([void (lambda (x)
					 (compiler:fatal-error
					  #f
					  (format "analysis died: ~a"
						  (if (exn? x)
						      (exn-message x)
						      x))))])
		   (mrspidey:analyze-program-sexps (block-source s:file-block)
						   input-directory)))
	      
		(compiler:report-messages! #t))

	      ; -- per-unit analysis? ----------------------------------------
	      (when (compiler:option:use-mrspidey-for-units)
		(when (compiler:option:verbose) (printf " MrSpidey: analyzing units~n"))

		(set-block-source! 
		 s:file-block
		 (with-handlers ([void (lambda (x)
					 (compiler:fatal-error
					  #f
					  (format "analysis died: ~a"
						  (if (exn? x)
						      (exn-message x)
						      x))))])
		   (map analyze-units (block-source s:file-block))))
		
		(compiler:report-messages! #t))

	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))
	      
	      ;;-----------------------------------------------------------------------
	      ;; Run a preprocessing phase on the input
	      ;;
	      
	      (when (compiler:option:verbose) (printf " pre-processing and scanning for errors~n"))
	      (when (compiler:option:debug) (debug " = PREPHASE =~n"))
	      
	      (let ([prephase-thunk 
		     (lambda () 
		       (set-block-source! 
			s:file-block
			(let loop ([source (block-source s:file-block)]
				   [errors compiler:messages])
			  (if (null? source)
			      source
			      (let ([ast (prephase! (car source) #f (pair? (cdr source)) #f)])
				(if (eq? errors compiler:messages)
				    ; no errors here
				    (cons ast (loop (cdr source) errors))
				    ; error, drop this one
				    (loop (cdr source) compiler:messages)))))))])
		(verbose-time prephase-thunk))
	      (compiler:report-messages! (not (compiler:option:test)))
	      (when (compiler:option:test)
		(printf "skipping over top-level expressions with errors...~n"))
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))
	      
	      ;;-----------------------------------------------------------------------
	      ;; A-normalize input
	      ;;
	      
	      (when (compiler:option:verbose) (printf " transforming to a-normal form~n"))
	      (when (compiler:option:debug) (debug " = ANORM =~n"))
	      
	      (let ([anorm-thunk
		     (lambda ()
		       (set-block-source! 
			s:file-block 
			(map (lambda (s) (a-normalize s identity)) 
			     (block-source s:file-block))))])
		(verbose-time anorm-thunk))
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; known-value analysis
	      ;;

	      (when (compiler:option:verbose) 
		(printf " determining known bindings~n"))
	      (when (compiler:option:debug)
		(debug " = KNOWN =~n"))

	      ; analyze top level expressions
	      (let ([known-thunk
		     (lambda ()
		       (set-block-source! 
			s:file-block 
			(map (lambda (s) (analyze-knowns! s)) 
			     (block-source s:file-block))))])
		(verbose-time known-thunk))

	      (compiler:report-messages! #t)
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; B-form transformation and analysis
	      ;;

	      (when (compiler:option:verbose) 
		(printf " transforming to b-normal form and analyzing~n"))
	      (when (compiler:option:debug)
		(debug " = ANALYZE =~n"))

	      ; analyze top level expressions, cataloguing local variables
	      (compiler:init-define-lists!)
	      (let ([bnorm-thunk
		     (lambda ()
		       (let-values ([(new-source new-codes max-arity)
				     (s:analyze-source-list 
				      (block-source s:file-block))])
			 (set-block-source! s:file-block new-source)
			 (set-block-codes! s:file-block new-codes)
			 (block:register-max-arity! s:file-block max-arity)
			 (s:register-max-arity! max-arity))
		       
		       ; take constant construction code and place it in front of the 
		       ; previously generated code. True constants first.
		       (set! number-of-true-constants (length compiler:define-list))
		       (s:append-block-sources! s:file-block 
						(append
						 compiler:define-list
						 compiler:per-load-define-list)))])
		(verbose-time bnorm-thunk))
	      (compiler:report-messages! #t)
	      
	      ; (map (lambda (ast) (pretty-print (zodiac->sexp/annotate ast))) (block-source s:file-block))

	      ;;-----------------------------------------------------------------------
	      ;; Closure conversion and explicit control transformation
	      ;;
	      
	      (when (compiler:option:verbose) 
		(printf " closure conversion and explicit control transformation~n"))
	      
	      (let ([closure-thunk
		     (lambda ()
		       ;; While converting closures, some `static' closures (nothing to
		       ;; close over but gloabls and per-load-constants) may be created.
		       ;; Merge them into s:file-block just before the top-level expression
		       ;; that the closure is a part of.
		       (compiler:init-closure-lists!)
		       (let loop ([el (block-source s:file-block)]
				  [cl (block-codes s:file-block)]
				  [e-acc null]
				  [c-acc null])
			 (if (null? el)
			     (begin
			       (set-block-source! s:file-block (reverse e-acc))
			       (set-block-codes! s:file-block (reverse c-acc)))
			     (begin
			       (compiler:init-once-closure-lists!)
			       (let ([s (closure-expression! (car el))])
				 (loop (cdr el) (cdr cl)
				       (append (list s)
					       compiler:once-closures-list
					       e-acc)
				       (append (list (car cl))
					       (map (lambda (globs) 
						      (make-code empty-set
								 empty-set
								 globs
								 empty-set
								 empty-set
								 #f #f null))
						    compiler:once-closures-globals-list)
					       c-acc)))))))])
	      
		(verbose-time closure-thunk))

	      ;;-----------------------------------------------------------------------
	      ;; Vehicle assignment
	      ;;
	      ;; Set export list offset for units at the same time
	      ;;
	      
	      (when (compiler:option:verbose)
		(printf " closure->vehicle mapping~n"))
	      
	      (when (eq? (compiler:option:vehicles) 'vehicles:automatic)
		(for-each 
		 (lambda (L)
		   (when (zodiac:case-lambda-form? L)
		     (map (lambda (body)
			    (relate-lambdas! L body))
			  (zodiac:case-lambda-form-bodies L))))
		 compiler:closure-list))
	      
	      (when (eq? (compiler:option:vehicles) 'vehicles:units)
		(compiler:fatal-error 
		 #f 
		 "unit-wise vehicle mapping not currently supported~n"))
	      (let ([vehicle-thunk
		     (lambda ()
		       (compiler:init-vehicles!)
		       (compiler:reset-label-number!)
		       (choose-vehicles!))])
		(verbose-time vehicle-thunk))

	      ;;-----------------------------------------------------------------------
	      ;; Representation Choosing 
	      ;;    From this stage, we have to work with separate code bodies, as well
	      
	      (when (compiler:option:verbose) 
		(printf 
		 " choosing data representations~n"))
	      
	      (let ([rep-thunk
		     (lambda ()
		       (compiler:init-structs!)
		       ; top-level
		       (map
			(lambda (c)
			  (choose-binding-representations! 
			   (code-local-vars c)
			   (code-global-vars c)
			   (code-used-vars c)
			   (code-captured-vars c)))
			(block-codes s:file-block))
		       ; code-bodies
		       (for-each (lambda (L)
				   (let* ([code (get-annotation L)]
					  [locals (code-local-vars code)]
					  [globals (code-global-vars code)]
					  [used (code-used-vars code)]
					  [captured (code-captured-vars code)])
				     (choose-binding-representations! locals globals used captured)
				     (choose-closure-representation! code)))
				 compiler:closure-list))])
		(verbose-time rep-thunk))
	      
	      ;;-----------------------------------------------------------------------
	      ;; Virtual Machine Scheme translation
	      ;;   Here we turn our code into VM Scheme as we enter the arena of
	      ;;   low level transformations and optimizations
	      
	      (when (compiler:option:verbose) (printf " transforming to Virtual Machine form~n"))
	      (when (compiler:option:debug) (debug " = VMPHASE =~n"))
	      
	      (let ([vmphase-thunk
		     (lambda ()
		       ; top-level.  The last expression will be in tail position and should
		       ; return its value
		       (let loop ([s (block-source s:file-block)]
				  [l (block-codes s:file-block)])
			 (unless (null? s)
			   (let-values ([(vm new-locals)
					 (vm-phase (car s) 
						   #t
						   #f
						   (if (null? (cdr s)) 
						       (lambda (ast)
							 (make-vm:return 
							  (zodiac:zodiac-origin ast)
							  (zodiac:zodiac-start ast)
							  (zodiac:zodiac-finish ast)
							  ast))
						       (lambda (ast)
							 (make-vm:void
							  (zodiac:zodiac-origin ast)
							  (zodiac:zodiac-start ast)
							  (zodiac:zodiac-finish ast)
							  ast)))
						   (null? (cdr s)))])
			     (set-car! s vm)
			     (add-code-local+used-vars! (car l) new-locals))
			   (loop (cdr s) (cdr l))))
		       ; code-bodies
		       (for-each 
			(lambda (L)
			  (let* ([code (get-annotation L)]
				 [tail-pos (lambda (ast)
					     (make-vm:return 
					      (zodiac:zodiac-origin ast)
					      (zodiac:zodiac-start ast)
					      (zodiac:zodiac-finish ast)
					      ast))]
				 [new-locals
				  (cond
				   [(zodiac:case-lambda-form? L)
				    (let-values ([(vms new-locals)
						  (let loop ([l (zodiac:case-lambda-form-bodies L)]
							     [case-codes (procedure-code-case-codes 
									  (get-annotation L))]
							     [vms null])
						    (if (null? l)
							(values (reverse! vms) 
								; empty: already added via case
								empty-set) 
							(let-values ([(vm new-locals)
								      (vm-phase (car l) #t #f tail-pos #t)])
							  (add-code-local+used-vars! 
							   (car case-codes)
							   new-locals )
							  (loop (cdr l)
								(cdr case-codes)
								(cons vm vms)))))])
				      (zodiac:set-case-lambda-form-bodies! L vms)
				      new-locals)]
				   [(zodiac:unit-form? L)
				    (let-values ([(vm new-locals)
						  (vm-phase (car (zodiac:unit-form-clauses L)) 
							    #t #f tail-pos #t)])
				      (zodiac:set-unit-form-clauses! L (list vm))
				      new-locals)]
				   [(zodiac:class*/names-form? L)
				    (let ([new-locals empty-set])
				      (let ([s (zodiac:sequence-clause-exprs
						(car (zodiac:class*/names-form-inst-clauses L)))])
					(let-values ([(vm new-locs) (vm-phase (car s) #t #f #f #f)])
					  (set-car! s vm)
					  (set! new-locals (set-union new-locs new-locals))))
				      (class-init-defaults-map! 
				       L
				       (lambda (var ast)
					 (let-values ([(vm new-locs)
						       (vm-phase ast
								 #f
								 (lambda (ast)
								   (list
								    (make-vm:set! 
								     #f #f #f
								     (list
								      (cons 
								       target-type:lexical
								       (vm:convert-bound-varref 
									(zodiac:binding->lexical-varref
									 var))))
								     ast
								     #f)))
								 identity #f)])
					   (set! new-locals (set-union new-locs new-locals))
					   vm)))
				      new-locals)]
				   [else (compiler:internal-error
					  L
					  "vmphase: unknown closure type")])])
			    (add-code-local+used-vars! code new-locals)))
			compiler:closure-list))])
		(verbose-time vmphase-thunk))

	      (compiler:report-messages! #t)

	      ;;-----------------------------------------------------------------------
	      ;; Virtual Machine Optimization Pass
	      ;;
	      
	      (when (compiler:option:verbose) (printf " optimizing Virtual Machine code~n"))
	      
	      (let ([vmopt-thunk
		     (lambda ()
		       ; top-level
		       (let loop ([bl (block-source s:file-block)]
				  [cl (block-codes s:file-block)])
			 (unless (null? bl)
			   (let-values ([(b new-locs) ((vm-optimize! #f #f) (car bl))])
			     (set-car! bl b)
			     (add-code-local+used-vars! (car cl) new-locs))
			   (loop (cdr bl) (cdr cl))))
		       
		       ; code-bodies
		       (for-each (lambda (L)
				   (let ([code (get-annotation L)])
				     (cond
				      [(zodiac:case-lambda-form? L)
				       (let loop ([bodies (zodiac:case-lambda-form-bodies L)]
						  [case-codes (procedure-code-case-codes code)]
						  [i 0])
					 (unless (null? bodies)
					   (let-values ([(new-body new-locs) ((vm-optimize! L i) (car bodies))])
					     (set-car! bodies new-body)
					     (add-code-local+used-vars! (car case-codes)
									new-locs)
					     (loop (cdr bodies)
						   (cdr case-codes)
						   (add1 i)))))]
				      [(zodiac:unit-form? L)
				       (let-values ([(new-clauses new-locs)
						     ((vm-optimize! L 0) (car (zodiac:unit-form-clauses L)))])
					 (set-car! (zodiac:unit-form-clauses L) new-clauses)
					 (add-code-local+used-vars! code new-locs))]
				      [(zodiac:class*/names-form? L)
				       (let ([s (zodiac:sequence-clause-exprs
						 (car (zodiac:class*/names-form-inst-clauses L)))])
					 (let-values ([(new-clauses new-locs) ((vm-optimize! L 0) (car s))])
					   (set-car! s new-clauses)
					   (add-code-local+used-vars! code new-locs)))
				       (class-init-defaults-map!
					L
					(lambda (var ast)
					  (let-values ([(new-expr new-locs) ((vm-optimize! L 0) ast)])
					    (add-code-local+used-vars! code new-locs)
					    new-expr)))]
				      [else (compiler:internal-error
					     L
					     "vmopt: unknown closure type")])))
				 compiler:closure-list))])
		(verbose-time vmopt-thunk))
		 
	      (compiler:report-messages! #t)
			  
	      ;;-----------------------------------------------------------------------
	      ;; Virtual Machine -> ANSI C translation
	      ;;  This code is probably too low level right now.
	      ;;
	      (when (compiler:option:verbose)
		(printf " [emitting ~a C to \"~a\"]~n" 
			"ANSI"
			c-output-path))
	
	      (let ([vm2c-thunk
		     (lambda ()
		       ;; set up bucket names - adds new symbols
		       (vm->c:make-bucket-names! (hash-table-map compiler:global-symbols
								 (lambda (x y) x)))
		       
		       (let ([c-port #f])
			 (dynamic-wind 
			  ;pre
			  (lambda () (set! c-port (open-output-file c-output-path)))
			  
			  ;value
			  (lambda ()
			    (fprintf c-port "#include \"~ascheme.h\"~n"
				     (if (compiler:option:compile-for-embedded)
					 ""
					 "e"))
			    (fprintf c-port "#include \"mzc.h\"~n~n")
			    (vm->c:emit-struct-definitions! compiler:structs c-port)
			    (vm->c:emit-symbol-declarations! c-port)
			    (vm->c:emit-inexact-declarations! c-port)
			    (vm->c:emit-prim-ref-declarations! c-port)
			    (vm->c:emit-static-declarations! c-port)
			    
			    (let loop ([n 0])
			      (unless (= n compiler:total-vehicles)
				(vm->c:emit-vehicle-declaration c-port n)
				(loop (+ n 1))))
			    (newline c-port)
			    
			    (unless compiler:multi-o-constant-pool?
			      (fprintf c-port "~nstatic void make_symbols()~n{~n")
			      (vm->c:emit-symbol-definitions! c-port)
			      (fprintf c-port "}~n"))
			    
			    (unless (zero? const:inexact-counter)
			      (fprintf c-port "~nstatic void make_inexacts()~n{~n")
			      (vm->c:emit-inexact-definitions! c-port)
			      (fprintf c-port "}~n"))

			    (fprintf c-port "~nstatic void make_export_symbols()~n{~n")
			    (vm->c:emit-export-symbol-definitions! c-port)
			    (fprintf c-port "}~n")

			    (fprintf c-port "~nstatic void gc_registration()~n{~n")
			    (vm->c:emit-registration! c-port)
			    (fprintf c-port "}~n")

			    (fprintf c-port "~nstatic void init_prims(Scheme_Env * env)~n{~n")
			    (vm->c:emit-prim-ref-definitions! c-port)
			    (fprintf c-port "}~n")
			    
			    (unless (null? compiler:case-lambdas)
			      (fprintf c-port "~nstatic void init_cases_arities()~n{~n")
			      (vm->c:emit-case-arities-definitions! c-port)
			      (fprintf c-port "}~n"))
			    (newline c-port)

			    (unless (null? compiler:compounds)
			      (fprintf c-port "~nstatic void init_compounds(Scheme_Env * env)~n{~n")
			      (vm->c:emit-compound-definitions! c-port)
			      (fprintf c-port "}~n"))
			    
			    (unless (null? compiler:classes)
			      (fprintf c-port "~nstatic void init_classes(Scheme_Env * env)~n{~n")
			      (vm->c:emit-class-definitions! c-port)
			      (fprintf c-port "}~n"))
			    (newline c-port)

			    (unless (null? compiler:interfaces)
			      (fprintf c-port "~nstatic void init_interfaces()~n{~n")
			      (vm->c:emit-interface-definitions! c-port)
			      (fprintf c-port "}~n"))
			    (newline c-port)

			    (let* ([codes (block-codes s:file-block)]
				   [locals (map code-local-vars codes)]
				   [globals (map code-global-vars codes)]
				   [init-constants-count
				   (if (zero? number-of-true-constants)
				       -1
				       (vm->c:emit-top-levels! "init_constants" #f #f number-of-true-constants
							       (block-source s:file-block)
							       locals globals
							       (block-max-arity s:file-block)
							       c-port))]
				  [top-level-count
				   (vm->c:emit-top-levels! "top_level" #t #t -1
							   (list-tail (block-source s:file-block) number-of-true-constants)
							   (list-tail locals number-of-true-constants)
							   (list-tail globals number-of-true-constants)
							   (block-max-arity s:file-block)
							   c-port)])
			      (fprintf c-port
				       "Scheme_Object * scheme_reload~a(Scheme_Env * env)~n{~n"
				       compiler:setup-suffix)
			      (fprintf c-port"~aScheme_Per_Load_Statics *PLS;~n"
				       vm->c:indent-spaces)
			      (fprintf c-port 
				       "~aPLS = (Scheme_Per_Load_Statics *)scheme_malloc(sizeof(Scheme_Per_Load_Statics));~n"
				       vm->c:indent-spaces)
			      (let loop ([c 0])
				(fprintf c-port "~a~atop_level_~a(env, PLS);~n" 
					 vm->c:indent-spaces 
					 (if (= c top-level-count) "return " "")
					 c)
				(unless (= c top-level-count)
				  (loop (add1 c))))
			      (fprintf c-port 
				       "}~n~n")

			      (fprintf c-port
				       "~nvoid scheme_setup~a(Scheme_Env * env)~n{~n"
				       compiler:setup-suffix)
			      (fprintf c-port
				       "~ascheme_set_tail_buffer_size(~a);~n"
				       vm->c:indent-spaces
				       s:max-arity)
			      (fprintf c-port "~agc_registration();~n"
				       vm->c:indent-spaces)
			      (unless compiler:multi-o-constant-pool?
				(fprintf c-port "~amake_symbols();~n"
					 vm->c:indent-spaces))
			      (unless (zero? const:inexact-counter)
				(fprintf c-port "~amake_inexacts();~n"
					 vm->c:indent-spaces))
			      (fprintf c-port "~amake_export_symbols();~n"
				       vm->c:indent-spaces)
			      (fprintf c-port "~ainit_prims(env);~n"
				       vm->c:indent-spaces)
			      (unless (null? compiler:case-lambdas)
				(fprintf c-port "~ainit_cases_arities();~n"
					 vm->c:indent-spaces))		       

			      (let loop ([c 0])
				(unless (> c init-constants-count)
				  (fprintf c-port "~ainit_constants_~a(env);~n" 
					   vm->c:indent-spaces
					   c)
				  (loop (add1 c))))

			      (unless (null? compiler:compounds)
				(fprintf c-port "~ainit_compounds(env);~n"
					 vm->c:indent-spaces))
			      (unless (null? compiler:classes)
				(fprintf c-port "~ainit_classes(env);~n"
					 vm->c:indent-spaces))
			      (unless (null? compiler:interfaces)
				(fprintf c-port "~ainit_interfaces();~n"
					 vm->c:indent-spaces))

			      (fprintf c-port 
				       "}~n~n")
			      
			      (when (string=? "" compiler:setup-suffix)
				(fprintf c-port
					 "~nScheme_Object * scheme_initialize(Scheme_Env * env)~n{~n")
				(fprintf c-port "~ascheme_setup(env);~n"
					 vm->c:indent-spaces)
				(fprintf c-port "~areturn scheme_reload(env);~n"
					 vm->c:indent-spaces)
				(fprintf c-port 
					 "}~n~n")))

			    (let emit-vehicles ([vehicle-number 0])
			      (unless (= vehicle-number compiler:total-vehicles)
				(let* ([vehicle (get-vehicle vehicle-number)]
				       [lambda-list (vehicle-lambdas vehicle)])
				  
				  (vm->c:emit-vehicle-header c-port vehicle-number)
				  (vm->c:emit-vehicle-prologue c-port vehicle)
				  
				  ; get the lambdas that appear in this vehicle
				  
				  ; sort the functions by index to get an optimal case statement
				  ; even for stupid compilers
				  (set! lambda-list
					(quicksort lambda-list
						   (lambda (l1 l2)
						     (< (closure-code-label (get-annotation l1))
							(closure-code-label (get-annotation l2))))))
				  (for-each (lambda (L)
					      (let ([code (get-annotation L)]
						    [start (zodiac:zodiac-start L)])
						(fprintf c-port "~a/* code body ~a ~a [~a,~a] */~n"
							 vm->c:indent-spaces (closure-code-label code)
							 (let ([n (closure-code-name code)])
							   (if n
							       (protect-comment 
								(vm->c:extract-inferred-name n))
							       ""))
							 (zodiac:location-line start)
							 (zodiac:location-column start))
						(cond
						 [(zodiac:case-lambda-form? L)
						  (let-values ([(count suffix?) 
								(vm->c:emit-function-prologue L c-port)])
						    (let loop ([i 0])
						      (unless (= i count)
							(let* ([indent
								(string-append
								 vm->c:indent-spaces vm->c:indent-spaces
								 (if suffix?  vm->c:indent-spaces ""))]
							       [undefines
								(vm->c:emit-case-prologue L i
											  (lambda ()
											    (if suffix?
												(fprintf c-port "~a~a/* begin case ~a */~n~a~a{~n" 
													 vm->c:indent-spaces vm->c:indent-spaces i
													 vm->c:indent-spaces vm->c:indent-spaces)
												(when (zero? i)
												  (fprintf c-port "~a{~n" vm->c:indent-spaces))))
											  (if suffix? (format "c~a" i) "")
											  indent
											  c-port)])
							  (vm->c-expression (list-ref (zodiac:case-lambda-form-bodies L) i)
									    code
									    c-port
									    (* (if suffix? 3 2) vm->c:indent-by)
									    #f)
							  (vm->c:emit-case-epilogue L i undefines indent c-port)
							  (when suffix?
							    (fprintf c-port "~a~a} /* end case ~a */~n" 
								     vm->c:indent-spaces 
								     vm->c:indent-spaces i)))
							
							(loop (add1 i))))
						    (vm->c:emit-function-epilogue code 
										  (if suffix? "" "}")
										  c-port))]
						 [(zodiac:unit-form? L)
						  (let* ([indent (string-append vm->c:indent-spaces)] 
							 [undefines (vm->c:emit-unit-prologue L indent c-port)])
						    (vm->c-expression (car (zodiac:unit-form-clauses L))
								      code
								      c-port
								      vm->c:indent-by
								      #f)
						    (vm->c:emit-unit-epilogue L undefines indent c-port))]
						 [(zodiac:class*/names-form? L)
						  (let* ([indent (string-append vm->c:indent-spaces)] 
							 [undefines (vm->c:emit-class-prologue L indent c-port)])
						    (vm->c-expression 
						     (car (zodiac:sequence-clause-exprs
							   (car (zodiac:class*/names-form-inst-clauses L))))
						     code
						     c-port
						     vm->c:indent-by
						     #f)

						    (vm->c:emit-class-epilogue L undefines indent c-port))]
						 [else
						  (compiler:internal-error
						   L
						   "vm2c: unknown closure type")])
						(newline c-port)))
					    lambda-list))
				
				(vm->c:emit-vehicle-epilogue c-port vehicle-number)
				(newline c-port)
				(emit-vehicles (+ 1 vehicle-number)))))
		   
			  ;post (dynamic wind cleanup)
			  (lambda ()  (close-output-port c-port)))))])
		(with-handlers ([void (lambda (exn)
					(delete-file c-output-path)
					(raise exn))])
		  (verbose-time vm2c-thunk)))
	      
	      (compiler:report-messages! #t)
	      
	      ;; Write out symbols for multi-o constant pool
	      (when compiler:multi-o-constant-pool?
		(call-with-output-file constant-pool-output-path
		  (lambda (port)
		    (fprintf port "(~s~n (symbols~n" compiler:setup-suffix)
		    (vm->c:emit-symbol-list! port "")
		    (fprintf port "  )~n )~n")))))))

	;;--------------------------------------------------------------------
	;; COMPILATION TO NATIVE CODE
	;;
	
	(if c-only?
	    (printf " [output to \"~a\"]~n" c-output-path)
	    
	    (begin
	      (unless input-path
		(printf "\"~a\": ~n" c-output-path))
	      
	      (when (compiler:option:verbose) (printf " [compiling native code to \"~a\"]~n"
						      obj-output-path))
	      
	      ;; Compile
	      (let ([compile-thunk
		     (lambda ()
		       (with-handlers
			   ([void (lambda (exn)
				    (compiler:fatal-error
				     #f
				     (string-append
				      " C compiler did not complete successfully"
				      (string #\newline)
				      (exn-message exn)))
				    (compiler:report-messages! #t))])
			 (compile-extension (not (compiler:option:verbose)) 
					    c-output-path obj-output-path
					    (list (collection-path "compiler")))))])
		(verbose-time compile-thunk))
	      
	      ;; clean-up
	      (when (and (compiler:option:clean-intermediate-files)
			 input-path)
		(delete-file c-output-path))
	      
	      (if multi-o?
		  (printf " [output to \"~a\"]~n" obj-output-path)
		  
		  (begin
		    ;; Link
		    (when (compiler:option:verbose) (printf " [linking to \"~a\"]~n"
							    dll-output-path))
		    (let ([link-thunk
			   (lambda ()
			     (with-handlers
				 ([void (lambda (exn)
					  (compiler:fatal-error 
					   #f 
					   (string-append 
					    " linker did not link successfully"
					    (string #\newline)
					    (exn-message exn)))
					  (compiler:report-messages! #t))])
			       (link-extension (not (compiler:option:verbose)) (list obj-output-path) dll-output-path)))])
		      (verbose-time link-thunk))
		    
		    ;; clean-up
		    (when (compiler:option:clean-intermediate-files)
		      (delete-file obj-output-path))
		    
		    (printf " [output to \"~a\"]~n" dll-output-path)))))
	
	(when debug:port
	  (close-output-port debug:port))
	
	;; clean up for the garbage collector
	(compiler:init-define-lists!)
	(const:init-tables!)
	(compiler:init-closure-lists!)
	(compiler:init-structs!)
	(set! s:file-block #f)
	(when (compiler:option:verbose)
	  (printf " finished [cpu ~a, real ~a].~n"
		  total-cpu-time
		  total-real-time))))))
