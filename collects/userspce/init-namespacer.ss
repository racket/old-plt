(unit/sig plt:init-namespace^
  (import plt:basis-import^
	  [init-params : plt:init-params^]
	  [prims : plt:prims^]
          [params : plt:userspace:params^]
          [zodiac : zodiac:system^]
	  mzlib:function^)

  (define (init-namespace)
    (teachpack-thunk)
    (setup-primitives)
    (make-keywords))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                               ;;;
  ;;;                  Teachpacks                   ;;;
  ;;;                                               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; adding the teachpack also adds some language-specific defns.

  (define (exploded->flattened exploded)
    (let ([sig exploded])
      (let loop ([l (vector->list sig)][r null])
	(cond
	 [(null? l) r]
	 [(symbol? (car l)) (loop (cdr l) (cons (car l) r))]
	 [else (let ([sub (loop (vector->list (cadr l)) null)]
		     [prefix (string-append (symbol->string (car l)) ":")])
		 (loop (cdr l)
		       (append
			(map (lambda (s)
			       (string->symbol
				(string-append
				 prefix
				 (symbol->string s))))
			     sub))))]))))

  (define (build-gdvs exploded)
    (let ([flattened (exploded->flattened exploded)])
      (map
       (lambda (x)
	 `(global-defined-value ',x ,x))
       flattened)))

  (define core-flat@ (require-library-unit/sig "coreflatr.ss"))
  
  ;; build-single-teachpack-unit : string boolean -> (union #f (unit () X))
  (define (build-single-teachpack-unit v)
    (with-handlers
     ([(lambda (x) #t)
       (lambda (x)
	 (invalid-teachpack (exn-message x))
	 #f)])
     (let ([new-unit (parameterize ([read-case-sensitive #t])
				   (load/cd v))])
       (if (unit/sig? new-unit)
					; Put the unit into a procedure that invokes it into
					;  the current namespace
	   (let* ([signature 
		   (exploded->flattened (unit-with-signature-exports new-unit))])
	     (eval
	      `(unit/sig ()
		 (import plt:userspace^)
		 (with-handlers ([(lambda (x) #t)
				  (lambda (x)
				    ((error-display-handler)
				     (format
				      "Invalid Teachpack: ~a~n~a"
				      ,v
				      (if (exn? x)
					  (exn-message x)
					  x))))])
		   (global-define-values/invoke-unit/sig
		    ,signature
		    ,new-unit
		    #f
		    plt:userspace^)))))
	   (begin
	     (invalid-teachpack 
	      (format "loading Teachpack file does not result in a unit/sig, got: ~e"
		      new-unit))
	     #f)))))

  ;; build-teachpack-thunk : (listof string)
  ;;                      -> (values (union #f (-> void))
  ;;                                 (listof string))
  ;; accepts a filename and returns two values:
  ;; - either #f or a thunk that invokes the teachpack
  ;; - a list of the invalid teachpacks (a subset of the input list)
  (define (build-teachpack-thunk v)
    (unless (and (list? v)
		 (andmap string? v))
      (error 'build-teachpack-thunk "expected a list of strings, got: ~e" v))
    (let* ([tagn 0]
	   [bad-teachpacks null]
	   [link-clauses
	    (let loop ([teachpack-strings v]
		       [link-clauses null])
	      (cond
	       [(null? teachpack-strings) (reverse link-clauses)]
	       [else
		(let ([unit (build-single-teachpack-unit (car teachpack-strings))])
		  (if unit
		      (begin
			(set! tagn (+ tagn 1))
			(loop (cdr teachpack-strings)
			      (cons
			       `[,(string->symbol (format "teachpack~a" tagn)) : ()
				 (,unit userspace)]
			       link-clauses)))
		      (begin
			(set! bad-teachpacks (cons (car teachpack-strings) bad-teachpacks))
			(loop (cdr teachpack-strings)
			      link-clauses))))]))]
	   [cu
	    (eval
	     `(compound-unit/sig
		(import)
		(link
		 ,@(list*
		    `[userspace
		      : plt:userspace^ 
		      (,(if (defined? 'mred@)
			    `(compound-unit/sig
			       (import)
			       (link [core : mzlib:core-flat^ (,core-flat@)]
				     [mred : mred^ (,(global-defined-value 'mred@))]
				     [turtles : turtle^ ((require-library "turtler.ss" "graphics")
							 (core : mzlib:function^))]
				     [posn : ((struct posn (x y)))
					   ((unit/sig ((struct posn (x y)))
					      (import)
					      (define-struct posn (x y))))])
			       (export (open core)
				       (open mred)
				       (open posn)
				       (open turtles)))
			    `(compound-unit/sig 
			       (import)
			       (link [core : mzlib:core-flat^ (,core-flat@)]
				     [posn : ((struct posn (x y)))
					   ((unit/sig ((struct posn (x y)))
					      (import)
					      (define-struct posn (x y))))])
			       (export (open core)
				       (open posn)))))]
		    `[language-specific-additions
		      : ()
		      ((unit/sig ()
			 (import plt:userspace^)

			 (cond
			  [(,init-params:beginner-language? (,init-params:current-setting))
			   ,@(build-gdvs (signature->symbols plt:beginner-extras^))]
			  [(,init-params:intermediate-language? (,init-params:current-setting))
			   ,@(build-gdvs (signature->symbols plt:intermediate-extras^))]
			  [(,init-params:advanced-language? (,init-params:current-setting))
			   ,@(build-gdvs (signature->symbols plt:advanced-extras^))]
			  [(,init-params:full-language? (,init-params:current-setting)) (void)]))
		       userspace)]

		    link-clauses))
		(export)))])
      (values
       (lambda ()
	 (invoke-unit/sig
	  cu))
       bad-teachpacks)))

  (define (teachpack-ok? x)
    (if (build-single-teachpack-unit x)
	#t
	#f))

  (define-values (teachpack-thunk bad-teachpacks) (build-teachpack-thunk null))
  (define (teachpack-changed v)
    (set!-values (teachpack-thunk bad-teachpacks) (build-teachpack-thunk v))
    bad-teachpacks)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                               ;;;
  ;;;          Clear out / add new names            ;;;
  ;;;                                               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define ricedefs@ (require-library "ricedefr.ss" "userspce"))

  (define (make-prims-to-remove prims)
    (let ([ht (make-hash-table)])
      (parameterize ([current-namespace (apply
					 make-namespace
					 (if (defined? 'mred@)
					     (list 'mred)
					     null))])
	(for-each
	 (lambda (pr)
	   (let ([name (car pr)]
		 [val (cdr pr)])
	     (when (or (struct-type? val)
		       (procedure? val))
	       (hash-table-put! ht name #t))))
	 (make-global-value-list)))
      (for-each
       (lambda (x)
	 (hash-table-remove! ht x)
	 (let ([hp-x (string->symbol
		      (string-append
		       "#%"
		       (symbol->string x)))])
	   (when (hash-table-get ht hp-x (lambda () #f))
	     (hash-table-remove! ht hp-x))))
       prims)
      (hash-table-map ht (lambda (x y) x))))

  (define to-remove-from-beginning (make-prims-to-remove prims:beginning))
  (define to-remove-from-intermediate (make-prims-to-remove prims:intermediate))
  (define to-remove-from-advanced (make-prims-to-remove prims:advanced))

  (define (setup-primitives)
    (let ([to-remove
	   (case (init-params:setting-primitives (init-params:current-setting))
	    [(beginner)
	     to-remove-from-beginning]
	    [(intermediate)
	     to-remove-from-intermediate]
	    [(advanced)
	     to-remove-from-advanced]
	    [else null])])
      (for-each undefine to-remove)
      
      ;; ricedefs
      (let* ([setting (init-params:current-setting)]
             [improper-lists?
              (or (not (init-params:zodiac-vocabulary? setting))
                  (init-params:setting-allow-improper-lists? setting))])
        (zodiac:allow-improper-lists improper-lists?)
        (params:allow-improper-lists improper-lists?)
        (params:eq?-only-compares-symbols (init-params:setting-eq?-only-compares-symbols? setting))
        (params:<=-at-least-two-args (init-params:setting-<=-at-least-two-args setting))
        (params:error-sym/string-only (init-params:setting-error-sym/string-only setting))
        (when (init-params:teaching-level? setting)
          (global-define-values/invoke-unit/sig ricedefs^ ricedefs@ #f (params : plt:userspace:params^))))))
  
  
  
  (define keyword-languages '(beginner intermediate advanced))

  (define (make-keywords)
    (when (memq (init-params:setting-primitives (init-params:current-setting))
                keyword-languages)

      ;; currently, we are opting for the second of these two options:

      ;; 1. this makes all names keywords (better error checking)
      '(for-each (lambda (x) (keyword-name (car x))) (make-global-value-list))
      
      ;; 2. this only make #% names and syntax names 
      ;; be keywords (the minimum -- more freedom with ids)
      (for-each (lambda (x)
                   (let ([name (car x)]
                         [str-name (symbol->string (car x))])
                     (when (or (syntax? (global-defined-value name))
                               (and (>= (string-length str-name) 2)
                                    (string=? (substring str-name 0 2)
                                              "#%")))
                       (keyword-name name))))
                 (make-global-value-list)))))

