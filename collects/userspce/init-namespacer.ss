
(unit/sig plt:init-namespace^
  (import plt:basis-import^
	  [init-params : plt:init-params^]
	  [prims : plt:prims^]
          [params : plt:userspace:params^]
          [zodiac : zodiac:system^]
          [zodiac:interface : drscheme:interface^]
	  mzlib:function^)
  
  (define (init-namespace vocab)
    ;; setup-primitives before the teachpack in case the teachpack
    ;; re-introduces any primitive definitions that were removed
    (teachpack-thunk vocab)

    (add-extra-macros)
    (make-keywords))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                               ;;;
  ;;;                 Extra Macros                  ;;;
  ;;;                                               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define extra-macros
    (let ([names '(local
		      split
		    split*
		    tprompt
		    nand
		    recur
		    rec
		    evcase
		    reference-file
		    polymorphic
		    mrspidey:control 
		    ;;type:
		    :
		    define-type
		    define-constructor)]
	  [namespace (make-namespace)])
      (parameterize ([current-namespace namespace])
	(require-library "macro.ss")
	(require-library "spidey.ss")
	(require-library "refer.ss")
	(require-library "tmacro.ss" "graphics")
	(map (lambda (x) (list x (global-defined-value x)))
	     names))))
  
  (define (add-extra-macros)
    (when (init-params:setting-teaching-primitives-and-syntax? (init-params:current-setting))
      (for-each (lambda (x) (apply global-defined-value x)) extra-macros)))
  
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
  
  ;; build-single-teachpack-unit : string boolean -> (union #f (cons `(unit () X)))
  ;; returns the syntax for a unit with the free variable `vocab' in it.
  (define (build-single-teachpack-unit v)
    (with-handlers
        ([(lambda (x) #t)
          (lambda (x)
            (invalid-teachpack (exn-message x))
            #f)])
      (let ([both-unit (parameterize ([read-case-sensitive #t])
                         (load/cd v))])
        (if (or (unit/sig? both-unit)
                (and (pair? both-unit)
                     (unit/sig? (car both-unit))
                     (unit/sig? (cdr both-unit))))
            ; Put the unit into a procedure that invokes it into
            ;  the current namespace
            (let* ([new-unit (if (pair? both-unit)
                                 (car both-unit)
                                 both-unit)]
                   [signature 
                    (exploded->flattened (unit-with-signature-exports new-unit))]
                   [macros-unit (if (pair? both-unit)
                                    (cdr both-unit)
                                    (unit/sig () (import plt:userspace^)))]
                   [macros-signature (exploded->flattened (unit-with-signature-exports macros-unit))])
              `(compound-unit/sig
                 (import [p : plt:userspace^])
                 (link [functions 
                        : ()
                        ((unit/sig ()
                           (import plt:userspace^)
                           (with-handlers ([(lambda (x) #t)
                                            (lambda (x)
                                              (,invalid-teachpack
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
                              plt:userspace^)))
                         p)]
                       [macros : ,macros-signature (,macros-unit p)]
                       [install-macros 
                        : ()
                        ((unit/sig ()
                           (import ,macros-signature)
                           ,@(map (lambda (x) `(,extend-vocab (unbox vocab-box) ',x ,x)) 
                                  macros-signature))
                         macros)])
                 (export)))
            (begin
              (invalid-teachpack 
               (format "loading Teachpack file does not result in a either a unit/sig or a pair of unit/sigs, got: ~e"
                       both-unit))
              #f)))))
  
    ;; pp:arity : procedure -> string
    ;; to format the arity of the string for an error message
  (define (pp-arity f)
    (let ([ar (arity f)]
          [string-append/between
           (lambda (bet lst)
             (apply string-append
                    (let loop ([lst lst])
                      (cond
                        [(null? lst) null]
                        [(null? (cdr lst)) lst]
                        [else (list* (car lst) bet (loop (cdr lst)))]))))]
          [pp-simple-arity
           (lambda (x)
             (cond
               [(number? x) (number->string x)]
               [(arity-at-least? x) (format "~a or more" (arity-at-least-value x))]))])
      (cond
        [(equal? 1 ar) "1 argument"]
        [(list? ar) (string-append
                     (string-append/between
                      " or "
                      (map pp-simple-arity ar))
                     " arguments")]
        [else (string-append (pp-simple-arity ar) " arguments")])))
  
  
  ;; extend-vocab : zodiac:vocab symbol procedure -> void
  ;; to extend the vocabulary with the syntactic form named by the
  ;; symbol with the expander `func' (ala define-macro)
  (define (extend-vocab vocab symbol func)
    (zodiac:add-micro-form
     symbol vocab
     (let* ([kwd '()]
            [in-pattern '(_ elements ...)]
            [m&e (zodiac:make-match&env in-pattern kwd)])
       (lambda (expr env attrib vocab)
         (cond
           [(zodiac:match-against m&e expr env)
            =>
            (lambda (pattern-env)
              (let ([cache-table (make-hash-table)]
		    [elements (zodiac:pexpand '(elements ...) pattern-env kwd)])
                (unless (procedure-arity-includes? func (length elements))
                  (zodiac:interface:static-error
                   (symbol->string symbol) symbol
                   (zodiac:sexp->raw expr)
                   (format "expected ~a, got ~a" (pp-arity func) (length elements))))
		(let* ([structurized
			(zodiac:structurize-syntax 
			 (apply func (map (lambda (x) (zodiac:sexp->raw x cache-table)) elements))
			 expr '() cache-table)]
		       [scheme-vocab
			(zodiac:create-vocabulary
			 'scheme-w/user-defined-macros/drscheme
			 zodiac:scheme-vocabulary)]
		       [expanded
			(parameterize ([current-namespace (make-namespace)])
			  (zodiac:prepare-current-namespace-for-vocabulary scheme-vocab)
			  (zodiac:expand-expr
			   structurized
			   env 
			   attrib
			   scheme-vocab))])
		  ;(printf "structurized~n~a~n~n" structurized)
		  ;(printf "expanded~n~a~n~n" expanded)
		  expanded)))]
           [else 
            (zodiac:interface:static-error
             (symbol->string symbol) symbol
             (zodiac:sexp->raw expr)
             (format "Malformed ~a" symbol))])))))
  
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
	   [vocab-box (box #f)]
           [cu
            (eval
             `(let ([vocab-box ,vocab-box])
		(compound-unit/sig
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
			    [(or (,init-params:advanced-language? (,init-params:current-setting))
				 (,init-params:setting-teaching-primitives-and-syntax?
				  (,init-params:current-setting)))
			     ,@(build-gdvs (signature->symbols plt:advanced-extras^))]
			    [else (void)]))
			 userspace)]
		      
		      link-clauses))
		  (export))))])
      (values
       (lambda (vocab)
	 (set-box! vocab-box vocab)
	 (with-handlers ([(lambda (x) #t)
			  (lambda (x)
			    (invalid-teachpack (exn-message x))
			    #f)])
	   (invoke-unit/sig cu)))
       bad-teachpacks)))
  
  (define (teachpack-ok? x)
    (if (build-single-teachpack-unit x)
        #t
        #f))
  
  (define-values (teachpack-thunk bad-teachpacks) (build-teachpack-thunk null))
  (define (teachpack-changed v)
    (set!-values (teachpack-thunk bad-teachpacks) (build-teachpack-thunk v))
    bad-teachpacks)
  
  (define teachpack-error-display (make-parameter (lambda (x) (display x) (newline))))
  (define (invalid-teachpack str) ((teachpack-error-display) str))
  
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
  
  (define keyword-languages '(beginner intermediate advanced))
  
  (define (make-keywords)
    (when (memq (init-params:setting-primitives (init-params:current-setting))
                keyword-languages)
      
      ;; currently, we are opting for the first of these two options:
      
      ;; 1. this makes all names keywords (matches the stepper)
      (for-each (lambda (x) (keyword-name (car x))) (make-global-value-list))
      
      ;; 2. this only make #% names and syntax names 
      ;; be keywords (the minimum -- more freedom with ids)
      '(for-each (lambda (x)
                  (let ([name (car x)]
                        [str-name (symbol->string (car x))])
                    (when (or (syntax? (global-defined-value name))
                              (and (>= (string-length str-name) 2)
                                   (string=? (substring str-name 0 2)
                                             "#%")))
                      (keyword-name name))))
                (make-global-value-list))
      (void))))
