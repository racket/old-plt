; vehicle.ss
; vehicle choosing phase
; (c) 1997 Sebastian Good, Rice PLT

(unit/sig
 compiler:vehicle^
 (import (compiler:option : compiler:option^)
	 compiler:library^
	 compiler:cstructs^
	 (zodiac : zodiac:system^)
	 compiler:zlayer^
	 compiler:const^
	 compiler:analyze^
	 compiler:closure^
	 compiler:driver^)


;; Used for union-find for lambda vehicles:
(define (get-vehicle-top code)
  (let loop ([code code])
    (let ([c (code-vehicle code)])
      (if (code? c)
	  (let ([top (loop c)])
	    (set-code-vehicle! code top)
	    top)
	  code))))

(define-struct vehicle (total-labels lambdas max-arity))
(define-struct (procedure-vehicle struct:vehicle) (max-args))
(define-struct (unit-vehicle struct:vehicle) ())
(define-struct (class-vehicle struct:vehicle) ())

(define vehicle:procedure 'vehicle:procedure)
(define vehicle:unit 'vehicle:unit)
(define vehicle:class 'vehicle:class)

(define vehicles:automatic 'vehicles:automatic)
(define vehicles:functions 'vehicles:functions)
(define vehicles:units 'vechicles:units)
(define vehicles:monolithic 'vehicles:monolithic)

(define (make-empty-vehicle type)
  (case type
    [(vehicle:procedure) (make-procedure-vehicle 0 null 0 0)]
    [(vehicle:unit) (make-unit-vehicle 0 null 0)]
    [(vehicle:class) (make-class-vehicle 0 null 0)]))
(define (vehicle-is-type? v type)
  (case type
    [(vehicle:procedure) (procedure-vehicle? v)]
    [(vehicle:unit) (unit-vehicle? v)]
    [(vehicle:class) (class-vehicle? v)]))

(define compiler:vehicles #f)
(define compiler:total-vehicles 0)
(define vehicle:add-lambda! 
  (lambda (v type l)
    (let ([old-v (hash-table-get compiler:vehicles v 
				 (lambda () (make-empty-vehicle type)))])
      (unless (vehicle-is-type? old-v type)
	      (compiler:internal-error
	       #f
	       "can't use vehicle ~a as type ~a"
	       old-v type))
      (set-vehicle-lambdas! old-v (cons l (vehicle-lambdas old-v)))
      (hash-table-put! compiler:vehicles v old-v))))
(define vehicle:register-max-arity!
  (lambda (v n)
    (set-vehicle-max-arity! v (max n (vehicle-max-arity v)))))
(define vehicle:register-max-args!
  (lambda (v n)
    (set-procedure-vehicle-max-args! v (max n (procedure-vehicle-max-args v)))))

; These lists are built up backwards, so reverse it before outputing the list
(define compiler:case-lambdas null)
(define compiler:total-unit-exports null)
(define compiler:classes null)

(define (compiler:init-vehicles!)
  (set! compiler:vehicles (make-hash-table))
  (set! compiler:total-vehicles 0)
  (set! compiler:case-lambdas null)
  (set! compiler:total-unit-exports null)
  (set! compiler:classes null))

(define choose-vehicles!
  (lambda ()
    (when (eq? (compiler:option:vehicles) vehicles:monolithic) 
      (set! compiler:total-vehicles (compiler:option:vehicles:monoliths)))

    (for-each (lambda (L)
		(let* ([code (get-annotation L)]
		       [type (cond
			      [(zodiac:case-lambda-form? L) vehicle:procedure]
			      [(zodiac:unit-form? L) vehicle:unit]
			      [else vehicle:class])]
		       [new-vehicle
			(lambda ()
			  (begin0 compiler:total-vehicles
				  (set! compiler:total-vehicles
					(+ 1 compiler:total-vehicles))))]
		       [vnum (case (compiler:option:vehicles)
			       [(vehicles:automatic)
				(case type
				  [(vehicle:procedure) 
				   (let* ([top (get-vehicle-top code)]
					  [n (or (code-vehicle top)
						 (new-vehicle))])
				     (set-code-vehicle! top n)
				     (set-code-vehicle! code n)
				     n)]
				  [(vehicle:unit vehicle:class) (new-vehicle)])]
			       [(vehicles:monolithic) 
				(case type
				  [(vehicle:procedure) (random (compiler:option:vehicles:monoliths))]
				  [(vehicle:unit vehicle:class) (new-vehicle)])]
			       [(vehicles:functions) (new-vehicle)]
			       [else (compiler:internal-error 
				      #f 
				      (format "bad option:vehicles - ~a" (compiler:option:vehicles)))])])
		  (set-code-vehicle! code vnum)
		  (vehicle:add-lambda! vnum type L)
		  ; assign label, too
		  (let* ([vehicle (hash-table-get compiler:vehicles 
						  vnum
						  (lambda ()
						    (compiler:internal-error 
						     #f "bad hash table lookup (2)~n")))]
			 [curr-label (vehicle-total-labels vehicle)])
		    (vehicle:register-max-arity! vehicle (code-max-arity code))
		    (s:register-max-arity! (code-max-arity code))
		    (cond
		     [(procedure-vehicle? vehicle)
		      (vehicle:register-max-args! 
		       vehicle
		       (apply max
			      (cons
			       0
			       (map (lambda (a) (length (zodiac:arglist-vars a)))
				    (zodiac:case-lambda-form-args L)))))]
		     [else (void)])
		    (set-code-label! code curr-label)
		    (set-vehicle-total-labels! vehicle (+ 1 curr-label)))

		  ; We take this opportunity to collect other top-level info
		  ; that is closure-type-specific
		  (cond
		   [(zodiac:case-lambda-form? L)
		    (unless (= 1 (length (zodiac:case-lambda-form-args L)))
			    (set-procedure-code-case-arities! code (length compiler:case-lambdas))
			    (set! compiler:case-lambdas (cons L compiler:case-lambdas)))]
		   [(zodiac:unit-form? L)
		    (let ([exports (zodiac:unit-form-exports L)])
		      (if (null? exports)
			  (set-unit-code-export-list-offset! code -1)
			  (begin
			    (set-unit-code-export-list-offset! code 
							       (length compiler:total-unit-exports))
			    (set! compiler:total-unit-exports
				  (append
				   (reverse! (map (lambda (x) 
						    (compiler:get-symbol-const! (cdr x) (zodiac:read-object (cdr x))))
						  (zodiac:unit-form-exports L)))
				   compiler:total-unit-exports)))))]
		   [(zodiac:class*/names-form? L)
		    ; create symbols for all the ivars:
		    (let* ([public-lookup-bindings (class-code-public-lookup-bindings code)]
			   [inherit-bindings (class-code-inherit-bindings code)]
			   [rename-bindings (class-code-rename-bindings code)]
			   [mk-syms
			    (lambda (l)
			      (for-each
			       (lambda (s) (compiler:get-symbol-const! s (zodiac:binding-orig-name s)))
			       l))])
		      (mk-syms public-lookup-bindings)
		      (mk-syms inherit-bindings)
		      (mk-syms rename-bindings))
		    
		    (set-class-code-assembly! code (length compiler:classes))
		    (set! compiler:classes (cons L compiler:classes))])))

	      compiler:lambda-list)))

(define (get-vehicle vehicle-number)
  (hash-table-get compiler:vehicles 
		  vehicle-number
		  (lambda () 
		    ; not an error because random placement
		    ; may leave some vehicles empty
		    (let ([v (make-empty-vehicle vehicle:procedure)])
		      (hash-table-put! compiler:vehicles vehicle-number v)
		      v))))



(define relate-lambdas!
  (letrec
      ([same-vehicle!
	(lambda (a b)
	  (let ([a-top (get-vehicle-top (get-annotation a))]
		[b-top (get-vehicle-top (get-annotation b))])
	    (unless (eq? a-top b-top)
	       (set-code-vehicle! a-top b-top))))]
       
       [relate!
	(lambda (current-lambda ast)
	  (cond
	   
	   ;;------------------------------------------------------------------
	   ;; LET EXPRESSIONS
	   ;;
	   [(zodiac:let-values-form? ast)
	    (relate! current-lambda (zodiac:let-values-form-body ast))]

	   [(zodiac:letrec*-values-form? ast)
	    (relate! current-lambda (zodiac:letrec*-values-form-body ast))]
	   
	   ;;-----------------------------------------------------------------
	   ;; IF EXPRESSIONS
	   ;;
	   [(zodiac:if-form? ast)
	    (relate! current-lambda (zodiac:if-form-else ast))]

	   ;;------------------------------------------------------------------
	   ;; BEGIN EXPRESSIONS
	   ;;
	   [(zodiac:begin-form? ast)
	    (let loop ([l (zodiac:begin-form-bodies ast)])
	      (if (null? (cdr l))
		  (relate! current-lambda (car l))
		  (loop (cdr l))))]
  
	   ;;------------------------------------------------------------------
	   ;; BEGIN0 EXPRESSIONS
	   ;;
	   [(zodiac:begin0-form? ast)
	    (relate! current-lambda (zodiac:begin0-form-first ast))]
  
	   ;;-----------------------------------------------------------------
	   ;; APPLICATIONS
	   ;;
	   ;; Check for known func & relate to this one
	   ;;
	   [(zodiac:app? ast)
	    (let ([f (zodiac:app-fun ast)])
	      (cond
	       [(zodiac:bound-varref? f)
		(let ([known (extract-varref-known-val f)])
		  (and known
		       (when (zodiac:case-lambda-form? known)
			     (same-vehicle! current-lambda known))))]
	       [else (void)]))]
	   
	   [else (void)]))])
    (lambda (current-lambda ast) (relate! current-lambda ast))))

(define (vehicle:only-code-in-vehicle? code)
  (= (vehicle-total-labels (get-vehicle (code-vehicle code))) 1))
)