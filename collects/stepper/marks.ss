(unit/sig stepper:marks^
  (import [z : zodiac:system^]
          [cp : stepper:client-procs^]
          mzlib:function^)
  
  (define (make-mark location label bindings)
    (lambda () (cons location (cons label bindings))))
  
  (define (mark-source mark)
    (car (mark)))
  
  (define (mark-bindings mark)
    (cddr (mark)))
  
  (define (mark-label mark)
    (cadr (mark)))
  
  (define (mark-binding-value mark-binding)
    ((car mark-binding)))
  
  (define (mark-binding-varref mark-binding)
    (cadr mark-binding))

  (define (original-name varref)
    (if (z:top-level-varref? varref)
        (z:varref-var varref)
        (let ([binding (z:bound-varref-binding varref)])
          (if binding
              (z:binding-orig-name binding)
              (z:varref-var varref))))) ; this happens for application temps
  
  (define (expose-mark mark)
    (let ([source (mark-source mark)]
          [label (mark-label mark)]
          [bindings (mark-bindings mark)])
      (list source
            label
            (map (lambda (binding)
                   (list (original-name (mark-binding-varref binding))
                         (mark-binding-value binding)))
                 bindings))))
  
  (define (display-mark mark)
    (let ([exposed (expose-mark mark)])
      (printf "source: ~a~n" (let ([read (cp:read-getter (car exposed))])
                               (and read
                                    (z:sexp->raw read))))
      (printf "label: ~a~n" (cadr exposed))
      (printf "bindings:~n")
      (for-each (lambda (binding-pair)
                  (printf " ~a : ~a~n" (car binding-pair) (cadr binding-pair)))
                (caddr exposed))))
  
  (define (find-var-binding mark-list var)
    (if (null? mark-list)
        ; must be a primitive
        (error 'find-var-binding "variable not found in environment: ~a" var)
	; (error var "no binding found for variable.")
	(let* ([bindings (mark-bindings (car mark-list))]
	       [matches (filter (lambda (mark-var)
				  (eq? var (z:varref-var (mark-binding-varref mark-var))))
                                bindings)])
	  (cond [(null? matches)
		 (find-var-binding (cdr mark-list) var)]
		[(> (length matches) 1)
		 (error 'find-var-binding "more than one variable binding found for var: ~a" var)]
		[else ; (length matches) = 1
		 (car matches)])))))
