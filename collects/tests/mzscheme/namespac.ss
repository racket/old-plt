
(load-relative "loadtest.ss")

(SECTION 'namespaces)

(arity-test eval 1 2)
(arity-test compile 1 1)
(arity-test compiled-expression? 1 1)

(test #f compiled-expression? 1)
(test #t values (compiled-expression? (compile 1)))
(test #t values (compiled-expression? (let ([c (compile 1)]
					    [p (open-output-string)])
					(display c p)
					(parameterize ([read-accept-compiled #t])
					  (read (open-input-string (get-output-string p)))))))

(test `,void eval `',void)

; FIXME: this is a useful test, but we have no make-global-value-list...
; Test primitive-name
'(let ([gvl (parameterize ([current-namespace (make-namespace)]) (make-global-value-list))]
      [aliases (list (cons "call/cc" "call-with-current-continuation")
		     (cons "call/ec" "call-with-escape-continuation")
		     (cons "interaction-environment" "current-namespace"))])
  (test #t 'names
	(andmap
	 (lambda (nv-pair)
	   (let ([name (car nv-pair)]
		 [value (cdr nv-pair)])
	     (or (not (primitive? value))
		 (let* ([s (symbol->string name)]
			[sr (if (char=? #\# (string-ref s 0))
				(substring s 2 (string-length s))
				s)]
			[st (let ([m (assoc sr aliases)])
			      (if m
				  (cdr m)
				  sr))])
		   (or (equal? st (primitive-name value))
		       (and (fprintf (current-error-port)
				     "different: ~a ~a~n" st (primitive-name value))
			    #f))))))
	 gvl)))

;;FIXME
#|
(define (test-empty . flags)
  (let ([e (apply make-namespace flags)])
    (parameterize ([current-namespace e])
      (test null make-global-value-list)
      (test 'unbound 'empty-namespace
	    (with-handlers ([void (lambda (exn) 'unbound)])
			   (eval 'car)))
      (test 'unbound 'empty-namespace
	    (with-handlers ([void (lambda (exn) 'unbound)])
			   (eval '#%car)))
      (global-defined-value 'hello 5)
      (test 5 'empty-namespace (eval 'hello))
      (test '((hello . 5)) make-global-value-list))))
(test-empty 'empty)
(apply test-empty (append '(empty) (map car flag-map) (map cadr flag-map)))
|#

(report-errs)
