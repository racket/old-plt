(module bad-subst mzscheme
  (require "mc.ss"
           (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "match.ss"))
  
  (define lang
    (language
     (e (e e)
        (lambda (variable) e)
        variable)
     (e-ctxt (e-ctxt e) 
             (v e-ctxt)
             hole)
     (v (lambda (variable) e))))
  
  (define reductions
    (list
     (reduction/context lang
                        e-ctxt
                        ((lambda ((name x variable)) (name body e)) (name arg v))
                        (bad-subst (term x) (term arg) (term body)))))
  
  (define (bad-subst var arg body)
    (let loop ([body body])
      (match body
        [`(lambda (,x) ,e)
         (if (eq? x arg)
             body
             `(lambda (,x) ,(loop e)))]
        [`(,e1 ,e2) `(,(loop e1) ,(loop e2))]
        [(? symbol?)
         (if (eq? body var)
             arg
             body)])))

  ;; test-cycles : reduction-graph -> void
  (define (test-cycles graph)
    (let ([visited-ht (make-hash-table 'equal)])
      (let loop ([term (reduction-graph-initial graph)])
        (cond
          [(hash-table-get visited-ht term (lambda () #f))
           (printf "cycle: ~s\n" (reduction-graph-initial graph))]
          [else
           (hash-table-put! visited-ht term #t)
           (for-each loop (hash-table-get (reduction-graph-ht graph) term (lambda () null)))]))))
            
  
  ;; test-same-as-mz : reduction-graph -> void
  ;; prints out any terms that fail
  (define (test-same-as-mz graph)
    (let* ([orig-term (reduction-graph-initial graph)]
           [orig-value 'uncomputed]
           [bads null])
      (hash-table-for-each
       (reduction-graph-ht graph)
       (lambda (term _1)
	 (unless (equal? term orig-term)
	   (when (eq? orig-value 'uncomputed)
	     (set! orig-value (evaluate orig-term)))
	   (let ([term-value (evaluate term)])
	     (unless (value-equal? term-value orig-value)
	       (set! bads (cons (cons term term-value) bads)))))))
      (unless (null? bads)
        (for-each 
         (lambda (bad)
           (fprintf (current-error-port)
                    "        term: ~s\nevaluates to: ~s\n  reduces to: ~s\nevaluates to: ~s\n\n"
                    orig-term
                    orig-value
                    (car bad)
                    (cdr bad)))
         bads))))
  
  ;; determines if two values should be considered the same
  (define (value-equal? v1 v2)
    (or (and (procedure? v1)
             (procedure? v2))
        (equal? v1 v2)))
  
  ;; evaluate : sexp -> (union string? any)
  ;; produces a string if the result of evaluating
  ;; the term is an error. otherwise, produces the scheme value
  ;; improve the efficiency by not creating so many threads.
  (define (evaluate term)
    (let* ([ans #f]
           [ans-semaphore (make-semaphore 0)]
           [ok-sema (make-semaphore 0)]
           [only-one (make-semaphore 1)]
           [main-thread 
            (thread 
             (lambda ()
               (error-display-handler void) ;; to avoid printing `user break'
               (let ([eval-ans 
                      (with-handlers ([not-break-exn? exn-message])
                        (eval term))])
                 (semaphore-wait only-one)
                 (set! ans eval-ans)
                 (semaphore-post ans-semaphore))))]
           [timeout-thread
            (thread
             (lambda ()
               (sleep 3)
               (semaphore-wait only-one)
               (set! ans "timeout")
               (break-thread main-thread)
               (semaphore-post ans-semaphore)))])
      (semaphore-wait ans-semaphore)
      (kill-thread timeout-thread)
      (thread-wait timeout-thread)
      (thread-wait main-thread)
      ans))
  
  ;; substructure? : sexp sexp -> boolean
  ;; determines if sub occurs as a term inside full
  (define (left-substructure? sub full)
    (cond
      [(equal? sub full) #t]
      [(pair? full) (left-substructure? sub (car full))]
      [else #f]))
  '(begin (generation-depth 21)
         (generate-and-test 
          lang 'e '() 
          (lambda (x) 
            (when (equal?
		   '(((lambda (x) (lambda (y) x)) (lambda (x) y))
		     (lambda (x) x))
                   (reduction-graph-initial x))
              (write (reduction-graph-initial x))
              (newline)))))
  
  '(begin (generation-depth 4)
         (generate-and-test lang 'e reductions test-cycles))

  (begin (generation-depth 25)
         (generate-and-test lang 'e reductions test-same-as-mz))

 '(let ([searching-for '((lambda (x) (lambda (y) x)) (lambda (x) y))])
    (generation-depth 20)
    (generate-and-test 
     lang
     'e
     null 
     (lambda (term) 
       (let loop ([x (reduction-graph-initial term)])
	 (cond
	   [(equal? x searching-for)
	    (write (reduction-graph-initial term))
	    (newline)]
	   [(pair? x)
	    (loop (car x))]
	   [else (void)])))))
  
  '(begin (generation-depth 7)
         (generate-and-test lang 'e null void))
  
  '(gui lang reductions `((((lambda (x) (lambda (y) x)) 
                            (lambda (x) y)) 
                           (lambda (x) x))
                          (lambda (x) x))))
