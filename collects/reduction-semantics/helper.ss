(module helper mzscheme
  (require (lib "contract.ss")
           "reduction-semantics.ss")
  
  (define counter 0)
  (define (generate-string)
    (set! counter (add1 counter))
    (format "s~a" counter))
  
  (define (unique-names? l)
    (let ([ht (make-hash-table)])
      (andmap (lambda (n)
                (if (hash-table-get ht n (lambda () #f))
                    #f
                    (begin
                      (hash-table-put! ht n #t)
                      #t)))
              l)))
  
  (define (all-of P ?)
    ;; Traverse P as an sexp, and look for class-name uses:
    (let ([l (let loop ([sexp P])
               (cond
                 [(? sexp) (list sexp)]
                 [(pair? sexp) (append (loop (car sexp)) (loop (cdr sexp)))]
                 [else null]))]
          [ht (make-hash-table)])
      ;; Filter duplicates by hashing:
      (for-each (lambda (i) (hash-table-put! ht i #t)) l)
      (hash-table-map ht (lambda (k v) k))))
  
  (define-syntaxes (lang-match-lambda lang-match-lambda-memoized)
    (let ([generic
           (lambda (lam)
             (lambda (stx)
               (syntax-case stx ()
                 [(_ (id) grammar [pattern result] ...)
                  (with-syntax ([red (generate-temporaries #'(pattern ...))]
                                [lam lam])
                    (syntax/loc
                     stx
                     (let ([lang grammar]
                           [escape (make-parameter void)])
                       (let ([reds (list (reduction grammar pattern ((escape) (lambda (id) result)))
                                         ...)])
                         (lam (id)
                              ((let/ec esc
                                 (parameterize ([escape esc])
                                   (reduce reds id)
                                   (error 'lang-match-lambda "no pattern matched input: ~e" id)))
                               id))))))])))])
      (values
       (generic #'lambda)
       (generic #'lambda-memoized))))
  
  
    (define (transitive-closure orig)
    ;; Copy initial mapping:
    (let ([map (map (lambda (p) (list (car p) (cdr p))) orig)])
      ;; Extend the map list until nothing changes
      (let loop ()
        (let ([changed? #f])
          (for-each (lambda (pair)
                      (let ([mapping (cdr pair)])
                        (for-each (lambda (item)
                                    (let ([trans (ormap (lambda (transitive)
                                                          (and (not (memq transitive mapping))
                                                               transitive))
                                                        (cdr (assq item map)))])
                                      (when trans
                                        (append! pair (list trans))
                                        (set! changed? #t))))
                                  mapping)))
                    map)
          (when changed? (loop))))
      ;; Done
      map))
  
  (define-syntax (lambda-memoized stx)
    (syntax-case stx ()
      [(_ () body1 body ...)
       (syntax/loc stx (lambda () body1 body ...))]
      [(_ (arg) body1 body ...)
       (syntax/loc 
        stx 
        (let ([ht (make-hash-table 'weak)])
          (lambda (arg)
            (hash-table-get
             ht
             arg
             (lambda ()
               (let ([v (begin body1 body ...)])
                 (hash-table-put! ht arg v)
                 v))))))]
      [(_ (arg1 arg ...) body1 body ...)
       (syntax/loc
        stx
        (let ([memo (lambda-memoized (arg1) (lambda-memoized (arg ...) body1 body ...))])
          (lambda (arg1 arg ...)
            ((memo arg1) arg ...))))]))
  
  (define-syntax define-memoized
    (syntax-rules ()
      [(_ (f . args) body1 body ...)
       (define f (lambda-memoized args body1 body ...))]))


  ;; function-reduce*
  (define (function-reduce* reds expr done?)
    (cons 
     expr
     (if (done? expr)
	 null
	 (let ([l (reduce reds expr)])
	   (cond
	    [(null? l) null]
	    [(= 1 (length l))
	     (function-reduce* reds (car l) done?)]
	    [else
	     (error 'function-reduce* 
		    "found ~a possible steps from ~e"
		    (length l)
		    expr)])))))

  (provide
   define-memoized
   lambda-memoized
   lang-match-lambda
   lang-match-lambda-memoized)
  (provide/contract
   (function-reduce* ((listof red?) any? (any? . -> . boolean?) 
		      . -> . (listof any?)))
   (unique-names? ((listof symbol?) . -> . boolean?))
   (generate-string (-> string?))
   (all-of (any? (any? . -> . any) . -> . (listof any?)))
   (transitive-closure ((listof pair?) . -> . (listof (listof any?))))))
