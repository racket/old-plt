
(module specs mzscheme
  (provide (rename -contract contract)
           ->
           ->d
           ->*
           ->d*
           case->
	   opt->
           opt->*
           (rename -contract? contract?)
           provide/contract)

  (require-for-syntax mzscheme
                      (lib "list.ss")
		      (lib "name.ss" "syntax")
                      (lib "stx.ss" "syntax"))
  
  (require (lib "class.ss"))
  
  ;; (provide/contract (id expr) ...)
  ;; provides each `id' with the contract `expr'.
  (define-syntax (provide/contract provide-stx)
    (syntax-case provide-stx ()
      [(_ (id ctrct) ...)
       (andmap identifier? (syntax->list (syntax (id ...))))
       (with-syntax ([(id-rename ...)
		      (map (lambda (x)
                             (datum->syntax-object
                              provide-stx
                              (string->symbol
                               (format "provide/contract-id-~a-ACK-DONT_USE_ME"
				       (syntax-object->datum x)))))
                           (syntax->list (syntax (id ...))))]
		     [(contract-id ...)
                      (map (lambda (x)
                             (datum->syntax-object
                              provide-stx
                              (string->symbol
                               (format "provide/contract-contract-id-~a-ACK-DONT_USE_ME"
				       (syntax-object->datum x)))))
                           (syntax->list (syntax (id ...))))]
                     [pos-blame-stx (datum->syntax-object provide-stx 'here)]
                     [module-source-as-symbol (datum->syntax-object provide-stx 'module-source-as-symbol)])
         (syntax
          (begin
            (provide (rename id-rename id) ...)
            (require (lib "contract-helpers.scm" "framework" "private"))            
	    (define contract-id ctrct) ...
            (define-syntax id-rename
              (make-set!-transformer
               (lambda (stx)
                 (with-syntax ([neg-blame-stx (datum->syntax-object stx 'here)])
                   (syntax-case stx (set!)
                     [(set! _ body) (raise-syntax-error
                                     #f 
                                     "cannot mutate provide/contract identifier"
                                     stx
                                     (syntax _))]
                     [(_ arg (... ...))
                      (syntax 
                       ((-contract contract-id
                                   id
                                   (module-source-as-symbol (quote-syntax pos-blame-stx)) 
                                   (module-source-as-symbol (quote-syntax neg-blame-stx))
                                   (quote-syntax _))
                        arg
                        (... ...)))]
                     [_
                      (identifier? (syntax _))
                      (syntax 
                       (-contract contract-id
                                  id  
                                  (module-source-as-symbol (quote-syntax pos-blame-stx)) 
                                  (module-source-as-symbol (quote-syntax neg-blame-stx))
                                  (quote-syntax _)))])))))
            ...)))]
      [(_ clauses ...)
       (for-each
	(lambda (clause)
	  (syntax-case clause ()
	    [(x y)
	     (identifier? (syntax x))
	     (void)]
	    [(x y)
	     (raise-syntax-error
	      'provide/contract
	      "malformed clause (expected an identifier as first item in clause)"
	      provide-stx
	      (syntax x))]
	    [_ (raise-syntax-error
		'provide/contract
		"malformed clause (expected two items in each clause)"
		provide-stx
		clause)]))
	(syntax->list (syntax (clauses ...))))]))
  
  ;; raise-contract-error : (union syntax #f) symbol symbol string args ... -> alpha
  ;; doesn't return
  (define (raise-contract-error src-info to-blame other-party fmt . args)
    (let ([blame-src (if (syntax? src-info)
			 (let ([source (syntax-source src-info)]
			       [line (syntax-line src-info)]
			       [col (syntax-column src-info)]
			       [pos (syntax-position src-info)])
			   (cond
			      [(and (string? source) line col)
			       (format "~a: ~a.~a: " source line col)]
			      [(and line col)
			       (format "~a.~a: " line col)]
			      [(and (string? source) pos)
			       (format "~a: ~a: " source pos)]
			      [pos
			       (format "~a: " pos)]
			      [else ""]))
			 "")]
	  [specific-blame
	   (let ([datum (syntax-object->datum src-info)])
	     (if (symbol? datum)
		 (format "broke ~a's contract" datum)
		 "failed contract"))])
    (raise
     (make-exn
      (string->immutable-string
       (string-append (format "~a~a: ~a ~a: "
			      blame-src
			      other-party
			      to-blame
			      specific-blame)
		      (apply format fmt args)))
      (current-continuation-marks)))))
  
  ;; contract = (make-contract (alpha sym sym sym -> alpha))
  ;; generic contract container
  (define-struct contract (f))

  ;; flat-named-contract = (make-flat-named-contract string (any -> boolean))
  ;; this holds flat contracts that have names for error reporting
  (define-struct flat-named-contract (type-name predicate))
  (provide (rename build-flat-named-contract flat-named-contract)
           flat-named-contract-type-name
           flat-named-contract-predicate)
  (define build-flat-named-contract
    (let ([flat-named-contract
	   (lambda (name contract)
	     (unless (and (string? name)
			  (procedure? contract)
			  (procedure-arity-includes? contract 1))
	       (error 'flat-named-contract "expected string and procedure of one argument as arguments, given: ~e and ~e"
		      name contract))
	     (make-flat-named-contract name contract))])
      flat-named-contract))

  (define-syntax -contract
    (lambda (stx)
      (syntax-case stx ()
        [(_ a-contract to-check pos-blame-e neg-blame-e)
         (with-syntax ([src-loc (datum->syntax-object stx 'here)])
           (syntax
            (-contract a-contract to-check pos-blame-e neg-blame-e
                       (quote-syntax src-loc))))]
        [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
         (let ([name (syntax-local-infer-name (syntax a-contract-e))])
           (with-syntax ([named-a-contract-e
                          (if name
                              (syntax-property (syntax a-contract-e) 'inferred-name name)
                              (syntax a-contract-e))])
             (syntax
              (let ([a-contract named-a-contract-e]
                    [name to-check]
                    [neg-blame neg-blame-e]
                    [pos-blame pos-blame-e]
                    [src-info src-info-e])
                (unless (-contract? a-contract)
                  (error 'contract "expected a contract as first argument, given: ~e, other args ~e ~e ~e ~e" 
                         a-contract
                         name
                         pos-blame
                         neg-blame
                         src-info))
                (unless (and (symbol? neg-blame)
                             (symbol? pos-blame))
                  (error 'contract "expected symbols as names for assigning blame, given: ~e and ~e, other args ~e ~e ~e"
                         neg-blame pos-blame
                         a-contract 
                         name
                         src-info))
                (unless (syntax? src-info)
                  (error 'contract "expected syntax as last argument, given: ~e, other args ~e ~e ~e ~e"
                         src-info
                         neg-blame 
                         pos-blame
                         a-contract 
                         name))
                (check-contract a-contract name pos-blame neg-blame src-info)))))])))
  
  (define-syntaxes (-> ->* ->d ->d* case->)
    (let ()
      ;; Each of the /h functions builds three pieces of syntax:
      ;;  - code that does error checking for the contract specs
      ;;    (were the arguments all contracts?)
      ;;  - code that does error checking on the contract'd value
      ;;    (is a function of the right arity?)
      ;;  - a piece of syntax that has the arguments to the wrapper
      ;;    and the body of the wrapper.
      ;; They are combined into a lambda for the -> ->* ->d ->d* macros,
      ;; and combined into a case-lambda for the case-> macro.
      
      ;; ->/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
      (define (->/h stx)
        (syntax-case stx ()
          [(_) (raise-syntax-error '-> "expected at least one argument" stx)]
          [(_ ct ...)
	   (let* ([rng-normal (car (last-pair (syntax->list (syntax (ct ...)))))]
		  [ignore-range-checking?
		   (syntax-case rng-normal (any)
		     [any #t]
		     [_ #f])])
	     (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (ct ...))))]
			   [rng (if ignore-range-checking?
				    (syntax any?)  ;; hack to simplify life...
				    rng-normal)])
	       (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
			     [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
			     [arity (length (syntax->list (syntax (dom ...))))])
		 (let ([->add-outer-check
			(lambda (body)
			  (with-syntax ([body body])
			    (syntax
			     (let ([dom-x dom] ...
				   [rng-x rng])
			       (unless (-contract? dom-x)
				 (error '-> "expected contract as argument, given: ~e" dom-x)) ...
				 (unless (-contract? rng-x)
				   (error '-> "expected contract as argument, given: ~e" rng-x))
				 body))))]
		       [->body (syntax (->* (dom-x ...) (rng-x)))])
		   (let-values ([(->*add-outer-check ->*make-inner-check ->*make-body) (->*/h ->body)])
		     (values 
		      (lambda (body) (->add-outer-check (->*add-outer-check body)))
		      (lambda (stx) (->*make-inner-check stx))
		      (if ignore-range-checking?
			  (lambda (stx)
			    (with-syntax ([(val pos-blame neg-blame src-info) stx])
			      (syntax
			       ((arg-x ...)
				(val
				 (check-contract dom-x arg-x neg-blame pos-blame src-info)
				 ...)))))
			  (lambda (stx)
			    (->*make-body stx)))))))))]))

      ;; ->*/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
      (define (->*/h stx)
        (syntax-case stx ()
          [(_ (dom ...) (rng ...))
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                         [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (values
              (lambda (body)
                (with-syntax ([body body])
                  (syntax
                   (let ([dom-x dom] ...
                         [rng-x rng] ...)
                     (unless (-contract? dom-x)
                       (error '->* "expected contract as argument, given: ~e" dom-x)) ...
                     (unless (-contract? rng-x)
                       (error '->* "expected contract as argument, given: ~e" rng-x)) ...
                     body))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   (unless (and (procedure? val)
                                (procedure-arity-includes? val arity))
                     (raise-contract-error
                      src-info
                      pos-blame
		      neg-blame
                      "expected a procedure that accepts ~a arguments, given: ~e"
                      arity
                      val)))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   ((arg-x ...)
                    (let-values ([(res-x ...)
                                  (val
                                   (check-contract dom-x arg-x neg-blame pos-blame src-info)
                                   ...)])
                      (values (check-contract
                               rng-x 
                               res-x
                               pos-blame
                               neg-blame
                               src-info)
                              ...))))))))]
          [(_ (dom ...) rest (rng ...))
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                         [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (values
              (lambda (body)
                (with-syntax ([body body])
                  (syntax
                   (let ([dom-x dom] ...
                         [dom-rest-x rest]
                         [rng-x rng] ...)
                     (unless (-contract? dom-x)
                       (error '->* "expected contract for domain position, given: ~e" dom-x)) ...
                     (unless (-contract? dom-rest-x)
                       (error '->* "expected contract for rest position, given: ~e" dom-rest-x))
                     (unless (-contract? rng-x)
                       (error '->* "expected contract for range position, given: ~e" rng-x)) ...
                     body))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   (unless (procedure? val)
                     (raise-contract-error
                      src-info
                      pos-blame
		      neg-blame
                      "expected a procedure that accepts ~a arguments, given: ~e"
                      arity
                      val)))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   ((arg-x ... . rest-arg-x)
                    (let-values ([(res-x ...)
                                  (apply
                                   val
                                   (check-contract dom-x arg-x neg-blame pos-blame src-info)
                                   ...
                                   (check-contract dom-rest-x rest-arg-x neg-blame pos-blame src-info))])
                      (values (check-contract
                               rng-x 
                               res-x
                               pos-blame
                               neg-blame
                               src-info)
                              ...))))))))]))
      
      ;; ->d/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
      (define (->d/h stx)
        (syntax-case stx ()
          [(_) (raise-syntax-error '->d "expected at least one argument" stx)]
          [(_ ct ...)
           (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (ct ...))))]
                         [rng (car (last-pair (syntax->list (syntax (ct ...)))))])
             (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                           [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                           [arity (length (syntax->list (syntax (dom ...))))])
               (values
                (lambda (body)
                  (with-syntax ([body body])
                    (syntax
                     (let ([dom-x dom] ...
                           [rng-x rng])
                       (unless (-contract? dom-x)
                         (error '->d "expected contract as argument, given: ~e" dom-x)) ...
                       (unless (and (procedure? rng-x)
                                    (procedure-arity-includes? rng-x arity))
                         (error '->d "expected range portion to be a function that takes ~a arguments, given: ~e"
                                arity
                                rng-x))
                       body))))
                (lambda (stx)
                  (with-syntax ([(val pos-blame neg-blame src-info) stx])
                    (syntax
                     (unless (and (procedure? val)
                                  (procedure-arity-includes? val arity))
                       (raise-contract-error
                        src-info
                        pos-blame
			neg-blame
                        "expected a procedure that accepts ~a arguments, given: ~e"
                        arity
                        val)))))
                (lambda (stx)
                  (with-syntax ([(val pos-blame neg-blame src-info) stx])
                    (syntax
                     ((arg-x ...)
                      (let ([rng-contract (rng-x arg-x ...)])
                        (unless (-contract? rng-contract)
                          (error '->d "expected range portion to return a contract, given: ~e"
                                 rng-contract))
                        (check-contract 
                         rng-contract
                         (val (check-contract dom-x arg-x neg-blame pos-blame src-info) ...)
                         pos-blame
                         neg-blame
                         src-info)))))))))]))
      
      ;; ->d*/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
      (define (->d*/h stx)
        (syntax-case stx ()
          [(_ (dom ...) rng-mk)
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (values
              (lambda (body)
                (with-syntax ([body body])
                  (syntax
                   (let ([dom-x dom] ...
                         [rng-mk-x rng-mk])
                     (unless (-contract? dom-x)
                       (error '->*d "expected contract as argument, given: ~e" dom-x)) ...
                     (unless (and (procedure? rng-mk-x)
                                  (procedure-arity-includes? rng-mk-x arity))
                       (error '->*d "expected range position to be a procedure that accepts ~a arguments, given: ~e"
                              arity rng-mk-x))
                     body))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   (unless (and (procedure? val)
                                (procedure-arity-includes? val arity))
                     (raise-contract-error
                      src-info
                      pos-blame
		      neg-blame
                      "expected a procedure that accepts ~a arguments, given: ~e"
                      arity
                      val)))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   ((arg-x ...)
                    (call-with-values
                     (lambda ()
                       (rng-mk-x arg-x ...))
                     (lambda rng-contracts
                       (call-with-values
                        (lambda ()
                          (val
                           (check-contract dom-x arg-x neg-blame pos-blame src-info)
                           ...))
                        (lambda results
                          (unless (= (length results) (length rng-contracts))
                            (error '->d* 
                                   "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
                                   (length results) (length rng-contracts)))
                          (apply 
                           values
                           (map (lambda (rng-contract result)
                                  (check-contract
                                   rng-contract
                                   result
                                   pos-blame
                                   neg-blame
                                   src-info))
                                rng-contracts
                                results))))))))))))]
          [(_ (dom ...) rest rng-mk)
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (values
              (lambda (body)
                (with-syntax ([body body])
                  (syntax
                   (let ([dom-x dom] ...
                         [dom-rest-x rest]
                         [rng-mk-x rng-mk])
                     (unless (-contract? dom-x)
                       (error '->*d "expected contract as argument, given: ~e" dom-x)) ...
                     (unless (-contract? dom-rest-x)
                       (error '->*d "expected contract for rest argument, given: ~e" dom-rest-x))
                     (unless (procedure? rng-mk-x)
                       (error '->*d "expected range position to be a procedure that accepts ~a arguments, given: ~e"
                              arity rng-mk-x))
                     body))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   (unless (procedure? val)
                     (raise-contract-error
                      src-info
                      pos-blame
		      neg-blame
                      "expected a procedure that accepts ~a arguments, given: ~e"
                      arity
                      val)))))
              (lambda (stx)
                (with-syntax ([(val pos-blame neg-blame src-info) stx])
                  (syntax
                   ((arg-x ... . rest-arg-x)
                    (call-with-values
                     (lambda ()
                       (apply rng-mk-x arg-x ... rest-arg-x))
                     (lambda rng-contracts
                       (call-with-values
                        (lambda ()
                          (apply 
                           val
                           (check-contract dom-x arg-x neg-blame pos-blame src-info)
                           ...
                           (check-contract dom-rest-x rest-arg-x neg-blame pos-blame src-info)))
                        (lambda results
                          (unless (= (length results) (length rng-contracts))
                            (error '->d* 
                                   "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
                                   (length results) (length rng-contracts)))
                          (apply 
                           values
                           (map (lambda (rng-contract result)
                                  (check-contract
                                   rng-contract
                                   result
                                   pos-blame
                                   neg-blame
                                   src-info))
                                rng-contracts
                                results))))))))))))]))

      ;; make-/f :    (syntax -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))) 
      ;;           -> (syntax -> syntax)
      (define (make-/f /h)
        (lambda (stx)
          (let-values ([(add-outer-check make-inner-check make-main) (/h stx)])
            (let ([outer-args (syntax (val pos-blame neg-blame src-info))])
              (with-syntax ([outer-args outer-args]
                            [inner-check (make-inner-check outer-args)]
                            [(inner-args body) (make-main outer-args)])
                (with-syntax ([inner-lambda 
                               (set-inferred-name-from
                                stx
                                (syntax (lambda inner-args body)))])
                  (add-outer-check
                   (syntax
                    (make-contract
                     (lambda outer-args
                       inner-check
                       inner-lambda))))))))))
      
      ;; set-inferred-name-from : syntax syntax -> syntax
      (define (set-inferred-name-from with-name to-be-named)
        (let ([name (syntax-local-infer-name with-name)])
          (if name
              (syntax-property to-be-named 'inferred-name name)
              to-be-named)))
      
      ;; ->/f : syntax -> syntax
      ;; the transformer for the -> macro
      (define ->/f (make-/f ->/h))

      ;; ->*/f : syntax -> syntax
      ;; the transformer for the ->* macro
      (define ->*/f (make-/f ->*/h))

      ;; ->d/f : syntax -> syntax
      ;; the transformer for the ->d macro
      (define ->d/f (make-/f ->d/h))

      ;; ->d*/f : syntax -> syntax
      ;; the transformer for the ->d* macro
      (define ->d*/f (make-/f ->d*/h))

      ;; case->/f : syntax -> syntax
      ;; the transformer for the case-> macro
      (define (case->/f stx)
        (syntax-case stx ()
          [(_ case ...)
           (let-values ([(add-outer-check make-inner-check make-bodies)
                         (case->/h (syntax->list (syntax (case ...))))])
             (let ([outer-args (syntax (val pos-blame neg-blame src-info))])
               (with-syntax ([outer-args outer-args]
                             [(inner-check ...) (make-inner-check outer-args)]
                             [(body ...) (make-bodies outer-args)])
                 (with-syntax ([inner-lambda 
                                (set-inferred-name-from
                                 stx
                                 (syntax (case-lambda body ...)))])
                   (add-outer-check
                    (syntax
                     (make-contract
                      (lambda outer-args
                        inner-check ...
                        inner-lambda))))))))]))
      
      ;; case->/h : (listof syntax) -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
      ;; like the other /h functions, but composes the wrapper functions
      ;; together and combines the cases of the case-lambda into a single list.
      (define (case->/h cases)
        (let loop ([cases cases])
          (cond
            [(null? cases) (values (lambda (x) x)
                                   (lambda (args) (syntax ()))
                                   (lambda (args) (syntax ())))]
            [else
             (let ([/h (syntax-case (car cases) (-> ->* ->d ->d*)
                         [(-> . args) ->/h]
                         [(->* . args) ->*/h]
                         [(->d . args) ->d/h]
                         [(->d* . args) ->d*/h])])
               (let-values ([(add-outer-checks make-inner-checks make-bodies) (loop (cdr cases))]
                            [(add-outer-check make-inner-check make-body) (/h (car cases))])
                 (values
                  (lambda (x) (add-outer-check (add-outer-checks x)))
                  (lambda (args)
                    (with-syntax ([checks (make-inner-checks args)]
                                  [check (make-inner-check args)])
                      (syntax (check . checks))))
                  (lambda (args)
                    (with-syntax ([case (make-body args)]
                                  [cases (make-bodies args)])
                      (syntax (case . cases)))))))])))

      (define (all-but-last l)
        (cond
          [(null? l) (error 'all-but-last "bad input")]
          [(null? (cdr l)) null]
          [else (cons (car l) (all-but-last (cdr l)))]))
      
      (values ->/f ->*/f ->d/f ->d*/f case->/f)))

  (define-syntax (opt-> stx)
    (syntax-case stx ()
      [(_ (reqs ...) (opts ...) res)
       (syntax (opt->* (reqs ...) (opts ...) (res)))]))
  
  (define-syntax (opt->* stx)
    (syntax-case stx ()
      [(_ (reqs ...) (opts ...) (ress ...))
       (let* ([res-vs (generate-temporaries (syntax->list (syntax (ress ...))))]
	      [req-vs (generate-temporaries (syntax->list (syntax (reqs ...))))]
	      [opt-vs (generate-temporaries (syntax->list (syntax (opts ...))))]
	      [cases
	       (reverse
		(let loop ([opt-vs (reverse opt-vs)])
		  (cond
		    [(null? opt-vs) (list req-vs)]
		    [else (cons (append req-vs (reverse opt-vs))
				(loop (cdr opt-vs)))])))])
	 (with-syntax ([((double-res-vs ...) ...) (map (lambda (x) res-vs) cases)]
		       [(res-vs ...) res-vs]
                       [(req-vs ...) req-vs]
		       [(opt-vs ...) opt-vs]
		       [((case-doms ...) ...) cases])
	   (syntax
	    (let ([res-vs ress] ...
		  [req-vs reqs] ...
		  [opt-vs opts] ...)
	      (case-> (->* (case-doms ...) (double-res-vs ...)) ...)))))]))

  (define -contract?
    (let ([contract?
           (lambda (val)
             (or (contract? val)  ;; refers to struct predicate
		 (flat-named-contract? val)
                 (and (procedure? val)
                      (procedure-arity-includes? val 1))))])
      contract?))
  
  (define (check-contract contract val pos neg src-info)
    (cond
      [(contract? contract)
       ((contract-f contract) val pos neg src-info)]
      [(flat-named-contract? contract)
       (if ((flat-named-contract-predicate contract) val)
           val
           (raise-contract-error
            src-info
            pos
	    neg
            "expected type <~a>, given: ~e"
	    (flat-named-contract-type-name contract)
	    val))]
      [else
       (if (contract val)
           val
           (raise-contract-error
            src-info
            pos
	    neg
            "~agiven: ~e"
	    (predicate->type-name contract)
	    val))]))

  ;; predicate->type-name : function -> string
  ;; if the function has a name and the name ends
  ;; with a question mark, turn it into a mzscheme
  ;; style type name
  (define (predicate->type-name pred)
    (let* ([name (object-name pred)])
      (if name
	  (let ([m (regexp-match "(.*)\\?" (symbol->string name))])
	    (if m
		(format "expected type <~a>, " (cadr m))
		""))
	  "")))

  (provide union)
  (define (union . args)
    (for-each
     (lambda (x) 
       (unless (-contract? x)
         (error 'union "expected procedures of arity 1, flat-named-contracts, or -> contracts, given: ~e" x)))
     args)
    (let-values ([(contracts procs)
                  (let loop ([ctcs null]
                             [procs null]
                             [args args])
                    (cond
                      [(null? args) (values ctcs procs)]
                      [else (let ([arg (car args)])
                              (if (contract? arg)
                                  (loop (cons arg ctcs) procs (cdr args))
                                  (loop ctcs (cons arg procs) (cdr args))))]))])
      (unless (or (null? contracts)
                  (null? (cdr contracts)))
        (error 'union "expected at most one function contract, given: ~e" args))
      (make-contract
       (lambda (val pos neg src-info)
         (cond
           [(ormap (lambda (proc)
		     (if (flat-named-contract? proc)
			 ((flat-named-contract-predicate proc) val)
			 (proc val)))
		   procs)
            val]
           [(null? contracts)
            (raise-contract-error src-info pos neg "union failed, given: ~e" val)]
           [(null? (cdr contracts))
            ((contract-f (car contracts)) val pos neg src-info)])))))

  (provide and/f or/f 
           >=/c <=/c </c >/c 
	   natural-number?
           false? any? 
	   printable?
           symbols
           subclass?/c implementation?/c is-a?/c
           listof vectorof cons/p list/p
	   mixin-contract make-mixin-contract)

  (define (symbols . ss)
    (unless ((length ss) . >= . 1)
      (error 'symbols "expected at least one argument"))
    (unless (andmap symbol? ss)
      (error 'symbols "expected symbols as arguments, given: ~a"
	     (apply string-append (map (lambda (x) (format "~e " x)) ss))))
    (make-flat-named-contract
     (apply string-append
	    (format "'~a" (car ss))
	    (map (lambda (x) (format ", '~a" x)) (cdr ss)))
     (lambda (x)
       (memq x ss))))

  (define (printable? x)
    (or (symbol? x)
	(string? x)
	(boolean? x)
	(char? x)
	(null? x)
	(number? x)
	(and (pair? x)
	     (printable? (car x))
	     (printable? (cdr x)))
	(and (vector? x)
	     (andmap printable? (vector->list x)))
	(and (box? x)
	     (printable? (unbox x)))))

  (define (and/f . fs)
    (for-each
     (lambda (x) 
       (unless (or (flat-named-contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'and/f "expected procedures of arity 1 or <flat-named-contract>s, given: ~e" x)))
     fs)
    (lambda (x)
      (andmap (lambda (f) 
                (if (flat-named-contract? f)
                    ((flat-named-contract-predicate f) x)
                    (f x)))
              fs)))

  (define (or/f . fs)
    (for-each
     (lambda (x) 
       (unless (or (flat-named-contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'or/f "expected procedures of arity 1 or <flat-named-contract>s, given: ~e" x)))
     fs)
    (lambda (x)
      (ormap (lambda (f)
               (if (flat-named-contract? f)
                   ((flat-named-contract-predicate f) x)
                   (f x)))
             fs)))
  
  (define (>=/c x)
    (make-flat-named-contract
     (format "number >= ~a" x)
     (lambda (y) (and (number? y) (>= y x)))))
  (define (<=/c x)
    (make-flat-named-contract
     (format "number <= ~a" x)
     (lambda (y) (and (number? y) (<= y x)))))
  (define (</c x)
    (make-flat-named-contract
     (format "number < ~a" x)
     (lambda (y) (and (number? y) (< y x)))))
  (define (>/c x)
    (make-flat-named-contract
     (format "number > ~a" x)
     (lambda (y) (and (number? y) (> y x)))))

  (define (natural-number? x)
    (and (number? x)
	 (integer? x)
	 (x . >= . 0)))

  (define (is-a?/c <%>)
    (unless (or (interface? <%>)
		(class? <%>))
      (error 'is-a?/c "expected <interface> or <class>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (make-flat-named-contract
       (if name
	   (format "instance of ~a" name)
	   "instance of <<unknown>>")
       (lambda (x) (is-a? x <%>)))))

  (define (subclass?/c %)
    (unless (class? %)
      (error 'subclass?/c "expected type <class>, given: ~e" %))
    (let ([name (object-name %)])
      (make-flat-named-contract
       (if name
	   (format "subclass of ~a" name)
	   "subclass of <<unknown>>")
       (lambda (x) (subclass? x %)))))

  (define (implementation?/c <%>)
    (unless (interface? <%>)
      (error 'implementation?/c "expected <interface>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (make-flat-named-contract
       (if name
	   (format "implementation of ~a" name)
	   "implementation of <<unknown>>")
       (lambda (x) (implementation? x <%>)))))

  (define (false? x) (not x))
  (define (any? x) #t)

  (define (listof p)
    (lambda (v)
      (and (list? v)
	   (andmap p v))))
  
  (define (vectorof p)
    (lambda (v)
      (and (vector? v)
	   (andmap p (vector->list v)))))

  (define (cons/p hdp tlp)
    (lambda (x)
      (and (pair? x)
	   (hdp (car x))
	   (tlp (cdr x)))))

  (define (list/p . args)
    (let loop ([args args])
      (cond
	[(null? args) null?]
	[else (cons/p (car args) (loop (cdr args)))])))

  (define mixin-contract
    (class?
     . ->d .
     subclass?/c))

  (define (make-mixin-contract . %/<%>s)
    ((and/f class? (apply and/f (map sub/impl?/c %/<%>s)))
     . ->d .
     subclass?/c))

  (define (sub/impl?/c %/<%>)
    (cond
      [(interface? %/<%>) (implementation?/c %/<%>)]
      [(class? %/<%>) (subclass?/c %/<%>)]
      [else (error 'make-mixin-contract "unknown input ~e" %/<%>)])))
