#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#

(module contract mzscheme
  (provide (rename -contract contract)
           ->
           ->d
           ->*
           ->d*
           case->
	   opt->
           opt->*
           ;class-contract
           ;class-contract/prim
           object-contract ;; not yet good enough
           provide/contract
           define/contract
	   contract?
           contract-name
           flat-contract?
           flat-contract
           flat-contract-predicate
	   flat-named-contract?
           flat-named-contract
           flat-named-contract-type-name)

  (require-for-syntax mzscheme
                      "list.ss"
                      (lib "stx.ss" "syntax")
                      (lib "name.ss" "syntax"))
  
  (require "private/class-sneaky.ss"
           "etc.ss")
  
  (require (lib "contract-helpers.scm" "mzlib" "private"))
  (require-for-syntax (prefix a: (lib "contract-helpers.scm" "mzlib" "private")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  deprecated
  ;;

  (define-syntax (deprecated stx)
    (syntax-case stx ()
      [(_ old new)
       (syntax 
	(define-syntax (old stx)
	  (syntax-case stx ()
	    [(_ args (... ...))
	     (fprintf
	      (current-error-port)
	      "WARNING: ~a is deprecated, use ~a instead ~a:~a.~a\n"
	      'old
	      'new
	      (syntax-source stx)
	      (syntax-line stx)
	      (syntax-column stx))
	     (syntax (new args (... ...)))])))]))

  (provide or/f and/f flat-named-contract-predicate)
  (deprecated or/f union)
  (deprecated and/f and/c)
  (deprecated flat-named-contract-predicate flat-contract-predicate)
  (deprecated flat-named-contract? flat-contract?)
  (deprecated flat-named-contract-type-name contract-name)

  ;;
  ;; end deprecated
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  
;                                                                                                    
;                                                                                                    
;                                                                                                    
;       ;           ;;; ;                     ;                                                      
;       ;          ;                          ;                                                      
;       ;          ;                         ;                           ;                       ;   
;    ;; ;    ;;;  ;;;;  ;   ; ;;     ;;;     ;     ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;  ;;   ;   ;  ;    ;   ;;  ;   ;   ;    ;    ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;    ;  ;    ;  ;    ;   ;   ;  ;    ;    ;   ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;    ;  ;;;;;;  ;    ;   ;   ;  ;;;;;;   ;    ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;    ;  ;       ;    ;   ;   ;  ;        ;    ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;  ;;   ;      ;    ;   ;   ;   ;       ;     ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;; ;    ;;;;  ;    ;   ;   ;    ;;;;  ;       ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                                          ;                                                         
;                                          ;                                                         
;                                                                                                    

  
  ;; (define/contract id contract expr)
  ;; defines `id' with `contract'; initially binding
  ;; it to the result of `expr'.  These variables may not be set!'d.
  (define-syntax (define/contract define-stx)
    (syntax-case define-stx ()
      [(_ name contract-expr expr)
       (identifier? (syntax name))
       (with-syntax ([pos-blame-stx (datum->syntax-object define-stx 'here)]
                     [contract-id 
                      (a:mangle-id define-stx
                                   "define/contract-contract-id"
                                   (syntax name))]
                     [id (a:mangle-id define-stx
                                      "define/contract-id"
                                      (syntax name))])
         (syntax/loc define-stx 
          (begin
            (define contract-id contract-expr)
            (define-syntax name
              (make-set!-transformer
               (lambda (stx)
                 (with-syntax ([neg-blame-str (or (a:build-src-loc-string stx) "")])
                   (syntax-case stx (set!)
                     [(set! _ arg) 
                      (raise-syntax-error 'define/contract
                                          "cannot set! a define/contract variable" 
                                          stx 
                                          (syntax _))]
                     [(_ arg (... ...))
                      (syntax/loc stx
                       ((-contract contract-id
                                   id
                                   (syntax-object->datum (quote-syntax _))
                                   (string->symbol neg-blame-str)
                                   (quote-syntax _))
                        arg
                        (... ...)))]
                     [_
                      (identifier? (syntax _))
                      (syntax/loc stx
                       (-contract contract-id
                                  id  
                                  (syntax-object->datum (quote-syntax _)) 
                                  (string->symbol neg-blame-str)
                                  (quote-syntax _)))])))))
            (define id (let ([name expr]) name))  ;; let for procedure naming
            )))]
      [(_ name contract-expr expr)
       (raise-syntax-error 'define/contract "expected identifier in first position"
                           define-stx
                           (syntax name))])) 
  
  
;                                                                                                            
;                                                                                                            
;                                                                                                            
;                               ;       ;             ;                                                      
;                                       ;             ;                                                      
;                                       ;            ;                           ;                       ;   
;   ; ;;    ; ;   ;;;   ;     ; ;    ;; ;    ;;;     ;     ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;;  ;   ;;   ;   ;   ;   ;  ;   ;  ;;   ;   ;    ;    ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;   ;    ;  ;   ;     ;  ;   ;  ;  ;    ;  ;    ;    ;   ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;   ;    ;  ;   ;     ;   ; ;   ;  ;    ;  ;;;;;;   ;    ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;   ;    ;  ;   ;     ;   ; ;   ;  ;    ;  ;        ;    ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;;  ;   ;    ;   ;     ;    ;   ;  ;;   ;       ;     ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;   ; ;;    ;     ;;;      ;    ;    ;; ;    ;;;;  ;       ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;   ;                                              ;                                                         
;   ;                                              ;                                                         
;   ;                                                                                                        

  
  ;; (provide/contract p/c-ele ...)
  ;; p/c-ele = (id expr) | (struct (id expr) ...)
  ;; provides each `id' with the contract `expr'.
  (define-syntax (provide/contract provide-stx)
    (syntax-case provide-stx (struct)
      [(_ p/c-ele ...)
       (let ()
         
         ;; code-for-each-clause : (listof syntax) -> (listof syntax)
         ;; constructs code for each clause of a provide/contract
         (define (code-for-each-clause clauses)
           (cond
             [(null? clauses) null]
             [else
              (let ([clause (car clauses)])
                (syntax-case clause (struct)
                  [(struct struct-name ((field-name contract) ...))
                   (and (identifier? (syntax struct-name))
                        (andmap identifier? (syntax->list (syntax (field-name ...)))))
                   (let ([sc (build-struct-code provide-stx
                                                (syntax struct-name)
                                                (syntax->list (syntax (field-name ...)))
                                                (syntax->list (syntax (contract ...))))])
                     (cons sc (code-for-each-clause (cdr clauses))))]
                  [(struct name)
                   (identifier? (syntax name))
                   (raise-syntax-error 'provide/contract
                                       "missing fields"
                                       provide-stx
                                       clause)]
                  [(struct name (fields ...))
                   (for-each (lambda (field)
                               (syntax-case field ()
                                 [(x y) 
                                  (identifier? (syntax x)) 
                                  (void)]
                                 [(x y) 
                                  (raise-syntax-error 'provide/contract
                                                      "malformed struct field, expected identifier"
                                                      provide-stx
                                                      (syntax x))]
                                 [else
                                  (raise-syntax-error 'provide/contract
                                                      "malformed struct field"
                                                      provide-stx
                                                      field)]))
                             (syntax->list (syntax (fields ...))))
                   
                   ;; if we didn't find a bad field something is wrong!
                   (raise-syntax-error 'provide/contract "internal error" provide-stx clause)]
                  [(struct name . fields)
                   (raise-syntax-error 'provide/contract
                                       "malformed struct fields"
                                       provide-stx
                                       clause)]
                  [(name contract)
                   (identifier? (syntax name))
                   (cons (code-for-one-id provide-stx (syntax name) (syntax contract))
                         (code-for-each-clause (cdr clauses)))]
                  [(name contract)
                   (raise-syntax-error 'provide/contract
                                       "expected identifier"
                                       provide-stx
                                       (syntax name))]
                  [unk
                   (raise-syntax-error 'provide/contract
                                       "malformed clause"
                                       provide-stx
                                       (syntax unk))]))]))
         
         ;; build-struct-code : syntax syntax (listof syntax) (listof syntax) -> syntax
         ;; constructs the code for a struct clause
         ;; first arg is the original syntax object, for source locations
         (define (build-struct-code stx struct-name field-names field-contracts)
           (let* ([field-contract-ids (map (lambda (field-name) 
                                             (a:mangle-id provide-stx
                                                          "provide/contract-field-contract"
                                                          field-name
                                                          struct-name))
                                           field-names)]
                  [selector-ids (map (lambda (field-name) 
                                       (build-selector-id struct-name field-name))
                                     field-names)]
                  [mutator-ids (map (lambda (field-name) 
                                      (build-mutator-id struct-name field-name))
                                    field-names)]
                  [predicate-id (build-predicate-id struct-name)]
                  [constructor-id (build-constructor-id struct-name)])
             (with-syntax ([(selector-codes ...)
                            (map (lambda (selector-id field-contract-id) 
                                   (code-for-one-id stx
                                                    selector-id
                                                    (build-selector-contract struct-name 
                                                                             predicate-id
                                                                             field-contract-id)))
                                 selector-ids
                                 field-contract-ids)]
                           [(mutator-codes ...)
                            (map (lambda (mutator-id field-contract-id)
                                   (code-for-one-id stx
                                                    mutator-id 
                                                    (build-mutator-contract struct-name 
                                                                            predicate-id
                                                                            field-contract-id)))
                                 mutator-ids
                                 field-contract-ids)]
                           [predicate-code (code-for-one-id stx predicate-id (syntax (-> any? boolean?)))]
                           [constructor-code (code-for-one-id
                                              stx
                                              constructor-id
                                              (build-constructor-contract stx
                                                                          field-contract-ids 
                                                                          predicate-id))]
                           [(field-contracts ...) field-contracts]
                           [(field-contract-ids ...) field-contract-ids]
                           [struct-name struct-name])
               (syntax/loc stx
                (begin
                  (define field-contract-ids field-contracts) ...
                  selector-codes ...
                  mutator-codes ...
                  predicate-code
                  constructor-code
                  (provide struct-name))))))
         
         ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
         (define (build-constructor-contract stx field-contract-ids predicate-id)
           (with-syntax ([(field-contract-ids ...) field-contract-ids]
                         [predicate-id predicate-id])
             (syntax/loc stx
               (field-contract-ids 
                ...
                . -> . 
                (let ([predicate-id (lambda (x) (predicate-id x))]) predicate-id)))))
         
         ;; build-selector-contract : syntax syntax -> syntax
         ;; constructs the contract for a selector
         (define (build-selector-contract struct-name predicate-id field-contract-id)
           (with-syntax ([field-contract-id field-contract-id]
                         [predicate-id predicate-id])
             (syntax ((let ([predicate-id (lambda (x) (predicate-id x))]) predicate-id)
                      . -> .
                      field-contract-id))))
         
         ;; build-mutator-contract : syntax syntax -> syntax
         ;; constructs the contract for a selector
         (define (build-mutator-contract struct-name predicate-id field-contract-id)
           (with-syntax ([field-contract-id field-contract-id]
                         [predicate-id predicate-id])
             (syntax ((let ([predicate-id (lambda (x) (predicate-id x))]) predicate-id)
                      field-contract-id
                      . -> .
                      void?))))
         
         ;; build-constructor-id : syntax -> syntax
         ;; constructs the name of the selector for a particular field of a struct
         (define (build-constructor-id struct-name)
           (datum->syntax-object
            struct-name
            (string->symbol
             (string-append
              "make-"
              (symbol->string (syntax-object->datum struct-name))))))
         
         ;; build-predicate-id : syntax -> syntax
         ;; constructs the name of the selector for a particular field of a struct
         (define (build-predicate-id struct-name)
           (datum->syntax-object
            struct-name
            (string->symbol
             (string-append
              (symbol->string (syntax-object->datum struct-name))
              "?"))))
         
         ;; build-selector-id : syntax syntax -> syntax
         ;; constructs the name of the selector for a particular field of a struct
         (define (build-selector-id struct-name field-name)
           (datum->syntax-object
            struct-name
            (string->symbol
             (string-append
              (symbol->string (syntax-object->datum struct-name))
              "-"
              (symbol->string (syntax-object->datum field-name))))))
         
         ;; build-mutator-id : syntax syntax -> syntax
         ;; constructs the name of the selector for a particular field of a struct
         (define (build-mutator-id struct-name field-name)
           (datum->syntax-object
            struct-name
            (string->symbol
             (string-append
              "set-"
              (symbol->string (syntax-object->datum struct-name))
              "-"
              (symbol->string (syntax-object->datum field-name))
              "!"))))
         
         ;; code-for-one-id : syntax syntax syntax -> syntax
         ;; given the syntax for an identifier and a contract,
         ;; builds a begin expression for the entire contract and provide
         ;; the first syntax object is used for source locations
         (define (code-for-one-id stx id ctrct)
           (with-syntax ([id-rename (a:mangle-id provide-stx "provide/contract-id" id)]
                         [contract-id (a:mangle-id provide-stx "provide/contract-contract-id" id)]
                         [pos-module-source (a:mangle-id provide-stx "provide/contract-pos-module-source" id)]
                         [pos-stx (datum->syntax-object provide-stx 'here)]
                         [id id]
                         [ctrct ctrct])
             (syntax/loc stx
              (begin
                (provide (rename id-rename id))
                
                ;; unbound id check
                (if #f id)
                (define pos-module-source (module-source-as-symbol #'pos-stx))
                (define contract-id (let ([id ctrct]) id))
                (define-syntax id-rename
                  (make-set!-transformer
                   (lambda (stx)
                     (with-syntax ([neg-stx (datum->syntax-object stx 'here)])
                       (syntax-case stx (set!)
                         [(set! _ body) (raise-syntax-error
                                         #f 
                                         "cannot set! provide/contract identifier"
                                         stx
                                         (syntax _))]
                         [(_ arg (... ...))
                          (syntax 
                           ((-contract contract-id
                                       id
                                       pos-module-source
                                       (module-source-as-symbol #'neg-stx)
                                       (quote-syntax _))
                            arg
                            (... ...)))]
                         [_
                          (identifier? (syntax _))
                          (syntax 
                           (-contract contract-id
                                      id  
                                      pos-module-source 
                                      (module-source-as-symbol #'neg-stx)
                                      (quote-syntax _)))])))))))))
         
         (with-syntax ([(bodies ...) (code-for-each-clause (syntax->list (syntax (p/c-ele ...))))])
           (syntax 
            (begin
              bodies ...))))]))
  
  
  ;                                                      
  ;                                                      
  ;                                                      
  ;                                                      
  ;                                                      
  ;                          ;                       ;   
  ;    ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
  ;   ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
  ;  ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
  ;  ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
  ;  ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
  ;   ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
  ;    ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
  ;                                                      
  ;                                                      
  ;                                                      
  
  ;; contract = (make-contract string
  ;;                           (sym
  ;;                            sym
  ;;                            (union syntax #f)
  ;;                            string
  ;;                            ->
  ;;                            (alpha -> alpha)))
  ;; generic contract container; 
  ;; the first arg to proc is a symbol representing the name of the positive blame
  ;; the second arg to proc is the symbol representing the name of the negative blame
  ;; the third argument to proc is the src-info.
  ;; the fourth argumet is a textual representation of the original contract
  ;;
  ;; the argument to the result function is the value to test.
  ;; (the result function is the projection)
  
  (define-struct contract (name proc))
  
  (define-values (make-flat-contract
                  flat-contract-predicate
                  flat-contract?)
    (let ()
      (define-struct (flat-contract contract) (predicate))
      (values make-flat-contract
              flat-contract-predicate
              flat-contract?)))
  
  (define (flat-contract predicate)
    (unless (and (procedure? predicate)
                 (procedure-arity-includes? predicate 1))
      (error 'flat-contract
             "expected procedure of one argument as argument, given ~e"
             predicate))
    (let ([pname (predicate->type-name predicate)])
      (if pname
          (flat-named-contract pname predicate)
          (flat-named-contract "???" predicate))))
  
  (define (flat-named-contract name predicate)
    (unless (and (string? name)
                 (procedure? predicate)
                 (procedure-arity-includes? predicate 1))
      (error 'flat-named-contract
             "expected string and procedure of one argument as arguments, given: ~e and ~e"
             name predicate))
    (make-flat-contract 
     name
     (lambda (pos neg src-info orig-str) 
       (lambda (val)
         (if (predicate val)
             val
             (raise-contract-error
              src-info
              pos
              neg
              orig-str
              "expected type <~a>, given: ~e"
              name
              val))))
     predicate))

  (define-syntax -contract
    (lambda (stx)
      (syntax-case stx ()
        [(_ a-contract to-check pos-blame-e neg-blame-e)
         (with-syntax ([src-loc (syntax/loc stx here)])
           (syntax/loc stx
             (-contract a-contract to-check pos-blame-e neg-blame-e (quote-syntax src-loc))))]
        [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
         (syntax/loc stx
           (let ([a-contract-raw a-contract-e]
                 [name to-check]
                 [neg-blame neg-blame-e]
                 [pos-blame pos-blame-e]
                 [src-info src-info-e])
             (unless (or (contract? a-contract-raw)
                         (and (procedure? a-contract-raw)
                              (procedure-arity-includes? a-contract-raw 1)))
               (error 'contract "expected a contract or a procedure of arity 1 as first argument, given: ~e, other args ~e ~e ~e ~e" 
                      a-contract-raw
                      name
                      pos-blame
                      neg-blame
                      src-info))
             (let ([a-contract (if (contract? a-contract-raw)
                                   a-contract-raw
                                   (flat-contract a-contract-raw))])
               (unless (and (symbol? neg-blame)
                            (symbol? pos-blame))
                 (error 'contract
                        "expected symbols as names for assigning blame, given: ~e and ~e, other args ~e ~e ~e"
                        neg-blame pos-blame
                        a-contract-raw 
                        name
                        src-info))
               (unless (syntax? src-info)
                 (error 'contract "expected syntax as last argument, given: ~e, other args ~e ~e ~e ~e"
                        src-info
                        neg-blame 
                        pos-blame
                        a-contract-raw
                        name))
               (((contract-proc a-contract) pos-blame neg-blame src-info (contract-name a-contract))
                name))))])))
  
  ;; raise-contract-error : (union syntax #f) symbol symbol string string args ... -> alpha
  ;; doesn't return
  (define (raise-contract-error src-info to-blame other-party orig-str fmt . args)
    (let ([blame-src (src-info-as-string src-info)]
	  [specific-blame
	   (let ([datum (syntax-object->datum src-info)])
	     (if (symbol? datum)
		 (format "broke ~a's contract:" datum)
		 "failed contract"))])
      (raise
       (make-exn
        (string->immutable-string
         (string-append (format "~a~a: ~a ~a ~a: "
                                blame-src
                                other-party
                                to-blame
                                specific-blame
                                orig-str)
                        (apply format fmt args)))
        (current-continuation-marks)))))
  
  ;; src-info-as-string : (union syntax #f) -> string
  (define (src-info-as-string src-info)
    (if (syntax? src-info)
        (let ([src-loc-str (build-src-loc-string src-info)])
          (if src-loc-str
              (string-append src-loc-str  ": ")
              ""))
        ""))
  
  ;; predicate->expected-msg : function -> string
  ;; if the function has a name and the name ends
  ;; with a question mark, turn it into a mzscheme
  ;; style type name
  (define (predicate->expected-msg pred)
    (let ([name (predicate->type-name pred)])
      (if name
          (format "expected type <~a>, " name)
          "")))
  
  ;; predicate->type-name : pred -> (union #f string)
  (define (predicate->type-name pred)
    (let* ([name (object-name pred)])
      (and name
           (symbol->string name))))

  ;; contract->type-name : any -> string
  (define (contract->type-name c)
    (cond
      [(contract? c) (contract-name c)]
      [(and (procedure? c)
            (procedure-arity-includes? c 1) ;; make sure it isn't a contract
            (predicate->type-name c))
       =>
       (lambda (x) x)]
      [else (format "unknown-contract:<~e>" c)]))

  
;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
;           ;                                    ;                       ;          
;            ;;            ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;;   ;;;  
;              ;;         ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;    ;     
;                ;;      ;      ;     ;  ;   ;   ;    ;       ;  ;       ;    ;;    
;   ;;;;;;       ;;      ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;     ;;   
;              ;;        ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;       ;  
;            ;;           ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;       ;  
;           ;              ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;;  ;;;   
;                                                                                   
;                                                                                   
;                                                                                   

  
  (define-syntax-set (-> ->* ->d ->d* case-> object-contract
                         class-contract class-contract/prim)
    
    ;; ->/proc : syntax -> syntax
    ;; the transformer for the -> macro
    (define (->/proc stx) (make-/proc ->/h stx))
    
    ;; ->*/proc : syntax -> syntax
    ;; the transformer for the ->* macro
    (define (->*/proc stx) (make-/proc ->*/h stx))
    
    ;; ->d/proc : syntax -> syntax
    ;; the transformer for the ->d macro
    (define (->d/proc stx) (make-/proc ->d/h stx))
    
    ;; ->d*/proc : syntax -> syntax
    ;; the transformer for the ->d* macro
    (define (->d*/proc stx) (make-/proc ->d*/h stx))
        
    ;; make-/proc : (syntax -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))) 
    ;;              syntax
    ;;           -> (syntax -> syntax)
    (define (make-/proc /h stx)
      (let-values ([(arguments-check build-proj check-val wrapper) (/h stx)])
        (let ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))])
          (with-syntax ([inner-check (check-val outer-args)]
                        [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                        [(val-args body) (wrapper outer-args)])
            (with-syntax ([inner-lambda
                           (set-inferred-name-from
                            stx
                            (syntax/loc stx (lambda val-args body)))])
              (let ([inner-lambda-w/err-check
                     (syntax
                      (lambda (val)
                        inner-check
                        inner-lambda))])
                (with-syntax ([proj-code (build-proj outer-args inner-lambda-w/err-check)])
                  (arguments-check
                   outer-args
                   (set-inferred-name-from
                    stx
                    (syntax/loc stx
                      (make-contract
                       name-id
                       (lambda (pos-blame neg-blame src-info orig-str)
                         proj-code))))))))))))
    
    ;; case->/proc : syntax -> syntax
    ;; the transformer for the case-> macro
    (define (case->/proc stx)
      (syntax-case stx ()
        [(_ cases ...)
         (let-values ([(arguments-check build-projs check-val wrapper)
                       (case->/h stx (syntax->list (syntax (cases ...))))])
           (let ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))])
             (with-syntax ([(inner-check ...) (check-val outer-args)]
                           [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                           [(body ...) (wrapper outer-args)])
               (with-syntax ([inner-lambda 
                              (set-inferred-name-from
                               stx
                               (syntax/loc stx (case-lambda body ...)))])
                 (let ([inner-lambda-w/err-check
                        (syntax
                         (lambda (val)
                           inner-check ...
                           inner-lambda))])
                   (with-syntax ([proj-code (build-projs outer-args inner-lambda-w/err-check)])
                     (arguments-check
                      outer-args
                      (syntax/loc stx
                        (make-contract
                         (apply build-compound-type-name 'case-> name-id)
                         (lambda (pos-blame neg-blame src-info orig-str)
                           proj-code))))))))))]))
    
    ;; exactract-argument-lists : syntax -> (listof syntax)
    (define (extract-argument-lists stx)
      (map (lambda (x)
             (syntax-case x ()
               [(arg-list body) (syntax arg-list)]))
           (syntax->list stx)))
    
    ;; ensure-cases-disjoint : syntax syntax[list] -> void
    (define (ensure-cases-disjoint stx cases)
      (let ([individual-cases null]
            [dot-min #f])
        (for-each (lambda (case)
                    (let ([this-case (get-case case)])
                      (cond
                        [(number? this-case) 
                         (cond
                           [(member this-case individual-cases)
                            (raise-syntax-error
                             'case-> 
                             (format "found multiple cases with ~a arguments" this-case)
                             stx)]
                           [(and dot-min (dot-min . <= . this-case))
                            (raise-syntax-error 
                             'case-> 
                             (format "found overlapping cases (~a+ followed by ~a)" dot-min this-case)
                             stx)]
                           [else (set! individual-cases (cons this-case individual-cases))])]
                        [(pair? this-case)
                         (let ([new-dot-min (car this-case)])
                           (cond
                             [dot-min
                              (if (dot-min . <= . new-dot-min)
                                  (raise-syntax-error
                                   'case->
                                   (format "found overlapping cases (~a+ followed by ~a+)" dot-min new-dot-min)
                                   stx)
                                  (set! dot-min new-dot-min))]
                             [else
                              (set! dot-min new-dot-min)]))])))
                  cases)))

    ;; get-case : syntax -> (union number (cons number 'more))
    (define (get-case stx)
      (let ([ilist (syntax-object->datum stx)])
        (if (list? ilist)
            (length ilist)
            (cons 
             (let loop ([i ilist])
               (cond
                 [(pair? i) (+ 1 (loop (cdr i)))]
                 [else 0]))
             'more))))

    
    ;; case->/h : syntax
    ;;            (listof syntax) 
    ;;         -> (values (syntax -> syntax)
    ;;                    (syntax -> syntax)
    ;;                    (syntax syntax -> syntax) 
    ;;                    (syntax -> syntax))
    ;; like the other /h functions, but composes the wrapper functions
    ;; together and combines the cases of the case-lambda into a single list.
    (define (case->/h orig-stx cases)
      (let loop ([cases cases]
                 [name-ids '()])
        (cond
          [(null? cases) (values (lambda (outer-args body)
                                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                                 [body body]
                                                 [(name-ids ...) (reverse name-ids)])
                                     (syntax
                                      (let ([name-id (list name-ids ...)])
                                        body))))
                                 (lambda (x y) y)
                                 (lambda (args) (syntax ()))
                                 (lambda (args) (syntax ())))]
          [else
           (let ([/h (select/h (car cases) 'case-> orig-stx)]
                 [new-id (car (generate-temporaries (syntax (case->name-id))))])
             (let-values ([(arguments-checks build-projs check-vals wrappers)
                           (loop (cdr cases) (cons new-id name-ids))]
                          [(arguments-check build-proj check-val wrapper)
                           (/h (car cases))])
               (values
                (lambda (outer-args x) 
                  (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                [new-id new-id])
                    (arguments-check 
                     (syntax (val pos-blame neg-blame src-info orig-str new-id)) 
                     (arguments-checks 
                      outer-args
                      x))))
                (lambda (args inner)
                  (build-projs
                   args
                   (build-proj
                    args
                    inner)))
                (lambda (args)
                  (with-syntax ([checks (check-vals args)]
                                [check (check-val args)])
                    (syntax (check . checks))))
                (lambda (args)
                  (with-syntax ([case (wrapper args)]
                                [cases (wrappers args)])
                    (syntax (case . cases)))))))])))
    
    (define (class-contract/proc stx) (class-contract-mo? stx #f))
    (define (class-contract/prim/proc stx) (class-contract-mo? stx #t))
    
    (define (class-contract-mo? stx use-make-object?)
      (syntax-case stx ()
        [(form (method-specifier meth-name meth-contract) ...)
         (and 
          (andmap method-specifier? (syntax->list (syntax (method-specifier ...))))
          (andmap identifier? (syntax->list (syntax (meth-name ...)))))
         (let* ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))]
                [val-meth-names (syntax->list (syntax (meth-name ...)))]
                [super-meth-names (map prefix-super val-meth-names)]
                [val-meth-contracts (syntax->list (syntax (meth-contract ...)))]
                [val-meth-contract-vars (generate-temporaries val-meth-contracts)])
           
           (ensure-no-duplicates stx 'class-contract val-meth-names)
           
           (with-syntax ([outer-args outer-args]
                         [(super-meth-name ...) super-meth-names]
                         [(get-meth-contract ...) (map method-name->contract-method-name val-meth-names)]
                         [(method ...)
                          (map (lambda (meth-name meth-contract-var contract-stx) 
                                 (make-class-wrapper-method outer-args
                                                            meth-name
                                                            meth-contract-var
                                                            contract-stx))
                               val-meth-names
                               val-meth-contract-vars
                               val-meth-contracts)]
                         [(meth-contract-var ...) val-meth-contract-vars]
                         [this (datum->syntax-object (syntax form) 'this stx)]
                         [super-init (datum->syntax-object (syntax form) 'super-instantiate stx)]
                         [super-make (datum->syntax-object (syntax form) 'super-make-object stx)])
             (with-syntax ([call-super-initializer 
                            (if use-make-object?
                                (syntax/loc stx
                                  (begin (init-rest args)
                                         (apply super-make args)))
                                (syntax/loc stx 
                                  (super-init ())))])
               (syntax/loc stx
                 (make-contract
                  "(object-contract ...)"
                  (lambda outer-args
                    (let ([super-contracts-ht 
                           (let loop ([cls val])
                             (cond
                               [(sneaky-class? cls) (sneaky-class-contract-table cls)]
                               [else (let ([super (class-super-class cls)])
                                       (and super
                                            (loop super)))]))]
                          [meth-contract-var meth-contract] ...)
                      (unless (class? val)
                        (raise-contract-error src-info pos-blame neg-blame orig-str "expected a class, got: ~e" val))
                      (let ([class-i (class->interface val)])
                        (void)
                        (unless (method-in-interface? 'meth-name class-i)
                          (raise-contract-error src-info
                                                pos-blame 
                                                neg-blame
                                                orig-str
                                                "expected class to have method ~a, got: ~e"
                                                'meth-name
                                                val)) ...)
                      (let ([c (class*/names-sneaky
                                (this super-init super-make) val ()
                                
                                (rename [super-meth-name meth-name] ...)
                                method ...
                                call-super-initializer)]
                            [ht (make-hash-table)])
                        (set-sneaky-class-contract-table! c ht)
                        (hash-table-put! ht 'meth-name meth-contract-var) ...
                        c))))))))]
        [(_ (meth-specifier meth-name meth-contract) ...)
         (for-each (lambda (specifier name)
                     (unless (method-specifier? name)
                       (raise-syntax-error 'class-contract "expected either public or override" stx specifier))
                     (unless (identifier? name)
                       (raise-syntax-error 'class-contract "expected name" stx name)))
                   (syntax->list (syntax (meth-specifier ...)))
                   (syntax->list (syntax (meth-name ...))))]
        [(_ clz ...)
         (for-each (lambda (clz)
                     (syntax-case clz ()
                       [(a b c) (void)]
                       [else (raise-syntax-error 'class-contract "bad method/contract clause" stx clz)]))
                   (syntax->list (syntax (clz ...))))]))
    
    (define (object-contract/proc stx)
      
      ;; name : syntax
      ;; ctc-stx : syntax[evals to a contract]
      ;; mtd-arg-stx : syntax[list of arg-specs] (ie, for use in a case-lambda)
      (define-struct mtd (name ctc-stx mtd-arg-stx))
      
      ;; name : syntax
      ;; ctc-stx : syntax[evals to a contract]
      (define-struct fld (name ctc-stx))
      
      ;; expand-field/mtd-spec : stx -> (union mtd fld)
      (define (expand-field/mtd-spec f/m-stx)
        (syntax-case f/m-stx (field)
          [(field field-name ctc)
           (identifier? (syntax field-name))
           (make-fld (syntax field-name) (syntax ctc))]
          [(field field-name ctc)
           (raise-syntax-error 'object-contract "expected name of field" stx (syntax field-name))]
          [(mtd-name ctc)
           (identifier? (syntax mtd-name))
           (let-values ([(ctc-stx proc-stx) (expand-mtd-contract (syntax ctc))])
             (make-mtd (syntax mtd-name)
                       ctc-stx
                       proc-stx))]
          [(mtd-name ctc)
           (raise-syntax-error 'object-contract "expected name of method" stx (syntax mtd-name))]
          [_ (raise-syntax-error 'object-contract "expected field or method clause" stx f/m-stx)]))
      
      ;; expand-mtd-contract : syntax -> (values ctc-stx mtd-arg-stx)
      (define (expand-mtd-contract mtd-stx)
        (syntax-case stx (case-> opt->)
          #|
          [(case-> cases ...) 
           (with-syntax ([(cases ...) (map expand-mtd-arrow (syntax->list (syntax (cases ...))))])
             (syntax (case-> cases ...)))]
          [(opt-> opts ...) ...]
          |#
          [else (expand-mtd-arrow mtd-stx)]))
      
      ;; expand-mtd-arrow : stx -> (values ctc-stx mtd-arg-stx)
      (define (expand-mtd-arrow mtd-stx)
        (syntax-case mtd-stx (-> ->* ->d ->d*)
          [(->) (raise-syntax-error 'object-contract "-> must have arguments" stx mtd-stx)]
          [(-> args ...)
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (args ...)))])
             (values (->/proc (syntax (-> any? args ...)))
                     (syntax ((arg-vars ...)))))]
          #|
          [(->* (doms ...) (rngs ...))
           (syntax (->* (this-ctc doms ...) (rngs ...)))]
          [(->* (doms ...) rst (rngs ...))
           (syntax (->* (this-ctc doms ...) rst (rngs ...)))]
          [(->* x ...)
           (raise-syntax-error 'object-object "malformed ->*" stx mtd-stx)]
          [(->d) (raise-syntax-error 'object-contract "->d must have arguments" stx mtd-stx)]
          [(->d args ...)
           (let* ([args-list (syntax->list (syntax (args ...)))]
                  [doms-val (all-but-last args-list)])
             (with-syntax ([(doms ...) doms-val]
                           [(arg-vars ...) (generate-temporaries doms-val)]
                           [rng-proc (car (last-pair args-list))]
                           [arity-count (- (length args-list) 1)])
               (syntax (->d this-ctc
                            doms ... 
                            (let ([f rng-proc])
                              (unless (procedure? f)
                                (error 'object-contract "expected last argument of ->d to be a procedure, got ~e" f))
                              (unless (procedure-arity-includes f arity-count)
                                (error 'object-contract 
                                       "expected last argument of ->d to be a procedure that accepts ~a arguments, got ~e"
                                       arity-count
                                       f))
                              (lambda (_this-var arg-vars ...)
                                (f arg-vars ...)))))))]
          [(->d* (doms ...) rng-proc)
           (let ([doms-val (syntax->list (syntax (doms ...)))])
             (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                           [arity-count (- (length doms-val) 1)])
               (syntax (->d* (this-ctc doms ...)
                             (let ([f rng-proc])
                               (unless (procedure? f)
                                 (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
                               (unless (procedure-arity-includes f arity-count)
                                 (error 'object-contract 
                                        "expected last argument of ->d* to be a procedure that accepts ~a arguments, got ~e"
                                        arity-count
                                        f))
                               (lambda (_this-var arg-vars ...)
                                 (f arg-vars ...)))))))]
          [(->d* (doms ...) rst-ctc rng-proc)
           (let ([doms-val (syntax->list (syntax (doms ...)))])
             (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                           [arity-count (- (length doms-val) 1)])
               (syntax (->d* (this-ctc doms ...)
                             rst-ctc
                             (let ([f rng-proc])
                               (unless (procedure? f)
                                 (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
                               (unless (procedure-arity-includes f arity-count)
                                 (error 'object-contract 
                                        "expected last argument of ->d* to be a procedure that accepts ~a arguments, got ~e"
                                        arity-count
                                        f))
                               (lambda (_this-var arg-vars ...)
                                 (f arg-vars ...)))))))]
          |#
          [else (raise-syntax-error 'object-contract "unknown method contract syntax" stx mtd-stx)]))
      
      
      ;; build-methods-stx : syntax[list of lambda arg specs] -> syntax[method realized as proc]
      (define (build-methods-stx arg-spec-stxss)
        (let loop ([arg-spec-stxss arg-spec-stxss]
                   [i 0])
          (cond
            [(null? arg-spec-stxss) null]
            [else (let ([arg-spec-stxs (car arg-spec-stxss)])
                    (with-syntax ([(cases ...)
                                   (map (lambda (arg-spec-stx)
                                          (with-syntax ([(this rest-ids ...) arg-spec-stx]
                                                        [i i])
                                            (syntax ((this rest-ids ...) 
                                                     ((field-ref this i) (wrapper-object-wrapped this) rest-ids ...)))))
                                        (syntax->list arg-spec-stxs))])
                      (cons (syntax (lambda (field-ref) (case-lambda cases ...)))
                            (loop (cdr arg-spec-stxss)
                                  (+ i 1)))))])))
      
      (syntax-case stx ()
        [(_ field/mtd-specs ...)
         (let* ([mtd/flds (map expand-field/mtd-spec (syntax->list (syntax (field/mtd-specs ...))))]
		[mtds (filter mtd? mtd/flds)]
		[flds (filter fld? mtd/flds)])
           (with-syntax ([(method-ctc-stx ...) (map mtd-ctc-stx mtds)]
                         [(method-name ...) (map mtd-name mtds)]
                         [(method-var ...) (generate-temporaries mtds)]
                         [(method/app-var ...) (generate-temporaries mtds)]
                         [(methods ...) (build-methods-stx (map mtd-mtd-arg-stx mtds))]
                         
                         [(field-ctc-stx ...) (map mtd-ctc-stx flds)]
                         [(field-name ...) (map fld-name flds)]
                         [(field-var ...) (generate-temporaries flds)]
                         [(field/app-var ...) (generate-temporaries flds)])
             (syntax
              (make-contract
               "class contract"
               (let ([method-var (contract-proc method-ctc-stx)] ...
                     [field-ctc-var field-ctc-stx] ...)
                 (begin
                   (void)
                   (unless (contract? field-ctc-var)
                     (error 'object-contract
                            "expected contract for field ~a, got ~e"
                            'field-name
                            field-ctc-var)) ...)
                 (let ([field-var (contract-proc field-ctc-var)] ...)
                   (lambda (pos-blame neg-blame src-info orig-str)
                     (let ([method/app-var (method-var pos-blame neg-blame src-info orig-str)] ...
                           [field/app-var (field-var pos-blame neg-blame src-info orig-str)]...)
                       (let ([cls (make-wrapper-class 'wrapper-class 
                                                      '(method-name ...)
                                                      (list methods ...)
                                                      '() ; '(field-name ...)
                                                      )])
                         (lambda (val)
                           (unless (object? val)
                             (raise-contract-error src-info
                                                   pos-blame
                                                   neg-blame
                                                   orig-str
                                                   "expected an object, got ~e"
                                                   val))
                           (let ([val-mtd-names
                                  (interface->method-names
                                   (object-interface
                                    val))])
                             (void)
                             (unless (memq 'method-name val-mtd-names)
                               (raise-contract-error src-info
                                                     pos-blame
                                                     neg-blame
                                                     orig-str
                                                     "expected an object with method ~s"
                                                     'method-name))
                             ...)
                           
                           (let ([vtable (extract-vtable val)]
                                 [method-ht (extract-method-ht val)])
                             (make-object cls
                               val
                               (method/app-var (vector-ref vtable (hash-table-get method-ht 'method-name))) ...
                               ;(field/app-var (field-res val 'field-name))
                               ))))))))))))]))
    
    (define (object-contract/proc2 stx)
      (syntax-case stx ()
        [(form (meth-name meth-contract) ...)
         (andmap identifier? (syntax->list (syntax (meth-name ...))))
         (let* ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))]
                [val-meth-names (syntax->list (syntax (meth-name ...)))]
                [val-meth-contracts (syntax->list (syntax (meth-contract ...)))]
                [val-meth-contract-vars (generate-temporaries val-meth-contracts)])
           
           (ensure-no-duplicates stx 'object/contract val-meth-names) 
           
           (with-syntax ([outer-args outer-args]
                         [(get-meth-contract ...) (map method-name->contract-method-name val-meth-names)]
                         [(method ...)
                          (map (lambda (x y z) (make-object-wrapper-method outer-args x y z))
                               val-meth-names
                               val-meth-contract-vars
                               val-meth-contracts)]
                         [(meth-contract-var ...) val-meth-contract-vars])
             (syntax/loc stx
               (make-contract
                "object contract"
                (lambda outer-args
                  (let ([meth-contract-var meth-contract] ...)
                    (unless (object? val)
                      (raise-contract-error src-info pos-blame neg-blame orig-str "expected an object, got: ~e" val))
                    (let ([obj-i (object-interface val)])
                      (void)
                      (unless (method-in-interface? 'meth-name obj-i)
                        (raise-contract-error src-info
                                              pos-blame 
                                              neg-blame
                                              orig-str
                                              "expected class to have method ~a, got: ~e"
                                              'meth-name
                                              val))
                      ...)
                    
                    (make-object/sneaky 
                     val
                     (class object%
                      method ...
                      (super-instantiate ())))))))))]
        [(_ (meth-name meth-contract) ...)
         (for-each (lambda (name)
                     (unless (identifier? name)
                       (raise-syntax-error 'object-contract "expected name" stx name)))
                   (syntax->list (syntax (meth-name ...))))]
        [(_ clz ...)
         (for-each (lambda (clz)
                     (syntax-case clz ()
                       [(b c) (void)]
                       [else (raise-syntax-error 'object-contract
                                                 "bad method/contract clause"
                                                 stx
                                                 clz)]))
                   (syntax->list (syntax (clz ...))))]))

    ;; ensure-no-duplicates : syntax (listof syntax[identifier]) -> void
    (define (ensure-no-duplicates stx form-name names)
      (let ([ht (make-hash-table)])
        (for-each (lambda (name)
                    (let ([key (syntax-e name)])
                      (when (hash-table-get ht key (lambda () #f))
                        (raise-syntax-error form-name
                                            "duplicate method name"
                                            stx
                                            name))
                      (hash-table-put! ht key #t)))
                  names)))
    
    ;; method-specifier? : syntax -> boolean
    ;; returns #t if x is the syntax for a valid method specifier
    (define (method-specifier? x)
      (or (eq? 'public (syntax-e x))
          (eq? 'override (syntax-e x))))
    
    ;; make-object-wrapper-method : syntax syntax[identifier] syntax[identifier] syntax -> syntax
    ;; constructs a wrapper method that checks the pre and post-condition, and
    ;; calls the original object's method
    (define (make-object-wrapper-method outer-args method-name contract-var contract-stx)
      (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                    [method-name method-name]
                    [method-name-string (symbol->string (syntax-e method-name))]
                    [contract-var contract-var])
        (syntax/loc contract-stx
          (define/public (method-name . args)
            (let ([other-method (lambda x (send/apply val method-name x))]
                  [method-specific-src-info 
                   (if (identifier? src-info)
                       (datum->syntax-object
                        src-info
                        (string->symbol
                         (string-append
                          (symbol->string (syntax-e src-info))
                          " method "
                          method-name-string)))
                       src-info)])
              (apply (contract-var
                      other-method
                      pos-blame
                      neg-blame
                      method-specific-src-info)
                     args))))))

    ;; make-class-wrapper-method : syntax syntax[identifier] syntax[identifier] syntax -> syntax
    ;; constructs a wrapper method that checks the pre and post-condition, and
    ;; calls the super method inbetween.
    (define (make-class-wrapper-method outer-args method-name contract-var contract-stx)
      (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                    [super-method-name (prefix-super method-name)]
                    [method-name method-name]
                    [method-name-string (symbol->string (syntax-e method-name))]
                    [contract-var contract-var])
        (syntax/loc contract-stx
          (define/override method-name
            (lambda args
              (let* ([super-method (lambda x (super-method-name . x))]
                     [method-specific-src-info 
                      (if (identifier? src-info)
                          (datum->syntax-object
                           src-info
                           (string->symbol
                            (string-append
                             (symbol->string (syntax-e src-info))
                             " method "
                             method-name-string)))
                          src-info)]
                     [super-contract (and super-contracts-ht
                                          (hash-table-get super-contracts-ht
                                                          'method-name
                                                          (lambda () #f)))]
                     [wrapped-method (contract-var
                                      super-method
                                      pos-blame
                                      neg-blame
                                      method-specific-src-info)])
                (apply wrapped-method args)))))))
    
    ;; prefix-super : syntax[identifier] -> syntax[identifier]
    ;; adds super- to the front of the identifier
    (define (prefix-super stx)
      (datum->syntax-object
       #'here
       (string->symbol
        (format 
         "super-~a"
         (syntax-object->datum
          stx)))))
    
    ;; method-name->contract-method-name : syntax[identifier] -> syntax[identifier]
    ;; given the syntax for a method name, constructs the name of a method
    ;; that returns the super's contract for the original method.
    (define (method-name->contract-method-name stx)
      (datum->syntax-object
       #'here
       (string->symbol
        (format 
         "ACK_DONT_GUESS_ME-super-contract-~a"
         (syntax-object->datum
          stx)))))
    
    ;; Each of the /h functions builds four pieces of syntax:
    ;;  - [arguments-check]
    ;;    code that binds the contract values to names and
    ;;    does error checking for the contract specs
    ;;    (were the arguments all contracts?)
    ;;  - [build-proj]
    ;;    code that partially applies the input contracts to build projections
    ;;  - [check-val]
    ;;    code that does error checking on the contract'd value itself
    ;;    (is it a function of the right arity?)
    ;;  - [wrapper]
    ;;    a piece of syntax that has the arguments to the wrapper
    ;;    and the body of the wrapper.
    ;; the first function accepts a body expression and wraps
    ;;    the body expression with checks. In addition, it
    ;;    adds a let that binds the contract exprssions to names
    ;;    the results of the other functions mention these names.
    ;; the second and third function's input syntax should be four
    ;;    names: val, pos-blame, neg-blame, src-info.
    ;; the third function returns a syntax list with two elements,
    ;;    the argument list (to be used as the first arg to lambda,
    ;;    or as a case-lambda clause) and the body of the function.
    ;; They are combined into a lambda for the -> ->* ->d ->d* macros,
    ;; and combined into a case-lambda for the case-> macro.
    
    ;; ->/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->/h stx)
      (syntax-case stx ()
        [(_) (raise-syntax-error '-> "expected at least one argument" stx)]
        [(_ arg ...)
         (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (arg ...))))]
                       [rng (car (last-pair (syntax->list (syntax (arg ...)))))])
           (syntax-case* (syntax rng) (any values) module-or-top-identifier=?

             [any 
              (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                            [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
                (values
                 (lambda (outer-args body)
                   (with-syntax ([body body]
                                 [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (let ([dom-contract-x (coerce-contract -> dom)] ...)
                        (let ([dom-x (contract-proc dom-contract-x)] ...)
                          (let ([name-id (build-compound-type-name '-> dom-contract-x ... 'any)])
                            body))))))
                 
                 (lambda (outer-args inner-lambda)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                 [inner-lambda inner-lambda])
                     (syntax
                      (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                        inner-lambda))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (unless (and (procedure? val)
                                   (procedure-arity-includes? val dom-length))
                        (raise-contract-error
                         src-info
                         pos-blame
                         neg-blame
                         orig-str
                         "expected a procedure that accepts ~a arguments, given: ~e"
                         dom-length
                         val)))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      ((arg-x ...)
                       (val (dom-projection-x arg-x) ...)))))))]
             [(values rng ...)
              (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                            [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                            
                            [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                            [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                            [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                            [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                            [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                            [(res-x ...) (generate-temporaries (syntax (rng ...)))])
                (values
                 (lambda (outer-args body)
                   (with-syntax ([body body]
                                 [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (let ([dom-contract-x (coerce-contract -> dom)] ...
                            [rng-contract-x (coerce-contract -> rng)] ...)
                        (let ([dom-x (contract-proc dom-contract-x)] ...
                              [rng-x (contract-proc rng-contract-x)] ...)
                          (let ([name-id (build-compound-type-name 
                                          '-> 
                                          dom-contract-x ...
                                          (build-compound-type-name 'values rng-contract-x ...))])
                            body))))))
                 
                 (lambda (outer-args inner-lambda)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                 [inner-lambda inner-lambda])
                     (syntax
                      (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                            [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                        inner-lambda))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (unless (and (procedure? val)
                                   (procedure-arity-includes? val dom-length))
                        (raise-contract-error
                         src-info
                         pos-blame
                         neg-blame
                         orig-str
                         "expected a procedure that accepts ~a arguments, given: ~e"
                         dom-length
                         val)))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      ((arg-x ...)
                       (let-values ([(res-x ...) (val (dom-projection-x arg-x) ...)])
                         (values (rng-projection-x
                                  res-x)
                                 ...))))))))]
             [rng
              (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                            [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                            [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                            
                            [(rng-x) (generate-temporaries (syntax (rng)))]
                            [(rng-contact-x) (generate-temporaries (syntax (rng)))]
                            [(rng-projection-x) (generate-temporaries (syntax (rng)))]
                            [(rng-ant-x) (generate-temporaries (syntax (rng)))]
                            [(res-x) (generate-temporaries (syntax (rng)))])
                (values
                 (lambda (outer-args body)
                   (with-syntax ([body body]
                                 [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (let ([dom-contract-x (coerce-contract -> dom)] ...
                            [rng-contract-x (coerce-contract -> rng)])
                        (let ([dom-x (contract-proc dom-contract-x)] ...
                              [rng-x (contract-proc rng-contract-x)])
                          (let ([name-id (build-compound-type-name '-> dom-contract-x ... rng-contract-x)])
                            body))))))
                 
                 (lambda (outer-args inner-lambda)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                 [inner-lambda inner-lambda])
                     (syntax
                      (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                            [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)])
                        inner-lambda))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (unless (and (procedure? val)
                                   (procedure-arity-includes? val dom-length))
                        (raise-contract-error
                         src-info
                         pos-blame
                         neg-blame
                         orig-str
                         "expected a procedure that accepts ~a arguments, given: ~e"
                         dom-length
                         val)))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      ((arg-x ...)
                       (let ([res-x (val (dom-projection-x arg-x) ...)])
                         (rng-projection-x res-x))))))))]))]))
    
    ;; ->*/h : stx -> (values (syntax -> syntax) (syntax syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->*/h stx)
      (syntax-case stx (any)
        [(_ (dom ...) (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(res-x ...) (generate-temporaries (syntax (rng ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [rng-contract-x (coerce-contract ->* rng)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-x (contract-proc rng-contract-x)] ...)
                     (let ([name-id (string-append "(->* "
                                                   (build-compound-type-name #f dom-contract-x ...)
                                                   " "
                                                   (build-compound-type-name #f rng-contract-x ...)
                                                   ")")])
                       body))))))
            
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                   inner-lambda))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (unless (and (procedure? val)
                              (procedure-arity-includes? val dom-length))
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    orig-str
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
                    val)))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (let-values ([(res-x ...) (val (dom-projection-x arg-x) ...)])
                    (values (rng-projection-x
                             res-x)
                            ...))))))))]
        [(_ (dom ...) any)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...)
                     (let ([name-id (string-append "(->* "
                                                   (build-compound-type-name #f dom-contract-x ...)
                                                   " any)")])
                       body))))))
            
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (unless (and (procedure? val)
                              (procedure-arity-includes? val dom-length))
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    orig-str
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
                    val)))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (val (dom-projection-x arg-x) ...)))))))]
        [(_ (dom ...) rest (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-projection-x (car (generate-temporaries (list (syntax rest))))]
                       [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [body body])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->* rest)]
                       [rng-contract-x (coerce-contract ->* rng)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)]
                         [rng-x (contract-proc rng-contract-x)] ...)
                     (let ([name-id (string-append "(->* "
                                                   (build-compound-type-name #f dom-contract-x ...)
                                                   " "
                                                   (contract->type-name dom-rest-contract-x)
                                                   " "
                                                   (build-compound-type-name #f rng-contract-x ...)
                                                   ")")])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-rest-projection-x (dom-rest-x neg-blame pos-blame src-info orig-str)]
                       [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (unless (procedure? val)
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    orig-str
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
                    val)))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ... . arg-rest-x)
                  (let-values ([(res-x ...)
                                (apply
                                 val
                                 (dom-projection-x arg-x)
                                 ...
                                 (dom-rest-projection-x arg-rest-x))])
                    (values (rng-projection-x res-x) ...))))))))]
	[(_ (dom ...) rest any)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-projection-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                       
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->* rest)])
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)])
                     (let ([name-id (string-append "(->* "
                                                   (build-compound-type-name #f dom-contract-x ...)
                                                   " "
                                                   (contract->type-name dom-rest-contract-x)
                                                   " any)")])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-projection-rest-x (dom-rest-x neg-blame pos-blame src-info orig-str)])
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (unless (procedure? val)
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    orig-str
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
                    val)))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ... . arg-rest-x)
                  (apply
		   val
		   (dom-projection-x arg-x)
		   ...
		   (dom-projection-rest-x arg-rest-x))))))))]))
    
    ;; ->d/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->d/h stx)
      (syntax-case stx ()
        [(_) (raise-syntax-error '->d "expected at least one argument" stx)]
        [(_ ct ...)
         (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (ct ...))))]
                       [rng (car (last-pair (syntax->list (syntax (ct ...)))))])
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (values
              (lambda (outer-args body)
                (with-syntax ([body body]
                              [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax
                   (let ([dom-contract-x (coerce-contract ->d dom)] ...)
                     (let ([dom-x (contract-proc dom-contract-x)] ...
                           [rng-x rng])
                       (unless (and (procedure? rng-x)
                                    (procedure-arity-includes? rng-x arity))
                         (error '->d "expected range portion to be a function that takes ~a arguments, given: ~e"
                                arity
                                rng-x))
                       (let ([name-id (build-compound-type-name '->d dom-contract-x ... '(... ...))])
                         
                         body))))))
              (lambda (outer-args inner-lambda)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                              [inner-lambda inner-lambda])
                  (syntax
                   (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                     inner-lambda))))
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax
                   (unless (and (procedure? val)
                                (procedure-arity-includes? val arity))
                     (raise-contract-error
                      src-info
                      pos-blame
                      neg-blame
                      orig-str
                      "expected a procedure that accepts ~a arguments, given: ~e"
                      arity
                      val)))))
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax
                   ((arg-x ...)
                    (let ([rng-contract (rng-x arg-x ...)])
                      (((coerce/select-contract ->d rng-contract)
                        pos-blame
                        neg-blame
                        src-info
                        orig-str)
                       (val (dom-projection-x arg-x) ...))))))))))]))
    
    ;; ->d*/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->d*/h stx)
      (syntax-case stx ()
        [(_ (dom ...) rng-mk)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d* dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-mk-x rng-mk])
                     (unless (and (procedure? rng-mk-x)
                                  (procedure-arity-includes? rng-mk-x dom-length))
                       (error '->d* "expected range position to be a procedure that accepts ~a arguments, given: ~e"
                              dom-length rng-mk-x))
                     (let ([name-id (string-append "(->d* "
                                                   (build-compound-type-name #f dom-contract-x ...)
                                                   " ...)")])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (unless (and (procedure? val)
                              (procedure-arity-includes? val dom-length))
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    orig-str
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
                    val)))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (call-with-values
                   (lambda () (rng-mk-x arg-x ...))
                   (lambda rng-contracts
                     (call-with-values
                      (lambda ()
                        (val (dom-projection-x arg-x) ...))
                      (lambda results
                        (unless (= (length results) (length rng-contracts))
                          (error '->d* 
                                 "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
                                 (length results) (length rng-contracts)))
                        (apply 
                         values
                         (map (lambda (rng-contract result)
                                (((coerce/select-contract ->d* rng-contract)
                                  pos-blame
                                  neg-blame
                                  src-info
                                  orig-str)
                                 result))
                              rng-contracts
                              results))))))))))))]
        [(_ (dom ...) rest rng-mk)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-rest-x) (generate-temporaries (syntax (rest)))]
                       [(dom-rest-contract-x) (generate-temporaries (syntax (rest)))]
                       [(dom-rest-projection-x) (generate-temporaries (syntax (rest)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->d* rest)])
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)]
                         [rng-mk-x rng-mk])
                     (unless (procedure? rng-mk-x)
                       (error '->d* "expected range position to be a procedure that accepts ~a arguments, given: ~e"
                              arity rng-mk-x))
                     (let ([name-id (string-append "(->d* "
                                                   (build-compound-type-name #f dom-contract-x ...)
                                                   " "
                                                   (contract->type-name dom-rest-contract-x)
                                                   " ...)")])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-rest-projection-x (dom-rest-x neg-blame pos-blame src-info orig-str)])
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 (unless (procedure? val)
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    orig-str
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    arity
                    val)))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
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
                         (dom-projection-x arg-x)
                         ...
                         (dom-rest-projection-x rest-arg-x)))
                      (lambda results
                        (unless (= (length results) (length rng-contracts))
                          (error '->d* 
                                 "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
                                 (length results) (length rng-contracts)))
                        (apply 
                         values
                         (map (lambda (rng-contract result)
                                (((coerce/select-contract ->d* rng-contract)
                                  pos-blame
                                  neg-blame
                                  src-info
                                  orig-str)
                                 result))
                              rng-contracts
                              results))))))))))))]))
    
    ;; select/h : syntax -> /h-function
    (define (select/h stx err-name ctxt-stx)
      (syntax-case stx (-> ->* ->d ->d*)
        [(-> . args) ->/h]
        [(->* . args) ->*/h]
        [(->d . args) ->d/h]
        [(->d* . args) ->d*/h]
        [(xxx . args) (raise-syntax-error err-name "unknown arrow constructor" ctxt-stx (syntax xxx))]
        [_ (raise-syntax-error err-name "malformed arrow clause" ctxt-stx stx)]))
    
    
    ;; set-inferred-name-from : syntax syntax -> syntax
    (define (set-inferred-name-from with-name to-be-named)
      (let ([name (syntax-local-infer-name with-name)])
        (cond
          [(identifier? name)
           (syntax-property to-be-named 'inferred-name (syntax-e name))]
          [(symbol? name)
           (syntax-property to-be-named 'inferred-name name)]
          [else to-be-named])))
    
    ;; (cons X (listof X)) -> (listof X)
    ;; returns the elements of `l', minus the last
    ;; element
    (define (all-but-last l)
      (cond
        [(null? l) (error 'all-but-last "bad input")]
        [(null? (cdr l)) null]
        [else (cons (car l) (all-but-last (cdr l)))]))
    
    ;; generate-indicies : syntax[list] -> (cons number (listof number))
    ;; given a syntax list of length `n', returns a list containing
    ;; the number n followed by th numbers from 0 to n-1
    (define (generate-indicies stx)
      (let ([n (length (syntax->list stx))])
        (cons n
              (let loop ([i n])
                (cond
                  [(zero? i) null]
                  [else (cons (- n i)
                              (loop (- i 1)))]))))))

  ;; coerce/select-contract : id (union contract? procedure-arity-1) -> contract-proc
  ;; contract-proc = sym sym stx -> alpha -> alpha
  ;; returns the procedure for the contract after extracting it from the
  ;; struct. Coerces the argument to a flat contract if it is procedure, but first.
  (define-syntax (coerce/select-contract stx)
    (syntax-case stx ()
      [(_ name val)
       (syntax
        (let ([x val])
          (cond
            [(contract? x)
             (contract-proc x)]
            [(and (procedure? x) (procedure-arity-includes? x 1))
             (contract-proc (flat-contract x))]
            [else
             (error 'name 
                    "expected contract or procedure of arity 1, got ~e"
                    x)])))]))
  
  ;; coerce-contract : id (union contract? procedure-arity-1) -> contract
  ;; contract-proc = sym sym stx -> alpha -> alpha
  ;; returns the procedure for the contract after extracting it from the
  ;; struct. Coerces the argument to a flat contract if it is procedure, but first.
  (define-syntax (coerce-contract stx)
    (syntax-case stx ()
      [(_ name val)
       (syntax
        (let ([x val])
          (cond
            [(contract? x) x]
            [(and (procedure? x) (procedure-arity-includes? x 1))
             (flat-contract x)]
            [else
             (error 'name 
                    "expected contract or procedure of arity 1, got ~e"
                    x)])))]))
    
  (define class-with-contracts<%> (interface ()))
  
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
	   (syntax/loc stx
	    (let ([res-vs ress] ...
		  [req-vs reqs] ...
		  [opt-vs opts] ...)
	      (case-> (->* (case-doms ...) (double-res-vs ...)) ...)))))]))


;                                                                                                   
;                                                                                                   
;                                                                                                   
;               ;                                                                                   
;                                                                                                   
;                                                                ;                       ;          
;   ; ;;  ;;    ;    ;;;    ;;;            ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;;   ;;;  
;   ;;  ;;  ;   ;   ;      ;   ;          ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;    ;     
;   ;   ;   ;   ;   ;;    ;              ;      ;     ;  ;   ;   ;    ;       ;  ;       ;    ;;    
;   ;   ;   ;   ;    ;;   ;              ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;     ;;   
;   ;   ;   ;   ;      ;  ;              ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;       ;  
;   ;   ;   ;   ;      ;   ;   ;  ;       ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;       ;  
;   ;   ;   ;   ;   ;;;     ;;;   ;        ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;;  ;;;   
;                                                                                                   
;                                                                                                   
;                                                                                                   

  
  
  (provide any? 
           flat-rec-contract
	   union
           and/c
	   not/f
           >=/c <=/c </c >/c 
           integer-in
	   real-in
           natural-number?
	   string/len
           false?
	   printable?
           symbols
	   is-a?/c subclass?/c implementation?/c
           listof list-immutableof 
           vectorof vector-immutableof vector/p vector-immutable/c 
           cons-immutable/c cons/p list-immutable/c list/p 
           box-immutable/c box/p
	   mixin-contract make-mixin-contract)
  
  (define-syntax (flat-rec-contract stx)
    (syntax-case stx  ()
      [(_ name ctc ...)
       (identifier? (syntax name))
       (with-syntax ([(ctc-id ...) (generate-temporaries (syntax (ctc ...)))]
                     [(pred-id ...) (generate-temporaries (syntax (ctc ...)))])
         (syntax 
          (let* ([pred (lambda (x) (error 'flat-rec-contract "applied too soon"))]
                 [name (flat-contract (let ([name (lambda (x) (pred x))]) name))])
            (let ([ctc-id (coerce-contract flat-rec-contract ctc)] ...)
              (begin 
                (void) ;; ensure begin has at least one arg.
                (unless (flat-contract? ctc-id)
                  (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
                ...)
              (set! pred
                    (let ([pred-id (flat-contract-predicate ctc-id)] ...)
                      (lambda (x)
                        (or (pred-id x) ...))))
              name))))]
      [(_ name ctc ...)
       (raise-syntax-error 'flat-rec-contract "expected first argument to be an identifier" stx (syntax name))]))
  
  ;; tidy contracts
  
  (define (union . args)
    (for-each
     (lambda (x) 
       (unless (or (contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'union "expected procedures of arity 1 or contracts, given: ~e" x)))
     args)
    (let-values ([(contract fc/predicates)
                  (let loop ([contract #f]
                             [fc/predicates null]
                             [args args])
                    (cond
                      [(null? args) (values contract (reverse fc/predicates))]
                      [else 
                       (let ([arg (car args)])
                         (cond
                           [(or (flat-contract? arg)
                                (not (contract? arg)))
                            (loop contract (cons arg fc/predicates) (cdr args))]
                           [contract
                            (error 'union "expected at most one non-flat contract, given ~e and ~e"
                                   contract
                                   arg)]
                           [else (loop arg fc/predicates (cdr args))]))]))])
      (let* ([flat-contracts (map (lambda (x) (if (flat-contract? x)
                                                  x
                                                  (flat-contract x)))
                                  fc/predicates)]
             [predicates (map flat-contract-predicate flat-contracts)])
        (cond
          [contract
           (let ([c-proc (contract-proc contract)])
             (make-contract
              (apply build-compound-type-name "union" (cons contract flat-contracts))
              (lambda (pos neg src-info orig-str)
                (let ([partial-contract (c-proc pos neg src-info orig-str)])
                  (lambda (val)
                    (cond
                      [(ormap (lambda (pred) (pred val)) predicates)
                       val]
                      [else
                       (partial-contract val)]))))))]
          [else
           (flat-named-contract
            (apply build-compound-type-name "union" flat-contracts)
            (lambda (x)
              (ormap (lambda (pred) (pred x)) predicates)))]))))
  
  (define false?
    (flat-named-contract
     "false?"
     (lambda (x) (not x))))
  
  (define any?
    (make-flat-contract
     "any?"
     (lambda (pos neg src-info orig-str) (lambda (val) val))
     (lambda (x) #t)))

  (define (string/len n)
    (unless (number? n)
      (error 'string/len "expected a number as argument, got ~e" n))
    (flat-named-contract 
     (format "(string/len ~a)" n)
     (lambda (x)
       (and (string? x)
            ((string-length x) . < . n)))))
  
  (define (symbols . ss)
    (unless ((length ss) . >= . 1)
      (error 'symbols "expected at least one argument"))
    (unless (andmap symbol? ss)
      (error 'symbols "expected symbols as arguments, given: ~a"
	     (apply string-append (map (lambda (x) (format "~e " x)) ss))))
    (flat-named-contract
     (apply build-compound-type-name 'symbols (map (lambda (x) (format "'~s" x)) ss))
     (lambda (x)
       (memq x ss))))
  
  (define printable?
    (flat-named-contract
     "printable?"
     (lambda (x)
       (let printable? ([x x])
	 (or (symbol? x)
	     (string? x)
	     (boolean? x)
	     (char? x)
	     (null? x)
	     (number? x)
	     (regexp? x)
	     (and (pair? x)
		  (printable? (car x))
		  (printable? (cdr x)))
	     (and (vector? x)
		  (andmap printable? (vector->list x)))
	     (and (box? x)
		  (printable? (unbox x))))))))
  
  (define (>=/c x)
    (flat-named-contract
     (format "(>=/c ~a)" x)
     (lambda (y) (and (number? y) (>= y x)))))
  (define (<=/c x)
    (flat-named-contract
     (format "(<=/c ~a)" x)
     (lambda (y) (and (number? y) (<= y x)))))
  (define (</c x)
    (flat-named-contract
     (format "(</c ~a)" x)
     (lambda (y) (and (number? y) (< y x)))))
  (define (>/c x)
    (flat-named-contract
     (format "(>/c ~a)" x)
     (lambda (y) (and (number? y) (> y x)))))

  (define natural-number?
    (flat-named-contract
     "natural-number?"
     (lambda (x)
       (and (number? x)
	    (integer? x)
	    (x . >= . 0)))))
  
  (define (integer-in start end)
    (unless (and (integer? start)
                 (integer? end))
      (error 'integer-in "expected two integers as arguments, got ~e and ~e" start end))
    (flat-named-contract 
     (format "(integer-in ~a ~a)" start end)
     (lambda (x)
       (and (integer? x)
            (<= start x end)))))

  (define (real-in start end)
    (unless (and (real? start)
                 (real? end))
      (error 'real-in "expected two real numbers as arguments, got ~e and ~e" start end))
    (flat-named-contract 
     (format "(real-in ~a ~a)" start end)
     (lambda (x)
       (and (real? x)
            (<= start x end)))))

  (define (test-flat-contract f x)
    (if (flat-contract? f)
        ((flat-contract-predicate f) x)
        (f x)))
  
  (define (and/c . fs)
    (for-each
     (lambda (x) 
       (unless (or (contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'and/c "expected procedures of arity 1 or <contract>s, given: ~e" x)))
     fs)
    (cond
      [(null? fs) any?]
      [(andmap flat-contract/predicate? fs)
       (let* ([to-predicate
	       (lambda (x)
		 (if (flat-contract? x)
		     (flat-contract-predicate x)
		     x))]
	      [pred
	       (let loop ([pred (to-predicate (car fs))]
			  [preds (cdr fs)])
		 (cond
		   [(null? preds) pred]
		   [else
		    (let* ([fst (to-predicate (car preds))])
		      (loop (let ([and/c-contract? (lambda (x) (and (pred x) (fst x)))])
                              and/c-contract?)
			    (cdr preds)))]))])
	 (flat-contract pred))]
      [else
       (let* ([contracts (map (lambda (x) (if (contract? x) x (flat-contract x))) fs)]
              [contract/procs (map contract-proc contracts)])
	 (make-contract
          (apply build-compound-type-name "and/c" contracts)
           (lambda (pos neg src-info orig-str)
             (let ([partial-contracts (map (lambda (contract/proc) (contract/proc pos neg src-info orig-str))
                                           contract/procs)])
               (let loop ([ctct (car partial-contracts)]
                          [rest (cdr partial-contracts)])
                 (cond
                   [(null? rest) ctct]
                   [else 
                    (let ([fst (car rest)])
                      (loop (lambda (x) (fst (ctct x)))
                            (cdr rest)))]))))))]))

  (define (not/f f)
    (unless (flat-contract/predicate? f)
      (error 'not/f "expected a procedure of arity 1 or <flat-named-contract>, given: ~e" f))
    (flat-named-contract
     (build-compound-type-name "not/f" f)
     (lambda (x)
       (not (test-flat-contract f x)))))

  (define (is-a?/c <%>)
    (unless (or (interface? <%>)
		(class? <%>))
      (error 'is-a?/c "expected <interface> or <class>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (flat-named-contract
       (cond
         [name
          (format "(is-a?/c ~a)" name)]
         [(class? <%>)
          "(is-a?/c unknown%)"]
         [else "(is-a?/c unknown<%>)"])
       (lambda (x) (is-a? x <%>)))))

  (define (listof p)
    (unless (flat-contract/predicate? p)
      (error 'listof "expected a flat contract or procedure of arity 1 as argument, got: ~e" p))
    (flat-named-contract
     (build-compound-type-name "listof" p)
     (lambda (v)
       (and (list? v)
	    (andmap (lambda (ele) (test-flat-contract p ele))
		    v)))))
  
  (define-syntax (*-immutableof stx)
    (syntax-case stx ()
      [(_ predicate? fill type-name name)
       (syntax
        (let ([predicate?-name predicate?]
              [fill-name fill])
          (lambda (input)
            (let* ([ctc (coerce-contract name input)]
                   [p (contract-proc ctc)])
              (make-contract
               (build-compound-type-name 'name ctc)
               (lambda (pos neg src-info orig-str)
                 (let ([p-app (p pos neg src-info orig-str)])
                   (lambda (val)
                     (unless (predicate?-name val)
                       (raise-contract-error
                        src-info
                        pos
                        neg
                        orig-str
                        "expected <~a>, given: ~e"
                        'type-name
                        val))
                     (fill-name p-app val)))))))))]))
  
  (define (map-immutable f lst)
    (let loop ([lst lst])
      (cond
        [(pair? lst)
         (cons-immutable (f (car lst))
                         (loop (cdr lst)))]
        [(null? lst) null])))
  
  (define (immutable-list? lst)
    (cond
      [(and (pair? lst)
            (immutable? lst))
       (immutable-list? (cdr lst))]
      [(null? lst) #t]
      [else #f]))
  
  (define list-immutableof
    (*-immutableof immutable-list? map-immutable immutable-list list-immutableof))  

  (define vector-immutableof
    (*-immutableof (lambda (x) (and (vector? x) (immutable? x)))
                   (lambda (f v) (vector->immutable-vector (list->vector (map f (vector->list v)))))
                   immutable-vector
                   vector-immutableof))
  
  (define (vectorof p)
    (unless (flat-contract/predicate? p)
      (error 'vectorof "expected a flat contract or procedure of arity 1 as argument, got: ~e" p))
    (flat-named-contract
     (build-compound-type-name "vectorof" p)
     (lambda (v)
       (and (vector? v)
	    (andmap (lambda (ele) (test-flat-contract p ele))
		    (vector->list v))))))

  (define (vector/p . args)
    (unless (andmap flat-contract/predicate? args)
      (error 'vector/p "expected flat contracts as arguments, got: ~a"
             (let loop ([args args])
               (cond
                 [(null? args) ""]
                 [(null? (cdr args)) (format "~e" (car args))]
                 [else (string-append
                        (format "~e " (car args))
                        (loop (cdr args)))]))))
    (let ([largs (length args)])
      (flat-named-contract
       (apply build-compound-type-name "vector/p" args)
       (lambda (v)
         (and (vector? v)
              (= (vector-length v) largs)
              (andmap test-flat-contract
                      args
                      (vector->list v)))))))
  
  (define (box/p pred)
    (unless (flat-contract/predicate? pred)
      (error 'box/p "expected a flat contract or a procedure of arity 1, got: ~e" pred))
    (flat-named-contract
     (build-compound-type-name "box/p" pred)
     (lambda (x)
       (and (box? x)
	    (test-flat-contract pred (unbox x))))))

  (define (cons/p hdp tlp)
    (unless (and (flat-contract/predicate? hdp)
                 (flat-contract/predicate? tlp))
      (error 'cons/p "expected two flat contracts or procedures of arity 1, got: ~e and ~e" hdp tlp))
    (flat-named-contract
     (build-compound-type-name "cons/p" hdp tlp)
     (lambda (x)
       (and (pair? x)
            (test-flat-contract hdp (car x))
            (test-flat-contract tlp (cdr x))))))
  
  (define-syntax (*-immutable/c stx)
    (syntax-case stx ()
      [(_ predicate? constructor (arb? selectors ...) type-name name)
       (eq? #f (syntax-object->datum (syntax arb?)))
       (with-syntax ([(params ...) (generate-temporaries (syntax (selectors ...)))]
                     [(p-apps ...) (generate-temporaries (syntax (selectors ...)))]
                     [(procs ...) (generate-temporaries (syntax (selectors ...)))]
                     [(selector-names ...) (generate-temporaries (syntax (selectors ...)))])
         (syntax
          (let ([predicate?-name predicate?]
                [constructor-name constructor]
                [selector-names selectors] ...)
            (lambda (params ...)
              (let ([procs (coerce/select-contract name params)] ...)
                (make-contract
                 (build-compound-type-name 'name params ...)
                 (lambda (pos neg src-info orig-str)
                   (let ([p-apps (procs pos neg src-info orig-str)] ...)
                     (lambda (v)
                       (if (and (immutable? v)
                                (predicate?-name v))
                           (constructor-name (p-apps (selector-names v)) ...)
                           (raise-contract-error
                            src-info
                            pos
                            neg
                            orig-str
                            "expected <~a>, given: ~e"
                            'type-name
                            v)))))))))))]
      [(_ predicate? constructor (arb? selector) correct-size type-name name)
       (eq? #t (syntax-object->datum (syntax arb?)))
       (syntax
        (let ([predicate?-name predicate?]
              [constructor-name constructor]
              [selector-name selector])
          (lambda params
            (let ([procs (map (lambda (param) (coerce/select-contract name param)) params)])
              (make-contract
               (apply build-compound-type-name 'name params)
               (lambda (pos neg src-info orig-str)
                 (let ([p-apps (map (lambda (proc) (proc pos neg src-info orig-str)) procs)]
                       [count (length params)])
                   (lambda (v)
                     (if (and (immutable? v)
                              (predicate?-name v)
                              (correct-size count v))
                         (apply constructor-name 
                                (let loop ([p-apps p-apps]
                                           [i 0])
                                  (cond
                                    [(null? p-apps) null]
                                    [else (let ([p-app (car p-apps)])
                                            (cons (p-app (selector-name v i))
                                                  (loop (cdr p-apps) (+ i 1))))])))
                         (raise-contract-error
                          src-info
                          pos
                          neg
                          orig-str
                          "expected <~a>, given: ~e"
                          'type-name
                          v))))))))))]))
  
  (define cons-immutable/c (*-immutable/c pair? cons (#f car cdr) immutable-cons cons-immutable/c))
  (define box-immutable/c (*-immutable/c box? box (#f unbox) immutable-box box-immutable/c))
  (define vector-immutable/c (*-immutable/c vector?
                                            vector
                                            (#t (lambda (v i) (vector-ref v i)))
                                            (lambda (n v) (= n (vector-length v)))
                                            immutable-vector
                                            vector-immutable/c))
       
  (define (list/p . args)
    (unless (andmap flat-contract/predicate? args)
      (error 'list/p "expected flat contracts, got: ~a"
             (let loop ([args args])
               (cond
                 [(null? args) ""]
                 [(null? (cdr args)) (format "~e" (car args))]
                 [else (string-append
                        (format "~e " (car args))
                        (loop (cdr args)))]))))
    (let loop ([args args])
      (cond
	[(null? args) (flat-contract null?)]
	[else (cons/p (car args) (loop (cdr args)))])))
  
  (define (list-immutable/c . args)
    (unless (andmap (lambda (x) (or (contract? x)
                                    (and (procedure? x)
                                         (procedure-arity-includes? x 1))))
                    args)
      (error 'list/p "expected flat contracts or procedures of arity 1, got: ~a"
             (let loop ([args args]) 
               (cond
                 [(null? args) ""]
                 [(null? (cdr args)) (format "~e" (car args))]
                 [else (string-append
                        (format "~e " (car args))
                        (loop (cdr args)))]))))
    (let loop ([args args])
      (cond
	[(null? args) (flat-contract null?)]
	[else (cons-immutable/c (car args) (loop (cdr args)))])))

  (define (syntax/p c)
    (unless (flat-contract/predicate? c)
      (error 'syntax/p "expected argument of type <flat-contract> or procedure of arity 1, got ~e" c))
    (flat-named-contract
     (let ([pred (flat-contract-predicate c)])
       (lambda (val)
         (and (syntax? val)
              (pred (syntax-e val)))))))
  
  (define (flat-contract/predicate? pred)
    (or (flat-contract? pred)
        (and (procedure? pred)
             (procedure-arity-includes? pred 1))))
  
  ;; build-compound-type-name : (union symbol #f) (union contract symbol string) ... -> string
  (define (build-compound-type-name name . fs)
    (let* ([strs (map (lambda (x) (cond
                                    [(symbol? x)
                                     (format "~a" x)]
                                    [(string? x) x]
                                    [else (contract->type-name x)]))
                      fs)]
           [with-spaces
            (let loop ([strs strs])
              (cond
                [(null? strs) null]
                [else (cons " "
                            (cons (car strs)
                                  (loop (cdr strs))))]))])
      (cond
        [name
         (format "(~a~a)" name (apply string-append with-spaces))]
        [(null? with-spaces)
         "()"]
        [else 
         (format "(~a)" (apply string-append (cdr with-spaces)))])))
  
  (define (subclass?/c %)
    (unless (class? %)
      (error 'subclass?/c "expected type <class>, given: ~e" %))
    (let ([name (object-name %)])
      (flat-named-contract
       (if name
           (format "(subclass?/c ~a)" name)
	   "(subclass?/c unknown%)")
       (lambda (x) (subclass? x %)))))

  (define (implementation?/c <%>)
    (unless (interface? <%>)
      (error 'implementation?/c "expected <interface>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (flat-named-contract
       (if name
	   (format "(implementation?/c ~a)" name)
	   "(implementation?/c unknown<%>)")
       (lambda (x) (implementation? x <%>)))))

  (define mixin-contract (class? . ->d . subclass?/c))
  
  (define (make-mixin-contract . %/<%>s)
    ((and/c (flat-contract class?)
            (apply and/c (map sub/impl?/c %/<%>s)))
     . ->d .
     subclass?/c))

  (define (sub/impl?/c %/<%>)
    (cond
      [(interface? %/<%>) (implementation?/c %/<%>)]
      [(class? %/<%>) (subclass?/c %/<%>)]
      [else (error 'make-mixin-contract "unknown input ~e" %/<%>)]))
  
  )
