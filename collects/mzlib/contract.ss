
(module contract mzscheme
  (provide (rename -contract contract)
           contract-=>
           ->
           ->d
           ->*
           ->d*
           case->
	   opt->
           opt->*
           class-contract
           class-contract/prim
           object-contract
           (rename -contract? contract?)
           provide/contract
           define/contract)

  (require-for-syntax mzscheme
                      "list.ss"
                      "match.ss"
                      (lib "stx.ss" "syntax")
                      (lib "name.ss" "syntax"))
  
  (require "private/class-sneaky.ss"
           "etc.ss")
  
  (require (lib "contract-helpers.scm" "mzlib" "private"))
  (require-for-syntax (prefix a: (lib "contract-helpers.scm" "mzlib" "private")))
  
  
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
                     [contract-id (datum->syntax-object 
                                   define-stx 
                                   (string->symbol
                                    (format
                                     "ACK-define/contract-contract-id-~a"
                                     (syntax-object->datum (syntax name)))))]
                     [id (datum->syntax-object 
                          define-stx
                          (string->symbol
                           (format
                            "ACK-define/contract-id-~a"
                            (syntax-object->datum (syntax name)))))])
         (syntax/loc define-stx 
          (begin
            (define contract-id contract-expr)
            (define-syntax name
              (make-set!-transformer
               (lambda (stx)
                 
                 ;; build-src-loc-string/unk : syntax -> (union #f string)
                 (define (build-src-loc-string/unk stx)
                   (let ([source (syntax-source stx)]
                         [line (syntax-line stx)]
                         [col (syntax-column stx)]
                         [pos (syntax-position stx)])
                     (cond
                       [(and (string? source) line col)
                        (format "~a: ~a.~a" source line col)]
                       [(and line col)
                        (format "~a.~a" line col)]
                       [(and (string? source) pos)
                        (format "~a: ~a" source pos)]
                       [pos
                        (format "~a" pos)]
                       [else #f])))
                 
                 (with-syntax ([neg-blame-str (or (build-src-loc-string/unk stx)
                                                  "")])
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
                                             (mangle-id "provide/contract-field-contract"
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
           (with-syntax ([id-rename (mangle-id "provide/contract-id" id)]
                         [contract-id (mangle-id "provide/contract-contract-id" id)]
                         [pos-module-source (mangle-id "provide/contract-pos-module-source" id)]
                         [pos-stx (datum->syntax-object provide-stx 'here)]
                         [module-source-as-symbol (datum->syntax-object provide-stx 'module-source-as-symbol)]
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
         
         ;; mangle-id : string syntax ... -> syntax
         ;; constructs a mangled name of an identifier from an identifier
         ;; the name isn't fresh, so `id' combined with `ids' must already be unique.
         (define (mangle-id prefix id . ids)
           (datum->syntax-object
            provide-stx
            (string->symbol
             (string-append
              prefix
              (format 
               "-~a~a-ACK-PLEASE_DONT_GUESS_THIS_ID"
               (syntax-object->datum id)
               (apply 
                string-append 
                (map 
                 (lambda (id)
                   (format "-~a" (syntax-object->datum id)))
                 ids)))))))
         
         (with-syntax ([(bodies ...) (code-for-each-clause (syntax->list (syntax (p/c-ele ...))))])
           (syntax 
            (begin
              (require (lib "contract-helpers.scm" "mzlib" "private"))
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

  ;; contract = (make-contract (alpha 
  ;;                            sym
  ;;                            sym
  ;;                            (union syntax #f)
  ;;                            -> 
  ;;                            alpha)
  ;;                            (contract contract alpha sym src-info -> alpha)
  ;; generic contract container; 
  ;; the first argument to wrap is the value to test the contract.
  ;; the second to wrap is a symbol representing the name of the positive blame
  ;; the third to wrap is the symbol representing the name of the negative blame
  ;; the fourth argument to wrap is the src-info.
  ;;
  ;; impl-builder and impl-info are two pieces used to build
  ;; implication contracts.
  (define-struct contract (wrap impl-builder))
  
  ;; proc-contract = (make-proc-contract ... <as above>
  ;;                                     (number boolean boolean -> (union false (vectorof contract)))
  (define-struct (proc-contract contract) (info))

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
	       (error 'flat-named-contract
                      "expected string and procedure of one argument as arguments, given: ~e and ~e"
		      name contract))
	     (make-flat-named-contract name contract))])
      flat-named-contract))

  (define -contract?
    (let ([contract?
           (lambda (val)
             (or (contract? val)  ;; refers to struct predicate
		 (flat-named-contract? val)
                 (and (procedure? val)
                      (procedure-arity-includes? val 1))))])
      contract?))

  (define-syntax -contract
    (lambda (stx)
      (syntax-case stx ()
        [(_ a-contract to-check pos-blame-e neg-blame-e)
         (with-syntax ([src-loc (syntax/loc stx here)])
           (syntax/loc stx
             (-contract a-contract to-check pos-blame-e neg-blame-e (quote-syntax src-loc))))]
        [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
         (syntax/loc stx
           (let ([a-contract a-contract-e]
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
               (error 'contract
                      "expected symbols as names for assigning blame, given: ~e and ~e, other args ~e ~e ~e"
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
             (check-contract a-contract name pos-blame neg-blame src-info)))])))

  ;; check-contract : contract any symbol symbol syntax -> any
  (define (check-contract contract val pos neg src-info)
    (cond
      [(contract? contract)
       ((contract-wrap contract) val pos neg src-info)]
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
	    (predicate->expected-msg contract)
	    val))]))

  (define-syntax (contract-=> stx)
    (syntax-case stx ()
      [(_ ant-e conq-e val-e tbb-e)
       (with-syntax ([src-loc (datum->syntax-object stx 'here)])
         (syntax/loc stx
           (contract-=> ant-e conq-e val-e tbb-e (quote-syntax src-loc))))]
      [(_ ant-e conq-e val-e tbb-e src-info-e)
       (syntax/loc stx
         (let ([c1 ant-e]
               [c2 conq-e]
               [val val-e]
               [tbb tbb-e]
               [src-info src-info-e])
           (unless (-contract? c1)
             (error 'contract-=> "expected a contract as first argument, given: ~e, other args ~e ~e ~e ~e" 
                    c1
                    c2
                    val
                    tbb
                    src-loc))
           (unless (-contract? c2)
             (error 'contract-=> "expected a contract as second argument, given: ~e, other args ~e ~e ~e ~e" 
                    c2
                    c1
                    val
                    tbb
                    src-loc))
           (unless (symbol? tbb)
             (error 'contract-=> 
                    "expected symbol as names for assigning blame, given: ~e, other args ~e ~e ~e ~e"
                    tbb
                    c1
                    c2
                    val
                    src-loc))
           (unless (syntax? src-info)
             (error 'contract "expected syntax as last argument, given: ~e, other args ~e ~e ~e ~e"
                    src-info
                    neg-blame 
                    pos-blame
                    a-contract 
                    name))
           (check-implication c1 c2 val tbb src-info)))]))

  ;; check-implication : contract contract any symbol (union syntax #f) -> any
  (define (check-implication antecedent consequent val tbb src-info)
    (cond
      [(and (contract? antecedent) (contract? consequent))
       ((contract-impl-builder consequent) 
        antecedent
        consequent
        val
        tbb 
        src-info)]
      [(or (contract? antecedent) (contract? consequent))
       (raise-contract-implication-error antecedent consequent val tbb src-info)]
      [else
       (let ([test-contract
              (lambda (c)
                (cond
                  [(flat-named-contract? c) ((flat-named-contract-predicate c) val)]
                  [else (c val)]))])
         (if (or (not (test-contract antecedent))
                 (test-contract consequent))
             val
             (raise-contract-implication-error antecedent consequent val tbb src-info)))]))

  ;; raise-contract-implication-error : contract contract any symbol (union syntax #f) -> alpha
  ;; escapes
  (define (raise-contract-implication-error antecedent consequent val tbb src-info)
    (let ([blame-src (src-info-as-string src-info)])
      (raise
       (make-exn
        (string->immutable-string
         (format "~a~a: ~a does not imply ~a for ~e"
                 blame-src
                 tbb
                 (contract->type-name antecedent)
                 (contract->type-name consequent)
                 val))
        (current-continuation-marks)))))
  
  ;; raise-contract-error : (union syntax #f) symbol symbol string args ... -> alpha
  ;; doesn't return
  (define (raise-contract-error src-info to-blame other-party fmt . args)
    (let ([blame-src (src-info-as-string src-info)]
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
           (let ([m (regexp-match "(.*)\\?" (symbol->string name))])
             (and m
                  (cadr m))))))

  ;; contract->type-name : contract -> string
  (define (contract->type-name c)
    (cond
      [(contract? c) "arrow contract"]
      [else (flat-contract->type-name c)]))
  
  ;; flat-contract->type-name : flat-contract -> string
  (define (flat-contract->type-name fc)
    (cond
      [(flat-named-contract? fc) (flat-named-contract-type-name fc)]
      [else (or (predicate->type-name fc)
                (format "unknown contract ~s" fc))]))

  
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

  
  (define-syntax-set (-> ->* ->d ->d* case-> object-contract class-contract class-contract/prim)
    
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
    
    ;; make-/proc : (syntax -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))) 
    ;;              syntax
    ;;           -> (syntax -> syntax)
    (define (make-/proc /h stx)
      (let-values ([(add-outer-check make-inner-check make-main impl-wrap impl-builder impl-info) (/h stx)])
        (let ([outer-args (syntax (val pos-blame neg-blame src-info))]
              [impl-args (syntax (ant conq val tbb src-info))])
          (with-syntax ([outer-args outer-args]
                        [inner-check (make-inner-check outer-args)]
                        [(inner-args body) (make-main outer-args)]
                        [(impl-builder-params impl-builder-body) impl-builder]
                        [impl-info impl-info])
            (with-syntax ([impl-first (impl-wrap (syntax (lambda impl-builder-params impl-builder-body)) impl-args)]
                          [inner-lambda
                           (set-inferred-name-from
                            stx
                            (syntax/loc stx (lambda inner-args body)))])
              (with-syntax ([impl-args impl-args])
                (add-outer-check
                 (set-inferred-name-from
                  stx
                  (syntax/loc stx
                    (make-proc-contract
                     (lambda outer-args
                       inner-check
                       inner-lambda)
                     (lambda impl-args impl-first)
                     impl-info))))))))))
    
    ;; case->/proc : syntax -> syntax
    ;; the transformer for the case-> macro
    (define (case->/proc stx)
      (syntax-case stx ()
        [(_ cases ...)
         (let-values ([(add-outer-check make-inner-check make-bodies wrap-impl impl-builder-cases impl-infos)
                       (case->/h stx (syntax->list (syntax (cases ...))))])
           (let ([outer-args (syntax (val pos-blame neg-blame src-info))]
                 [impl-args (syntax (ant conq val tbb src-info))])
             (ensure-cases-disjoint stx (extract-argument-lists impl-builder-cases))
             (with-syntax ([outer-args outer-args]
                           [(inner-check ...) (make-inner-check outer-args)]
                           [(body ...) (make-bodies outer-args)]
                           [(impl-builder-case ...) impl-builder-cases]
                           [(impl-info ...) impl-infos])               
               (with-syntax ([inner-lambda 
                              (set-inferred-name-from
                               stx
                               (syntax/loc stx (case-lambda body ...)))]
                             [impl-lambda-body
                              (wrap-impl
                               (set-inferred-name-from
                                stx
                                (syntax/loc stx (case-lambda impl-builder-case ...)))
                               impl-args)])
                 (with-syntax ([impl-args impl-args])
                   (add-outer-check
                    (syntax/loc stx
                      (make-proc-contract
                       (lambda outer-args
                         inner-check ...
                         inner-lambda)
                       (lambda impl-args impl-lambda-body)
                       (lambda (x y z) (or (impl-info x y z) ...))))))))))]))
    
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
                            (raise-syntax-error 'case-> (format "found multiple cases with ~a arguments" this-case) stx)]
                           [(and dot-min (dot-min . <= . this-case))
                            (raise-syntax-error 'case-> 
                                                (format "found overlapping cases (~a+ followed by ~a)" dot-min this-case)
                                                stx)]
                           [else (set! individual-cases (cons this-case individual-cases))])]
                        [(pair? this-case)
                         (let ([new-dot-min (car this-case)])
                           (cond
                             [dot-min
                              (if (dot-min . <= . new-dot-min)
                                  (raise-syntax-error 'case->
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
    
    
    ;; case->/h : syntax (listof syntax) -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax) (syntax -> syntax) syntax syntax)
    ;; like the other /h functions, but composes the wrapper functions
    ;; together and combines the cases of the case-lambda into a single list.
    (define (case->/h orig-stx cases)
      (let loop ([cases cases])
        (cond
          [(null? cases) (values (lambda (x) x)
                                 (lambda (args) (syntax ()))
                                 (lambda (args) (syntax ()))
                                 (lambda (x arg-stx) x)
                                 (syntax ())
                                 (syntax ()))]
          [else
           (let ([/h (select/h (car cases) 'case-> orig-stx)])
             (let-values ([(add-outer-checks make-inner-checks make-bodies wrap-impls impl-builder-cases impl-infos)
                           (loop (cdr cases))]
                          [(add-outer-check make-inner-check make-body wrap-impl impl-builder-case impl-info)
                           (/h (car cases))])
               (values
                (lambda (x) (add-outer-check (add-outer-checks x)))
                (lambda (args)
                  (with-syntax ([checks (make-inner-checks args)]
                                [check (make-inner-check args)])
                    (syntax (check . checks))))
                (lambda (args)
                  (with-syntax ([case (make-body args)]
                                [cases (make-bodies args)])
                    (syntax (case . cases))))
                (lambda (body arg-stx) (wrap-impl (wrap-impls body arg-stx) arg-stx))
                (with-syntax ([impl-builder-case impl-builder-case]
                              [impl-builder-cases impl-builder-cases])
                  (syntax (impl-builder-case . impl-builder-cases)))
                (with-syntax ([impl-info impl-info]
                              [impl-infos impl-infos])
                  (syntax (impl-info . impl-infos))))))])))
    
    (define (class-contract/proc stx) (class-contract-mo? stx #f))
    (define (class-contract/prim/proc stx) (class-contract-mo? stx #t))
    
    (define (class-contract-mo? stx use-make-object?)
      (syntax-case stx ()
        [(form (method-specifier meth-name meth-contract) ...)
         (and 
          (andmap method-specifier? (syntax->list (syntax (method-specifier ...))))
          (andmap identifier? (syntax->list (syntax (meth-name ...)))))
         (let* ([outer-args (syntax (val pos-blame neg-blame src-info))]
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
                        (raise-contract-error src-info pos-blame neg-blame "expected a class, got: ~e" val))
                      (let ([class-i (class->interface val)])
                        (void)
                        (unless (method-in-interface? 'meth-name class-i)
                          (raise-contract-error src-info
                                                pos-blame 
                                                neg-blame
                                                "expected class to have method ~a, got: ~e"
                                                'meth-name
                                                val))
                        ...)
                      (let ([c (class*/names-sneaky
                                (this super-init super-make) val ()
                                
                                (rename [super-meth-name meth-name] ...)
                                method ...
                                call-super-initializer)]
                            [ht (make-hash-table)])
                        (set-sneaky-class-contract-table! c ht)
                        (hash-table-put! ht 'meth-name meth-contract-var) ...
                        c)))
                  (lambda x (error 'impl-contract "unimplemented for class contracts")))))))]
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
      (syntax-case stx ()
        [(form (meth-name meth-contract) ...)
         (andmap identifier? (syntax->list (syntax (meth-name ...))))
         (let* ([outer-args (syntax (val pos-blame neg-blame src-info))]
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
                (lambda outer-args
                  (let ([meth-contract-var meth-contract] ...)
                    (unless (object? val)
                      (raise-contract-error src-info pos-blame neg-blame "expected an object, got: ~e" val))
                    (let ([obj-i (object-interface val)])
                      (void)
                      (unless (method-in-interface? 'meth-name obj-i)
                        (raise-contract-error src-info
                                              pos-blame 
                                              neg-blame
                                              "expected class to have method ~a, got: ~e"
                                              'meth-name
                                              val))
                      ...)
                    
                    (make-object/sneaky 
                     val
                     (class object%
                      method ...
                      (super-instantiate ())))))
                (lambda x (error 'impl-contract "unimplemented for object contracts"))))))]
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
      (with-syntax ([(val pos-blame neg-blame src-info) outer-args]
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
              (apply (check-contract contract-var
                                     other-method
                                     pos-blame
                                     neg-blame
                                     method-specific-src-info)
                     args))))))

    ;; make-class-wrapper-method : syntax syntax[identifier] syntax[identifier] syntax -> syntax
    ;; constructs a wrapper method that checks the pre and post-condition, and
    ;; calls the super method inbetween.
    (define (make-class-wrapper-method outer-args method-name contract-var contract-stx)
      (with-syntax ([(val pos-blame neg-blame src-info) outer-args]
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
                     [wrapped-method (check-contract contract-var
                                                     super-method
                                                     pos-blame
                                                     neg-blame
                                                     method-specific-src-info)])
                (apply 
                 (if super-contract
                     (check-implication contract-var
                                        super-contract
                                        wrapped-method
                                        pos-blame
                                        src-info)
                     wrapped-method)
                 args)))))))
    
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
    
    ;; Each of the /h functions builds three pieces of syntax:
    ;;  - code that binds the contract values to names and
    ;;    does error checking for the contract specs
    ;;    (were the arguments all contracts?)
    ;;  - code that does error checking on the contract'd value itself
    ;;    (is a function of the right arity?)
    ;;  - a piece of syntax that has the arguments to the wrapper
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
        [(_ ct ...)
         (let* ([rng-normal (car (last-pair (syntax->list (syntax (ct ...)))))]
                [ignore-range-checking?
                 (syntax-case rng-normal (any)
                   [any #t]
                   [_ #f])]
                [range-values
                 (syntax-case* rng-normal (any values) module-or-top-identifier=?
                   [any (syntax (any?))] ;; range-values isn't actually used in this case
                   [(values x ...)
                    (syntax (x ...))]
                   [_ (with-syntax ([rng-normal rng-normal])
                        (syntax (rng-normal)))])])
           (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (ct ...))))])
             (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                           [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                           [(rng-x ...) (generate-temporaries range-values)]
                           [(rng ...) range-values]
                           [arity (length (syntax->list (syntax (dom ...))))])
               (let ([->add-outer-check
                      (lambda (body)
                        (with-syntax ([body body])
                          (syntax/loc stx
                           (let ([dom-x dom] ...
                                 [rng-x rng] ...)
                             (unless (-contract? dom-x)
                               (error '-> "expected contract as argument, given: ~e" dom-x)) ...
                             (unless (-contract? rng-x)
                               (error '-> "expected contract as argument, given: ~e" rng-x)) ...
                             body))))]
                     [->body (syntax (->* (dom-x ...) (rng-x ...)))])
                 (let-values ([(->*add-outer-check 
                                ->*make-inner-check 
                                ->*make-body
                                impl-wrap
                                impl-builder
                                impl-info)
                               (->*/h ->body)])
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
                          (->*make-body stx)))
                    impl-wrap
                    impl-builder 
                    impl-info))))))]))
    
    ;; ->*/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->*/h stx)
      (syntax-case stx ()
        [(_ (dom ...) (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(res-x ...) (generate-temporaries (syntax (rng ...)))])
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
                              (procedure-arity-includes? val dom-length))
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
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
                            ...))))))
            (lambda (body arg-stx)
              (with-syntax ([(ant conq val tbb src-info) arg-stx]
                            [body body])
                (syntax
                 (if (and (procedure? val)
                          (procedure-arity-includes? val dom-length)
                          (proc-contract? ant))
                     (let* ([ant-info (proc-contract-info ant)]
                            [dom-ant-info (ant-info dom-length #t #f)]
                            [rng-ant-info (ant-info rng-length #f #f)])
                       (if (and rng-ant-info dom-ant-info)
                           (let ([dom-ant-x (vector-ref dom-ant-info dom-index)] ...
                                 [rng-ant-x (vector-ref rng-ant-info rng-index)] ...)
                             body)
                           val))
                     (raise-contract-implication-error ant conq val tbb src-info)))))
            (syntax
             ((arg-x ...)
              (let-values ([(res-x ...)
                            (val (check-implication dom-x dom-ant-x arg-x tbb src-info) ...)])
                (values 
                 (check-implication rng-ant-x rng-x res-x tbb src-info) ...))))
            (syntax 
             (lambda (len dom? and-more?)
               (if and-more?
                   #f
                   (if dom?
                       (cond
                         [(= len dom-length) (vector dom-x ...)]
                         [else #f])
                       (cond
                         [(= len rng-length) (vector rng-x ...)]
                         [else #f])))))))]
        [(_ (dom ...) rest (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
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
              (with-syntax ([(val check-rev-contract check-same-contract failure) stx])
                (syntax
                 (unless (procedure? val)
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
                    val)))))
            (lambda (stx)
              (with-syntax ([(val pos-blame neg-blame src-info) stx])
                (syntax
                 ((arg-x ... . arg-rest-x)
                  (let-values ([(res-x ...)
                                (apply
                                 val
                                 (check-contract dom-x arg-x neg-blame pos-blame src-info)
                                 ...
                                 (check-contract dom-rest-x arg-rest-x neg-blame pos-blame src-info))])
                    (values (check-contract
                             rng-x 
                             res-x
                             pos-blame
                             neg-blame
                             src-info)
                            ...))))))
            (lambda (body arg-stx)
              (with-syntax ([(ant conq val tbb src-info) arg-stx]
                            [body body])
                (syntax
                 (if (and (procedure? val)
                          (procedure-arity-includes? val dom-length)
                          (proc-contract? ant))
                     (let* ([ant-info (proc-contract-info ant)]
                            [dom-ant-info (ant-info dom-length #t #t)]
                            [rng-ant-info (ant-info rng-length #f #t)])
                       (if (and rng-ant-info dom-ant-info)
                           (let ([dom-ant-rest-x (vector-ref dom-ant-info 0)]
                                 [dom-ant-x (vector-ref dom-ant-info (+ dom-index 1))] ...
                                 [rng-ant-x (vector-ref rng-ant-info rng-index)] ...)
                             body)
                           (raise-contract-implication-error ant conq val tbb src-info)))
                     (raise-contract-implication-error ant conq val tbb src-info)))))
            (syntax
             ((arg-x ... . arg-rest-x)
              (let-values ([(res-x ...)
                            (apply
                             val 
                             (check-implication dom-x dom-ant-x arg-x tbb src-info) ...
                             (check-implication dom-rest-x dom-ant-rest-x arg-rest-x tbb src-info))])
                (values 
                 (check-implication rng-ant-x rng-x res-x tbb src-info) ...))))
            (syntax 
             (lambda (len dom? and-more?)
               (if and-more?
                   (if dom?
                       (cond
                         [(= len dom-length) (vector dom-rest-x dom-x ...)]
                         [else #f])
                       (cond
                         [(= len rng-length) (vector rng-x ...)]
                         [else #f]))
                   #f)))))]))
    
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
                       src-info))))))
              (lambda (body stx) (syntax (lambda x (error 'impl-contract "unimplemented for ->d"))))
              (syntax (x (error 'impl-contract "unimplemented for ->d")))
              (syntax (lambda x (error 'impl-contract "unimplemented for ->d"))))))]))
    
    ;; ->d*/h : stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->d*/h stx)
      (syntax-case stx ()
        [(_ (dom ...) rng-mk)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (values
            (lambda (body)
              (with-syntax ([body body])
                (syntax
                 (let ([dom-x dom] ...
                       [rng-mk-x rng-mk])
                   (unless (-contract? dom-x)
                     (error '->*d "expected contract as argument, given: ~e" dom-x)) ...
                   (unless (and (procedure? rng-mk-x)
                                (procedure-arity-includes? rng-mk-x dom-length))
                     (error '->*d "expected range position to be a procedure that accepts ~a arguments, given: ~e"
                            dom-length rng-mk-x))
                   body))))
            (lambda (stx)
              (with-syntax ([(val pos-blame neg-blame src-info) stx])
                (syntax
                 (unless (and (procedure? val)
                              (procedure-arity-includes? val dom-length))
                   (raise-contract-error
                    src-info
                    pos-blame
                    neg-blame
                    "expected a procedure that accepts ~a arguments, given: ~e"
                    dom-length
                    val)))))
            (lambda (stx)
              (with-syntax ([(val pos-blame neg-blame src-info) stx])
                (syntax
                 ((arg-x ...)
                  (call-with-values
                   (lambda () (rng-mk-x arg-x ...))
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
                              results))))))))))
            (lambda (body arg-stx)
              (with-syntax ([(ant conq val tbb src-info) arg-stx]
                            [body body])
                (syntax
                 (error '->d* "=> contracts unimplemented"))))
            (syntax
             ((arg-x ...)
              (error '->d* "=> contracts unimplemented")))
            (syntax 
             (lambda (len dom? and-more?)
               #f))))]
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
                                 src-info ))
                              rng-contracts
                              results))))))))))
            (lambda (body stx) (syntax (lambda x (error 'impl-contract "unimplemented for ->d*"))))
            (syntax (x (error 'impl-contract "unimplemented for ->d*")))
            (syntax (lambda x (error 'impl-contract "unimplemented for ->d*")))))]))
    
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
      (cond
        [(null? contracts) 
         (make-flat-named-contract
          (apply build-compound-type-name "union" procs)
          (lambda (x)
            (ormap (lambda (proc) (test-flat-contract proc x))
                    procs)))]
        [else
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
               ((contract-wrap (car contracts)) val pos neg src-info)]))
          (lambda x (error 'impl-contract "unimplemented for union")))])))

  (provide and/f or/f not/f
           >=/c <=/c </c >/c 
           integer-in real-in
           string/len
	   natural-number?
           false? any? 
	   printable?
           symbols
           subclass?/c implementation?/c is-a?/c
           listof vectorof vector/p cons/p list/p box/p
	   mixin-contract make-mixin-contract)

  ;; test-flat-contract : (union pred flat-named-contract) any -> boolean
  (define (test-flat-contract flat-contract x)
    (cond
      [(flat-named-contract? flat-contract)
       ((flat-named-contract-predicate flat-contract) x)]
      [else
       (flat-contract x)]))

  
  ;; flat-contract? : any -> boolean?
  ;; determines if a value is a flat contract
  (define (flat-contract? fc)
    (or (flat-named-contract? fc)
	(and (procedure? fc)
	     (procedure-arity-includes? fc 1))))

  (define (build-compound-type-name name . fs)
    (let ([strs (map flat-contract->type-name fs)])
      (format "(~a~a)" 
              name
              (apply string-append
                     (let loop ([strs strs])
                       (cond
                         [(null? strs) null]
                         [else (cons " "
                                     (cons (car strs)
                                           (loop (cdr strs))))]))))))
  
  (define (string/len n)
    (unless (number? n)
      (error 'string/len "expected a number as argument, got ~e" n))
    (make-flat-named-contract 
     (format "string (up to ~a characters)" n)
     (lambda (x)
       (and (string? x)
            ((string-length x) . < . n)))))
  
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

  (define printable?
    (make-flat-named-contract
     "printable"
     (lambda (x)
       (let printable? ([x x])
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
		  (printable? (unbox x))))))))

  (define (and/f . fs)
    (for-each
     (lambda (x) 
       (unless (or (flat-named-contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'and/f "expected procedures of arity 1 or <flat-named-contract>s, given: ~e" x)))
     fs)
    (make-flat-named-contract
     (apply build-compound-type-name "and/f" fs)
     (lambda (x)
       (andmap (lambda (f) (test-flat-contract f x))
               fs))))
  
  (define (or/f . fs)
    (for-each
     (lambda (x) 
       (unless (or (flat-named-contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'or/f "expected procedures of arity 1 or <flat-named-contract>s, given: ~e" x)))
     fs)
    (make-flat-named-contract
     (apply build-compound-type-name "or/f" fs)
     (lambda (x)
       (ormap (lambda (f) (test-flat-contract f x))
              fs))))

  (define (not/f f)
    (unless (or (flat-named-contract? f)
                (and (procedure? f)
                     (procedure-arity-includes? f 1)))
      (error 'not/f "expected a procedure of arity 1 or <flat-named-contract>, given: ~e" f))
    (make-flat-named-contract
     (build-compound-type-name "not/f" f)
     (lambda (x)
       (not (f x)))))
  
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

  (define natural-number?
    (make-flat-named-contract
     "natural-number"
     (lambda (x)
       (and (number? x)
	    (integer? x)
	    (x . >= . 0)))))

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

  (define false?
    (make-flat-named-contract
     "false"
     (lambda (x) (not x))))
  
  (define any?
    (make-flat-named-contract
     "any"
     (lambda (x) #t)))

  (define (listof p)
    (unless (flat-contract? p)
      (error 'listof "expected a flat contract as argument, got: ~e" p))
    (make-flat-named-contract
     (build-compound-type-name "listof" p)
     (lambda (v)
       (and (list? v)
	    (andmap (lambda (ele) (test-flat-contract p ele))
		    v)))))
  
  (define (vectorof p)
    (unless (flat-contract? p)
      (error 'vectorof "expected a flat contract as argument, got: ~e" p))
    (make-flat-named-contract
     (build-compound-type-name "vectorof" p)
     (lambda (v)
       (and (vector? v)
	    (andmap (lambda (ele) (test-flat-contract p ele))
		    (vector->list v))))))

  (define (vector/p . args)
    (unless (andmap flat-contract? args)
      (error 'vector/p "expected flat contracts as arguments, got: ~a"
             (let loop ([args args])
               (cond
                 [(null? args) ""]
                 [(null? (cdr args)) (format "~e" (car args))]
                 [else (string-append
                        (format "~e " (car args))
                        (loop (cdr args)))]))))
    (make-flat-named-contract
     (apply build-compound-type-name "vector/p" args)
     (lambda (v)
       (and (vector? v)
            (= (vector-length v) (length args))
            (andmap test-flat-contract
                    args
                    (vector->list v))))))
  
  (define (integer-in start end)
    (unless (and (integer? start)
                 (integer? end))
      (error 'integer-in "expected two integers as arguments, got ~e and ~e" start end))
    (make-flat-named-contract 
     (format "integer between ~a and ~a, inclusive" start end)
     (lambda (x)
       (and (integer? x)
            (<= start x end)))))
  
  (define (real-in start end)
    (unless (and (real? start)
                 (real? end))
      (error 'real-in "expected two real numbers as arguments, got ~e and ~e" start end))
    (make-flat-named-contract 
     (format "real between ~a and ~a, inclusive" start end)
     (lambda (x)
       (and (real? x)
            (<= start x end)))))
  
  (define (box/p pred)
    (unless (flat-contract? pred)
      (error 'box/p "expected a flat contract, got: ~e" pred))
    (make-flat-named-contract
     (build-compound-type-name "box/p" pred)
     (lambda (x)
       (and (box? x)
	    (test-flat-contract pred (unbox x))))))

  (define (cons/p hdp tlp)
    (unless (and (flat-contract? hdp)
                 (flat-contract? tlp))
      (error 'cons/p "expected two flat contracts, got: ~e and ~e" hdp tlp))
    (make-flat-named-contract
     (build-compound-type-name "cons/p" hdp tlp)
     (lambda (x)
       (and (pair? x)
            (test-flat-contract hdp (car x))
            (test-flat-contract tlp (cdr x))))))

  (define (list/p . args)
    (unless (andmap flat-contract? args)
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
	[(null? args) null?]
	[else (cons/p (car args) (loop (cdr args)))])))

  (define mixin-contract (class? . ->d . subclass?/c))

  (define (make-mixin-contract . %/<%>s)
    ((and/f class? (apply and/f (map sub/impl?/c %/<%>s)))
     . ->d .
     subclass?/c))

  (define (sub/impl?/c %/<%>)
    (cond
      [(interface? %/<%>) (implementation?/c %/<%>)]
      [(class? %/<%>) (subclass?/c %/<%>)]
      [else (error 'make-mixin-contract "unknown input ~e" %/<%>)])))
