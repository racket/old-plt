(require-library "unitsig.ss")

(define-signature zodiac:misc^
  (pretty-print debug-level symbol-append flush-printf))

(define-signature zodiac:correlate^
  (make-correlator add-to-correlator find-in-correlator))

(define-signature zodiac:sexp^
  (structurize-syntax sexp->raw
    syntax-null? syntax-car syntax-cdr syntax-map
    new-mark mark-expression add/remove-mark expose-list))

(define-signature zodiac:pattern^
  (make-match&env match-against penv-merge pexpand extend-penv
    match-and-rewrite))

(define-signature zodiac:interface^
  (static-error dynamic-error internal-error))

(define-signature zodiac:expander^
  (expand expand-program expand-expr
    zodiac-user-parameterization
    add-micro-form add-macro-form
    add-list-micro add-ilist-micro add-lit-micro add-sym-micro
    get-list-micro get-ilist-micro get-lit-micro get-sym-micro
    make-attributes get-attribute put-attribute
    extend-env retract-env
    resolve resolve-in-env
    macro-resolution? micro-resolution? top-level-resolution?
    introduce-identifier introduce-fresh-identifier introduce-bound-id
    make-vocabulary copy-vocabulary merge-vocabulary))

(define-signature zodiac:scheme-core^
  (name-eq? marks-equal?
    parsed->raw extend-parsed->raw
    lexically-resolved? in-lexically-extended-env
    generate-name
    scheme-expand scheme-expand-program
    set-top-level-status get-top-level-status at-top-level?
    scheme-vocabulary
    (struct parsed (back))
    (struct varref (var))
    (struct top-level-varref ())          create-top-level-varref
    (struct top-level-varref/bind (slot)) create-top-level-varref/bind
    (struct bound-varref (binding))   create-bound-varref
    (struct lexical-varref ())        create-lexical-varref
    (struct app (fun args))           create-app
    (struct binding (var orig-name))  create-binding+marks
    (struct lexical-binding ())       create-lexical-binding+marks
    (struct form ())
    valid-syntactic-id? valid-syntactic-id/s?
    distinct-valid-syntactic-id/s?
    valid-id? valid-id/s?
    distinct-valid-id/s?
    language<=? language>=?
    optarglist-pattern
    (struct optarglist-entry (var+marks))
    (struct initialized-optarglist-entry (expr))
    (struct optarglist (vars))
    (struct sym-optarglist ())
    (struct list-optarglist ())
    (struct ilist-optarglist ())
    optarglist-decls-vocab
    make-optargument-list
    paroptarglist-pattern
    (struct paroptarglist-entry (var+marks))
    (struct initialized-paroptarglist-entry (expr))
    (struct paroptarglist (vars))
    (struct sym-paroptarglist ())
    (struct list-paroptarglist ())
    (struct ilist-paroptarglist ())
    paroptarglist-decls-vocab
    make-paroptargument-list
    arglist-pattern
    (struct arglist (vars))
    (struct sym-arglist ())
    (struct list-arglist ())
    (struct ilist-arglist ())
    arglist-decls-vocab
    make-argument-list))

(define-signature zodiac:scheme-main^
  (create-const
    (struct struct-form (type super fields))        create-struct-form
    (struct if-form (test then else))               create-if-form
    (struct quote-form (expr))                      create-quote-form
    (struct begin-form (bodies))                    create-begin-form
    (struct begin0-form (bodies))                   create-begin0-form
    (struct let-values-form (vars vals body))       create-let-values-form
    (struct letrec*-values-form (vars vals body))   create-letrec*-values-form
    (struct define-values-form (vars val))          create-define-values-form
    (struct set!-form (var val))                    create-set!-form
    (struct case-lambda-form (args bodies))         create-case-lambda-form
    generate-struct-names
    ))

(define-signature zodiac:scheme-objects^
  (create-class*-form
    (struct supervar-varref ())  create-supervar-varref
    (struct superinit-varref ()) create-superinit-varref
    (struct public-varref ())    create-public-varref
    (struct private-varref ())   create-private-varref
    (struct inherit-varref ())   create-inherit-varref
    (struct rename-varref ())    create-rename-varref
    (struct supervar-binding ())  create-supervar-binding+marks
    (struct superinit-binding ()) create-superinit-binding+marks
    (struct public-binding ())    create-public-binding+marks
    (struct private-binding ())   create-private-binding+marks
    (struct inherit-binding ())   create-inherit-binding+marks
    (struct rename-binding ())    create-rename-binding+marks
    (struct class*-form (this super-names super-exprs super-inits
			  init-vars inst-clauses))
    (struct public-clause (exports internals exprs))
    (struct private-clause (internals exprs))
    (struct inherit-clause (internals imports))
    (struct inherit-from-clause (super))
    (struct rename-clause (internals imports))
    (struct rename-from-clause (super))
    (struct sequence-clause (exprs))))

(define-signature zodiac:scheme-units^
  (create-unit-form
    create-compound-unit-form
    create-invoke-unit-form
    create-invoke-open-unit-form
    (struct unit-form (imports exports clauses))
    (struct compound-unit-form (imports links exports))
    (struct invoke-unit-form (unit variables))
    (struct invoke-open-unit-form (unit name-specifier variables))
    unit-clauses-vocab update-unresolved-attribute))

(define-signature zodiac:scheme-objects+units^
  ())

(define-signature zodiac:scheme-mrspidey^
  ((struct poly-form (exp))
    (struct :-form (exp type))
    (struct type:-form (type attrs))
    (struct st:control-form (para val))
    (struct cache-form (exp za kind cd))
    (struct define-type-form (sym type))
    (struct define-constructor-form (sym modes))
    create-poly-form
    create-:-form
    create-type:-form
    create-st:control-form
    create-cache-form
    create-define-type-form
    create-define-constructor-form))

(define-signature zodiac:back-protocol^
  (make-empty-back-box register-client))

(define-signature zodiac:system^
  ((open zodiac:structures^)
    (open zodiac:scanner-parameters^)
    (open zodiac:reader-structs^)
    (open zodiac:reader-code^)
    (open zodiac:sexp^)
    (open zodiac:pattern^)
    (open zodiac:correlate^)
    (open zodiac:back-protocol^)
    (open zodiac:expander^)
    (open zodiac:scheme-core^)
    (open zodiac:scheme-main^)
    (open zodiac:scheme-objects^)
    (open zodiac:scheme-units^)
    (open zodiac:scheme-objects+units^)
    (open zodiac:scheme-mrspidey^)))
