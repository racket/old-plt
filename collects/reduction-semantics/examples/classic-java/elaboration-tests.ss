;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elaboration-tests.ss
;;
;; Richard Cobbe
;; $Id: elaboration-tests.ss,v 1.3 2004/12/31 22:12:15 cobbe Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module elaboration-tests mzscheme

  (require (lib "etc.ss")
           "test.ss"
           "environment.ss"
           "ast.ss"
           "program.ss")
  (provide elaboration-tests)

  (require/expose "parser.ss" (parse-expr))
  (require/expose "elaboration.ss"
                  (methods-once fields-once methods-ok elab-class elab-expr
                                elab-method))

  (define-syntax assert-elab-exn
    (syntax-rules ()
      [(_ msg val expr)
       (assert-exn (lambda (exn)
                     (and (exn:cj:elab? exn)
                          (string=? msg (exn-message exn))
                          (equal? val (exn:cj:elab-obj exn))))
                   (lambda () expr))]))

  (define test-program
    (parse-program
     '((class base Object ([int x] [int y])
         (int get-x () (ref this x))
         (int get-y () 0))
       (class derived base ([bool y] [int z])
         (int get-x () (+ (ref this x) 3))
         (int get-z () (ref this z)))
       (class derived2 derived ([bool x]))
       (class math Object ()
         (int add ([int x] [int y]) (+ x y))
         (bool is-derived? ([Object o]) false)
         (bool foo? ([base b]) (== (send b get-x) 3)))
       (let d (new derived)
         (let dummy1 (set d x 3)
           (let dummy2 (set d z false)
             (send d get-x)))))))

  (define-syntax with-class
    (syntax-rules ()
      [(_ c-id body)
       (let ([c-id (find-class test-program (make-class-type (quote c-id)))])
         body)]))

  (define-syntax with-program/class
    (syntax-rules ()
      [(_ p-id c-id src body)
       (let* ([p-id (parse-program (quote src))]
              [c-id (find-class p-id (make-class-type (quote c-id)))])
         body)]))

  (define elaboration-tests
   (make-test-suite
    "ClassicJava Elaboration Tests"

    (make-test-case "methods unique"
      (assert-not-exn (lambda () (methods-once test-program))))

    (make-test-case "methods-once: duplicate method"
      (with-program/class program foo
                          ((class foo Object ()
                             (int bar () 3)
                             (Object bar () null))
                           3)
                          (assert-elab-exn "duplicate method definition"
                                           foo
                                           (methods-once program))))

    (make-test-case "fields unique"
      (assert-not-exn (lambda () (fields-once test-program))))

    (make-test-case "fields-once: duplicate field name"
      (with-program/class
       program foo
       ((class foo Object ([int x] [bool x]))
        3)
       (assert-elab-exn "duplicate field definition"
                        foo
                        (fields-once program))))

    (make-test-case "methods OK"
      (assert-not-exn (lambda () (methods-ok test-program))))

    (make-test-case "methods-ok: override breaks type"
      (with-program/class
       program derived
       ((class base Object ()
          (int m ([int i]) (+ i 3)))
        (class derived base ()
          (bool m ([int i]) (zero? i)))
        3)
       (assert-elab-exn "method override doesn't preserve type"
                        derived
                        (methods-ok program))))

    (make-test-case "methods-ok: unrelated classes are independent"
      (let* ([program (parse-program
                       '((class c1 Object ()
                           (int m ([int i]) (+ i 3)))
                         (class c2 Object ()
                           (Object m () null))
                         3))])
        (assert-not-exn (lambda () (methods-ok program)))))

    (make-test-case "elab-class: ok"
      (let* ([table (make-hash-table)]
             [object (make-class (make-class-type 'Object) #f null null)]
             [base (make-class (make-class-type 'base) object
                               (list (make-field (make-ground-type 'int)
                                                 (make-class-type 'base)
                                                 'x)
                                     (make-field (make-ground-type 'int)
                                                 (make-class-type 'base)
                                                 'y))
                               (list (make-method (make-ground-type 'int)
                                                  'get-x
                                                  null
                                                  null
                                                  (make-tagged-ref
                                                   (make-var-ref 'this)
                                                   (make-class-type 'base)
                                                   'x))
                                     (make-method (make-ground-type 'int)
                                                  'get-y
                                                  null
                                                  null
                                                  (make-num-lit 0))))]
             [derived
              (make-class (make-class-type 'derived) base
                          (list (make-field (make-ground-type 'bool)
                                            (make-class-type 'derived)
                                            'y)
                                (make-field (make-ground-type 'int)
                                            (make-class-type 'derived)
                                            'z))
                          (list
                           (make-method (make-ground-type 'int)
                                        'get-x
                                        null
                                        null
                                        (make-binary-prim
                                         '+
                                         (make-tagged-ref
                                          (make-var-ref 'this)
                                          (make-class-type 'base)
                                          'x)
                                         (make-num-lit 3)))
                           (make-method (make-ground-type 'int)
                                        'get-z
                                        null
                                        null
                                        (make-tagged-ref
                                         (make-var-ref 'this)
                                         (make-class-type 'derived)
                                         'z))))])
        (mv-assert equal?
                   (values
                    (elab-class test-program table
                                (find-class test-program
                                            (make-class-type 'derived)))
                    table)
                   derived
                   (hash-table ('Object object) ('base base)
                               ('derived derived)))))

    (make-test-case "elab-class: bad field type"
      (with-program/class
       p foo
       ((class foo Object ([bar z]))
        3)
       (assert-elab-exn "bad field type"
                        foo
                        (elab-class p (make-hash-table) foo))))

    (make-test-case "elab-method"
      (with-program/class
       p foo
       ((class foo Object ()
          (int add ([int x] [int y]) (+ (+ x y) 1)))
        3)
       (let ([m (car (class-methods foo))])
         (assert-equal? ((elab-method p foo) m)
                        (make-method (make-ground-type 'int)
                                     'add
                                     '(x y)
                                     (list (make-ground-type 'int)
                                           (make-ground-type 'int))
                                     (make-binary-prim
                                      '+
                                      (make-binary-prim
                                       '+ (make-var-ref 'x)
                                       (make-var-ref 'y))
                                      (make-num-lit 1)))))))

    (make-test-case "elab-method: body elaborated"
      (with-program/class
       p c
       ((class b Object ()
          (int m () 3))
        (class c b ()
          (int m () (+ (super m) 3)))
        3)
       (let ([m (car (class-methods c))])
         (assert-equal? ((elab-method p c) m)
                        (make-method (make-ground-type 'int)
                                     'm
                                     null
                                     null
                                     (make-binary-prim
                                      '+
                                      (make-tagged-super
                                       (make-class-type 'b)
                                       'm
                                       null)
                                      (make-num-lit 3)))))))

    (make-test-case "elab-method: bad return type"
      (with-program/class
       p c
       ((class c Object ()
          (d get-d () null))
        3)
       (let ([m (car (class-methods c))])
         (assert-elab-exn "method return type doesn't exist"
                          (list c m)
                          ((elab-method p c) m)))))

    (make-test-case "elab-method: bad arg type"
      (with-program/class
       p c
       ((class c Object ()
          (int foo ([d wrong]) 0))
        3)
       (let ([m (car (class-methods c))])
         (assert-elab-exn "method arg type doesn't exist"
                          (list c m)
                          ((elab-method p c) m)))))

    (make-test-case "elab-method: bad return type"
      (with-program/class
       p c
       ((class c Object ([foo f] [bar b])
          (foo get-foo () (ref this b)))
        (class foo Object ())
        (class bar Object ())
        3)
       (let ([m (car (class-methods c))])
         (assert-elab-exn "method return type incompatible w/ body"
                          (list c m)
                          ((elab-method p c) m)))))

    (make-test-case "elab-method: null body handled correctly"
      (with-program/class
       p c
       ((class c Object ()
          (c get-c () null))
        3)
       (assert-equal? ((elab-method p c) (car (class-methods c)))
                      (make-method (make-class-type 'c)
                                   'get-c
                                   null
                                   null
                                   (make-nil)))))

    (make-test-case "elab-expr: good ctor"
      (let ([e (make-new (make-class-type 'derived))])
        (mv-assert equal?
                   (elab-expr test-program (make-empty-env) e)
                   e
                   (make-class-type 'derived))))

    (make-test-case "elab-expr: ctor: nonexistent type"
      (let ([ctor (make-new (make-class-type 'foo))])
        (assert-elab-exn "constructor for nonexistent type"
                         (make-class-type 'foo)
                         (elab-expr test-program (make-empty-env) ctor))))

    (make-test-case "elab-expr: good var ref"
      (mv-assert equal?
                 (elab-expr test-program
                            (env (x (make-ground-type 'int))
                                 (y (make-class-type 'derived)))
                            (make-var-ref 'y))
                 (make-var-ref 'y)
                 (make-class-type 'derived)))

    (make-test-case "elab-expr: bad var ref"
      (assert-elab-exn "unbound identifier"
                       'x
                       (elab-expr test-program
                                  (make-empty-env)
                                  (make-var-ref 'x))))

    (make-test-case "elab-expr: null"
      (mv-assert equal?
                 (elab-expr test-program (make-empty-env) (make-nil))
                 (make-nil)
                 (make-any-type)))

    (make-test-case "elab-expr: num lit"
      (mv-assert equal?
                 (elab-expr test-program (make-empty-env) (make-num-lit 3))
                 (make-num-lit 3)
                 (make-ground-type 'int)))

    (make-test-case "elab-expr: bool lit"
      (mv-assert equal?
                 (elab-expr test-program (make-empty-env) (make-bool-lit #t))
                 (make-bool-lit #t)
                 (make-ground-type 'bool)))

    (make-test-case "elab-ref: good field direct"
      (mv-assert equal?
                 (elab-expr test-program
                            (env (b (make-class-type 'base)))
                            (parse-expr '(ref b x)))
                 (make-tagged-ref (make-var-ref 'b)
                                  (make-class-type 'base)
                                  'x)
                 (make-ground-type 'int)))

    (make-test-case "elab-ref: good field inherited"
      (mv-assert equal?
                 (elab-expr test-program
                            (env (d (make-class-type 'derived)))
                            (parse-expr '(ref d x)))
                 (make-tagged-ref (make-var-ref 'd)
                                  (make-class-type 'base)
                                  'x)
                 (make-ground-type 'int)))

    (make-test-case "elab-ref: expr of bogus type"
      (let ([ref (parse-expr '(ref null x))])
        (assert-elab-exn "ref: subexpr not of object type (possibly null)"
                         ref
                         (elab-expr test-program (make-empty-env) ref))))

    (make-test-case "elab-ref: expr of ground type"
      (let ([ref (parse-expr '(ref (+ 3 4) x))])
        (assert-elab-exn "ref: subexpr not of object type (possibly null)"
                         ref
                         (elab-expr test-program (make-empty-env) ref))))

    (make-test-case "elab-ref: bad field name"
      (let ([ref (parse-expr '(ref d a))])
        (assert-elab-exn "ref: field doesn't exist"
                         ref
                         (elab-expr test-program
                                    (env (d (make-class-type 'derived)))
                                    ref))))

    (make-test-case "elab-ref: bad field name; only in subclass"
      (let ([ref (parse-expr '(ref d z))])
        (assert-elab-exn "ref: field doesn't exist"
                         ref
                         (elab-expr test-program
                                    (env (d (make-class-type 'base)))
                                    ref))))

    (make-test-case "elab-ref: shadowed direct"
      (mv-assert equal?
                 (elab-expr test-program
                            (env [b (make-class-type 'base)])
                            (parse-expr '(ref b y)))
                 (make-tagged-ref (make-var-ref 'b)
                                  (make-class-type 'base)
                                  'y)
                 (make-ground-type 'int)))

    (make-test-case "elab-ref: shadowed inherited"
      (mv-assert equal?
                 (elab-expr test-program
                            (env [d (make-class-type 'derived)])
                            (parse-expr '(ref d x)))
                 (make-tagged-ref (make-var-ref 'd)
                                  (make-class-type 'base)
                                  'x)
                 (make-ground-type 'int)))

    (make-test-case "elab-ref: shadowing direct"
      (mv-assert equal?
                 (elab-expr test-program
                            (env [d (make-class-type 'derived)])
                            (parse-expr '(ref d y)))
                 (make-tagged-ref (make-var-ref 'd)
                                  (make-class-type 'derived)
                                  'y)
                 (make-ground-type 'bool)))

    (make-test-case "elab-ref: shadowing inherited"
      (mv-assert equal?
                 (elab-expr test-program
                            (env [d2 (make-class-type 'derived2)])
                            (parse-expr '(ref d2 y)))
                 (make-tagged-ref (make-var-ref 'd2)
                                  (make-class-type 'derived)
                                  'y)
                 (make-ground-type 'bool)))

    (make-test-case "elab-send: good direct"
      (let ([s (parse-expr '(send b get-x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (b (make-class-type 'base)))
                              s)
                   s
                   (make-ground-type 'int))))

    (make-test-case "elab-send: good inherited"
      (let ([s (parse-expr '(send d get-y))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (d (make-class-type 'derived)))
                              s)
                   s
                   (make-ground-type 'int))))

    (make-test-case "elab-send: good overridden"
      (let ([s (parse-expr '(send d get-x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (d (make-class-type 'derived)))
                              s)
                   s
                   (make-ground-type 'int))))

    (make-test-case "elab-send: good w/ args"
      (let ([s (parse-expr '(send m is-derived? d))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (m (make-class-type 'math))
                                   (d (make-class-type 'derived2)))
                              s)
                   s
                   (make-ground-type 'bool))))

    (make-test-case "elab-send: good w/ args, subtyping"
      (let ([s (parse-expr '(send m is-derived? null))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (m (make-class-type 'math)))
                              s)
                   s
                   (make-ground-type 'bool))))

    (make-test-case "elab-send: bad object subexpr"
      (let ([s (parse-expr '(send (+ 3 4) is-derived? null))])
        (assert-elab-exn "send: subexpr not of object type (possibly null)"
                         s
                         (elab-expr test-program (env) s))))

    (make-test-case "elab-send: null literal"
      (let ([s (parse-expr '(send null is-derived? null))])
        (assert-elab-exn "send: subexpr not of object type (possibly null)"
                         s
                         (elab-expr test-program (env) s))))

    (make-test-case "elab-send: bad arguments"
      (let ([s (parse-expr '(send m is-derived? 3))])
        (assert-elab-exn "elab-args: arg type mismatch"
                         s
                         (elab-expr test-program
                                    (env (m (make-class-type 'math)))
                                    s))))

    (make-test-case "elab-send: bad arity"
      (let ([s (parse-expr '(send m add 3 4 5))])
        (assert-elab-exn "elab-args: arity mismatch"
                         s
                         (elab-expr test-program
                                    (env (m (make-class-type 'math)))
                                    s))))

    (make-test-case "elab-send: no such method"
      (let ([s (parse-expr '(send d get-a 3))])
        (assert-elab-exn "send: method doesn't exist"
                         s
                         (elab-expr test-program
                                    (env (d (make-class-type 'derived)))
                                    s))))

    (make-test-case "elab-super: ok"
      (let ([s (parse-expr '(super get-x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (this (make-class-type 'derived)))
                              s)
                   (make-tagged-super (make-class-type 'base)
                                      'get-x
                                      null)
                   (make-ground-type 'int))))

    (make-test-case "elab-super: arity mismatch"
      (let ([s (parse-expr '(super get-x 3))])
        (assert-elab-exn "elab-args: arity mismatch"
                         s
                         (elab-expr test-program
                                    (env (this (make-class-type 'derived)))
                                    s))))

    (make-test-case "elab-super: no such method"
      (let ([s (parse-expr '(super get-z))])
        (assert-elab-exn "super method doesn't exist"
                         s
                         (elab-expr test-program
                                    (env (this (make-class-type 'derived)))
                                    s))))

    (make-test-case "elab-cast: widening"
      (mv-assert equal?
                 (elab-expr test-program
                            (env (d (make-class-type 'derived)))
                            (parse-expr '(cast base d)))
                 (make-var-ref 'd)
                 (make-class-type 'base)))

    (make-test-case "elab-cast: narrowing"
      (let ([e (parse-expr '(cast derived b))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (b (make-class-type 'base)))
                              e)
                   e
                   (make-class-type 'derived))))

    (make-test-case "elab-cast: stupid"
      (let ([e (parse-expr '(cast math d))])
        (assert-elab-exn "cast between unrelated types"
                         e
                         (elab-expr test-program
                                    (env (d (make-class-type 'derived)))
                                    e))))

    (make-test-case "elab-let: straightforward"
      (let ([e (parse-expr '(let x y x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (y (make-class-type 'math)))
                              e)
                   e
                   (make-class-type 'math))))

    (make-test-case "elab-let: slightly more complex"
      (let ([e (parse-expr '(let x y (send x add 3 4)))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (y (make-class-type 'math)))
                              e)
                   e
                   (make-ground-type 'int))))

    (make-test-case "elab-unary: simple"
      (let ([e (parse-expr '(zero? 3))])
        (mv-assert equal?
                   (elab-expr test-program (env) e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-unary: subtyping"
      (let ([e (parse-expr '(null? x))])
        (mv-assert equal?
                   (elab-expr test-program (env (x (make-class-type 'derived)))
                              e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-unary: null arg"
      (let ([e (parse-expr '(null? null))])
        (mv-assert equal?
                   (elab-expr test-program (env) e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-unary: type mismatch"
      (let ([e (parse-expr '(null? 3))])
        (assert-elab-exn "unary primitive: bad arg type"
                         e
                         (elab-expr test-program (env) e))))

    (make-test-case "elab-binary: simple"
      (let ([e (parse-expr '(+ 3 4))])
        (mv-assert equal?
                   (elab-expr test-program (env) e)
                   e
                   (make-ground-type 'int))))

    (make-test-case "elab-binary: simple 2"
      (let ([e (parse-expr '(and true (or false true)))])
        (mv-assert equal?
                   (elab-expr test-program (env) e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-binary: type mismatch"
      (let ([e (parse-expr '(and true 0))])
        (assert-elab-exn "binary primitive: bad arg type"
                         e
                         (elab-expr test-program (env) e))))

    (make-test-case "elab-if: simple/ground"
      (let ([e (parse-expr '(if true 3 4))])
        (mv-assert equal?
                   (elab-expr test-program (env) e)
                   e
                   (make-ground-type 'int))))

    (make-test-case "elab-if: simple/object"
      (let ([e (parse-expr '(if true b c))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env [b (make-class-type 'derived)]
                                   [c (make-class-type 'derived)])
                              e)
                   e
                   (make-class-type 'derived))))

    (make-test-case "elab-if: lub"
      (let ([e (parse-expr '(if false d b))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env [b (make-class-type 'base)]
                                   [d (make-class-type 'derived)])
                              e)
                   e
                   (make-class-type 'base))))

    (make-test-case "elab-if: null lub"
      (let ([e (parse-expr '(if false d null))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env [d (make-class-type 'derived)])
                              e)
                   e
                   (make-class-type 'derived))))

    (make-test-case "elab-if: bad conditional"
      (let ([e (parse-expr '(if (- 3 3) 3 4))])
        (assert-elab-exn "if: conditional must have boolean type"
                         e
                         (elab-expr test-program (env) e))))

    (make-test-case "elab-if: unrelated branches"
      (let ([e (parse-expr '(if true 3 d))])
        (assert-elab-exn "if: branches have unrelated types"
                         e
                         (elab-expr test-program
                                    (env [d (make-class-type 'derived)])
                                    e))))
    )))
