;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elaboration-tests.ss
;;
;; Richard Cobbe
;; $Id: elaboration-tests.ss,v 1.18 2004/04/21 21:40:45 cobbe Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module elaboration-tests mzscheme

  (require (lib "test.ss" "test")
           (lib "etc.ss")
           "environment.ss"
           "ast.ss"
           "program.ss")

  (require/expose "parser.ss" (parse-expr))
  (require/expose "elaboration.ss"
                  (methods-once fields-once methods-ok
                                fields-ok elab-class elab-expr
                                check-any-class check-contained-class
                                subset? elab-method))

  (define-assertion (assert-elab-exn msg val expr)
    (with-handlers ([exn:aj:elab?
                     (lambda (exn)
                       (and (string=? msg (exn-message exn))
                            (equal? val (exn:application-value exn))))]
                    [(lambda _ #t) (lambda _ #f)])
      (begin expr #f)))

  (define test-program
    (parse-program
     '((class base object () ([int x])
         (contain) (acquire)
         (int get-x () (ivar this x))
         (int get-y () 0))
       (class derived base (container) ([int z])
         (contain) (acquire [object acq-foo])
         (int get-x () (+ (ivar this x) 3))
         (int get-z () (ivar this z)))
       (class container object () ([object acq-foo])
         (contain [derived d]) (acquire))
       (class math object () () (contain) (acquire)
         (int add ([int x] [int y]) (+ x y))
         (bool is-derived? ([Object o]) false)
         (bool foo? ([base b]) (== (send b get-x) 3)))
       (let c (new container null (new derived 3 4))
         (send (ivar c d) get-x)))))

  (define-syntax with-program/class
    (syntax-rules ()
      [(_ p-id c-id src body)
       (let* ([p-id (parse-program (quote src))]
              [c-id (find-class p-id (make-class-type (quote c-id)))])
         body)]))

  (schemeunit-test
   (make-test-suite
    "AJava Elaboration Tests"

    (make-test-case "methods unique"
      (assert-void (methods-once test-program)))

    (make-test-case "methods-once: duplicate method"
      (with-program/class program foo
                          ((class foo object () ()
                             (contain) (acquire)
                             (int bar () 3)
                             (Object bar () null))
                           3)
                          (assert-elab-exn "duplicate method definition"
                                           foo
                                           (methods-once program))))

    (make-test-case "fields unique"
      (assert-void (fields-once test-program)))

    (make-test-case "fields-once: duplicate field name"
      (with-program/class
       program foo
       ((class foo object ()
          ([int x])
          (contain)
          (acquire [bool x]))
        3)
       (assert-elab-exn "duplicate field definition"
                        foo
                        (fields-once program))))

    (make-test-case "methods OK"
      (assert-void (methods-ok test-program)))

    (make-test-case "methods-ok: override breaks type"
      (with-program/class
       program derived
       ((class base object () () (contain) (acquire)
          (int m ([int i]) (+ i 3)))
        (class derived base () () (contain) (acquire)
          (bool m ([int i]) (zero? i)))
        3)
       (assert-elab-exn "method override doesn't preserve type"
                        derived
                        (methods-ok program))))

    (make-test-case "methods-ok: unrelated classes are independent"
      (let* ([program (parse-program
                       '((class c1 object () () (contain) (acquire)
                           (int m ([int i]) (+ i 3)))
                         (class c2 object () () (contain) (acquire)
                           (Object m () null))
                         3))])
        (assert-void (methods-ok program))))

    (make-test-case "fields OK"
      (assert-void (fields-ok test-program)))

    (make-test-case "fields-ok: field shadowed, same type"
      (with-program/class
       program derived
       ((class base object () ([int x]) (contain) (acquire))
        (class derived base () ([int x]) (contain)
          (acquire))
        3)
       (assert-elab-exn "shadowed field" derived
                        (fields-ok program))))

    (make-test-case "fields-ok: field shadowed, different type"
      (with-program/class
       program derived
       ((class base object () ([int x]) (contain) (acquire))
        (class derived base () () (contain [Object x])
          (acquire))
        3)
       (assert-elab-exn "shadowed field" derived
                        (fields-ok program))))

    (make-test-case "fields-ok: fields independent in unrelated classes"
      (assert-void
       (parse-program '((class c1 object () ([int x]) (contain) (acquire))
                        (class c2 object () ([bool x]) (contain) (acquire))
                        3))))

    (make-test-case "elab-class: ok"
      (let* ([table (make-hash-table)]
             [object (make-class (make-class-type 'object)
                                 #f null null null null null)]
             [base (make-class (make-class-type 'base) object null
                               (list (make-field (make-ground-type 'int)
                                                 'x
                                                 'normal))
                               null
                               null
                               (list (make-method (make-ground-type 'int)
                                                  'get-x
                                                  null
                                                  null
                                                  (make-ivar
                                                   (make-var-ref 'this)
                                                   'x))
                                     (make-method (make-ground-type 'int)
                                                  'get-y
                                                  null
                                                  null
                                                  (make-num-lit 0))))]
             [derived
              (make-class (make-class-type 'derived) base
                          (list (make-class-type 'container))
                          (list (make-field (make-ground-type 'int) 'z
                                            'normal))
                          null
                          (list (make-field (make-class-type 'object)
                                            'acq-foo
                                            'acquired))
                          (list
                           (make-method (make-ground-type 'int)
                                        'get-x
                                        null
                                        null
                                        (make-binary-prim
                                         '+
                                         (make-ivar (make-var-ref 'this) 'x)
                                         (make-num-lit 3)))
                           (make-method (make-ground-type 'int)
                                        'get-z
                                        null
                                        null
                                        (make-ivar (make-var-ref 'this)
                                                   'z))))])
        (mv-assert equal?
                   (values
                    (elab-class test-program table
                                (find-class test-program
                                            (make-class-type 'derived)))
                    table)
                   derived
                   (hash-table ('object object) ('base base)
                               ('derived derived)))))

    (make-test-case "elab-class: bad field type"
      (with-program/class
       p foo
       ((class foo Object () ([bar z])
          (contain)
          (acquire))
        3)
       (assert-elab-exn "bad field type"
                        foo
                        (elab-class p (make-hash-table) foo))))

    (make-test-case "elab-class: bad contained field type"
      (with-program/class
       p foo
       ((class foo Object () ()
          (contain [int x]) (acquire))
        3)
       (assert-elab-exn "contained field not object type"
                        foo
                        (elab-class p (make-hash-table) foo))))

    (make-test-case "elab-class: bad contained field type: not container"
      (with-program/class
       p foo
       ((class foo Object () () (contain [bar x]) (acquire))
        (class bar Object () () (contain) (acquire))
        3)
       (assert-elab-exn "class not container for contained field"
                        foo
                        (elab-class p (make-hash-table) foo))))

    (make-test-case "check-any-class: ok"
      (with-program/class
       p bar
       ((class foo Object () () (contain) (acquire))
        (class bar foo any () (contain) (acquire))
        3)
       (assert-true (begin (check-any-class p bar)
                           #t))))

    (make-test-case "check-any-class: bad acquired fields"
      (with-program/class
       p bar
       ((class foo Object (container) ()
          (contain) (acquire [int x]))
        (class bar foo any () (contain) (acquire))
        (class container Object () ([int x])
          (contain [foo f]) (acquire))
        3)
       (assert-elab-exn "class with 'any container cannot acquire"
                        bar
                        (check-any-class p bar))))

    (make-test-case "check-contained-class: good"
      (with-program/class
       p derived
       ((class container Object () ([int x])
          (contain [base b])
          (acquire))
        (class container2 Object () ([int x])
          (contain [derived d])
          (acquire))
        (class base Object (container) ([int y])
          (contain)
          (acquire [int x]))
        (class derived base (container container2)
          ([int z])
          (contain)
          (acquire))
        3)
       (assert-true
        (begin (check-contained-class p derived) #t))))

    (make-test-case "check-contained-class: subclass of any"
      (with-program/class
       p b
       ((class a Object any ([int x]) (contain) (acquire))
        (class b a (contained) ([int y]) (contain)
          (acquire [int x]))
        3)
       (assert-elab-exn "cannot specify containers for subclass of ANY"
                         b
                         (check-contained-class p b))))

    (make-test-case "check-contained-class: not superset of superclass"
      (with-program/class
       p b
       ((class a Object (c1 c2) ([int x]) (contain)
          (acquire [int y]))
        (class c1 Object () ([int y])
          (contain [a an-a]) (acquire))
        (class c2 Object () ([int y])
          (contain [a an-a]) (acquire))
        (class b a (c1) ([int z]) (contain) (acquire [int y]))
        3)
       (assert-elab-exn "class has fewer containers than superclass"
                        b
                        (check-contained-class p b))))

    (make-test-case "check-contained-class: acquire fields w/ no containers"
      (with-program/class
       p foo
       ((class foo Object () ([int x]) (contain)
          (acquire [int y]))
        3)
       (assert-elab-exn "class acquires fields with no containers"
                        foo
                        (check-contained-class p foo))))

    (make-test-case "check-contained-class: invalid container"
      (with-program/class
       p contained
       ((class container Object () () (contain) (acquire))
        (class contained Object (container)
          ([int x]) (contain) (acquire [int y]))
        3)
       (assert-elab-exn "class cannot be contained in container"
                        contained
                        (check-contained-class p contained))))

    (make-test-case "check-contained-class: invalid context"
      (with-program/class
       p contained
       ((class container Object ()
          ([int x] [int y]) (contain [contained c]) (acquire))
        (class contained Object (container)
          () (contain) (acquire [bool y]))
        3)
       (assert-elab-exn "class acquires field from invalid context"
                        contained
                        (check-contained-class p contained))))

    (make-test-case "subset"
      (assert-true (subset? '(x y z) '(a b c y x z))))

    (make-test-case "subset: empty"
      (assert-true (subset? null '(a b c d e f))))

    (make-test-case "subset: failed"
      (assert-false (subset? '(a b c d) '(a b c))))

    (make-test-case "elab-method"
      (with-program/class
       p foo
       ((class foo Object () () (contain) (acquire)
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
       ((class b Object () () (contain) (acquire)
          (int m () 3))
        (class c b () () (contain) (acquire)
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
       ((class c Object () () (contain) (acquire)
          (d get-d () null))
        3)
       (let ([m (car (class-methods c))])
         (assert-elab-exn "method return type doesn't exist"
                          (list c m)
                          ((elab-method p c) m)))))

    (make-test-case "elab-method: bad arg type"
      (with-program/class
       p c
       ((class c Object () () (contain) (acquire)
          (int foo ([d wrong]) 0))
        3)
       (let ([m (car (class-methods c))])
         (assert-elab-exn "method arg type doesn't exist"
                          (list c m)
                          ((elab-method p c) m)))))

    (make-test-case "elab-method: bad return type"
      (with-program/class
       p c
       ((class c Object () ([foo f] [bar b]) (contain) (acquire)
          (foo get-foo () (ivar this b)))
        (class foo Object () () (contain) (acquire))
        (class bar Object () () (contain) (acquire))
        3)
       (let ([m (car (class-methods c))])
         (assert-elab-exn "method return type incompatible w/ body"
                          (list c m)
                          ((elab-method p c) m)))))

    (make-test-case "elab-method: null body handled correctly"
      (with-program/class
       p c
       ((class c Object () () (contain) (acquire)
          (c get-c () null))
        3)
       (assert-equal? ((elab-method p c) (car (class-methods c)))
                      (make-method (make-class-type 'c)
                                   'get-c
                                   null
                                   null
                                   (make-nil)))))

    (make-test-case "elab-expr: good ctor"
      (let ([e (make-new (make-class-type 'derived)
                         (list (make-num-lit 3) (make-num-lit 4)))])
        (mv-assert equal?
                   (elab-expr test-program (make-empty-env)
                              (list (make-class-type 'container))
                              e)
                   e
                   (make-class-type 'derived))))

    (make-test-case "elab-expr: ctor: bad arity"
      (let ([ctor (make-new (make-class-type 'container) null)])
        (assert-elab-exn "arity mismatch in ctor"
                         ctor
                         (elab-expr test-program
                                    (make-empty-env)
                                    null
                                    ctor))))

    (make-test-case "elab-expr: ctor: bad arg type"
      (let ([ctor (make-new (make-class-type 'base)
                            (list (make-nil)))])
        (assert-elab-exn "arg type mismatch in ctor"
                         ctor
                         (elab-expr test-program
                                    (make-empty-env)
                                    null
                                    ctor))))

    (make-test-case "elab-expr: ctor: missing containment context"
      (let ([ctor (make-new (make-class-type 'derived)
                            (list (make-num-lit 3)
                                  (make-bool-lit #f)
                                  (make-num-lit 4)
                                  (make-nil)))])
        (assert-elab-exn "object must be constructed in context"
                         ctor
                         (elab-expr test-program
                                    (make-empty-env)
                                    null
                                    ctor))))

    (make-test-case "elab-expr: ctor: bad containment context"
      (let ([p (parse-program
                '((class c1 object () () (contain [d a-d]) (acquire))
                  (class c2 object () () (contain [d a-d]) (acquire))
                  (class d object (c1) () (contain) (acquire))
                  3))]
            [ctor (make-new (make-class-type 'c2)
                            (list (make-new (make-class-type 'd) null)))])
        (assert-elab-exn "object constructed in bad context"
                         (car (new-args ctor))
                         (elab-expr p
                                    (make-empty-env)
                                    null
                                    ctor))))

    (make-test-case "elab-expr: ctor: good containment context"
      (let ([ctor (make-new (make-class-type 'derived)
                            (list (make-num-lit 3)
                                  (make-num-lit 4)))])
      (mv-assert equal?
                 (elab-expr test-program
                            (make-empty-env)
                            (list (make-class-type 'container))
                            ctor)
                 ctor
                 (make-class-type 'derived))))

    (make-test-case "elab-expr: nonexistent type"
      (let ([ctor (make-new (make-class-type 'foo) null)])
        (assert-elab-exn "constructor for nonexistent type"
                         (make-class-type 'foo)
                         (elab-expr test-program
                                    (make-empty-env)
                                    null
                                    ctor))))

    (make-test-case "elab-expr: ctor: context with subtyping"
      (let ([program
             (parse-program
              '((class base object () () (contain [c a-c]) (acquire))
                (class derived base () () (contain) (acquire))
                (class c object (base) () (contain) (acquire))
                3))]
            [ctor (make-new
                   (make-class-type 'derived)
                   (list (make-new (make-class-type 'c) null)))])
        (mv-assert equal?
                   (elab-expr program (make-empty-env) null ctor)
                   ctor
                   (make-class-type 'derived))))

    (make-test-case "elab-expr: ctor: subtyping in arguments"
      (let ([program
             (parse-program
              '((class root object () ()
                  (contain [list l]) (acquire))
                (class list object (list root) () (contain) (acquire)
                  (int length () 0))
                (class empty list (list root) () (contain) (acquire))
                (class cons list (list root)
                  ([Object car])
                  (contain [list cdr])
                  (acquire)
                  (int length () (+ 1 (send cdr length))))
                3))]
            [ctor (make-new (make-class-type 'root)
                            (list (make-new (make-class-type 'cons)
                                            (list (make-nil)
                                                  (make-new
                                                   (make-class-type 'empty)
                                                   null)))))])
        (mv-assert equal?
                   (elab-expr program
                              (make-empty-env)
                              null
                              ctor)
                   ctor
                   (make-class-type 'root))))

    (make-test-case "elab-expr: good var ref"
      (mv-assert equal?
                 (elab-expr test-program
                            (env (x (make-ground-type 'int))
                                 (y (make-class-type 'derived)))
                            null
                            (make-var-ref 'y))
                 (make-var-ref 'y)
                 (make-class-type 'derived)))

    (make-test-case "elab-expr: bad var ref"
      (assert-elab-exn "unbound identifier"
                       'x
                       (elab-expr test-program
                                  (make-empty-env)
                                  null
                                  (make-var-ref 'x))))

    (make-test-case "elab-expr: null"
      (mv-assert equal?
                 (elab-expr test-program
                            (make-empty-env)
                            null
                            (make-nil))
                 (make-nil)
                 (make-any-type)))

    (make-test-case "elab-expr: num lit"
      (mv-assert equal?
                 (elab-expr test-program
                            (make-empty-env)
                            null
                            (make-num-lit 3))
                 (make-num-lit 3)
                 (make-ground-type 'int)))

    (make-test-case "elab-expr: bool lit"
      (mv-assert equal?
                 (elab-expr test-program
                            (make-empty-env)
                            null
                            (make-bool-lit #t))
                 (make-bool-lit #t)
                 (make-ground-type 'bool)))

    (make-test-case "elab-ivar: good ivar direct"
      (let ([ivar (parse-expr '(ivar b x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (b (make-class-type 'base)))
                              null
                              ivar)
                   ivar
                   (make-ground-type 'int))))

    (make-test-case "elab-ivar: good ivar inherited"
      (let ([ivar (parse-expr '(ivar d x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (d (make-class-type 'derived)))
                              null
                              ivar)
                   ivar
                   (make-ground-type 'int))))

    (make-test-case "elab-ivar: acquired ivar"
      (let ([ivar (parse-expr '(ivar d acq-foo))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (d (make-class-type 'derived)))
                              null
                              ivar)
                   ivar
                   (make-class-type 'object))))

    (make-test-case "elab-ivar: expr of bogus type"
      (let ([ivar (parse-expr '(ivar null x))])
        (assert-elab-exn "ivar: subexpr not of object type (possibly null)"
                         ivar
                         (elab-expr test-program
                                    (make-empty-env)
                                    null
                                    ivar))))

    (make-test-case "elab-ivar: expr of ground type"
      (let ([ivar (parse-expr '(ivar (+ 3 4) x))])
        (assert-elab-exn "ivar: subexpr not of object type (possibly null)"
                         ivar
                         (elab-expr test-program
                                    (make-empty-env)
                                    null
                                    ivar))))

    (make-test-case "elab-ivar: bad field name"
      (let ([ivar (parse-expr '(ivar d a))])
        (assert-elab-exn "ivar: field doesn't exist"
                         ivar
                         (elab-expr test-program
                                    (env (d (make-class-type 'derived)))
                                    null
                                    ivar))))

    (make-test-case "elab-ivar: bad field name; only in subclass"
      (let ([ivar (parse-expr '(ivar d z))])
        (assert-elab-exn "ivar: field doesn't exist"
                         ivar
                         (elab-expr test-program
                                    (env (d (make-class-type 'base)))
                                    null
                                    ivar))))

    (make-test-case "elab-send: good direct"
      (let ([s (parse-expr '(send b get-x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (b (make-class-type 'base)))
                              null
                              s)
                   s
                   (make-ground-type 'int))))

    (make-test-case "elab-send: good inherited"
      (let ([s (parse-expr '(send d get-y))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (d (make-class-type 'derived)))
                              null
                              s)
                   s
                   (make-ground-type 'int))))

    (make-test-case "elab-send: good overridden"
      (let ([s (parse-expr '(send d get-x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (d (make-class-type 'derived)))
                              null
                              s)
                   s
                   (make-ground-type 'int))))

    (make-test-case "elab-send: good w/ args"
      (let ([s (parse-expr '(send m is-derived? c))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (m (make-class-type 'math))
                                   (c (make-class-type 'container)))
                              null
                              s)
                   s
                   (make-ground-type 'bool))))

    (make-test-case "elab-send: good w/ args, subtyping"
      (let ([s (parse-expr '(send m is-derived? null))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (m (make-class-type 'math)))
                              null
                              s)
                   s
                   (make-ground-type 'bool))))

    (make-test-case "elab-send: bad object subexpr"
      (let ([s (parse-expr '(send (+ 3 4) is-derived? null))])
        (assert-elab-exn "send: subexpr not of object type (possibly null)"
                         s
                         (elab-expr test-program (env) null s))))

    (make-test-case "elab-send: null literal"
      (let ([s (parse-expr '(send null is-derived? null))])
        (assert-elab-exn "send: subexpr not of object type (possibly null)"
                         s
                         (elab-expr test-program (env) null s))))

    (make-test-case "elab-send: bad arguments"
      (let ([s (parse-expr '(send m is-derived? 3))])
        (assert-elab-exn "elab-args: arg type mismatch"
                         s
                         (elab-expr test-program
                                    (env (m (make-class-type 'math)))
                                    null
                                    s))))

    (make-test-case "elab-send: bad arity"
      (let ([s (parse-expr '(send m add 3 4 5))])
        (assert-elab-exn "elab-args: arity mismatch"
                         s
                         (elab-expr test-program
                                    (env (m (make-class-type 'math)))
                                    null
                                    s))))

    (make-test-case "elab-send: no such method"
      (let ([s (parse-expr '(send d get-a 3))])
        (assert-elab-exn "send: method doesn't exist"
                         s
                         (elab-expr test-program
                                    (env (d (make-class-type 'derived)))
                                    null
                                    s))))

    (make-test-case "elab-super: ok"
      (let ([s (parse-expr '(super get-x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (this (make-class-type 'derived)))
                              null
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
                                    null
                                    s))))

    (make-test-case "elab-super: no such method"
      (let ([s (parse-expr '(super get-z))])
        (assert-elab-exn "super method doesn't exist"
                         s
                         (elab-expr test-program
                                    (env (this (make-class-type 'derived)))
                                    null
                                    s))))

    (make-test-case "elab-cast: widening"
      (mv-assert equal?
                 (elab-expr test-program
                            (env (d (make-class-type 'derived)))
                            null
                            (parse-expr '(cast base d)))
                 (make-var-ref 'd)
                 (make-class-type 'base)))

    (make-test-case "elab-cast: narrowing"
      (let ([e (parse-expr '(cast derived b))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (b (make-class-type 'base)))
                              null
                              e)
                   e
                   (make-class-type 'derived))))

    (make-test-case "elab-cast: stupid"
      (let ([e (parse-expr '(cast math d))])
        (assert-elab-exn "cast between unrelated types"
                         e
                         (elab-expr test-program
                                    (env (d (make-class-type 'derived)))
                                    null
                                    e))))

    (make-test-case "elab-let: straightforward"
      (let ([e (parse-expr '(let x y x))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (y (make-class-type 'math)))
                              null
                              e)
                   e
                   (make-class-type 'math))))

    (make-test-case "elab-let: slightly more complex"
      (let ([e (parse-expr '(let x y (send x add 3 4)))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env (y (make-class-type 'math)))
                              null
                              e)
                   e
                   (make-ground-type 'int))))

    (make-test-case "elab-unary: simple"
      (let ([e (parse-expr '(zero? 3))])
        (mv-assert equal?
                   (elab-expr test-program (env) null e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-unary: subtyping"
      (let ([e (parse-expr '(null? x))])
        (mv-assert equal?
                   (elab-expr test-program (env (x (make-class-type 'derived)))
                              null
                              e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-unary: null arg"
      (let ([e (parse-expr '(null? null))])
        (mv-assert equal?
                   (elab-expr test-program (env) null e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-unary: type mismatch"
      (let ([e (parse-expr '(null? 3))])
        (assert-elab-exn "unary primitive: bad arg type"
                         e
                         (elab-expr test-program (env) null e))))

    (make-test-case "elab-binary: simple"
      (let ([e (parse-expr '(+ 3 4))])
        (mv-assert equal?
                   (elab-expr test-program (env) null e)
                   e
                   (make-ground-type 'int))))

    (make-test-case "elab-binary: simple 2"
      (let ([e (parse-expr '(and true (or false true)))])
        (mv-assert equal?
                   (elab-expr test-program (env) null e)
                   e
                   (make-ground-type 'bool))))

    (make-test-case "elab-binary: type mismatch"
      (let ([e (parse-expr '(and true 0))])
        (assert-elab-exn "binary primitive: bad arg type"
                         e
                         (elab-expr test-program (env) null e))))

    (make-test-case "elab-if: simple/ground"
      (let ([e (parse-expr '(if true 3 4))])
        (mv-assert equal?
                   (elab-expr test-program (env) null e)
                   e
                   (make-ground-type 'int))))

    (make-test-case "elab-if: simple/object"
      (let ([e (parse-expr '(if true b c))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env [b (make-class-type 'container)]
                                   [c (make-class-type 'container)])
                              null
                              e)
                   e
                   (make-class-type 'container))))

    (make-test-case "elab-if: lub"
      (let ([e (parse-expr '(if false d b))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env [b (make-class-type 'base)]
                                   [d (make-class-type 'derived)])
                              null
                              e)
                   e
                   (make-class-type 'base))))

    (make-test-case "elab-if: null lub"
      (let ([e (parse-expr '(if false d null))])
        (mv-assert equal?
                   (elab-expr test-program
                              (env [d (make-class-type 'derived)])
                              null
                              e)
                   e
                   (make-class-type 'derived))))

    (make-test-case "elab-if: bad conditional"
      (let ([e (parse-expr '(if (- 3 3) 3 4))])
        (assert-elab-exn "if: conditional must have boolean type"
                         e
                         (elab-expr test-program (env) null e))))

    (make-test-case "elab-if: unrelated branches"
      (let ([e (parse-expr '(if true 3 d))])
        (assert-elab-exn "if: branches have unrelated types"
                         e
                         (elab-expr test-program
                                    (env [d (make-class-type 'derived)])
                                    null
                                    e))))
    )))
