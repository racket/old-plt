;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; reduction-tests.ss
;; Richard Cobbe
;; $Id: reduction-tests.ss,v 1.3 2004/09/21 19:39:34 cobbe Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module reduction-tests mzscheme

  (require (lib "list.ss")
           (lib "test.ss" "test")
           "utils.ss"
           "ast.ss"
           "elaboration.ss"
           "parser.ss"
           "store.ss")
  (provide reduction-tests)

  (require/expose "reduction.ss" ())

  (define test-program-src
    '((class numerics object ([int i] [bool b])
        (int factorial ([int n])
             (if (zero? n) 1 (* n (send this factorial (- n 1))))))
      (class base object ([int base-field] [int shadowed-field])
        (int f () 3))
      (class derived base ([bool shadowed-field] [bool derived-field])
        (int f () (+ (super f) 1)))
      (class blist object ()
        (int length () -1)
        (bool andmap () false))
      (class bempty blist ()
        (int length () 0)
        (bool andmap () true))
      (class bcons blist
        ([bool value] [blist next])
        (int length () (+ 1 (send (ref this next) length)))
        (bool andmap () (if (ref this value)
                            (send (ref this next) andmap)
                            false)))
      (class dag-node object
        ([dag-node left] [dag-node right]))
      null))

  (define test-program (elab-program (parse-program test-program-src)))

  (define-syntax assert-functional-small-step
    (syntax-rules ()
      [(_ program store exp result)
       (assert-equal?
        (small-step cj-reductions `(,program ,store exp))
        `(,program ,store result))]))

  (define reduction-tests
   (make-test-suite "ClassicJava Reduction Tests"

     (make-test-case "constant/variable substitution"
       (assert-equal? (cj-subst 'x 3 '(send y x x null true false 4 this))
                      '(send y x 3 null true false 4 this)))

     (make-test-case "substitution: ref"
       (assert-equal? (cj-subst 'x 3 '(ref (send x fd) ctype x))
                      '(ref (send 3 fd) ctype x)))

     (make-test-case "substitution: send"
       (assert-equal? (foldl cj-subst '(send a b c (ref d t y) e null 3)
                             '(a b c d e)
                             '(6 7 8 9 10))
                      '(send 6 b 8 (ref 9 t y) 10 null 3)))

     (make-test-case "substitution: super"
       (assert-equal? (foldl cj-subst
                             '(super a b c d (ref e c field) f null 3)
                             '(a b c d e f)
                             '(11 12 13 14 15 16))
                      '(super 11 b c 14 (ref 15 c field) 16 null 3)))

     (make-test-case "substitution: cast"
       (assert-equal? (cj-subst 'x 42 '(cast x (ref x t fd)))
                      '(cast x (ref 42 t fd))))

     (make-test-case "substitution: let (not bound var)"
       (assert-equal? (cj-subst 'x 42 '(let y (ref x t z)
                                         (send (new a) x y z)))
                      '(let y (ref 42 t z) (send (new a) x y z))))
     ;; second x not renamed, since it's a method name not a variable.

     (make-test-case "substitution: let bound var"
       (assert-equal? (cj-subst 'x 42 '(let x (ref x t foo)
                                         (send (new a) x y z)))
                      '(let x (ref 42 t foo)
                         (send (new a) x y z))))

     (make-test-case "substitution: binary prim"
       (assert-equal? (cj-subst 'x 42 '(+ (ref x t size) (ref y t size)))
                      '(+ (ref 42 t size) (ref y t size))))

     (make-test-case "substitution: unary prim"
       (assert-equal? (cj-subst 'x 42 '(null? (ref x t parent)))
                      '(null? (ref 42 t parent))))

     (make-test-case "substitution: if"
       (assert-equal? (cj-subst 'x 42 '(if (ref x t flag)
                                           (ref x t this-field)
                                           (ref x t that)))
                      '(if (ref 42 t flag)
                           (ref 42 t this-field)
                           (ref 42 t that))))

     (make-test-case "t2r: ctor"
       (assert-equal? (texpr->rexpr
                       (make-new (make-class-type 'foo)))
                      '(new foo)))

     (make-test-case "t2r: var-ref"
       (assert-equal? (texpr->rexpr (make-var-ref 'var)) 'var))

     (make-test-case "t2r: null"
       (assert-equal? (texpr->rexpr (make-nil)) 'null))

     (make-test-case "t2r: tagged ref"
       (assert-equal? (texpr->rexpr
                       (make-tagged-ref (make-send (make-var-ref 'x)
                                                   'y
                                                   (list (make-nil)))
                                        (make-class-type 'foo)
                                        'field))
                      '(ref (send x y null) foo field)))

     (make-test-case "t2r: send"
       (assert-equal? (texpr->rexpr (make-send (make-var-ref 'x)
                                               'md
                                               (list (make-var-ref 'y)
                                                     (make-var-ref 'z))))
                      '(send x md y z)))

     (make-test-case "t2r: super"
       (assert-equal? (texpr->rexpr (make-tagged-super
                                     (make-class-type 'c)
                                     'md
                                     (list (make-var-ref 'y)
                                           (make-num-lit 42))))
                      '(super this c md y 42)))

     (make-test-case "t2r: cast"
       (assert-equal? (texpr->rexpr (make-cast (make-class-type 'c)
                                               (make-tagged-ref
                                                (make-var-ref 'x)
                                                (make-class-type 'foo)
                                                'fd)))
                      '(cast c (ref x foo fd))))

     (make-test-case "t2r: let"
       (assert-equal? (texpr->rexpr (make-cj-let 'id
                                                 (make-tagged-ref
                                                  (make-var-ref 'x)
                                                  (make-class-type 'foo)
                                                  'fd)
                                                 (make-send
                                                  (make-var-ref 'id)
                                                  'md
                                                  (list (make-num-lit 32)
                                                        (make-var-ref 'id)))))
                      '(let id (ref x foo fd)
                         (send id md 32 id))))

     (make-test-case "t2r: num-lit"
       (assert-equal? (texpr->rexpr (make-num-lit 3)) 3))

     (make-test-case "t2r: bool-lit"
       (assert-equal? (texpr->rexpr (make-bool-lit #t)) 'true))

     (make-test-case "t2r: binary prim"
       (assert-equal? (texpr->rexpr (make-binary-prim '+
                                                      (make-num-lit 1)
                                                      (make-var-ref 'x)))
                      '(+ 1 x)))

     (make-test-case "t2r: unary prim"
       (assert-equal? (texpr->rexpr (make-unary-prim 'zero?
                                                     (make-num-lit 34)))
                      '(zero? 34)))

     (make-test-case "t2r: if"
       (assert-equal? (texpr->rexpr (make-if-expr (make-bool-lit #t)
                                                  (make-var-ref 'x)
                                                  (make-send
                                                   (make-var-ref 'x)
                                                   'get-parent
                                                   (list (make-num-lit 3)))))
                      '(if true x (send x get-parent 3))))

     (make-test-case "evaluation: 3!"
       (assert-equal?
        (big-step cj-reductions
                  `(,test-program
                    ,empty-store
                    (send (new numerics) factorial 3)))
        `(,test-program
          ,(store
            [0 (make-instance (make-class-type 'numerics)
                              (list
                               (make-ivar (make-class-type 'numerics) 'i 0)
                               (make-ivar (make-class-type 'numerics)
                                          'b 'false)))])
          6)))

     (make-test-case "evaluation: 6!"
       (assert-equal?
        (big-step cj-reductions
                  `(,test-program
                    ,empty-store
                    (send (new numerics) factorial 6)))
        `(,test-program
          ,(store
            [0 (make-instance (make-class-type 'numerics)
                              (list
                               (make-ivar (make-class-type 'numerics) 'i 0)
                               (make-ivar (make-class-type 'numerics)
                                          'b 'false)))])
          720)))

     (make-test-case "reduction [new]"
       (assert-equal?
        (small-step cj-reductions
                    `(,test-program
                      ,empty-store
                      (new bcons)))
        `(,test-program
          ,(store
            [0 (make-instance (make-class-type 'bcons)
                              (list
                               (make-ivar (make-class-type 'bcons)
                                          'value 'false)
                               (make-ivar (make-class-type 'bcons)
                                          'next 'null)))])
          0)))

     (make-test-case "reduction [get]"
       (let ([test-store
              (store
               [0 (make-instance (make-class-type 'derived)
                                 (list
                                  (make-ivar (make-class-type 'base)
                                             'base-field 3)
                                  (make-ivar (make-class-type 'base)
                                             'shadowed-field 4)
                                  (make-ivar (make-class-type 'derived)
                                             'shadowed-field 'false)
                                  (make-ivar (make-class-type 'derived)
                                             'derived-field 'true)))]
               [1 (make-instance (make-class-type 'base)
                                 (list
                                  (make-ivar (make-class-type 'base)
                                             'base-field 10)
                                  (make-ivar (make-class-type 'base)
                                             'shadowed-field 13)))])])
         (assert-equal?
          (small-step cj-reductions
                      `(,test-program ,test-store
                                      (ref 0 derived shadowed-field)))
          `(,test-program ,test-store false))
         (assert-equal?
          (small-step cj-reductions
                      `(,test-program ,test-store
                                      (ref 0 derived derived-field)))
          `(,test-program ,test-store true))
         (assert-equal?
          (small-step cj-reductions
                      `(,test-program ,test-store
                                      (ref 0 base base-field)))
          `(,test-program ,test-store 3))
         (assert-equal?
          (small-step cj-reductions
                      `(,test-program ,test-store
                                      (ref 0 base shadowed-field)))
          `(,test-program ,test-store 4))))

     (make-test-case "reduction [nget]"
       (assert-equal?
        (small-step cj-reductions
                    `(,test-program ,empty-store (ref null base base-field)))
        `(,test-program ,empty-store "error: dereferenced null")))

     (make-test-case "reduction [set]"
       (let ([test-store
              (store
               [0 (make-instance
                   (make-class-type 'derived)
                   (list
                    (make-ivar (make-class-type 'base) 'base-field 3)
                    (make-ivar (make-class-type 'base) 'shadowed-field 4)
                    (make-ivar (make-class-type 'derived)
                               'shadowed-field 'false)
                    (make-ivar (make-class-type 'derived)
                               'derived-field 'true)))])])
         (assert-equal?
          (small-step cj-reductions
                      `(,test-program
                        ,test-store
                        (set 0 base base-field 5)))
          `(,test-program
            ,(store
              [0 (make-instance
                  (make-class-type 'derived)
                  (list
                   (make-ivar (make-class-type 'base) 'base-field 5)
                   (make-ivar (make-class-type 'base) 'shadowed-field 4)
                   (make-ivar (make-class-type 'derived)
                              'shadowed-field 'false)
                   (make-ivar (make-class-type 'derived)
                              'derived-field 'true)))])
            5))

         (assert-equal?
          (small-step cj-reductions
                      `(,test-program
                        ,test-store
                        (set 0 base shadowed-field 5)))
          `(,test-program
            ,(store
              [0 (make-instance
                  (make-class-type 'derived)
                  (list
                   (make-ivar (make-class-type 'base) 'base-field 3)
                   (make-ivar (make-class-type 'base) 'shadowed-field 5)
                   (make-ivar (make-class-type 'derived)
                              'shadowed-field 'false)
                   (make-ivar (make-class-type 'derived)
                              'derived-field 'true)))])
            5))

         (assert-equal?
          (small-step cj-reductions
                      `(,test-program
                        ,test-store
                        (set 0 derived shadowed-field true)))
          `(,test-program
            ,(store
              [0 (make-instance
                  (make-class-type 'derived)
                  (list
                   (make-ivar (make-class-type 'base) 'base-field 3)
                   (make-ivar (make-class-type 'base) 'shadowed-field 4)
                   (make-ivar (make-class-type 'derived)
                              'shadowed-field 'true)
                   (make-ivar (make-class-type 'derived)
                              'derived-field 'true)))])
            true))

         (assert-equal?
          (small-step cj-reductions
                      `(,test-program
                        ,test-store
                        (set 0 derived derived-field false)))
          `(,test-program
            ,(store
              [0 (make-instance
                  (make-class-type 'derived)
                  (list
                   (make-ivar (make-class-type 'base) 'base-field 3)
                   (make-ivar (make-class-type 'base) 'shadowed-field 4)
                   (make-ivar (make-class-type 'derived)
                              'shadowed-field 'false)
                   (make-ivar (make-class-type 'derived)
                              'derived-field 'false)))])
            false))))

     (make-test-case "reduction [nset]"
       (assert-equal?
        (small-step cj-reductions `(,test-program
                                    ,empty-store
                                    (set null derived derived-field false)))
        `(,test-program ,empty-store "error: dereferenced null")))

     (make-test-case "reduction [uprim]"
       (assert-functional-small-step test-program empty-store
                                     (not true)
                                     false)
       (assert-functional-small-step test-program empty-store
                                     (not false)
                                     true)
       (assert-functional-small-step test-program empty-store
                                     (null? null)
                                     true)
       (assert-functional-small-step test-program empty-store
                                     (null? 3)
                                     false)
       (assert-functional-small-step test-program empty-store
                                     (zero? 1)
                                     false)
       (assert-functional-small-step test-program empty-store
                                     (zero? 0)
                                     true))

     (make-test-case "reduction [bprim]"
       (assert-functional-small-step test-program empty-store
                                     (+ 3 4)
                                     7)
       (assert-functional-small-step test-program empty-store
                                     (- 3 4)
                                     -1)
       (assert-functional-small-step test-program empty-store
                                     (* 3 4)
                                     12)
       (assert-functional-small-step test-program empty-store
                                     (== 3 4)
                                     false)
       (assert-functional-small-step test-program empty-store
                                     (== 4 4)
                                     true))

     (make-test-case "reduction [and-true]"
       (assert-functional-small-step test-program empty-store
                                     (and true (not false))
                                     (not false)))

     (make-test-case "reduction [and-false]"
       (assert-functional-small-step test-program empty-store
                                     (and false (+ 3 4))
                                     false))

     (make-test-case "reduction [or-true]"
       (assert-functional-small-step test-program empty-store
                                     (or true (+ 3 4))
                                     true))

     (make-test-case "reduction [or-false]"
       (assert-functional-small-step test-program empty-store
                                     (or false (not true))
                                     (not true)))

     (make-test-case "reduction [if-true]"
       (assert-functional-small-step test-program empty-store
                                     (if true (+ 3 4) (* 5 6))
                                     (+ 3 4)))

     (make-test-case "reduction [if-false]"
       (assert-functional-small-step test-program empty-store
                                     (if false (+ 3 4) (* 5 6))
                                     (* 5 6)))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     #|
     (make-test-case "reduction: super"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program
                    (send (new derived) f)
                    ,empty-store))
        `(,test-program
          4
          ,(store [0 (make-instance (make-class-type 'derived) 'null null)]))))

     (make-test-case "reduction: downcast"
       (assert-equal?
        (big-step aj-reductions
                  (list test-program
                        '(let s (new statics)
                           (cast bempty (send s get-list)))
                        empty-store))
        (list test-program
              '(addr 1)
              (store [2 (make-instance (make-class-type 'container) 'null
                                       '([l (addr 1)] [node null]))]
                     [1 (make-instance (make-class-type 'bempty) '(addr 2)
                                       null)]
                     [0 (make-instance (make-class-type 'statics) 'null
                                       null)]))))

     (make-test-case "reduction: err-cast"
       (let ([s (store [0 (make-instance (make-class-type 'bcons)
                                         '(addr 1)
                                         `((value true)
                                           (next (addr 2))))]
                       [1 (make-instance (make-class-type 'container)
                                         'null
                                         '((l (addr 0))
                                           (node null)))]
                       [2 (make-instance (make-class-type 'bempty)
                                         '(addr 0) null)])])
         (assert-equal?
          (small-step aj-reductions
                      (list test-program '(cast dag-node (addr 0)) s))
          (list test-program "error: bad cast" s))))

     (make-test-case "reduction: null cast"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(cast numerics null) empty-store))
        (list test-program 'null empty-store)))

     (make-test-case "reduction: let"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(let x 7 (* x 5)) empty-store))
        (list test-program '(* 7 5) empty-store)))

     (make-test-case "reduction: let (bound var not free in body)"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(let y 20
                                          (let x 7
                                            (+ 3 x)))
                          empty-store))
        (list test-program '(let x 7 (+ 3 x)) empty-store)))

     (make-test-case "reduction: null?"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(null? null) empty-store))
        (list test-program 'true empty-store)))

     (make-test-case "reduction: null? (2)"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(null? (addr 34)) empty-store))
        (list test-program 'false empty-store)))

     (make-test-case "reduction: zero?"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(zero? 3) empty-store))
        (list test-program 'false empty-store)))

     (make-test-case "reduction: complement"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(not false) empty-store))
        (list test-program 'true empty-store)))

     (make-test-case "reduction: addition"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(zero? (+ 3 4)) empty-store))
        (list test-program '(zero? 7) empty-store)))

     (make-test-case "reduction: subtraction"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(== 5 (- 10 5)) empty-store))
        (list test-program '(== 5 5) empty-store)))

     (make-test-case "reduction: multiplication"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(* 4 5) empty-store))
        (list test-program 20 empty-store)))

     (make-test-case "reduction: conjunction"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(and true false) empty-store))
        (list test-program 'false empty-store)))

     (make-test-case "reduction: disjunction"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(or false true) empty-store))
        (list test-program 'true empty-store)))

     (make-test-case "reduction: numeric equality"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(== 4 5) empty-store))
        (list test-program 'false empty-store)))

     (make-test-case "reduction: if-true"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(if true (+ 3 4) (- 2 5))
                          empty-store))
        (list test-program '(+ 3 4) empty-store)))

     (make-test-case "reduction: if-false"
       (assert-equal?
        (small-step aj-reductions
                    (list test-program '(if false (+ 3 4) (- 2 5))
                          empty-store))
        (list test-program '(- 2 5) empty-store)))
     |#
     )))
