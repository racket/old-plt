;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; reduction-tests.ss
;; Richard Cobbe
;; $Id: reduction-tests.ss,v 1.14 2004/06/03 19:34:39 cobbe Exp $
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

  (require/expose "reduction.ss" (aj-subst aj-syntax
                                           texpr->rexpr
                                           make-instance
                                           get-acq-field
                                           aj-reductions))

  (define test-program-src
    '((class numerics object ()
        ([int i]
         [bool b])
        (contain) (acquire)
        (int factorial ([int n])
             (if (zero? n) 1 (* n (send this factorial (- n 1))))))
      (class container object ()
        ()
        (contain [blist l]
                 [dag-node node])
        (acquire))
      (class base object ()
        () (contain) (acquire)
        (int f () 3))
      (class derived base ()
        () (contain) (acquire)
        (int f () (+ (super f) 1)))
      (class blist object (container bcons)
        () (contain) (acquire)
        (int length () -1)
        (bool andmap () false))
      (class bempty blist (container bcons)
        () (contain) (acquire)
        (int length () 0)
        (bool andmap () true))
      (class bcons blist (container bcons)
        ([bool value])
        (contain [blist next])
        (acquire)
        (int length () (+ 1 (send (ivar this next) length)))
        (bool andmap () (if (ivar this value)
                            (send (ivar this next) andmap)
                            false)))
      (class dag-node object (container dag-node)
        ()
        (contain [dag-node left]
                 [dag-node right])
        (acquire))
      (class statics object ()
        ()
        (contain)
        (acquire)
        (blist get-list ()
               (let c (new container (new bempty) null)
                 (ivar c l))))
      (class acq-container object ()
        ([int field])
        (contain [acq-contained c])
        (acquire))
      (class acq-contained object (acq-container)
        ()
        (contain)
        (acquire [int field]))
      null))

  (define test-program (elab-program (parse-program test-program-src)))

  (schemeunit-test
   (make-test-suite "AJava Reduction Tests"

     (make-test-case "constant/variable substitution"
       (assert-equal? (aj-subst 'x 3 '(send y x x null true false 4 this))
                      '(send y x 3 null true false 4 this)))

     (make-test-case "substitution: ctor"
       (assert-equal? (aj-subst 'c 'null '(new c c (ivar c x)))
                      '(new c null (ivar null x))))

     (make-test-case "substitution: ivar"
       (assert-equal? (aj-subst 'x 3 '(ivar (send x fd) x))
                      '(ivar (send 3 fd) x)))

     (make-test-case "substitution: send"
       (assert-equal? (foldl aj-subst '(send a b c (ivar d y) e null 3)
                             '(a b c d e)
                             '(6 7 8 9 10))
                      '(send 6 b 8 (ivar 9 y) 10 null 3)))

     (make-test-case "substitution: super"
       (assert-equal? (foldl aj-subst
                             '(super a b c d (ivar e field) f null 3)
                             '(a b c d e f)
                             '(11 12 13 14 15 16))
                      '(super 11 b c 14 (ivar 15 field) 16 null 3)))

     (make-test-case "substitution: cast"
       (assert-equal? (aj-subst 'x 42 '(cast x (ivar x fd)))
                      '(cast x (ivar 42 fd))))

     (make-test-case "substitution: let (not bound var)"
       (assert-equal? (aj-subst 'x 42 '(let y (ivar x z)
                                         (new a x y z)))
                      '(let y (ivar 42 z) (new a 42 y z))))

     (make-test-case "substitution: let bound var"
       (assert-equal? (aj-subst 'x 42 '(let x (ivar x foo)
                                         (new a x y z)))
                      '(let x (ivar 42 foo)
                         (new a x y z))))

     (make-test-case "substitution: binary prim"
       (assert-equal? (aj-subst 'x 42 '(+ (ivar x size) (ivar y size)))
                      '(+ (ivar 42 size) (ivar y size))))

     (make-test-case "substitution: unary prim"
       (assert-equal? (aj-subst 'x 42 '(null? (ivar x parent)))
                      '(null? (ivar 42 parent))))

     (make-test-case "substitution: if"
       (assert-equal? (aj-subst 'x 42 '(if (ivar x flag)
                                           (ivar x this)
                                           (ivar x that)))
                      '(if (ivar 42 flag)
                           (ivar 42 this)
                           (ivar 42 that))))

     (make-test-case "t2r: ctor"
       (assert-equal? (texpr->rexpr
                       (make-new (make-class-type 'foo)
                                 (list (make-var-ref 'bar)
                                       (make-ivar (make-var-ref 'baz)
                                                  'quux)
                                       (make-send (make-var-ref 'x)
                                                  'y
                                                  null))))
                      '(new foo bar (ivar baz quux) (send x y))))

     (make-test-case "t2r: var-ref"
       (assert-equal? (texpr->rexpr (make-var-ref 'var)) 'var))

     (make-test-case "t2r: null"
       (assert-equal? (texpr->rexpr (make-nil)) 'null))

     (make-test-case "t2r: ivar"
       (assert-equal? (texpr->rexpr (make-ivar (make-send (make-var-ref 'x)
                                                          'y
                                                          (list (make-nil)))
                                               'field))
                      '(ivar (send x y null) field)))

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
                                               (make-ivar (make-var-ref 'x)
                                                          'fd)))
                      '(cast c (ivar x fd))))

     (make-test-case "t2r: let"
       (assert-equal? (texpr->rexpr (make-aj-let 'id
                                                 (make-ivar (make-var-ref 'x)
                                                            'fd)
                                                 (make-send
                                                  (make-var-ref 'id)
                                                  'md
                                                  (list (make-num-lit 32)
                                                        (make-var-ref 'id)))))
                      '(let id (ivar x fd)
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

     (make-test-case "get-acq-field: null parent"
       (assert-false (get-acq-field 'fd empty-store
                                    (make-instance (make-class-type 'foo)
                                                   'null
                                                   null))))

     (make-test-case "get-acq-field: valid chain"
       (assert-equal?
        (get-acq-field 'fd
                       (store [0 (make-instance (make-class-type 'a)
                                                '(addr 2)
                                                '((x false)))]
                              [1 (make-instance (make-class-type 'b)
                                                'null
                                                '((fd 3)))]
                              [2 (make-instance (make-class-type 'c)
                                                '(addr 1)
                                                '((a 4)
                                                  (b 5)))])
                       (make-instance (make-class-type 'd)
                                      '(addr 0)
                                      null))
        3))

     (make-test-case "get-acq-field: not found but long chain"
       (assert-false
        (get-acq-field 'fd2
                       (store [0 (make-instance (make-class-type 'a)
                                                '(addr 2)
                                                '((x false)))]
                              [1 (make-instance (make-class-type 'b)
                                                'null
                                                '((fd 3)))]
                              [2 (make-instance (make-class-type 'c)
                                                '(addr 1)
                                                '((a 4)
                                                  (b 5)))])
                       (make-instance (make-class-type 'd)
                                      '(addr 0)
                                      null))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (make-test-case "evaluation: 3!"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program
                    (send (new numerics 3 false) factorial 3)
                    ,empty-store))
        `(,test-program
          6
          ,(store [0 (make-instance (make-class-type 'numerics)
                                    'null
                                    '([i 3] [b false]))]))))

     (make-test-case "evaluation: 6!"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program
                    (send (new numerics 3 false) factorial 6)
                    ,empty-store))
        `(,test-program
          720
          ,(store [0 (make-instance (make-class-type 'numerics)
                                    'null
                                    '([i 3] [b false]))]))))

     (make-test-case "reduction: new"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program
                    (new container
                         (new bcons true (new bcons false (new bempty)))
                         null)
                    ,empty-store))
        `(,test-program
          (addr 3)
          ,(store [3 (make-instance (make-class-type 'container)
                                    'null
                                    '([l (addr 2)] [node null]))]
                  [2 (make-instance (make-class-type 'bcons)
                                    '(addr 3)
                                    '([value true] [next (addr 1)]))]
                  [1 (make-instance (make-class-type 'bcons)
                                    '(addr 2)
                                    '([value false] [next (addr 0)]))]
                  [0 (make-instance (make-class-type 'bempty)
                                    '(addr 1) null)]))))

     (make-test-case "reduction: err-new"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program
                    (let ctr (new container (new bcons true (new bempty)) null)
                      (let l (ivar ctr l)
                        (new bcons false l)))
                    ,empty-store))
        `(,test-program
          "error: container violation"
          ,(store [2 (make-instance (make-class-type 'container)
                                    'null
                                    '([l (addr 1)] [node null]))]
                  [1 (make-instance (make-class-type 'bcons)
                                    '(addr 2)
                                    '([value true] [next (addr 0)]))]
                  [0 (make-instance (make-class-type 'bempty)
                                    '(addr 1) null)]))))

     (make-test-case "reduction: ivar"
       (let ([s (store [0 (make-instance (make-class-type 'numerics)
                                         'null
                                         '([i 3] [b false]))])])
         (assert-equal?
          (small-step aj-reductions `(,test-program (ivar (addr 0) i) ,s))
          `(,test-program 3 ,s))))

     (make-test-case "reduction: ivar-acq"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program
                    (let ctr (new acq-container 3 (new acq-contained))
                      (let ctd (ivar ctr c)
                        (ivar ctd field)))
                    ,empty-store))
        `(,test-program
          3
          ,(store [1 (make-instance (make-class-type 'acq-container)
                                    'null
                                    '([field 3] [c (addr 0)]))]
                  [0 (make-instance (make-class-type 'acq-contained)
                                    '(addr 1)
                                    null)]))))

     (make-test-case "reduction: ivar-null"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program
                     (ivar (cast bcons null) next)
                     ,empty-store))
        `(,test-program "error: dereferenced null" ,empty-store)))

     (make-test-case "reduction: send"
       (assert-equal?
        (big-step aj-reductions
                  `(,test-program (send (new base) f) ,empty-store))
        `(,test-program
          3
          ,(store [0 (make-instance (make-class-type 'base) 'null null)]))))

     (make-test-case "reduction: send-null"
       (assert-equal?
        (big-step aj-reductions
                    `(,test-program
                      (let l (cast blist null)
                        (send l length))
                      ,empty-store))
        `(,test-program "error: dereferenced null" ,empty-store)))

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
     )))
