;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; parser-tests.ss
;; Richard Cobbe
;; $Id: parser-tests.ss,v 1.1 2004/07/27 22:41:36 cobbe Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module parser-tests mzscheme

  (require (lib "test.ss" "test")
           (lib "etc.ss")
           "ast.ss")
  (require/expose "parser.ss" (make-temp-class
                               parse-expr
                               parse-defn
                               parse-init-program
                               make-final-classes))

  (define-assertion (assert-parse-exn msg val expr)
    (with-handlers ([exn:aj:parse?
                     (lambda (exn)
                       (and (string=? msg (exn-message exn))
                            (equal? val (exn:application-value exn))))]
                    [(lambda _ #t) (lambda _ #f)])
      (begin expr #f)))

  (schemeunit-test
   (make-test-suite "Tests for Acquired Java parser."

     (make-test-case "initial program"
       (mv-assert equal?
                  (parse-init-program
                   '((class a Object ([int foo]))
                     (class b a ([bool bar]))
                     (class c a ([Object baz]))
                     (* 3 (+ 4 5))))
                  (hash-table
                   ('Object
                    (make-temp-class 'Object #f null null))
                   ('a (make-temp-class 'a 'Object
                                        (list (make-field
                                               (make-ground-type 'int)
                                               'foo))
                                        null))
                   ('b (make-temp-class 'b 'a
                                        (list
                                         (make-field (make-ground-type 'bool)
                                                     'bar))
                                        null))
                   ('c (make-temp-class 'c 'a
                                        (list
                                         (make-field (make-class-type 'Object)
                                                     'baz))
                                        null)))
                  (make-binary-prim '* (make-num-lit 3)
                                    (make-binary-prim '+
                                                      (make-num-lit 4)
                                                      (make-num-lit 5)))))

     (make-test-case "bad initial program"
       (assert-parse-exn "bad program" 'Object
                         (parse-init-program 'Object)))

     (make-test-case "bad initial program 2"
       (assert-parse-exn "bad program" null (parse-init-program null)))

     (make-test-case "parse-init-program: no defns"
       (mv-assert equal?
                  (parse-init-program '(true))
                  (hash-table ('Object (make-temp-class 'Object #f null null)))
                  (make-bool-lit #t)))

     (make-test-case "init-program: duplicate class name"
       (assert-parse-exn
        "duplicate class definition"
        (make-temp-class 'a 'Object
                         (list (make-field (make-ground-type 'int) 'foo)) null)
        (parse-init-program
         '((class a Object ([int first-defn-of-a]))
           (class a Object ([int foo]))
           (+ 3 4)))))

     (make-test-case "make-final-classes: Object only"
       (assert-equal?
        (make-final-classes
         (hash-table ('Object (make-temp-class 'Object #f null null))))
        (hash-table ('Object (make-class (make-class-type 'Object)
                                         #f null null)))))

     (make-test-case "make-final-classes: small hierarchy"
       (let* ([object-class (make-class
                             (make-class-type 'Object)
                             #f null null)]
              [a-class (make-class (make-class-type 'a) object-class
                                   null null)]
              [b-class (make-class (make-class-type 'b) a-class
                                   null null)])
         (assert-equal?
          (make-final-classes
           (hash-table ('Object (make-temp-class 'Object #f null null))
                       ('b (make-temp-class 'b 'a null null))
                       ('a (make-temp-class 'a 'Object null null))))
          (hash-table ('Object object-class)
                      ('a a-class)
                      ('b b-class)))))

     (make-test-case "make-final-classes: missing superclass"
       (let ([a (make-temp-class 'a 'b null null)])
         (assert-parse-exn "parent class doesn't exist" a
                           (make-final-classes
                            (hash-table
                             ('Object (make-temp-class 'Object #f null null))
                             ('a a))))))

     (make-test-case "make-final-classes: inheritance cycle"
       (let ([a (make-temp-class 'a 'b null null)]
             [b (make-temp-class 'b 'c null null)]
             [c (make-temp-class 'c 'a null null)])
         (assert-parse-exn "inheritance cycle" a
                           (make-final-classes
                            (hash-table ['a a]
                                        ['b b]
                                        ['c c])))))

     (make-test-case "Numeric expression"
       (assert-equal? (parse-expr '(* (+ 3 4) (- 7 -2)))
                      (make-binary-prim '*
                                        (make-binary-prim '+
                                                          (make-num-lit 3)
                                                          (make-num-lit 4))
                                        (make-binary-prim '-
                                                          (make-num-lit 7)
                                                          (make-num-lit -2)))))

     (make-test-case "Boolean expression"
       (assert-equal? (parse-expr '(not (or (or true (zero? 4))
                                            (or false (and (null? null)
                                                           (== 3 6))))))
                      (make-unary-prim 'not
                                       (make-binary-prim
                                        'or
                                        (make-binary-prim
                                         'or
                                         (make-bool-lit #t)
                                         (make-unary-prim 'zero?
                                                          (make-num-lit 4)))
                                        (make-binary-prim
                                         'or
                                         (make-bool-lit #f)
                                         (make-binary-prim
                                          'and
                                          (make-unary-prim 'null?
                                                           (make-nil))
                                          (make-binary-prim
                                           '==
                                           (make-num-lit 3)
                                           (make-num-lit 6))))))))

     (make-test-case "primitive arity problem"
       (assert-parse-exn "bad expression" '(+ 3 4 5)
                         (parse-expr '(+ 3 4 5))))

     (make-test-case "unary primitive: arity problem"
       (assert-parse-exn "bad expression" '(null? x y)
                         (parse-expr '(null? x y))))

     (make-test-case "primitives aren't nullary"
       (assert-parse-exn "bad expression" '(null?)
                         (parse-expr '(null?))))

     (make-test-case "primitives aren't variables"
       (assert-parse-exn "bad expression" 'null?
                         (parse-expr 'null?)))

     (make-test-case "If expression"
       (assert-equal? (parse-expr '(if x y z))
                      (make-if-expr (make-var-ref 'x)
                                    (make-var-ref 'y)
                                    (make-var-ref 'z))))

     (make-test-case "one-armed if"
       (assert-parse-exn "bad expression" '(if x y)
                         (parse-expr '(if x y))))

     (make-test-case "classes not expressions"
       (let ([defn '(class foo Object () () () ())])
         (assert-parse-exn "bad expression"
                           defn
                           (parse-expr `(if ,defn x y)))))

     (make-test-case "ctor call"
       (assert-equal? (parse-expr '(new foo))
                      (make-new (make-class-type 'foo))))

     (make-test-case "ctor: first arg must be class name"
       (assert-parse-exn "bad expression" '(new (x y) z)
                         (parse-expr '(new (x y) z))))

     (make-test-case "ref"
       (assert-equal? (parse-expr '(ref this x))
                      (make-ref (make-var-ref 'this) 'x)))

     (make-test-case "ref: field must be name"
       (assert-parse-exn "bad expression" '(ref this (x y))
                         (parse-expr '(ref this (x y)))))

     (make-test-case "set"
       (assert-equal? (parse-expr '(set this x (+ y z)))
                      (make-set (make-var-ref 'this)
                                'x
                                (make-binary-prim '+
                                                  (make-var-ref 'y)
                                                  (make-var-ref 'z)))))

     (make-test-case "set: field must be name"
       (let ([set '(set this (+ a b) (- c d))])
         (assert-parse-exn "bad expression" set
                           (parse-expr set))))

     (make-test-case "call"
       (assert-equal? (parse-expr '(call (ref x y) foo (ref z a) null))
                      (make-call (make-ref (make-var-ref 'x) 'y)
                                 'foo
                                 (list (make-ref (make-var-ref 'z)
                                                  'a)
                                       (make-nil)))))

     (make-test-case "call: method name must be name"
       (let ([call '(call (ref x y) (z q))])
         (assert-parse-exn "bad expression" call
                           (parse-expr call))))

     (make-test-case "super"
       (assert-equal? (parse-expr '(super foo x (ref a b)))
                      (make-super 'foo
                                  (list (make-var-ref 'x)
                                        (make-ref (make-var-ref 'a)
                                                   'b)))))

     (make-test-case "super: method name must be literal"
       (assert-parse-exn "bad expression" '(super (x y))
                         (parse-expr '(super (x y)))))

     (make-test-case "cast"
       (assert-equal? (parse-expr '(cast foo (ref x y)))
                      (make-cast (make-class-type 'foo)
                                 (make-ref (make-var-ref 'x) 'y))))

     (make-test-case "cast: class name must be literal"
       (assert-parse-exn "bad expression" '(cast (x y) z)
                         (parse-expr '(cast (x y) z))))

     (make-test-case "cast: bad arity"
       (assert-parse-exn "bad expression" '(cast x)
                         (parse-expr '(cast x))))

     (make-test-case "definition"
       (assert-equal?
        (parse-defn '(class foo Object
                       ((int x)
                        (Object y)
                        (bar a)
                        (baz b)
                        (quux c))
                       (foo get-foo ((int x)) null)))
        (make-temp-class
         'foo
         'Object
         (list (make-field (make-ground-type 'int) 'x)
               (make-field (make-class-type 'Object) 'y)
               (make-field (make-class-type 'bar) 'a)
               (make-field (make-class-type 'baz) 'b)
               (make-field (make-class-type 'quux) 'c))
         (list (make-method (make-class-type 'foo) 'get-foo
                            (list 'x)
                            (list (make-ground-type 'int))
                            (make-nil))))))

     (make-test-case "defn: malformed"
       (assert-parse-exn "bad definition" '(class foo (x y) () ())
                         (parse-defn '(class foo (x y) () ()))))

     (make-test-case "defn: bad field"
       (let ([bad-field '(x y z)])
         (assert-parse-exn "bad field definition" bad-field
                           (parse-defn `(class foo Object
                                          ((a b) ,bad-field))))))

     (make-test-case "defn: bad method"
       (let ([bad-method '(int foo ())])
         (assert-parse-exn "bad method definition" bad-method
                           (parse-defn `(class bar Object () ,bad-method)))))

     (make-test-case "defn: bad argument"
       (assert-parse-exn "bad argument definition" '(int x y)
                         (parse-defn '(class bar Object ()
                                        (int foo ((int x y) (foo bar))
                                             null)))))

     (make-test-case "program"
       (assert-equal?
        (parse-program
         '((class foo Object ())
           (class bar foo ((int x) [foo f])
             (int zero () 0)
             (Object get-root () null)
             (int sum ((int x) (int y)) (+ x y)))
           (call (new bar) zero)))
        (let* ([obj (make-class (make-class-type 'Object) #f null null)]
               [foo (make-class (make-class-type 'foo) obj null null)]
               [bar (make-class
                     (make-class-type 'bar) foo
                     (list (make-field (make-ground-type 'int) 'x)
                           (make-field (make-class-type 'foo) 'f))
                     (list (make-method (make-ground-type 'int)
                                        'zero
                                        null
                                        null
                                        (make-num-lit 0))
                           (make-method (make-class-type 'Object)
                                        'get-root
                                        null
                                        null
                                        (make-nil))
                           (make-method (make-ground-type 'int)
                                        'sum
                                        '(x y)
                                        (list (make-ground-type 'int)
                                              (make-ground-type 'int))
                                        (make-binary-prim
                                         '+
                                         (make-var-ref 'x)
                                         (make-var-ref 'y)))))])
          (make-program
           (hash-table ('Object obj) ('foo foo) ('bar bar))
           (make-call (make-new (make-class-type 'bar))
                      'zero
                      null))
          ))))))
