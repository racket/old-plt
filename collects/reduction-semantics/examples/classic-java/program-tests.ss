;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; program-tests.ss
;; Richard Cobbe
;; $Id: program-tests.ss,v 1.9 2004/04/23 20:03:34 cobbe Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module program-tests mzscheme

  (require (lib "test.ss" "test")
           (lib "etc.ss")
           "parser.ss"
           "ast.ss")

  (require/expose "program.ss" ())

  (define test-program
    (parse-program
     '((class A Object () ([int i] [bool b] [Object o])
         (contain [B a-b])
         (acquire)
         (int method-1 () 3)
         (Object method-2 ([int a] [bool c]) null))
       (class C A () ([B second-b]) (contain) (acquire)
         (int method-1 () 4)
         (int method-3 () 6))
       (class B Object (A) ([int x]) (contain) (acquire [int i] [Object o]))
       (class D B (A) () (contain) (acquire))
       (class E B (A) () (contain) (acquire))
       (class F Object any () (contain) (acquire))
       (+ 3 4))))

  (define test-program-2
    (parse-program
     '((class a object (b) () (contain) (acquire [int x]))
       (class b object (b c) () (contain [a datum] [b next]) (acquire))
       (class c object () ([int x]) (contain [b list]) (acquire))

       (class a2 object (b2) () (contain [b2 next]) (acquire [int x]))
       (class b2 object (a2 c2) () (contain [a2 next]) (acquire))
       (class c2 object () ([int x]) (contain [b2 first]) (acquire))

       (class a3 object (b3 c3) () (contain) (acquire))
       (class b3 object (d3) () (contain) (acquire))
       (class c3 object (d3) () (contain) (acquire))
       (class d3 object () ([int x]) (contain [b3 b] [c3 c]) (acquire))

       (class a4 object (b4) () (contain) (acquire [int x]))
       (class b4 object (c4) () (contain [c4 next]) (acquire))
       (class c4 object (b4) () (contain [b4 next]) (acquire))

       (class a5 object (b5 c5) () (contain) (acquire))
       (class b5 object (d5) () (contain) (acquire))
       (class c5 object (e5) () (contain) (acquire))
       (class d5 object () ([int x]) (contain) (acquire))
       (class e5 object () () (contain) (acquire))

       (class a6 object (b6) () (contain) (acquire [int x]))
       (class b6 object (b6 c6) () (contain [a6 datum] [b6 next]) (acquire))
       (class c6 object () ([int y]) (contain [b list]) (acquire))
       3)))

  (define get-class (lambda (n) (find-class test-program (make-class-type n))))

  (schemeunit-test
   (make-test-suite
       "AJava Program module"

     (make-test-case "find a direct method"
       (assert-equal? (find-method (find-class test-program
                                               (make-class-type 'a))
                                   'method-1)
                      (make-method (make-ground-type 'int)
                                   'method-1
                                   null
                                   null
                                   (make-num-lit 3))))

     (make-test-case "find inherited method"
       (assert-equal? (find-method (find-class test-program
                                               (make-class-type 'c))
                                   'method-2)
                      (make-method (make-class-type 'Object)
                                   'method-2
                                   '(a c)
                                   (list (make-ground-type 'int)
                                         (make-ground-type 'bool))
                                   (make-nil))))

     (make-test-case "find overriding method"
       (assert-equal? (find-method (find-class test-program
                                               (make-class-type 'c))
                                   'method-1)
                      (make-method (make-ground-type 'int)
                                   'method-1
                                   null
                                   null
                                   (make-num-lit 4))))

     (make-test-case "find nonexistent method"
       (assert-false (find-method (find-class test-program
                                              (make-class-type 'b))
                                  'bad-method)))

     (make-test-case "find direct field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'a)) 'i)
                      (make-field (make-ground-type 'int)
                                  'i
                                  'normal)))

     (make-test-case "find direct contained field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'a)) 'a-b)
                      (make-field (make-class-type 'b) 'a-b 'contained)))

     (make-test-case "find inherited normal field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'c)) 'b)
                      (make-field (make-ground-type 'bool) 'b 'normal)))

     (make-test-case "find inherited contained field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'c)) 'a-b)
                      (make-field (make-class-type 'B) 'a-b 'contained)))

     (make-test-case "find direct acquired field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'b)) 'i)
                      (make-field (make-ground-type 'int) 'i 'acquired)))

     (make-test-case "find inherited acquired field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'd)) 'o)
                      (make-field (make-class-type 'Object) 'o 'acquired)))

     (make-test-case "find-field: no such field"
       (assert-false (find-field (find-class test-program (make-class-type 'd))
                                 'bad-field)))

     (make-test-case "find-class: straightforward"
       (let* ([object (make-class (make-class-type 'object) #f null
                                  null null null null)]
              [b (make-class (make-class-type 'b) object
                             (list (make-class-type 'a))
                             (list (make-field (make-ground-type 'int)
                                               'x 'normal))
                             null
                             (list (make-field (make-ground-type 'int)
                                               'i 'acquired)
                                   (make-field (make-class-type 'Object)
                                               'o 'acquired))
                             null)])
         (assert-equal? (find-class test-program
                                    (make-class-type 'b)) b)))

     (make-test-case "find-class: object"
       (assert-equal? (find-class test-program (make-class-type 'object))
                      (make-class (make-class-type 'object) #f
                                  null null null null null)))

     (make-test-case "find-class: does not exist"
       (assert-true
        (with-handlers ([exn:application:mismatch? (lambda _ #t)])
          (begin
            (find-class test-program (make-class-type 'does-not-exist))
            #f))))

     (make-test-case "type-exists: any type"
       (assert-true ((type-exists? test-program) (make-any-type))))

     (make-test-case "type-exists: good ground type"
       (assert-true ((type-exists? test-program)
                     (make-ground-type 'int))))

     (make-test-case "type-exists: good class type"
       (assert-true ((type-exists? test-program)
                     (make-class-type 'c))))

     (make-test-case "type-exists: implicit class type"
       (assert-true ((type-exists? test-program)
                     (make-class-type 'object))))

     (make-test-case "type-exists: bad class type"
       (assert-false ((type-exists? test-program)
                      (make-class-type 'does-not-exist))))

     (make-test-case "int <: int?"
       (assert-true (type<=? test-program (make-ground-type 'int)
                             (make-ground-type 'int))))

     (make-test-case "bool <: int?"
       (assert-false (type<=? test-program (make-ground-type 'bool)
                              (make-ground-type 'int))))

     (make-test-case "any <: int?"
       (assert-false (type<=? test-program (make-any-type)
                              (make-ground-type 'int))))

     (make-test-case "any <: any?"
       (assert-true (type<=? test-program (make-any-type)
                             (make-any-type))))

     (make-test-case "any <: object?"
       (assert-true (type<=? test-program
                             (make-any-type)
                             (make-class-type 'Object))))

     (make-test-case "c <: any?"
       (assert-false (type<=? test-program
                              (make-class-type 'c)
                              (make-any-type))))

     (make-test-case "d <: object?"
       (assert-true (type<=? test-program
                             (make-class-type 'd)
                             (make-class-type 'object))))

     (make-test-case "b <: d?"
       (assert-false (type<=? test-program
                              (make-class-type 'b)
                              (make-class-type 'd))))

     (make-test-case "b <: b?"
       (assert-true (type<=? test-program
                             (make-class-type 'b)
                             (make-class-type 'b))))

     (make-test-case "lub int int"
       (assert-equal? (type-lub test-program (make-ground-type 'int)
                                (make-ground-type 'int))
                      (make-ground-type 'int)))

     (make-test-case "lub int bool"
       (assert-false (type-lub test-program
                               (make-ground-type 'int)
                               (make-ground-type 'bool))))

     (make-test-case "lub any int"
       (assert-false (type-lub test-program
                               (make-any-type)
                               (make-ground-type 'int))))

     (make-test-case "lub any object"
       (assert-equal? (type-lub test-program
                                (make-any-type)
                                (make-class-type 'object))
                      (make-class-type 'object)))

     (make-test-case "lub object any"
       (assert-equal? (type-lub test-program
                                (make-class-type 'object)
                                (make-any-type))
                      (make-class-type 'object)))

     (make-test-case "lub D E"
       (assert-equal? (type-lub test-program
                                (make-class-type 'd)
                                (make-class-type 'e))
                      (make-class-type 'b)))

     (make-test-case "lub E B"
       (assert-equal? (type-lub test-program
                                (make-class-type 'e)
                                (make-class-type 'b))
                      (make-class-type 'b)))

     (make-test-case "lub E A"
       (assert-equal? (type-lub test-program
                                (make-class-type 'e)
                                (make-class-type 'a))
                      (make-class-type 'object)))

     (make-test-case "init-fields: no inheritance, acquired"
       (assert-equal? (init-fields (find-class test-program
                                               (make-class-type 'a)))
                      (list (make-field (make-ground-type 'int) 'i 'normal)
                            (make-field (make-ground-type 'bool) 'b 'normal)
                            (make-field (make-class-type 'object) 'o 'normal)
                            (make-field (make-class-type 'b) 'a-b
                                        'contained))))

     (make-test-case "init-fields: inherited"
       (assert-equal? (init-fields (find-class test-program
                                               (make-class-type 'c)))
                      (list (make-field (make-ground-type 'int) 'i 'normal)
                            (make-field (make-ground-type 'bool) 'b 'normal)
                            (make-field (make-class-type 'object) 'o 'normal)
                            (make-field (make-class-type 'b) 'a-b 'contained)
                            (make-field (make-class-type 'b) 'second-b
                                        'normal))))

     (make-test-case "init-fields: inherited, acquired"
       (assert-equal? (init-fields (find-class test-program
                                               (make-class-type 'd)))
                      (list (make-field (make-ground-type 'int) 'x 'normal))))

     (make-test-case "acquired-fields: none"
       (assert-equal? (acquired-fields (find-class test-program
                                                   (make-class-type 'c)))
                      null))

     (make-test-case "acquired-fields: direct"
       (assert-equal? (acquired-fields (find-class test-program
                                                   (make-class-type 'b)))
                      (list (make-field (make-ground-type 'int)
                                        'i
                                        'acquired)
                            (make-field (make-class-type 'Object)
                                        'o
                                        'acquired))))

     (make-test-case "acquired-fields: inherited"
       (assert-equal? (acquired-fields (find-class test-program
                                                   (make-class-type 'd)))
                      (list (make-field (make-ground-type 'int)
                                        'i 'acquired)
                            (make-field (make-class-type 'Object)
                                        'o 'acquired))))

     (make-test-case "can-be-contained-in?: any container"
       (assert-true (can-be-contained-in?
                     test-program (get-class 'f) (get-class 'b))))

     (make-test-case "can-be-contained-in?: any container 2"
       (assert-true (can-be-contained-in?
                     test-program (get-class 'f) (get-class 'object))))

     (make-test-case "can-be-contained-in?: direct"
       (assert-true (can-be-contained-in?
                     test-program (get-class 'e) (get-class 'a))))

     (make-test-case "can-be-contained-in?: subtyping"
       (assert-true (can-be-contained-in?
                     test-program (get-class 'e) (get-class 'c))))

     (make-test-case "can-be-contained-in?: bogus"
       (assert-false (can-be-contained-in?
                      test-program (get-class 'e) (get-class 'f))))

     (make-test-case "can-contain?: direct"
       (assert-true (can-contain? test-program
                                  (make-class-type 'a) (get-class 'b))))

     (make-test-case "can-contain?: inherited field"
       (assert-true (can-contain? test-program
                                  (make-class-type 'c) (get-class 'b))))

     (make-test-case "can-contain?: subclassed containee"
       (assert-true (can-contain? test-program
                                  (make-class-type 'a) (get-class 'e))))

     (make-test-case "can-contain? double subclassing"
       (assert-true (can-contain? test-program
                                  (make-class-type 'c) (get-class 'd))))

     (make-test-case "can-contain?: bogus"
       (assert-false (can-contain? test-program
                                   (make-class-type 'c)
                                   (get-class 'f))))

     (make-test-case "provides-field?: straightforward"
       (assert-true (provides-field? test-program
                                     (make-class-type 'a)
                                     (make-field (make-ground-type 'int)
                                                 'i
                                                 'acquired))))

     (make-test-case "provides-field?: cycle with good exit"
       (assert-true (provides-field? test-program-2
                                     (make-class-type 'b)
                                     (make-field (make-ground-type 'int)
                                                 'x
                                                 'acquired))))

     (make-test-case "provides-field?: cycle 2 with good exit"
       (assert-true (provides-field? test-program-2
                                     (make-class-type 'b2)
                                     (make-field (make-ground-type 'int)
                                                 'x
                                                 'acquired))))

     (make-test-case "provides-field?: lattice"
       (assert-true (provides-field? test-program-2
                                     (make-class-type 'a3)
                                     (make-field (make-ground-type 'int)
                                                 'x
                                                 'acquired))))

     (make-test-case "provides-field?: cycle 3 with no field"
       (assert-false (provides-field? test-program-2
                                      (make-class-type 'b4)
                                      (make-field (make-ground-type 'int)
                                                  'x
                                                  'acquired))))

     (make-test-case "provides-field?: tree; missing branch"
       (assert-false (provides-field? test-program-2
                                      (make-class-type 'a5)
                                      (make-field (make-ground-type 'int)
                                                  'x
                                                  'acquired))))

     (make-test-case "provides-field: cycle, no such field"
       (assert-false (provides-field? test-program-2
                                      (make-class-type 'b6)
                                      (make-field (make-ground-type 'int)
                                                  'x
                                                  'acquired))))

     (make-test-case "provides-field?: simple no such field"
       (assert-false (provides-field? test-program
                                      (make-class-type 'a)
                                      (make-field (make-ground-type 'int)
                                                  'foo
                                                  'acquired))))

     (make-test-case "provides-field?: bad type"
       (assert-false (provides-field? test-program
                                      (make-class-type 'a)
                                      (make-field (make-class-type 'd)
                                                  'i
                                                  'acquired))))

     (make-test-case "provides-field?: simple field with subtyping"
       (assert-true (provides-field? test-program
                                     (make-class-type 'c)
                                     (make-field (make-class-type 'object)
                                                 'second-b
                                                 'acquired))))
     )))
