;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; program-tests.ss
;; Richard Cobbe
;; $Id: program-tests.ss,v 1.7 2005/02/02 15:06:47 cobbe Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module program-tests mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1))
           (planet "util.ss" ("schematics" "schemeunit.plt" 1))
           "parser.ss"
           "ast.ss")
  (provide program-tests)

  (require/expose "program.ss" ())

  (define test-program
    (parse-program
     '((class A Object ([int i] [bool b] [Object o] [B a-b])
         (int method-1 () 3)
         (Object method-2 ([int a] [bool c]) null))
       (class C A ([B second-b] [D i])
         (int method-1 () 4)
         (int method-3 () 6))
       (class B Object ([int x] [int i] [Object o]))
       (class D B ())
       (class E B ())
       (class F Object ())
       (class G C ([B b]))
       (+ 3 4))))

  (define program-tests
   (make-test-suite
       "ClassicJava Program module"

     (make-test-case "find a direct method"
       (assert-equal? (find-method (find-class test-program
                                               (make-class-type 'A))
                                   'method-1)
                      (make-method (make-ground-type 'int)
                                   'method-1
                                   null
                                   null
                                   (make-num-lit 3))))

     (make-test-case "find inherited method"
       (assert-equal? (find-method (find-class test-program
                                               (make-class-type 'C))
                                   'method-2)
                      (make-method (make-class-type 'Object)
                                   'method-2
                                   '(a c)
                                   (list (make-ground-type 'int)
                                         (make-ground-type 'bool))
                                   (make-nil))))

     (make-test-case "find overriding method"
       (assert-equal? (find-method (find-class test-program
                                               (make-class-type 'C))
                                   'method-1)
                      (make-method (make-ground-type 'int)
                                   'method-1
                                   null
                                   null
                                   (make-num-lit 4))))

     (make-test-case "find nonexistent method"
       (assert-false (find-method (find-class test-program
                                              (make-class-type 'B))
                                  'bad-method)))

     (make-test-case "find direct field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'A)) 'i)
                      (make-field (make-ground-type 'int)
                                  (make-class-type 'A) 'i)))

     (make-test-case "find inherited field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'C)) 'b)
                      (make-field (make-ground-type 'bool)
                                  (make-class-type 'A) 'b)))

     (make-test-case "find shadowing field direct"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'C)) 'i)
                      (make-field (make-class-type 'D)
                                  (make-class-type 'C) 'i)))

     (make-test-case "find shadowed field direct"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'A)) 'i)
                      (make-field (make-ground-type 'int)
                                  (make-class-type 'A) 'i)))

     (make-test-case "find shadowing field inherited"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'G)) 'i)
                      (make-field (make-class-type 'D)
                                  (make-class-type 'C) 'i)))

     (make-test-case "find shadowed field inherited"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'C)) 'b)
                      (make-field (make-ground-type 'bool)
                                  (make-class-type 'A) 'b)))

     (make-test-case "find-field: no such field"
       (assert-false (find-field (find-class test-program (make-class-type 'D))
                                 'bad-field)))

     (make-test-case "find-all-fields: object"
       (assert-equal? (find-all-fields (find-class test-program
                                                   (make-class-type 'Object)))
                      null))

     (make-test-case "find-all-fields: A"
       (assert-equal? (find-all-fields (find-class test-program
                                                   (make-class-type 'A)))
                      (list (make-field (make-ground-type 'int)
                                        (make-class-type 'A)
                                        'i)
                            (make-field (make-ground-type 'bool)
                                        (make-class-type 'A)
                                        'b)
                            (make-field (make-class-type 'Object)
                                        (make-class-type 'A)
                                        'o)
                            (make-field (make-class-type 'B)
                                        (make-class-type 'A)
                                        'a-b))))

     (make-test-case "find-all-fields: C"
       (assert-equal? (find-all-fields (find-class test-program
                                                   (make-class-type 'C)))
                      (list (make-field (make-class-type 'B)
                                        (make-class-type 'C)
                                        'second-b)
                            (make-field (make-class-type 'D)
                                        (make-class-type 'C)
                                        'i)
                            (make-field (make-ground-type 'int)
                                        (make-class-type 'A)
                                        'i)
                            (make-field (make-ground-type 'bool)
                                        (make-class-type 'A)
                                        'b)
                            (make-field (make-class-type 'Object)
                                        (make-class-type 'A)
                                        'o)
                            (make-field (make-class-type 'B)
                                        (make-class-type 'A)
                                        'a-b))))

     (make-test-case "find-class: straightforward"
       (let* ([object (make-class (make-class-type 'Object) #f null null)]
              [b (make-class (make-class-type 'B) object
                             (list (make-field (make-ground-type 'int)
                                               (make-class-type 'B)
                                               'x)
                                   (make-field (make-ground-type 'int)
                                               (make-class-type 'B) 'i)
                                   (make-field (make-class-type 'Object)
                                               (make-class-type 'B) 'o))
                             null)])
         (assert-equal? (find-class test-program
                                    (make-class-type 'B)) b)))

     (make-test-case "find-class: object"
       (assert-equal? (find-class test-program (make-class-type 'Object))
                      (make-class (make-class-type 'Object) #f null null)))

     (make-test-case "find-class: does not exist"
       (assert-true
        (with-handlers ([exn:fail:contract? (lambda _ #t)])
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
                     (make-class-type 'C))))

     (make-test-case "type-exists: implicit class type"
       (assert-true ((type-exists? test-program)
                     (make-class-type 'Object))))

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
                              (make-class-type 'C)
                              (make-any-type))))

     (make-test-case "d <: object?"
       (assert-true (type<=? test-program
                             (make-class-type 'D)
                             (make-class-type 'Object))))

     (make-test-case "b <: d?"
       (assert-false (type<=? test-program
                              (make-class-type 'B)
                              (make-class-type 'D))))

     (make-test-case "b <: b?"
       (assert-true (type<=? test-program
                             (make-class-type 'B)
                             (make-class-type 'B))))

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
                                (make-class-type 'Object))
                      (make-class-type 'Object)))

     (make-test-case "lub object any"
       (assert-equal? (type-lub test-program
                                (make-class-type 'Object)
                                (make-any-type))
                      (make-class-type 'Object)))

     (make-test-case "lub D E"
       (assert-equal? (type-lub test-program
                                (make-class-type 'D)
                                (make-class-type 'E))
                      (make-class-type 'B)))

     (make-test-case "lub E B"
       (assert-equal? (type-lub test-program
                                (make-class-type 'E)
                                (make-class-type 'B))
                      (make-class-type 'B)))

     (make-test-case "lub E A"
       (assert-equal? (type-lub test-program
                                (make-class-type 'E)
                                (make-class-type 'A))
                      (make-class-type 'Object))))))
