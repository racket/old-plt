;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; program-tests.ss
;; Richard Cobbe
;; $Id: program-tests.ss,v 1.1 2004/07/27 22:41:36 cobbe Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module program-tests mzscheme

  (require (lib "test.ss" "test")
           "parser.ss"
           "ast.ss")

  (require/expose "program.ss" ())

  (define test-program
    (parse-program
     '((class A Object ([int i] [bool b] [Object o] [B a-b])
         (int method-1 () 3)
         (Object method-2 ([int a] [bool c]) null))
       (class C A ([B second-b])
         (int method-1 () 4)
         (int method-3 () 6))
       (class B Object ([int x] [int i] [Object o]))
       (class D B ())
       (class E B ())
       (class F Object ())
       (+ 3 4))))

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
                      (make-field (make-ground-type 'int) 'i)))

     (make-test-case "find direct contained field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'a)) 'a-b)
                      (make-field (make-class-type 'b) 'a-b)))

     (make-test-case "find inherited normal field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'c)) 'b)
                      (make-field (make-ground-type 'bool) 'b)))

     (make-test-case "find inherited contained field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'c)) 'a-b)
                      (make-field (make-class-type 'B) 'a-b)))

     (make-test-case "find direct acquired field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'b)) 'i)
                      (make-field (make-ground-type 'int) 'i)))

     (make-test-case "find inherited acquired field"
       (assert-equal? (find-field (find-class test-program
                                              (make-class-type 'd)) 'o)
                      (make-field (make-class-type 'Object) 'o)))

     (make-test-case "find-field: no such field"
       (assert-false (find-field (find-class test-program (make-class-type 'd))
                                 'bad-field)))

     (make-test-case "find-class: straightforward"
       (let* ([object (make-class (make-class-type 'object) #f null null)]
              [b (make-class (make-class-type 'b) object
                             (list (make-field (make-ground-type 'int) 'x)
                                   (make-field (make-ground-type 'int) 'i)
                                   (make-field (make-class-type 'Object) 'o))
                             null)])
         (assert-equal? (find-class test-program
                                    (make-class-type 'b)) b)))

     (make-test-case "find-class: object"
       (assert-equal? (find-class test-program (make-class-type 'object))
                      (make-class (make-class-type 'object) #f null null)))

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
                      (make-class-type 'object))))))
