#| Data Defs

 Class          = (list Name SuperClass Fields [Comment])
 ;; the name of the class, the name of the supertype ("" if none), and 
 ;; the class's fields

 DataType       = (list TypeName VariantClasses [Comment])
 ;; the name of the type and its variants
 
 VariantClasses = (list VariantClass)
 VariantClass   = (list Name Fields [Comment])

 Name           = String 
 TypeName       = String 
 SuperClass     = String 
 Fields         = (Listof Field)
 Field          = (list String String)
|#

#cs(module datadefs mzscheme 
     
     (require-for-syntax (file "aux.scm"))
     (require (lib "contract.ss"))
     
     (provide 
      java-id?
      class-purpose 
      union-purpose
      Class
      Union 
      Variant 
      )
     
     ;; DataType -> String
     (define (union-purpose dt) (if (null? (cddr dt)) "" (caddr dt)))
     
     ;; Class -> String
     (define (class-purpose c) 
       (if (null? (cdddr c)) "" (cadddr c)))
     
     ;; Any -> Boolean
     ;; the string isn't empty and contains no spaces 
     ;; I should really import this from Kathy's parser or whatever
     ;; so I get qualified names and whatever right
     (define (java-id? s)
       (and (string? s) (not (string=? "" s)) (not (regexp-match "[ |\t|\n]" s))))
     
     (define-syntax (define-as-contract stx)
       (syntax-case stx ()
         [(_ message (name . args) . body)
          (with-syntax ([is-name (prefix-id-suffix "is-" (syntax name) "?")]
                        [ct-name (cap-id (syntax name))])
            (syntax
             (begin 
               (define (is-name . args) . body)
               (define ct-name (flat-named-contract message is-name)))))]))
     
     (define-as-contract "Class representation" (class c)
                         (and (pair? c) (pair? (cdr c)) (pair? (cddr c)) 
                              (or (null? (cdddr c))
                                  (and (pair? (cdddr c))
                                       (null? (cddddr c))
                                       (string? (cadddr c))))
                              ; (list? c) (= (length c) 3)
                              (java-id? (car c))
                              (let ([super (cadr c)])
                                (or (java-id? super) (string=? super "")))
                              (is-fields? (caddr c))))
     
     (define (is-fields? l)
       (and (list? l) (andmap is-field? l)))
     
     (define (is-field? l)
       (and (pair? l) (pair? (cdr l)) (null? (cddr l))
            (java-id? (car l)) (java-id? (cadr l))))
     
     (define-as-contract "Union (datatype) representation" (union l)
                         (and (pair? l) (pair? (cdr l))
                              (or (null? (cddr l))
                                  (and (pair? (cddr l))
                                       (null? (cdddr l))
                                       (string? (caddr l))))
                              (java-id? (car l))
                              (andmap is-variant? (cadr l))))
     
     (define-as-contract "Variant (in a union)" (variant c) 
                         (and (pair? c) (pair? (cdr c)) (null? (cddr c))
                              ; (list? c) (= (length c) 2)
                              (java-id? (car c))
                              (andmap is-fields? (cadr c))))
     
     
     #| Tests: |#
     (require (lib "testing.scm" "testing"))
     
     (test== (java-id? "oops no") #f)
     (test== (java-id? " oops 2") #f)
     (test== (java-id? " oops2 ") #f)
     (test== (java-id? "") #f)
     (test== (java-id? (string #\tab)) #f)
     (test== (java-id? (string #\newline)) #f)
     
     (test== (is-class? '("Foo" "" ())) #t)
     (test== (is-class? '("Foo" "" () "hello world")) #t)
     (test== (is-class? '("Foo" "Moo" (("int" "x") ("int" "y")) "hello world")) #t)
     
     (test== (is-class? '("Foo" "Moo")) #f "no fields")
     (test== (is-class? '("Foo" "Moo Oops" ())) #f "space in super name")
     
     (test== (class-purpose '("a" "b" ())) "")
     (test== (class-purpose '("a" "b" () "hello world")) "hello world")
     
     (test== (is-variant? (list "B" '())) #t "variant class")
     (test== (andmap is-variant?  (list (list "B" '()) (list "C" '()))) #t "variants")
     (test== (java-id? "A") #t)
     (test== (is-union? (list "A" (list (list "B" '()) (list "C" '())))) #t)
     )
