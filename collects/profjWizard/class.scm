#cs(module class mzscheme 
     (require (lib "etc.ss") 
              (lib "list.ss")
              (lib "contract.ss")
              (file "data-defs.scm"))
     
     ;; ------------------------------------------------------------------------
     (provide/contract
      [make-class ((Class) (boolean? boolean?) . opt-> . string?)] 
      ) 
     
     #| Usage: 
          (make-class a-class toString? template?) 
        create a Java class from the class specification (a-class).

        The first optional boolean parameter specifies whether we want toString 
        in the class hiearchy. Default: true

        The second optional boolean parameter specifies whether we want 
        a draft method template in comments. Default: true
     |#
     
     (provide/contract
      [make-union ((Union) (boolean? boolean?) . opt-> . string?)]
      )
     
     #| Usage: 
         (make-union a-union toString? template?)
        create a Java implementation of a datatype specification using an 
        abstract class and N variants.

        The first optional boolean parameter specifies whether we want toString 
        in the class hiearchy. Default: true

        The second optional boolean parameter specifies whether we want 
        a draft method template in comments. Default: true
     |#
     
     ;; ------------------------------------------------------------------------
     ;; Construct a union as a collection of Java classes 
     
     ;; String (Listof VariantClass) -> String 
     (define make-union
       (opt-lambda (the-union [toString? #t][template? #t])
         (let ([type (car the-union)][subt (cadr the-union)])
           (string-append
            (car (purpose-statement (union-purpose the-union)))
            (abstractClass type template?)
            "\n"
            (commas
             (map (lambda (sc)
                    (make-class
                     (list (car sc) type (cadr sc)) toString? template?))
                  subt)
             "\n")))))
     
     ;; String -> String 
     (define (abstractClass type template?)
       (apply string-append 
              `(,(format abstractF type)
                 ,@(if (not template?)
                       (list "")
                       `( ,cmnt/*
                           ,warnin
                           ,purpos                            
                           ,absteg
                           ,cmnt*/))
                 ,endclf)))
     
     ;; ------------------------------------------------------------------------
     ;; Construct individual Java classes 
     
     ;; create class definition (as string) from name, super and list of fields
     (define make-class 
       (opt-lambda (the-class [toString? #t][template? #t])
         (let ([type   (car the-class)]
               [super  (cadr the-class)]
               [fields (caddr the-class)]
               [prps   (class-purpose the-class)])
           (apply string-append
                  `(  ,@(purpose-statement prps)
                      ,(format classf type (extends super))
                      ;; fields
                      ,@(map (lambda (f) (format declaf (car f) (cadr f))) fields)
                      "\n"
                      ;; constructor
                      ,(format constf type (parameters fields))
                      ,@(map (lambda (f) (format initif (cadr f) (cadr f))) fields)
                      ,endMet
                      ;; optional template draft:
                      ,@(make-template super fields template?)
                      ;; optional toString method:
                      ,@(toString type fields toString?)
                      ,endclf)))))
     
     ;; String -> String
     (define (extends super) (if (string=? "" super) "" (format extendsf super)))
     
     ;; Fields -> String 
     ;; create a paremeter list from a field specifications
     (define (parameters fs)
       (commas  (map (lambda (f) (format paraf (car f) (cadr f))) fs)))
     
     ;; String Fields Boolean -> (listof String)
     (define (make-template super fields template?)
       (if (not template?)
           (list "")
           `("\n"
             ;; template method
             ,cmnt/*
             ,warnin
             ,contef
             ,@(map (lambda (f) (format (if (string=? (car f) super)
                                            temprf
                                            tempsf)
                                        (cadr f)))
                    fields)
             ,endMet
             ,cmnt*/)))
     
     ;; String Fields -> (cons String (listof String))
     ;; create a toString method for class type with _fields_
     (define toString 
       (opt-lambda (type fields [toString? #t])
         (if (not toString?)
             (list "")
             (list "\n"
                   (string-append 
                    toStrf     
                    (format prefix type)
                    ; (apply string-append)
                    (if (null? fields)
                        " + "
                        (format
                         " + ~a + " (commas (map addToString fields) infix)))
                    postfix
                    endMet)))))
     
     (provide toString)
     
     ;; Field -> String
     ;; create a call to toString, if necessary 
     (define (addToString f)
       (let ([t (car f)] [s (cadr f)])
         (if (member t StringNative) s (format "~a.toString()" s))))
     
     
     ;; String -> (list String)
     (define (purpose-statement prps)
       (if (string=? "" prps) '("") (list (format "// ~a~n" prps))))

     ;; identifiers ending in f are format strings, and require ~n for newline
     
     ;; Abstract Class 
     (define abstractF "abstract class ~a {\n")
     ;; Abstract Template 
     (define purpos "  // purpose statement \n")
     (define absteg "  abstract ??? mmm();\n")       
     ;; Class 
     (define classf "class ~a ~a{~n") (define extendsf "extends ~a ")
     ;; Fields
     (define declaf "  ~a ~a;~n")
     ;; Constructor
     (define constf "  ~a(~a) {~n") (define paraf "~a ~a")
     (define initif "    this.~a = ~a;~n")
     (define endMet "  }\n")
     (define endclf "}\n")
     ;; Concrete Template
     (define warnin "  // ** DRAFT TEMPLATE ** Edit as needed.\n")
     (define contef "  ??? mmm() {\n")
     (define tempsf "    ... this.~a ...\n")
     (define temprf "    ... this.~a.mmm() ...\n")
     ;; toString
     (define toStrf "  public String toString() {\n")
     (define prefix "    return \"new ~a(\"")
     (define infix " + \",\" + ")
     (define postfix "\")\";\n")
     (define StringNative '("int" "boolean" "String" "double")) ;; and others
     ;; Comments
     (define cmnt/* "/*\n")
     (define cmnt*/ "*/\n")
     
     ;; ------------------------------------------------------------------------
     ;; Library 
     
     ;; (Listof String) -> String
     ;; create a comma-separated string from a list of strings 
     (define (commas l . comma)
       (let ([comma (if (null? comma) ", " (car comma))])
         (if (null? l) ""
             (apply string-append (car l)
                    (map (lambda (x) (string-append comma x)) (cdr l))))))
     
     ;; ------------------------------------------------------------------------
     ;; TESTS: 
     
     (define empty-template 
       `("/*\n" 
         ,warnin
         "  ??? mmm() {\n"
         "  }\n"
         "*/\n"
         "\n"))
     
     #| Tests : 
     (require (lib "testing.scm" "testing"))
     
     (test== (commas '()) "")
     (test== (commas '("x" "y") ) "x, y")
     (test== (commas '("x" "y") " + ") "x + y")
     
     (test== (parameters '()) "")
     (test== (parameters '(("int" "x"))) "int x")
     (test== (parameters '(("int" "x") ("foo" "y"))) "int x, foo y")
     (test== (commas '("x" "y") " + ") "x + y")
     (test== (commas '("x" "y" "z") " + ") "x + y + z")
     
     (test== (cadr (toString "foo" '()))
             (string-append
              toStrf
              "    return \"new foo(\" + \")\";\n"
              endMet))
     
     (test== (cadr (toString "Foo" '(("Foo" "x") ("Moo" "y"))))
             (string-append
              toStrf
              "    return \"new Foo(\" + x.toString() + \",\" + y.toString() + \")\";\n"
              endMet))
     
     (test== (cadr (toString "Foo" '(("int" "x"))))
             (string-append
              toStrf
              "    return \"new Foo(\" + x + \")\";\n"
              endMet))
     
     (test== (cadr (toString "Foo" '(("Boolean" "x"))))
             (string-append
              toStrf
              "    return \"new Foo(\" + x.toString() + \")\";\n"
              endMet))
     
     (test== (cadr (toString "Foo" '(("Foo" "x") ("int" "y") ("Z" "z"))))
             (string-append
              toStrf
              "    return \"new Foo(\" + x.toString() + \",\" + y + \",\" + z.toString() + \")\";\n"
              endMet))
     
     (test== (toString "Foo" '(("Foo" "x") ("int" "y") ("Z" "z")) #f)
             (list ""))
     
     (test== (make-class (list "foo" "" '()))
             (apply string-append 
                    `( "class foo {\n"
                       "\n"
                       "  foo() {\n"
                       "  }\n"
                       "\n"
                       ,@empty-template
                       "  public String toString() {\n"
                       "    return \"new foo(\" + \")\";\n"
                       "  }\n"
                       "}\n")))
     
     (test== (make-class (list "moo" "foo" '()))
             (apply string-append 
                    `("class moo extends foo {\n"
                      "\n"
                      "  moo() {\n"
                      "  }\n"
                      "\n"
                      ,@empty-template
                      "  public String toString() {\n"
                      "    return \"new moo(\" + \")\";\n"
                      "  }\n"
                      "}\n")))
     
     (test== (make-class (list "moo" "foo" '(("int" "x") ("foo" "f"))))
             (apply string-append
                    `("class moo extends foo {\n"
                      "  int x;\n"
                      "  foo f;\n"
                      "\n"
                      "  moo(int x, foo f) {\n"
                      "    this.x = x;\n"
                      "    this.f = f;\n"
                      "  }\n"
                      "\n"
                      ,cmnt/*
                      ,warnin
                      "  ??? mmm() {\n"
                      "    ... this.x ...\n"
                      "    ... this.f.mmm() ...\n"
                      "  }\n"
                      ,cmnt*/
                      "\n"
                      "  public String toString() {\n"
                      "    return \"new moo(\" + x + \",\" + f.toString() + \")\";\n"
                      "  }\n"
                      "}\n")))
     
     (test== (make-class (list "CartPt" "" '(("int" "x") ("int" "y"))))
             (apply string-append
                    `("class CartPt {\n"
                      "  int x;\n"
                      "  int y;\n"
                      "\n"
                      "  CartPt(int x, int y) {\n"
                      "    this.x = x;\n"
                      "    this.y = y;\n"
                      "  }\n"
                      "\n"
                      ,cmnt/*
                      ,warnin
                      "  ??? mmm() {\n"
                      "    ... this.x ...\n"
                      "    ... this.y ...\n"
                      "  }\n"
                      ,cmnt*/
                      "\n"
                      "  public String toString() {\n"
                      "    return \"new CartPt(\" + x + \",\" + y + \")\";\n"
                      "  }\n"
                      "}\n")))
     
     (test== (abstractClass "Foo" #f)
             (string-append 
              "abstract class Foo {\n"
              "}\n"))
     
     (test== (abstractClass "Foo" #t)
             (apply string-append 
                    `("abstract class Foo {\n"
                      ,cmnt/*                        
                      ,warnin
                      ,purpos
                      "  abstract ??? mmm();\n"
                      ,cmnt*/
                      "}\n")))
     
     (test== 
      (make-union
       (list "AList" '(("MT" ()) ("Cons" (("int" "first") ("AList" "rest"))))))
      (apply string-append 
             `(
               "abstract class AList {\n"
               "/*\n"
               ,warnin
               "  // purpose statement \n"
               "  abstract ??? mmm();\n"
               "*/\n"
               "}\n"
               "\n"
               "class MT extends AList {\n"
               "\n"
               "  MT() {\n"
               "  }\n"
               "\n"
               "/*\n"
               ,warnin
               "  ??? mmm() {\n"
               "  }\n"
               "*/\n"
               "\n"
               "  public String toString() {\n"
               "    return \"new MT(\" + \")\";\n"
               "  }\n"
               "}\n"
               "\n"
               "class Cons extends AList {\n"
               "  int first;\n"
               "  AList rest;\n"
               "\n"
               "  Cons(int first, AList rest) {\n"
               "    this.first = first;\n"
               "    this.rest = rest;\n"
               "  }\n"
               "\n"
               "/*\n"
               ,warnin
               "  ??? mmm() {\n"
               "    ... this.first ...\n"
               "    ... this.rest.mmm() ...\n"
               "  }\n"
               "*/\n"
               "\n"
               "  public String toString() {\n"
               "    return \"new Cons(\" + first + \",\" + rest.toString() + \")\";\n"
               "  }\n"
               "}\n"
               )
             )
      "full make union")
     
     (test== 
      (make-union
       (list "AList" '(("MT" ()) ("Cons" (("int" "first") ("AList" "rest")))))
       #f #f)
      (apply string-append 
             `(
               "abstract class AList {\n"
               "}\n"
               "\n"
               "class MT extends AList {\n"
               "\n"
               "  MT() {\n"
               "  }\n"
               "}\n"
               "\n"
               "class Cons extends AList {\n"
               "  int first;\n"
               "  AList rest;\n"
               "\n"
               "  Cons(int first, AList rest) {\n"
               "    this.first = first;\n"
               "    this.rest = rest;\n"
               "  }\n"
               "}\n"
               )
             )
      "partial make union")
   
     (test== (make-class (list "foo" "" '() "hello world"))
             (apply string-append 
                    `( "// hello world\n"
                       "class foo {\n"
                       "\n"
                       "  foo() {\n"
                       "  }\n"
                       "\n"
                       ,@empty-template
                       "  public String toString() {\n"
                       "    return \"new foo(\" + \")\";\n"
                       "  }\n"
                       "}\n")))
     
     (test== 
      (make-union
       (list "AList" '(("MT" ()) ("Cons" (("int" "first") ("AList" "rest")))) "hello world")
       #f #f)
      (apply string-append 
             `("// hello world\n"
               "abstract class AList {\n"
               "}\n"
               "\n"
               "class MT extends AList {\n"
               "\n"
               "  MT() {\n"
               "  }\n"
               "}\n"
               "\n"
               "class Cons extends AList {\n"
               "  int first;\n"
               "  AList rest;\n"
               "\n"
               "  Cons(int first, AList rest) {\n"
               "    this.first = first;\n"
               "    this.rest = rest;\n"
               "  }\n"
               "}\n"
               )
             )
      "make union with purpose statement")

     |#
     )
   