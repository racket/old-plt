#| Data Defs

 Class          = (list Name SuperClass Fields [Comment])
 ;; the name of the class, the name of the supertype ("" if none), and 
 ;; the class's fields

 DataType       = (make-union TypeName Fields VariantClasses Comment)
 ;; the name of the type and its variants
 
 VariantClasses = (Listof VariantClass)
 VariantClass   = (list Name Fields [Comment])

 Name           = String 
 TypeName       = String 
 SuperClass     = String 
 Fields         = (Listof Field)
 Field          = (list String String)
|#
#cs
(module draw-txt mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (file "data-defs.scm"))
  
  ;; ---------------------------------------------------------------------------
  ;; Deal with a Union of classes 
  
  ;; VariantClasses Super -> (Listof String)
  (define (variants*-draw variants spr)
    (flatten-string-matrix (variants*-to-strings variants spr)))
  
  ;; VariantClasses Super -> (Listof String)
  ;; turns a list of Variants into a list of strings, one per line 
  (define (variants*-to-strings variants spr)
    (let* ([d (apply max (map (lambda (vc) (length (second vc))) variants))])
      (let loop ([v variants][cnnctd #f])
        (cond
          [(null? v) '()]
          [else (let-values ([(s b) (variant-to-strings (car v) spr cnnctd d)])
                  (cons s (loop (cdr v) (or cnnctd b))))]))))
  
  ;; VariantClass Super Boolean Number -> String 
  (define (variant-draw class super left-connected depth)
    (let-values ([(s b) (variant-to-strings class super left-connected depth)])
      (strings->string-as-lines s)))
  
  ;; VariantClass Super Boolean Number ->* String Boolean 
  ;; turns a variant class into a list of strings, 
  ;; computes whether the class points to super
  ;; with hooks for refinement arrows and for recursive containment arrows
  ;; with depth :: max number of fields in a variant class of the uion 
  ;; with left-connected :: whether or not a variant to the left is already rec
  ;; with super :: the name of the datatype
  (define (variant-to-strings variant super left-connected depth)
    (let* ([cs
            (class-to-strings (cons (car variant) (cons super (cdr variant))))]
           [head (list (car cs) (cadr cs) (caddr cs))]
           [tail (cdddr cs)]
           [fields (second variant)]
           [types (map first fields)]
           [recursion #f]
           [CON "-+"]
           [CN2 " +"]
           [STG "--"] ;; (= (string-length CON) (string-length BLK))
           [BLK "  "] ;; (= (string-length CON) (string-length BLK))          
           [LIN " |"] ;; (= (string-length CON) (string-length LIN))
           [junk (lambda _ (symbol->string (gensym)))]
           [width (string-length (car cs))]
           [mkln (lambda (lft ch str) (string-append lft (make-string width ch) str))])
      (values 
       (append
        (list (string-append BLK (centered "|" width) BLK))
        (map (lambda (line type)
               (string-append BLK 
                              line 
                              (cond
                                [(string=? type super) (set! recursion #t) CON]
                                [recursion LIN]
                                [else BLK])))
             cs
             ;; pad types with junk lines for class header and class bottom
             (append (map junk head) types (list (junk))))
        (build-list (- depth -1 (length fields)) (lambda _  (mkln BLK #\space (if recursion LIN BLK))))
        (list
         (if left-connected
             (mkln STG #\- (if recursion CON STG))
             (mkln BLK #\space (if recursion CN2 BLK)))))
       recursion)))
  
  ;; String Number -> String
  ;; place str in the center of an otherwise blank string
  (define (centered str width)
    (let* ([len-str (string-length str)]
           [lft (quotient (- width len-str) 2)]
           [rgt (- width len-str lft)])
      (string-append (make-string lft #\space) str (make-string rgt #\space))))
  
  (test== (centered "|" 2) "| ")
  (test== (centered "|" 3) " | ")
  
  ;; ---------------------------------------------------------------------------
  ;; Deal with a single class 
  
  ;; Class -> String
  (define (class-draw class) 
    (strings->string-as-lines (class-to-strings class)))
  
  ;; Class -> (cons String (cons String (cons String (Listof String))))
  ;; turns a class into a list of strings that represents the class 
  (define (class-to-strings class)
    (let* ([name    (first class)]
           [super   (second class)]
           [fields  (third class)]
           [types   (map first fields)]
           [names   (map second fields)]   
           ;; start drawing 
           [fields  (create-field-declarations fields)]                         
           [width   (width-class name fields)] 
           [separat (make-separator-line width)])
      `(,separat
         ,((make-line width) name)
         ,separat
         ,@(map (make-line width) fields)
         ,separat)))
  
  ;; (Listof Field) -> (Listof String)
  ;; create text lines from Fields
  (define (create-field-declarations fields)
    (map (lambda (f) (string-append (string-append (first f) " " (second f)))) 
         fields))
  
  ;; Number -> String
  ;; make a separator line (+----+) of width
  (define (make-separator-line width)
    (string-append 
     LFT+ 
     (make-string (- width (string-length LFT+) (string-length RGT+)) #\-)
     RGT+))
  
  ;; Number -> (String -> String)
  ;; make one line in class of width from txt
  (define (make-line width)
    (lambda (txt)
      (string-append 
       LEFT 
       txt
       (make-string
        (- width (string-length txt) (string-length LEFT) (string-length RIGHT))
        #\space)
       RIGHT)))
  
  ;; String (Cons String (Listof String)) -> Number
  ;; compute width of class as the widest field/name 
  (define (width-class name fields)
    (+ (string-length LEFT)
       ;; longest field spec of name/class and fields
       (apply max (map string-length (cons name fields)))
       (string-length RIGHT)))
  
  ;; ---------------------------------------------------------------------------
  ;; Library
  
  ;; (Listof String) -> String
  ;; turn the list of strings into a single string, separating lines with newline 
  (define (strings->string-as-lines s)
    (apply string-append (map (lambda (x) (string-append x "\n")) s)))
  
  
  ;; (Listof (Listof String)) -> (Listof String)
  ;; contract: (apply = (map length smatrix))
  ;; this requires Pretty Big, it could be written in Intermediate 
  (define (flatten-string-matrix smatrix) 
    (apply map (lambda l (apply string-append l)) smatrix))
  
  (define LEFT  "| ")
  (define LFT+  "+-")
  (define RIGHT " |")
  (define RGT+  "-+") ; (= (string-length RIGHT) (string-length RGT+))
  ; (define HOOK  " |-o") ; (= (string-length RIGHT) (string-length HOOK))
  
  
  #|Tests: |#
  (require (lib "testing.scm" "testing"))
  
  "testing classes"
  (define class1 (list "Class" "Super" '(("int" "capacity") ("hello" "world"))))
  (define class2 (list "Class" "Super" '()))
  
  (define expected-class
    (list
     "+--------------+"
     "| Class        |"
     "+--------------+"
     "| int capacity |"
     "| hello world  |"
     "+--------------+"))
  
  (define expected-class2
    (list
     "+-------+"
     "| Class |"
     "+-------+"
     "+-------+"))
  
  (test== (class-draw class1) (strings->string-as-lines expected-class))
  (test== (class-draw class2) (strings->string-as-lines expected-class2))
  
  ; (printf "~a~n" (class-draw class1))
  
  "testing variants"
  (define vclass1 (list "Variant1" '()))
  (define vclass2 (list "Variant2" '(("int" "x") ("boolean" "y") ("Super" "z"))))
  (define vclass3 (list "Variant3" '(("String" "x") ("Super" "y") ("Super" "z"))))
  
  (define expected-variant1
    (list
     "       |        "
     "  +----------+  "
     "  | Variant1 |  "
     "  +----------+  "
     "  +----------+  "
     "                "
     "                "
     "                "
     "                "
     "                "))
  
  (define expected-variant2
    (list
     "        |        "
     "  +-----------+  "
     "  | Variant2  |  "
     "  +-----------+  "
     "  | int x     |  "
     "  | boolean y |  "
     "  | Super z   |-+"
     "  +-----------+ |"
     "                |"
     "                +"))
  
  (define expected-variant3
    (list
     "       |        "
     "  +----------+  "
     "  | Variant3 |  "
     "  +----------+  "
     "  | String x |  "
     "  | Super y  |-+"
     "  | Super z  |-+"
     "  +----------+ |"
     "               |"
     "---------------+"))
  
  (test== (let-values ([(s b) (variant-to-strings vclass1 "Super" #f 3)]) s)
          expected-variant1)
  (test== (variant-draw vclass1 "Super" #f 3) 
          (strings->string-as-lines expected-variant1))
  (test== (variant-draw vclass2 "Super" #f 3) 
          (strings->string-as-lines expected-variant2))
  (test== (variant-draw vclass3 "Super" #t 3) 
          (strings->string-as-lines expected-variant3))
  
  
  (test== (variants*-to-strings (list vclass1 vclass2 vclass3) "Super") 
          (list expected-variant1 expected-variant2 expected-variant3))
  
  (test== (variants*-draw (list vclass1 vclass2 vclass3) "Super") 
          (flatten-string-matrix
           (list expected-variant1 expected-variant2 expected-variant3)))
  
  (printf "~a~n" (strings->string-as-lines (variants*-draw (list vclass1 vclass2 vclass3) "Super")))
  
  #|  ---------------------------------------------------------------------------
  
  ;; Class String *-> String
  (define (classes-draw classes . super) 
    (strings->string-as-lines (apply classes-to-strings classes super)))
  
  ;; Class (Listof Classes) -> (Listof String)
  (define (class-union-to-strings utype variants) 
    (let* ([ac (class-to-strings utype)]
           [classes (classes-to-strings variants (car utype))]
           [v  (flatten-string-matrix classes)]
           [Lv (string-length (first v))]
           [the-core
            (append 
             (center-picture Lv ac)
             (center-picture Lv REFINEMENT-ARROW)
             (center-picture Lv (refinement-connectors classes))
             v)]
           [foo 0])
      (map (lambda (x) 
             (set! foo (+ foo 1))
             (cond
               [(> foo 2) (string-append x "|")]
               [(= foo 1) x]
               [(= foo 2) (replace-end-with-back-arrow x)]))
           the-core)))
  
  ;; String -> String 
  ;; add the containment arrow to an abstract class from the right fringe
  (define (replace-end-with-back-arrow x0)
    (list->string
     (reverse!
      (cons #\+ 
            (let loop ([x (reverse (string->list x0))])
              (cond
                [(char=? (cadr x) #\|) (cons #\< (cdr x))]
                [else (cons #\- (loop (cdr x)))]))))))
  
  ;; (Listof String) -> (Listof String)
  (define (refinement-connectors class-pictures)
    (let ([center-char 
           (lambda (line c l r)
             (car (center-picture (string-length line) (list c)  l r)))])
      (list 
       (string-append
        (center-char (caar class-pictures) "+" #\space #\-)
        (let loop ([cp (rest class-pictures)])
          (cond
            [(null? (rest cp))
             (center-char (caar cp) "+" #\- #\space)]
            [else (string-append (center-char (caar cp) "+" #\- #\-)
                                 (loop (rest cp)))])))
       (foldr (lambda (f r) 
                (string-append (car (center-picture (string-length (car f)) (list "|"))) r))
              ""
              class-pictures)
       )))
  
  ;; Number (Listof String) -> (Listof String)
  (define center-picture 
    (opt-lambda (Lv ac (l #\space)[r #\space])
      (let* ([delta (- Lv (string-length (first ac)))]
             [lft (quotient delta 2)])
        (map (pad-lines (make-string lft l) (make-string (- delta lft) r)) ac))))
  
  (define REFINEMENT-ARROW
    (list "/ \\"
          "---"
          " | "))
  

  
  ;; (Listof Class) String *-> (Listof (Listof String))
  ;; take a list of classes and produce a single "line" (listof string-lines)
  (define (classes-to-strings classes0 . super)
    (let* (;; (Listof (Listof String))
           [classes (map (lambda (c) (apply class-to-strings c super)) classes0)] 
           [L (apply max (map length classes))]
           [FOO "   "]
           [classes (foldr (lambda (class-los is-self-recursive rest)
                             (if (null? super)
                                 (map (pad-lines FOO FOO)
                                    (if (>= (length class-los) L)
                                        class-los
                                        (pad-class-with-blank-lines L class-los)))
                             (cons
                              (append 
                               (map (pad-lines FOO FOO)
                                    (if (>= (length class-los) L)
                                        class-los
                                        (pad-class-with-blank-lines L class-los)))
                               (list
                                (case is-self-recursive
                                  [(long)
                                   (string-append 
                                    "--"
                                    (make-string (string-length (first class-los)) #\-)
                                    CROSS
                                    (if (null? rest) "--" ""))]
                                  [(short)
                                   (string-append 
                                    "  "
                                    (make-string (string-length (first class-los)) #\space)
                                    CROSS
                                    (if (null? rest) "--" ""))]
                                  [(conn)
                                   (string-append 
                                    "--"
                                    (make-string (string-length (first class-los)) #\-)
                                    NOCROSS
                                    (if (null? rest) "--" ""))
                                   ]
                                  [(none)
                                   (string-append 
                                    "  "
                                    (make-string (string-length (first class-los)) #\space)
                                    FOO)])))
                              rest)))
                           '() 
                           classes
                           (let loop ([c classes0][prior #f])
                             (cond
                               [(null? c) '()]
                               [else (let ([self-recursive (apply is-recursive? (car c) super)])
                                       (cond
                                         [(and self-recursive prior) (cons 'long (loop (cdr c) #t))]
                                         [self-recursive (cons 'short (loop (cdr c) #t))]
                                         [prior (cons 'conn (loop (cdr c) #t))]
                                         [else (cons 'none (loop (cdr c) #f))]))])))])
      classes))
  
  
  
  (define is-recursive? 
    (case-lambda 
      [(class) #f]
      [(class super)
       (let* ([name    (first class)]
              [fields  (second class)]
              [types   (map first fields)])
         (member super types))]))
  
  
  (define CROSS "-+-")
  (define NOCROSS "---")
  
  ;; Number (Cons String (Listof String)) -> (Listof String)
  (define (pad-class-with-blank-lines n l)
    (let ([blanks (make-string (string-length (first l)) #\space)])
      (append l (build-list (- n (length l)) (lambda (i) blanks)))))
  
  ;; (Listof Field) -> (Listof String)
  (define (create-field-declarations fields)
    (map (lambda (f) (string-append (string-append (first f) " " (second f)))) 
         fields))
  
  ;; String Boolean *-> String
  (define (make-separator-line width . hasa)
    (string-append LFT+ (make-string (- width (string-length LFT+) (string-length RGT+)) #\-) 
                   (if (null? hasa) RGT+ HASA+)))
  
  ;; Number String -> String
  ;; make one line in class of width from txt
  (define (make-line width . rgt)
    (lambda (txt)
      (string-append 
       LEFT 
       txt
       (make-string 
        (- width (string-length txt) (string-length LEFT) (string-length RIGHT))
        #\space)
       (if (null? rgt) RIGHT (car rgt)))))
  
  (define LEFT  "  | ")
  (define LFT+  "  +-")
  (define RIGHT " |  ")
  (define HOOK  " |-o") ; (= (string-length RIGHT) (string-length HOOK))
  (define RGT+  "-+  ") ; (= (string-length RIGHT) (string-length RGT+))
  (define HASA  " | |") ; (= (string-length RIGHT) (string-length HASA))
  (define HASA+ "-+ |") ; (= (string-length RIGHT) (string-length RGT+))
  
  ;; String (Cons String (Listof String)) -> Number
  ;; compute width of class as the widest field/name 
  (define (width-class name fields)
    (+ (string-length LEFT)
       ;; longest field spec of name/class and fields
       (apply max (map string-length (cons name fields)))
       (string-length RIGHT)))
  
  ;; String String -> (String -> String)
  ;; add lft and rgt to txt 
  (define (pad-lines lft rgt) (lambda (txt) (string-append lft txt rgt)))
  
  
  
  ;; (Listof String) -> String
  ;; turn the list of strings into a single string, separating lines with newline 
  (define (strings->string-as-lines s)
    (apply string-append (map (lambda (x) (string-append x "\n")) s)))
  
  ;; Basic Tests: 
  
  
  (equal? 
   (flatten-string-matrix
    (list (list "a1" "a2" "a3") (list "b1" "b2" "b3") (list "c1" "c2" "c3")))
   (list "a1b1c1" "a2b2c2" "a3b3c3"))
  
  ;; Tests
  (require (lib "testing.scm" "testing"))
  

  (test== (width-class (car aTrain) (create-field-declarations (cadr aTrain)))
          (+ 12 (string-length LEFT) (string-length RIGHT)))
  
  (test== ([make-line (+ 12 (string-length LEFT) (string-length RIGHT))] "int capacity")
          "  | int capacity |  ")
  
  (test== (make-separator-line (+ 12 (string-length LEFT) (string-length RIGHT)))
          "  +--------------+  ")
  

  (test== (classes-to-strings (list sTrain aTrain))
          '(("     +--------------+      "
             "     | Train        |      "
             "     +--------------+      "
             "     | int capacity |      "
             "     +--------------+      "
             "                           ")
            ("     +--------------+      "
             "     | Train        |      "
             "     +--------------+      "
             "     | int capacity |      "
             "     | hello world  |      "
             "     +--------------+      ")))
  
  ;; Union: ARiver = Source(Location) | Confluence(Location, ARiver, ARiver)
  (define ARiver
    (list "ARiver" '()))
  (define Source
    (list "Source" '(("Location" "loc"))))
  (define Confluence
    (list "Confluence" '(("Location" "loc") ("ARiver" "left") ("ARiver" "right"))))
  
  (test== (strings->string-as-lines
           (class-union-to-strings ARiver (list Source Confluence)))
          (strings->string-as-lines
          '("                    +--------+                      "
            "                    | ARiver |<---------------------+"
            "                    +--------+                      |"
            "                    +--------+                      |"
            "                       / \\                          |" ;; note escape 
            "                       ---                          |"
            "                        |                           |"
            "           +--------------------------+             |"
            "           |                          |             |"
            "   +--------------+           +--------------+      |"
            "   | Source       |           | Confluence   |      |"
            "   +--------------+           +--------------+      |"
            "   | Location loc |           | Location loc |      |"
            "   +--------------+           | ARiver left  |-o    |"
            "                              | ARiver right |-o    |"
            "                              +--------------+ |    |"
            "                                              -+----|")))
  
  (test== (replace-end-with-back-arrow "| ARiver |     ")
          "| ARiver |<----+"
          "replace end with back arrow")
  
  (printf "~a" (strings->string-as-lines (class-union-to-strings ARiver (list Source Confluence))))
  |#
  )
