(module java-class-box mzscheme

  (provide java-class-box%)
  
  (require
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "framework.ss" "framework")
   (lib "aligned-pasteboard.ss" "embedded-gui")
   (lib "parser.ss" "profj")
   (lib "button-snip.ss" "embedded-gui")
   (lib "match.ss")
   "contracted-ast.ss")
  
  (define (wrap x) (write x) x)
  
  (define-syntax (string-constant stx)
    (syntax-case stx ()
      [(_ s)
       #'(case (quote s)
           [(purpose) "Purpose"]
           [(class) "Class"]
           [(extends) "Extends"]
           [(fields) "Fields"]
           [(methods) "Methods"]
           [(implements) "Implements"]
           [(type) "Type"]
           [(name) "Name"]
           [(add) "Add"])]))
  
  (define-struct jfield (name type))
  
  (define java-class-box%
    (class* (decorated-editor-snip-mixin editor-snip%) (readable-snip<%>)
      
      (field
       [purpose (new text%)]
       [class (new color:text%)]
       [extends (new color:text%)]
       [implements (new color:text%)])
            
      #;((integer? any?)
         ((union integer? false?) (union integer? false?) (union integer? false?))
         . opt-> .
         (values (symbol? any? any? any? . -> . any?) number? boolean?))
      (define/public read-one-special
        (opt-lambda (index source (line false) (column false) (position false))
          (values
           (lambda (level class-loc box-pos input-spec)
             
             #;(jfield? . -> . var-decl?)
             ;; Converts a jfield into a profj AST field
             (define (build-var-decl ajfield)
               (make-var-decl (build-id (jfield-name ajfield))
                              (list (make-modifier 'public false))
                              (make-type-spec
                               (build-name (jfield-type ajfield))
                               0
                               false)
                              false))
             
             #;((is-a?/c text%) . -> . method?)
             ;; Converts a string into a profj AST method
             (define (build-method amethod)
               (parse-method (open-input-text-editor amethod)
                             amethod
                             level))
             
             #;((is-a?/c text%) . -> . id?)
             ;; Make an id out of the given text
             ;; STATUS: I'm parsing the ID with a regexp that probablly not
             ;; the correct Java variable regexp. Furthermore, I need to parse
             ;; it differently if it's a class name vs. field name.
             (define (build-id atext)
               (let ([str (send atext get-text)])
                 (match-let ([((start . end))
                              (regexp-match-positions (regexp "[A-Za-z_]+") str)])
                   (make-id (substring str start end)
                            (make-src 1 start (add1 start) (- end start) atext)))))
             
             #;((is-a?/c text%) . -> . name?)
             ;; Make a name out of the given text.
             ;; STATUS: Is the src the same from the ID to the name?
             (define (build-name atext)
               (let ([id (build-id atext)])
                 (make-name id empty (id-src id))))
             
             #;(jfield? . -> . assignment?)
             ;; makes an assignment for the constructor
             (define (build-assignment field)
               (make-assignment
                false
                false
                (make-access
                 false
                 false
                 (make-field-access
                  (make-special-name false dummy-src "this")
                  (build-id (jfield-name field))
                  false))
                '=
                (make-access
                 false
                 false
                 (list (build-id (jfield-name field))))
                false))
             
             #;src?
             ;; "A dummy src that should be ignored
             (define dummy-src (make-src 1 1 1 1 "window"))
             
             (let* (;(listof member?)
                    [fields (map build-var-decl (send fields-area get-fields))]
                    
                    ;(listof member?)
                    [methods (map build-method (send methods-area get-methods))]
                    
                    ;(listof name?)
                    ;; The list of classes this class extends
                    ;; STATUS: I don't know why this is a list, it's Kathy's data definition
                    [extends (list (build-name extends))]
                    
                    ;(listof name?)
                    ;; The interfaces this class implements
                    ;; STATUS: Implements not part of box just yet.
                    [implements
                     (case level
                       [(beginner) empty]
                       [(intermediate advanced full)
                        (list)])]
                    
                    ;header?
                    ;; The header of this class in profj AST data types
                    [header
                     (make-header
                      (build-id class)
                      (list (make-modifier 'public false))
                      extends
                      implements
                      empty
                      (make-src line column position 1 source))]
                    
                    ;method?
                    ;; The constructor of the Java class
                    [constructor
                     (make-method
                      (list (make-modifier 'public false))
                      (make-type-spec 'ctor 0 #f)
                      empty
                      (build-id class)
                      (map build-var-decl
                           (send fields-area get-fields))
                      empty
                      (make-block
                       (cons (make-call false false false
                                        (make-special-name false false "super")
                                        empty false)
                             (map build-assignment
                                  (send fields-area get-fields)))
                       false)
                      false
                      false)]
                    )
               
               (arange-box level)
               (make-class-def header
                               (append fields (cons constructor methods))
                               #f
                               box-pos
                               class-loc
                               level
                               empty
                               'top)))
           1
           #t)))
      
      ;;;;;;;;;;
      ;; Layout
      
      (field
       [main (new aligned-pasteboard%)]
       [purpose-line (add-purpose-line main purpose)]
       [class-line (new class-line% (class class) (extends extends) (parent main))]
       [implements-line (new implements-line% (implements implements) (parent main))]
       [fields-area (new fields-area% (parent main))]
       [methods-area (new methods-area%
                          (super extends)
                          (fields-area fields-area)
                          (parent main))])
      
      #;(level? . -> . void?)
      ;; Aranges the box to look the way it should for a given language level
      (define (arange-box level)
        (case level
          [(beginner) (send implements-line show false)]
          [(intermediate advanced full)
           (send implements-line show true)]))
      
      (super-new (editor main))
      (inherit set-snipclass)
      (set-snipclass jcb-sc)))
    
  (define java-class-box-snipclass%
    (class snip-class%
      (define/override (read f)
        (let ([box (new java-class-box%)])
          (send box read-from-file f)
          box))
      (super-new)))
  
  (define jcb-sc (new java-class-box-snipclass%))
  (send jcb-sc set-classname "java-class-box%")
  (send jcb-sc set-version 1)
  (send (get-the-snip-class-list) add jcb-sc)
  
  (define (add-purpose-line parent text)
    (let ([line (new horizontal-alignment% (parent parent))])
      (send* line
        (add (make-object string-snip% (string-constant purpose)))
        (add (new editor-snip% (editor text))))))
  
  (define class-line%
    (class horizontal-alignment%
      (inherit add)
      (init-field class extends)
      (super-new)
      (add (make-object string-snip% (string-constant class)))
      (add (new editor-snip% (editor class)))
      (add (make-object string-snip% (string-constant extends)))
      (add (new editor-snip% (editor extends)))))
  
  (define implements-line%
    (class horizontal-alignment%
      (inherit add)
      (init-field implements)
      (super-new)
      (add (make-object string-snip% (string-constant implements)))
      (add (new editor-snip% (editor implements)))))
    
  (define fields-area%
    (class vertical-alignment%
      (inherit add)
      (inherit-field children)
      (field [fields empty])
      #;(-> (listof jfield?))
      (define/public (get-fields) fields)
      (super-new)
      (let ([bar (new horizontal-alignment% (parent this))]
            [table (new grid-alignment% (parent this) (columns 2))])
        (send table add (vector (make-object string-snip% (string-constant type))
                                (make-object string-snip% (string-constant name))))
        (send* bar
          (add (make-object string-snip% (string-constant fields)))
          (add (new text-button-snip%
                    (label (string-constant add))
                    (callback
                     (lambda (button event)
                       (let ([new-field (make-jfield (new color:text%) (new color:text%))])
                         (set! fields (append fields (list new-field)))
                         (send table add
                               (vector (new editor-snip% (editor (jfield-type new-field)))
                                       (new editor-snip%
                                            (editor (jfield-name new-field))))))))))))
      ))
  
  (define methods-area%
    (class vertical-alignment%
      (inherit add)
      (inherit-field children)
      (init-field super fields-area)
      #;(-> (listof text%))
      (define/public (get-methods)
        (map (lambda (eta)
               (send eta get-body))
             (filter
              (lambda (child) (is-a? child method-line%))
              children)))
      (super-new)
      (let ([bar (new horizontal-alignment% (parent this))])
        (send* bar
          (add (make-object string-snip% (string-constant methods)))
          (add (new text-button-snip%
                    (label (string-constant add))
                    (callback
                     (lambda (button event)
                       (new method-line%
                            (super super)
                            (fields (send fields-area get-fields))
                            (parent this))))))))
      ))
  
  (define method-line%
    (class horizontal-alignment%
      (inherit add)
      (init-field super fields)
      (field [body (new text%)])
      (define/public (get-body) body)
      (super-new)
      (send body insert
            (template
             (send super get-text)
             fields))
      (add (new editor-snip% (editor body)))))
  
  #;(string? (listof jfield?) . -> . string?)
  ;; A template of a method with the given class and fields.
  (define (template super fields)
    "This is a template")
  
  #|
  (define f (new frame% (label "f")))
  (define e (new text%))
  (define c (new editor-canvas% (parent f) (editor e)))
  (define j (new java-class-box%))
  (send e insert j)
  (send f show true)
  #;(define class-func (let-values ([(a b c) (send j read-one-special #f #f)]) a))
  #;(class-func 'beginner #f #f 1)
  |#
  )