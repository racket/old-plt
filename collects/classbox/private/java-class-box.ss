(module java-class-box mzscheme

  (provide java-class-box%)
  
  (require
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "framework.ss" "framework")
   (lib "gui-editor-snip.ss" "alignment")
   (lib "parser.ss" "profj")
   (lib "button-snip.ss" "test-suite" "private")
   "contracted-ast.ss")
  
  (define-syntax (string-constant stx)
    (syntax-case stx ()
      [(_ s)
       #'(case (quote s)
           [(purpose) "Purpose"]
           [(class) "Class"]
           [(extends) "Extends"]
           [(fields) "Fields"]
           [(methods) "Methods"]
           [(type) "Type"]
           [(name) "Name"]
           [(add) "Add"])]))
  
  (define-struct jfield (name type))
  
  (define java-class-box%
    (class* (decorated-editor-snip-mixin editor-snip%) (readable-snip<%>)
      
      (field
       [purpose (new text%)]
       [class (new text%)]
       [extends (new text%)])
            
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
             (define (build-field ajfield)
               (make-var-decl (build-id (jfield-name ajfield))
                              (list (make-modifier 'public false))
                              (make-type-spec
                               (build-name (jfield-type ajfield))
                               0
                               dummy-src)
                              dummy-src))
             
             #;((is-a?/c text%) . -> . method?)
             ;; Converts a string into a profj AST method
             (define (build-method amethod)
               (parse-method (open-input-text-editor amethod)
                             amethod
                             level))
             
             #;((is-a?/c text%) . -> . id?)
             ;; Make an id out of the given text
             (define (build-id atext)
               (make-id (send atext get-text)
                        dummy-src))
             
             #;((is-a?/c text%) . -> . name?)
             ;; Make a name out of the given text.
             (define (build-name atext)
               (make-name (build-id atext)
                          empty
                          dummy-src))
             
             #;src?
             ;; "A dummy src that should be ignored
             (define dummy-src false)
             
             (let* (;(listof member?)
                    ;; The list of profj ASTs that make up the members of this class
                    [members
                     (append (map build-field (send fields-area get-fields))
                             (map build-method (send methods-area get-methods)))]
                    
                    ;(listof name?)
                    ;; The list of classes this class extends
                    ;; STATUS: I don't know why this is a list, it's Kathy's data definition
                    [extends
                     (case level
                       [(beginner intermediate advanced full)
                        (list (build-name extends))])]
                    
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
                      (make-id (send class get-text)
                               ;; FIXME This make-src is wrong. I don't account for the
                               ;; possible error of multiple lines and I don't account for
                               ;; not starting at the first position of the text.
                               (make-src 1 0 1 (send class line-length 0) class))
                      (list (make-modifier 'public false))
                      extends
                      implements
                      empty
                      dummy-src #;(make-src line column position 1 source))]
                    
                    ;method?
                    ;; The constructor of the Java class
                    [constructor
                     (make-method
                      (list (make-modifier 'public false))
                      (make-type-spec 'ctor 0 dummy-src)
                      empty
                      (make-id (send class get-text) false)
                      (map (lambda (field)
                             (make-var-decl
                              (make-id (send (jfield-name field) get-text) false)
                              empty
                              (make-type-spec
                               (build-name (jfield-type field))
                               0
                               false)
                              false))
                           (send fields-area get-fields))
                      empty
                      (make-block
                       (map (lambda (field)
                              (make-call false false false
                                         (make-special-name false false "super")
                                         empty false)
                              (make-assignment
                               false
                               false
                               (make-access
                                false
                                false
                                (make-field-access
                                 (make-special-name false false "this")
                                 (make-id "car" false)
                                 false))
                               '=
                               (make-access false false (list (make-id "car" false)))
                               false))
                            (send fields-area get-fields))
                       false)
                      false
                      false)]
                    )
               
               ;; Right here I need to manipulate the boxes look and feel to be the
               ;; correct language level.
               (make-class-def header
                               (cons constructor members)
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
       [fields-area (new fields-area% (parent main))]
       [methods-area (new methods-area%
                          (super extends)
                          (fields-area fields-area)
                          (parent main))])
      
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
                       (let ([new-field (make-jfield (new text%) (new text%))])
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
  #;(define class-func (let-values ([(a b c) (send j read-one-special #f #f)])
                                  a))
  #;(class-func 'beginner #f #f 1)
  |#
  )