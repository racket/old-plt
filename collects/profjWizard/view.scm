#cs(module view mzscheme 
     
     (require (lib "mred.ss" "mred")
              (lib "class.ss")
              (lib "etc.ss")
              (lib "list.ss")
              (lib "string.ss" "srfi" "13")
              (lib "contract.ss"))
     
     (require (file "assoc-list.scm")
              (file "data-defs.scm")
              (file "aux-class.scm"))
     
     (provide/contract
      [get-class-info (-> (union false? (list/p Class boolean? boolean?)))]
      ; [get-union-info (-> (union false? (list/p Union boolean? boolean?)))]
      )
     
     #|
     present a dialog to create a single class; 
     if programmer aborts, return #f
     otherwise, produce a class and two booleans, requesting toString and draft 
     templates, respectively

     ; (() (string? Class) . opt-> . (union false? (list/p Class boolean? boolean?)))
     |#
     
     (define CLASS-WIZARD "The Class Wizard")
     (define UNION-WIZARD "The Union Wizard")
     (define INSERT-CLASS "Insert Class")
     (define ADD-FIELD    "Add Field")

     (define (get-class-info)
       (let ([ci (new class-info% (title CLASS-WIZARD) (insert INSERT-CLASS) (add ADD-FIELD))])
         (send ci show #t)
         (send ci class)))
     
     '(define (get-union-info)
       (let ([ui (new union-info%)])
         (send ui show #t)
         (send ci produce-union)))
         
     #|

            dialog%
              |
            class-union-wizard%
              |
             / \
        --------------
        |            |
     class-info%  union-info%

     |#
     
     ;; ------------------------------------------------------------------------
     ;; Set up the entire frame, including the info panel where subclasses can 
     ;; request specific information. The frame includes buttons for aborting
     ;; the edit process, for packaging up the information in the edit, and for 
     ;; adding some component (field, variant)

     ;; String String String -> Wizard
     (define class-union-wizard%
       (class dialog% (init-field title insert add)
         (super-new (label title) (width 500) (height 300))
         
         ; (define f (new dialog% (label title) (width 500) (height 300)))
         (define p (new vertical-pane% (parent this)))
         
         (define button-panel (add-horizontal-panel p))

         (define quit
           (add-button button-panel "Abort" (lambda (x e) (send this show #f))))
         
         (define/abstract make-class-cb)
         (define class-button
           (add-button button-panel insert (lambda (x e) (make-class-cb x e))))
         
         (define/abstract add-field-cb)
         (define add-field-button
           (add-button button-panel add (lambda (x e) (add-field-cb x e))))
         
         (define switch-pane (add-horizontal-panel p))
         (field
          (tostring? (make-checkbox switch-pane "add toString()"))
          (template? (make-checkbox switch-pane "add method template")))
         
         ;; --------------------------------------------------------------------
         ;; info panel
         (field 
          (info-pane (new vertical-panel% (parent p) (style '(border)))))
         
         ;; --------------------------------------------------------------------
         ;; error panel         
         
         (define message-size 100)
         (define spec-error (cons 1 2))
         (define message
           (new message%
                (parent (add-horizontal-panel p)) 
                (label (make-string 100 #\space))))

         ;; String -> false
         (define/public (error-message m)
           (send message set-label (string-append "error: "m))
           (raise spec-error))
         ;; Any -> Boolean 
         (define/public (spec-error? x) (eq? spec-error x ))))
     
     ;; ------------------------------------------------------------------------
     (define class-info%
       (class class-union-wizard%
         (init-field (a-super null) (a-v-class null))
         (super-new)
         (inherit-field tostring? template? info-pane)
         (inherit error-message spec-error?)

         ;; --------------------------------------------------------------------
         ;; filling the info-pane 
         
         (define PURPOSE-CLASS "// purpose of class: ")
         
         ;; Information about the class in general: 
         (define purpose 
           (new text-field% 
                (parent info-pane) (label PURPOSE-CLASS) (callback void)))
         
         (define class-pane (add-horizontal-panel info-pane))
         
         ; (define class-privacy (make-modifier-menu class-pane))
         
         (define class-name 
           (make-text-field class-pane "class"))

         (define super-name
           (make-text-field class-pane "extends" 
                            (lambda (x e) (send field-panel add-on-return x e))))
         
         ;; Information about the class's fields:
         (define field-panel
           (new field-panel%
                (parent info-pane) (window this) 
                (error-message (lambda (x) (error-message x)))))
         
         (define/override (add-field-cb x e) (send field-panel add))
                  
         ;; -----------------------------------------------------------------------
         ;; creating the class from the specification 

         ;; -> (union false (list Class boolean? boolean?))
         (define/public (class)           
           (with-handlers ([(lambda (x) (spec-error? x)) (lambda _ #f)]) 
             (list (list (produce-name-from-text class-name "class")
                         (produce-name-from-text super-name "super class")
                         (send field-panel produce-fields))
                   (send tostring? get-value) 
                   (send template? get-value))))

         ;; TextField String -> java-id?
         (define (produce-name-from-text name msg)
           (let ([x (string-trim-both (send name get-value))])
             (if (java-id? x) x (error-message (format "check name of ~a" msg)))))
         
         ;; if the class specification is proper, hide dialog
         (define/override (make-class-cb x e) (when (class) (send this show #f)))

         ;; -----------------------------------------------------------------------
         ;; setting it all up
         
         ;; String -> Void
         ;; set up the super class, uneditable 
         (define (setup-super a-super)
           (send super-name set-value a-super)
           (send (send super-name get-editor) lock #t))
         
         (cond
           [(and (null? a-super) (null? a-v-class))
            (send field-panel add)]
           [(null? a-v-class)
            (send field-panel add)
            (setup-super a-super)]
           [(null? a-super)
            (error 'internal "can't happen: no super, but class provided")]
           [else ; 
            (setup-super a-super)
            (let ([name (car a-v-class)]
                  [the-fields (cdr a-v-class)])
              (send class-name set-value name)
              (for-each (lambda (f) (send field-panel add f))
                        the-fields))])
         
         ))
     
     (define field-panel%
       (class vertical-panel% 
         (init-field parent window error-message)
         (super-new (parent parent))
         
         ;; FieldsAssoc 
         (define fields (new fields-assoc% (error-message error-message)))
         
         (define/public (produce-fields) (send fields produce-fields))
         
         ;; (Listof TextField)
         ;; the list of name TextFields that have been added via (add)
         ;; a stack in that the bottom field is always at beginning of list
         ;; if empty, there are no fields
         (define the-last-field-name '())
         
         ;; TextField Event -> Void
         ;; a callback that on return creates a new "add field" panel when 
         ;; it's the bottom most text field
         (define/public (add-on-return x e)
           (when (eq? (send e get-event-type) 'text-field-enter)
             (when (or (null? the-last-field-name)
                       (eq? (car the-last-field-name) x))
               (add))
             (send window on-traverse-char (new key-event% (key-code #\tab)))))
         
         ;; Fields-Assoc (list Modifier String String) *-> Void
         ;; add a field panel so that a new field for the class can be specified
         ;; if rest arguments, it consists of two strings: 
         ;; one for the type, one for name
         (define/public (add . a-field)
           (let* ([fp (add-horizontal-panel this)]
                  ; [modi (make-modifier-menu fp)]
                  [type (make-text-field fp "type:")]
                  [name (make-text-field fp "name:"
                                         (lambda (x e) (add-on-return x e)))]
                  [get-values (lambda ()
                                (list ;(send modi get-string-selection)
                                 (send type get-value)
                                 (send name get-value)))])
             (when (pair? a-field)
               (send type set-value (car a-field))
               (send name set-value (cadr a-field)))
             (add-field-name name)
             (send fields add type get-values)
             (new button% 
                  (label "Delete Field") (parent fp)
                  (callback
                   (lambda (x e)
                     (send fields remove type)
                     (remove-field-name name)
                     (send this change-children (remove-panel fp)))))))
         
         ;; TextField -> Void
         ;; push f on the-last-field-name
         (define (add-field-name f) 
           (set! the-last-field-name (cons f the-last-field-name)))
         
         ;; TextField -> Void
         ;; remove from "stack"
         (define (remove-field-name f)
           (set! the-last-field-name (remove f the-last-field-name)))))
     
     ;; managing information about the fields in a class 
     (define fields-assoc% 
       (class assoc% 
         (init-field error-message)
         (super-new)
    
         ;; Assoc -> (Listof (list String String))
         ;; extract field info from _fields_ and produce list of strings 
         ;; from non-empty text fields 
         ;; effect: raise an error if a field spec misses the type xor name
         (define/public (produce-fields)
           (foldr ;; r gives me the right order
            (lambda (x r)
              (let* ([v x] ; the privacy information isn't collected
                     ; [v (cdr x)]  ; cdr means skip privacy attribute
                     [type (string-trim-both (car v))]
                     [name (string-trim-both (cadr v))])
                (cond
                  [(and (java-id? type) (java-id? name))
                   (cons (list type name) r)]
                  [(java-id? type)
                   (error-message (format "check field name for ~a" type))]
                  [(java-id? name)
                   (error-message (format "check type for ~a" name))]
                  [else r])))
            '()
            (map (lambda (th) (th)) (send this list))))
         ))

     ;; ------------------------------------------------------------------------
     ;; Pane -> HorizontalPanel
     ;; add a fixed-width horizontal panel (50) to p
     (define (add-horizontal-panel p)
       (new horizontal-panel% (parent p) (min-height 50) (stretchable-height #f)))
     
     ;; Panel -> (Panels -> Panels)
     ;; remove vp from cs 
     (define (remove-panel vp)
       (lambda (cs) (filter (lambda (c) (not (eq? vp c))) cs)))
     
     ;; String CallBack -> Button
     (define (add-button bp l cb) ;; to button-panel
       (new button% (label l) (parent bp) (callback cb)))
     
     ;; Panel String -> CheckBox
     (define (make-checkbox p l)
       (new check-box% (parent p) (label l) (callback void)))
     
     ;; Panel String [Callback] -> TextField
     (define make-text-field 
       (opt-lambda (p l (c void))
         (new text-field% 
              (parent p) (label l) (callback c)
              (min-width 50) (stretchable-width #f))))
     
     ;; ------------------------------------------------------------------------
     #| Run, program, run: |#
     
     (require (file "class.scm"))

     (define x (get-class-info))
     (if x (printf "~a~n" (apply make-class x)))

     #|
     (define y (get-union-info))
     (if y (printf "~a~n" (apply make-union y)))
     |#
     )