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
     
     #|

            dialog%
              |
            wizard%
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
     (define wizard%
       (class dialog% (init-field title insert add)
         (super-new (label title) (width 500) (height 300))
         
         ; (define f (new dialog% (label title) (width 500) (height 300)))
         (define p (new vertical-pane% (parent this)))
         
         (define button-panel (add-horizontal-panel p))
         
         (define/abstract quit-cb)
         (define quit
           (add-button button-panel "Abort" (lambda (x e) (quit-cb x e))))
         
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
       (class wizard%
         (init title insert add)
         
         (init-field (a-super null) (a-v-class null))
         
         (super-new (title title) (insert insert) (add add))
         
         (inherit-field tostring? template? info-pane)
                  
         (inherit error-message spec-error?)

         ;; --------------------------------------------------------------------
         ;; Managing the class
         
         ;; (union false (list Class Boolean Boolean))
         ;; should the dialog return a class representation at the end 
         (define the-class #f)
         
         (define fields (new assoc%))
         
         ;; (Listof (-> (list String String)) ->  (list Class Boolean Boolean)
         ;; produce a class from fields
         (define (produce-class-from-fields fields)
           (with-handlers ([(lambda (x) (spec-error? x)) (lambda _ #f)]) 
             (let* ([class (string-trim-both (send class-name get-value))]
                    [super (string-trim-both (send super-name get-value))]
                    [field (map (lambda (th) (th)) (send fields list))]
                    [field 
                     (foldr ;; r gives me the right order
                      (lambda (x r)
                        (let* ([v x] ; the privacy information isn't collecte
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
                      field)])
               (if (java-id? class)
                   (list (list class super field)
                         (send tostring? get-value)
                         (send template? get-value))
                   (error-message "check class name")))))
         
         ;; Information about the class in general: 
         ; (define info-pane (new vertical-panel% (parent this) (style '(border))))
         (define purpose 
           (new text-field% 
                (parent info-pane) (label "// purpose of class: ") (callback void)))
         (define class-pane (add-horizontal-panel info-pane))
         ; (define class-privacy (make-modifier-menu class-pane))
         (define class-name (make-text-field class-pane "class"))
         (define super-name
           (make-text-field class-pane "extends" (lambda (x e) (send/create-field x e))))
         
         ;; Information about the class's fields:
         (define field-panel (new vertical-panel% (parent info-pane)))
                  
         ;; --------------------------------------------------------------------
         ;; Managing the creation of new "add field" panels
         
         ;; (Listof TextField)
         ;; the list of name TextFields that have been added via (add-field-panel)
         ;; a stack in that the bottom field is always at beginning of list
         ;; if empty, there are no fields
         (define the-last-field-name '())
         
         ;; TextField Event -> Void
         ;; a callback that on return creates a new "add field" panel when 
         ;; it's the bottom most text field
         (define (send/create-field x e)
           (when (eq? (send e get-event-type) 'text-field-enter)
             (when (or (null? the-last-field-name)
                       (eq? (car the-last-field-name) x))
               (add-field-panel))
             (send this on-traverse-char (new key-event% (key-code #\tab)))))
         
         ;; (list Modifier String String) *-> Void
         ;; add a field panel so that a new field for the class can be specified
         ;; if rest arguments, it consists of two strings: 
         ;; one for the type, one for name
         (define (add-field-panel . a-field)
           (let* ([fp (add-horizontal-panel field-panel)]
                  ; [modi (make-modifier-menu fp)]
                  [type (make-text-field fp "type:")]
                  [name (make-text-field fp "name:"
                                         (lambda (x e) (send/create-field x e)))]
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
                     (send field-panel change-children (remove-panel fp)))))))
         
         ;; TextField -> Void
         ;; push f on the-last-field-name
         (define (add-field-name f) 
           (set! the-last-field-name (cons f the-last-field-name)))
         
         ;; TextField -> Void
         ;; remove from "stack"
         (define (remove-field-name f)
           (set! the-last-field-name (remove f the-last-field-name)))
         
         ;; -----------------------------------------------------------------------
         (define/override (add-field-cb x e) (add-field-panel))
         
         (define/override (make-class-cb x e)
           (set! the-class (produce-class-from-fields fields))
           (when the-class (send this show #f)))
         
         (define/override (quit-cb x e)
           (send this show #f))
        
         
         ;; -----------------------------------------------------------------------
         ;; setting it all up
         
         ;; String -> Void
         ;; set up the super class, uneditable 
         (define (setup-super a-super)
           (send super-name set-value a-super)
           (send (send super-name get-editor) lock #t))
         
         (cond
           [(and (null? a-super) (null? a-v-class))
            (add-field-panel)]
           [(null? a-v-class)
            (add-field-panel)
            (setup-super a-super)]
           [(null? a-super)
            (error 'internal "can't happen: no super, but class provided")]
           [else ; 
            (setup-super a-super)
            (let ([name (car a-v-class)]
                  [fields (cdr a-v-class)])
              (send class-name set-value name)
              (for-each (lambda (f) (apply add-field-panel f)) fields))])
         
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

     (define x (new class-info% (title "hello") (add "add x") (insert "insert y")))
     
     (send x show #t)

     )