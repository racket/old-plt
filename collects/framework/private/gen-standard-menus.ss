(module gen-standard-menus mzscheme
  (require (lib "pretty.ss"))
  (require (lib "list.ss"))
  (require "standard-menus-items.ss")
  
  ;; build-before-super-item-clause : an-item -> (listof clause)
  (define build-before-super-item-clause
    (lambda (item)
      (list
       `(public ,(an-item->callback-name item)
                ,(an-item->get-item-name item)
                ,(an-item->string-name item)
                ,(an-item->help-string-name item)
                ,(an-item->on-demand-name item)
                ,(an-item->create-menu-item-name item))
       `[define ,(an-item->callback-name item)
          ,(or (an-item-proc item) `(lambda (x y) (void)))]
       `[define ,(an-item->get-item-name item)
          (lambda () ,(an-item->item-name item))]
       `[define ,(an-item->string-name item)
          (lambda () "")]
       `[define ,(an-item->help-string-name item)
          (lambda () ,(an-item-help-string item))]
       `[define ,(an-item->on-demand-name item)
          ,(an-item-on-demand item)]
       `[define ,(an-item->create-menu-item-name item)
          (lambda () ,(not (not (an-item-proc item))))])))
  
  ;; build-before-super-clause : ((X -> sym) (X sexp) -> X -> (listof clause))
  (define build-before-super-clause
    (lambda (->name -procedure)
      (lambda (obj)
        (list `(public ,(->name obj))
              `[define ,(->name obj)
                 ,(case (-procedure obj)
                    [(nothing) '(lambda (menu) (void))]
                    [(separator) '(lambda (menu) (make-object separator-menu-item% menu))])]))))
  
  ;; build-before-super-between-clause : between -> (listof clause)
  (define build-before-super-between-clause
    (build-before-super-clause
     between->name
     between-procedure))
  
  ;; build-before-super-before/after-clause : before/after -> (listof clause)
  (define build-before-super-before/after-clause
    (build-before-super-clause
     before/after->name
     before/after-procedure))
  
  ;; build-after-super-item-clause : an-item -> (list clause)
  (define (build-after-super-item-clause item)
    (let* ([callback-name (an-item->callback-name item)]
           [create-menu-item-name (an-item->create-menu-item-name item)]
           [callback-name-string (symbol->string callback-name)]
           [menu-before-string (an-item-menu-string-before item)]
           [menu-after-string (an-item-menu-string-after item)]
           [key (an-item-key item)]
           [join (lambda (base-text suffix-text special-text)
                   `(let ([special ,special-text]
                          [base ,base-text]
                          [suffix ,suffix-text])
                      (if (string=? special "")
                          (string-append base suffix)
                          (string-append base " " special suffix))))])
      (list `(define
               ,(an-item->item-name item)
               (and (,create-menu-item-name)
                    (instantiate (get-menu-item%) ()
                      (label ,(join menu-before-string menu-after-string
                                    `(,(an-item->string-name item))))
                      (parent ,(menu-item-menu-name item))
                      (callback (let ([,callback-name (lambda (item evt) (,callback-name item evt))])
                                  ,callback-name))
                      (shortcut ,key)
                      (help-string (,(an-item->help-string-name item)))
                      (demand-callback (lambda (menu-item) (,(an-item->on-demand-name item) menu-item)))))))))
  
  ;; build-after-super-clause : ((X -> symbol) -> X -> (listof clause))
  (define build-after-super-clause
    (lambda (->name)
      (lambda (between/after)
        (list 
         `(,(->name between/after)
           (,(menu-name->get-menu-name between/after)))))))
  
  ;; build-after-super-between-clause : between -> (listof clause)
  (define build-after-super-between-clause (build-after-super-clause between->name))
  ;; build-after-super-before/after-clause : before/after -> (listof clause)
  (define build-after-super-before/after-clause (build-after-super-clause before/after->name))
  
  ;; build-after-super-generic-clause : generic -> (listof clause)
  (define (build-after-super-generic-clause x) 
    (cond
      [(generic-private-field? x)
       (list `(define
                ,(generic-name x)
                ,(generic-initializer x)))]
      [(generic-override? x)
       (list `(rename [,(string->symbol (format "super-~a" (generic-name x)))
                       ,(generic-name x)]))]
      [(generic-method? x)
       null]))

  ;; build-before-super-generic-clause : generic -> (listof clause)
  (define (build-before-super-generic-clause generic)
    (cond
      [(generic-private-field? generic)
       null]
      [(generic-override? generic)
       (list `(override ,(generic-name generic))
             `[define ,(generic-name generic)
                ,(generic-initializer generic)])]
      [(generic-method? generic)
       (list `(public ,(generic-name generic) )
             `[define ,(generic-name generic)
                ,(generic-initializer generic)])]))
  
  
  (define standard-menus.ss-filename (build-path (collection-path "framework" "private") "standard-menus.ss"))
  (printf "writing to ~a~n" standard-menus.ss-filename)  
  
  (call-with-output-file standard-menus.ss-filename
    (lambda (port)
      (pretty-print
       `(define standard-menus<%>
          (interface (basic<%>)
            ,@(apply append (map
                             (lambda (x)
                               (cond
                                 [(an-item? x) 
                                  (list 
                                   (an-item->callback-name x)
                                   (an-item->get-item-name x)
                                   (an-item->string-name x)
                                   (an-item->help-string-name x)
                                   (an-item->on-demand-name x)
                                   (an-item->create-menu-item-name x))]
                                 [(between? x) (list (between->name x))]
                                 [(or (after? x) (before? x))
                                  (list (before/after->name x))]
                                 [(generic? x) 
                                  (if (generic-method? x)
                                      (list (generic-name x))
                                      null)])) 
                             items))))
       port)
      
      (newline port)
      
      (pretty-print
       `(define standard-menus-mixin
          (mixin (basic<%>) (standard-menus<%>)
            (inherit on-menu-char on-traverse-char)
            (define remove-prefs-callback
              (preferences:add-callback
               'framework:menu-bindings
               (lambda (p v)
                 (let ([mb (get-menu-bar)])
                   (let loop ([menu (get-menu-bar)])
                     (cond
                       [(is-a? menu menu-item-container<%>)
                        (for-each loop (send menu get-items))]
                       [(is-a? menu selectable-menu-item<%>)
                        (when (is-a? menu menu:can-restore<%>)
                          (if v
                              (send menu restore-keybinding)
                              (send menu set-shortcut #f)))]))))))
            
            (inherit get-menu-bar show can-close? get-edit-target-object)
            ,@(apply append (map (lambda (x)
                                   (cond
                                     [(between? x) (build-before-super-between-clause x)]
                                     [(or (after? x) (before? x)) (build-before-super-before/after-clause x)]
                                     [(an-item? x) (build-before-super-item-clause x)]
                                     [(generic? x) (build-before-super-generic-clause x)]
                                     [else (printf "~a~n" x)]))
                                 items))
            (super-instantiate ())
            ,@(apply append (map (lambda (x)
                                   (cond
                                     [(between? x) (build-after-super-between-clause x)]
                                     [(an-item? x) (build-after-super-item-clause x)]
                                     [(or (after? x) (before? x)) (build-after-super-before/after-clause x)]
                                     [(generic? x) (build-after-super-generic-clause x)]))
                                 items))
            (reorder-menus this)))
       port))
    'text
    'truncate))
