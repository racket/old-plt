(module color-selection mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (prefix fw: (lib "framework.ss" "framework"))
           (lib "string-constant.ss" "string-constants"))
  
  (provide color-selection% set-default)

  (define standard-style-list-text% (fw:editor:standard-style-list-mixin text%))

  (define color-selection%
    (class horizontal-panel%
      (init symbol prefix)
      (super-instantiate () (style '(border)))
      
      (define sym (string->symbol (format "~a:~a" prefix symbol)))
      
      (define delta (fw:preferences:get sym))
      (define style-name (symbol->string sym))
      (define c (make-object editor-canvas% this
                  #f
                  (list 'hide-hscroll
                        'hide-vscroll)))
      (send c set-line-count 1)
      (send c allow-tab-exit #t)
      (define e (new (class standard-style-list-text%
                       (inherit change-style get-style-list)
                       (rename [super-after-insert after-insert])
                       (override after-insert)
                       (define (after-insert pos offset)
                         (super-after-insert pos offset)
                         (let ([style (send (get-style-list)
                                            find-named-style
                                            style-name)])
                           (change-style style pos (+ pos offset) #f)))
                       (super-instantiate ()))))
      (fw:preferences:add-callback sym
                                   (lambda (sym v)
                                     (set-slatex-style sym v)
                                     #t))
      (set-slatex-style sym delta)
      (define (make-check name on off)
        (let* ([c (lambda (check command)
                    (if (send check get-value)
                        (on)
                        (off))
                    (fw:preferences:set sym delta))]
               [check (make-object check-box% name this c)])
          check))
      (send c set-editor e)
      (send* e
        (insert (symbol->string symbol))
        (set-position 0))
      (define slant-check
        (make-check (string-constant cs-italic)
                    (lambda ()
                      (send delta set-style-on 'slant)
                      (send delta set-style-off 'base))
                    (lambda ()
                      (send delta set-style-on 'base)
                      (send delta set-style-off 'slant))))
      (define bold-check
        (make-check (string-constant cs-bold)
                    (lambda ()
                      (send delta set-weight-on 'bold)
                      (send delta set-weight-off 'base))
                    (lambda ()
                      (send delta set-weight-on 'base)
                      (send delta set-weight-off 'bold))))
      (define underline-check
        (make-check (string-constant cs-underline)
                    (lambda ()
                      (send delta set-underlined-on #t)
                      (send delta set-underlined-off #f))
                    (lambda ()
                      (send delta set-underlined-off #t)
                      (send delta set-underlined-on #f))))
      (define color-button
        (and (>= (get-display-depth) 8)
             (make-object button%
               (string-constant cs-change-color)
               this
               (lambda (color-button evt)
                 (let* ([add (send delta get-foreground-add)]
                        [color (make-object color%
                                 (send add get-r)
                                 (send add get-g)
                                 (send add get-b))]
                        [users-choice
                         (get-color-from-user
                          (format "Choose a color for ~a"
                                  (symbol->string symbol))
                          (send color-button get-top-level-window)
                          color)])
                   (when users-choice
                     (send delta set-delta-foreground users-choice)
                     (fw:preferences:set sym delta)))))))
      (define style (send (send e get-style-list) find-named-style style-name))
      (send slant-check set-value (eq? (send style get-style) 'slant))
      (send bold-check set-value (eq? (send style get-weight) 'bold))
      (send underline-check set-value (send style get-underlined))))
  
  (define add/mult-set
    (lambda (m v)
      (send m set (car v) (cadr v) (caddr v))))
  
  (define add/mult-get
    (lambda (m)
      (let ([b1 (box 0)]
            [b2 (box 0)]
            [b3 (box 0)])
        (send m get b1 b2 b3)
        (map unbox (list b1 b2 b3)))))
  
  (define style-delta-get/set
    (list (cons (lambda (x) (send x get-alignment-off))
                (lambda (x v) (send x set-alignment-off v)))
          (cons (lambda (x) (send x get-alignment-on))
                (lambda (x v) (send x set-alignment-on v)))
          (cons (lambda (x) (add/mult-get (send x get-background-add)))
                (lambda (x v) (add/mult-set (send x get-background-add) v)))
          (cons (lambda (x) (add/mult-get (send x get-background-mult)))
                (lambda (x v) (add/mult-set (send x get-background-mult) v)))
          (cons (lambda (x) (send x get-face))
                (lambda (x v) (send x set-face v)))
          (cons (lambda (x) (send x get-family))
                (lambda (x v) (send x set-family v)))
          (cons (lambda (x) (add/mult-get (send x get-foreground-add)))
                (lambda (x v) (add/mult-set (send x get-foreground-add) v)))
          (cons (lambda (x) (add/mult-get (send x get-foreground-mult)))
                (lambda (x v) (add/mult-set (send x get-foreground-mult) v)))
          (cons (lambda (x) (send x get-size-add))
                (lambda (x v) (send x set-size-add v)))
          (cons (lambda (x) (send x get-size-mult))
                (lambda (x v) (send x set-size-mult v)))
          (cons (lambda (x) (send x get-style-off))
                (lambda (x v) (send x set-style-off v)))
          (cons (lambda (x) (send x get-style-on))
                (lambda (x v) (send x set-style-on v)))
          (cons (lambda (x) (send x get-underlined-off))
                (lambda (x v) (send x set-underlined-off v)))
          (cons (lambda (x) (send x get-underlined-on))
                (lambda (x v) (send x set-underlined-on v)))
          (cons (lambda (x) (send x get-weight-off))
                (lambda (x v) (send x set-weight-off v)))
          (cons (lambda (x) (send x get-weight-on))
                (lambda (x v) (send x set-weight-on v)))))
  
  (define (marshall-style style)
    (map (lambda (fs) ((car fs) style)) style-delta-get/set))
  
  (define (unmarshall-style info)
    (let ([style (make-object style-delta%)])
      (for-each (lambda (fs v) ((cdr fs) style v)) style-delta-get/set info)
      style))
      
  (define (set-default sym code-style)
    (fw:preferences:set-default
     sym
     code-style
     (lambda (x)
       (is-a? x style-delta%)))
    (fw:preferences:set-un/marshall sym marshall-style unmarshall-style))

  
  ; a symbol naming the style  and a delta to set it to
  (define set-slatex-style
    (lambda (sym delta)
      (let* ([style-list (fw:editor:get-standard-style-list)]
             [name (symbol->string sym)]
             [style (send style-list find-named-style name)])
        (if style
            (send style set-delta delta)
            (send style-list new-named-style name
                  (send style-list find-or-create-style
                        (send style-list find-named-style "Standard")
                        delta))))))
  
   
  )