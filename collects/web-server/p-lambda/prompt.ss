(module prompt mzscheme
  (require (lib "mred.ss" "mred"))
  (require (lib "class.ss"))
  (provide prompt-frame%)

  ;; make-history-list: -> (value -> void)
  ;;                       (-> value number number)
  ;;                       (-> value number number)
  ;; produce a functions to:
  ;; (1) insert an item into the current position of the history list
  ;; (2) move forward in the history list and return next forward item
  ;; (3) move backward in the history list and return next backward item
  (define (make-history-list current)
    (let ([depth 0]
          [height 0]
          [down-stack '()]
          [up-stack '()])
      (values
       ;; insert: value -> value number number
       (lambda (val)
         (set! down-stack (cons current down-stack))
         (set! depth (add1 depth))
         (set! current val)
         (set! height 0)
         (values current depth height))
       
       ;; forward: -> value number number
       (lambda ()
         (set! down-stack (cons current down-stack))
         (set! depth (add1 depth))
         (set! current (car up-stack))
         (set! up-stack (cdr up-stack))
         (set! height (sub1 height))
         (values current depth height))
       
       ;; back: -> value number number
       (lambda ()
         (set! up-stack (cons current up-stack))
         (set! height (add1 height))
         (set! current (car down-stack))
         (set! down-stack (cdr down-stack))
         (set! depth (sub1 depth))
         (values current depth height)))))
  
  (define prompt-frame%
    (class* frame% ()
      (super-new (width 500) (height 200))
      
      (init-field result-channel)
      
      (inherit reflow-container add-child delete-child)
      
      (define (update-buttons! dp ht)
        (if (> dp 1)
            (send back-button enable #t)
            (send back-button enable #f))
        (if (> ht 0)
            (send forward-button enable #t)
            (send forward-button enable #f))
        (reflow-container))
      
      (define (on-b-f proc!)
        (let-values ([(cur dp ht) (proc!)])
          (delete-child current)
          (set! current cur)
          (add-child current)
          (update-buttons! dp ht)))
      
      (define (on-spawn btn evt)
        (void))
      
      (define-values (back-button #;spawn-button forward-button)
        (let ([pan (new horizontal-panel% (parent this) (alignment '(center top)))])
          (values (new button% (label "Back") (parent pan) (enabled #f)
                       (callback
                        (lambda (btn evt)
                          (on-b-f backward!))))
                  ;(new button% (label "Spawn") (parent pan) (callback on-spawn))
                  (new button% (label "Forward") (parent pan) (enabled #f)
                       (callback
                        (lambda (btn  evt)
                          (on-b-f forward!)))))))
      
      (define current
        (let ([pan (new vertical-panel% (parent this) (alignment '(center center)))])
          (new message% (label "No Page") (parent pan))
          pan))
      
      (define-values (insert! forward! backward!)
        (make-history-list current))
      
      (define (navigate page)
        (let-values ([(cur dp ht) (insert! page)])
          (delete-child current)
          (set! current page)
          (send current show #t)
          (update-buttons! dp ht)))
      
      (define/public (prompt qtn . args)
        (let ([new-page (new page%
                             (parent this)
                             (question qtn)
                             (on-submit
                              (lambda (val)
                                (channel-put result-channel (cons val args)))))])
          (navigate new-page)))
      ))
  
  (define page%
    (class vertical-panel% ()
      (init-field question)
      (init-field on-submit)
      (super-new)
      (let* ([pan (new horizontal-panel% (parent this))]
             [field (new text-field%
                         (parent pan)
                         (label question)
                         (callback
                          (lambda (a-t-field evt)
                            (when (eqv? 'text-field-enter
                                        (send evt get-event-type))
                              (on-submit
                               (send a-t-field get-value)))))
                         )])
        (new button%
             (label "Submit")
             (parent this)
             (callback
              (lambda (a-button evt)
                (on-submit
                 (send field get-value))))))))
  )