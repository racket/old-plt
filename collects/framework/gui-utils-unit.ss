(module gui-utils-unit mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "mred-sig.ss" "mred")
	   "gui-utils-sig.ss")
  
  (provide framework:gui-utils@)
  
  (define framework:gui-utils@
    (unit/sig framework:gui-utils^
      (import mred^)
      
      (define clickback-delta (make-object style-delta% 'change-underline #t))
      (send clickback-delta set-delta-foreground "BLUE")
      (define (get-clickback-delta) clickback-delta)
      (define clicked-clickback-delta (make-object style-delta%))
      (send clicked-clickback-delta set-delta-background "BLACK")
      (define (get-clicked-clickback-delta) clicked-clickback-delta)
      
      (define next-untitled-name
        (let ([n 1])
          (lambda ()
            (begin0
              (cond
                [(= n 1) (string-constant untitled)]
                [else (format (string-constant untitled-n) n)])
              (set! n (+ n 1))))))
      
      (define cursor-delay
        (let ([x 0.25])
          (case-lambda
           [() x]
           [(v) (set! x v) x])))
      
      (define show-busy-cursor
        (lambda (thunk)
          (local-busy-cursor #f thunk)))
      
      (define delay-action
        (lambda (delay-time open close)
          (let ([semaphore (make-semaphore 1)]
                [open? #f]
                [skip-it? #f])
            (thread 
             (lambda ()
               (sleep delay-time)
               (semaphore-wait semaphore)
               (unless skip-it?
                 (set! open? #t)
                 (open))
               (semaphore-post semaphore)))
            (lambda ()
              (semaphore-wait semaphore)
              (set! skip-it? #t)
              (when open?
                (close))
              (semaphore-post semaphore)))))
      
      (define local-busy-cursor
        (let ([watch (make-object cursor% 'watch)])
          (case-lambda
           [(win thunk) (local-busy-cursor win thunk (cursor-delay))]
           [(win thunk delay)
            (let* ([old-cursor #f]
                   [cursor-off void])
              (dynamic-wind
               (lambda ()
                 (set! cursor-off
                       (delay-action
                        delay
                        (lambda ()
                          (if win
                              (begin (set! old-cursor (send win get-cursor))
                                     (send win set-cursor watch))
                              (begin-busy-cursor)))
                        (lambda ()
                          (if win
                              (send win set-cursor old-cursor)
                              (end-busy-cursor))))))
               (lambda () (thunk))
               (lambda () (cursor-off))))])))
      
      (define unsaved-warning
        (case-lambda
         [(filename action-anyway) (unsaved-warning filename action-anyway #f)]
         [(filename action-anyway can-save-now?) (unsaved-warning filename action-anyway can-save-now? #f)]
         [(filename action-anyway can-save-now? parent)
          (let* ([result (void)]
                 [unsaved-dialog%
                  (class100 dialog% ()
                    (inherit show center)
                    (private-field
                     [on-dont-save
                      (lambda args
                        (set! result 'continue)
                        (show #f))]
                     [on-save-now
                      (lambda rags
                        (set! result 'save)
                        (show #f))]
                     [on-cancel
                      (lambda args
                        (set! result 'cancel)
                        (show #f))])
                    (sequence
                      (super-init (string-constant warning) parent)
                      (let* ([panel (make-object vertical-panel% this)]
                             [msg
                              (make-object message%
                                (format (string-constant file-is-not-saved) filename)
                                panel)]
                             [button-panel
                              (make-object horizontal-panel% panel)])
                        (make-object button% 
                          (string-append action-anyway)
                          button-panel
                          on-dont-save)
                        (let ([now (make-object button% 
                                     (string-constant save)
                                     button-panel
                                     on-save-now
                                     (if can-save-now?
                                         '(border)
                                         '()))]
                              [cancel (make-object button%
                                        (string-constant cancel)
                                        button-panel
                                        on-cancel
                                        (if can-save-now?
                                            '()
                                            '(border)))])
                          (if can-save-now?
                              (send now focus)
                              (begin (send cancel focus)
                                     (send now show #f)))))
                      
                      (center 'both)
                      
                      (show #t)))])
            (make-object unsaved-dialog%)
            result)]))
      
      (define get-choice
        (case-lambda 
         [(message true-choice false-choice)
          (get-choice message true-choice false-choice (string-constant warning))]
         [(message true-choice false-choice title)
          (get-choice message true-choice false-choice title 'disallow-close)]
         [(message true-choice false-choice title default-result)
          (get-choice message true-choice false-choice title default-result #f)]
         [(message true-choice false-choice title default-result parent)
          (letrec ([result default-result]
                   [dialog (make-object 
                               (class100 dialog% ()
                                 (rename [super-on-close on-close]
                                         [super-can-close? can-close?])
                                 (override
                                   [can-close?
                                    (lambda ()
                                      (cond
                                        [(eq? default-result 'disallow-close)
                                         (bell)
                                         (message-box title
                                                      (format (string-constant please-choose-either)
                                                              true-choice false-choice))
                                         #f]
                                        [else
                                         (super-can-close?)]))]
                                   [on-close
                                    (lambda ()
                                      (set! result default-result)
                                      (super-on-close))])
                                 (sequence
                                   (super-init title parent))))]
                   [on-true
                    (lambda args
                      (set! result #t)
                      (send dialog show #f))]
                   [on-false
                    (lambda rags
                      (set! result #f)
                      (send dialog show #f))]
                   [vp (make-object vertical-panel% dialog)]
                   [hp (make-object horizontal-panel% dialog)])
            
            (let loop ([m message])
              (let ([match (regexp-match (format "^([^~n]*)~n(.*)")
                                         m)])
                (if match
                    (begin (make-object message% (cadr match) vp)
                           (loop (caddr match)))
                    (make-object message% m vp))))
            
            (send vp set-alignment 'left 'center)
            (send hp set-alignment 'right 'center)
            (send (make-object button% true-choice hp on-true '(border)) focus)
            (make-object button% false-choice hp on-false)
            (send dialog center 'both)
            (send dialog show #t)
            result)]))
      
      (define text-snip<%> (interface () get-string))
      (define read-snips/chars-from-text
        (letrec ([get-snips
                  (lambda (text start end)
                    (let* ([pos-box (box 0)]
                           [snip (send text find-snip start 'after-or-none pos-box)])
                      (cond
                        [(not snip) null]
                        [(> (+ (unbox pos-box) (send snip get-count)) end) null]
                        [else (cons snip (get-snips text (+ start (send snip get-count)) end))])))])
          (case-lambda
           [(text) (read-snips/chars-from-text text 0)]
           [(text start) (read-snips/chars-from-text text start (send text last-position))]
           [(text start end)
            (define pos-box (box 0))
            (define (get-next)
              (send text split-snip start)
              (send text split-snip end)
              
	  ;; must get all of the snips out of the buffer before reading -- they may change.
              (let loop ([snips (get-snips text start end)])
                
                (cond
                  [(null? snips)
                   (set! get-next (lambda () eof))
                   eof]
                  [(or (is-a? (car snips) string-snip%)
                       (is-a? (car snips) text-snip<%>))
                   (let ([str (if (is-a? (car snips) text-snip<%>)
                                  (send (car snips) get-string)
                                  (send (car snips) get-text 0 (send (car snips) get-count)))])
                     (let string-loop ([n 0])
                       (cond
                         [(< n (string-length str))
                          (set! get-next (lambda () (string-loop (+ n 1))))
                          (string-ref str n)]
                         [else
                          (loop (cdr snips))])))]
                  [else
                   (set! get-next (lambda () (loop (cdr snips))))
                   (car snips)])))
            (let ([read-snips/chars-from-text-thunk
                   (lambda ()
                     (get-next))])
              read-snips/chars-from-text-thunk)])))
      
      (define open-input-buffer
        (lambda (buffer)
          (let ([pos 0])
            (make-input-port
             (lambda ()
               (let ([c (send buffer get-character pos)])
                 (if (char=? c #\null)
                     eof
                     (begin
                       (set! pos (add1 pos))
                       c))))
             (lambda ()
               #t)
             (lambda ()
               (void))))))
      
      (define repeated-keystroke-timeout 300)
      (define alphabetic-list-box%
        (class list-box%
          (init-field choices callback)
          
          (field (chars null)
                 (last-time-stamp #f))
          
          (rename [super-on-subwindow-event on-subwindow-event]
                  [super-on-subwindow-char on-subwindow-char])
          (define/override (on-subwindow-event receiver evt)
            (set! chars null)
            (super-on-subwindow-event receiver evt))
          (define/override (on-subwindow-char receiver evt)
            (let ([code (send evt get-key-code)])
              (when (or (not (char? code))
                        (and last-time-stamp
                             ((- (send evt get-time-stamp) last-time-stamp)
                              . >= .
                              repeated-keystroke-timeout)))
                (set! chars null))
              (set! last-time-stamp (send evt get-time-stamp))
              (cond
                [(and (char? code) (or (char-alphabetic? code) (char-numeric? code)))
                 (set! chars (cons code chars))
                 (scroll-to-matching)
                 (callback this (instantiate control-event% () 
                                  (event-type 'list-box)
                                  (time-stamp (send evt get-time-stamp))))]
                [else
                 (set! chars null)
                 (super-on-subwindow-char receiver evt)])))
          
          ;; scroll-to-matching : -> void
          ;; scrolls the list box to the first string
          ;; that matches the typed chars in `chars'.
          (define (scroll-to-matching)
            (let* ([typed-str (apply string (reverse chars))]
                   [typed-len (string-length typed-str)])
              (let loop ([n 0])
                (when (< n (get-number))
                  (let ([str (get-string n)])
                    (cond
                      [(and ((string-length str) . >= . typed-len)
                            (string=? typed-str (substring str 0 typed-len)))
                       (set-selection n)
                       (make-visible n)]
                      [else (loop (+ n 1))]))))))
          (inherit get-number get-string set-selection)
          
          ;; make-visible : number -> void
          ;; scrolls the list box so that the nth item is visible,
          ;; unless the n'th item is already visible, in which case
          ;; it does nothing.
          (define (make-visible n)
            (set-first-visible-item n))
          (inherit set-first-visible-item)

          (super-instantiate ()
            (choices choices)
            (callback callback)))))))
