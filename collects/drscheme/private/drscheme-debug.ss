
(module drscheme-debug mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss"))
  
  (let ([main-eventspace-thread (current-thread)]
        [main-eventspace (current-eventspace)])
    (let-values ([(sw sh) (get-display-size)])
      (let* ([stack-frame (parameterize ([current-eventspace (make-eventspace)])
                            (make-object frame% "Debugging Window" #f 500))]
             [button-panel (make-object horizontal-panel% stack-frame)]
             [messages-panel (make-object vertical-panel% stack-frame)])
        
        (letrec ([profiling-record-enabled (dynamic-require '(lib "errortrace.ss" "errortrace") 'profiling-record-enabled)]
                 [mem (make-object message% "000.000.000" button-panel)]
                 [c (make-object canvas% button-panel)]
                 [quit-button (make-object button% "Quit" button-panel (lambda (x y) (exit)))]
                 [break-button (make-object button% "Break" button-panel 
                                 (lambda (x y) (break-thread main-eventspace-thread)))]
                 [profile-button (make-object button% "Profile Results" button-panel
                                   (lambda (x y)
                                     (show-profiling-results)))]
                 [toggle-profile-button
                  (make-object button% "Disable Profiling" button-panel
                    (lambda (x y)
                      (toggle-profiling-enabled)))]                  
                 
                 [toggle-profiling-enabled
                  (lambda ()
                    (send toggle-profile-button set-label "Toggling...")
                    (send toggle-profile-button enable #f)
                    (parameterize ([current-eventspace main-eventspace])
                      (queue-callback
                       (lambda ()
			 (profiling-record-enabled
			  (not (profiling-record-enabled)))
                         (sync-profiling-state/main-eventspace)
                         (send toggle-profile-button enable #t)))))]
                 [sync-profiling-state/main-eventspace
                  (lambda ()
                    (let ([profiling-enabled? (profiling-record-enabled)])
                      (send toggle-profile-button set-label (if profiling-enabled? 
                                                                "Disable Profiling"
                                                                "Enable Profiling"))))]
                 
                 
                 [onb (make-object bitmap% (build-path (collection-path "icons")
                                                       "recycle.gif"))]
                 [offb
                  (let ([bdc (make-object bitmap-dc%)]
                        [bitmap (make-object bitmap%
                                  (send onb get-width)
                                  (send onb get-height))])
                    (send bdc set-bitmap bitmap)
                    (send bdc clear)
                    (send bdc set-bitmap #f)
                    bitmap)])
          
          (thread
           (lambda ()
             (let loop ()
               (sleep 1)
               (let* ([mem-usage (current-memory-use)]
                      [spacer 1000]
                      [pad (lambda (x)
                             (cond
                               [(x . < . 10) (format "00~a" x)]
                               [(x . < . 100) (format "0~a" x)]
                               [else (number->string x)]))]
                      [f1 (pad (modulo mem-usage spacer))]
                      [f2 (pad (modulo (quotient mem-usage spacer) spacer))]
                      [f3 (pad (quotient (quotient mem-usage spacer) spacer))])
                 (send mem set-label (string-append f3 "." f2 "." f1)))
               (loop))))
          
          (register-collecting-blit c 0 0 (send onb get-width) (send onb get-height) onb offb)
          (send c min-width (send onb get-width))
          (send c min-height (send onb get-height))
          (send c stretchable-width #f)
          (send c stretchable-height #f)
          (send button-panel set-alignment 'center 'center)
          (unless ((dynamic-require '(lib "errortrace.ss" "errortrace") 'profiling-enabled))
            (send profile-button enable #f)
            (send toggle-profile-button enable #f))
          (send profile-button stretchable-height #t)
          (send toggle-profile-button stretchable-height #t)
          (send quit-button stretchable-height #t)
          (send break-button stretchable-height #t)
          
          (let* ([new-message
                  (lambda ()
                    (let ([m (make-object message% "" messages-panel)])
                      (send m stretchable-width #t)
                      m))]
                 [messages (list (new-message)
                                 (new-message)
                                 (new-message)
                                 (new-message)
                                 (new-message)
                                 (new-message)
                                 (new-message)
                                 (new-message))]
                 [stack null]
                 [ol (current-load)]
                 [update-messages
                  (lambda ()
                    (let ([new-messages-needed (- (length stack) (length messages))])
                      (when (new-messages-needed . > . 0)
                        (let loop ([n new-messages-needed])
                          (unless (zero? n)
                            (let ([m (new-message)])
                              (set! messages (append messages (list m)))
                              (loop (- n 1)))))))
                    (let loop ([stack (reverse stack)]
                               [messages messages])
                      (cond
                        [(null? messages) (void)]
                        [(null? stack) 
                         (for-each (lambda (m)
                                     (unless (equal? "" (send m get-label))
                                       (send m set-label "")))
                                   messages)]
                        [else
                         (let ([msg (car messages)]
                               [fn (car stack)])
                           (unless (string=? (send msg get-label) fn)
                             (send msg set-label fn)))
                         (loop (cdr stack) (cdr messages))])))])
            (send stack-frame reflow-container)
            (send stack-frame move 
                  (- sw (send stack-frame get-width))
                  0)
            
            
            (sync-profiling-state/main-eventspace)
            
            (send stack-frame show #t)
            (current-load
             (lambda (fn module)
               (set! stack (cons fn stack))
               (update-messages)
               (begin0 (ol fn module)
                       (set! stack (cdr stack))
                       (update-messages))))
            
            (error-print-width 600))))))

  (define (show-profiling-results)
    (define f (make-object frame% "Profile Results" #f 400 600))
    (define mb (new menu-bar% (parent f)))
    (define em (new menu% (label "Edit") (parent mb)))
    (define copy (new menu-item%
		      (label "Copy")
		      (shortcut #\c)
		      (parent em)
		      (callback
		       (lambda (x y)
			 (send t copy)))))
    (define t (make-object text%))
    (define ec (make-object editor-canvas% f t))
    (define p (open-output-string))
    
    (parameterize ([current-output-port p])
      ((dynamic-require '(lib "errortrace.ss" "errortrace") 'output-profile-results) #f #t)
      (newline)
      (newline)
      ((dynamic-require '(lib "errortrace.ss" "errortrace") 'output-profile-results) #f #f))
    (send t insert (get-output-string p))
    (with-handlers ([not-break-exn?
		     (lambda (x)
		       (send t insert "\ndidn't save transcript in /home/robby/OUTPUT\n  ")
		       (send t insert (if (exn? x)
					  (format "~a" (exn-message x))
					  (format "~s" x))))])

      (call-with-output-file "/home/robby/OUTPUT"
	(lambda (op) (display (get-output-string p) op))
	'truncate
	'text)
      (send t insert "\nsaved transcript in /home/robby/OUTPUT\n"))

    (send t change-style (make-object style-delta% 'change-family 'modern) 0 (send t last-position))
    (send t lock #t)
    (send f show #t))
  
  ;; continue with regular startup
  (dynamic-require '(lib "drscheme-normal.ss" "drscheme" "private") #f))