(module erepl mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class100.ss")
           (lib "class.ss"))
  
  (provide execute-read-eval-print-loop)
  
  (define grey-style-delta 
    (send (make-object style-delta%) set-delta-foreground "grey"))
  (define bold-blue-style-delta 
    (send (make-object style-delta% 'change-bold) set-delta-foreground "blue"))
  (define italic-red-style-delta 
    (send (make-object style-delta% 'change-italic) set-delta-foreground "red"))
  
  (define (execute-read-eval-print-loop)
  ;; The REPL buffer class
    (define esq:text%
      (class100 text% ()
        (inherit insert last-position get-text erase change-style clear-undos
                 begin-edit-sequence end-edit-sequence lock is-locked?)
        (rename [super-on-char on-char])
        (private-field 
         [prompt-pos 0] 
         [locked? #f])
        (override
          [can-insert? (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
          [can-delete? (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
          [on-char (lambda (c)
                     (super-on-char c)
                     (when (and (memq (send c get-key-code) '(#\return #\newline #\003))
                                (not locked?)
                                (not (is-locked?)))
                       (set! locked? #t)
                       (evaluate (get-text prompt-pos (last-position)))))])
        (public
          [new-prompt (lambda ()
                        (output "> ")
                        (set! prompt-pos (last-position))
                        (set! locked? #f)
                        (clear-undos))]
          [output (lambda (str)
                    (let ([l? locked?])
                      (set! locked? #f)
                      (insert str)
                      (set! locked? l?)))]
          [kill-repl 
           (lambda ()
             (set! locked? #f)
             (lock #f)
             (change-style grey-style-delta 0 (last-position))
             (lock #t))]
          [reset (lambda (prompt?)
                   (set! locked? #f)
                   (lock #f)
                   (begin-edit-sequence)
                   (set! prompt-pos 0)
                   (erase)
                   (let ([s (last-position)]
                         [m (regexp-match "^(.*), (Copyright.*)$" (banner))])
                     (insert (format "Welcome to ~a." (cadr m)))
                     (let ([e (last-position)])
                       (insert #\newline)
                       (change-style bold-blue-style-delta s e))
                     (output (caddr m)))
                   (insert "This is a simple window for evaluating MrEd Scheme expressions.") 
                   (insert #\newline)
                   (let ([s (last-position)])
                     (insert "Quit now and run DrScheme to get a better window.")
                     (let ([e (last-position)])
                       (insert #\newline)
                       (change-style italic-red-style-delta s e)))
                   (insert "The current input port always returns eof.") 
                   (insert #\newline)
                   (when prompt?
                     (new-prompt))
                   (end-edit-sequence)
                   (clear-undos))])
        (sequence 
          (super-init)
          (reset #t))))
    
  ;; GUI creation
    (define frame (make-object (class100 frame% args
                                 (inherit accept-drop-files)
                                 (override
                                   [on-close (lambda () 
                                               (custodian-shutdown-all user-custodian)
                                               (semaphore-post waiting))]
                                   [on-drop-file (lambda (f) (evaluate (format "(load ~s)" f)))])
                                 (sequence 
                                   (apply super-init args) (accept-drop-files #t)))
                    "MrEd eREPL" #f 500 400))
    (define repl-buffer (make-object esq:text%))
    
    (define execute-panel (make-object horizontal-panel% frame))
    (define execute-button 
      (make-object button% "Execute File..." execute-panel
        (lambda (x y) (do-execute))))
    (define reset-button 
      (make-object button% "Reset" execute-panel
        (lambda (x y)
          (do-reset))))
    (define kill-button 
      (make-object button% "Kill" execute-panel
        (lambda (x y)
          (do-kill))))
    
    (define repl-display-canvas (make-object editor-canvas% frame))
    
    (define esq-eventspace (current-eventspace))
    (define (queue-output proc)
      (parameterize ((current-eventspace esq-eventspace))
        (queue-callback proc #f)))
    
  ;; User space initialization
    (define user-custodian 'user-custodian-not-yet-set)
    (define user-namespace 'user-namespace-not-yet-set)
    (define user-output-port
      (make-output-port
       (lambda (s) (queue-output (lambda () (send repl-buffer output s))))
       (lambda () 'nothing-to-close)))
    
    (define user-eventspace 'user-eventspace-not-yet-set)
    
    (define (user-space-init dir)
      (set! user-custodian (make-custodian))
      (set! user-eventspace 
            (parameterize ((current-custodian user-custodian))
              (make-eventspace)))
      
      (set! user-namespace (make-namespace))
      (let ([mred-name ((current-module-name-resolver) '(lib "mred.ss" "mred") #f #f)]
            [orig-namespace (current-namespace)]
            [program (namespace-variable-binding 'program)])
        (parameterize ([current-namespace user-namespace])
          (namespace-variable-binding 'argv #())
          (namespace-variable-binding 'program program)
          (namespace-attach-module orig-namespace mred-name)
          (namespace-require '(lib "mred.ss" "mred"))))
      
      (let ([initial-directory (or dir (current-directory))])
        (parameterize ((current-eventspace user-eventspace))
          (queue-callback
           (lambda ()
             (current-directory initial-directory)
             (current-namespace user-namespace)
             (current-output-port user-output-port)
             (current-error-port user-output-port)
             (current-input-port (make-input-port (lambda () eof) void void)))
           #t))))
    
  ;; Evaluation
    
    (define (evaluate expr-str)
      (parameterize ((current-eventspace user-eventspace))
        (queue-callback
         (lambda ()
           (dynamic-wind
            (lambda () (send execute-button enable #f))
            (lambda () 
              (call-with-values
               (lambda () (eval (read (open-input-string expr-str))))
               (lambda results
                 (for-each 
                  (lambda (v) 
                    (parameterize ([current-output-port user-output-port])
                      (print v) 
                      (newline)))
                  results))))
            (lambda ()
              (queue-output (lambda () (send repl-buffer new-prompt)))
              (send execute-button enable #t)))))))
    
    (define waiting (make-semaphore 0))
    
    (define execute-menu-item 'execute-menu-item-not-yet-set)
    (define execute-filename #f)
    (define (update-execute-label)
      (when execute-button
        (let ([label (if execute-filename
                         (format "Execute ~a" execute-filename)
                         "Execute File...")])
          (send execute-button set-label label)
          (send execute-menu-item set-label label))))
    (define (do-execute)
      (unless execute-filename
        (set! execute-filename (get-file #f frame))
        (when execute-filename
          (update-execute-label)))
      (when execute-filename
        (custodian-shutdown-all user-custodian)
        (let-values ([(base _1 _2) (split-path execute-filename)])
          (user-space-init base))
        (send repl-buffer reset #f)
        (send execute-button enable #f)
        (evaluate (format "(load ~s)" execute-filename))))
    (define (do-reset)
      (custodian-shutdown-all user-custodian)
      (user-space-init #f)
      (send repl-buffer reset #t)
      (send execute-button enable #t))
    (define (do-kill)
      (custodian-shutdown-all user-custodian)
      (send execute-button enable #t)
      (send repl-buffer kill-repl))
    
    (send execute-panel stretchable-height #f)
    (when execute-button
      (send execute-button stretchable-width #t))
    
    (let ([mb (make-object menu-bar% frame)])
      (let ([m (make-object menu% "&File" mb)])
        (make-object menu-item% "Load File..." m
          (lambda (i e) (let ([f (get-file #f frame)]) (and f (evaluate (format "(load ~s)" f))))))
        (make-object menu-item% 
          (if (eq? (system-type) 'windows)
              "E&xit"
              "&Quit")
          m (lambda (i e) (send frame on-close) (send frame show #f)) #\q))
      (let ([m (make-object menu% "&Edit" mb)])
        (append-editor-operation-menu-items m #f))
      (let ([m (make-object menu% "&Scheme" mb)])
        (set! execute-menu-item
              (make-object menu-item% "Execute" m
                (lambda (i e)
                  (do-execute))
                #\t))
        (make-object menu-item% "Reset" m
          (lambda (i e)
            (do-reset))
          #\r)
        (make-object menu-item% "Kill" m
          (lambda (i e)
            (do-kill))
          #\k)
        (make-object menu-item% "Reset Execute Button" m
          (lambda (i e) 
            (set! execute-filename #f)
            (update-execute-label)))))
    
    (update-execute-label)
    
  ;; Just a few extra key bindings:
    ((current-text-keymap-initializer) (send repl-buffer get-keymap))
    (send repl-buffer auto-wrap #t)
    
  ;; Go
    (user-space-init #f)
    
    (send repl-display-canvas set-editor repl-buffer)
    
    (send frame show #t)
    
    (send repl-display-canvas focus)
    
    (yield waiting)))
