(module colorer mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "token-tree.ss")
  
  (provide colorer text-mixin)

  (define (text-mixin %)
    (class %

      ;; ---------------------- Lexing state ----------------------------------
      
      ;; The tree of valid tokens, starting at start-pos
      (define tokens #f)
      
      ;; The tree of tokens that have been invalidated by an edit
      ;; but might still be valid.
      (define invalid-tokens #f)

      ;; The starting position of the invalid-tokens tree
      (define invalid-tokens-start +inf.0)

      ;; The position of the next token to be read
      (define current-pos start-pos)
      
      ;; The lexer
      (define get-token #f)

      ;; If the tree is completed
      (define up-to-date? #t)
      
      ;; ---------------------- Interactions state ----------------------------
      ;; The position to start the coloring at.
      (define start-pos 0)
      ;; The position to stop coloring at.
      (define end-pos 'end)
      
      ;; ---------------------- Preferences -----------------------------------
      (define should-color? #t)
      (define remove-prefs-callback-thunk #f)
      (define prefix #f)
      
      ;; ---------------------- Multi-threading -------------------------------
      ;; A list of thunks that color the buffer
      (define colors null)
      ;; The thread handle to the background colorer
      (define background-thread #f)
      ;; Prevent the background thread from being put to sleep while modifying
      ;; global state
      (define lock (make-semaphore 1))
      
      (inherit get-prompt-position
               change-style begin-edit-sequence end-edit-sequence
               get-surrogate set-surrogate get-style-list)
            
      (define (reset)
        (set! tokens #f)
        (set! invalid-tokens #f)
        (set! invalid-tokens-start +inf.0)
        (set! up-to-date? #t)
        (set! current-pos start-pos)
        (set! colors null))
      
      (define/public (modify)
        (when background-thread
          (break-thread background-thread)))
      
      
      (define (color)
        (unless (null? colors)
          ((car colors))
          (set! colors (cdr colors))
          (color)))
      
      (rename (super-do-eval do-eval)
              (super-insert-prompt insert-prompt)
              (super-initialize-console initialize-console)
              (super-reset-console reset-console))
      (define/override (do-eval start end)
        (set! end-pos end)
        (super-do-eval start end))

      (define/override (insert-prompt)
        (super-insert-prompt)
        (reset)
        (set! end-pos 'end)
        (set! start-pos (get-prompt-position)))
      
      (define/override (initialize-console)
        (reset)
        (set! start-pos 0)
        (set! end-pos 0)
        (super-initialize-console))
     
      (define/override (reset-console)
        (reset)
        (set! start-pos 0)
        (set! end-pos 0)
        (super-reset-console))
      
      (define (sync-invalid)
        (when (and invalid-tokens (< invalid-tokens-start current-pos))
          (let ((min-tree (search-min! invalid-tokens null)))
            (set! invalid-tokens (node-right min-tree))
            (set! invalid-tokens-start (+ invalid-tokens-start
                                          (node-token-length min-tree)))
            (sync-invalid))))
      
      ;; re-tokenize should be called with breaks enabled and exit with breaks disabled
      ;; re-tokenize should be called when lock is not held.  When it exits, the lock
      ;;   will be held.
      (define (re-tokenize in in-start-pos)
        (let-values (((type data new-token-start new-token-end) (get-token in)))
          (break-enabled #f)
          (semaphore-wait lock)
          (unless (eq? 'eof type)
            (let ((len (- new-token-end new-token-start)))
              (set! current-pos (+ len current-pos))
              (sync-invalid)
              (set! colors (cons
                            (lambda ()
                              (change-style
                               (preferences:get (string->symbol (format "syntax-coloring:~a:~a"
                                                                        prefix
                                                                        type)))
                               (sub1 (+ in-start-pos new-token-start))
                               (sub1 (+ in-start-pos new-token-end))
                               #f))
                            colors))
              (set! tokens (insert-after! tokens (make-node len data 0 #f #f)))
              (cond
                ((and invalid-tokens (= invalid-tokens-start current-pos))
                 (set! tokens (insert-after! tokens (search-min! invalid-tokens null)))
                 (set! invalid-tokens #f)
                 (set! invalid-tokens-start +inf.0))
                (else
                 (semaphore-post lock)
                 (break-enabled #t)
                 (re-tokenize in in-start-pos)))))))
      
      (define/public (do-insert/delete edit-start-pos change-length)
        (when should-color?
          (when (> edit-start-pos start-pos)
            (set! edit-start-pos (sub1 edit-start-pos)))
          (modify)
          (cond
            (up-to-date?
             (let-values (((orig-token-start orig-token-end valid-tree invalid-tree)
                           (split tokens (- edit-start-pos start-pos))))
               (set! invalid-tokens invalid-tree)
               (set! tokens valid-tree)
               (set! invalid-tokens-start 
                     (cond
                       (invalid-tree (+ start-pos orig-token-end change-length))
                       (else +inf.0)))
               (set! current-pos (+ start-pos orig-token-start))
               (set! up-to-date? #f)
               (colorer-callback)))
            ((>= edit-start-pos invalid-tokens-start)
             (let-values (((tok-start tok-end valid-tree invalid-tree)
                           (split invalid-tokens (- edit-start-pos invalid-tokens-start))))
               (set! invalid-tokens invalid-tree)
               (set! invalid-tokens-start (+ start-pos tok-end change-length))))
            ((>= edit-start-pos current-pos)
             (set! invalid-tokens-start (+ change-length invalid-tokens-start)))
            (else
             (let-values (((tok-start tok-end valid-tree invalid-tree)
                           (split tokens (- edit-start-pos start-pos))))
               (set! tokens valid-tree)
               (set! invalid-tokens-start (+ change-length invalid-tokens-start))
               (set! current-pos (+ start-pos tok-start)))))))
                   
      
      (define/public (start prefix- get-token-)
        (reset)
        (set! prefix prefix-)
        (set! get-token get-token-)
        (unless remove-prefs-callback-thunk
          (set! remove-prefs-callback-thunk
                (preferences:add-callback
                 (string->symbol (format "syntax-coloring:~a:active" prefix))
                 (lambda (_ on?)
                   (set! should-color? on?)
                   (set-surrogate (get-surrogate))))))
        (unless background-thread
          (break-enabled #f)
          (set! background-thread (thread (lambda () (background-colorer-entry))))
          (break-enabled #t))
        (do-insert/delete start-pos 0))
        
        
      (define/public (stop)
        (when remove-prefs-callback-thunk
          (remove-prefs-callback-thunk)
          (set! remove-prefs-callback-thunk #f))
        (change-style (send (get-style-list) find-named-style "Standard")
                      start-pos end-pos #f)
        (reset)
        (set! prefix #f)
        (set! get-token #f))
      
      (define (colorer-callback)
        (thread-resume background-thread)
        (sleep .01)    ;; This is when the background thread is working.
        (semaphore-wait lock)
        (thread-suspend background-thread)
        (semaphore-post lock)
        (begin-edit-sequence #f)
        (color)
        (end-edit-sequence)
        (unless up-to-date?
          (queue-callback colorer-callback #f)))
      
      
      ;; Breaks should be disabled on entry
      (define (background-colorer-entry)
        (thread-suspend (current-thread))
        (background-colorer))
      
      ;; Breaks should be disabled on entry
      (define (background-colorer)
        (let/ec restart
          (parameterize ((current-exception-handler
                          (lambda (exn)
                            ;; Lock is not held here because breaks are disabled
                            ;; whenever lock is held
                            (break-enabled #f)
                            (restart))))
            (break-enabled #t)
            (with-handlers ((not-break-exn? void))
              (re-tokenize (open-input-text-editor this current-pos end-pos)
                           current-pos))
            ;; Breaks should be disabled from exit of re-tokenize
            ;; lock will be held
            (set! up-to-date? #t)
            (semaphore-post lock)
            (thread-suspend (current-thread))))
        (background-colorer))
      
      (super-instantiate ())))
  
  
  (define (colorer %)
    (class %
      ;; get-token takes an input port and returns 4 values:
      ;; A symbol in `(keyword string literal comment error identifier default)
      ;; Data to be kept with the token
      ;; The token's starting offset
      ;; The token's ending offset
      (init-field get-token prefix port-wrapper)
            
      (rename (super-on-disable-surrogate on-disable-surrogate))
      (define/override (on-disable-surrogate text)
        (super-on-disable-surrogate text)
        (send text stop))
      
      (rename (super-on-enable-surrogate on-enable-surrogate))
      (define/override (on-enable-surrogate text)
        (super-on-enable-surrogate text)
        (send text start prefix get-token))
      
      (rename (super-after-insert after-insert))
      (define/override (after-insert text _ edit-start-pos change-length)
        (super-after-insert text _ edit-start-pos change-length)
        (send text do-insert/delete edit-start-pos change-length))
      
      (rename (super-after-delete after-delete))
      (define/override (after-delete text _ edit-start-pos change-length)
        (super-after-delete text _ edit-start-pos change-length)
        (send text do-insert/delete edit-start-pos (- change-length)))
      
      (wrap on-change)
      (wrap after-set-position)
      (wrap on-change-style a b)
;    after-edit-sequence 
;    on-char 
;    on-default-char 
;    on-default-event 
;    on-disable-surrogate 
;    on-display-size 
;    on-edit-sequence 
;    on-enable-surrogate 
;    on-event 
;    on-focus 
;    on-load-file 
;    on-local-char 
;    on-local-event 
;    on-new-box 
;    on-new-image-snip 
;    on-new-string-snip 
;    on-new-tab-snip 
;    on-paint 
;    on-save-file 
;    on-set-size-constraint 
;    on-snip-modified 
      
      (super-instantiate ())
      ))
  
  (define-syntax wrap
    (syntax-rules ()
      ((_ name args ...)
       (begin
         (rename (x name))
         (define/override (name text _ args ...)
           (send text modify)
           (x text _ args ...))))))

  )