(module tokenized-text mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "token-tree.ss"
           "paren-tree.ss")
  
  (provide tokenized-text-mixin tokenized-interactions-text-mixin)
  
  (define-local-member-name start-pos end-pos reset-tokens)
  
  (define (tokenized-text-mixin %)
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
      
      (define stopped? #t)
      
      ;; ---------------------- Parnethesis matching --------------------------
      
      (define parens (new paren-tree% (matches '(|(| |)|))))
      
      ;; ---------------------- Interactions state ----------------------------
      ;; The positions to start and end the coloring at.
      (field (start-pos 0) (end-pos 'end))
      
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
      
      (inherit change-style begin-edit-sequence end-edit-sequence
               get-style-list in-edit-sequence?)
      
      (define/public (reset-tokens)
        (set! tokens #f)
        (set! invalid-tokens #f)
        (set! invalid-tokens-start +inf.0)
        (set! up-to-date? #t)
        (set! parens (new paren-tree% (matches '(|(| |)|))))
        (set! current-pos start-pos)
        (set! colors null)
        (modify))
      
      (define (modify)
        (when background-thread
          (break-thread background-thread)))
      
      (define (color)
        (unless (null? colors)
          ((car colors))
          (set! colors (cdr colors))
          (color)))
      
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
          ;; breaks must be disabled before the semaphore wait so we can't be
          ;; broken out of the critical section
          (break-enabled #f)
          ;; If a break occurs while we are suspended, the break will occur
          ;; and the critical section will not be entered
          (semaphore-wait/enable-break lock)
          (unless (eq? 'eof type)
            (let ((len (- new-token-end new-token-start)))
              (set! current-pos (+ len current-pos))
              (sync-invalid)
              (when (and should-color? (not (eq? 'white-space type)))
                (set! colors (cons
                              (lambda ()
                                (change-style
                                 (preferences:get (string->symbol (format "syntax-coloring:~a:~a"
                                                                          prefix
                                                                          type)))
                                 (sub1 (+ in-start-pos new-token-start))
                                 (sub1 (+ in-start-pos new-token-end))
                                 #f))
                              colors)))
              (set! tokens (insert-after! tokens (make-node len data 0 #f #f)))
              (send parens add-token data (- current-pos start-pos len) len)
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
        (unless stopped?
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

      (define (colorer-callback)
	(unless (in-edit-sequence?)
	  (thread-resume background-thread)
	  (sleep .01)    ;; This is when the background thread is working.
	  (semaphore-wait lock)
	  (thread-suspend background-thread)
	  (semaphore-post lock)
	  (begin-edit-sequence #f #f)
	  (color)
	  (end-edit-sequence))
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
            (with-handlers ((not-break-exn?
                             (lambda (exn)
                               (printf "~a~n" exn)
                               (break-enabled #f)
                               (semaphore-wait lock))))
              (re-tokenize (open-input-text-editor this current-pos end-pos)
                           current-pos))
            ;; Breaks should be disabled from exit of re-tokenize
            ;; lock will be held
            (set! up-to-date? #t)
            (send parens print)
            (semaphore-post lock)
            (thread-suspend (current-thread))))
        (background-colorer))
      
      (define/public (start prefix- get-token-)
        (set! stopped? #f)
        (reset-tokens)
        (set! prefix prefix-)
        (set! get-token get-token-)
        (unless remove-prefs-callback-thunk
          (set! remove-prefs-callback-thunk
                (preferences:add-callback
                 (string->symbol (format "syntax-coloring:~a:active" prefix))
                 (lambda (_ on?)
                   (set! should-color? on?)
                   (cond
                     (on?
                      (reset-tokens)
                      (do-insert/delete start-pos 0))
                     (else (change-style (send (get-style-list) find-named-style "Standard")
                                         start-pos end-pos #f)))))))
        (unless background-thread
          (break-enabled #f)
          (set! background-thread (thread (lambda () (background-colorer-entry))))
          (break-enabled #t))
        (do-insert/delete start-pos 0))
      
      (define/public (stop)
        (set! stopped? #t)
        (when remove-prefs-callback-thunk
          (remove-prefs-callback-thunk)
          (set! remove-prefs-callback-thunk #f))
        (change-style (send (get-style-list) find-named-style "Standard")
                      start-pos end-pos #f)
        (reset-tokens)
        (set! prefix #f)
        (set! get-token #f))
      
      (rename (super-on-change on-change))
      (define/override (on-change)
        (super-on-change)
        (modify))
      
      (rename (super-after-set-position after-set-position))
      (define/override (after-set-position)
        (super-after-set-position)
        (modify))
      
      (rename (super-on-change-style on-change-style))
      (define/override (on-change-style a b)
        (super-on-change-style a b)
        (modify))

      (rename (super-on-set-size-constraint on-set-size-constraint))
      (define/override (on-set-size-constraint)
        (super-on-set-size-constraint)
        (modify))

      (rename (super-after-insert after-insert))
      (define/override (after-insert edit-start-pos change-length)
        (super-after-insert edit-start-pos change-length)
        (do-insert/delete edit-start-pos change-length))
      
      (rename (super-after-delete after-delete))
      (define/override (after-delete edit-start-pos change-length)
        (super-after-delete edit-start-pos change-length)
        (do-insert/delete edit-start-pos (- change-length)))
  
      (super-instantiate ())))
  
  
  (define (tokenized-interactions-text-mixin %)
    (let ((% (tokenized-text-mixin %)))
      (let ((set-start-pos! (class-field-mutator % start-pos))
            (set-end-pos! (class-field-mutator % end-pos)))
        (class %
          
          (rename (super-do-eval do-eval)
                  (super-insert-prompt insert-prompt)
                  (super-initialize-console initialize-console)
                  (super-reset-console reset-console))
          
          (inherit reset-tokens get-prompt-position)
          
          (define/override (do-eval start end)
            (super-do-eval start end)
            (set-end-pos! this end))
          
          (define/override (insert-prompt)
            (super-insert-prompt)
            (set-end-pos! this 'end)
            (set-start-pos! this (get-prompt-position))
            (reset-tokens))
          
          (define/override (initialize-console)
            (super-initialize-console)
            (set-start-pos! this 0)
            (set-end-pos! this 'end)
            (reset-tokens))
          
          (define/override (reset-console)
            (super-reset-console)
            (set-start-pos! this 0)
            (set-end-pos! this 'end)
            (reset-tokens))
          (super-instantiate ())))))
  )
