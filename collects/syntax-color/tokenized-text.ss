(module tokenized-text mzscheme
  (require (lib "class.ss")
           (lib "etc.ss")
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
      (define tokens (new token-tree%))
      
      ;; The tree of tokens that have been invalidated by an edit
      ;; but might still be valid.
      (define invalid-tokens (new token-tree%))
      
      ;; The position right before the invalid-tokens tree
      (define invalid-tokens-start +inf.0)
      
      ;; The position right before the next token to be read
      (define current-pos start-pos)
      
      ;; The lexer
      (define get-token #f)

      ;; If the tree is completed
      (define up-to-date? #t)
      
      (define stopped? #t)
      
      ;; ---------------------- Parnethesis matching --------------------------
      
      (define pairs '((|(| |)|)
                      (|[| |]|)
                      (|{| |}|)))
      (define parens (new paren-tree% (matches pairs)))

      
      ;; ---------------------- Interactions state ----------------------------
      ;; The positions right before and right after the area to be tokenized
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
      
      (inherit change-style begin-edit-sequence end-edit-sequence highlight-range
               get-style-list in-edit-sequence? get-start-position get-end-position
               local-edit-sequence? get-styles-fixed has-focus?)
      (define/public (reset-tokens)
        (send tokens reset-tree)
        (send invalid-tokens reset-tree)
        (set! invalid-tokens-start +inf.0)
        (set! up-to-date? #t)
        (set! parens (new paren-tree% (matches pairs)))
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
        (when (and (not (send invalid-tokens is-empty?))
                   (< invalid-tokens-start current-pos))
          (send invalid-tokens search-min!)
          (let ((length (send invalid-tokens get-root-length)))
            (send invalid-tokens remove-root!)
            (set! invalid-tokens-start (+ invalid-tokens-start length)))
          (sync-invalid)))
      
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
                                 (+ in-start-pos (sub1 new-token-start))
                                 (+ in-start-pos (sub1 new-token-end))
                                 #f))
                              colors)))
              (insert-last! tokens (new token-tree% (length len) (data data)))
              (send parens add-token data len)
              (cond
                ((and (not (send invalid-tokens is-empty?))
                      (= invalid-tokens-start current-pos))
                 (send invalid-tokens search-max!)
                 (send parens merge-tree (send invalid-tokens get-root-end-position))
                 (insert-last! tokens invalid-tokens)
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
             (send tokens search! (- edit-start-pos start-pos))
             (let-values (((orig-token-start orig-token-end valid-tree invalid-tree)
                           (send tokens split)))
               (send parens split-tree orig-token-start)
               (set! invalid-tokens invalid-tree)
               (set! tokens valid-tree)
               (set! invalid-tokens-start (+ start-pos orig-token-end change-length))
               (set! current-pos (+ start-pos orig-token-start))
               (set! up-to-date? #f)
               (colorer-callback)))
            ((>= edit-start-pos invalid-tokens-start)
             (printf "here1~n")
             (send invalid-tokens search! (- edit-start-pos invalid-tokens-start))
             (let-values (((tok-start tok-end valid-tree invalid-tree)
                           (send invalid-tokens split)))
               (set! invalid-tokens invalid-tree)
               (set! invalid-tokens-start (+ invalid-tokens-start tok-end change-length))))
            ((>= edit-start-pos current-pos)
             (printf "here2~n")
             (set! invalid-tokens-start (+ change-length invalid-tokens-start)))
            (else
             (printf "here3~n") 
             (send tokens search! (- edit-start-pos start-pos))
             (let-values (((tok-start tok-end valid-tree invalid-tree)
                           (send tokens split)))
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
        (match-parens #t)
        (reset-tokens)
        (set! prefix #f)
        (set! get-token #f))

      
      ;; ----------------------- Match parentheses ----------------------------
      
      (define clear-old-locations 'dummy)
      (set! clear-old-locations void)

      (define mismatch-color (make-object color% "PINK"))
      (define (get-match-color) (preferences:get 'framework:paren-match-color))

      (define (highlight start end caret-pos error?)
        (let ([off (highlight-range start end
                                    (if error? mismatch-color (get-match-color))
                                    (and (send (icon:get-paren-highlight-bitmap) ok?)
                                         (icon:get-paren-highlight-bitmap))
                                    (= caret-pos start))])
          (set! clear-old-locations
                (let ([old clear-old-locations])
                  (lambda ()
                    (old)
                    (off))))))
      
      (define in-match-parens? #f)
      (define match-parens
        (opt-lambda ([just-clear? #f])
          (unless in-match-parens?
            (set! in-match-parens? #t)
            (begin-edit-sequence #f #f)
            (clear-old-locations)
            (set! clear-old-locations void)
            (when (preferences:get 'framework:highlight-parens)
              (unless just-clear?
                (let* ((here (get-start-position)))
                  (when (= here (get-end-position))
                    (let-values (((start-f end-f error-f) (send parens match-forward here))
                                 ((start-b end-b error-b) (send parens match-backward here)))
                      (when (and start-f end-f)
                        (highlight start-f end-f here error-f))
                      (when (and start-b end-b)
                        (highlight start-b end-b here error-b)))))))
            (end-edit-sequence)
            (set! in-match-parens? #f))))
      
      
      (define/override highlight-parens
        (lambda x (void)))
      
      ;; ------------------------- Callbacks to Override ----------------------
      
      (rename (super-on-focus on-focus))
      (define/override (on-focus on?)
        (super-on-focus on?)
        (match-parens (not on?)))
      
      (rename (super-on-change on-change))
      (define/override (on-change)
        (super-on-change)
        (modify))

      (rename (super-after-edit-sequence after-edit-sequence))
      (define/override (after-edit-sequence)
        (super-after-edit-sequence)
        (when (has-focus?)
          (match-parens)))
          
      (rename (super-after-set-position after-set-position))
      (define/override (after-set-position)
        (super-after-set-position)
        (unless (local-edit-sequence?)
          (when (has-focus?)
            (match-parens)))
        (modify))

      (rename (super-after-change-style after-change-style))
      (define/override (after-change-style a b)
        (super-after-change-style a b)
        (unless (local-edit-sequence?)
          (unless (get-styles-fixed)
            (when (has-focus?)
              (match-parens))))
        (modify))

      (rename (super-on-set-size-constraint on-set-size-constraint))
      (define/override (on-set-size-constraint)
        (super-on-set-size-constraint)
        (unless (local-edit-sequence?)
          (when (has-focus?)
            (match-parens)))
        (modify))

      (rename (super-after-insert after-insert))
      (define/override (after-insert edit-start-pos change-length)
        (super-after-insert edit-start-pos change-length)
        ;(unless (local-edit-sequence?)
        ;  (when (has-focus?)
        ;    (match-parens)))
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
