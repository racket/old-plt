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
      (define invalid-tokens-start #f)

      ;; The position of the next token to be read
      (define current-pos start-pos)
      
      (define get-token #f)
      
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
      ;; A channel for communication between the background tokenizer and the foreground
      ;; thread.
      (define sync (make-channel))
      (define background-thread #f)
      ;; The input port tokens are read from.  Any change to the text% invalidates this port.
      (define in #f)
      (define input-port-start-pos start-pos)

      
      (inherit get-prompt-position
               change-style begin-edit-sequence end-edit-sequence
               get-surrogate set-surrogate get-style-list)
            
      (define (reset)
        (set! tokens #f)
        (set! invalid-tokens #f)
        (set! invalid-tokens-start #f)
        (set! current-pos start-pos)
        (set! colors null)
        (set! in #f)
        (set! input-port-start-pos start-pos))
      
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
      
      (define (re-tokenize)
        (let-values (((type data new-token-start new-token-end) (get-token in)))
          (let ((old-breaks (break-enabled)))
            (break-enabled #f)
            (cond
              ((not (eq? 'eof type))
               (let ((len (- new-token-end new-token-start)))
                 (set! current-pos (+ len current-pos))
                 (sync-invalid)
                 (set! colors (cons
                               (lambda ()
                                 (change-style
                                  (preferences:get (string->symbol (format "syntax-coloring:~a:~a"
                                                                           prefix
                                                                           type)))
                                  (sub1 (+ input-port-start-pos new-token-start))
                                  (sub1 (+ input-port-start-pos new-token-end))
                                  #f))
                               colors))
                 (set! tokens (insert-after! tokens (make-node len data 0 #f #f)))
                 (cond
                   ((and invalid-tokens (= invalid-tokens-start current-pos))
                    (set! tokens (insert-after! tokens (search-min! invalid-tokens null)))
                    (set! invalid-tokens #f)
                    (set! invalid-tokens-start #f)
                    (break-enabled old-breaks))
                  (else
                   (break-enabled old-breaks)
                   (re-tokenize)))))
              (else
               (break-enabled old-breaks))))))
    
      (define/public (do-insert/delete edit-start-pos change-length)
        (when should-color?
          (when (> edit-start-pos start-pos)
            (set! edit-start-pos (sub1 edit-start-pos)))
          (let-values (((orig-token-start orig-token-end valid-tree invalid-tree)
                        (split tokens (- edit-start-pos start-pos))))
            (set! invalid-tokens invalid-tree)
            (set! tokens valid-tree)
            (set! invalid-tokens-start (+ orig-token-end change-length))
            (set! current-pos (+ start-pos orig-token-start))
            (set! input-port-start-pos (+ start-pos orig-token-start))
            (set! in (open-input-text-editor
                      this
                      input-port-start-pos
                      end-pos)))
          (colorer-callback)))
      
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
          (set! background-thread (thread (lambda () (background-colorer)))))
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
        (channel-put sync #f)
        (sleep .1)
        (break-thread background-thread)
        (begin-edit-sequence #f)
        (color)
        (end-edit-sequence)
        (queue-callback colorer-callback))
                
      (define (background-colorer)
        (channel-get sync)
        (parameterize ((current-exception-handler
                        (lambda (exn)
                          (channel-get sync)
                          ((exn:break-continuation exn)))))
          (with-handlers ((not-break-exn? void))
            (re-tokenize)))
        (background-colorer))
      
      
;      (define (colorer-callback)
;        (channel-put sync #f)
;        (channel-get sync)
;        (begin-edit-sequence #f)
;        (color)
;        (end-edit-sequence))
;      
;      
;      (define (background-colorer)
;        (channel-get sync)
;        (with-handlers ((void void))
;          (re-tokenize))
;        (channel-put sync #f)
;        (background-colorer))
  
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
      
      (super-instantiate ())
      ))
  )