(module colorer mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "token-tree.ss")
  
  (provide colorer text-mixin)

  (define (text-mixin %)
    (class %
      
      ;; The tree of valid tokens, starting at (get-prompt-position)
      (define tokens #f)
      
      ;; The tree of tokens that have been invalidated by an edit
      ;; but might still be valid.
      (define invalid-tokens #f)

      ;; The starting position of the invalid-tokens tree
      (define invalid-tokens-start #f)

      ;; A list of thunks that color the buffer
      (define colors null)

      ;; The last known (get-prompt-position)
      (define last-prompt-position (get-prompt-position))

      ;; A channel for communication between the background tokenizer and the foreground
      ;; thread.
      (define sync (make-channel))
      
      (define background-thread #f)
      
      (define should-color? #t)
      
      (define remove-prefs-callback-thunk #f)
      
      (inherit get-prompt-position change-style begin-edit-sequence end-edit-sequence
               get-surrogate set-surrogate get-style-list)
      
      (define (reset)
        (set! tokens #f)
        (set! invalid-tokens #f)
        (set! invalid-tokens-start #f)
        (set! colors null)
        (set! last-prompt-position (get-prompt-position)))
      
      (define (color)
        (unless (null? colors)
          ((car colors))
          (set! colors (cdr colors))
          (color)))
      
      (define (re-tokenize prefix get-token in input-start-pos current-pos)
        (cond
          ((and invalid-tokens (= invalid-tokens-start current-pos))
           (set! tokens (insert-after! tokens (search-min! invalid-tokens null))))
          (else
           (let-values (((type data new-token-start new-token-end) (get-token in)))
             (unless (eq? 'eof type)
               (let ((len (- new-token-end new-token-start)))
                 (let loop ()
                   (cond
                     ((and invalid-tokens (< invalid-tokens-start current-pos))
                      (let ((min-tree (search-min! invalid-tokens null)))
                        (set! invalid-tokens (node-right min-tree))
                        (set! invalid-tokens-start (+ invalid-tokens-start
                                                       (node-token-length min-tree)))
                        (loop)))
                     (else
                      (set! colors (cons
                                    (lambda ()
                                      (change-style
                                       (preferences:get (string->symbol (format "syntax-coloring:~a:~a"
                                                                                prefix
                                                                                type)))
                                       (sub1 (+ input-start-pos new-token-start))
                                       (sub1 (+ input-start-pos new-token-end))
                                       #f))
                                    colors))
                      (set! tokens (insert-after! tokens (make-node len data 0 #f #f)))
                      (re-tokenize prefix get-token in input-start-pos (+ current-pos len)))))))))))
    
      (define/public (do-insert/delete prefix get-token edit-start-pos change-length)
        (when should-color?
          (let ((buffer-start (get-prompt-position)))
            (unless (= last-prompt-position buffer-start)
              (reset))
            (when (> edit-start-pos buffer-start)
              (set! edit-start-pos (sub1 edit-start-pos)))
            (channel-put sync
                         (lambda ()
                           (let-values (((orig-token-start orig-token-end valid-tree invalid-tree)
                                         (split tokens (- edit-start-pos buffer-start))))
                             (let ((in (open-input-text-editor this (+ buffer-start orig-token-start) 'end)))
                               (set! tokens valid-tree)
                               (set! invalid-tokens invalid-tree)
                               (set! invalid-tokens-start (+ orig-token-end change-length))
                               (re-tokenize prefix get-token in
                                            (+ buffer-start orig-token-start)
                                            (+ buffer-start orig-token-start)))))))
          (channel-get sync)
          (begin-edit-sequence #f)
          (color)
          (end-edit-sequence)))
      
      (define/public (start prefix get-token)
        (reset)
        (unless remove-prefs-callback-thunk
          (set! remove-prefs-callback-thunk
                (preferences:add-callback
                 (string->symbol (format "syntax-coloring:~a:active" prefix))
                 (lambda (_ on?)
                   (set! should-color? on?)
                   (set-surrogate (get-surrogate))))))
        (when should-color?
          (unless background-thread
            (set! background-thread (thread background-colorer)))
          (channel-put sync
                       (lambda ()
                         (re-tokenize prefix get-token (open-input-text-editor this last-prompt-position 'end)
                                      last-prompt-position last-prompt-position)))
          (channel-get sync)
          (begin-edit-sequence #f)
          (color)
          (end-edit-sequence)))
      
      (define/public (stop prefix get-token)
        (reset)
        (when remove-prefs-callback-thunk
          (remove-prefs-callback-thunk)
          (set! remove-prefs-callback-thunk #f))
        (change-style (send (get-style-list) find-named-style "Standard") last-prompt-position 'end #f))
  
      (define (background-colorer)
        ((channel-get sync))
        (channel-put sync #f)
        (background-colorer))
  
      (super-instantiate ())))
  
  (define (colorer %)
    (class %
      ;; get-token takes an input port and returns 4 values:
      ;; A symbol in `(keyword string literal comment error identifier default)
      ;; Data to be kept with the token
      ;; The token's starting offset
      ;; The token's ending offset
      (init-field get-token prefix)
            
      (rename (super-on-disable-surrogate on-disable-surrogate))
      (define/override (on-disable-surrogate text)
        (super-on-disable-surrogate text)
        (send text stop prefix get-token))
      
      (rename (super-on-enable-surrogate on-enable-surrogate))
      (define/override (on-enable-surrogate text)
        (super-on-enable-surrogate text)
        (send text start prefix get-token))
      
      (rename (super-after-insert after-insert))
      (define/override (after-insert text _ edit-start-pos change-length)
        (super-after-insert text _ edit-start-pos change-length)
        (send text do-insert/delete prefix get-token edit-start-pos change-length))
      
      (rename (super-after-delete after-delete))
      (define/override (after-delete text _ edit-start-pos change-length)
        (super-after-delete text _ edit-start-pos change-length)
        (send text do-insert/delete prefix get-token edit-start-pos (- change-length)))
      
      (super-instantiate ())
      ))
  )