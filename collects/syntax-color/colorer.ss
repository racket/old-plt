(module colorer mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "token-tree.ss")
  
  (provide colorer)
  
  
  (define (wrap-port p)
    (make-custom-input-port
     (lambda (s)
       (let ((v (read-string-avail! s p)))
         (write s)
         v))
     #f
     (lambda () (close-input-port p))))
  
  (define (colorer %)
    (class %
      ;; get-token takes an input port and returns 4 values:
      ;; A symbol in `(keyword string literal comment error identifier default)
      ;; Data to be kept with the token
      ;; The token's starting offset
      ;; The token's ending offset
      (init-field get-token prefix)
      
      (define (update-style text start end tok-type)
        (queue-callback
         (lambda ()
           (send text change-style
                 (preferences:get (string->symbol (format "drscheme:editor-modes:~a:~a"
                                                          prefix
                                                          tok-type)))
                 (sub1 start)
                 (sub1 end)
                 #f))))
      
      (rename (super-on-disable-surrogate on-disable-surrogate))
      (define/override (on-disable-surrogate text)
        (super-on-disable-surrogate text)
        (send text set-tokens #f))

      (rename (super-on-enable-surrogate on-enable-surrogate))
      (define/override (on-enable-surrogate text)
        (super-on-enable-surrogate text)
        (send text set-tokens #f)
        (re-tokenize (open-input-text-editor text 0 'end)
                     text
                     0
                     #f
                     #f
                     0))
            
      (define (re-tokenize in text real-offset valid-tree invalid-tree invalid-tree-offset)
        (cond
          ((and invalid-tree (= invalid-tree-offset 0))
           (send text set-tokens (insert-after! valid-tree (search-min! invalid-tree null))))
          (else
           (let-values (((type data new-token-start new-token-end) (get-token in)))
             (cond
               ((eq? 'eof type)
                (send text set-tokens valid-tree))
               (else
                (let ((len (- new-token-end new-token-start)))
                  (let loop ((next-invalid-offset (- invalid-tree-offset len))
                             (next-invalid-tree invalid-tree))
                    (cond
                      ((and next-invalid-tree (< next-invalid-offset 0))
                       (let ((min-tree (search-min! next-invalid-tree null)))
                         (loop (+ next-invalid-offset (node-token-length min-tree)) (node-right min-tree))))
                      (else
                       (update-style text
                                     (+ real-offset new-token-start)
                                     (+ real-offset new-token-end)
                                     type)
                       (re-tokenize in text real-offset
                                    (insert-after! valid-tree (make-node len data 0 #f #f))
                                    next-invalid-tree
                                    next-invalid-offset)))))))))))
      
      (rename (super-after-insert after-insert))
      (define/override (after-insert text _ edit-start-pos change-length)
        (super-after-insert text _ edit-start-pos change-length)
        (after-insert-or-delete text edit-start-pos change-length))
      
      (rename (super-after-delete after-delete))
      (define/override (after-delete text _ edit-start-pos change-length)
        (super-after-delete text _ edit-start-pos change-length)
        (after-insert-or-delete text edit-start-pos (- change-length)))
        
      (define (after-insert-or-delete text edit-start-pos change-length)
        (when (> edit-start-pos 0)
          (set! edit-start-pos (sub1 edit-start-pos)))
        (let-values (((orig-token-start orig-token-end valid-tree invalid-tree)
                      (split (send text get-tokens) edit-start-pos)))
          (let ((in (open-input-text-editor text orig-token-start 'end)))
            (re-tokenize in text orig-token-start valid-tree invalid-tree
                         (- (+ orig-token-end change-length) orig-token-start)))))
      
        (super-instantiate ())
        ))
  )