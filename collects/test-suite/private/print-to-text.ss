(module print-to-text mzscheme
  
  (require
   (lib "list.ss")
   (lib "etc.ss")
   (lib "class.ss")
   (lib "contract.ss")
   (lib "mred.ss" "mred"))
  
  (provide/contract
   (print-to-text ((is-a?/c text%) (listof any?) . -> . void)))
  
  ;; Using the current languages print operations, print the list of values to the text
  (define (print-to-text atext vals)
    (unless (empty? vals)
      (send* atext
        (lock false)
        (begin-edit-sequence)
        (erase))
      (let ([port
             (make-output-port
              'set-actuals
              always-evt
              (lambda (s start end block? enable-breaks?)
                (send atext insert
                      (list->string
                       (map integer->char
                            (bytes->list (subbytes s start end)))))
                (- end start))
              void)])
        (print (first vals) port)
        (for-each
         (lambda (val)
           (newline port)
           (print val port))
         (rest vals)))
      (send* atext
        (end-edit-sequence)
        (lock true))))
  )