(module dbg mzscheme
  (require (lib "pretty.ss")
           (lib "contract.ss"))
  
  (provide (all-from-except (lib "contract.ss") provide/contract)
           (rename dbg-provide/contract provide/contract)
           lambda/contract

           ;debug
           ;get-debug-level
           ;set-debug-level
           )
  
  (define-syntax (dbg-provide/contract stx)
    ; with contracts
    (syntax-case stx ()
      [(_ all ...)
       (datum->syntax-object stx `(,#'provide/contract ,@(syntax->list #'(all ...))))])
    ; without contracts
    #;(syntax-case stx (struct)
      [(_) #'(provide)]
      [(_ (id contract) other ...)
       #'(begin (provide id) (dbg-provide/contract other ...))]
      [(_ (struct id ((field contract) ...)) other ...)
       #'(begin (provide (struct id (field ...))) (dbg-provide/contract other ...))])
    )
  
  #;(define-syntax (dbg-define/contract stx)
    (syntax-case stx ()
      [(_ id contract body)
       ;(datum->syntax-object stx `(define ,#'id ,#'body) stx stx)
       (datum->syntax-object stx `(,#'define/contract ,#'id ,#'contract ,#'body) stx stx)]))
  

  #;(define-values (set-debug-level get-debug-level)
    (let* ([debug-levels (make-hash-table)]
           [set-debug-level
            (lambda (level val) (hash-table-put! debug-levels level val))]
           [get-debug-level
            (lambda (level)
              (hash-table-get debug-levels level (lambda () #f)))])
      (values set-debug-level get-debug-level)))
      

  #;(define-syntax (debug stx)
    (syntax-case stx ()
      ([_ cat exp]
       (syntax 
        ;(when (get-debug-level cat)
        ;  (begin (pretty-display exp)))
        (void)
        ))))
  
  (define-syntax (lambda/contract stx)
    (syntax-case stx ()
      ([_ args contract body]
       (with-syntax  ((funname (car (generate-temporaries '(1)))))
         (syntax 
          (let ()
            (define/contract funname contract
              (lambda args body))
            funname))))))
  )
