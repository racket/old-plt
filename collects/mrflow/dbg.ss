(module dbg mzscheme
  (require (lib "contract.ss"))
  
  (provide (all-from (lib "contract.ss"))
           dbg-define/contract)
  
  (define-syntax (dbg-define/contract stx)
    (syntax-case stx ()
      ((_ id contract body)
       #'(define/contract id contract body))))
       ;#'(define id body))))
)
