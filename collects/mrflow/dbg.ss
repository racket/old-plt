(module dbg mzscheme
  (require (lib "contract.ss"))
  
  (provide (all-from (lib "contract.ss"))
           dbg-define/contract)
  
  (define-syntax (dbg-define/contract stx)
    (syntax-case stx ()
      ((_ id contract body)
       ; doesn't work because the created syntax object gets the context of this module,
       ; not the context of the module where id, contract, and body are defined...
       ;#'(define/contract id contract body))))
       ;#'(define id body))))
       (datum->syntax-object stx
                             `(define/contract ,#'id ,#'contract ,#'body)
                             stx stx))))
  )
