
(module sba-errors mzscheme
  (require
   (lib "specs.ss" "framework") ; contracts
   
   ;"provide-structs.ss" ; struct contracts
   "labels.ss"
   (prefix lab: "labels.ss")
   ;(prefix asset: "assoc-set-list.ss")
   (prefix asset: "assoc-set-hash.ss")
   (prefix cst: "constants.ss")
   )

  ; (listof label) symbol string
  ; need this before the contracts because of sba-error?
  (define-struct sba-error (labels gravity message) (make-inspector))
  
  ; (assoc-setof label (listof sba-error))
  ; we use a list instead of a set for the sba-errors, because that's what error-table-get
  ; has to return anyway
  (define-struct error-table (assoc-set))
  
  ;(provide-struct/contract
  ; ((struct sba-error (labels gravity message)) lab:label? (symbols 'red 'orange 'green) string?)
  ; )
  (provide/contract
   (sba-error-gravity (sba-error? . -> . (symbols 'red 'orange 'green)))
   (sba-error-message (sba-error? . -> . string?))
   (make-error-table  (-> error-table?))
   (error-table? (any? . -> . boolean?))
   (error-table-set (error-table? (listof lab:label?) (symbols 'red 'orange 'green) string? . -> . void?))
   (error-table-get (error-table? lab:label? . -> . (listof sba-error?)))
   )
  
  (define real-make-error-table make-error-table)
  
  ; -> error-table
  (set! make-error-table
        (lambda ()
          (real-make-error-table (asset:make-assoc-set))))
  
  ; top -> boolean
  ; error-table? comes from the structure definition
  
  ; error-table (listof label) (union 'red 'orange 'green) string -> void
  ; adds error to the error list for each label
  ; we use terms instead of labels as the key, because a primitive will have several labels
  ; associated with it (one created from the program text, and at least one created from the
  ; type for that primitive), so we need to use as key something unique about the primitive.
  (define (error-table-set error-table labels gravity message)
    (let ([assoc-set (error-table-assoc-set error-table)]
          [error (make-sba-error labels gravity message)])
      (for-each (lambda (label)
                  (let ([term (label-term label)])
                    (asset:assoc-set-set 
                     assoc-set
                     term
                     (cons error (asset:assoc-set-get assoc-set term cst:thunk-empty))
                     #f)))
                labels)))
  
  ; error-table label -> (listof sba-error)
  (define (error-table-get error-table label)
    (asset:assoc-set-get (error-table-assoc-set error-table) (label-term label) cst:thunk-empty))
  
  )
