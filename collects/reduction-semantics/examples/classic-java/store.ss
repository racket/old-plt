;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; store.ss
;; Richard Cobbe
;; July 2004
;;
;; $Id$
;;
;; Implementation of a functional store.  This store is an alist, but note that
;; store-update does not produce a longer alist; this is helpful for debugging
;; and testing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module store mzscheme

  (require (lib "contract.ss")
           (lib "etc.ss")
           "utils.ss")

  (with-public-inspector
   (define-struct store (top contents)))
  ;; (Store X) ::= (make-store Number (Listof (List Number X)))

  ;; store-alloc :: (Store X) X -> Number (Store X)
  ;; allocates & inits new entry in store; returns new location and new store
  (define store-alloc
    (lambda (store new-val)
      (let* ([addr (store-top store)]
             [next-top (add1 addr)])
        (values addr
                (make-store next-top (cons (list addr new-val)
                                           (store-contents store)))))))

  ;; store-ref :: (Store X) Number -> X
  ;; looks up value at locn in store; invokes FK if bad address.
  ;; Default fk raises exn:application:mismatch.
  (define store-ref
    (opt-lambda (store locn [fk (lambda ()
                                  (raise (make-exn:application:mismatch
                                          "address not in domain of store"
                                          (current-continuation-marks)
                                          locn)))])
      (let loop ([entries (store-contents store)])
        (cond
          [(null? entries) (fk)]
          [(= locn (caar entries)) (cadar entries)]
          [else (loop (cdr entries))]))))

  ;; store-update :: (Store X) Number X -> (Store X)
  ;; Updates store at locn.  If locn invalid, raises exn:application:mismatch.
  (define store-update
    (lambda (store locn val)
      (make-store (store-top store)
                  (let loop ([entries (store-contents store)])
                    (cond
                      [(null? entries)
                       (raise (make-exn:application:mismatch
                               "attempted to update address not in store"
                               (current-continuation-marks)
                               locn))]
                      [(= locn (caar entries))
                       (cons (list locn val) (cdr entries))]
                      [else (cons (car entries) (loop (cdr entries)))])))))

  (define empty-store (make-store 0 null))

  ;; create-store :: (List Number X)* -> (Store X)
  ;; Creates a store from a sequence of address/value lists.
  (define create-store
    (lambda entries
      (let ([max-addr (apply max (cons -1 (map car entries)))])
        (make-store (add1 max-addr) entries))))

  ;; builds a store out of a list of address/value pairs (much like the
  ;; hash-table macro in etc.ss).
  (define-syntax build-store
    (syntax-rules ()
      [(_ (addr value) ...)
       (create-store (list addr value) ...)]))

  (provide/contract (store-alloc (-> store? any? (values number? store?)))
                    (store-ref (opt->* (store? number?)
                                       ((-> any))
                                       any))
                    (store-update (-> store? number? any? store?))
                    (empty-store store?)
                    (store? (-> any? boolean?)))

  (provide (rename build-store store)))
