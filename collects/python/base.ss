(module base mzscheme
  (require "runtime-support.ss"
           (lib "contract.ss")
           (lib "etc.ss")
           )

  (define guarded
    (opt-lambda (fn [name #f])
      (make-py-function
       (make-py-code (or name (object-name fn))
                     (lambda args
                       (with-handlers ([(lambda (e)
                                          (regexp-match #rx"contract"
                                                        (exn-message e)))
                                        (lambda (e)
                                          (error (or name (object-name fn))
                                                 "You broke my contract."))])
                         (apply fn args)))
                     (procedure-arity fn)
                     null))))
  
  (define wrap
    (opt-lambda (fn [name #f])
      (make-py-function
       (make-py-code (or name (object-name fn))
                     fn
                     (procedure-arity fn)
                     null))))
  
  (define myappend (guarded my-append))


  (define callcc (wrap py-call/cc))
  
  (provide #%top
           #%app
           #%datum
           #%module-begin
           
           myappend
           callcc
           
           ;#%provide
           ;#%define
             ;; not sure if it's ok to remove these
           ;(rename #%begin begin)
           ;(rename #%provide provide)
           ;(rename #%define define)
           ;(rename #%require require)
           ;(rename #%datum->syntax-object datum->syntax-object)
           ;syntax
           
           (all-from "runtime-support.ss")
           
           ;; types
           (rename cpy-object object)
           (rename cpy-str str)
           (rename cpy-list list)
           (rename cpy-type type)
           (rename cpy-tuple tuple)
           (rename cpy-dict dict)
           
           )
  
  )
