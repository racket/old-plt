(module proc-table mzscheme
  (provide genproc
           number->proc
           make-proc->number
           )
  
  (define the-proc-table (make-hash-table))
  (define the-index 0)
  
  ;; genproc: symbol -> symbol
  ;; generate a new procedure name and associate a number with it
  (define (genproc sym)
    (let ([new-name (gensym sym)])
      (hash-table-put! the-proc-table the-index new-name)
      (set! the-index (add1 the-index))
      new-name))
  
  ;; number->proc: number -> procedure
  ;; lookup a procedure based on the number
  (define (number->proc n)
    (let ([proc-name (hash-table-get the-proc-table n)])
      (namespace-variable-value proc-name)))
  
  ;; make-proc->number: -> procedure -> number
  ;; make a procedure to lookup numbers associated with procedures
  (define (make-proc->number)
    (let ([reverse-directory (make-hash-table)])
      (hash-table-for-each
       the-proc-table
       (lambda (number proc-name)
         (hash-table-put!
          reverse-directory
          (namespace-variable-value proc-name)
          number)))
      (lambda (proc)
        (hash-table-get reverse-directory proc))))  
  )
