(module structure-helper mzscheme
  (require (lib "stx.ss" "syntax"))
  (provide str str? str-renames make-str remove-dups split open open-as-helper)
  (define-struct str (renames))
  
  (define (remove-dups l)
    (let ((ht (make-hash-table)))
      (let loop ((l l))
        (cond
          ((null? l) null)
          ((hash-table-get ht (caar l) (lambda () #f)) (loop (cdr l)))
          (else 
           (hash-table-put! ht (caar l) #t)
           (cons (car l) (loop (cdr l))))))))
  
  (define (split path)
    (let ((r (reverse path)))
      (values (reverse (cdr r)) (car r))))

  (define (open path type)
    (unless (identifier? (car path))
      (raise-syntax-error type "Path component must be an identifier" (car path)))
    (let ((err (lambda (name)
                 (lambda ()
                   (raise-syntax-error type "Unknown structure" name)))))
      (let loop ((path (cdr path))
                 (env (str-renames (syntax-local-value 
                                    (car path)
                                    (err (car path))))))
        (cond
          ((null? path) env)
          (else
           (unless (identifier? (car path))
             (raise-syntax-error type "Path component must be an identifier" (car path)))
           (let ((bind (assq (syntax-object->datum (car path)) env)))
             (unless bind
               ((err (car path))))
             (let ((new-env
                    (str-renames
                     (syntax-local-value (cdr bind) (err (cdr bind))))))
               (loop (cdr path) new-env))))))))

  (define (open-as-helper path field)
    (unless (identifier? field)
      (raise-syntax-error 'open-as "Field to open must be an identifier" field))
    (let ((hid (assq (syntax-object->datum field )
                     (open (stx->list path) 'open-as))))
      (unless hid
        (raise-syntax-error 'open-as "Unknown field" field))
      (make-rename-transformer (cdr hid))))
  
  )