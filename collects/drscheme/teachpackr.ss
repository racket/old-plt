(unit/sig drscheme:teachpack^
  (import [mred : mred^])
  
  (define core-flat@ (require-library-unit/sig "coreflatr.ss"))
  
  ;; build-teachpack-thunk : string -> (union #f (list (union 'mz 'mr) (-> void)))
  ;; accepts a filename and returns a thunk that invokes the corresponding teachpack and
  ;; a symbol indicating if this is a mzscheme teachpack or a mred teachpack.
  (define (build-teachpack-thunk v)
    (with-handlers
	([(lambda (x) #t)
	  (lambda (x)
	    (mred:message-box "Invalid Teachpack" (exn-message x))
	    #f)])
      (let ([raw-new-unit (parameterize ([read-case-sensitive #t])
                            (load/cd v))])
        (let-values ([(kind-of-unit new-unit)
                      (cond
                        [(pair? raw-new-unit)
                         (unless (and (list? raw-new-unit)
                                      (= 2 (length raw-new-unit))
                                      (or (eq? 'mz (car raw-new-unit))
                                          (eq? 'mr (car raw-new-unit))))
                           (error "malformed Teachpack, expected a list of length two whose first element is either 'mz or 'mr, got: ~e"
                                  raw-new-unit))
                         (apply values raw-new-unit)]
                        [else
                         (values 'mr raw-new-unit)])])
          (if (unit/sig? new-unit)
              ; Put the unit into a procedure that invokes it into
              ;  the current namespace
              (let* ([signature 
                      ; exploded -> flattened
                      (let ([sig (unit-with-signature-exports new-unit)])
                        (let loop ([l (vector->list sig)][r null])
                          (cond
                            [(null? l) r]
                            [(symbol? (car l)) (loop (cdr l) (cons (car l) r))]
                            [else (let ([sub (loop (vector->list (cadr l)) null)]
                                        [prefix (string-append (symbol->string (car l)) ":")])
                                    (loop (cdr l)
                                          (append
                                           (map (lambda (s)
                                                  (string->symbol
                                                   (string-append
                                                    prefix
                                                    (symbol->string s))))
                                                sub))))])))])
                (list 
                 kind-of-unit
                 (eval
                  `(lambda ()
                     (with-handlers ([(lambda (x) #t)
                                      (lambda (x)
                                        ((error-display-handler)
                                         (format
                                          "Invalid Teachpack:~n~a"
                                          (if (exn? x) (exn-message x) x))))])
                       ,(if (eq? 'mz kind-of-unit)
                            `(global-define-values/invoke-unit/sig
                              ,signature
                              (compound-unit/sig
                                (import)
                                (link [userspace : plt:mz-userspace^ 
                                                 ((compound-unit/sig 
                                                    (import)
                                                    (link [core : mzlib:core-flat^ (,core-flat@)])
                                                    (export (open core))))]
                                      [teachpack : ,signature (,new-unit userspace)])
                                (export (open teachpack))))
                            `(global-define-values/invoke-unit/sig
                              ,signature
                              (compound-unit/sig
                                (import)
                                (link [userspace : plt:userspace^ 
                                                 ((compound-unit/sig 
                                                    (import)
                                                    (link [core : mzlib:core-flat^ (,core-flat@)]
                                                          [mred : mred^ (,mred:mred@)])
                                                    (export (open core)
                                                            (open mred))))]
                                      [teachpack : ,signature (,new-unit userspace)])
                                (export (open teachpack))))))))))
              (begin
                (mred:message-box 
                 "Invalid Teachpack"
                 (format "loading Teachpack file does not result in a unit/sig, got: ~e"
                         new-unit))
                #f)))))))