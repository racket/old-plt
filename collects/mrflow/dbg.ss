(module dbg mzscheme
  (require (lib "pretty.ss")
           (lib "contract.ss"))
  
  (provide (all-from (lib "contract.ss"))
           dbg-define/contract
           lambda/contract

           show-png
           show-ps
           dot-file->png
           dot-file->ps

           debug
           get-debug-level
           set-debug-level)
  
  (define-syntax (dbg-define/contract stx)
    (syntax-case stx ()
      ((_ id contract body)
       (datum->syntax-object stx `(define ,#'id ,#'body) stx stx)
       ;(datum->syntax-object stx `(define/contract ,#'id ,#'contract ,#'body) stx stx)
       )))

  (define-values (set-debug-level get-debug-level)
    (let* ([debug-levels (make-hash-table)]
           [set-debug-level
            (lambda (level val) (hash-table-put! debug-levels level val))]
           [get-debug-level
            (lambda (level)
              (hash-table-get debug-levels level (lambda () #f)))])
      (values set-debug-level get-debug-level)))
      

  (define-syntax (debug stx)
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

  ;; It would be cool to pull the contracts from something like
  ; (lambda ((x : integer?) (y : integer?) . -> . (z : integer?)) x)

  (define dot-file->png
    (lambda (filename)
      (let-values ([(sp sp-stdout sp-stdin sp-stderr)
                    (subprocess #f #f #f
                                "c:/Program Files/ATT/Graphviz/bin/dot.exe"
                                "-Tpng"
                                "-Gratio=1.29"
                                (string-append filename ".dot")
                                (string-append "-o" filename ".png"))])
        (subprocess-wait sp)
        filename)))

  (define dot-file->ps
    (lambda (filename)
      (let-values ([(sp sp-stdout sp-stdin sp-stderr)
                    (subprocess #f #f #f
                                "c:/Program Files/ATT/Graphviz/bin/dot.exe"
                                "-Tps2"
                                "-Gratio=1.29"
                                (string-append filename ".dot")
                                (string-append "-o" filename ".ps"))])
        (subprocess-wait sp)
        filename)))

  (define show-png
    (lambda (filename)
      (let-values ([(sp sp-stdout sp-stdin sp-stderr)
                    (subprocess #f #f #f
                                "c:/Program Files/SlowView/Slowview.exe"
                                (string-append filename ".png"))])
        filename)))

  (define show-ps
    (lambda (filename)
      (let-values ([(sp sp-stdout sp-stdin sp-stderr)
                    (subprocess #f #f #f
                                "c:/Program Files/Ghostgum/gsview/gsview32.exe"
                                (string-append filename ".ps"))])
        filename)))
  )
