(module string-constant mzscheme
  (require-for-syntax (lib "etc.ss"))
  
  (provide string-constant string-constants this-language all-languages)
  
  (define-syntaxes (string-constant string-constants this-language all-languages)
    (let ()
    ;; type sc = (make-sc string (listof (hash-table symbol string)))
    (define-struct sc (language-name constants))

    (define (get-string-constants filename)
      (let* ([filename (build-path (this-expression-source-directory) filename)]
             [sexp (call-with-input-file filename read 'text)])
        (unless (and (list? sexp)
                     (andmap (lambda (x) 
                               (and (list? x)
                                    (= 2 (length x))
                                    (symbol? (car x))
                                    (string? (cadr x))))
                             sexp))
          (raise-syntax-error 'string-constant
                              (format "expected `((,symbol string) ...), got: ~s" sexp)))
        (let ([ht (make-hash-table)])
          (for-each (lambda (x) 
                      (when (hash-table-get ht (car x) (lambda () #f))
                        (raise-syntax-error
                         'string-constants 
                         (format "found duplicate for ~a in ~a"
                                 (car x) filename)))
                      (hash-table-put! ht (car x) (cadr x)))
                    sexp)
          ht)))
    
    (define available-string-constant-sets
      (list ;(make-sc "xEnglish" (get-string-constants "xenglish-string-constants.ss"))
            (make-sc "English" (get-string-constants "english-string-constants.ss"))))
    
    (define first-string-constant-set (car available-string-constant-sets))
    
    (define dummy
      (let ([check-one-way
             (lambda (sc1 sc2)
               (let ([ht1 (sc-constants sc1)]
                     [ht2 (sc-constants sc2)])
                 (hash-table-for-each
                  ht1
                  (lambda (constant value) 
                    (unless (hash-table-get ht2 constant (lambda () #f))
                      (raise-syntax-error
                       'string-constant
                       (format "language ~a defines ~a, but language ~a does not"
                               (sc-language-name sc1)
                               constant
                               (sc-language-name sc2))))))))])
        (for-each (lambda (x) 
                    (check-one-way x first-string-constant-set)
                    (check-one-way first-string-constant-set x))
                  (cdr available-string-constant-sets))))
    
    ;; use mred resources to save this -- framework prefs won't work
    ;; since this is elaboration time, not run-time of DrScheme
    (define language "English")
    
    (define string-constant-set
      (if language
          (or (ormap (lambda (x) 
                       (string=? language (sc-language-name x))
                       x)
                     available-string-constant-sets)
              first-string-constant-set)
          first-string-constant-set))
    
    (define (get-one name sc stx)
      (let ([ht (sc-constants sc)]
            [datum (syntax-object->datum name)])
        (unless (symbol? datum)
          (raise-syntax-error #f
                              (format "expected name, got: ~s" datum)
                              stx))
        (let ([table-entry (hash-table-get ht datum (lambda () #f))])
          (unless table-entry
            (raise-syntax-error 
             (quote-syntax string-constant)
             (format "~a is not a known string constant" datum) stx))
          (with-syntax ([constant table-entry])
            (syntax constant)))))

    (define (string-constant stx)
      (syntax-case stx ()
        [(_ name)
         (let ([datum (syntax-object->datum (syntax name))])
           (get-one (syntax name) string-constant-set stx))]))
    
    (define(string-constants stx)
      (syntax-case stx ()
        [(_ name)
         (with-syntax ([(all ...) (map (lambda (x) (get-one (syntax name) x stx))
                                       available-string-constant-sets)])
           (syntax (list all ...)))]))
    
    (define (this-language stx)
      (syntax-case stx ()
        [(_)
         (with-syntax ([language (sc-language-name string-constant-set)])
           (syntax language))]))

    (define (all-languages stx)
      (syntax-case stx ()
        [(_) 
         (with-syntax ([(languages ...) (map sc-language-name available-string-constant-sets)])
           (syntax (list languages ...)))]))

    (values
     string-constant
     string-constants
     this-language
     all-languages))))

#|
(require string-constant)
(string-constant is-this-your-native-language)
(string-constants is-this-your-native-language)
(all-languages)
(this-language)
|#