(module string-constant mzscheme
  (require-for-syntax (lib "etc.ss"))
  (require (lib "mred.ss" "mred"))

  (provide string-constant string-constants this-language all-languages set-language-pref)
  
  ;; set-language-pref : string -> void
  (define (set-language-pref language)
    (write-resource "mred" "gui_language" language (find-graphical-system-path 'setup-file)))
  
  ;; language : symbol
  (define language 
    (let* ([b (box "")]
           [r (get-resource "mred" "gui_language" b #f)]
           [default-str "English"]
           [default (string->symbol default-str)])
      (if r
          (string->symbol (unbox b))
          (begin
            (write-resource "mred" "gui_language" default-str)
            default))))
  
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
        (list 
         (make-sc "English" (get-string-constants "english-string-constants.ss"))
         (make-sc "Spanish" (get-string-constants "spanish-string-constants.ss"))
         (make-sc "French" (get-string-constants "french-string-constants.ss"))
         (make-sc "German" (get-string-constants "german-string-constants.ss"))))
      
      (define first-string-constant-set (car available-string-constant-sets))
      
      (define dummy
        (let ([check-one-way
               (lambda (sc1 sc2)
                 (let ([ht1 (sc-constants sc1)]
                       [ht2 (sc-constants sc2)]
                       [already-warned #&()])
                   (hash-table-for-each
                    ht1
                    (lambda (constant value)
                      (unless (hash-table-get ht2 constant (lambda () #f))
                        (let ([unknown-word (string-append "UNK" value)]
                              [no-warning-cache-key (cons (sc-language-name sc1) (sc-language-name sc2))])
                          (hash-table-put! ht2 constant unknown-word)
                          (unless (member no-warning-cache-key (unbox already-warned))
                            (set-box! already-warned (cons no-warning-cache-key (unbox already-warned)))
                            
                            ;; in some cases, the printf may raise an exception because the error port
                            ;; is gone. If so, just don't display the warning.
                            (with-handlers ([not-break-exn?
                                             (lambda (x) (void))])
                              (fprintf
                               (current-error-port)
                               "WARNING: language ~a defines ~a, but\n         language ~a does not, substituting:\n         \"~a\"\n         other words may be substituted without warning.\n"
                               (sc-language-name sc1)
                               constant
                               (sc-language-name sc2)
                               unknown-word)))))))))])
          (for-each (lambda (x) 
                      (check-one-way x first-string-constant-set)
                      (check-one-way first-string-constant-set x))
                    (cdr available-string-constant-sets))))
           
      (define (string-constant stx)
        (syntax-case stx ()
          [(_ name)
           (let ([ht (sc-constants first-string-constant-set)]
                 [datum (syntax-object->datum (syntax name))])
             (unless (symbol? datum)
               (raise-syntax-error #f
                                   (format "expected name, got: ~s" datum)
                                   stx))
             (unless (hash-table-get ht datum (lambda () #f))
               (raise-syntax-error 
                #f
                (format "~a is not a known string constant" datum) 
                stx))
             (with-syntax ([(constants ...) (map (lambda (x) (hash-table-get (sc-constants x) datum))
                                                 available-string-constant-sets)]
                           [(languages ...) (map (lambda (x) (string->symbol (sc-language-name x)))
                                                 available-string-constant-sets)]
                           [first-constant (hash-table-get (sc-constants first-string-constant-set) datum)])
               (syntax (cond
                         [(eq? language 'languages) constants] ...
                         [else first-constant]))))]))
      
      (define(string-constants stx)
        (syntax-case stx ()
          [(_ name)
           (let ([ht (sc-constants first-string-constant-set)]
                 [datum (syntax-object->datum (syntax name))])
             (unless (symbol? datum)
               (raise-syntax-error #f
                                   (format "expected name, got: ~s" datum)
                                   stx))
             (unless (hash-table-get ht datum (lambda () #f))
               (raise-syntax-error 
                (quote-syntax string-constants)
                (format "~a is not a known string constant" datum) stx))
             (with-syntax ([(constants ...) (map (lambda (x) (hash-table-get (sc-constants x) datum))
                                                 available-string-constant-sets)])
               (syntax (list constants ...))))]))
      
      (define (this-language stx)
        (syntax-case stx ()
          [(_)
           (syntax (symbol->string language))]))
      
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
(expand #'(string-constant is-this-your-native-language))
(expand #'(string-constants is-this-your-native-language))
|#
