
(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide plt:prims^
           plt:beginner-extras^
           plt:intermediate-extras^
           plt:userspace^
           plt:init-namespace^
           plt:basis^
           plt:basis-import^)
  
  (define-signature plt:prims^
    (beginning
     intermediate
     advanced))
  
  (define-signature plt:beginner-extras^
    ((struct posn (x y) -setters)))
  
  (define-signature plt:intermediate-extras^
    plt:beginner-extras^)
  
;; will be redefined in guserspace's plt:userspace^
;; file if in drscheme
  (define-signature plt:userspace^
    ((struct posn (x y))))
  (define-signature plt:advanced-extras^
    ((struct posn (x y))))
  
;; extend structs with a parsing constructor
  (define-syntax (define-struct/parse stx)
    (syntax-case stx ()
      [(_ str (fields ...))
       (unless (symbol? (syntax-object->datum (syntax str)))
         (error 'define-struct/parse "no super structs allowed"))
       (let* ([first car]
              [second cadr]
              [third caddr]
              [defn (syntax->list (expand (syntax (define-struct str (fields ...)))))]
              [_ (unless (and (pair? defn)
                              (eq? (syntax-e (first defn)) 'define-values))
                   (error 'define-struct/parse
                          "expand didn't return expected value: ~s~n" (syntax-object->datum defn)))]
              
              [bindings/list (syntax->list (second defn))]
              [make-parse (string->symbol 
                           (string-append
                            "make-"
                            (symbol->string (syntax-e (syntax str)))
                            "/parse"))])
         (with-syntax ([bindings (cons make-parse (second defn))]
                       [exp (third defn)]
                       [make-parser make-parse]
                       [maker-name (second (syntax->list (second defn)))])
           (syntax
            (define-values bindings
              (let ([make-parser
                     (lambda (inits)
                       (let ([select-field
                              (lambda (field)
                                (let ([m (assq field inits)])
                                  (unless m
                                    (error 'make-parse "no binding for: ~a" field))
                                  (unless (= (length m) 2)
                                    (error 'make-parse "malformed binding: ~a" m))
                                  (cadr m)))])
                         (maker-name (select-field 'fields) ...)))])
                (call-with-values (lambda () exp)
                                  (lambda bindings (apply values make-parser bindings))))))))]))
  
  (define-signature plt:init-params^
    (initial-line
     initial-offset
     initial-column
     
     initialize-parameters
     settings
     get-default-setting
     get-default-setting-name
     
     drscheme-load-handler
     
     raw-reader
     zodiac-reader
     
     zodiac-vocabulary?
     beginner-language?
     intermediate-language?
     advanced-language?
     full-language?
     
     error-display/debug-handler
     current-vocabulary
     current-setting
     intermediate-values-during-load
     bottom-escape-handler
     
     drscheme-print
     
     format-source-loc
     
     primitive-eval
     primitive-load
     syntax-checking-primitive-eval
     
     process/zodiac
     process/no-zodiac
     
     process-file/zodiac
     process-file/no-zodiac
     process-sexp/zodiac
     process-sexp/no-zodiac
     
     (struct process-finish (error?))
     
     setting-name->number
     number->setting
     setting/unparse
     (struct setting (name
                      vocabulary-symbol
                      primitives
                      macro-libraries
                      case-sensitive?
                      allow-set!-on-undefined?
                      unmatched-cond/case-is-error?
                      allow-improper-lists?
                      sharing-printing?
                      abbreviate-cons-as-list?
                      signal-undefined
                      signal-not-boolean
                      eq?-only-compares-symbols?
                      <=-at-least-two-args
                      error-sym/string-only
                      disallow-untagged-inexact-numbers
                      print-tagged-inexact-numbers
                      whole/fractional-exact-numbers
                      print-booleans-as-true/false
                      printing
                      use-pretty-printer?
                      teaching-primitives-and-syntax?))
     make-setting/parse
     
     teaching-level?
     
     find-setting-named
     add-setting
     copy-setting
     
     r4rs-style-printing?))
  
  (define-signature plt:init-namespace^
    (teachpack-error-display
     init-namespace
     bad-teachpacks
     teachpack-ok?
     teachpack-changed))
  
  (define-signature plt:basis^
    ((open plt:init-params^)
     (open plt:init-namespace^)))
  
  (define-signature plt:basis-import^
    (in-mzscheme?)))
