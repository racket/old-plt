(module runtime-support mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))
  
  ;;;;;;;;;; Python Runtime Support by Daniel ;;;;;;;;;
  
  ;; ==: X X -> bool
  (define(== a b)
    (cond
      [(number? a) (= a b)]
      [(string? a) (string=? a b)]
      [(symbol? a) (eq? a b)]
      [(tuple? a) (andmap == (tuple-list a) (tuple-list b))]
      [(list? a) (andmap == a b)]
      [else (error (format "No runtime support to compare ~a and ~a yet" a b))]))
  
  ;; make-dictionary: (listof (list Symbol X)) -> (hash-tableof Symbol X)
  (define (make-dictionary al)
    (let ([dict (make-hash-table)])
      (begin
        (for-each (lambda (pair)
                    (hash-table-put! dict (car pair) (cadr pair)))
                  al)
        dict)))
  
  ;; repr: X -> string
  (define (repr x)
    (cond
      [(string? x) (string-append "'" x "'")]
      [(number? x) (number->string x)]
      [(symbol? x) (symbol->string x)]
      [(list? x) (letrec ([list-items-repr
                           (lambda (l)
                             (cond
                               [(empty? l) ""]
                               [(empty? (rest l)) (repr (first l))]
                               [else (string-append (repr (first l))
                                                     ", "
                                                     (list-items-repr (rest l)))]))])
                   (string-append "["
                                  (list-items-repr x)
                                  "]"))]
      [else (format "~a" x)]))
  
  ;; a tuple is:
  ;; (make-tuple list)
  (define-struct tuple (list) (make-inspector))
  
  
  ;; py-print: (listof X) -> void
  (define (py-print lst)
    (for-each (lambda (x)
                (display x) (display #\space))
              lst)
    (newline))

  
  (define-syntax py-suite
    (lambda (stx)
      (syntax-case stx ()
        [(_ statement-list)
         (let ([stmts (syntax statement-list)])
           (datum->syntax-object stmts
                                 `(call-with-current-continuation
                                   (lambda (py-return)
                                     (for-each (lambda (stmt)
                                                 (eval stmt))
                                               ,(syntax-e stmts))))))])))
  
  )