(module runtime-support mzscheme
  (require (lib "list.ss")
           "primitives.ss")
  (require-for-syntax "compiler.ss") ;; get the compiler context
  (provide (all-defined))
  
  ;;;;;;;;;; Python Runtime Support by Daniel ;;;;;;;;;
  
  ;; ==: X X -> bool
  (define(== a b)
    (cond
      [(py-is-a? a py-number%) (and (py-is-a? b py-number%)
                                    (= (py-number%->number a)
                                       (py-number%->number b)))]
      [(py-is-a? a py-string%) (and (py-is-a? b py-string%)
                                    (string=? (py-string%->string a)
                                              (py-string%->string b)))]
      [(py-is-a? a py-list%) (and (py-is-a? b py-list%)
                                  (andmap ==
                                          (py-list%->list a)
                                          (py-list%->list b)))]
      [else (error (format "No runtime support to compare ~a and ~a yet"
                           (python-get-type-name a)
                           (python-get-type-name b)))]))
  
  (define scheme> >)
  
  (define (py> a b)
    (cond
      [(py-is-a? a py-number%) (and (py-is-a? b py-number%)
                                    (scheme> (py-number%->number a)
                                             (py-number%->number b)))]
      [else (scheme> a b)]))
  
  (define (py< a b)
    (and (not (== a b))
         (not (py> a b))))
  
  (define (py-compare x op y comp-lst)
    (and (op x y)
         (if (null? comp-lst)
             #t
             (py-compare y
                         (car comp-lst)
                         (car (cdr comp-lst))
                         (cdr (cdr comp-lst))))))
  
  ;; py-print: (listof X) -> void
  (define (py-print lst)
    (for-each (lambda (x)
                (display (py-string%->string (if (py-is-a? x py-string%)
                                                 x
                                                 (py-call py-repr (list x))))) (display #\space))
              lst)
    (newline))

  
  (define-syntax build-class-body-entry
    (lambda (stx)
      (datum->syntax-object stx
                            (syntax-case stx (define)
                              [(_ (define method-name procedure))
                               `(list ',(syntax method-name) ,(syntax procedure))])
                            stx
                            stx)))
       
  
  (define-syntax build-class-body
    (lambda (stx)
      (datum->syntax-object
       stx
       `(list ,@(syntax-case stx (define)
                  [(_ (def-or-expr ...))
                   (let ([defs-and-exprs (syntax->list (syntax (def-or-expr ...)))])
                     (map (lambda (d-or-e)
                            (datum->syntax-object
                             d-or-e
                             (syntax-case d-or-e (define)
                               [(__ member-name member-value)
                                ;; make sure the "define" here is introduced by the compiler
                                (free-identifier=? (datum->syntax-object compiler-context
                                                                         'define)
                                                   (syntax __))
                                `(list ',(syntax method-name) ,(syntax procedure))]
                               ;; if it's not a member def, create a thunk and execute it later
                               [expr `(lambda () ,(syntax expr))])
                             d-or-e d-or-e))
                          defs-and-exprs))]))
       stx stx)))
 
  )