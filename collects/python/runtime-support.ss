(module runtime-support mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           ;"primitives.ss"
           "python-import.ss"
           "c-bindings.ss"
           "python-node.ss"
           (lib "contract.ss")
           )
  
  
;  (require-for-syntax "compiler.ss") ;; get the compiler context
  (provide get-py-string ;; from C bindings
           get-py-number
           get-py-list
           get-py-tuple
           get-py-dict
           make-py-list
           make-py-string
           make-py-symbol
           make-py-tuple
           make-py-number
           make-py-code
           make-py-function
           make-py-dict
           py-object->py-string
           (rename spy-cpython-apply py-apply)
           py-is-a? ;(rename spy-cpython-instanceof py-is-a?)
           (rename cpy-none py-none)
           py-number?
           py-list?
           python-node?
           (rename spy-cpython-getattr/obj py-get-attr/obj)
           (rename spy-cpython-getattr/sym py-get-attr/sym)
           (rename spy-cpython-getattr/str py-get-attr/str)
           (rename spy-cpython-setattr/obj py-set-attr/obj)
           (rename spy-cpython-setattr/sym py-set-attr/sym)
           (rename spy-cpython-setattr/str py-set-attr/str)
           (rename spy-cpython-add py-add)
           (rename spy-cpython-sub py-sub)
           (rename spy-cpython-mul py-mul)
           (rename spy-cpython-div py-div)
           py-call/cc
           ;; types
           cpy-object
           cpy-str
           cpy-list
           cpy-type
           cpy-tuple
           cpy-dict
           ;;;;; pure Scheme
           ==
           py>
           py<
           py-compare
           py-print
           py-if
           py-not
           current-runtime-support-context
           current-toplevel-context)
           
  
  ;;;;;;;;;; Python Runtime Support by Daniel ;;;;;;;;;
  
  (define py-number%->number get-py-number)
  (define py-string%->string get-py-string)
  (define py-list%->list get-py-list)
  (define py-file%->port get-py-file)
  (define py-is-a? spy-cpython-instanceof)
  (define py-number? spy-cpython-number?)
  (define py-string? spy-cpython-string?)
  (define py-list? spy-cpython-list?)
  (define python-get-type-name spy-cpython-get-type-name)
  
  (define (py-none? x) (eqv? x cpy-none))
  
  (define (my-append lst item)
    (spy-cpython-apply (spy-cpython-getattr/sym lst 'append) item))

  (provide/contract [my-append ((and/c py-list?
                                       (lambda (lst)
                                         (< (spy-cpython-len/scm lst)
                                            5)))
                                python-node?
                                . -> .
                                py-none?)])
  
  (define (py-call/cc py-fn)
    (let/cc k
      (spy-cpython-apply py-fn (make-py-function (make-py-code 'kontinuation
                                                      k
                                                      1
                                                      null)))))
  
  
  
  (define (py-object%->string x)
    (get-py-string (py-object->py-string x)))
  
  (define (bool->py-number% b)
    (make-py-number (if b 1 0)))
  
  (define (py-object%->bool obj)
    (cond
      [(py-number? obj) (not (zero? (get-py-number obj)))]
      [(py-list? obj) (not (zero? (spy-cpython-len/scm obj)))]
      [else (error 'py-object%->bool "Can't handle this object yet: ~a" (py-object%->string obj))]))
  
  ;; ==: X X -> bool
  (define(== a b)
    ;(printf "equal equal~n")
    (cond
      [(py-number? a) (and (py-number? b)
                                    (= (py-number%->number a)
                                       (py-number%->number b)))]
      [(py-string? a) (and (py-string? b)
                                    (string=? (py-string%->string a)
                                              (py-string%->string b)))]
      [(py-list? a) (and (py-list? b)
                                  (andmap ==
                                          (py-list%->list a)
                                          (py-list%->list b)))]
      [else (error (format "No runtime support to compare ~a and ~a yet"
                           (python-get-type-name a)
                           (python-get-type-name b)))]))
  
  (define scheme> >)
  
  (define (py> a b)
    (printf "py>: is ~a greater than ~a ? " (py-object%->string a) (py-object%->string b))
    (let ([res 
    (cond
      [(py-number? a) (and (py-number? b)
                                    (scheme> (py-number%->number a)
                                             (py-number%->number b)))]
      [(py-string? a) (and (py-string? b)
                                    (string>? (py-string%->string a)
                                              (py-string%->string b)))]
      [else (error (format "No runtime support to compare ~a and ~a yet") ;(scheme> a b)]))
                   (python-get-type-name a)
                   (python-get-type-name b))])
    ])
      (printf "~a~n" res)
      res))
                    
                    
  (define (py< a b)
    (and (not (== a b))
         (not (py> a b))))
  
  (define (py-compare x op y comp-lst)
    ;(printf "py-compare~n")
    (bool->py-number%
     (and (op x y)
          (or (null? comp-lst)
              (py-compare y
                          (first comp-lst)
                          (second comp-lst)
                          (cddr comp-lst))))))
  
  ;; py-print: (or py-file% #f) (listof X) -> void
  (define (py-print file lst)
    (parameterize ([current-output-port (if file
                                            (py-file%->port file)
                                            (current-output-port))])
      (for-each (lambda (x)
                  (display (py-string%->string (if (py-string? x)
                                                   x
                                                   (py-object->py-string x))))
                  (display #\space))
                lst)
      (newline)))
  
  (define-syntax (build-class-body-entry stx)
      (datum->syntax-object stx
                            (syntax-case stx (define)
                              [(_ (define method-name procedure))
                               `(list ',(syntax method-name) ,(syntax procedure))])
                            stx
                            stx))
       

  
  (define-syntax py-if
    (lambda (stx)
      (syntax-case stx ()
        [(_ test then else) #`(if (let ([test-evald test])
                                    (py-object%->bool test-evald))
                                  then
                                  else)]
        [(_ test then) #`(when (let ([test-evald test])
                                   (py-object%->bool test-evald))
                           then)])))
  
;  (define-syntax (py-not stx)
;    (syntax-case stx ()
;      [(_ expr) #`(py-if expr (number->py-number% 0) (number->py-number% 1))]))

  (define (py-not x)
    (bool->py-number% (not (py-object%->bool x))))
  
  (define-syntax (build-class-body stx)
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
                                ;; make sure the "define" or "set!" here is introduced by the compiler
                                (or (free-identifier=? (datum->syntax-object #'here ;runtime-context
                                                                             'define)
                                                       (syntax __))
                                    (free-identifier=? (datum->syntax-object #'here
                                                                             'set!)
                                                       (syntax __)))
                                `(list ',(syntax method-name) ,(syntax procedure))]
                               ;; if it's not a member def, create a thunk and execute it later
                               [expr `(lambda () ,(syntax expr))])
                             d-or-e d-or-e))
                          defs-and-exprs))]))
       stx stx))
  

  (define current-runtime-support-context (make-parameter #f))
  (define current-toplevel-context (make-parameter #f))
 
  )