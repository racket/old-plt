
(module pconvert mzscheme
  
  (require (prefix s: "string.ss")
           (prefix f: "list.ss"))
  (require "class.ss")
  (require "unit.ss")
  
  (provide show-sharing
           constructor-style-printing
           quasi-read-style-printing
           abbreviate-cons-as-list
           whole/fractional-exact-numbers
           booleans-as-true/false
           named/undefined-handler
           use-named/undefined-handler
           
           print-convert
           print-convert-expr
           build-share
           get-shared
           current-read-eval-convert-print-prompt 
           install-converting-printer
           
           current-build-share-name-hook
           current-build-share-hook
           current-print-convert-hook)
  
  (define undefined-val (letrec ([x x]) x))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; the value stored in the hash table.  Contains the name
  ;; <which is a number unless we are in donkey and it already has a name>
  ;; and whether or not it is shared in the expr.
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-struct share-info (name shared?))
  
  (define boolean-filter (lambda (x) (and x #t)))
  
  (define show-sharing (make-parameter #t boolean-filter))
  (define constructor-style-printing (make-parameter #f boolean-filter))
  (define quasi-read-style-printing (make-parameter #t boolean-filter))
  (define abbreviate-cons-as-list (make-parameter #t boolean-filter))
  (define whole/fractional-exact-numbers (make-parameter #f boolean-filter))
  (define booleans-as-true/false (make-parameter #t boolean-filter))
  (define use-named/undefined-handler (make-parameter (lambda (x) #f)))
  (define named/undefined-handler (make-parameter (lambda (x) #f)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; share-hash is the hash-table containing info on what cons cells
  ;; of the expression are shared.
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; sometimes you want to go ahead and start displaying a shared
  ;; expression rather than just showing its name.  For instance, in
  ;; the shared list, you want (shared ((-1- (list 1 2))... not
  ;; (shared ((-1- -1-) ...
  ;; expand-shared? controls this
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-struct convert-share-info (share-hash expand-shared?))
  
  (define current-build-share-name-hook 
    (make-parameter (let ([original-build-share-name-hook (lambda (e) #f)]) original-build-share-name-hook)
                    (lambda (f)
                      (unless (procedure-arity-includes? f 1)
                        (raise-type-error 'current-build-share-name-hook "procedure of arity 1" f))
                      f)))
  (define current-build-share-hook 
    (make-parameter (lambda (e base sub) (base e))
                    (lambda (f)
                      (unless (procedure-arity-includes? f 3)
                        (raise-type-error 'current-build-share-hook "procedure of arity 3" f))
                      f)))
  (define current-print-convert-hook 
    (make-parameter (lambda (e base sub) (base e))
                    (lambda (f)
                      (unless (procedure-arity-includes? f 3)
                        (raise-type-error 'current--hook "procedure of arity 3" f))
                      f)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; builds the hash table
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define build-share
    (lambda (expr)
      (letrec
          ([share-cnt 0]
           [share-hash (make-hash-table)]
           [csi (make-convert-share-info share-hash #f)]
           [hash
            (lambda (obj)
              (let ([name ((current-build-share-name-hook) obj)])
                (hash-table-put! share-hash obj
                                 (make-share-info (if name (car name) share-cnt) #f)))
              (set! share-cnt (add1 share-cnt)))]
           [build-sub
            (lambda (expr)
              (let/ec k
                (if (or (equal? expr "")
                        (equal? expr #()))
                    (k #f)
                    (let ([val (hash-table-get share-hash expr 
                                               (lambda ()
                                                 (hash expr)
                                                 (k #f)))])
                      (when val
                        (set-share-info-shared?! val #t))
                      val))))]
           [build
            (lambda (expr)
              ((current-build-share-hook)
               expr
               (lambda (expr)
                 (cond
                   [(or (number? expr)
                        (symbol? expr)
                        (boolean? expr)
                        (char? expr) (void? expr)
                        (null? expr)
                        (eq? expr undefined-val) ; #<undefined> test - yuck
                        )
                    'atomic]
                   [(and (not (struct? expr))  ;; struct names are the wrong thing, here
                         (not (regexp? expr))
                         (not (procedure? expr))
                         (object-name expr))
                    'atomic]
                   [(box? expr) 
                    (unless (build-sub expr)
                      (build (unbox expr)))]
                   [(hash-table? expr) 
                    (unless (build-sub expr)
                      (hash-table-for-each 
                       expr
                       (lambda (key value)
                         (build value))))]
                   [(pair? expr)  
                    (unless (build-sub expr)
                      (build (car expr))
                      (build (cdr expr)))]
                   [(vector? expr) 
                    (unless (build-sub expr)
                      (for-each build (vector->list expr)))]
                   [(struct? expr) 
                    (unless (build-sub expr)
                      (for-each build (vector->list (struct->vector expr))))]
                   [else (build-sub expr)]))
               build-sub))])
        (build expr)
        csi)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; creates a distinctive symbol out of a name (usually just a number)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define map-share-name
    (lambda (name)
      (string->symbol
       (string-append "-" (s:expr->string name) "-"))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; prints an expression given that it has already been hashed. This
  ;; does not include the list of shared items.
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define print-convert-expr
    (lambda (csi expr unroll-once?)
      (letrec
          ([share-hash (convert-share-info-share-hash csi)]
           [find-hash
            (lambda (expr)
              (hash-table-get share-hash expr (lambda () #f)))]
           [shared?
            (lambda (expr)
              (let* ([info (find-hash expr)]
                     [ans (and info
                               (share-info-shared? info))])
                ans))]
           
           ;; make-syntax-name : -> string
           ;; creates a distinctive symbol print-converting syntax.
           [make-syntax-name
            (let ([n 0])
              (lambda ()
                (set! n (+ n 1))
                (string->symbol (format "=~a=" n))))]
           
           [make-list
            (lambda (f n)
              (letrec ([helper
                        (lambda (n l)
                          (cond [(zero? n) l]
                                [else (helper (sub1 n) (cons (f n) l))]))])
                (helper n null)))]
           [make-lambda-helper
            (lambda (arity)
              (cond
                [(arity-at-least? arity)
                 (let ([v (arity-at-least-value arity)])
                   (if (zero? v)
                       'args
                       (append (make-lambda-helper v) 'args)))]
                [(list? arity)
                 (map (lambda (x)
                        (list (make-lambda-helper x) '...))
                      arity)]
                [else (make-list
                       (lambda (x)
                         (string->symbol
                          (string-append "a" (number->string x))))
                       arity)]))]
           [use-quasi-quote? (not (constructor-style-printing))]
           [use-read-syntax (quasi-read-style-printing)]
           [doesnt-contain-shared-conses
            (lambda (input-expr)
              (letrec ([doesnt-contain-shared-conses
                        (lambda (expr)
                          (cond
                            [(and (pair? expr)
                                  (shared? expr))
                             #f]
                            [(pair? expr)
                             (doesnt-contain-shared-conses (cdr expr))]
                            [else #t]))])
                (let ([answer (doesnt-contain-shared-conses input-expr)])
                  answer)))]
           [get-whole/frac
            (lambda (exact-num)
              (let ([split 
                     (lambda (real)
                       (let* ([num (numerator (abs real))]
                              [den (denominator (abs real))]
                              [sign (if (< real 0) - +)])
                         (values (sign (quotient num den))
                                 (sign (* (if (negative? num) -1 1)
                                          (/ (modulo num den) den))))))])
                (let-values ([(whole frac) (split (real-part exact-num))]
                             [(whole-i frac-i) (split (imag-part exact-num))])
                  (values whole frac whole-i frac-i))))]
           [print
            (lambda (in-quasiquote? first-time)
              (lambda (expr)
                (letrec
                    ([lookup (find-hash expr)]
                     [recur (print in-quasiquote? #f)]
                     [self-quoting?
                      (lambda (expr)
                        (or (and (number? expr)
                                 (or (inexact? expr)
                                     (not (whole/fractional-exact-numbers))
                                     (and (real? expr)
                                          (or (let-values ([(whole frac whole-i frac-i) 
                                                            (get-whole/frac expr)])
                                                (and (or (zero? whole)
                                                         (zero? frac))))))))
                            (and (symbol? expr)
                                 (not (eq? expr 'quasiquote))
                                 (not (eq? expr 'quote))
                                 (not (eq? expr 'unquote))
                                 (not (eq? expr 'quote-syntax))
                                 (not (eq? expr 'syntax)))
                            (char? expr)
                            (string? expr)
                            (not expr)
                            (eq? #t expr)))]
                     [quasi-read-style
                      (lambda ()
                        (cond
                          [(box? expr) (box (recur (unbox expr)))]
                          [(vector? expr) (apply vector (map recur (vector->list expr)))]
                          [else (quasi-style)]))]
                     [quasi-style
                      (lambda ()
                        (cond
                          [(null? expr) '()]
                          [(and (pair? expr)
                                (pair? (cdr expr))
                                (null? (cddr expr))
                                (or (eq? (car expr) 'quote)
                                    (eq? expr 'quasiquote)
                                    (eq? expr 'quote)
                                    (eq? expr 'unquote)
                                    (eq? expr 'quote-syntax)
                                    (eq? expr 'syntax)))
                           `(,(car expr) ,(recur (cadr expr)))]
                          [(and (list? expr)
                                (doesnt-contain-shared-conses expr))
                           (map recur expr)]
                          [(pair? expr) 
                           (cons (recur (car expr)) (recur (cdr expr)))]
                          [(self-quoting? expr) expr]
                          [else `(,'unquote ,((print #f first-time) expr))]))]
                     
                     [syntax-style
                      (lambda ()
                        (cond
                          [(or (pair? expr)
                               (null? expr))
                           ((print in-quasiquote? #f) (datum->syntax-object #f expr))]
                          [else
                           (void)]))]
                     
                     [guard/quasiquote
                      (lambda (f)
                        (cond
                          [use-quasi-quote?
                           `(,'quasiquote ,(if use-read-syntax
                                               ((print #t first-time) expr)
                                               ((print #t first-time) expr)))]
                          [else
                           (f)]))]
                     [constructor-style
                      (let ([build-named
                              (lambda (expr build-unnamed)
                                (let ([answer (and (not (struct? expr))
                                                   (object-name expr))])
                                  (cond
                                    [(not answer)
                                     (build-unnamed)]
                                    [(eq? (with-handlers ([not-break-exn? 
                                                           (lambda (x) #f)])
                                            (eval answer))
                                          expr)
                                     answer]
                                    [((use-named/undefined-handler) expr)
                                     ((named/undefined-handler) expr)]
                                    [else
                                     (build-unnamed)])))])
                        (lambda ()
                          ((current-print-convert-hook)
                           expr
                           (lambda (expr)
                             (cond
                               [(null? expr) (guard/quasiquote (lambda () 'empty))]
                               [(and (list? expr)
                                     (abbreviate-cons-as-list)
                                     (or (and first-time
                                              (doesnt-contain-shared-conses (cdr expr)))
                                         (doesnt-contain-shared-conses expr)))
                                (guard/quasiquote
                                 (lambda ()
                                   `(list ,@(map recur expr))))]
                               [(pair? expr)
                                (guard/quasiquote
                                 (lambda ()
                                   `(cons ,(recur (car expr)) ,(recur (cdr expr)))))]
                               [(weak-box? expr) `(make-weak-box ,(recur (weak-box-value expr)))]
                               [(box? expr) `(box ,(recur (unbox expr)))]
                               [(hash-table? expr) `(make-hash-table)]
                               [(vector? expr) `(vector ,@(map recur (vector->list expr)))]
                               [(symbol? expr) `',expr]
                               [(string? expr) expr]
                               [(primitive? expr) (object-name expr)]
                               [(procedure? expr)
                                (build-named
                                 expr
                                 (lambda ()
                                   (let ([arity (procedure-arity expr)])
                                     (if (list? arity)
                                         `(case-lambda . ,(make-lambda-helper arity))
                                         `(lambda ,(make-lambda-helper arity) ...)))))]
                               [(regexp? expr) `(regexp ,(or (object-name expr)
                                                             '...))]
                               [(syntax? expr) 
                                (let* ([objs null]
                                       [names null]
                                       [datum
                                        (let loop ([expr expr])
                                          (cond
                                            [(syntax? expr)
                                             (let ([datum (syntax-e expr)])
                                               (cond
                                                 [(null? datum) null]
                                                 [(self-quoting? datum) datum]
                                                 [(pair? datum)
                                                  (let ([lst (syntax->list expr)])
                                                    (cond
                                                      [(and lst
                                                            (= (length lst) 2)
                                                            (symbol? (syntax-e (car lst))))
                                                       (list (syntax-e (car lst))
                                                             (loop (cadr lst)))]
                                                      [else (improper-map loop datum)]))]
                                                 [else 
                                                  (let ([name (make-syntax-name)])
                                                    (set! names (cons name names))
                                                    (set! objs (cons (recur datum)
                                                                     objs))
                                                    name)]))]
                                            [else (recur expr)]))])
                                  (if (null? objs)
                                      `(syntax ,datum)
                                      `(with-syntax (,@(map (lambda (obj name) `[,name ,obj])
                                                            (reverse objs)
                                                            (reverse names)))
                                         (syntax ,datum))))]
                               [(module-path-index? expr) 
                                (let-values ([(left right) (module-path-index-split expr)])
                                  `(module-path-index-join ,(recur left) ,(recur right)))]
                               [(interface? expr) `(interface ...)]
                               [(class? expr) 
                                (build-named 
                                 expr
                                 (lambda () '(class ...)))]
                               [(object? expr) `(make-object
                                                    ,(build-named 
                                                      (object-interface expr)
                                                      (lambda () '(class ...)))
                                                  ...)]
                               [(void? expr) '(void)]
                               [(promise? expr) '(delay ...)]
                               [(unit? expr) (build-named 
                                              expr
                                              (lambda () 
                                                '(unit ...)))]
                               [(struct? expr)
                                (let ([name (symbol->string
                                             (vector-ref (struct->vector expr) 0))])
                                  `(,(string->symbol
                                      (string-append
                                       "make-" (substring name
                                                          (string-length "struct:")
                                                          (string-length name))))
                                    ,@(map recur (cdr (vector->list
                                                       (struct->vector expr))))))]
                               [(and (number? expr) (exact? expr))
                                (let-values ([(whole frac whole-i frac-i) (get-whole/frac expr)])
                                  (cond
                                    [(not (whole/fractional-exact-numbers)) expr]
                                    [(and (or (zero? whole)
                                              (zero? frac))
                                          (zero? whole-i)
                                          (zero? frac-i))
                                     expr]
                                    [(real? expr) `(+ ,whole ,frac)]
                                    [(and (or (zero? whole) (zero? frac))
                                          (or (zero? whole-i) (zero? frac-i)))
                                     `(+ ,(real-part expr) (* +1i ,(imag-part expr)))]
                                    [(or (zero? whole-i) (zero? frac-i))
                                     `(+ (+ ,whole ,frac) (* +1i ,(imag-part expr)))]
                                    [(or (zero? whole) (zero? frac))
                                     `(+ ,(real-part expr) (* +1i (+ ,whole-i ,frac-i)))]
                                    [else `(+ (+ ,whole ,frac) (* +1i (+ ,whole-i ,frac-i)))]))]
                               [(eq? expr #f) (if (booleans-as-true/false) 'false #f)]
                               [(eq? expr #t) (if (booleans-as-true/false) 'true #t)]
                               [else expr]))
                           recur)))])
                  (let ([es (convert-share-info-expand-shared? csi)])
                    (set-convert-share-info-expand-shared?! csi #f)
                    (if (and lookup
                             (not es)
                             (not first-time)
                             (share-info-shared? lookup))
                        (let ([name (map-share-name (share-info-name lookup))])
                          (if in-quasiquote?
                              `(,'unquote ,name)
                              name))
                        (if in-quasiquote?
                            (if use-read-syntax
                                (quasi-read-style)
                                (quasi-style))
                            (constructor-style)))))))])
        ((print #f unroll-once?) expr))))
  
  ;; type (improper-list a) = (union (cons (improper-list a) (improper-list a)) null a)
  ;; improper-map : (a -> b) -> (improper-list a) -> (improper-list b)
  (define (improper-map f x)
    (cond
      [(pair? x) (cons (f (car x)) (improper-map f (cdr x)))]
      [(null? x) null]
      [else (f x)]))
  
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; these functions get the list of shared items.  If just-circular is
  ;; true, then it will modify the hash table so that the only shared
  ;; items are those that are circular.
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define get-shared-helper
    (lambda (csi)
      (let ([shared '()]
            [share-hash (convert-share-info-share-hash csi)])
        (hash-table-for-each share-hash
                             (lambda (key val)
                               (when (share-info-shared? val)
                                 (set! shared (cons (list key val) shared)))))
        (map (lambda (s)
               (set-convert-share-info-expand-shared?! csi #t)
               (let* ([info (cadr s)]
                      [name (share-info-name info)])
                 (list info
                       (map-share-name name)
                       (print-convert-expr csi (car s) #t))))
             shared))))
  
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  (define get-shared
    (case-lambda
      [(csi) (get-shared csi #f)]
      [(csi just-circular)
       (let ([shared-listss
              (if just-circular
                  (let ([shared (get-shared-helper csi)])
                    (for-each (lambda (x)
                                (unless (member* (cadr x) (caddr x))
                                  (set-share-info-shared?! (car x) #f)))
                              shared)
                    (get-shared-helper csi))
                  (get-shared-helper csi))]
             [cmp 
              (lambda (x y)
                (string<? (s:expr->string (share-info-name (car x)))
                          (s:expr->string (share-info-name (car y)))))])
         (map cdr (f:quicksort shared-listss cmp)))]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; helper function for determining if an item is circular.  In the
  ;; shared list: (shared ((-1- (list 1 2)) (-2- (list -2- 2 3)))), you
  ;; can tell by doing a member* of the first item on the second. In this
  ;; case, the second item in the shared list is circular because -2- appears
  ;; in the value
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define member*
    (lambda (a l)
      (cond [(or (not (pair? l)) (null? l)) #f]
            [(eq? a (car l)) #t]
            [else (or (member* a (car l)) (member* a (cdr l)))])))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; takes an expression and completely converts it to show sharing
  ;; (or if just-circular, just circularity) and special forms.
  ;; --------- THIS PROCEDURE IS EXPORTED ----------
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define print-convert
    (case-lambda
      [(expr) (print-convert expr (not (show-sharing)))]
      [(expr just-circ)
       (let* ([csi (build-share expr)])
         (let ([shared (get-shared csi just-circ)]
               [body (print-convert-expr csi expr #f)])
           (if (null? shared)
               body
               `(shared ,shared ,body))))]))
  
  (define current-read-eval-convert-print-prompt
    (make-parameter "|- "))
  
  (define install-converting-printer
    (lambda ()
      (let ([print (current-print)])
        (current-print (lambda (v)
                         (unless (void? v)
                           (print (print-convert v))))))
      (current-prompt-read (lambda ()
                             (display (current-read-eval-convert-print-prompt)))))))
