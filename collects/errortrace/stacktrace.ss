(module stacktrace mzscheme
  (require (lib "unitsig.ss")
           (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax")
           (lib "shared.ss" "stepper" "private")
           (lib "marks.ss" "stepper" "private"))
  
  (provide stacktrace@ stacktrace^ stacktrace-imports^)
  
  (define-signature stacktrace-imports^ (with-mark
                                         profile-key
                                         profiling-enabled
                                         initialize-profile-point
                                         register-profile-start
                                         register-profile-done))
  (define-signature stacktrace^ (annotate-top))
  
  (define stacktrace@
    (unit/sig stacktrace^
      (import stacktrace-imports^)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Profiling instrumenter
      
      ;; profile-point : (syntax symbol-or-#f syntax boolean -> syntax)
      
      ;; This procedure is called by `annotate' and `annotate-top' to wrap
      ;; expressions with profile collecting information.  Returning the
      ;; first argument means no profiling information is collected.
      
      ;; The second argument is the point's inferred name, if any, the third
      ;; argument is the source expression, and the fourth argument is #t for
      ;; a transformer expression and #f for a normal expression.
      
      (define (profile-point body name expr trans?)
        (if (profiling-enabled)
            (let ([key (gensym 'profile-point)])
              (initialize-profile-point key name expr)
              (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
                            [start (datum->syntax-object #f (gensym) (quote-syntax here))]
                            [profile-key (datum->syntax-object #f profile-key (quote-syntax here))]
                            [register-profile-start register-profile-start]
                            [register-profile-done register-profile-done])
                (with-syntax ([rest 
                               (insert-at-tail*
                                (syntax (register-profile-done 'key start))
                                body
                                trans?)])
                  (syntax
                   ((let ([start (register-profile-start 'key)])
                      (with-continuation-mark 'profile-key 'key
                                              (begin . rest))))))))
            body))
      
      (define (insert-at-tail* e exprs trans?)
        (if (stx-null? (stx-cdr exprs))
            (list (insert-at-tail e (stx-car exprs) trans?))
            (cons (stx-car exprs) (insert-at-tail* e (stx-cdr exprs) trans?))))
      
      (define (insert-at-tail se sexpr trans?)
        (with-syntax ([expr sexpr]
                      [e se])
          (kernel-syntax-case sexpr trans?
            ;; negligible time to eval
            [id
             (identifier? sexpr)
             (syntax (begin e expr))]
            [(quote _) (syntax (begin e expr))]
            [(quote-syntax _) (syntax (begin e expr))]
            [(#%datum . d) (syntax (begin e expr))]
            [(#%top . d) (syntax (begin e expr))]
            
            ;; No tail effect, and we want to account for the time
            [(lambda . _) (syntax (begin0 expr e))]
            [(case-lambda . _) (syntax (begin0 expr e))]
            [(set! . _) (syntax (begin0 expr e))]
            
            [(let-values bindings . body)
             (with-syntax ([rest (insert-at-tail* se (syntax body) trans?)])
               (syntax (let-values bindings . rest)))]
            [(letrec-values bindings . body)
             (with-syntax ([rest (insert-at-tail* se (syntax body) trans?)])
               (syntax (letrec-values bindings . rest)))]
            
            [(begin . _)
             (insert-at-tail* se sexpr trans?)]
            [(with-continuation-mark . _)
             (insert-at-tail* se sexpr trans?)]
            
            [(begin0 body ...)
             (syntax (begin0 body ... e))]
            
            [(if test then)
             (with-syntax ([then2 (insert-at-tail se (syntax then) trans?)])
               (syntax (if test then2)))]
            [(if test then else)
             ;; WARNING: e inserted twice!
             (with-syntax ([then2 (insert-at-tail se (syntax then) trans?)]
                           [else2 (insert-at-tail se (syntax else) trans?)])
               (syntax (if test then2 else2)))]
            
            [(#%app . rest)
             (if (stx-null? (syntax rest))
                 ;; null constant
                 (syntax (begin e expr))
                 ;; application; exploit guaranteed left-to-right evaluation
                 (insert-at-tail* se sexpr trans?))]
            
            [_else
             (error 'errortrace
                    "unrecognized (non-top-level) expression form: ~e"
                    (syntax-object->datum sexpr))])))
      
      
      ;; ANNOTATION
      
      ;; TEMPLATE FUNCTIONS:
      ;;  these functions' definitions follow the data definitions presented in the Syntax
      ;;  chapter of the MzScheme Manual. 
      
      (define (annotate-top-level-expr stx trans?)
        (let ([rebuild
               (lambda (new-stx) 
                 (rebuild-stx (syntax->list new-stx) stx))])
          (kernel-syntax-case stx #f
            [(module identifier name (#%plain-module-begin . module-level-exprs))
             (rebuild #`(module identifier name 
                          (#%plain-module-begin 
                           #,@(map (lambda (expr) (annotate-module-level-expr expr trans?))
                                   (syntax->list #'module-level-exprs)))))]
            [else-stx
             (annotate-general-top-level-expr stx trans?)])))
      
      (define (annotate-module-level-expr stx trans?)
        (kernel-syntax-case stx #f
          [(provide . provide-specs)
           stx]
          [else-stx
           (annotate-general-top-level-expr stx trans?)]))
      
      (define (annotate-general-top-level-expr stx trans?)
        (let ([rebuild 
               (lambda (new-stx) 
                 (rebuild-stx (syntax->list new-stx) stx))])
          (kernel-syntax-case stx #f
            [(define-values (var ...) expr)
             (rebuild #`(define-values (var ...) #,(annotate-expr #`expr
                                                                  (syntax-case #`(var ...) ()
                                                                    [(var) #`var]
                                                                    [_ #f])
                                                                  trans?)))]
            [(define-syntaxes (var ...) expr)
             (rebuild #`(define-syntaxes (var ...) #,(annotate-expr #`expr 
                                                                    (syntax-case #`(var ...) ()
                                                                      [(var) #`var]
                                                                      [_ #f])
                                                                    #t)))]
            [(begin . top-level-exprs)
             (rebuild #`(begin #,@(map (lambda (expr) (annotate-top-level-expr expr trans?))
                                       (syntax->list #'top-level-exprs))))]
            [(require . require-specs)
             stx]
            [(require-for-syntax . require-specs)
             stx]
            [else
             (annotate-expr stx #f trans?)])))
      
      ;; annotate-expr is written using a functional visitor. That is, 
      ;; annotate-expr is the visitor, and `expose' is a function that,
      ;; given a syntax-object, produces a cons-tree of sub-expressions
      ;; paired with contextual information and also a `rebuild' closure
      ;; that knows how to take the cons-tree back into the desired syntax
      ;; object.
      ;;
      ;; the stx-info structure represents the contextual information revealed
      ;; by expose about subexpressions.
      
      (define (annotate-expr expr inferred-name trans?)
        (let-values ([(annotated _)
                      ((annotate-from-info null ; unimportant when tailness = `lambda-body & resulting
                                           ; free-vars are ignored
                                           trans?)
                       (make-stx-info expr 
                                      `lambda-body ; this tailness causes full capture
                                      null ; new-bindings does not affect computation
                                      ; when tailness = 'lambda-body & resulting
                                      ; free-vars are ignored.
                                      inferred-name))])
          annotated))
      
      (define-struct stx-info (stx tailness new-bindings inferred-name))
      
      (define (annotate-from-info prior-tail-bound trans?)
        (lambda (info)
          (let*-values 
              ([(src) (stx-info-stx info)]
               [(local-free-vars) (free-vars-of src trans?)]
               [(my-tail-bound) (tail-bound prior-tail-bound 
                                            (stx-info-new-bindings info)
                                            (stx-info-tailness info))]
               [(pieces rebuilder) (expose src trans?)]
               [(annotated-tree sub-free-vars) (cons-tree-map/fv (annotate-from-info my-tail-bound trans?)
                                                                 pieces trans?)]
               [(free-here) (varref-set-union (list local-free-vars sub-free-vars))]
               [(free-for-parent) (varref-set-remove-bindings free-here (stx-info-new-bindings info))]
               [(rebuilt) (rebuilder annotated-tree)]
               [(profiled) (if (eq? (stx-info-tailness info) 'lambda-body)
                               (profile-point rebuilt (stx-info-inferred-name info) src trans?)
                               rebuilt)]
               [(mark-maker) (lambda (label) 
                               (make-debug-info src my-tail-bound free-here label #f))])
            (if (should-be-annotated? info (null? pieces) trans?)
                (values (with-mark src mark-maker profiled)
                        free-for-parent)
                (values rebuilt
                        free-for-parent)))))
      
      ;; cons-tree-map/fv walks over a cons tree, building a new one by applying fn to each
      ;; non-cons non-null item.  fn is also expected to return a list of free-vars.  These
      ;; free-vars are accumulated with union-vars as the tree is built.
      
      (define (cons-tree-map/fv fn tree trans?)
        (cond [(null? tree) (values null null)]
              [(pair? tree) (let*-values ([(lhs-ann lhs-fv) (cons-tree-map/fv fn (car tree) trans?)]
                                          [(rhs-ann rhs-fv) (cons-tree-map/fv fn (cdr tree) trans?)])
                              (values (cons lhs-ann rhs-ann) (varref-set-union (list lhs-fv rhs-fv))))]
              [else (fn tree)]))
      
      ;; expose : (syntax-object? boolean? -> (values stx-info? (cons-tree? -> syntax-object?))
      ;; as described above, expose takes a syntax object and returns two values; a 
      ;; cons-tree of stx-info structs, and a rebuilder procedure that takes a cons-tree
      ;; with that shape back into the syntactic form it came from.  The stx-info structure
      ;; contains the syntax object itself, and associated contextual information necessary
      ;; for annotation: is it tail, non-tail, or a lambda body? what new bindings is it tail
      ;; w.r.t.?
      
      (define (expose stx trans?)
        (let* ([tr-info (lambda (stx) 
                          (make-stx-info stx 'tail null #f))]        ; tail position
               [nt-info (lambda (stx) 
                          (make-stx-info stx 'non-tail null #f))]    ; non-tail position
               [lb-info (lambda (bindings) 
                          (lambda (stx)
                            (make-stx-info stx 'lambda-body bindings #f)))] ; lambda body
               [lt-info (lambda (bindings) 
                          (lambda (stx)
                            (make-stx-info stx 'tail bindings #f)))]     ; body of a let
               [rh-info (lambda (bindings)
                          (lambda (stx inferred-name)
                            (make-stx-info stx 'non-tail bindings inferred-name)))] ; let rhs
               [begin-info
                (lambda (stx)
                  (let* ([bodies (syntax->list stx)]
                         [reversed-bodies (reverse bodies)]
                         [last-body (car reversed-bodies)]
                         [all-but-last (reverse! (cdr reversed-bodies))])
                    (append (map nt-info all-but-last) (list (tr-info last-body)))))]
               [begin0-info
                (lambda (stx)
                  (map nt-info (syntax->list stx)))]
               [rebuild
                (lambda (new-stx) 
                  (rebuild-stx (syntax->list new-stx) stx))]
               [seq-rebuilder
                (lambda (token)
                  (lambda (stx-list)
                    (rebuild #`(#,token #,@stx-list))))]
               [lambda-clause-abstraction
                (lambda (clause)
                  (kernel-syntax-case clause #f
                    [(arglist . bodies)
                     (values
                      (map (lb-info (arglist-bindings #'arglist)) 
                           (syntax->list #'bodies))
                      #`arglist)]
                    [else
                     (error 'expr-syntax-object-iterator 
                            "unexpected (case-)lambda clause: ~a" 
                            (syntax-object->datum stx))]))]
               [let-values-abstraction
                (lambda (stx rec? token)
                  (kernel-syntax-case stx #f
                    [(kwd (((variable ...) rhs) ...) . bodies)
                     (let* ([new-bindings 
                             (varref-set-union 
                              (map syntax->list 
                                   (syntax->list #'((variable ...) ...))))]
                            [rhs-new-bindings (if rec? new-bindings null)]
                            [inferred-names (map (lambda (lhs)
                                                   (syntax-case lhs ()
                                                     [(var) #`var]
                                                     [_ #f]))
                                                 (syntax->list #`((variable ...) ...)))])
                       (values (cons (map (rh-info rhs-new-bindings) 
                                          (syntax->list #'(rhs ...))
                                          inferred-names)
                                     (map (lt-info new-bindings)
                                          (syntax->list #'bodies)))
                               (lambda (annotateds)
                                 (with-syntax ([(rhs ...) (car annotateds)]
                                               [(body ...) (cdr annotateds)])
                                   (rebuild #`(#,token ([(variable ...) rhs] ...) body ...))))))]
                    [else
                     (error 'errortrace 
                            "unexpected let(rec) expression: ~a"
                            (syntax-object->datum stx))]))]) 
          (kernel-syntax-case stx trans?
            [var-stx
             (identifier? (syntax var-stx))
             (values null (lambda (_) stx))]
            [(lambda . clause)
             (let-values ([(subexps arglist) (lambda-clause-abstraction #`clause)])
               (values subexps
                       (lambda (annotateds)
                         (rebuild #`(lambda #,arglist #,@annotateds)))))]
            [(case-lambda . clauses)
             (let-values ([(subexps-list arglist-list) (values-map lambda-clause-abstraction (syntax->list #`clauses))])
               (values subexps-list 
                       (lambda (annotateds-list)
                         (with-syntax ([(arglist ...) arglist-list]
                                       [(bodies ...) annotateds-list])
                           (rebuild #`(case-lambda (arglist . bodies) ...))))))]
            [(if test then)
             (values (list (nt-info #`test) (tr-info #`then))
                     (seq-rebuilder #`if))]
            [(if test then else)
             (values (list (nt-info #`test) (tr-info #`then) (tr-info #`else))
                     (seq-rebuilder #`if))]
            [(begin . bodies)
             (values (begin-info #`bodies)
                     (seq-rebuilder #`begin))]
            [(begin0 . bodies)
             (values (begin0-info #`bodies)
                     (seq-rebuilder #`begin0))]
            [(let-values . _)
             (let-values-abstraction stx #f #`let-values)]
            [(letrec-values . _)
             (let-values-abstraction stx #t #`letrec-values)]
            [(set! var val)
             (values (nt-info #`val)
                     (lambda (rhs)
                       (rebuild #`(set! var #,rhs))))]
            [(quote _)
             (values null (lambda (_) stx))]
            [(quote-syntax _)
             (values null (lambda (_) stx))]
            [(with-continuation-mark key mark body)
             (values (list (nt-info #`key) (nt-info #`mark) (tr-info #`body))
                     (seq-rebuilder #`with-continuation-mark))]
            [(#%app . exprs)
             (values (begin0-info #`exprs)
                     (seq-rebuilder #`#%app))]
            [(#%datum . _)
             (values null (lambda (_) stx))]
            [(#%top . var)
             (values null (lambda (_) stx))]
            [else
             (error 'expr-syntax-object-iterator "unknown expr: ~a" 
                    (syntax-object->datum stx))])))
      
      
      ; arglist-bindings : (syntax? -> (listof syntax?))
      ;  return a list of the names in the arglist
      
      (define (arglist-bindings arglist-stx)
        (syntax-case arglist-stx ()
          [var
           (identifier? arglist-stx)
           (list arglist-stx)]
          [(var ...)
           (syntax->list arglist-stx)]
          [(var . others)
           (cons #'var (arglist-bindings #'others))]))
      
      ; tail-bound : (binding-set? (listof syntax?) symbol -> binding-set?)
      ;  prior: the variables that were tail-bound in the enclosing expression
      ;  newly-bound: the variables whose bindings were created in the enclosing 
      ;               expression
      ;  tailness: 'lambda-body if this expression is the body of a lambda,
      ;            'tail if this expression is tail wrt the enclosing expression, and
      ;            'non-tail otherwise
      
      (define (tail-bound prior newly-bound tailness)
        (case tailness
          ((lambda-body) 'all)
          ((tail) (binding-set-union (list prior newly-bound)))
          ((non-tail) newly-bound)
          (else (error 'tail-bound "unexpected value ~s for tailness argument" 
                       tailness))))
      
      ; should-be-annotated? : (stx-info? boolean? boolean? -> boolean?)
      ; decides whether an expression should be annotated. 
      
      (define (should-be-annotated? info no-subexps? trans?)
        (if no-subexps?
            ;; wrap only if an error could occur:
            (kernel-syntax-case (stx-info-stx info) trans?
              [var-stx
               (identifier? (syntax var-stx))
               (not (eq? 'lexical (identifier-binding #`var-stx)))]
              [(#%top . var) #t]
              [else #f])
            ;; wrap if it could cause an error or if it has new bindings or if it's the body of a lambda:
            (or (kernel-syntax-case (stx-info-stx info) trans?
                  [(set! v body) #t]
                  [(#%app . _) #t]
                  [_ #f])
                (not (null? (stx-info-new-bindings info)))
                (eq? (stx-info-tailness info) 'lambda-body))))
      
      ; free-vars-of: (syntax? boolean? -> (listof syntax?))
      ;  if this expression is a variable reference or a set! of some variable, return it in a list. 
      ;  otherwise return null.
      
      (define (free-vars-of stx trans?)
        (kernel-syntax-case stx trans?
          [(#%top . var)
           (list #`var)]
          [var-stx
           (identifier? stx)
           (list stx)]
          [(set! v body)
           (list #`v)]
          [_
           null]))
      
      (define annotate-top annotate-top-level-expr))))
