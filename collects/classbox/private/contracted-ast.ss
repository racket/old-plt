(module contracted-ast mzscheme
  
  (require
   (lib "contract.ss")
   (lib "mred.ss" "mred")
   (lib "ast.ss" "profj"))
  
  (define loc? any?)
  (define gj-info? any?)
  
  (define member?
    (union var-decl?
           var-init?
           method?
           initialize?
           class-def?
           interface-def?))
  
  (define expression?
    (union literal?
           bin-op?
           access?
           special-name?
           specified-this?
           call?
           class-alloc?
           array-alloc?
           cond-expression?
           array-access?
           post-expr?
           pre-expr?
           unary?
           cast?
           instanceof?
           assignment?))
  
  (define level? (symbols 'beginner 'intermediate 'advanced))
  (define (pos-int? v) (and (number? v) (integer? v) (positive? v)))
  (define opt-src? (union src? false?))
  
  (provide/contract
   (struct modifier ((kind symbol?) (src opt-src?)))
   (struct type-spec ((name (union name? type-var? symbol?)) (dim integer?) (src opt-src?)))
   (struct var-decl ((name id?)
                     (modifiers (listof modifier?))
                     (type type-spec?)
                     (src opt-src?)))
   (struct id ((string string?) (src opt-src?)))
   (struct name ((id id?) (path (listof id?)) (src opt-src?)))
   (struct src ((line (union pos-int? false?))
                (col (union natural-number? false?))
                (pos (union pos-int? false?))
                (span (union natural-number? false?))
                (file loc?)))
   (struct header ((id id?)
                   (modifiers (listof modifier?))
                   (extends (listof name?))
                   (implements (listof name?))
                   (type-parms (listof gj-info?))
                   (src opt-src?)))
   (make-class-def (header?
                    (listof member?)
                    opt-src?
                    opt-src?
                    loc?
                    level?
                    (listof req?)
                    symbol?
                   . -> .
                   class-def?))
   (struct method ((modifiers (listof modifier?))
                   (type type-spec?)
                   (type-parms (listof gj-info?))
                   (name id?)
                   (parms (listof var-decl?))
                   (throws (listof name?))
                   (body statement?)
                   (all-tail? boolean?)
                   (src opt-src?)))
   (struct block ((stmts (listof (union var-decl? var-init? statement?))) (src opt-src?)))
   (make-call ((union type-spec? false?)
               opt-src?
               (union expression? false?)
               (union name? special-name?)
               (listof expression?)
               (union any? #;method-record? false?)
               . -> .
               call?))
   (make-special-name ((union false? type-spec?) opt-src? string? . -> . special-name?))
   (make-assignment ((union false? type-spec?)
                     opt-src?
                     (union access? array-access?)
                     symbol?
                     expression?
                     opt-src?
                     . -> .
                     assignment?))
   (make-access ((union false? type-spec?)
                 opt-src?
                 (union (listof id?) field-access? local-access?)
                 . -> .
                 access?))
   (struct field-access ((object (union expression? false?))
                         (field id?)
                         (access (union var-access? false?))))
   (struct var-access ((static? boolean?)
                       (final? boolean?)
                       (init? boolean?)
                       (access symbol?)
                       (class string?))))
  )