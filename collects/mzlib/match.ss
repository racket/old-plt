;; (documentation (name match))
;; <pre>Pattern Matching Syntactic Extensions for Scheme
;;
;; All bugs or questions concerning this software should be directed to
;; Bruce Hauman <bhauman@cs.wcu.edu>.  The latest version of this software
;; can be obtained from http://sol.cs.wcu.edu/~bhauman/scheme/pattern.php.
;;
;; Special thanks go out to:
;; Robert Bruce Findler for support and bug detection.
;; Doug Orleans for pointing out that pairs should be reused while
;; matching lists.
;;
;;
;; Originally written by Andrew K. Wright, 1993 (wright@research.nj.nec.com)
;; which in turn was adapted from code written by Bruce F. Duba, 1991.
;;
;; This software is in the public domain.  Feel free to copy,
;; distribute, and modify this software as desired.  No warranties
;; nor guarantees of any kind apply.  Please return any improvements
;; or bug fixes to bhauman@cs.wcu.edu so that they may be included
;; in future releases.
;;
;; This macro package extends Scheme with several new expression forms.
;; Following is a brief summary of the new forms.  See the associated
;; LaTeX documentation for a full description of their functionality.
;;
;;
;;         match expressions:
;;
;; exp ::= ...
;;       | (match exp clause ...)
;;       | (match-lambda clause ...) 
;;       | (match-lambda* clause ...)
;;       | (match-let ((pat exp) ...) body ...)
;;       | (match-let var ((pat exp) ...) body ...)
;;       | (match-let* ((pat exp) ...) body ...)
;;       | (match-letrec ((pat exp) ...) body ...)
;;       | (match-define pat exp)
;;
;; clause ::= (pat body) | (pat (=> identifier) exp)
;;
;;         patterns:                       matches:
;;
;; pat ::= 
;;         identifier                      this binds an identifier if it 
;;                                         doesn't conflict with
;;                                         ..k, var, $, =, and, 
;;                                         or, not, ?, set!, or get!
;;       | _                               anything
;;       | ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (lvp_1 ... lvp_n)               list of n elements
;;       | (pat ... pat_n . pat_{n+1})           list of n or more
;;       | #(lvp_1 ... lvp_n)              vector of n elements
;;       | #&pat                           box
;;       | ($ struct-name pat_1 ... pat_n) a structure
;;       | (= field pat)                   a field of a structure (field is 
;;                                         an accessor)
;;                                         Actually field can be any function 
;;                                         which can be
;;                                         applied to the data being matched.
;;                                         Ex: (match 5 ((= add1 b) b)) => 6
;;
;;       | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
;;       | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
;;       | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
;;       | (? predicate pat_1 ... pat_n)   if predicate true and all of
;;                                           pat_1 thru pat_n match
;;       | (set! identifier)               anything, and binds setter
;;       | (get! identifier)               anything, and binds getter
;;       | `qp                             a quasi-pattern
;;
;; lvp ::= pat ooo                         greedily matches n or more of pat, 
;;                                         each element must match pat
;;       | pat                             matches pat
;;
;; ooo ::= ...                             zero or more
;;       | ___                             zero or more
;;       | ..k                             k or more
;;       | __k                             k or more
;;
;;         quasi-patterns:                 matches:
;;
;; qp  ::= ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | identifier                      a symbol
;;       | (qp_1 ... qp_n)                 list of n elements
;;       | (qp_1 ... qp_n . qp_{n+1})      list of n or more
;;       | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #(qp_1 ... qp_n)                vector of n elements
;;       | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
;;                                           of remainder must match qp_n+1
;;       | #&qp                            box
;;       | ,pat                            a pattern
;;       | ,@(lvp . . . lvp-n)
;;       | ,@(pat . . . pat_n . pat_{n+1})
;;       | ,@`qp                           qp must evaluate to a list as 
;;                                         so that this rule resembles the 
;;                                         above two rules
;;
;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
;; and, or, not, set!, get!, list-no-order, hash-table, ..., ___) 
;; cannot be used as pattern variables.</pre>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module match mzscheme
  (provide
   match
   match-lambda
   match-lambda*
   match-let
   match-let*
   match-letrec
   match-define
   match-test
   )

  (require-for-syntax (lib "stx.ss" "syntax")
                      (lib "etc.ss")
                      (lib "list.ss")
                      (lib "include.ss")
                      (lib "pretty.ss")
                      (lib "struct.ss" "syntax"))

  (require (lib "etc.ss")
           (lib "list.ss"))

  (define-struct (exn:misc:match exn:misc) (value))

  (define match:error
    (case-lambda
      ((val)
       (raise
        (make-exn:misc:match
         (format "match: no matching clause for ~e" val)
         (current-continuation-marks)
         val)))
      ((val expr)
       (raise
        (make-exn:misc:match
         (format "match: no matching clause for ~e: ~s" val expr)
         (current-continuation-marks)
         val)))))

  (define-syntax-set (match
                      match-lambda
                      match-lambda*
                      match-let
                      match-let*
                      match-letrec
                      match-define
                      match-test
                      m:match-test)

    (include (build-path "private" "plt-match" "match-inc.scm"))

    (define node-count 0)

    (define (match-func-plt stx stx-orig)
      (syntax-case stx (=>)
        ((_ exp clause ...)
         (quasisyntax/loc 
          stx-orig 
          (let ((x exp))
            #,(gen-match (syntax x)
                         '()
                         (syntax (clause ...))
                         stx-orig))))))
    
    (define match-lambda-func
      (lambda (stx stx-orig)
        (syntax-case stx ()
          [(k clause ...)
           (quasisyntax/loc 
             stx-orig 
             (lambda (exp) #,(match-func
                         (syntax/loc stx (match exp clause ...))
                         stx-orig)))])))
    
    (define match-lambda*-func
      (lambda (stx stx-orig)
        (syntax-case stx ()
          [(k clause ...)
           (quasisyntax/loc 
            stx-orig 
            (lambda exp #,(match-func
                      (syntax/loc stx (match exp clause ...))
                      stx-orig)))])))
    
    (define match-let-func
      (lambda (stx stx-orig)
        (syntax-case stx ()
          [(_ name () body1 body ...)
           (syntax/loc stx-orig (let name () body1 body ...))]
          [(_ name ([pat1 exp1] [pat exp]...) body1 body ...)
           (identifier? (syntax name))
           (let ((pat-list (syntax-object->datum (syntax (pat1 pat ...))))
                 (real-name (syntax-object->datum (syntax name))))
             (if (andmap pattern-var? pat-list)
                 (syntax/loc 
                   stx-orig 
                   (let name ([pat1 exp1] [pat exp] ...) body1 body ...))
                 (quasisyntax/loc 
                   stx-orig
                   (letrec ([name
                             #,(match-lambda*-func (syntax/loc stx-orig (match-lambda* ((pat1 pat ...) body1 body ...)))
                                              stx-orig)
                             ])
                     (name exp1 exp ...)))))]
          [(_ () body1 body ...)
           (syntax/loc stx-orig (begin body1 body...))]
          [(_ ([pat1 exp1] [pat exp]...) body1 body ...)
           (quasisyntax/loc 
             stx-orig 
             ( #,(match-lambda*-func (syntax/loc stx-orig (match-lambda* ((pat1 pat ...) body1 body ...)))
                                stx-orig)
              exp1 exp ...))])))

    (define match-let*-func
      (lambda (stx stx-orig)
        (syntax-case stx ()
          ((_ () body body1 ...)
           (syntax/loc stx-orig (let* () body body1 ...)))
          ((_ ([pat exp] rest ...) body body1 ...)
           (if (pattern-var? (syntax-object->datum (syntax pat)))
               (quasisyntax/loc 
                 stx-orig
                 (let ([pat exp])
                   #,(match-let*-func (syntax/loc stx-orig (match-let* (rest ...) body body1 ...)) stx-orig)
                   )
                 )
               (match-func
                (quasisyntax/loc
                 stx-orig
                 (match exp [pat #,(match-let*-func
                                    (syntax/loc stx-orig (match-let* (rest ...) body body1 ...))
                                    stx-orig)]))
                stx-orig))))))

    (define match-letrec-func-plt
      (lambda (stx stx-orig)
        (syntax-case stx ()
          ((_ () body body1 ...)
           (syntax/loc stx (let () body body1 ...)))
          ((_ ([pat exp] ...) body body1 ...)
           (andmap pattern-var?
                   (syntax-object->datum (syntax (pat ...)))) 
           (syntax/loc stx (letrec ([pat exp] ...) body body1 ...)))
          ((_ ([pat exp] ...) body body1 ...)
           (let* ((**match-bound-vars** '())
                  (compiled-match 
                   (gen-match (syntax the-exp);(syntax (list exp ...))
                              '()
                              (syntax (((list pat ...) never-used)))
                              stx-orig
                              (lambda (sf bv)
                                (set! **match-bound-vars** bv)
                                (quasisyntax/loc 
                                 stx 
                                 (begin
                                   #,@(map (lambda (x)
                                             (quasisyntax/loc 
                                              stx 
                                              (set! #,(car x) #,(cdr x))))
                                           (reverse bv))
                                   body body1 ...))))))
             (quasisyntax/loc 
              stx-orig 
              (letrec (#,@(map
                           (lambda (x) (quasisyntax/loc stx (#,(car x) #f)))
                           (reverse **match-bound-vars**))
                       (the-exp (list exp ...)))
                #,compiled-match)))))))

    (define match-define-func-plt
      (lambda (stx stx-orig)
        (syntax-case stx ()
          [(_ pat exp)
           (identifier? (syntax pat))
           (syntax/loc stx-orig (begin (define pat exp)))]
          [(_ pat exp)
           (let* ((**match-bound-vars** '())
                  (compiled-match
                   (gen-match (syntax the-exp)
                              '()
                              (syntax/loc (syntax pat) ((pat never-used)))
                              stx-orig
                              (lambda (sf bv)
                                (set! **match-bound-vars** bv)
                                (quasisyntax/loc 
                                 stx-orig 
                                 (begin
                                   #,@(map (lambda (x)
                                             (quasisyntax/loc 
                                              stx-orig
                                              (set! #,(car x) #,(cdr x))))
                                           (reverse bv))))))))
             (quasisyntax/loc stx-orig
                              (begin #,@(map
                                         (lambda (x) (quasisyntax/loc 
                                                      stx 
                                                      (define #,(car x) #f)))
                                         (reverse **match-bound-vars**))
                                     (let ((the-exp exp))
                                       #,compiled-match))))])))
    
    ;; these are the translators
    (define m:match-test/proc 
      (lambda (stx)
        (syntax-case stx (=>)
          ((_ clause ...)
           (begin
             (set! node-count 0)
              (let-values (((stx t rt gc) (time-apply gen-match 
                                   (list (syntax x)
                                         '()
                                         (syntax (clause ...))
                                         stx))))
                #`(list ; (let ((dat-struct (seconds->date (current-seconds))))
                        ;         (list (date-month dat-struct)
                        ;              (date-day dat-struct)
                        ;           (date-year dat-struct)));
                        ;           (list #,@(get-date)) 
                        #,node-count
                        #,rt)))))))

    (define match-test/proc
      (lambda (stx)
        (syntax-case stx ()
          ((_ clause ...)
           (quasisyntax/loc 
            stx 
            (m:match-test  
             #,@(map handle-clause 
                     (syntax-e (syntax (clause ...))))))))))

    (define match/proc 
      (lambda (stx)
        (match-func stx stx)))
    
    (define match-lambda/proc
      (lambda (stx)
        (match-lambda-func stx stx)))

    (define match-lambda*/proc
      (lambda (stx)
        (match-lambda*-func stx stx))) 
    
    (define match-let/proc
      (lambda (stx)
        (match-let-func stx stx)))
    
    (define match-let*/proc
      (lambda (stx)
        (match-let*-func stx stx)))

    (define match-letrec/proc
      (lambda (stx)
        (match-letrec-func stx stx)))

    (define match-define/proc
      (lambda (stx)
        (match-define-func stx stx)))

   (define match-func
      (lambda (stx stx-orig)
        (syntax-case stx ()
          ((_ exp clause ...)
            (match-func-plt
             (quasisyntax/loc
              stx-orig
              (match exp 
                     #,@(map handle-clause 
                             (syntax-e (syntax (clause ...))))))
             stx-orig)))))

    (define match-letrec-func
      (lambda (stx stx-orig)
        (syntax-case stx ()
          ((_ () body body1 ...)
           (syntax/loc stx (let () body body1 ...)))
          ((_ ([pat exp] ...) body body1 ...)
           (andmap pattern-var?
                   (syntax-object->datum (syntax (pat ...))))
           (syntax/loc stx (letrec ([pat exp] ...) body body1 ...)))
          ((_ ([pat exp] ...) body body1 ...)
           (match-letrec-func-plt
            (quasisyntax/loc
             stx
             (match-letrec #,(map 
                              handle-let-clause 
                              (syntax->list (syntax ([pat exp] ...))))
                           body body1 ...))
            stx-orig)))))
           

    (define match-define-func
      (lambda (stx stx-orig)
        (syntax-case stx ()
          [(_ pat exp)
           (identifier? (syntax pat))
           (syntax/loc stx (begin (define pat exp)))]
          [(_ pat exp)
           (match-define-func-plt
            (quasisyntax/loc
             stx-orig
             (match-define #,(convert-pat (syntax pat)) exp))
            stx-orig)])))
            


    ;; these functions convert the patterns from the old syntax 
    ;; to the new syntax
    (define (handle-let-clause stx)
      (syntax-case stx ()
        ((pat exp)
         (quasisyntax/loc
          stx
          (#,(convert-pat (syntax pat)) exp)))))

    (define handle-clause
      (lambda (stx)
        (syntax-case stx (=>)
          ((pat (=> id) expr ...)
           (quasisyntax/loc 
            stx 
            (#,(convert-pat (syntax pat)) (=> id) expr ...)))
          ((pat expr ...)
           (quasisyntax/loc 
            stx 
            (#,(convert-pat (syntax pat)) expr ...))))))


    (define (convert-pat stx)
      (define (imp-list? x)
        (define (keyword? x)
          (member (syntax-object->datum x)
                  '(
                    quote
                    quasiquote
                    ?
                    =
                    and
                    or
                    not
                    $
                    set!
                    get!
                    ;unquote
                    ;unquote-splicing
                    )))
        (let/ec out
          (let loop ((x x))
            (cond ((null? x) (out #f))
                  ((or (not (pair? x))
                       (and (list? x)
                            (keyword? (car x)))) 
                   (list 
                    (quasisyntax/loc stx #,x)))
                  (else (cons (car x) (loop (cdr x))))))))
      (define stx-equal? 
        (lambda (a b)
          (equal? (syntax-object->datum a)
                  (syntax-object->datum b))))
      (define (convert-quasi stx)
        (syntax-case stx (unquote quasiquote unquote-splicing)
          (,pat
           (quasisyntax/loc stx ,#,(convert-pat (syntax pat))))
          (,@pat
           (quasisyntax/loc stx ,@#,(convert-pat (syntax pat))))
          ((x . y)
           (quasisyntax/loc 
            stx (#,(convert-quasi (syntax x)) . #,(convert-quasi (syntax y)))))
          (pat
           (vector? (syntax-e stx))
           (quasisyntax/loc 
            stx 
            #,(list->vector (map convert-quasi 
                                 (vector->list (syntax-e stx))))))
          (pat
           (box? (syntax-e stx))
           (quasisyntax/loc 
            stx #,(box (convert-quasi (unbox (syntax-e stx))))))
          (pat stx)))
      ;(write (syntax-object->datum stx))(newline)
      (syntax-case* 
       stx
       (_ ? = and or not $ set! get! quasiquote 
          quote unquote unquote-splicing) stx-equal?
          (p
           (dot-dot-k? (syntax-object->datum (syntax p)))
           stx)
          (_ stx)
          (() (quasisyntax/loc stx (list)))
          ('() (quasisyntax/loc stx (list)))
          ('item stx)
          (p
           (let ((old-pat (syntax-object->datum stx)))
             (or (string? old-pat)
                 (boolean? old-pat)
                 (char? old-pat)
                 (number? old-pat)))
           stx)
          ((? pred)
           stx)
          ((? pred a ...)
           (quasisyntax/loc
            stx
            (? pred #,@(map convert-pat 
                            (syntax-e (syntax (a ...)))))))
          (`pat
           (quasisyntax/loc stx `#,(convert-quasi (syntax pat))))
          ((= op pat)
           (quasisyntax/loc
            stx
            (app op #,(convert-pat (syntax pat)))))
          ((and pats ...)
           (quasisyntax/loc
            stx
            (and #,@(map convert-pat 
                         (syntax-e (syntax (pats ...)))))))
          ((or pats ...)
           (quasisyntax/loc
            stx
            (or #,@(map convert-pat 
                        (syntax-e (syntax (pats ...)))))))
          ((not pat)
           (quasisyntax/loc
            stx
            (not #,(convert-pat (syntax pat)))))
          (($ struct-name fields ...)
           (quasisyntax/loc
            stx
            (struct struct-name 
                    #,(map convert-pat 
                           (syntax-e (syntax (fields ...)))))))
          ((get! id)
           stx)
          ((set! id)
           stx)
          ((quote p) stx)
          ((car-pat . cdr-pat)
           (let ((l (imp-list? (syntax-e stx))))
             (if l
                 (quasisyntax/loc 
                  stx (list-rest #,@(map convert-pat l)))
                 (quasisyntax/loc 
                  stx (list #,@(map convert-pat (syntax-e stx)))))))
          (pt
           (vector? (syntax-object->datum stx))
           (quasisyntax/loc 
            stx 
            (vector #,@(map convert-pat 
                            (vector->list (syntax-e stx))))))
          (pt
           (box? (syntax-e stx))
           (quasisyntax/loc 
            stx 
            (box #,(convert-pat (unbox (syntax-e stx))))))
          (pt
           (identifier? stx)
           stx)
          (got-too-far 
           (match:syntax-err
            (syntax/loc stx got-too-far)
            "syntax error in pattern"))))
    )

  )
