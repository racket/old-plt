;; (documentation (name plt-match))
;; <pre>Pattern Matching Syntactic Extensions for Scheme
;;
;; All bugs or questions concerning this software should be directed to
;; Bruce Hauman <bhauman@cs.wcu.edu>.  The latest version of this software
;; can be obtained from http://sol.cs.wcu.edu/~bhauman/scheme/pattern.html.
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
;;                                         doesn't conflict with ..k or _
;;       | _                               anything
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (list lvp_1 ... lvp_n)               list of n elements
;;       | (list-rest lvp_1 ... lvp_n pat) an improper list of n elements
;;                                         plus a last element which represents
;;                                         the last cdr of the list
;;       | (vector lvp_1 ... lvp_n)        vector of n elements
;;       | (box pat)                       box
;;       | (struct struct-name (pat_1 ... pat_n)) a structure
;;       | (list-no-order pat ...)         matches a list with no regard for 
;;                                         the order of the
;;                                         items in the list
;;       | (list-no-order pat ... pat_n ooo) pat_n matches the remaining 
;;                                           unmatched items
;;       | (hash-table pat ...)            matches the elements of a hash table
;;       | (hash-table pat ... pat_n ooo)  pat_n must match the remaining 
;;                                         unmatched elements
;;       | (app field pat)                 a field of a structure (field is 
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
;;       | ,@(lvp-1 . . . lvp-n . pat)
;;       | ,@`qp                           qp must evaluate to a list as 
;;                                         so that this rule resembles the 
;;                                         above two rules
;;
;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
;; and, or, not, set!, get!, list-no-order, hash-table, ..., ___) 
;; cannot be used as pattern variables.</pre>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module plt-match mzscheme
  (provide 
   match
   match-lambda
   match-lambda*
   match-let
   match-let*
   match-letrec
   match-define
   match-count
   match:test-no-order
   ) 
  
  (require-for-syntax (lib "stx.ss" "syntax")
                      (lib "etc.ss")
                      ;(lib "pretty.ss")
                      (lib "list.ss")
                      (lib "include.ss"))
  
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
                      match-count
                      )

    (include "private/plt-match/match-inc.scm")    

    (define node-count 0)

    ;;!(syntax match-count)
    ;; This macro only returns a number.  This number represents the
    ;; number of nodes generated in the process of compiling the match 
    ;; expresseion.  This gives one and idea as to the size of the 
    ;; compiled expression.  This is mostly used for testing.
    (define match-count/proc 
      (lambda (stx)
        (syntax-case stx (=>)
          ((_ exp clause ...)
           (begin
             (set! node-count 0)
             (quasisyntax/loc 
              stx 
              (let ((x exp)) #,(gen-match (syntax x)
                                        '()
                                        (syntax (clause ...))
                                        stx)))
             #`#,node-count)))))

    (define match/proc 
      (lambda (stx)
        (syntax-case stx (=>)
          ((_ exp clause ...)
           (quasisyntax/loc 
            stx 
            (let ((x exp)) #,(gen-match (syntax x)
                                        '()
                                        (syntax (clause ...))
                                        stx)))))))

    (define match-lambda/proc 
      (lambda (stx)
        (syntax-case stx ()
          [(k clause ...)
           (syntax/loc 
             stx 
             (lambda (exp) (match exp clause ...)))])))
    
    (define match-lambda*/proc 
      (lambda (stx)
        (syntax-case stx ()
          [(k clause ...)
           (syntax/loc stx (lambda exp (match exp clause ...)))])))

    (define match-let/proc
      (lambda (stx)
        (syntax-case stx ()
          [(_ name () body1 body ...)
           (syntax/loc stx (let name () body1 body ...))]
          [(_ name ([pat1 exp1] [pat exp]...) body1 body ...)
           (identifier? (syntax name))
           (let ((pat-list (syntax-object->datum (syntax (pat1 pat ...))))
                 (real-name (syntax-object->datum (syntax name))))
             (if (andmap pattern-var? pat-list)
                 (syntax/loc 
                   stx 
                   (let name ([pat1 exp1] [pat exp] ...) body1 body ...))
                 (syntax/loc 
                   stx
                   (letrec ([name
                             (match-lambda* ((list pat1 pat ...) 
                                             body1 body ...))])
                     (name exp1 exp ...)))))]
          [(_ () body1 body ...)
           (syntax/loc stx (begin body1 body...))]
          [(_ ([pat1 exp1] [pat exp]...) body1 body ...)
           (syntax/loc 
             stx 
             ((match-lambda* ((list pat1 pat ...) body1 body ...)) 
              exp1 exp ...))])))
    
    (define match-let*/proc
      (lambda (stx)
        (syntax-case stx ()
          ((_ () body body1 ...)
           (syntax/loc stx (let* () body body1 ...)))
          ((_ ([pat exp] rest ...) body body1 ...)
           (if (pattern-var? (syntax-object->datum (syntax pat)))
               (syntax/loc 
                 stx 
                 (let ([pat exp]) (match-let* (rest ...) body body1 ...)))
               (syntax/loc 
                 stx 
                 (match exp [pat (match-let* (rest ...) body body1 ...)])))))))
    
    (define match-letrec/proc
      (lambda (stx)
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
                              stx
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
              stx 
              (letrec (#,@(map
                           (lambda (x) (quasisyntax/loc stx (#,(car x) #f)))
                           (reverse **match-bound-vars**))
                       (the-exp (list exp ...)))
                #,compiled-match)))))))
    
    (define match-define/proc
      (lambda (stx)
        (syntax-case stx ()
          [(_ pat exp)
           (identifier? (syntax pat))
           (syntax/loc stx (begin (define pat exp)))]
          [(_ pat exp)
           (let* ((**match-bound-vars** '())
                  (compiled-match
                   (gen-match (syntax the-exp)
                              '()
                              (syntax/loc (syntax pat) ((pat never-used)))
                              stx
                              (lambda (sf bv)
                                (set! **match-bound-vars** bv)
                                (quasisyntax/loc 
                                 stx 
                                 (begin
                                   #,@(map (lambda (x)
                                             (quasisyntax/loc 
                                              stx
                                              (set! #,(car x) #,(cdr x))))
                                           (reverse bv))))))))
             (quasisyntax/loc stx
                              (begin #,@(map
                                         (lambda (x) (quasisyntax/loc 
                                                      stx 
                                                      (define #,(car x) #f)))
                                         (reverse **match-bound-vars**))
                                     (let ((the-exp exp))
                                       #,compiled-match))))])))

    )
  
  ;;!(function match:test-no-order 
  ;;          (form (match:test-no-order tests l last-test ddk-num)
  ;;                -> 
  ;;                bool)
  ;;          (contract (list list test integer) -> bool))
  ;; This is a recursive depth first search for a sequence of
  ;; items in list l which will satisfy all of the tests in list
  ;; tests.  This is used for list-no-order and hash-table patterns.
  ;; This function also handles ddk patterns by passing it the last
  ;; test before the ddk and the value of k.
  (define (match:test-no-order tests l last-test ddk-num)
    (define (handle-last-test test l)
      (and (>= (length l) ddk-num) 
           (andmap test l)))
    (define (dep-first-test head rest tests)
      (cond ((null? tests) 
             (if last-test 
                 (handle-last-test last-test (cons head rest)) 
                 #f))
            ((null? rest)
             (if last-test
                 (and (= 0 ddk-num)
                      (= 1 (length tests))
                      ((car tests) head))
                 (and (= 1 (length tests))
                      ((car tests) head))))
            (else (and (pair? tests)
                       ((car tests) head)
                       (match:test-no-order (cdr tests) 
                                            rest 
                                            last-test 
                                            ddk-num)))))
    (let loop ((lst l))
      (if (null? lst) 
          #f
          (or (dep-first-test (car lst) (remove (car lst) l) tests)
              (loop (cdr lst))))))


)
                                 
                                 
     

