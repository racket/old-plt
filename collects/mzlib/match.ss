;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Matching Syntactic Extensions for Scheme
;;
;; Specialized for MzScheme by Bruce Hauman
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
;; or bug fixes to wright@research.nj.nec.com so that they may be included
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
;;       | (match-let ((pat exp) ...) body)
;;       | (match-let* ((pat exp) ...) body)
;;       | (match-letrec ((pat exp) ...) body)
;;       | (match-define pat exp)
;;
;; clause ::= (pat body) | (pat => exp)
;;
;;         patterns:                       matches:
;;
;; pat ::= identifier                      anything, and binds identifier
;;       | _                               anything
;;       | ()                              the empty list
;;       | #t                              #t
;;       | #f                              #f
;;       | string                          a string
;;       | number                          a number
;;       | character                       a character
;;       | 'sexp                           an s-expression
;;       | 'symbol                         a symbol (special case of s-expr)
;;       | (pat_1 ... pat_n)               list of n elements
;;       | (pat_1 ... pat_n . pat_{n+1})   list of n or more
;;       | (pat_1 ... pat_n pat_n+1 ooo)   list of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #(pat_1 ... pat_n)              vector of n elements
;;       | #(pat_1 ... pat_n pat_n+1 ooo)  vector of n or more, each element
;;                                           of remainder must match pat_n+1
;;       | #&pat                           box
;;       | ($ struct-name pat_1 ... pat_n) a structure
;;       | (= field pat)                   a field of a structure (field is an accessor)
;;                                         Actually field can be any function which can be 
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
;;       | ,@pat                           a pattern
;;
;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
;; and, or, not, set!, get!, ..., ___) cannot be used as pattern variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module match mzscheme
  (provide match 
           match-lambda 
           match-lambda* 
           match-let 
           match-let* 
           match-letrec 
           match-define)

  (require-for-syntax (lib "stx.ss" "syntax")
                      (lib "etc.ss"))
 
 
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
     
  (define-syntaxes (match 
                    match-lambda 
                    match-lambda* 
                    match-let 
                    match-let* 
                    match-letrec
                    match-define)
    
    (letrec (
             ;; Is x a pattern variable?
             (pattern-var? 
              (lambda (x)
                (and (symbol? x)
                     (not (dot-dot-k? x))
                     (not (memq x
                                '(quasiquote
                                  quote
                                  unquote
                                  unquote-splicing
                                  ?
                                  _
                                  $
                                  =
                                  and
                                  or
                                  not
                                  set!
                                  get!
                                  ...
                                  ___))))))
             
           (match:syntax-err (lambda (obj msg . detail)
                               (apply
                                raise-syntax-error
                                'match
                                msg
                                obj
                                detail)))
           (stx-length (lambda (syntax-obj)
                         (length (syntax->list syntax-obj))))
  
           (stx-? (lambda (test val) 
                    (test (syntax-object->datum val))))
           (stx-equal? (lambda (a b)
                         (equal? (syntax-object->datum a)
                                 (syntax-object->datum b))))
           (symbol-append (lambda l
                            (string->symbol
                             (apply
                              string-append
                              (map (lambda (x)
                                     (cond
                                      ((symbol? x) (symbol->string x))
                                      ((number? x) (number->string x))
                                      (else x)))
                                   l)))))
           ;; struct-pred-accessors - given a syntax object that is the 
           ;;   name of a structure this function returns two values: 
           ;; 1) the predicate function for that structure (i.e. posn?)
           ;; 2) a list of accessor functions in order (i.e. (posn-x posn-y))
           (struct-pred-accessors
            (let ((accessors-index 3)
                  (pred-index 2)
                  (handle-acc-list
                   (lambda (l)
                     (letrec ((RC 
                               (lambda (ac-list)
                                 (cond ((null? ac-list) '())
                                       ((not ac-list) '())
                                       (else (cons (car ac-list) 
                                                   (RC (cdr ac-list))))))))
                       (reverse (RC l))))))
              (lambda (struct-name failure-thunk)
                (let* ((info-on-struct (syntax-local-value struct-name failure-thunk))   
                       (accessors (handle-acc-list 
                                   (list-ref info-on-struct accessors-index)))
                       (pred (list-ref info-on-struct pred-index)))
                  (values pred accessors))))) 
           
           ;; unreachable - takes a list of unreached clauses and the original
           ;; match expression and prints a warning for each of the unreached
           ;; match clauses to the current output port
           ;; 
           ;; plist - is a list of unreached pattern clauses
           ;; match-expr - is the origional match expr the clauses came from
           (unreachable
             (lambda (plist match-expr)
               (for-each
                (lambda (x)
                  (fprintf
                   (current-error-port)
                   "Warning: unreachable match clause ~e in ~e~n"
                   x
                   (syntax-object->datum match-expr)))
                plist)))
           ;; it is important that we start
           ;; the count over for each new row so
           ;; that we can eliminate duplicate tests
           (get-exp-var 
            (let ((count 0))
              (lambda () 
                (set! count (add1 count))
                (string->symbol 
                 (string-append "exp" 
                                (number->string count))))))
           ;; gen-match and its helper function gen are the workhorses
           ;; of the library.  It compiles a series of if expressions 
           ;; for a given pattern.  
           ;; exp - the expression that is to be tested against the pattern
           ;; this should normally be a piece of syntax that indirectly 
           ;; represents the expression.  Because if it is the syntax of the
           ;; expression itself it will be duplicated many times throughout
           ;; the generated match test.
           ;; tsf - is a list the tests-seen-so-far and is used to 
           ;; prevent generating tests for the same condition twice
           ;; patlist - is a list of the pattern clauses of the match expr 
           ;; these can be of either form (pat body ...) or 
           ;; (pat (=> fail) body ...)
           ;; stx is the original syntax of the match expression.
           ;; This is only used for error reporting.
           ;; success-func - an optional argument which allows one to 
           ;; specify how a successful match is treated.  This made
           ;; the creation of match-letrec and match-define macros simple.
           ;; The reason for this function is that most of the information 
           ;; about a match (namely the bound match variables) is at the bottom
           ;; of the recursion tree. The success function must take two arguments
           ;; and it should return a syntax object.

           (gen-match 
            (opt-lambda (exp tsf patlist stx [success-func #f]) 
              (let* ((unrb (box #f))
                     (compiled-match
                      (quasisyntax/loc stx
                                       (let ((match-failure 
                                              (lambda () 
                                                (match:error #,exp (quote #,stx)))))
                                         #,(gen exp tsf patlist 
                                                        stx 
                                                        unrb 
                                                        '()
                                                        (syntax (match-failure)) 
                                                        success-func)))))
                (if (unbox unrb)
                  (unreachable (unbox unrb) stx))
                compiled-match)))
           
           ;; gen is is the helper function for gen-match. In reality gen-match
           ;; is just a wrapper for gen that allows for the detection of unreached
           ;; patterns.  This is implemented through the use of the unreached
           ;; box which should always contain the list of clauses that
           ;; have not been reached so far.  If they have all been reached then
           ;; the unreached box is set to #f.
           ;;
           ;; notes on implementation:
           ;; This implementation follows Andrew K. Wright's implementation.
           ;; It has been changed to work with the syntax objects of hygenic
           ;; macros.  The major difference between this implementation and 
           ;; Andrew's is that this repeatedly takes advantage of the fact 
           ;; all of the bound variable information is available at the 
           ;; bottom of the recursion tree.  For more information on this
           ;; function see the _next_ function.
           (gen 
            (opt-lambda (exp tsf patlist stx unreach-box lbsf failure-func [success-func #f])
              (if (stx-null? patlist)
                  failure-func ;(quasisyntax/loc stx (match:error #,exp (quote #,stx)))
                  (with-syntax (((clause1 clauselist ...) patlist))
                    (let-values (((pat body fail-sym)
                                  (syntax-case (syntax clause1) (=>)
                                    ((pat (=> fail-sym) body ...)
                                     (values (syntax pat) 
                                             (syntax (body ...)) 
                                             (syntax fail-sym)))
                                    ((pat body ...)
                                     (values (syntax pat) 
                                             (syntax (body ...)) #f)))))
                      (let* ((rest-of-clauses (syntax (clauselist ...)))
                             (fail (lambda (sf bv lbsf)
                                     ;; i don't pass the success-func forward
                                     ;; because it is only used for match-define
                                     ;; and match-letrec which only have one
                                     ;; clause
                                     (gen exp
                                          sf
                                          rest-of-clauses
                                          stx 
                                          unreach-box
                                          lbsf
                                          failure-func)))
                             (success 
                              (begin (let ((tail (syntax-object->datum 
                                                  (syntax (clauselist ...)))))
                                           (set-box! unreach-box 
                                                     (if (null? tail) #f tail)))
                                     (if (not success-func)
                                         (lambda (sf bv lbsf)
                                           (if fail-sym 
                                               (quasisyntax/loc stx (call-with-current-continuation 
                                                  (lambda (fail-cont)
                                                    (let 
                                                        ((failure 
                                                           (lambda ()
                                                             (fail-cont 
                         ; it seems like fail is called twice in this situation
                                                              #,(fail sf bv lbsf)))))
                                                      ((lambda (#,fail-sym
                                                                #,@(map car bv))
                                                         #,@body) 
                                                       failure 
                                                       #,@(map cdr bv))))))
                                               (quasisyntax/loc stx ((lambda #,(map car bv) 
                                                    #,@body) #,@(map cdr bv)))))
                                         success-func))))
                        ;; next is the major internal function of gen
                        ;; This is implemented in what Wright terms as mock-continuation-passing
                        ;; style.  The functions that create the syntax for a match success and failure 
                        ;; are passed forward
                        ;; but they are always called in emit.  This is extremely effective for 
                        ;; handling the different structures that are matched.  This way we can
                        ;; specify ahead of time how the rest of the elements of a list or vector
                        ;; should be handled.  Otherwise we would have to pass more information
                        ;; forward in the argument list of next and then test for it later and 
                        ;; then take the appropriate action.  To understand this better take a 
                        ;; look at how proper and improper lists are handled.
                        ;;
                        (let next ((p pat)
                                   (e exp)  ;; this is the expression that has been abreviated 
                                            ;; by reusing pairs
                                   (ae exp) ;; this is the actual expression
                                   (let-bound lbsf) ;; alist of let-bindings for pair reuse
                                   (sf tsf)
                                   (bv '())
                                   (kf fail)
                                   (ks success))
                          ;; this is a hacky way to get variables that are to be bound for a pattern
                          ;(write let-bound) (newline)
                          ;(write e) (newline) (write ae) (newline)
                          (letrec ((call-next-and-bind 
                                    (lambda (pat e ae let-bound sf bv kf ks)
                                      ;; first check to se if it is already bound by a let
                                      ;; if not continue on with the bound name
                                      ;; otherwise bind this one
                                      (let ((binding-pair (assoc (syntax-object->datum ae) let-bound)))
                                        (if binding-pair 
                                            (next pat (cdr binding-pair) ae let-bound sf bv kf ks)
                                            (let ((exp-var (get-exp-var)))
                                              #`(let ((#,exp-var #,e))
                                                  #,(next pat
                                                          #`#,exp-var
                                                          ae
                                                          (cons (cons (syntax-object->datum ae) 
                                                                      #`#,exp-var)
                                                                let-bound)
                                                          sf 
                                                          bv 
                                                          kf
                                                          ks)))))))
                                   (getbindings               
                                    (lambda (pat-syntax)
                                      (let/cc out
                                              (next
                                               pat-syntax
                                               (quote-syntax dummy)
                                               (quote-syntax dummy)
                                               let-bound
                                               '()
                                               '()
                                               (lambda (sf bv lbsf) '(dummy-symbol))
                                               (lambda (sf bv lbsf) (out (map car bv)))))))
                                   (parse-quasi (lambda (phrase)
                                            (syntax-case phrase (unquote unquote-splicing)
                                              (p
                                               (let ((pat (syntax-object->datum (syntax p))))
                                                 (or (null? pat)
                                                     (string? pat)
                                                     (boolean? pat)
                                                     (char? pat)
                                                     (number? pat)))
                                               (syntax p))
                                              (p
                                               ;; although it is not in the grammer for quasi patterns 
                                               ;; it seems important to not allow unquote splicing to be 
                                               ;; a symbol in this case `,@(a b c). In this unquote-splicing 
                                               ;; is treated as a symbol and quoted to be matched.
                                               ;; this is probably not what the programmer intends so
                                               ;; it may be better to throw a syntax error
                                               (identifier? (syntax p))
                                               (syntax/loc phrase 'p))
                                              (,p (syntax p))
                                              ((,@p . ()) (syntax p))
                                              ((,@p . rest)
                                               #`#,(append (syntax->list (syntax p))
                                                           (parse-quasi (syntax rest))))
                                              ((p ddk)
                                               (stx-dot-dot-k? (syntax ddk))
                                               #`(#,(parse-quasi (syntax p)) ddk))
                                              ((x . y) #`(#,(parse-quasi (syntax x)) .
                                                           #,(parse-quasi (syntax y))))
                                              (p
                                               (vector? (syntax-object->datum (syntax p)))
                                                #`#,(apply vector
                                                           (syntax->list
                                                            (parse-quasi
                                                             (vector->list (syntax-e (syntax p)))))))
                                              (p
                                               (box? (syntax-object->datum (syntax p)))
                                               #`#,(box (parse-quasi (unbox (syntax-e (syntax p))))))
                                              (p (match:syntax-err
                                                  (syntax-object->datum (syntax p))
                                                  "syntax error in quasi-pattern"))))))
                            (syntax-case* p (_ quote quasiquote ? = and or not $ set! 
                                             get! ... ___ unquote unquote-splicing) stx-equal? 
                              (_ (ks sf bv let-bound))
                              (pt 
                               (and (identifier? (syntax pt))
                                    (pattern-var? (syntax-object->datum (syntax pt)))
                                    (not (stx-dot-dot-k? (syntax pt))))
                               (ks sf (cons (cons (syntax pt) e) bv) let-bound))
                              (() (emit (quasisyntax/loc p (null? #,e)) 
                                        #`(null? #,ae)
                                        let-bound
                                        sf 
                                        bv 
                                        kf 
                                        ks))
                              (pt 
                               ;; could convert the syntax once
                               (or (stx-? string? (syntax pt))  
                                   (stx-? boolean? (syntax pt)) 
                                   (stx-? char? (syntax pt)) 
                                   (stx-? number? (syntax pt)))
                               (emit (quasisyntax/loc p (equal? #,e pt)) 
                                     #`(equal? #,ae pt)
                                     let-bound
                                     sf bv kf ks))
                              ((quote _)
                               (emit (quasisyntax/loc p (equal? #,e #,p)) 
                                     #`(equal? #,ae #,p)
                                     let-bound
                                     sf bv kf ks))
                              (`quasi-pat
                               (next (parse-quasi (syntax quasi-pat)) e ae let-bound sf bv kf ks))
                              ('item
                               (emit (quasisyntax/loc p (equal? #,e #,p)) 
                                     #`(equal? #,ae #,p)
                                     let-bound
                                     sf bv kf ks))
                              ;('(items ...)
                               ;(emit (quasisyntax/loc p (equal? #,e #,p)) sf bv kf ks))
                              ((? pred pat1 pats ...)
                               (next (syntax (and (? pred) pat1 pats ...))
                                     e
                                     ae
                                     let-bound
                                     sf
                                     bv
                                     kf
                                     ks))
                              ;; could we check to see if a predicate is a procedure here?
                              ((? pred)
                               (emit (quasisyntax/loc p (pred #,e)) 
                                     #`(pred #,ae)
                                     let-bound
                                     sf bv kf ks))
                              ;; syntax checking
                              ((? pred ...)
                               (match:syntax-err
                                p
                                (if (zero? (length (syntax-e (syntax (pred ...)))))
                                    "a predicate pattern must have a predicate following the ?"
                                    "syntax error in predicate pattern")))
                              ((= op pat)
                               (call-next-and-bind (syntax pat) 
                                                   (quasisyntax/loc p (op #,e)) 
                                                   #`(op #,ae) 
                                                   let-bound 
                                                   sf bv kf ks))
                              ;; syntax checking
                              ((= op ...)
                               (match:syntax-err
                                p
                                (if (zero? (length (syntax-e (syntax (op ...)))))
                                    "an operation pattern must have a procedure following the ="
                                    "there should be one pattern following the operator")))
                              ((and pats ...)
                               (let loop
                                   ((p (syntax (pats ...)))
                                    (seensofar sf)
                                    (boundvars bv)
                                    (let-bound let-bound))
                                 (syntax-case p ()
                                   (() (ks sf boundvars let-bound))
                                   ((pat1 pats ...)
                                    (next (syntax pat1) 
                                          e 
                                          ae
                                          let-bound
                                          seensofar 
                                          boundvars  ;; keep collecting vars 
                                          kf
                                          (lambda (sf bv lbsf) ;; if it succeeds check nest one
                                            (loop (syntax (pats ...))
                                                  sf bv lbsf)))))))
                              ((or pats ...)
                               (let loop
                                   ((p (syntax (pats ...)))
                                    (seensofar sf)
                                    (boundvars bv)
                                    (let-bound let-bound))
                                 (syntax-case p ()
                                   (() (kf sf boundvars let-bound))
                                   ((pat1 pats ...)
                                    (next (syntax pat1) 
                                          e 
                                          ae
                                          let-bound
                                          seensofar 
                                          bv         ;; get rid of collected vars and start over
                                          (lambda (sf bv lbsf)             ; if it fails check next one  
                                            (loop (syntax (pats ...))
                                                  sf bv lbsf))
                                          ks)))))
                              ((not pat)
                               (next (syntax pat) e ae let-bound sf bv ks kf)) ;; swap success and fail
                              ;; could try to catch syntax local value error and rethrow syntax error
                              (($ struct-name fields ...)
                               (let ((num-of-fields (stx-length (syntax (fields ...)))))
                                 (let-values (((pred accessors) 
                                               (struct-pred-accessors 
                                                (syntax struct-name)
                                                (lambda () 
                                                  (match:syntax-err
                                                   (syntax struct-name) 
                                                   "not a defined structure")))))
                                   (let ((dif (- (length accessors) num-of-fields)))
                                     (if (not (zero? dif))
                                         (match:syntax-err
                                          p
                                          (string-append 
                                           (if (> dif 0) "not enough " "too many ") 
                                           "fields for structure in pattern"))
                                         (emit (quasisyntax/loc stx (#,pred #,e)) 
                                               #`(#,pred #,ae)
                                               let-bound
                                               sf
                                               bv
                                               kf
                                               (let rloop ((n 0))
                                                 (lambda (sf bv lbsf)
                                                   (if (= n num-of-fields)
                                                       (ks sf bv lbsf)
                                                       (call-next-and-bind
                                                        (list-ref (syntax->list (syntax (fields ...))) n)
                                                        (quasisyntax/loc stx (#,(list-ref accessors n) #,e))
                                                        #`(#,(list-ref accessors n) #,ae)
                                                        let-bound
                                                        sf
                                                        bv
                                                        kf
                                                        (rloop (+ 1 n))))))))))))
                              ;; syntax checking
                              (($ ident ...)
                               (match:syntax-err
                                p
                                (if (zero? (length (syntax-e (syntax (ident ...)))))
                                    (format "~a~n~a~n~a" 
                                            "a structure pattern must have the name "
                                            "of a defined structure followed with patterns "
                                            "to match each field of that structure")
                                    "syntax error in structure pattern")))
                              ((set! ident)
                               (identifier? (syntax ident))
                               (ks sf (cons (cons (syntax ident) (setter ae p)) bv) let-bound))
                              ;; syntax checking
                              ((set! ident ...)
                               (let ((x (length (syntax-e (syntax (ident ...))))))
                                 (match:syntax-err
                                  p
                                  (if (= x 1)
                                      "there should be an identifier after set! in pattern"
                                      (string-append "there should "
                                                     (if (zero? x) "" "only ") 
                                                     "be one identifier after set! in pattern")))))
                              ((get! ident)
                               (identifier? (syntax ident))
                               (ks sf (cons (cons (syntax ident) (getter ae p)) bv) let-bound))
                              ((get! ident ...)
                               (let ((x (length (syntax-e (syntax (ident ...))))))
                                 (match:syntax-err
                                  p
                                  (if (= x 1)
                                      "there should be an identifier after get! in pattern"
                                      (string-append "there should "
                                                     (if (zero? x) "" "only ") 
                                                     "be one identifier after get! in pattern")))))
                              ((pat dot-dot-k)
                               (and (not (or (memq (syntax-e (syntax pat)) 
                                                   '(unquote unquote-splicing ... ___))
                                             (stx-dot-dot-k? (syntax pat))))
                                    (stx-dot-dot-k? (syntax dot-dot-k)))
                               (emit 
                                (quasisyntax/loc stx (list? #,e))
                                #`(list? #,ae)
                                let-bound
                                sf 
                                bv 
                                kf
                                (lambda (sf bv lbsf)
                                  (let* ((k (stx-dot-dot-k? (syntax dot-dot-k)))
                                         (ksucc (lambda (sf bv lbsf)
                                                  (let ((bound (getbindings (syntax pat)))) 
                                                    (syntax-case (syntax pat) (_)
                                                      (_ (ks sf bv lbsf))
                                                      (the-pat
                                                       (null? bound)
                                                       (with-syntax ((exp-sym (syntax exp-sym)))
                                                         (let* ((ptst (next 
                                                                       (syntax pat)
                                                                       (syntax exp-sym)
                                                                       (syntax exp-sym)
                                                                       lbsf
                                                                       sf
                                                                       bv
                                                                       (lambda (sf bv lbsf) (syntax #f))
                                                                       (lambda (sf bv lbsf) (syntax #t))))
                                                                (tst (syntax-case ptst ()
                                                                       ((pred eta)
                                                                        (and (identifier? 
                                                                              (syntax pred))
                                                                             ;free-identifier=?
                                                                             (stx-equal? 
                                                                              (syntax eta) 
                                                                              (syntax exp-sym)))
                                                                        (syntax pred))
                                                                       (whatever
                                                                        (quasisyntax/loc stx (lambda (exp-sym)
                                                                            #,ptst))))))
                                                           (assm (quasisyntax/loc stx (andmap #,tst #,e))
                                                                 (kf sf bv lbsf)
                                                                 (ks sf bv lbsf)))))
                                                      (id
                                                       (and (identifier? (syntax id))
                                                            (stx-equal? (syntax id)
                                                                        (car bound)))
                                                       (next (syntax id) e ae let-bound sf bv kf ks))
                                                      (the-pat
                                                       (let ((binding-list-names 
                                                              (map (lambda (x)
                                                                     (datum->syntax-object 
                                                                      (quote-syntax here)
                                                                      (symbol-append
                                                                       (syntax-object->datum x)
                                                                       '-bindings)))
                                                                   bound)))
                                                        (quasisyntax/loc stx (let loop ((exp #,e)
                                                                      #,@(map 
                                                                          (lambda (x) (quasisyntax/loc stx (#,x '())))
                                                                          binding-list-names))
                                                             (if (null? exp)
                                                                 #,(ks sf 
                                                                       (append 
                                                                        (map cons 
                                                                             bound 
                                                                             (map 
                                                                              (lambda (x)
                                                                                (quasisyntax/loc stx (reverse #,x)))
                                                                              binding-list-names))
                                                                                  bv) 
                                                                       lbsf)
                                                                 #,(next (syntax the-pat)
                                                                         (syntax (car exp)) 
                                                                         (syntax (car exp))
                                                                         lbsf
                                                                         sf 
                                                                         bv  ;; we always start 
                                                                             ;; over with the old
                                                                             ;; bindings
                                                                         kf
                                                                         (lambda (sf bv lbsf)
                                                                           (quasisyntax/loc stx (loop 
                                                                              (cdr exp)
                                                                              #,@(map 
                                                                                  (lambda 
                                                                                      (b-var
                                                                                       bindings-var)
                                                                                    (quasisyntax/loc stx (cons 
                                                                                       #,(cdr 
                                                                                          (assq
                                                                                           b-var
                                                                                           bv))
                                                                                    #,bindings-var))) 
                                                                                  bound binding-list-names)))))))))))))))
                                    (case k
                                      ((0) (ksucc sf bv let-bound))
                                      ((1) (emit (quasisyntax/loc stx (pair? #,e)) 
                                                 #`(pair? #,ae)
                                                 lbsf
                                                 sf bv kf ksucc))
                                      (else (emit (quasisyntax/loc stx (>= (length #,e) #,k))
                                                  #`(>= (length #,ae) #,k)
                                                  lbsf
                                                  sf bv kf ksucc)))))))
                              ;; handle proper and improper lists
                              ((car-pat . cdr-pat) ;pattern ;(pat1 pats ...)
                               (not (or (memq (syntax-e (syntax car-pat)) 
                                                   '(unquote unquote-splicing))
                                             (stx-dot-dot-k? (syntax car-pat))))
                               (emit
                                (quasisyntax/loc stx (pair? #,e))
                                #`(pair? #,ae)
                                let-bound
                                sf
                                bv
                                kf
                                (lambda (sf bv lbsf)
                                  (call-next-and-bind (syntax car-pat) 
                                        #`(car #,e) ;(add-a e)
                                        (add-a ae)
                                        lbsf
                                        sf
                                        bv 
                                        kf
                                        (lambda (sf bv lbsf)
                                             (call-next-and-bind
                                              (syntax cdr-pat)
                                              #`(cdr #,e)
                                              (add-d ae)
                                              lbsf
                                              sf
                                              bv
                                              kf
                                              ks))))))
                              (pt 
                               (and (vector? (syntax-e (syntax pt)))
                                    (let* ((temp (syntax-e (syntax pt)))
                                           (len (vector-length temp)))
                                      (and (>= len 2)
                                           (stx-dot-dot-k? (vector-ref temp (sub1 len))))))
                               (let* ((vec-stx (syntax-e (syntax pt)))
                                      (vlen (- (vector-length vec-stx) 2)) ;; length minus 
                                                                           ;; the pat ...
                                      (k (stx-dot-dot-k? (vector-ref vec-stx (add1 vlen))))
                                      (minlen (+ vlen k))
                                      ;; get the bindings for the second to last element: 
                                      ;; 'pat' in pat ...
                                      (bound (getbindings (vector-ref vec-stx vlen))))
                                 (emit (quasisyntax/loc stx (vector? #,e))
                                       #`(vector? #,ae)
                                       let-bound
                                       sf
                                       bv
                                       kf
                                       (lambda (sf bv lbsf)
                                         (assm (quasisyntax/loc stx (>= (vector-length #,e) #,minlen))
                                               (kf sf bv lbsf)
                                               ((let vloop ((n 0))
                                                  (lambda (sf bv lbsf)
                                                    (cond
                                                     ((not (= n vlen))
                                                      (call-next-and-bind
                                                       (vector-ref vec-stx n)
                                                       (quasisyntax/loc stx (vector-ref #,e #,n))
                                                       #`(vector-ref #,ae #,n)
                                                       lbsf
                                                       sf
                                                       bv
                                                       kf
                                                       (vloop (+ 1 n))))
                                                     ((eq? (syntax-object->datum 
                                                            (vector-ref vec-stx vlen)) 
                                                           '_)
                                                      (ks sf bv lbsf))
                                                     (else 
                                                      (let* ((binding-list-names 
                                                              (map (lambda (x)
                                                                     (datum->syntax-object
                                                                      (quote-syntax here)
                                                                      (symbol-append
                                                                       (syntax-object->datum x)
                                                                       '-bindings)))
                                                                   bound)))
                                                        (quasisyntax/loc stx (let vloop 
                                                            ((index (- (vector-length #,e) 1))
                                                             #,@(map (lambda (x) (quasisyntax/loc stx (#,x '())))
                                                                     binding-list-names))
                                                            (if (> #,vlen index)
                                                                #,(ks sf 
                                                                      (append (map cons bound
                                                                                 binding-list-names)
                                                                                 bv)
                                                                      lbsf)
                                                                #,(call-next-and-bind
                                                                   (vector-ref vec-stx n)
                                                                   (quasisyntax/loc stx (vector-ref #,e index))
                                                                   #`(vector-ref #,ae index)
                                                                   lbsf
                                                                   sf
                                                                   bv ;; we alway start over
                                                                   ;; with the old bindings
                                                                   kf
                                                                   (lambda (sf bv lbsf)
                                                                     (quasisyntax/loc 
                                                                      stx (vloop 
                                                                           (- index 1)
                                                                           #,@(map 
                                                                               (lambda (b-var
                                                                                        bindings-var)
                                                                                 (quasisyntax/loc stx (cons
                                                                                                       #,(cdr
                                                                                                          (assq
                                                                                                           b-var
                                                                                                           bv))
                                                                                                       #,bindings-var)))
                                                                               bound
                                                                               binding-list-names)))))))))))))
                                                sf
                                                bv
                                                lbsf))))))
                              (pt 
                               (stx-? vector? (syntax pt))
                               (let ((vlen (stx-? vector-length (syntax pt))))
                                 (emit 
                                  (quasisyntax/loc stx (vector? #,e))
                                  #`(vector? #,ae)
                                  let-bound
                                  sf bv kf
                                  (lambda (sf bv lbsf)
                                    (emit (quasisyntax/loc stx (equal? (vector-length #,e) #,vlen))
                                          #`(equal? (vector-length #,ae) #,vlen)
                                          lbsf
                                          sf bv kf
                                          (let vloop ((n 0))
                                            (lambda (sf bv lbsf)
                                              (if (= n vlen)
                                                  (ks sf bv lbsf)
                                                  (call-next-and-bind 
                                                   (vector-ref (syntax-e (syntax pt)) n)
                                                   (quasisyntax/loc stx (vector-ref #,e #,n))
                                                   #`(vector-ref #,ae #,n)
                                                   lbsf
                                                   sf
                                                   bv
                                                   kf
                                                   (vloop (+ 1 n)))))))))))
                              (pt 
                               (stx-? box? (syntax pt))
                               (emit 
                                (quasisyntax/loc stx (box? #,e))
                                #`(box? #,ae)
                                lbsf
                                sf bv kf
                                (lambda (sf bv lbsf)
                                  (call-next-and-bind (unbox (syntax-e (syntax pt)))
                                        (quasisyntax/loc stx (unbox #,e))
                                        #`(unbox #,ae)
                                        lbsf
                                        sf
                                        bv
                                        kf
                                        ks))))
                              (got-too-far
                               (match:syntax-err
					    (syntax/loc stx got-too-far)
                                            "syntax error in pattern")))))))))))
           
           ;; emit's true function is to manage the tests-seen-so-far lists
           ;; it decides whether a new test needs to be added to the list
           ;; or whether this condition has already been tested for and if 
           ;; it is true emit calls the success function. If it has been 
           ;; determined to be a false property emit calls the fail function.
           ;; emit adds implied truths to the test seen so far list so that
           ;; these truths can be checked against later.
           (emit
            (lambda (tst act-test lbsf sf bv kf ks)
              (let ((test (syntax-object->datum act-test)))
                (cond
                 ((in test sf) (ks sf bv lbsf))
                 ((in `(not ,test) sf) (kf sf bv lbsf))
                 (else
                  (let* ((pred (car test))
                         (exp (cadr test))
                         (implied
                          (cond
                            ((equal? pred 'equal?)
                             (cond ((string? exp)
                                    (list `(string? ,exp)))
                                   ((boolean? exp)
                                    (list `(boolean? ,exp)))
                                   ((char? exp)
                                    (list `(char? ,exp)))
                                   ((number? exp)
                                    (list `(number? ,exp)))
                                   (else '())))
                            ((equal? pred 'null?)
                             (list `(list? ,exp)))
                            (else '())))
                         (not-imp
                          (if (equal? pred 'list?)
                              (list `(not (null? ,exp)))
                              '()))
                         (s (ks (cons test (append implied sf)) bv lbsf))
                         (k (kf (cons `(not ,test) (append not-imp sf)) bv lbsf)))
                    (assm tst k s)))))))

           ;; assm - this function is responsible for constructing the actual
           ;; if statements.  It examines the incoming failure action and compares
           ;; it to the current one if they are the same it concats the tests
           ;; with an and.
           (assm 
            (lambda (tst main-fail main-succ) 
              (let ((s (syntax-object->datum main-succ))
                    (f (syntax-object->datum main-fail)))
                (cond ((equal? s f) main-succ)
                      ((and (eq? s #t) (eq? f #f)) tst)
                      (else 
                       (syntax-case main-succ (if 
                                               and 
                                               call-with-current-continuation 
                                               lambda 
                                               let) ;free-identifier=?  ;stx-equal? 
                         ((if (and tsts ...) true-act fail-act)
                          (equal? f (syntax-object->datum (syntax fail-act)))
                          (quasisyntax/loc tst (if (and #,tst tsts ...) true-act fail-act)))
                         ((if tst-prev true-act fail-act)
                          (equal? f (syntax-object->datum (syntax fail-act)))
                          (quasisyntax/loc tst (if (and #,tst tst-prev) true-act fail-act)))
                         ((call-with-current-continuation 
                           (lambda (k) (let ((fail (lambda () (_ f2)))) s2)))
                          (equal? f (syntax-object->datum (syntax f2)))
                          (quasisyntax/loc tst (call-with-current-continuation 
                                                (lambda (k) 
                                                  (let ((fail (lambda () (k #,main-fail))))
                                                    #,(assm tst ((syntax fail)) (syntax s2)))))))
                         ;; leaving out pattern that is never used in original
                         (_ (quasisyntax/loc tst (if #,tst #,main-succ #,main-fail)))))))))
  
           (in (lambda (e l)
                 (or (member e l)
                     (and (eq? (car e) 'list?)
                          (or (member `(null? ,(cadr e)) l)
                              (member `(pair? ,(cadr e)) l)))
                     (and (eq? (car e) 'not)
                          (let* ((srch (cadr e))
                                 (const-class (equal-test? srch)))
                            (cond
                             (const-class (let mem ((l l))
                                            (if (null? l)
                                               #f
                                               (let ((x (car l)))
                                                 (or (and (equal?
                                                           (cadr x)
                                                           (cadr srch))
                                                          (disjoint? x)
                                                          (not (equal?
                                                                const-class
                                                                (car x))))
                                                     (equal?
                                                      x
                                                      `(not (,const-class
                                                             ,(cadr srch))))
                                                     (and (equal?
                                                           (cadr x)
                                                           (cadr srch))
                                                          (equal-test?
                                                           x)
                                                          (not (equal?
                                                                (caddr
                                                                 srch)
                                                                (caddr
                                                                 x))))
                                                     (mem (cdr l)))))))
                            ((disjoint? srch) (let mem ((l l))
                                                (if (null? l)
                                                    #f
                                                    (let ((x (car l)))
                                                      (or (and (equal?
                                                                (cadr x)
                                                                (cadr srch))
                                                               (disjoint?
                                                                x)
                                                               (not (equal?
                                                                     (car x)
                                                                     (car srch))))
                                                          (mem (cdr l)))))))
                            ((eq? (car srch) 'list?) (let mem ((l l))
                                                       (if (null? l)
                                                           #f
                                                           (let ((x (car l)))
                                                             (or (and (equal?
                                                                       (cadr x)
                                                                       (cadr srch))
                                                                      (disjoint?
                                                                       x)
                                                                      (not (memq (car x)
                                                                                 '(list?
                                                                                   pair?
                                                                                   null?))))
                                                                 (mem (cdr l)))))))
                            ((vec-structure? srch) (let mem ((l l))
                                                     (if (null? l)
                                                         #f
                                                         (let ((x (car l)))
                                                           (or (and (equal?
                                                                     (cadr x)
                                                                     (cadr srch))
                                                                    (or (disjoint?
                                                                         x)
                                                                        (vec-structure?
                                                                         x))
                                                                    (not (equal?
                                                                          (car x)
                                                                          'vector?))
                                                                    (not (equal?
                                                                          (car x)
                                                                          (car srch))))
                                                               (equal?
                                                                x
                                                                `(not (vector?
                                                                       ,(cadr srch))))
                                                               (mem (cdr l)))))))
                            (else #f)))))))
  
  
           (equal-test? (lambda (tst)
                        (and (eq? (car tst) 'equal?)
                             (let ((p (caddr tst)))
                               (cond
                                 ((string? p) 'string?)
                                 ((boolean? p) 'boolean?)
                                 ((char? p) 'char?)
                                 ((number? p) 'number?)
                                 ((and (pair? p)
                                       (pair? (cdr p))
                                       (null? (cddr p))
                                       (eq? 'quote (car p))
                                       (symbol? (cadr p))) 'symbol?)
                                 (else #f))))))
           (match:disjoint-predicates
            '(null? pair? symbol? boolean? number? string? char?
                    procedure? vector?
                    box?)) ; These are based on chez scheme
           (match:vector-structures '())
           (disjoint? 
            (lambda (tst)
              (memq (car tst) match:disjoint-predicates)))
           (vec-structure? (lambda (tst)
                             (memq (car tst) match:vector-structures)))
 
  ;; Add car operation, ie. given (c...r x), return (ca...r x).
           (add-a
            (lambda (exp-syntax)
              (syntax-case exp-syntax ()
                ((car-thing exp)
                 (let ((new (assq (syntax-object->datum (syntax car-thing)) c---rs)))
                   (if new 
                       (quasisyntax/loc exp-syntax (#,(cadr new) exp))
                       (syntax/loc exp-syntax (car (car-thing exp))))))
                (exp (syntax/loc exp-syntax (car exp))))))
  
           (add-d
            (lambda (exp-syntax)
              (syntax-case exp-syntax ()
                ((car-thing exp)
                 (let ((new (assq (syntax-object->datum (syntax car-thing)) c---rs)))
                   (if new 
                       (quasisyntax/loc exp-syntax (#,(cddr new) exp)) 
                       (syntax/loc exp-syntax (cdr (car-thing exp))))))
                (exp (syntax/loc exp-syntax (cdr exp))))))
  
           (c---rs '((car caar . cdar)
                     (cdr cadr . cddr)
                     (caar caaar . cdaar)
                     (cadr caadr . cdadr)
                     (cdar cadar . cddar)
                     (cddr caddr . cdddr)
                     (caaar caaaar . cdaaar)
                     (caadr caaadr . cdaadr)
                     (cadar caadar . cdadar)
                     (caddr caaddr . cdaddr)
                     (cdaar cadaar . cddaar)
                     (cdadr cadadr . cddadr)
                     (cddar caddar . cdddar)
                     (cdddr cadddr . cddddr)))
  
           (stx-dot-dot-k? 
            (lambda (syntax)
              (dot-dot-k? (syntax-object->datum syntax))))
  
           (dot-dot-k? (lambda (s)
                         (and (symbol? s)
                              (if (memq s '(... ___))
                                  0
                                  (let* ((s (symbol->string s))
                                         (n (string-length s)))
                                    (and (<= 3 n)
                                         (memq (string-ref s 0)
                                               '(#\. #\_))
                                         (memq (string-ref s 1)
                                               '(#\. #\_))
                                         (andmap
                                          char-numeric?
                                          (string->list
                                           (substring s 2 n)))
                                         (string->number
                                          (substring s 2 n))))))))
  
           (setter (lambda (e ident)
                     (let ((mk-setter (lambda (s)
                                        (symbol-append 'set- s '!))))
                       (syntax-case e (vector-ref unbox car cdr)
                         (p 
                          (not (stx-pair? (syntax p))) 
                          (match:syntax-err ident 
                                            "set! pattern should be nested inside of a list, vector or box"))
                         ((vector-ref vector index)
                          (syntax (let ((x vector))
                                    (lambda (y)
                                      (vector-set!
                                       x
                                       index
                                       y)))))
                         ((unbox boxed)
                          (syntax (let ((x boxed))
                                    (lambda (y)
                                      (set-box! x  y)))))
                         ((car exp)
                          (syntax (let ((x exp))
                                    (lambda (y)
                                      (set-car! x y)))))
                         ((cdr exp)
                          (syntax (let ((x exp))
                                    (lambda (y)
                                      (set-cdr! x y)))))
                         ((acc exp) 
                          (let ((a (assq (syntax-object->datum (syntax acc))
                                         get-c---rs)))
                            (if a 
                                (quasisyntax/loc ident 
                                                 (let ((x (#,(cadr a) exp)))
                                                   (lambda (y)
                                                     (#,(mk-setter (cddr a)) x y))))
                                (quasisyntax/loc ident 
                                                 (let ((x exp))
                                                   (lambda (y)
                                                     (#,(mk-setter (syntax-object->datum (syntax acc)))
                                                       x y)))))))))))
  
           (getter (lambda (e ident)
                     (syntax-case e (vector-ref unbox car cdr)
                       (p 
                        (not (stx-pair? (syntax p))) 
                        (match:syntax-err ident 
                                          "get! pattern should be nested inside of a list, vector or box"))
                       ((vector-ref vector index)
                        (syntax (let ((x vector))
                                  (lambda ()
                                    (vector-ref
                                     x
                                     index)))))
                       ((unbox boxed)
                        (syntax (let ((x boxed))
                                  (lambda () (unbox x)))))
                       ((car exp)
                        (syntax (let ((x exp))
                                  (lambda () (car x)))))
                       ((cdr exp)
                        (syntax (let ((x exp))
                                  (lambda () (cdr x y)))))
                       ((acc exp) 
                        (let ((a (assq (syntax-object->datum (syntax acc))
                                       get-c---rs)))
                          (if a 
                              (quasisyntax/loc ident 
                                               (let ((x (#,(cadr a) exp)))
                                                 (lambda () (#,(cddr a) x))))
                              (syntax/loc ident (let ((x exp))
                                                  (lambda ()
                                                    (acc x))))))))))
  
  
           (get-c---rs '((caar car . car)
                         (cadr cdr . car)
                         (cdar car . cdr)
                         (cddr cdr . cdr)
                         (caaar caar . car)
                         (caadr cadr . car)
                         (cadar cdar . car)
                         (caddr cddr . car)
                         (cdaar caar . cdr)
                         (cdadr cadr . cdr)
                         (cddar cdar . cdr)
                         (cdddr cddr . cdr)
                         (caaaar caaar . car)
                         (caaadr caadr . car)
                         (caadar cadar . car)
                         (caaddr caddr . car)
                         (cadaar cdaar . car)
                         (cadadr cdadr . car)
                         (caddar cddar . car)
                         (cadddr cdddr . car)
                         (cdaaar caaar . cdr)
                         (cdaadr caadr . cdr)
                         (cdadar cadar . cdr)
                         (cdaddr caddr . cdr)
                         (cddaar cdaar . cdr)
                         (cddadr cdadr . cdr)
                         (cdddar cddar . cdr)
                         (cddddr cdddr . cdr)))

           (match-mac (lambda (stx)
                        (syntax-case stx (=>)
                          ((_ exp clause ...)
                           (quasisyntax/loc stx (let ((x exp)) #,(gen-match (syntax x) 
                                                                            '() 
                                                                            (syntax (clause ...)) 
                                                                            stx)))))))
           (match-lambda-mac (lambda (stx)
                               (syntax-case stx ()
                                 [(k clause ...)
                                  (syntax/loc stx (lambda (exp) (match exp clause ...)))])))
           
           
           (match-lambda*-mac (lambda (stx)
                               (syntax-case stx ()
                                 [(k clause ...)
                                  (syntax/loc stx (lambda exp (match exp clause ...)))])))
           (match-let-mac
             (lambda (stx)
               (syntax-case stx ()
                 [(_ name () body1 body ...)
                  (syntax/loc stx (let name () body1 body ...))]
                 [(_ name ([pat1 exp1] [pat exp]...) body1 body ...)
                  (identifier? (syntax name))
                  (let ((pat-list (syntax-object->datum (syntax (pat1 pat ...))))
                        (real-name (syntax-object->datum (syntax name))))
                    (if (andmap pattern-var? pat-list)
                        (syntax/loc stx (let name ([pat1 exp1] [pat exp] ...) body1 body ...))
                        (syntax/loc stx
                         (letrec ([name
                                   (match-lambda* ((pat1 pat ...) body1 body ...))])
                           (name exp1 exp ...)))))]
                 [(_ () body1 body ...)
                  (syntax/loc stx (begin body1 body...))]
                 [(_ ([pat1 exp1] [pat exp]...) body1 body ...)
                  (syntax/loc stx ((match-lambda* ((pat1 pat ...) body1 body ...)) exp1 exp ...))])))
           (match-let*-mac
               (lambda (stx)
                 (syntax-case stx ()
                   ((_ () body body1 ...)
                    (syntax/loc stx (let* () body body1 ...)))
                   ((_ ([pat exp] rest ...) body body1 ...)
                    (if (pattern-var? (syntax-object->datum (syntax pat)))
                        (syntax/loc stx (let ([pat exp]) (match-let* (rest ...) body body1 ...)))
                        (syntax/loc stx (match exp [pat (match-let* (rest ...) body body1 ...)])))))))
           
           (match-letrec-mac
             (lambda (stx)
               (syntax-case stx ()
                 ((_ () body body1 ...)
                  (syntax/loc stx (let () body body1 ...)))
                 ((_ ([pat exp] ...) body body1 ...)
                  (andmap pattern-var?
                          (syntax-object->datum (syntax (pat ...)))) ;if they are not patterns
                  (syntax/loc stx (letrec ([pat exp] ...) body body1 ...)))
                 ((_ ([pat exp] ...) body body1 ...)
                  (let* ((**match-bound-vars** '())
                         (compiled-match (gen-match (syntax the-exp);(syntax (list exp ...)) 
                                             '() 
                                             (syntax (((pat ...) never-used))) 
                                             stx
                                             (lambda (sf bv lbsf)
                                               (set! **match-bound-vars** bv) 
                                               (quasisyntax/loc stx (begin 
                                                                      #,@(map (lambda (x)
                                                                                #`(set! #,(car x) #,(cdr x)))
                                                                              (reverse bv))
                                                                      body body1 ...)))))) 
                    (quasisyntax/loc stx (letrec (#,@(map 
                                                      (lambda (x) (quasisyntax/loc stx (#,(car x) #f)))
                                                      (reverse **match-bound-vars**))
                                                   (the-exp (list exp ...)))
                                     #,compiled-match)))))))
           
          (match-define-mac
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
                                   (lambda (sf bv lbsf)
                                     (set! **match-bound-vars** bv) 
                                     (quasisyntax/loc stx (begin 
                                                            #,@(map (lambda (x)
                                                                      (quasisyntax/loc stx 
                                                                                       (set! #,(car x) #,(cdr x))))
                                                                    (reverse bv))))))))
                  (quasisyntax/loc stx 
                                   (begin #,@(map 
                                              (lambda (x) (quasisyntax/loc stx (define #,(car x) #f)))
                                              (reverse **match-bound-vars**))
                                          (let ((the-exp exp))
                                            #,compiled-match))))])))
           ) ;; end of let rec binding area
    (values match-mac 
            match-lambda-mac 
            match-lambda*-mac 
            match-let-mac 
            match-let*-mac 
            match-letrec-mac 
            match-define-mac)))

)
 ;end