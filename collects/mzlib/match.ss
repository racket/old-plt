;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Matching Syntactic Extensions for Scheme
;;
;; Specialized for MzScheme; works with define-struct
;;
;; Report bugs to wright@research.nj.nec.com.  The most recent version of
;; this software can be obtained by anonymous FTP from ftp.nj.nec.com
;; in file pub/wright/match.tar.Z.  Be sure to set "type binary" when
;; transferring this file.
;;
;; Written by Andrew K. Wright, 1993 (wright@research.nj.nec.com).
;; Adapted from code originally written by Bruce F. Duba, 1991.
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
;;       | (match-define> pat exp)
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
;; End of user visible/modifiable stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module match mzscheme
  (require-for-syntax "private/mkmatch.ss")

  (provide
   match
   match-lambda
   match-lambda*
   match-letrec
   match-let
   match-let*)

 (define match:version "Version 1.10mz, Feb 5, 1996")

 (define-struct (exn:misc:match struct:exn:misc) (value))

 (define match:error
   (case-lambda
     ((val)
      (raise
       (make-exn:misc:match
         (format "match: no matching clause for ~s" val)
         (current-continuation-marks)
         val)))
     ((val expr)
      (raise
       (make-exn:misc:match
         (format "match: no matching clause for ~s: ~s" val expr)
         (current-continuation-marks)
         val)))))

 (define-syntax parse-pattern
   ;; NOT A MACRO: this is a macro utility function
   (lambda (p)
     (let parse-pattern ([p p])
       (define (r l) (map parse-pattern (syntax->list l)))
       (define (i v) (match:syntax-err p (format "illegal use of ~a" v)))
       (syntax-case p (_ quote $ ? and or not set! get! quasiquote ... ___)
	 [_ '_]
	 [(quote x) `(quote ,(syntax->datum (syntax x)))]
	 [(quote . _) (i "quote")]
	 [($ struct p ...)
	  `($ struct ,@(r (syntax (p ...))))]
	 [($ . _) (i "$")]
	 [(and p ...)
	  `(and ,@(r (syntax (p ...))))]
	 [(and . _) (i "and")]
	 [(or p ...)
	  `(or ,@(r (syntax (p ...))))]
	 [(or . _) (i "or")]
	 [(not p ...)
	  `(not ,@(r (syntax (p ...))))]
	 [(not . _) (i "not")]
	 [(? pred p ...)
	  `(? ,(syntax pred) ,@(r (syntax (p ...))))]
	 [(? . _) (i "?")]
	 [(set! i)
	  `(set! ,(syntax i))]
	 [(set! . _) (i "set!")]
	 [(get! i)
	  `(get! ,(syntax i))]
	 [(get! . _) (i "get!")]
	 [(quasiquote q)
	  `(,'quasiquote ,(:ucall parse-quasipattern (syntax q)))]
	 [(quasiquote . _) (i "quasiquote")]
	 [(p (... ...))
	  `(,(parse-pattern (syntax p)) ...)]
	 [(p ___)
	  `(,(parse-pattern (syntax p)) ___)]
	 [(p ..k)
	  (and (identifier? (syntax ..k))
	       (let ([s (symbol->string (syntax-e (syntax ..k)))])
		 (regexp-match re:..k s)))
	  `(,(parse-pattern (syntax p)) ,(syntax-e (syntax ..k)))]
	 [(p . rest)
	  (identifier? (syntax i))
	  `(,(parse-pattern (syntax p)) ,@(parse-pattern (syntax rest)))]
	 [i (identifier? (syntax i)) (syntax i)]
	 [_else
	  (let ([s (syntax-e p)])
	    (cond
	     [(vector? s) (list->vector (map parse-pattern (vector->list s)))]
	     [(box? s) (box (parse-pattern (unbox s)))]
	     [else s]))]))))

 (define-syntax parse-quasipattern
   ;; NOT A MACRO: this is a macro utility function
   (lambda (p)
     (define (i v) (match:syntax-err p (format "illegal use of ~a" v)))
     (let parse-quasipattern ([p p])
       (syntax-case p (unquote unquote-splicing ...)
	 [(unquote x) `(,'unquote ,(:ucall parse-pattern (syntax x)))]
	 [(unquote . _) (i "unquote")]
	 [(unquote-splicing x) `(,'unquote-splicing ,(:ucall parse-pattern (syntax x)))]
	 [(unquote-splicing . _) (i "unquote-splicing")]
	 [(p (... ...))
	  `(,(parse-quasipattern (syntax p)) ...)]
	 [(p ..k)
	  (and (identifier? (syntax ..k))
	       (let ([s (symbol->string (syntax-e (syntax ..k)))])
		 (regexp-match re:..k s)))
	  `(,(parse-quasipattern (syntax p)) ,(syntax-e (syntax ..k)))]
	 [(i . rest)
	  (identifier? (syntax i))
	  `(,(syntax->datum (syntax i)) ,@(parse-quasipattern (syntax rest)))]
         [(qp . rest)
	  `(,(parse-quasipattern (syntax qp)) ,@(parse-quasipattern (syntax rest)))]
	 [_else
	  (let ([s (syntax-e p)])
	    (cond
	     [(vector? s) (list->vector (map parse-quasipattern (vector->list s)))]
	     [(box? s) (box (parse-quasipattern (unbox s)))]
	     [else s]))]))))

 



 (define-syntax match
   (lambda (stx)
     (syntax-case stx ()
       [(_ exp clause ...)
	(with-syntax ([body
		       (datum->syntax
			(genmatch
			 (quote-syntax mv)
			 (map
			  (lambda (c)
			    (syntax-case c (=>)
			      [(p (=> i) e)
			       `(,(:ucall parse-pattern (syntax p))
				 (=> ,(syntax i))
				 ,(syntax e))]
			      [(p e)
			       `(,(:ucall parse-pattern (syntax p))
				 ,(syntax e))]
			      [_else
			       (match:syntax-err
				c
				"bad match clause")]))
			  (syntax->list (syntax (clause ...))))
			 stx)
			stx (quote-syntax here))])
	  (syntax
	   (let ([mv exp])
	     body)))])))

 (define-syntax match-lambda
   (lambda (stx)
     (syntax-case stx ()
       [(_ clause ...)
	(syntax (lambda (x) (match x clause ...)))])))

 (define-syntax match-lambda*
   (lambda (stx)
     (syntax-case stx ()
       [(_ clause ...)
	(syntax (lambda x (match x clause ...)))])))

 (define-syntax match-let*
   (lambda (stx)
     (syntax-case stx ()
       [(_ () body1 body ...)
	(syntax (begin body1 body ...))]
       [(_ ([pat1 exp1] [pat exp] ...) body1 body ...)
	(syntax (match exp1 
		  [pat1 (match-let* ([pat exp] ...) 
				    body1 body ...)]))])))
 (define-syntax match-let
   (lambda (stx)
     (syntax-case stx ()
       [(_ ([pat exp] ...) body1 body ...)	
	(syntax (match-let* ([(pat ...) (list exp ...)]) 
			    body1 body ...))])))

 (define-syntax match-letrec
   (lambda (stx)
     (syntax-case stx ()
       [(_ ([pat exp] ...) body1 body ...)
	(datum->syntax
	 (genletrec 
	  (map (lambda (p) (:ucall parse-pattern p)) (syntax->list (syntax (pat ...))))
	  (syntax->list (syntax (exp ...)))
	  (syntax->list (syntax (body1 body ...)))
	  stx)
	 stx (quote-syntax here))])))

 (define-syntax match-define
   (lambda (stx)
     (syntax-case stx ()
       [(_ pat exp)
	(datum->syntax
	 (gendefine (map (lambda (p) (:ucall parse-pattern p)) (syntax pat))
		    (syntax exp)
		    stx)
	 stx (quote-syntax here))]))))
