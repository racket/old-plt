
;; This implementation of `unit/sig' was ported from the old v100
;; implementation, and then hacked a bit to produce more compact
;; output, and finally mangled to handle the v200 `struct' (with
;; compile-time information). It's in dire need of an overhaul.

(module unitsig mzscheme
  (require "unit.ss")
  (require "private/sigmatch.ss")

  (require-for-syntax "private/sigutil.ss")
  (require-for-syntax "private/sigmatch.ss")
  (require-for-syntax (lib "kerncase.ss" "syntax"))

  (define-struct unit/sig (unit imports exports))

  (define-syntax define-signature
    (lambda (expr)
      (syntax-case expr ()
	[(_ name sig)
	 (identifier? (syntax name))
	 (let ([sig (get-sig 'define-signature expr (syntax-e (syntax name))
			     (syntax sig))])
	   (with-syntax ([content (explode-sig sig #f)])
	     (syntax (define-syntax name
		       (make-sig (quote content))))))])))

  (define-syntax let-signature
    (lambda (expr)
      (syntax-case expr ()
	[(_ name sig . body)
	 (identifier? (syntax name))
	 (let ([sig (get-sig 'let-signature expr (syntax-e (syntax name))
			     (syntax sig))])
	   (with-syntax ([content (explode-sig sig #f)])
	     (syntax (letrec-syntax ([name (make-sig (quote content))])
		       . body))))])))
  
  (define-syntax unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ sig . rest)
	 (let ([sig (get-sig 'unit/sig expr #f (syntax sig))])
	  (let ([a-unit (parse-unit expr (syntax rest) sig
				    (kernel-form-identifier-list (quote-syntax here))
				    (quote-syntax define-values)
				    (quote-syntax begin))])
	    (check-signature-unit-body sig a-unit (parse-unit-renames a-unit) 'unit/sig expr)
	    (with-syntax ([imports (datum->syntax-object
				    expr
				    (flatten-signatures (parse-unit-imports a-unit))
				    expr)]
			  [exports (datum->syntax-object
				    expr
				    (map
				     (lambda (name)
				       (list (do-rename name (parse-unit-renames a-unit))
					     name))
				     (signature-vars sig))
				    expr)]
			  [body (append
				 ((parse-unit-stxes a-unit) expr)
				 (reverse! (parse-unit-body a-unit))
				 ((parse-unit-stx-checks a-unit) expr))]
			  [import-sigs (explode-named-sigs (parse-unit-imports a-unit) #f)]
			  [export-sig (explode-sig sig #f)])
	      (syntax/loc expr
		(make-unit/sig
		 (unit
		   (import . imports)
		   (export . exports)
		   . body)
		 (quote import-sigs)
		 (quote export-sig))))))])))

  (define-syntax compound-unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ . body)
	 (let-values ([(tags
			exprs
			exploded-link-imports
			exploded-link-exports
			flat-imports
			link-imports
			flat-exports
			exploded-imports
			exploded-exports
			boxed-interned-symbol-vectors)
		       (parse-compound-unit expr (syntax body))]
		      [(t) (lambda (l) (datum->syntax-object expr l expr))])
	   (with-syntax ([(tag ...) (t tags)]
			 [(uexpr ...) (t exprs)]
			 [(tagx ...) (t (map (lambda (t) (string->symbol (format "u:~a" t))) tags))]
			 [exploded-link-imports (t exploded-link-imports)]
			 [exploded-link-exports (t exploded-link-exports)]
			 [flat-imports (t flat-imports)]
			 [(link-import ...) (t link-imports)]
			 [flat-exports (t flat-exports)]
			 [exploded-imports (t exploded-imports)]
			 [exploded-exports (t exploded-exports)]
			 [interned-vectors (t (map (lambda (x) `(,(car x) (quote ,(cadr x))))
						   (unbox boxed-interned-symbol-vectors)))])
	     (syntax/loc
	      expr
	      (let ([tagx uexpr] ... . interned-vectors)
		(verify-linkage-signature-match
		 'compound-unit/sig
		 '(tag ...)
		 (list tagx ...)
		 `exploded-link-imports
		 `exploded-link-exports)
		;; All checks done. Make the unit:
		(make-unit/sig
		 (compound-unit
		  (import . flat-imports)
		  (link [tag ((unit/sig-unit tagx)
			      . link-import)]
			...)
		  (export . flat-exports))
		 `exploded-imports
		 `exploded-exports)))))])))

  (define-syntax invoke-unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ u sig ...)
	 (let ([sigs (parse-invoke-vars 'invoke-unit/sig (syntax (sig ...)) expr)])
	   (with-syntax ([exploded-sigs (datum->syntax-object
					 expr
					 (explode-named-sigs sigs #f)
					 expr)]
			 [flat-sigs (datum->syntax-object
				     expr
				     (flatten-signatures sigs) 
				     expr)])
	     (syntax/loc
	      expr
	      (let ([unt u])
		(verify-linkage-signature-match
		 (quote invoke-unit/sig)
		 (quote (invoke))
		 (list unt)
		 (quote ((#() . #())))
		 (quote (exploded-sigs)))
		(invoke-unit (unit/sig-unit u)
			     . flat-sigs)))))])))
  
  (define-syntax unit->unit/sig
    (lambda (expr)
      (syntax-case expr ()
	[(_ e (im-sig ...) ex-sig)
	 (let ([im-sigs (map (lambda (sig)
			       (get-sig 'unit->unit/sig expr #f sig))
			     (syntax->list (syntax (im-sig ...))))]
	       [ex-sig (get-sig 'unit->unit/sig expr #f (syntax ex-sig))])
	   (with-syntax ([exploded-imports (datum->syntax-object
					    expr
					    (explode-named-sigs im-sigs #f)
					    expr)]
			 [exploded-exports (datum->syntax-object
					    expr
					    (explode-sig ex-sig #f)
					    expr)])
	     (syntax
	      (make-unit/sig
	       e
	       (quote exploded-imports)
	      (quote exploded-exports)))))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define verify-linkage-signature-match
    (let ([make-exn make-exn:unit]
	  [p-suffix (lambda (pos) (case pos [(1) 'st][(2) 'nd][(3) 'rd][else 'th]))])
      (lambda (who tags units esigs isigs)
	(for-each
	 (lambda (u tag)
	   (unless (unit/sig? u)
	     (raise
	      (make-exn
	       (string->immutable-string
		(format
		 "~s: expression for \"~s\" is not a signed unit: ~e"
		 who tag u))
	       (current-continuation-marks)))))
	 units tags)
	(for-each
	 (lambda (u tag esig)
	   (verify-signature-match
	    who #f
	    (format "specified export signature for ~a" tag)
	    esig
	    (format "export signature for actual ~a sub-unit" tag)
	    (unit/sig-exports u)))
	 units tags esigs)
	(for-each
	 (lambda (u tag isig)
	   (let ([n (length (unit/sig-imports u))]
		 [c (length isig)])
	     (unless (= c n)
	       (raise
		(make-exn
		 (string->immutable-string
		  (format
		   "~s: ~a unit imports ~a units, but ~a units were provided"
		   who tag n c))
		 (current-continuation-marks))))))
	 units tags isigs)
	(for-each
	 (lambda (u tag isig)
	   (let loop ([isig isig][expecteds (unit/sig-imports u)][pos 1])
	     (unless (null? isig)
	       (let ([expected (car expecteds)]
		     [provided (car isig)])
		 (verify-signature-match
		  who #t
		  (format "~a unit's ~s~s import (which is ~a)" tag
			  pos (p-suffix pos)
			  (car expected))
		  (cdr expected)
		  (format "~a's ~s~s linkage (which is ~a)"
			  tag
			  pos (p-suffix pos)
			  (car provided))
		  (cdr provided))
		 (loop (cdr isig) (cdr expecteds) (add1 pos))))))
	 units tags isigs))))

  (define-syntax signature->symbols
    (lambda (stx)
      (syntax-case stx ()
	[(_ name)
	 (identifier? (syntax name))
	 (let ([sig (get-sig 'signature->symbols stx #f (syntax name))])
	   (with-syntax ([e (explode-sig sig #f)])
	     (syntax 'e)))])))

  ;; Internal:
  (define-syntax do-define-values/invoke-unit/sig
    (lambda (stx)
      (syntax-case stx ()
	[(_ global? signame unite prefix imports orig)
	 (let* ([formname (if (syntax-e (syntax global?))
			      'namespace-variable-bind/invoke-unit/sig
			      'define-values/invoke-unit/sig)]
		[badsyntax (lambda (s why)
			     (raise-syntax-error
			      formname
			      (format "bad syntax (~a)" why)
			      (syntax orig)
			      s))])
	   (unless (or (not (syntax-e (syntax prefix)))
		       (identifier? (syntax prefix)))
	     (badsyntax (syntax prefix) "prefix is not an identifier"))
	   (let ([ex-sig (get-sig formname (syntax orig) #f (syntax signame))])
	     (let ([ex-exploded (explode-sig ex-sig #f)]
		   [ex-flattened (flatten-signature #f ex-sig)])
	       (let ([im-sigs
		      (parse-invoke-vars formname (syntax imports) (syntax orig))])
		 (let ([im-explodeds (explode-named-sigs im-sigs #f)]
		       [im-flattened (flatten-signatures im-sigs)]
		       [d->s (lambda (x) (datum->syntax-object (syntax orig) x (syntax orig)))])
		   (with-syntax ([dv/iu (if (syntax-e (syntax global?))
					    (quote-syntax namespace-variable-bind/invoke-unit)
					    (quote-syntax define-values/invoke-unit))]
				 [ex-flattened (d->s ex-flattened)]
				 [ex-exploded (d->s ex-exploded)]
				 [im-explodeds (d->s im-explodeds)]
				 [im-flattened (d->s im-flattened)]
				 [formname formname]
				 [stx-decls (if (syntax-e (syntax global?))
						null
						(make-struct-stx-decls ex-sig #f #f (syntax signame) #f))])
		     (syntax
		      (begin
			(dv/iu
			 ex-flattened
			 (let ([unit-var unite])
			   (verify-linkage-signature-match
			    'formname
			    '(invoke)
			    (list unit-var)
			    '(ex-exploded)
			    '(im-explodeds))
			   (unit/sig-unit unit-var))
			 prefix
			 . im-flattened)
			. stx-decls))))))))])))
  
  (define-syntax define-values/invoke-unit/sig
    (lambda (stx)
      (with-syntax ([orig stx])
	(syntax-case stx ()
	  [(_ signame unit prefix . imports)
	   (syntax (do-define-values/invoke-unit/sig #f signame unit prefix imports orig))]
	  [(_ signame unit)
	   (syntax (do-define-values/invoke-unit/sig #f signame unit #f () orig))]))))

  (define-syntax namespace-variable-bind/invoke-unit/sig
    (lambda (stx)
      (with-syntax ([orig stx])
	(syntax-case stx ()
	  [(_ signame unit prefix . imports)
	   (syntax (do-define-values/invoke-unit/sig #t signame unit prefix imports orig))]
	  [(_ signame unit)
	   (syntax (do-define-values/invoke-unit/sig #t signame unit #f () orig))]))))

  (define-syntax provide-signature-elements
    (lambda (stx)
      (with-syntax ([orig stx])
	(syntax-case stx ()
	  [(_ signame)
	   (let ([sig (get-sig 'provide-signature-elements stx #f (syntax signame))])
	     (let ([flattened (flatten-signature #f sig)])
	       (with-syntax ([flattened (map (lambda (x) (datum->syntax-object (syntax signame) x #f))
					     flattened)])
		 (syntax/loc stx
		   (provide . flattened)))))]))))
  
  (provide define-signature
	   let-signature
	   unit/sig
	   compound-unit/sig
	   invoke-unit/sig
	   unit->unit/sig
	   signature->symbols
	   verify-linkage-signature-match

	   (struct unit/sig (unit imports exports))
	   
	   define-values/invoke-unit/sig
	   namespace-variable-bind/invoke-unit/sig
	   provide-signature-elements))

