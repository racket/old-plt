(module package-helper mzscheme
  (require (lib "stx.ss" "syntax")
	   (lib "boundmap.ss" "syntax"))

  (provide str str? str-renames str-all-renames make-str 
	   check-defn-context
	   pre-register-package
	   re-pre-register-package 
	   remove-dups stx-assoc mark-to-localize rebuild rebuild-cons
	   split open
	   protect not-bound-tag
	   walk-path)

  ;; A compile-time struct for package info:
  (define-struct str (renames all-renames))
  ;; renames = renames for exports
  ;; all-renames = all internal renames (needed to determine
  ;;               the appropriate shadowing variable when `open'
  ;;               appears in a `package' body)

  ;; The mark-to-localize function detects uses of `protect'
  ;; to prevent localization.
  (define-syntax protect
    (syntax-rules ()
      [(_ id) (quote-syntax id)]))

  ;; Used in a macro expansion to check that an id was defined:
  (define not-bound-tag (gensym))

  ;; Helper:
  (define (check-defn-context stx)
    (when (eq? 'expression (syntax-local-context))
      (raise-syntax-error
       #f
       "allowed only in definition contexts"
       stx)))
  
  (define pre-registered (make-hash-table 'weak))
  ;; maps context keys to context-hash,
  ;; where a contetx hash maps id to (cons renames sub-context-hash-or-#f)

  (define (pre-register-package expand-ctx name renames all-renames introducers protect-stx)
    ;; expand-ctx is the context used for expanding the body;
    ;; if it's a list longer than 1, then the package itself was
    ;; expanded in an internal-def position. In that case,
    ;; we register the just-created package in the table. We
    ;; also remember any packages that were registered with
    ;; `(car expand-ctx)'.
    (when (> (length expand-ctx) 1)
      (let ([sub-ht (hash-table-get pre-registered (car expand-ctx) (lambda () #f))])
	(do-pre-register-package
	 (cadr expand-ctx)
	 (syntax-local-introduce name)
	 (list (map (lambda (i)
		      (cons (syntax-local-introduce (car i)) 
			    (syntax-local-introduce (cdr i))))
		    renames)
	       (map (lambda (i)
		      (cons (syntax-local-introduce (car i)) 
			    (syntax-local-introduce (cdr i))))
		    all-renames)
	       (convert-subs sub-ht introducers protect-stx))))))
  
  (define (do-pre-register-package immediate-ctx name val)
    (let ([ht (hash-table-get pre-registered immediate-ctx
			      (lambda ()
				(let ([ht (make-bound-identifier-mapping)])
				  (hash-table-put! pre-registered immediate-ctx ht)
				  ht)))])
      (bound-identifier-mapping-put! 
       ht 
       name
       val)))

  ;; As pre-registration shifts the registrations of packages
  ;; within the pre-registered package, it needs to localize
  ;; the info in the sub-pre-register
  (define (convert-subs rn introducers protect-stx)
    (and rn
	 (let ([naya (make-bound-identifier-mapping)])
	   (bound-identifier-mapping-for-each
	    rn
	    (lambda (id v)
	      (bound-identifier-mapping-put!
	       naya
	       id
	       (list (map (lambda (i)
			    (cons (car i)
				  (intro-mark-to-localize (cdr i) introducers protect-stx)))
			  (car v))
		     (map (lambda (i)
			    (cons (car i)
				  (intro-mark-to-localize (cdr i) introducers protect-stx)))
			  (cadr v))
		     (convert-subs (caddr v) introducers protect-stx)))))
	   naya)))

  ;; Gets info for a package that has been expanded but not
  ;; yet executed as syntax.
  (define (get-pre-registered-package use-ctx name)
    (and (pair? use-ctx)
	 (ormap (lambda (ctx)
		  (let ([ht (hash-table-get pre-registered ctx (lambda () #f))])
		    (and ht
			 (bound-identifier-mapping-get ht 
						       name
						       (lambda () #f)))))
		use-ctx)))

  ;; When `open' exposes a package, we need to pre-register it.
  ;; Ditto for renaming a package.
  (define (re-pre-register-package subs expand-ctx name id)
    (let ([v (if subs
		 (bound-identifier-mapping-get subs id (lambda () #f))
		 (get-renames id (lambda (x) (lambda () #f))))])
      (when v
	(do-pre-register-package
	 (car expand-ctx)
	 name
	 v))))

  ;; Removes dups from * defns
  (define (remove-dups l)
    (let ((ht (make-bound-identifier-mapping)))
      (let loop ((l l))
        (cond
          ((null? l) (bound-identifier-mapping-map ht (lambda (k v) v)))
          ((bound-identifier-mapping-get ht (caar l) (lambda () #f)) (loop (cdr l)))
          (else 
           (bound-identifier-mapping-put! ht (caar l) (car l))
	   (loop (cdr l)))))))
  
  (define (stx-assoc id renames)
    (cond
     ((null? renames) #f)
     ((bound-identifier=? id (caar renames)) (car renames))
     (else (stx-assoc id (cdr renames)))))
  
  (define (rebuild ctxt val)
    (if (syntax? ctxt)
	(datum->syntax-object ctxt val ctxt ctxt)
	val))
  
  (define (rebuild-cons car cdr stx)
    (rebuild stx (cons car cdr)))

  ;; Traverses an S-expression, "introducing" identifiers
  ;; so that they refer to bindings that will be hidden 
  ;; by the package. Don't localize protected ids, though.
  (define (mark-to-localize def introducers protect-stx)
    (let ((contents 
	   (if (syntax? def)
	       (syntax-e def)
	       def)))
      (cond
       ((symbol? contents)
	(let ((introducer (stx-assoc def introducers)))
	  (if introducer ((cdr introducer) def) def)))
       ((pair? contents)
	(if (and (identifier? (car contents))
		 (module-transformer-identifier=? protect-stx (car contents)))
	    def
	    (rebuild-cons (mark-to-localize (car contents) introducers protect-stx)
			  (mark-to-localize (cdr contents) introducers protect-stx)
			  def)))
       ((vector? contents)
	(rebuild def (list->vector
		      (map (lambda (x) (mark-to-localize x introducers protect-stx))
			   (vector->list contents)))))
       (else def))))

  (define (intro-mark-to-localize def introducers protect-stx)
    (syntax-local-introduce
     (mark-to-localize (syntax-local-introduce def) introducers protect-stx)))

  (define (split path)
    (let ((r (reverse path)))
      (values (reverse (cdr r)) (car r))))

  ;; Finds a package, either as a syntax definition or in the
  ;; pre-registration table.
  (define (get-renames id err)
    (let ((x (or (get-pre-registered-package (syntax-local-context) id)
		 (let ([v (syntax-local-value id (err id))])
		   (and (str? v)
			(list (str-renames v) (str-all-renames v) #f))))))
      (or x
	  ((err id)))))
  
  ;; Wraps `get-renames' with suitable error handling.
  (define (open name orig-name stx)
    (unless (identifier? name)
      (raise-syntax-error #f "path component must be an identifier" stx name))
    (let ((err (lambda (name)
                 (lambda ()
                   (raise-syntax-error #f "unknown package" stx (if (identifier? orig-name)
								    orig-name 
								    name))))))
      (get-renames (syntax-local-introduce name) err)))

  (define (walk-path path env+rns+subs stx rename cp-rename)
    (let loop ([path path][env+rns+subs env+rns+subs][rename rename])
      (cond
       [(null? path) (values env+rns+subs rename)]
       [else (let* ([id (cp-rename (syntax-local-introduce (car path)))]
		    [new-name (if (caddr env+rns+subs)
				  (cons id id)
				  (stx-assoc id (cadr env+rns+subs)))]
		    [v (and new-name
			    (if (caddr env+rns+subs)
				(bound-identifier-mapping-get (caddr env+rns+subs) 
							      (rename (cdr new-name))
							      (lambda () #f))
				(get-renames (rename (cdr new-name))
					     (lambda (x) (lambda () #f)))))])
	       (if v
		   (loop (cdr path) v (lambda (id)
					(let ([a (stx-assoc id (cadr env+rns+subs))])
					  (rename (if a
						      (cdr a)
						      id)))))
		   (raise-syntax-error
		    #f
		    "no such exported subpackage"
		    stx
		    (car path))))])))
  
  )
