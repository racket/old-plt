
;; by Jacob Matthews

(module struct mzscheme
  (provide copy-struct)

  (require-for-syntax (lib "struct.ss" "syntax")
		      (lib "stx.ss" "syntax"))

  (define-syntax (copy-struct stx)
    (syntax-case stx ()
      [(_ info structure (accessor-name new-val) ...)
       (let ([ans (syntax->list  #'((accessor-name new-val) ...))])
	 (unless (identifier? #'info)
	   (raise-syntax-error #f "not an identifier for structure type" stx #'info))
	 (for-each (lambda (an)
		     (unless (identifier? (stx-car an))
		       (raise-syntax-error #f "not an identifier for accessor name" stx (stx-car an))))
		   ans)
	 
	 ;; new-binding-for : syntax[field-name] -> (union syntax[expression] #f)
	 (let ((new-binding-for 
		(lambda (f)
		  (ormap (lambda (x) 
			   (if (eq? (syntax-object->datum (stx-car x)) (syntax-object->datum f)) 
			       (cadr (syntax-e x)) 
			       #f))
			 ans))))
	   
	   (let-values ([(construct pred accessors)
			 (let ([v (syntax-local-value #'info (lambda () #f))])
			   (unless (struct-declaration-info? v)
			     (raise-syntax-error #f "identifier is not bound to a structure type" stx #'info))
			   (values (cadr v)
				   (caddr v)
				   (cadddr v)))]
			[(as) (map (lambda (an) (stx-car an)) ans)])
	     (for-each 
	      (lambda (field)
		(unless (ormap (lambda (f2) (module-or-top-identifier=? field f2)) accessors)
		  (raise-syntax-error #f "accessor name not associated with the given structure type" stx field)))
	      as)
	     
	     (let ((dupe (check-duplicate-identifier as)))
	       (when dupe 
		 (raise-syntax-error #f 
				     "duplicate field assignment" 
				     stx 
				     dupe)))
	     
	     ;; the actual result
	     #`(let ((the-struct structure))
		 (if (#,pred the-struct)
		     (#,construct
		      #,@(map 
			  (lambda (field) (or (new-binding-for field) #`(#,field the-struct)))
			  (reverse accessors)))
		     (raise-type-error '_  #,(format "struct:~a" (syntax-object->datum #'info)) the-struct))))))])))

    