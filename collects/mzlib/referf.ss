
 (unit
  (import)
  (export make-reference-unit make-reference)
  
  (define make-reference-unit
   ; reference-unit, etc.
   (lambda (must-string? require? reqrel? sig? sname)
     (lambda names
       (let ([len (length names)]
	     [expect (if require? +inf.0 1)])
	 (unless (and (positive? len) (<= len expect))
		 ((raise-syntax-error sname
				      (format "expected ~a names; given ~a"
					      expect len)
				      (list* sname names)))))
       (let ([names (if must-string?
			(map local-expand-defmacro names)
			names)])
	 (unless (or (not must-string?))
		 (for-each
		  (lambda (s)
		    (unless s
			    (raise-syntax-error sname
						"name is not a string"
						(list* sname names)
						s)))
		  names))
	 `(#%let ([result (,(if require?
				(if reqrel? 
				    '#%require-relative-library
				    '#%require-library)
				'#%load/use-compiled) ,@names)])
		 (#%unless (,(if sig?
				 '#%unit/sig?
				 '#%unit?)
			    result)
			   (#%raise
			    (,(if sig?
				  '#%make-exn:unit:signature:non-signed-unit
				  '#%make-exn:unit:non-unit)
			     ,(format "~s: result from ~s is not a ~aunit"
				      sname names (if sig? "signed " ""))
			     ((debug-info-handler))
			     result)))
		 result)))))

  (define make-reference
   ; reference
   (lambda (must-string? require? reqrel?)
     (lambda names
       (let ([sname (if require? 
			(if reqrel?
			    'reference-relative-library
			    'reference-library)
			'reference)]
	     [len (length names)]
	     [expect (if require? +inf.0 1)])
	 (unless (and (positive? len) (<= len expect))
		 ((raise-syntax-error sname
				      (format "expected ~a names; given ~a"
					      expect len)
				      (list* sname names))))
	 (let ([names (if must-string?
			  (map local-expand-defmacro names)
			  names)])
	   (unless (or (not must-string?) (map string? names))
		   (raise-syntax-error sname
				       "filename is not a string"
				       (list* sname names)))
	   `(,(if require? 
		  (if reqrel?
		      'require-relative-library
		      'require-library)
		  '#%load/use-compiled) 
	     ,@names))))))

  (values make-reference-unit make-reference))
