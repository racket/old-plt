
(invoke-unit
 (unit 
  (import)
  (export)
  
  (values
   ; reference-unit, etc.
   (lambda (must-string? require? sig? sname)
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
				'#%require-library
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
		 result))))
 
   ; reference
   (lambda (must-string? require?)
     (lambda names
       (let ([sname (if require? 
			'reference-library
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
	   `(,(if require? 'require-library '#%load/use-compiled) 
	     ,@names))))))))


