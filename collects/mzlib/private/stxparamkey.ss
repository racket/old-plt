
(module stxparamkey mzscheme
  
  (define-values (struct:syntax-parameter make-syntax-parameter syntax-parameter? syntax-parameter-ref syntax-parameter-set!)
    (make-struct-type 'syntax-parameter #f 2 0 #f null (current-inspector) 0))

  (define (syntax-parameter-target sp)
    (syntax-parameter-ref sp 1))

  (define (syntax-parameter-target-value target)
    (syntax-local-value (syntax-local-get-shadower target)
			(lambda ()
			  #f
			  (syntax-local-value 
			   target
			   (lambda () #f)))))

  (provide syntax-parameter?
	   make-syntax-parameter
	   syntax-parameter-target
	   syntax-parameter-target-value))
	   

