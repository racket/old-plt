
(define startup-url 
  (string-append "file:" 
		 (with-handlers ([void 
				  (lambda (x) 
				    (build-path (collection-path "help") "index.htm"))])
		   (let ([f (build-path (collection-path "doc") "help" "index.html")])
		     (if (file-exists? f)
			 f
			 (error "not there"))))))