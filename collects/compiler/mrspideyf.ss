  (unit/sig mrspidey:file-read^
    (import) 
    (define (make-file-thunk-thunk filename)   	
      (lambda () 
	(let ([file-port (open-input-file filename)])
	  (lambda ()
	    (let ([c (read-char file-port)])
	      (when (eof-object? c)
		    (close-input-port file-port))
		 c))))))
   
