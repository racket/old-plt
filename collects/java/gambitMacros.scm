;; Mario Latendresse, December 2000
;; Tentative definition of macros for Gambit 3.0 to emulate the
;; define-struct and match of MzScheme.

(define-macro (define-struct name l)
  `(begin
     ;; The make-...  function.
     (set! ,(string->symbol (string-append "make-" (symbol->string name)))
	   (lambda ,l (vector (quote ,name) ,@l)))
     ;; The ...? function
     (set! ,(string->symbol (string-append (symbol->string name) "?"))
	 (lambda (x) (and (vector? x) (eqv? (vector-ref x 0) (quote ,name)))))
     ;; All setter and access field functions.
     ,@(mapi
	(lambda (x i)
	  `(set! ,(string->symbol (string-append (symbol->string name)  "-"
						 (symbol->string x)))
		 (lambda (x) (vector-ref x ,(+ i 1))))
	  `(set! ,(string->symbol 
		   (string-append "set-" (symbol->string name)  "-"
				  (symbol->string x) "!"))
		 (lambda (x v) (vector-set! x ,(+ i 1) v))))
	l)
     ))

;; Test
;; (define-struct type (cl? name modifiers extend interfaces table inside?))

;; This is a simplified version of match where there is only one clause.
;; That's enough for the Java compiler.
(define-macro (match v l)
  (if (or (not (pair? l)) (not (pair? (car l))) (not (pair? (cdar l))))  
      (error "Match syntax error " l)
      `(if (or (not (vector? ,v)) (not (eqv? (vector-ref ,v 0) (quote ,(cadar l)))))
	   (error "No match possible with value " ,v " and parms " ,l)
	   (let* ,(mapi (lambda (parm i)
			   `(,parm  (vector-ref ,v ,(+ i 1))))
			(cddar l))
	     ,(cadr l)))))

;; Test match

;;(display
;; (match (make-type 1 2 3 4 5 6 7) (($ type a b c d e f g) (list a b c d e f g))))

(define my-directory-exists? file-exists?)
;; In Gambit, with-output-to-file replaces an existing file.
(define my-with-output-to-file (lambda (f p) (with-output-to-file f p)))
