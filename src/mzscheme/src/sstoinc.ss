
(define to-zo? (member "--zo" (vector->list argv)))

(define DIGS-PER-LINE 20)

; (require (lib "src2src.ss" "compiler"))

(require mzscheme)

;; Uncomment to disable src->src optimization:
; (define (optimize x) x)

(let loop ()
  (let ([expr (read)])
    (unless (eof-object? expr)
      (let ([c (compile expr)]
	    [p (open-output-string)])
	(write c p)
	(let ([s (get-output-string p)])
	  (printf "  {~n    static MZCOMPILED_STRING_FAR unsigned char expr[] = {")
	  (let loop ([chars (string->list s)][pos 0])
	    (unless (null? chars)
	      (let ([char (car chars)])
		(printf "~a," (char->integer char)))
	      (loop (cdr chars)
		    (if (= pos DIGS-PER-LINE)
			(begin
			  (newline)
			  0)
			(add1 pos)))))
	  (printf "0};~n    EVAL_ONE_SIZED_STR((char *)expr, ~a);~n" (string-length s))
	  (printf "  }~n")))
      (loop))))


		   
