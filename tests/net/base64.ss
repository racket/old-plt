
;; Uses (unix) `mimencode' program from your path.
;; Run this in a directory with lots of files to use as tests
;;  for base64 encoding

(require-library "base64.ss" "net")

(define (check-file/fastest p in)
  (let ([s1 (make-string 5000)]
	[s2 (make-string 5000)])
    (let loop ([leftover 0][startpos 0][pos 0])
      (let* ([n1 (if (zero? leftover)
		     (read-string-avail! s1 p)
		     leftover)]
	     [n2 (read-string-avail! s2 in 0 (if (eof-object? n1)
						 1
						 n1))])
	(unless (if (or (eof-object? n1)
			(eof-object? n2))
		    (and (eof-object? n1)
			 (eof-object? n2))
		    (if (= n2 n1 5000)
			(string=? s1 s2)
			(string=? (substring s1 startpos (+ startpos n2))
				  (substring s2 0 n2))))
	  (error 'check "failed at ~a (~a@~a ~a)" pos n1 startpos n2))
	(unless (eof-object? n1)
	  (loop (- n1 n2) 
		(if (= n1 n2)
		    0
		    (+ startpos n2))
		(+ pos n2)))))))

(define mimencode (find-executable-path "mimencode" #f))
(unless mimencode
  (error "Can't find mimencode"))

(for-each
 (lambda (f)
   (when (file-exists? f)
     (printf "trying ~a~n" f)
     (let-values ([(ceo cei cen cee cef) 
		   (apply values (process* mimencode f))]
		  [(ei eo) (make-pipe 4096)]
		  [(di do) (make-pipe 4096)])
       (close-output-port cei)
       (close-input-port cee)
       ;; Encode thread:
       (thread 
	(lambda ()
	  (let ([p (open-input-file f)])
	    (base64-encode-stream p eo)
	    (close-output-port eo)
	    (close-input-port p))))
       ;; Encode/decode thread:
       (thread 
	(lambda ()
	  (let ([p (open-input-file f)])
	    (define-values (xr xw) (make-pipe 4096))
	    (thread (lambda ()
		      (base64-encode-stream p xw)
		      (close-output-port xw)))
	    (base64-decode-stream xr do)
	    (close-output-port do)
	    (close-input-port p))))
       ;; Compare output
       (printf " encode~n")
       (check-file/fastest ei ceo)
       (close-input-port ceo)
       (printf " decode~n")
       (let ([p (open-input-file f)])
	 (check-file/fastest di p)
	 (close-input-port p)))))
 (directory-list))
