
(require-library "macro.ss")

(let ([c%
       (class-asi canvas%
	 (override
	   [on-char
	    (lambda (ev)
	      (printf "code: ~a  meta: ~a  control: ~a  alt: ~a  shift: ~a~n" 
		      (let ([v (send ev get-key-code)])
			(if (symbol? v)
			    v
			    (char->integer v)))
		      (send ev get-meta-down)
		      (send ev get-control-down)
		      (send ev get-alt-down)
		      (send ev get-shift-down)))]))])
  (define f (make-object frame% "tests" #f 100 100))
  (define c (make-object c% f))
  (send f show #t))

