
(require "../parse-compiled.ss")

(parse-compiled (compile '(map (lambda (x) (+ 1 x))
			       '(1 2 3))))

