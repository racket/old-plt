(time (load "mrsystem.ss"))

(let ([old mred:initialize])
  (set! mred:initialize 
	(lambda args
	  (set! mred:initialize old)
	  (time (apply old args)))))
