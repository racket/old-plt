(let ([old (error-escape-handler)])
  (+ (let/ec k
       (dynamic-wind
	(lambda () (error-escape-handler (lambda () (k 5))))
	(lambda () (car))
	(lambda () (error-escape-handler old))))
     10))
    
