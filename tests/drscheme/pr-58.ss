;;; pr-58.ss

;;; tests check-syntax when given bogus improper list
;;; tested at each language level

;;; Author: Paul Steckler

(load-relative "drscheme-test-util.ss")

(let* ([drs-frame (wait-for-drscheme-frame)] 
       [interactions-edit (ivar drs-frame interactions-edit)]
       [execute-button (ivar drs-frame execute-button)]
       [get-int-pos (lambda () (get-text-pos interactions-edit))]
       [check-check-syntax ; type in term, call check-syntax
	(lambda (str expected)
	  (clear-definitions drs-frame)
	  (type-in-definitions drs-frame str)
	  (push-button-and-wait execute-button)
	  (let ([answer-begin (+ (get-int-pos) 3)])
	    (mred:test:button-push (ivar drs-frame check-syntax-button))
	    (let ([answer-end (- (get-int-pos) 1)])
	      (let ([actual (send interactions-edit get-text
				  answer-begin answer-end)])
		(unless (string=? actual expected)
			(printf "Expected: ~a~n Actual: ~a~n~n"
				expected actual)))
	      (let ([frame (mred:test:get-active-frame)])
		(unless (eq? frame drs-frame)
			(error 'check-syntax "Unexpected window ~a" frame))))))]
       [set-language-level!
	(lambda (level)
	  (mred:test:menu-select "Language" "Configure Language...")
	  (mred:test:new-window (wx:find-window-by-name "Language" null))
	  (let* ([frame 
		  (letrec ([loop 
			    (lambda () 
			      (let ([active (mred:test:get-active-frame)])
				(if (or (eq? active #f)
					(eq? active drs-frame))
				    (begin
				      (sleep 1/2)
				      (loop))
				    active)))])
		     (loop))]
		 [o-panel (send frame get-top-panel)]
		 [o-children (ivar o-panel children)]
		 [i-panel (car o-children)]
		 [i-children (ivar i-panel children)]
		 [choice (cadr i-children)])
	    (mred:test:set-choice! choice level)
	    (mred:test:button-push "OK")))])

  (printf "Starting tests~n") 

  (set-language-level! "Beginner")

  (check-check-syntax "'(a . b)" "improper lists are not allowed")

  ; from pr-246 
  ; execute says "cons: second argument must be of type <list>, given 1 and 2")

  (check-check-syntax "(cons 1 2)" "")

  ; end pr-246

  (set-language-level! "Intermediate")
  (check-check-syntax "'(a . b)" "improper lists are not allowed")

  (set-language-level! "Advanced")
  (check-check-syntax "'(a . b)" "improper lists are not allowed")

  (set-language-level! "Quasi-R4RS")
  (check-check-syntax "'(a . b)" "")

  (printf "Finished tests~n"))
       
