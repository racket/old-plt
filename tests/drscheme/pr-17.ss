;;; pr-17.ss

;;; Create new frame, check that all buttons and menus exist

;;; Author: Paul Steckler 

(load "/home/steck/plt/tests/drscheme/drscheme-test-util.ss")

(define-macro check-for-button 
  (lambda (button s)
    `(unless (ivar drscheme-frame-new ,button)
	     (printf "Missing ~a button" ,s))))

(let* ([drscheme-frame (wait-for-drscheme-frame)] 
       [drscheme-frame-new 'dummy]
       [menubar (send drscheme-frame get-menu-bar)]
       [menubar-new 'dummy]
       [menus-expected '("File" "Edit" "Show" "Scheme" "Language" "Help")]
       [buttons-expected '(check-syntax analyze execute break help)]
       [check-menus
	(lambda ()
	  (letrec ([loop
		    (lambda (lst n)
		      (if (null? lst) 
			  #t
			  (let ([expected-item (car lst)]
				[actual-item (send menubar-new get-label-top n)])
			    (if (string=? expected-item actual-item)
				(loop (cdr lst) (add1 n))
				(printf "Expected menu ~a but found ~a~n"
					expected-item
					actual-item)))))])
	    (loop menus-expected 0)))]
       [button-error
	(lambda (s)
	  (printf "Can't find ~a button~n" s))])

  ; open new unit window

  (mred:test:menu-select "File" "New")

  ; get data structures for new window

  (set! drscheme-frame-new (wait-for-new-drscheme-frame drscheme-frame))
  (set! menubar-new (send drscheme-frame-new get-menu-bar))

  ; compare old and new

  (printf "Checking menus ... ")

  (check-menus)

  (printf "checking buttons ... ")

  (check-for-button check-syntax-button "check syntax") 
  (check-for-button analyze-button "analyze") 
  (check-for-button execute-button "execute")
  (check-for-button stop-execute-button "break")
  (check-for-button help-button "help")

  (printf "done~n")

  (mred:test:menu-select "File" "Close"))



        
