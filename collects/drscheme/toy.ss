(printf "loading toy@~n")
(define tool@
  (unit/sig ()
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [print-convert : mzlib:print-convert^]
	    [drscheme : drscheme:export^]
	    [zodiac : zodiac:system^])

    (printf "invoking toy@~n")
    ;; get the frame group
    (define group (ivar drscheme:console group))

    ;; this function will be applied to every frame in the project
    (define to-each-frame 
      (lambda (frame)
	(let ([callback
	       (lambda ()
		 (let ([edit (send frame get-program-edit)]
		       [console-edit (send frame get-console-edit)])
		   ;; grab the current edit for this frame (it changes)
		   (send edit insert "toy")
		   (send console-edit insert "toy")
		   (send console-edit send-scheme 'toy)
		   (wx:bell)))])
	  
	  ;; install a menu in each frame
	  (let* ([menu-bar (send frame get-menu-bar)]
		 [new-menu (make-object mred:menu%)])
	    (send menu-bar append new-menu "Toy")
	    (send new-menu append-item "Toy" callback 
		  "This is a toy example help string" #f "t"))

	  ;; install a button in each frame
	  (let* ([panel (ivar frame button-panel)]
		 [button (make-object mred:button% panel
				      (lambda (button evt) (callback))
				      "Toy")])

	    (send panel change-children
		  (lambda (l)
		    (cons button
			  (mzlib:function@:remove button l))))))))

    ;; apply the function to every frame in the group
    (send group for-each-frame to-each-frame)

    (for-each (lambda (x)
		(printf "checking: ~a against ~a" x (eval x)))
	      (list 'mred:make-child-info
		    'zodiac:make-parsed))

    (printf "invoked toy@~n")))

(printf "loaded toy@~n")
