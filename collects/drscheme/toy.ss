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
	;; install a menu in each frame
	(let ([callback
	       (lambda ()
		 (let ([edit (send frame get-program-edit)])
		   ;; grab the current edit for this frame (it changes)

		   (wx:bell)))])
	  
	  ;; install a menu in each frame
	  (let* ([menu-bar (send frame get-menu-bar)]
		 [new-menu (make-object mred:menu%)])
	    (send menu-bar append new-menu "Toy")
	    (send new-menu append-item "Toy" callback 
		  "This is a toy example help string" #f "t"))

	  ;; install a button in each frame
	  (let ([new-button (make-object mred:button% "Toy" callback)])
	    (send (ivar frame button-panel) add-child new-button)))))

    ;; apply the function to every frame in the group
    (send group for-each-frame to-each-frame)
    
    (printf "invoked toy@~n")))

(printf "loaded toy@~n")
