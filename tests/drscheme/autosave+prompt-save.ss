(load-relative "util.ss")

(let* ([frame (wait-for-drscheme-frame)]
       [_ (type-in-definitions frame "a")]
       [_ (mred:test:menu-select "File" "Close")]
       [_ (wait (lambda () (not (eq? (mred:test:get-active-frame) frame)))
		"didn't get a new frame after selecting File|Close")]
       [_ (mred:test:button-push "Cancel")]
       [_ (wait-pending)]
       [_ (wait (lambda () (eq? (mred:test:get-active-frame) frame))
		"didn't go back to original drscheme frame after cancelling close")]

       [_ (mred:test:menu-select "File" "Close")]
       [_ (wait (lambda () (not (eq? (mred:test:get-active-frame) frame)))
		"didn't get a new frame after selecting File|Close")]
       [_ (mred:test:button-push "Cancel")]
       [_ (wait-pending)]
       [_ (wait (lambda () (eq? (mred:test:get-active-frame) frame))
		"didn't go back to original drscheme frame after cancelling close")]

       [_ (mred:test:menu-select "File" "Close")]
       [_ (wait (lambda () (not (eq? (mred:test:get-active-frame) frame)))
		"didn't get a new frame after selecting File|Close")]
       [_ (mred:test:button-push "Cancel")]
       [_ (wait-pending)]
       [_ (wait (lambda () (eq? (mred:test:get-active-frame) frame))
		"didn't go back to original drscheme frame after cancelling close")])
  (printf "closing last frame test complete~n"))

((load-relative (build-path 'up "mred" "gui-main.ss"))
 "New Unit"
 "Save Definitions"
 wx:frame%)



       
       
       