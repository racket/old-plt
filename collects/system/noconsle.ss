;; this file will allow you to start up mred without a console window,
;; for debugging purposes. to use this file, run mred like this:
;;		   mred -a noconsole.ss noconsig.ss
;; note that under X windows, you must still have the display set
;; to something valid.

(unit/sig mred:application^
  (import [mred : mred^]
	  [core : mzlib:core^])
  (define app-name "MrEdNoConsole")
  (define console #f)
  (define eval-string (lambda (s) (eval (read (open-input-string s)))))
  (display (mred:welcome-message))
  (thread (lambda () (read-eval-print-loop) (mred:exit))))
