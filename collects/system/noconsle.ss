;; This file will application is like the system console application,
;; except that stdin/stdout/stderr REP is started instead of a console
;; window. Run this application like this:
;;		   mred -a system noconsole.ss sig.ss
;; Note that under X Windows, you must still have the X display set
;; to something valid.

(unit/sig mred:application^
  (import [mred : mred^]
	  [core : mzlib:core^])
  (define app-name "MrEdNoConsole")
  (define console #f)
  (define eval-string (lambda (s) (eval (read (open-input-string s)))))
  (define startup (lambda args (for-each mred:edit-file args)))
  (display (mred:welcome-message))
  (thread (lambda () (read-eval-print-loop) (mred:exit))))
