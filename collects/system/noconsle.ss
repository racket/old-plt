;; This file will application is like the system console application,
;; except that stdin/stdout/stderr REP is started instead of a console
;; window. Run this application like this:
;;		   mred -a system noconsole.ss sig.ss
;; Note that under X Windows, you must still have the X display set
;; to something valid.

((reference-library "appl.ss" "system")
 (unit/sig (console)
   (import [mred : mred^]
	   [I : mred:application-imports^])
   (thread (lambda () (read-eval-print-loop) (mred:exit)))
   (define console #f)))
