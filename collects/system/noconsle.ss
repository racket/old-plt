;; this file will allow you to start up mred without a console window,
;; for debugging purposes. to use this file, run mred like this:
;;			 mred -f noconsole.ss
;; note that under X windows, you must still have the display set
;; to something valid.

;; if you want to use the mred:debug:make-new-console,
;; you must eval the definition of mred:make-application@ in mrsystem.ss
(define mred:make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred : mred^]
	      [core : mzlib:core^])
      (define console #f)
      (define eval-string (lambda (s) (eval (read (open-input-string s)))))
      (display mred:welcome-message)
      (newline)
      (thread read-eval-print-loop))))
