
;; Poor man's stack-trace-on-exceptions/profiler

;; see doc.txt for information

(define-values (print-error-trace 
		error-context-display-depth 
		instrumenting-enabled 
		profiling-enabled
		profile-paths-enabled 
		get-profile-results)
  (invoke-unit (require-library "errortracer.ss" "errortrace")))

 
