(load-relative "sig.ss")
(unit/sig error^
  (import jvm^)
  
  ;; report-error : Int jobject(String) -> Void
  (define (report-error pos str)
    (printf "Compilation error: ~a : ~a ~n" pos (jstring->string str)))

  ;; report-error : Int jobject(String) -> Void
  (define (report-warning pos str)
    (printf "Warning: ~a : ~a ~n" pos (jstring->string str))))