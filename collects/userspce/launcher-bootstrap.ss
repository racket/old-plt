;; set things up so that the load-handler opens files into
;; a text when the file begins with WXME so that mred saved
;; files still load properly.

(when '(getenv "MREDDEBUG")
  (require-library "errortrace.ss" "errortrace") (error-print-width 200)
  (current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x)))))

(require-library "core.ss" "drscheme-jr")

((make-go
  (if (defined? 'mred@)
      (require-library "launcher-bootstrap-mred.ss" "guserspce")
      (require-library "launcher-bootstrap-mzscheme.ss" "userspce"))))
