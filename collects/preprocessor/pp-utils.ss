(module pp-utils mzscheme

(provide stdin stdout stderr cd)
(define stdin  current-input-port)
(define stdout current-output-port)
(define stderr current-error-port)
(define cd     current-directory)

)
