
; Map "(constant x)" to "(constant-symbol 'x)" if constants are
;  disabled
(define-macro constant 
  (lambda (symbol)
    `(constant-name (quote ,symbol))))

