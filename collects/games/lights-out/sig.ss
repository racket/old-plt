(require-library "functios.ss")

;; (define-type board (vector-of (vector-of (union 'x 'o))))

(define-signature lights-out:gui^
  (init-board))   ;; : (board -> void) resets the window(s)

(define-signature lights-out:board^
  (new-board      ;; : (-> board)  querys user
   random-board)) ;; : (num -> board)
