(unit/sig newspidey:datadef-setexp^
  (import)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set expressions

;; val = number | symbol | string
(define-struct Const (val))
;; name = 'pair
(define-struct Token (name))
;; name = symbol
(define-struct Set-var (name))
;; arity = Arity, pos = number, set-var = Set-var
(define-struct Dom-arity (arity pos set-var))
;; interval = Interval, pos = number, n = number, set-var = Set-var
(define-struct Dom-interval (interval pos n set-var))
;; arity = Arity, set-var = Set-var
(define-struct Rng-arity (arity set-var))
;; interval = Interval, n = number, set-var = Set-var
(define-struct Rng-interval (interval n set-var))
;; name = symbol
(define-struct Label (name))
;; set-var = Set-var
(define-struct Car (set-var))
;; set-var = Set-var
(define-struct Cdr (set-var))
;; name = symbol, fields = (listof Set-var)
(define-struct Struct (name fields))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arity information

;; lo = number, hi = number | symbol (omega)
(define-struct Interval (lo hi))
;; req = Interval, proh = (listof Interval)
(define-struct Arity (req proh))

  ) ;; unit/sig
