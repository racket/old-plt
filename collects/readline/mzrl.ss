(module mzrl mzscheme

(require (lib "ffi.ss"))

(define libtermcap  (ffi-lib "libtermcap.so")) ; needed
(define libreadline (ffi-lib "libreadline.so"))

(define* readline
  (get-ffi-obj #"readline" libreadline (_fun _string -> _string/eof)))

(define* add-history
  (get-ffi-obj #"add_history" libreadline (_fun _string -> _void)))

(define (completion-function func)
  (let ([cur '()])
    (define (complete str state)
      (if (zero? state)
        (begin (set! cur (func str)) (complete str 1))
        (and (pair? cur)
             (begin0 (malloc (add1 (string-length (car cur)))
                             (car cur) 'eternal)
               (set! cur (cdr cur))))))
    complete))

(define* (set-completion-function! func)
  (if func
    (set-ffi-obj! #"rl_completion_entry_function" libreadline
                  (_fun _string _int -> _pointer)
                  (completion-function func))
    (set-ffi-obj! #"rl_completion_entry_function" libreadline _pointer #f)))

(set-ffi-obj! #"rl_readline_name" libreadline _string "mzscheme")

;; make it possible to run Scheme threads while waiting for input
(set-ffi-obj! #"rl_event_hook" libreadline (_fun -> _int)
              (lambda () (sync/enable-break (current-input-port)) 0))

)
