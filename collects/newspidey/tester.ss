(require-library "functio.ss")
(require-library "invoke.ss" "newspidey")

;; TESTS

;; (: parse-i (nothing -> void))
;; parse expression interactively
(define (parse-i)
  (newspidey:newspidey-driver zodiac:read))

;;; (: parse-f (string -> (listof Ast)))
;(define (parse-f filename)
;  (typeparser:parse-types (read-file zodiac:read zodiac:eof? filename)))
;
;(require-library "compat.ss")
;
;;; (: parse-t (nothing -> (listof (list string boolean))))
;(define (parse-t)
;  (map (lambda (f-in f-out)
;         (printf "~n~s~n" f-in)
;         ;; there is a zodiac:parsed->raw, so read-file could just take
;         ;; the port name and use zodiac:read...
;         (let ([out (read-file (lambda (port) (lambda () (read port)))
;                               eof-object? f-out)])
;           (list f-in
;                 (if (and (symbol? (car out)) (eq? 'error (car out)))
;                     (testerr (cadr out) (lambda () (parse-f f-in)))
;                     (equal? (eval (car out)) (parse-f f-in))))))
;       (map (lambda (fn)
;              (string-append "tests-in/" fn))
;            (sort string<? (filter string->number (directory-list "tests-in"))))
;       (map (lambda (fn)
;              (string-append "tests-out/" fn))
;            (sort string<? (filter string->number (directory-list "tests-out"))))))
;
;;; (: read-file ((port -> (nothing -> Sexp)) (everything -> boolean) string -> (listof Sexp)))
;(define (read-file myread myeof? filename)
;  (letrec ([port (open-input-file filename)]
;           ;; (: read-from-thunk ((nothing -> Sexp) -> (listof Sexp)))
;           [read-from-thunk (lambda (f)
;                              (let ([sexp (f)])
;                                (if (myeof? sexp)
;                                    ()
;                                    (cons sexp (read-from-thunk f)))))])
;    (let ([result (read-from-thunk (myread port))])
;      (close-input-port port)
;      result)))
;
;;; (: testerr (string (nothing -> everything) -> boolean))
;(define (testerr expected-error-message thunk)
;  (with-handlers ([exn:user? (lambda (x)
;                               (printf "message: ~s~n" (exn-message x))
;                               (equal? (exn-message x)
;                                       expected-error-message))])
;    (begin
;      (thunk)
;      #f))) ;; if no error occurs, return false 
;
;;; (: create-test-file (string number -> nothing))
;(define (create-test-file fn n)
;  (letrec ([port (open-output-file fn 'truncate)]
;           [write-one-line (lambda (sym n)
;                             (let ([new-sym (gensym)])
;                               (if (not (zero? n))
;                                   (begin
;                                     (fprintf port "~a~n"
;                                              (string-append "(define-type "
;                                                             (symbol->string sym)
;                                                             " "
;                                                             (symbol->string new-sym)
;                                                             ")"))
;                                     (write-one-line new-sym (sub1 n)))
;                                   sym)))])
;    (let ([last-sym (write-one-line (gensym) n)])
;      (fprintf port "~a~n"
;               (string-append "(define-type "
;                              (symbol->string last-sym)
;                              " number)"))
;      (close-output-port port))))
;
;;; (: time-it (number -> number))
;(define (time-it n)
;  (let ([fn "test"])
;    (create-test-file fn n)
;    (let ([start1 (current-milliseconds)])
;      (parse-f fn)
;      (let ([end1 (current-milliseconds)]
;            [start2 (current-milliseconds)])
;        (parse-f fn)
;        (let ([end2 (current-milliseconds)]
;              [start3 (current-milliseconds)])
;          (parse-f fn)
;          (let ([end3 (current-milliseconds)])
;            (exact->inexact (/ (+ (- end1 start1) (- end2 start2) (- end3 start3)) 3))))))))
;  
;(define (mesure-perf n inc stop)
;  (if (<= n stop)
;      (begin
;        (printf "~a ~a~n" n (time-it n))
;        (mesure-perf (+ n inc) inc stop))))
