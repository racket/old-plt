(let ()
  (define beginner-program
    (format "~
;; f2c : number -> number~
~n;; converts a temperature in fahrenheit to one in celsius~
~n(define (f2c c)~
~n  (* (- c 32) 5/9))~
~n~
~n;; Examples~
~n(f2c 212) = 100~
~n(f2c 32) = 0"))

  (define intermediate-program
    (format "~
;; max : list-of-natural-numbers -> number~
~n;; finds the largest number in the list, or~
~n;; zero if there are no numbers in the list.~
~n(define (max l)~
~n  (cond~
~n    [(empty? l) 0]~
~n    [else (local [(define max-rest (max (rest l)))]~
~n            (cond~
~n              [(<= max-rest (first l)) (first l)]~
~n              [else max-rest]))]))~
~n~
~n;; Examples~
~n(max empty) = 0~
~n(max (list 1 2 3)) = 3~
~n(max (list 3 2 1)) = 3"))

  (define advanced-program
    (format "~
;; counter : -> number~
~n;; this function counts the number of times it has been called~
~n(define counter~
~n  (local [(define calls 0)]~
~n    (lambda ()~
~n      (set! calls (+ calls 1))~
~n      calls)))~
~n~
~n;; Examples~
~n(counter) = 1~
~n(counter) = 2"))

  (define mzscheme-program
    (format "~
(require-library \"make.ss\" \"make\")~
~n(require-library \"slatex.ss\" \"slatex\")~
~n(make ((\"paper.dvi\" (\"paper.tex\") (slatex \"paper.tex\")))~
~n  argv)"))

  (define mred-program
    (format "~
(define f (make-object frame% \"Frame\"))~
~n(define b (make-object button%~
~n            \"Close\"~
~n            frame~
~n            (lambda x (send frame show #f))))~
~n(send f show #t)"))

(list (cons (list (string-constant how-to-design-programs)
                  (string-constant beginner-language))
            beginning-program)
      (cons (list (string-constant how-to-design-programs)
                  (string-constant beginner-language/abbrev))
            beginning-program)
      (cons (list (string-constant how-to-design-programs)
                  (string-constant intermediate-language))
            intermediate-program)
      (cons (list (string-constant how-to-design-programs)
                  (string-constant advanced-language))
            advanced-language)
      (cons (list (string-constant full-languages)
                  (string-constant mzscheme-language))
            mzscheme-program)
      (cons (list (string-constant full-languages)
                  (string-constant mzscheme-language/debug))
            mzscheme-program)
      (cons (list (string-constant full-languages)
                  (string-constant mred-language/debug))
            mred-program)
      (cons (list (string-constant full-languages)
                  (string-constant mred-language/debug))
            mred-program)))
