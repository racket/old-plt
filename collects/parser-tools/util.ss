#cs
(module util mzscheme
  
  (require "lex.ss"
           (lib "readerr.ss" "syntax"))
 
  (define-lex-abbrevs (letter (: (- a z) (- A Z)))
                      (digit (- #\0 #\9))
                      (whitespace (: #\newline #\return #\tab #\space #\vtab))
                      (any (- #\000 #\377)))
  (provide letter digit whitespace any)
  
  (define-struct OPEN-LIST ())
  (define-struct OPEN-VECTOR ())
  (define-struct CLOSE ())
  (define-struct QUOTE ())
  (define-struct QUASI-QUOTE ())
  (define-struct UNQUOTE ())
  (define-struct UNQUOTE-SPLICING ())
  (define-struct DOT ())
  
  (provide make-OPEN-LIST
           make-OPEN-VECTOR
           make-CLOSE
           make-QUOTE
           make-QUASI-QUOTE
           make-UNQUOTE
           make-UNQUOTE-SPLICING
           make-DOT)
  
  (define (improp-reverse list last)
    (cond
      ((null? list) last)
      (else
       (improp-reverse (cdr list) (cons (car list) last)))))
  
  (define (raise-read-err err ip)
    (let-values (((a b c) (port-next-location ip)))
      (raise-read-error err #f a b c 0)))

  (define (build-reader lexer)
    (lambda (input-port)
      (let ((lex-buf (make-lex-buf input-port)))
        (letrec ((read-one
                  (lambda (in)
                    (cond
                      ((OPEN-LIST? in)
                       (read-list null))
                      ((OPEN-VECTOR? in)
                       (list->vector (read-list null)))
                      ((CLOSE? in)
                       (raise-read-err "unexpected )" input-port))
                      ((QUOTE? in)
                       (list 'quote (read-one (lexer lex-buf))))
                      ((QUASI-QUOTE? in)
                       (list 'quasiquote (read-one (lexer lex-buf))))
                      ((UNQUOTE? in)
                       (list 'unquote (read-one (lexer lex-buf))))
		      ((UNQUOTE-SPLICING? in)
		       (list 'unquote-splicing (read-one (lexer lex-buf))))
                      ((DOT? in)
                       (raise-read-err "unexpected ." input-port))
                      (else
                       in))))
                 (read-list
                  (lambda (acc)
                    (let ((in (lexer lex-buf)))
                      (cond
                        ((CLOSE? in)
                         (reverse acc))
                        ((and (DOT? in) (null? acc))
                         (raise-read-err "unexpected ." input-port))
                        ((DOT? in)
                         (let ((next (read-one (lexer lex-buf))))
                           (let ((close (lexer lex-buf)))
                             (cond
                               ((CLOSE? close)
                                (improp-reverse acc next))
                               (else
                                (raise-read-err "expected )" input-port))))))
                        ((eof-object? in)
                         (raise-read-err "unexpected eof" input-port))
                        (else
                         (read-list (cons (read-one in) acc))))))))
          (read-one (lexer lex-buf))))))
  (provide build-reader)
  )