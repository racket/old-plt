(module input-parse "restricted-mzscheme.ss"

  (provide peek-next-char
	   assert-curr-char
	   skip-until skip-while
	   next-token next-token-of
	   read-text-line
	   read-string
	   parser-error
	   exn:ssax?
	   exn:ssax-port exn:ssax-stuff)

  (require (rename (lib "13.ss" "srfi")
		   string-concatenate-reverse string-concatenate-reverse))

  (require "define-opt.ss")
  (require "ascii.ss")
  (require "char-encodings.ss")
  (require "crementing.ss")

  (define-struct (exn:ssax exn) (port stuff))

  (define (parser-error port message . rest)
    (raise (make-exn:ssax message
			  (current-continuation-marks)
			  port
			  rest)))

  (require (lib "include.ss"))
  (include "input-parse.scm"))