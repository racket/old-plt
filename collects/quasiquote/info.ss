(lambda (request failure)
  (let* ([sig "qqs.ss"]
	 [signatures (list sig)])
    (case request
      [(name) "QuasiQuote"]
      [(compile-prefix) '(begin
			   (require-library "urls.ss" "net")
			   (require-library "refer.ss")
			   (require-library "coreu.ss")
			   (require-library "qqu.ss" "quasiquote"))]
      [(compile-omit-files) signatures]
      [(compile-elaboration-zos) signatures]
      [(blurb)
	`("The QuasiQuote collection is an implementation of a "
	   "simple program for culling equity quotes from the network. "
	   "It is primarily intended as an illustration that PLT Scheme "
	   "can be used, quickly and easily, to implement such tasks. "
	   "It is " (strong "not") " intended to be a financial tool. "
	   (strong ()
	     "The author makes no representation of correctness "
	     "of this program.  It is provided here for illustration "
	     "purposes only.  The author bears no liability for the "
	     "results of using this program."))]
      [else (failure)])))

