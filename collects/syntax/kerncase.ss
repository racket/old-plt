
(module kerncase mzscheme

  (define-syntax kernel-syntax-case
    (lambda (stx)
      (syntax-case stx ()
	[(_ stx trans? clause ...)
	 (syntax (syntax-case* stx (quote 
				    quote-syntax #%datum #%top
				    lambda case-lambda
				    let-values letrec-values
				    begin begin0 set!
				    with-continuation-mark
				    if #%app
				    define-values define-syntax
				    module require provide require-for-syntax)
			       (if trans? module-transformer-identifier=? module-identifier=?)
			  clause ...))])))

  (define (kernel-form-identifier-list stx)
    (map (lambda (s)
	   (datum->syntax-object stx s #f))
	 '(begin
	    define-values
	    define-syntax
	    set!
	    let
	    let-values
	    let*
	    let*-values
	    letrec
	    letrec-values
	    lambda
	    case-lambda
	    if
	    quote
	    letrec-syntax
	    with-continuation-mark
	    #%app
	    #%top
	    #%datum)))
  
  (provide kernel-syntax-case
	  kernel-form-identifier-list))
