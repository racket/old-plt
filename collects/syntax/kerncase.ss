
(module kerncase mzscheme

  (define-syntax kernel-syntax-case
    (lambda (stx)
      (syntax-case stx ()
	[(_ stx trans? clause ...)
	 (syntax (syntax-case* stx (quote 
				    quote-syntax #%datum #%unbound
				    lambda case-lambda
				    let-values letrec-values
				    begin begin0 set! struct
				    with-continuation-mark
				    if #%app
				    define-values define-syntax
				    module import export import-for-syntax export-indirect)
			       (if trans? module-transformer-identifier=? module-identifier=?)
			  clause ...))])))

  (define (kernel-form-identifier-list stx)
    (map (lambda (s)
	   (datum->syntax s #f stx))
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
	    struct
	    quote
	    letrec-syntax
	    with-continuation-mark
	    #%app
	    #%unbound
	    #%datum)))
  
  (export kernel-syntax-case
	  kernel-form-identifier-list))
