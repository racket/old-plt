(module fake-break mzscheme
  (provide fake-break@)

  (define fake-break@
    (unit/sig (break)
      (import)
      
      (define (break)
	(error 'break "break not implemented in drscheme-jr")))))

