(unit/sig (break)
  (import [e : zodiac:interface^])
  
  (define (break)
    (e:internal-error #f "break called in drscheme-jr")))
