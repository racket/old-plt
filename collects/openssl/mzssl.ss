(module mzssl mzscheme
  (define (ssl-connect . args)
    (error 'ssl-connect "extension not compiled"))
  (define (ssl-connect/enable-break . args)
    (error 'ssl-connect "extension not compiled"))
  (define ssl-available? #f)
  (provide ssl-connect ssl-connect/enable-break ssl-available?))
