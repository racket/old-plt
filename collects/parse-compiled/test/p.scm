
(require (lib "pretty.ss") (lib "etc.ss"))
(require "../parse-compiled.ss")

(define (p sxp)
  (pretty-print (parse-compiled (compile sxp))))

