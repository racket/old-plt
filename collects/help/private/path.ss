(module path mzscheme
  (provide servlet-path?)

  ; forward-slashed-path-string -> (union #f list)
  (define (servlet-path? path)
    (regexp-match "^/servlets/" path)))
