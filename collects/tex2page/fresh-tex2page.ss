(module fresh-tex2page mzscheme
  (provide tex2page/fresh-namespace)

  (define (tex2page/fresh-namespace file)
    (parameterize ([current-namespace (make-namespace)])
      (namespace-require '(lib "tex2page.ss" "tex2page"))
      ((namespace-variable-value 'tex2page) file))))
