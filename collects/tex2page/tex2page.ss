(module tex2page mzscheme
  (require (lib "etc.ss"))
  (provide tex2page)
  (define
   tex2page
   (lambda (f)
     (parameterize ((current-namespace (make-namespace)))
       (namespace-require `(lib "tex2page-aux.ss" "tex2page"))
       ((namespace-variable-value 'tex2page) f)))))
