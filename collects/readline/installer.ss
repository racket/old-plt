
(module installer mzscheme
  (define (installer plthome)
    (parameterize ([current-namespace (make-namespace)]
		   [current-directory (collection-path "readline")])
      (namespace-variable-binding 'argv #())
      (load (build-path (collection-path "readline") "mzmake.ss"))))
  (provide installer))
