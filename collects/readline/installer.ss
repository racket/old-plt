
(module installer mzscheme
  (define (installer plthome)
    (parameterize ([current-namespace (make-namespace)]
		   [current-directory (collection-path "readline")])
      (load (build-path (collection-path "readline") "mzmake.ss"))))
  (provide installer))
