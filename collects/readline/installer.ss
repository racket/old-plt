
(module installer mzscheme
  (define (installer plthome)
    (parameterize ([current-namespace (make-namespace)]
		   [current-directory (collection-path "readline")]
		   [current-command-line-arguments #()])
      (load (build-path (collection-path "readline") "mzmake.ss"))))
  (provide installer))
