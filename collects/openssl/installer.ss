(module installer mzscheme
  (define (installer plthome)
    (parameterize ([current-namespace (make-namespace)]
		   [current-directory (collection-path "openssl")]
		   [current-command-line-arguments #()])
      (load (build-path (collection-path "openssl") "mzmake.ss"))))
  (provide installer))