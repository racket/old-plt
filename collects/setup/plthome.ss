(module plthome mzscheme
  (provide plthome)
  (define plthome
    (or (getenv "PLTHOME")
        (with-handlers ([void (lambda (e) #f)])
          ;; use `split-path' to strip off the trailing "/"
          (let-values ([(base name dir?)
                        (split-path
                         (simplify-path
                          (build-path (collection-path "mzlib") 'up 'up)))])
            (build-path (if (eq? 'relative base) (current-directory) base)
                        name))))))
