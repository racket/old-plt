(module plthome mzscheme
  (provide plthome plthome-ify un-plthome-ify)

  (define plthome
    (or (getenv "PLTHOME")
        (with-handlers ([void (lambda (e) #f)])
          ;; use `split-path' to strip off the trailing "/"
          (let-values ([(base name dir?)
                        (split-path
                         (simplify-path
                          (build-path (collection-path "mzlib") 'up 'up)))])
            (build-path (if (eq? 'relative base) (current-directory) base)
                        name)))))

  ;; The following two functions are used to store paths that are relative to
  ;; plthome as such in dep files.  This means that if the plt tree is moved
  ;; .dep files still work.  `plthome-ify' uses `plthome' with a hard-wired "/"
  ;; suffix, so it will not work properly if there is a different separator or
  ;; if the input path uses a directory that is equivalent to plthome but not
  ;; equal? to it -- that's fine as long as it works when we prepare a
  ;; distribution tree using a proper PLTHOME env variable.  Otherwise, things
  ;; will continue to work fine and .dep files will just contain absolute path
  ;; names.  They work on dep elements -- either a pathname or a pair with a
  ;; pathname in its cdr, the plthome-ified pathname will itself be a pair.
  (define plthome/ (regexp-replace "/?$" plthome "/"))
  (define plthome/-len (string-length plthome/))
  (define (maybe-cdr-op f)
    (lambda (x) (if (pair? x) (cons (car x) (f (cdr x))) (f x))))
  (define plthome-ify
    (maybe-cdr-op (lambda (path)
                    (if (and (string? path)
                             (> (string-length path) plthome/-len)
                             (equal? (substring path 0 plthome/-len) plthome/))
                      (cons 'plthome (substring path plthome/-len))
                      path))))
  (define un-plthome-ify
    (maybe-cdr-op (lambda (path)
                    (if (and (pair? path) (eq? 'plthome (car path)))
                      (build-path plthome (cdr path))
                      path)))))
