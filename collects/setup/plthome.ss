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
  ;; equal? to it.  The only processing that is performed is replacing all
  ;; backslashes with slashes on Windows.  It is generally fine if this still
  ;; misses some usages, as long as it works when we prepare a distribution
  ;; tree using a proper PLTHOME env variable.  Otherwise, things will continue
  ;; to work fine and .dep files will just contain absolute path names.  These
  ;; functions work on dep elements -- either a pathname or a pair with a
  ;; pathname in its cdr, the plthome-ified pathname will itself be a pair.
  (define simplify-path*
    (if (eq? 'windows (system-type))
      (lambda (str) (regexp-replace* #rx"\\\\" (simplify-path str) "/"))
      simplify-path))
  (define plthome/
    (and plthome
         (regexp-replace #rx"/?$" (simplify-path* plthome) "/")))
  (define plthome/-len
    (and plthome/ (string-length plthome/)))
  (define (maybe-cdr-op fname f)
    (lambda (x)
      (cond [(not plthome/) (error fname "no PLTHOME and no mzlib found")]
            [(and (pair? x) (not (eq? 'plthome (car x))))
             (cons (car x) (f (cdr x)))]
            [else (f x)])))
  (define plthome-ify
    (maybe-cdr-op 'plthome-ify
     (lambda (path)
       (let ([path* (and (string? path) (simplify-path* path))])
         (cond [(and path*
                     (> (string-length path*) plthome/-len)
                     (equal? (substring path* 0 plthome/-len) plthome/))
                (cons 'plthome (substring path* plthome/-len))]
               [(equal? path* plthome) (cons 'plthome "")]
               [else path])))))
  (define un-plthome-ify
    (maybe-cdr-op 'un-plthome-ify
     (lambda (path)
       (if (and (pair? path) (eq? 'plthome (car path)))
         (if (equal? (cdr path) "") plthome (build-path plthome (cdr path)))
         path)))))
