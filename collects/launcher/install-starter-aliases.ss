(define (install-aliases plthome mz-app mr-app)
  (let ([launcher-path (collection-path "launcher")])
    ((load-extension (build-path launcher-path "starter-setup.so"))
     (build-path plthome mz-app)
     (build-path plthome mr-app)
     (build-path launcher-path "gomz")
     (build-path launcher-path "gomr"))))

(install-aliases "barbican:plt:" "mzscheme ppc" "mred ppc")

