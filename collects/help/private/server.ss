(module server mzscheme 

  (require "server-config.ss"
           (lib "internal-server.ss" "web-server")
           (lib "configuration.ss" "web-server")
           (lib "etc.ss")
           "browser-extensions.ss")
  
  (provide start-help-server)
  
  (define start-help-server
    (opt-lambda ([addl-browser-frame-mixin (lambda (x) x)])
      (let* ([configuration (build-developer-configuration (build-config-exp))]
             [hd-cookie (make-hd-cookie min-port #f #f #f addl-browser-frame-mixin)]
             [combined-browser-mixin
              (compose addl-browser-frame-mixin
                       (make-help-desk-frame-mixin hd-cookie))])
        (let-values ([(exit-proc browser-maker internal-url-test)
                      (serve configuration min-port #f combined-browser-mixin)])
          (set-hd-cookie-exit-proc! hd-cookie exit-proc)
          (set-hd-cookie-browser! hd-cookie browser-maker)
          (set-hd-cookie-internal-url-test! hd-cookie internal-url-test)
          hd-cookie)))))