(module link mzscheme 
  (require (lib "web-server-unit.ss" "web-server")
           (lib "sig.ss" "web-server")
           (lib "configuration.ss" "web-server")
           
           (lib "etc.ss")
           (lib "file.ss")
           (lib "unitsig.ss")
           
           (lib "tcp-sig.ss" "net")
           (lib "tcp-redirect.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url-unit.ss" "net")
           
           (lib "browser-sig.ss" "browser")
           (lib "browser-unit.ss" "browser")
           
           (lib "plt-installer-sig.ss" "setup")
           
           (lib "mred-sig.ss" "mred")
           
           "internal-hp.ss"
           
           "tcp-intercept.ss"
           "sig.ss"
           
           "gui.ss"
           "main.ss")
  
  (provide help-desk@)
  
  (define config
    (let* ([build-normal-path
            (lambda args
              (normalize-path
               (apply build-path args)))]
           [help-path (build-normal-path (collection-path "help"))]
           [doc-path (build-normal-path help-path 'up "doc")]
           [file-root (build-normal-path doc-path 'up)]
           [host-root (build-normal-path help-path "web-root")]
           [servlet-root help-path])
      (build-developer-configuration 
       `((port ,internal-port)
         (max-waiting 40)
         (initial-connection-timeout 30)
         (default-host-table
          (host-table
           (default-indices "index.html" "index.htm")
           (log-format parenthesized-default)
           (messages
            (servlet-message "servlet-error.html")
            (authentication-message "forbidden.html")
            (servlets-refreshed "servlet-refresh.html")
            (passwords-refreshed "passwords-refresh.html")
            (file-not-found-message "not-found.html")
            (protocol-message "protocol-error.html"))
           (timeouts
            (default-servlet-timeout 120)
            (password-connection-timeout 300)
            (servlet-connection-timeout 86400)
            (file-per-byte-connection-timeout 1/20)
            (file-base-connection-timeout 30))
           (paths
            (configuration-root "conf")
            (host-root ,host-root)
            (log-file-path "log")
            (file-root ,file-root)
            (servlet-root ,servlet-root)
            (password-authentication "passwords"))))
         (virtual-host-table)))))

  (define help-desk@
    (compound-unit/sig
      (import [plt-installer : setup:plt-installer^]
              [mred : mred^]
              [real-tcp : net:tcp^])
      (link
       [config : web-config^ (config)]
       [real-url : net:url^ (url@ real-tcp)]
       [web-server : web-server^ (web-server@ real-tcp config)]
       
       [ic-tcp : net:tcp^ (tcp-intercept@ web-server)]
       [pre-ic-url : net:url^ (url@ ic-tcp)]
       [ic-url : net:url^ (url-intercept@ pre-ic-url)]
       [browser : browser^ (browser@ plt-installer mred ic-tcp ic-url)]
       [gui : gui^ (gui@ browser ic-url)]

       [m : () (main@ gui)])
      (export (open gui)))))