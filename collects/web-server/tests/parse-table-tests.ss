(require (lib "parse-table.ss" "web-server")
         (lib "configuration-table-structs.ss" "web-server"))


 (define (make-tab paths-exp)
    `((port 80)
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
        ,paths-exp))
        (virtual-host-table)))
 
 (define (err->string tab)
   (with-handlers ([exn? (lambda (an-exn)
                           (exn-message an-exn))])
     (parse-configuration-table tab)))
 
  (define tab01
    (make-tab '(paths
                (configuration-root "conf")
                (host-root "default-web-root")
                (log-file-path "log")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords"))))
  
  (define tab02
    (make-tab '(paths
                (configuration-root "conf")
                (configuration-root "dupe")
                (host-root "default-web-root")
                (log-file-path "log")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords"))))
  
  (define tab03
    (make-tab '(paths
                (configuration-root "conf")
                (host-root "default-web-root")
                (host-root "dupe")
                (log-file-path "log")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords"))))
  (define tab04
    (make-tab '(paths
                (configuration-root "conf")
                (host-root "default-web-root")
                (log-file-path "log")
                (log-file-path "dupe")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords"))))
  
  (define tab05
    (make-tab '(paths
                (configuration-root "conf")
                (host-root "default-web-root")
                (log-file-path "log")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords")
                (password-authentication "dupe"))))
  
  (configuration-table? (parse-configuration-table tab01))
  
 
  
  (equal? (map err->string (list tab02 tab03 tab04 tab05))
          '("parse-path: duplicate configuration-root"
            "parse-path: duplicate host-root"
            "parse-path: duplicate log-file-path"
            "parse-path: duplicate password-authentication"))
  
  (define tab06
    (make-tab '(paths
                (host-root "default-web-root")
                (log-file-path "log")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords"))))
  
  (define tab07
    (make-tab '(paths
                (configuration-root "conf")
                (log-file-path "log")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords"))))
  
  (define tab08
    (make-tab '(paths
                (configuration-root "conf")
                (host-root "default-web-root")
                (file-root "htdocs")
                (servlet-root ".")
                (password-authentication "passwords"))))
  
  (define tab09
    (make-tab '(paths
                (configuration-root "conf")
                (host-root "default-web-root")
                (log-file-path "log")
                (servlet-root ".")
                (password-authentication "passwords"))))
  
 
    (define tab10
      (make-tab '(paths
                  (configuration-root "conf")
                  (host-root "default-web-root")
                  (log-file-path "log")
                  (file-root "htdocs")
                  (password-authentication "passwords"))))
    
    (define tab11
      (make-tab '(paths
                  (configuration-root "conf")
                  (host-root "default-web-root")
                  (log-file-path "log")
                  (file-root "htdocs")
                  (servlet-root "."))))
    
    (equal?
     (map err->string (list tab06 tab07 tab08 tab09 tab10 tab11))
     '("parse-paths: malformed host, missing configuration-root"
       "parse-paths: malformed host, missing host-root"
       "parse-paths: malformed host, missing log-file-path"
       "parse-paths: malformed host, missing file-root"
       "parse-paths: malformed host, missing servlet-root"
       "parse-paths: malformed host, missing password-authentication"))
                      
    (define tab12
      (make-tab '(paths
                  (configuration-root "conf")
                  (host-root "default-web-root")
                  (log-file-path "log")
                  (file-root "htdocs1")
                  (file-root "htdocs2")
                  (servlet-root "servlets")
                  (password-authentication "passwords"))))
    
    
    (define tab13
      (make-tab '(paths
                  (configuration-root "conf")
                  (host-root "default-web-root")
                  (log-file-path "log")
                  (file-root "htdocs")
                  (servlet-root "servlets1")
                  (servlet-root "servlets2")
                  (password-authentication "passwords"))))
    
    (define (ctab->htdocs x)
      (paths-htdocs (host-table-paths (configuration-table-default-host (parse-configuration-table x)))))

    (define (ctab->servlet x)
      (paths-servlet (host-table-paths (configuration-table-default-host (parse-configuration-table x)))))
 
    (equal?
     (ctab->htdocs tab12)     
     '(("/" . "htdocs2") ("/" . "htdocs1")))
    
    (equal?
     (ctab->servlet tab13)
     '(("/servlets" . "servlets2") ("/servlets" . "servlets1")))

     (define tab14
      (make-tab '(paths
                  (configuration-root "conf")
                  (host-root "default-web-root")
                  (log-file-path "log")
                  (file-root "foo/bar/boo" "htdocs")
                  (servlet-root "servlets")
                  (password-authentication "passwords"))))
    
    
    (define tab15
      (make-tab '(paths
                  (configuration-root "conf")
                  (host-root "default-web-root")
                  (log-file-path "log")
                  (file-root "htdocs")
                  (servlet-root "foo/bar/boo" "servlets")
                  (password-authentication "passwords"))))
    
    (equal?
     (ctab->htdocs tab14)
     '(("foo/bar/boo" . "htdocs")))
    
    (equal?
     (ctab->servlet tab15)
     '(("foo/bar/boo" . "servlets")))