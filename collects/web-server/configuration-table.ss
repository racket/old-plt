(module
  configuration-table
  "configuration-table-language.ss"
  (provide
    port
    max-waiting
    initial-connection-timeout
    virtual-host-table
    default-host-table)
  (define port 80)
  (define max-waiting 40)
  (define initial-connection-timeout 30)
  (define default-host-table
    (let* ((host-root
             (build-path-maybe web-server-collection "default-web-root"))
           (log-file-path (build-path-maybe host-root "log"))
           (file-root (build-path-maybe host-root "htdocs"))
           (servlet-root (build-path-maybe host-root "."))
           (configuration-root (build-path-maybe host-root "conf"))
           (default-indices (list "index.html" "index.htm"))
           (log-format 'parenthesized-default)
           (password-authentication (build-path-maybe host-root "passwords"))
           (servlet-message
             (build-path-maybe configuration-root "servlet-error.html"))
           (authentication-message
             (build-path-maybe configuration-root "forbidden.html"))
           (servlets-refreshed
             (build-path-maybe configuration-root "servlet-refresh.html"))
           (passwords-refreshed
             (build-path-maybe configuration-root "passwords-refresh.html"))
           (file-not-found-message
             (build-path-maybe configuration-root "not-found.html"))
           (protocol-message
             (build-path-maybe configuration-root "protocol-error.html"))
           (default-servlet-timeout 120)
           (password-connection-timeout 300)
           (servlet-connection-timeout 86400)
           (file-per-byte-connection-timeout 1/20)
           (file-base-connection-timeout 30))
      (make-host-table
        default-indices
        servlet-root
        log-format
        password-authentication
        (make-messages
          servlet-message
          authentication-message
          servlets-refreshed
          passwords-refreshed
          file-not-found-message
          protocol-message)
        (make-timeouts
          default-servlet-timeout
          password-connection-timeout
          servlet-connection-timeout
          file-per-byte-connection-timeout
          file-base-connection-timeout)
        (make-paths host-root log-file-path file-root servlet-root))))
  (define virtual-host-table (list)))
