(require (lib "parse-table.ss" "web-server")
         (lib "configuration.ss" "web-server")
         (lib "sig.ss" "web-server")
         (lib "configuration-structures.ss" "web-server")
         (lib "dispatcher.ss" "web-server")
         (lib "web-server.ss" "web-server")
         )

(define (make-tab paths-exp)
  `((port 9000)
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

;; table-err->string: configuration-table -> string
;; trap an error when parsing a configuration and return the error string
(define (table-err->string tab)
  (with-handlers ([exn? (lambda (the-exn)
                          (exn-message the-exn))])
    (complete-configuration (current-directory) (parse-configuration-table tab))))

(string=? "duplicate path:  \"/\""
          (table-err->string
           (make-tab '(paths
                       (configuration-root "conf")
                       (host-root "my-web-root")
                       (log-file-path "log")
                       (file-root "htdocs")
                       (file-root "htdocs2")
                       (servlet-root ".")
                       (password-authentication "passwords")))))

(string=? "duplicate path:  \"/servlets\""
          (table-err->string
           (make-tab '(paths
                       (configuration-root "conf")
                       (host-root "my-web-root")
                       (log-file-path "log")
                       (file-root "htdocs")
                       (servlet-root ".")
                       (servlet-root "/bing")
                       (password-authentication "passwords")))))

(string=? "duplicate path:  \"/foo\""
          (table-err->string
           (make-tab '(paths
                       (configuration-root "conf")
                       (host-root "my-web-root")
                       (log-file-path "log")
                       (file-root "htdocs")
                       (file-root "/foo" "bar")
                       (file-root "/foo" "baz")
                       (servlet-root ".")
                       (password-authentication "passwords")))))

(string=? "duplicate path:  \"/foo\""
          (table-err->string
           (make-tab '(paths
                       (configuration-root "conf")
                       (host-root "my-web-root")
                       (log-file-path "log")
                       (file-root "htdocs")
                       (file-root "/foo" "bar")
                       (servlet-root ".")
                       (servlet-root "/foo" "baz")
                       (password-authentication "passwords")))))

(string=? "duplicate path:  \"/foo\""
          (table-err->string
           (make-tab '(paths
                       (configuration-root "conf")
                       (host-root "my-web-root")
                       (log-file-path "log")
                       (file-root "htdocs")
                       (servlet-root "/foo" "bar")
                       (servlet-root "/foo" "baz")
                       (password-authentication "passwords")))))

(define tab01
  (make-tab '(paths
              (configuration-root "conf")
              (host-root "my-web-root")
              (log-file-path "log")
              (file-root "htdoodle")
              (servlet-root ".")
              (servlet-root "/serv1/" "servlets1")
              (servlet-root "/serv2/" "servlets2")
              (servlet-root "/serv3/" "servlets3/urlmap.ss")
              (file-root "/stat1/" "static1")
              (file-root "/stat2/" "static2")
              
              (file-root "/fiddle/sticks1/" "bing/bang")
              (file-root "/fiddle/sticks2/" "/bing/bang")
              (file-root "/fiddle/sticks3/" "./bing/bang")
              (file-root "/fiddle/sticks4/" "../bing/bang")
              
              (file-root "/a/b/c/" "1")
              (file-root "/a/" "2")
              (file-root "/a/b/" "3")
              (file-root "/a/b/d/" "4")
              (password-authentication "passwords"))))


(define conf
  (complete-configuration (current-directory) (parse-configuration-table tab01)))

(define-values/invoke-unit/sig web-config^ conf)

(define d-host (virtual-hosts 'ignored))

(define disp (host-dispatcher d-host))

(define (test-base str . strs)
  (string=? (resource-base (disp str))
            (apply build-path (cons (current-directory) strs))))

(not (disp "foo/bar"))
(not (disp "/serv3/urlmap.ss"))
(dynamic-resource? (disp "/serv3/foo/bar/baz.ss"))
(dynamic-resource? (disp "/serv3/foo.scm"))
(static-resource? (disp "/serv3/foo.html"))

(test-base "/serv1/foo" "my-web-root" "servlets1")
(test-base "/serv2/bar" "my-web-root" "servlets2")
(test-base "/stat1/" "my-web-root" "static1")
(test-base "/stat2/" "my-web-root" "static2")

(test-base "/fiddle/sticks1/foo/bar/baz" "my-web-root" "bing" "bang")
(string=? (resource-base (disp "/fiddle/sticks2/pop/corn")) "/bing/bang")
(test-base "/fiddle/sticks3/big/bad/momma" "my-web-root" "bing" "bang")
(test-base "/fiddle/sticks4/happy/go/lucky" "bing" "bang")

(test-base "/a/b/c/this/is/a/test" "my-web-root" "1")
(test-base "/a/this/is/fun" "my-web-root" "2")
(test-base "/a/b/lets/ride" "my-web-root" "3")
(test-base "/a/b/d/and/some/more" "my-web-root" "4")




