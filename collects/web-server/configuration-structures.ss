(module configuration-structures mzscheme
  (require "util.ss"
           "configuration-table-structures.ss")
  (provide (struct timeouts (default-servlet password servlet-connection file-per-byte file-base))
           (struct paths (host-base log htdocs servlet)))
    
  ; host = (make-host (listof str) (str str -> (U #f str)) (str str sym url str -> str)
  ;                   passwords resopnders timeouts paths (U oport #f))
  (provide-define-struct host (indices servlet-path format-log-message passwords responders timeouts paths log))
  
  ; passwords = (listof (list* relm:str protected-dir-regexp:str (listof (list user:sym password:str))))
  
  ; responders = (make-responders (url tst -> response)
  ;                               (url tst -> response)
  ;                               (url (cons sym str) -> response)
  ;                               response
  ;                               response
  ;                               (url -> response)
  ;                               response)
  (provide-define-struct responders
    (servlet servlet-loading authentication servlets-refreshed passwords-refreshed file-not-found protocol)))
