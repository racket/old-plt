#| planet-server.ss -- PLaneT server

Serves up fresh PLaneT files, steaming hot

Follows the protocol listed in the PLaneT client file

|#
(module planet-server mzscheme
  
  (require 
   (lib "match.ss")
   "private/planet-shared.ss"
   "server-config.ss")
  
  (provide start)
  
  (define VERSION-STRING "PLaneT/1.0")

  ; start : -> [diverges]
  ; listens for PLaneT requests and handles them
  (define (start)
    (let ((listener (tcp-listen (PLANET-SERVER-PORT) 4 #t)))
      (repeat-forever 
       (lambda () 
         (let-values ([(ip op) (tcp-accept listener)])
           (thread (handle ip op)))))))
  
  ; handle : input-port output-port -> -> void
  ; handles protocol actions coming in from the given input port
  (define (handle ip op)
    
    (define (close-ports)
      (close-input-port ip)
      (close-output-port op))
    
    ; transmit-file : Nat string[filename] -> void
    ; transmits the file named by the given string over op. The given number is the
    ; transaction's sequence number.
    (define (transmit-file seqno maj min file)
      (let ([bytes (file-size file)]
            [file-port (open-input-file file)])
        (write-line (list seqno 'get 'ok maj min bytes) op)
        (copy-n-chars bytes file-port op)))
    
    ; transmit-failure : Nat FULL-PKG ERROR-CODE string -> void
    ; reports a failure to handle a get request
    (define (transmit-failure seqno thepkg error-code msg)
      (write-line (list seqno 'get 'error error-code msg) op))    
    
    (define (state:initialize)
      (let ((version (read-line ip)))
        (cond
          [(string=? version VERSION-STRING)
           (write-line 'ok op)
           (state:get-requests)]
          [else
           (write-line `(invalid ,(format "This server uses only the ~a protocol" VERSION-STRING)))
           (close-ports)])))
    
    (define (nat-or-false? n) (or (not n) (nat? n)))
    (define (legal-language? lstr)
      (and (not (string=? lstr ".."))
           (not (string=? lstr "."))
           (member lstr (directory-list (PLANET-SERVER-REPOSITORY)))))

    (define (state:get-requests)
      (let ((request (read ip)))
        (match request
          [((? nat? seqno) 
            'get 
            (? string? language-version)
            (? string? name)
            (? nat-or-false? maj) 
            (? nat-or-false? min-lo)
            (? nat-or-false? min-hi) 
            (? string? path) ...)
           (if (legal-language? language-version)
               (match-let* ([thepkg (make-pkg-spec name maj min-lo min-hi path #f)]
                            [repository (build-path (PLANET-SERVER-REPOSITORY) language-version)]
                            [cache-pkg (lookup-package thepkg repository)])
                 #;(printf "Looking for ~v in ~v, found ~v\n"
                         (struct->vector thepkg) 
                         repository
                         cache-pkg)
                 (if cache-pkg
                     (let* ([path (pkg-path cache-pkg)]
                            [maj (pkg-maj cache-pkg)]
                            [min (pkg-min cache-pkg)]
                            [file (build-path path (pkg-spec-name thepkg))])
                       (if (file-exists? file)
                           (transmit-file seqno maj min file)
                           (transmit-failure seqno thepkg 'not-found "Internal error: inconsistent server state")))
                     (transmit-failure seqno thepkg 'not-found "No package matched the specified criteria"))
                 (state:get-requests))
               (write-line (list seqno
                                 'error
                                 'bad-language 
                                 (format "Unknown package language: ~s" language-version))
                           op))]
          [((? nat? seqno) _ ...)
           (write-line (list seqno 'error 'malformed-request (format "Unknown request: ~s" request)) op)
           (state:get-requests)]
          ['end 
            (close-ports) ; this is okay only because we're not multithreading the request-handling
            ]
          [_ 
           (write-line (list 'error 'malformed-input (format "Not a request: ~a" request)) op)
           (close-ports)])))

    state:initialize))