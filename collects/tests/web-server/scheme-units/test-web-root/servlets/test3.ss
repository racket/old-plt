;; Non-incremental servlet with no arguments on the URL, in text/plain, no URL
;; path.
(module test3 mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 1)
  (define interface-version 'v1)

  (define (start req)
    (send/finish
      '("text/plain"
        "blah blah plain text")))
  )
