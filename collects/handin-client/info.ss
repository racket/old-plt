(module info (lib "infotab.ss" "setup")
  ;; Modify these four definitions to customize the tool.  
  ;; Also replace the "icon.png" and "server-cert.pem" files.
  (define name "Course Handin")
  (define collection "handin-client")
  (define server "localhost")
  (define port-no 7979)

  (define tool-icons (list (list "icon.png" collection)))
  (define tools '(("tool.ss")))
  (define tool-names (list name)))
