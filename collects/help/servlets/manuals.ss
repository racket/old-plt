(module manuals mzscheme
  (require (lib "servlet-helpers.ss" "web-server")
           "../private/manuals.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    (list
     "text/html"	 
     (find-manuals))))