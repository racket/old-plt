(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "acks.ss" "drscheme"))

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

  `(HTML 
    (TITLE "Acknowledgements")
    (BODY 
     (A ((NAME "acknowledgements") (VALUE "acknowledgements")))
     (H1  "Acknowledgements")
     (P)
     ,(get-general-acks)
     (P)
     ,(get-translating-acks))))
