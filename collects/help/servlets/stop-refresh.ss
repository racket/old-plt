(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/refresh-util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (let* ([bindings (request-bindings initial-request)]
	 [tmp-dir (extract-binding/single 'tmp-dir bindings)])
    (delete-directory/r tmp-dir)

    `(HTML 
      (HEAD ,hd-css)	  
      (BODY
	(H2 (B ,(color-with "red" "Refresh of CVS manuals stopped")))
	(P)
	(A ((HREF "/doc/")
	    (TARGET "_top")) "Help Desk home")))))



