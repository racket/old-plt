(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "help-desk.ss" "help"))

(require "private/hd-css.ss")
(require "private/util.ss")

(unit/sig ()
  (import servlet^)

  (let* ([bindings (request-bindings initial-request)]
	 [manual (extract-binding/single 'manual bindings)]
	 [raw-section (extract-binding/single 'section bindings)]
         ; remove quotes
	 [section (substring raw-section 
			     1 (sub1 (string-length raw-section)))]
	 [url (format "/doc/~a/~a" 
		      manual
		      (finddoc-page-anchor manual section))])
    `(HTML
      (HEAD ,hd-css
	    (META ((HTTP-EQUIV "refresh")
		   (CONTENT ,(format "0;URL=~a" url)))))

      (BODY
       "Click "
       (A ((HREF ,url)) "here") 
       " if this page does not refresh"))))


