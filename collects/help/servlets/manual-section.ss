(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "help-desk-mz.ss" "help"))

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
	 [page-anchor (finddoc-page-anchor manual section)]
	 [url (if (hd-servlet? page-anchor)
		  page-anchor
		  (format "/doc/~a/~a" 
			  manual
			  page-anchor))])
    (send/finish
     (redirect-to url))))
