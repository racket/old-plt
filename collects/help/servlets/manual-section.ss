(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "help-desk-mz.ss" "help"))

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

  (let* ([bindings (request-bindings initial-request)]
	 [manual (extract-binding/single 'manual bindings)]
	 [raw-section (extract-binding/single 'section bindings)]
         ; remove quotes
	 [section (substring raw-section 
			     1 (sub1 (string-length raw-section)))]
	 [page (finddoc-page-anchor manual section)])
    (send/finish
     (redirect-to page))))
