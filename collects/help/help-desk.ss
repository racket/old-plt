(module help-desk mzscheme
  (require "private/standard-urls.ss"
           "private/docpos.ss"
	   "private/search.ss"
	   "private/manuals.ss"
	   "private/installed-components.ss"
           "private/server.ss"
           "private/buginfo.ss"
           "private/cookie.ss"
	   "bug-report.ss" ;; this is require'd here to get the prefs defaults setup done early.
           (lib "contract.ss")
           (lib "mred.ss" "mred"))

  
  (provide/contract 
   (start-help-server (mixin-contract . -> . hd-cookie?))
   (hd-cookie? (any? . -> . boolean?))
   (hd-cookie-shutdown-server (hd-cookie? . -> . (-> any)))
   (hd-cookie-find-browser (hd-cookie? . -> . (-> (union false? (is-a?/c frame%)))))
   (hd-cookie->browser (hd-cookie? . -> . (is-a?/c frame%)))
   (hd-cookie-port (hd-cookie? . -> . number?))
   (visit-url-in-browser (hd-cookie? string? . -> . void?))
   (visit-url-in-new-browser (hd-cookie? string? . -> . void?)))
  
  (provide 
   
   ;; manual ordering
   standard-html-doc-position
   user-defined-doc-position
   set-doc-position!
   reset-doc-positions!
   known-docs

   doc-collections-changed
   
   ;; manuals
   find-manuals
   find-doc-directories
   find-doc-directory
   find-doc-names
   
   ;; manual search
   finddoc
   findreldoc
   finddoc-page
   finddoc-page-anchor
   
   ;; supplemental
   set-bug-report-info!
   help-desk:installed-components

   search-for-docs
   goto-manual-link
   goto-hd-location
   
   make-home-page-url
   ))
