(module help-desk mzscheme
  (require "private/browser.ss"
  	   "private/external-search.ss"
           "private/server-config.ss"
           "private/docpos.ss"
	   "private/finddoc.ss"
	   "private/search.ss"
	   "private/manuals.ss"
	   "private/installed-components.ss"
           "private/server.ss"
           "private/buginfo.ss"
           (lib "contract.ss"))

  
  (provide/contract (start-help-server (mixin-contract . -> . hd-cookie?)))
  
  (provide 
   ;; server functions
   hd-cookie->port
   hd-cookie->exit-proc
   hd-cookie?
   ;; manual ordering
   standard-html-doc-position
   user-defined-doc-position
   set-doc-position!
   reset-doc-positions!
   known-docs
   ;; search functions
   do-search
   doc-collections-changed
   ;; manuals
   find-manuals
   ;; manual search
   finddoc
   findreldoc
   finddoc-page
   finddoc-page-anchor
   ;; supplemental
   set-bug-report-info!
   help-desk:installed-components

   help-desk-browser
   search-for-docs
   goto-manual-link
   goto-hd-location))
