(module help-desk mzscheme
  (require "private/server.ss"  
	   "private/browser.ss"
	   "private/docpos.ss"
	   "private/finddoc.ss"
	   "private/search.ss"
	   "private/manuals.ss"
	   "private/installed-components.ss")

  (provide 
    ;; server functions
    start-help-server
    hd-cookie->port
    hd-cookie?
    ;; browser functions
    help-desk-browser
    ;; manual ordering
    standard-html-doc-position
    user-defined-doc-position
    set-doc-position!
    reset-doc-positions!
    known-docs
    ;; search functions
    search-for-docs
    do-search
    doc-collections-changed
    ;; manuals
    find-manuals
    ;; manual search
    finddoc
    findreldoc
    finddoc-page
    ;; supplemental
    help-desk:installed-components))



