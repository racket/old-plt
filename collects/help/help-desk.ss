(module help-desk mzscheme
  (require "help-desk-mz.ss"
  	   "private/browser.ss"
	   "private/external-search.ss")

  ; from help-desk-mz
  (provide 
    ;; server functions
    start-help-server
    hd-cookie->port
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
    help-desk:installed-components)

  ; these depend on MrEd
  (provide help-desk-browser
           search-for-docs))



