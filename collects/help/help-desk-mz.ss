(module help-desk-mz mzscheme
  (require "private/external-server.ss"  
           "private/server-config.ss"
	   "private/docpos.ss"
	   "private/finddoc.ss"
	   "private/search.ss"
	   "private/manuals.ss"
	   "private/installed-components.ss")

  (provide 
    ;; server functions
    (rename external-start-help-server start-help-server)
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
    help-desk:installed-components))



