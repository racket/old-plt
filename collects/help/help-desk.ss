(module help-desk mzscheme
  (require "help-desk-mz.ss"
  	   "private/browser.ss"
	   "private/external-search.ss")

  (provide (all-from "help-desk-mz.ss"))

  ; these depend on MrEd
  (provide help-desk-browser
           search-for-docs
	   goto-manual-link))



