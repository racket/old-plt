(module help-desk mzscheme
  (require "help-desk-mz.ss"
  	   "private/browser.ss"
  	   (prefix server: "private/server.ss")
	   "private/external-search.ss")

  (provide (all-from-except "help-desk-mz.ss" start-help-server))

  ; these depend on MrEd
  ; unlike the one in help-desk-mz.ss, which only starts a TCP/IP
  ;  server, the start-help-server here may also launch the PLT browser
  (provide (rename server:start-help-server start-help-server)
	   help-desk-browser
           in-help-desk-navigate?
           search-for-docs
	   goto-manual-link
	   goto-hd-location))




