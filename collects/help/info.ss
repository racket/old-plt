; help collection
(module info (lib "infotab.ss" "setup")
  (define name "Help")
  (define compile-subcollections 
    '(("help" "private") ("help" "servlets" "private")))
  (define help-desk-message
    "Mr: (require (lib \"help-desk.ss\" \"help\")), Mz: read docs")
  (define mred-launcher-libraries (list "help.ss"))
  (define mred-launcher-names (list "Help Desk"))
  (define mzscheme-launcher-libraries (list "help-bg.ss"))
  (define mzscheme-launcher-names (list "Background Help Desk"))
  (define install-collection "installer.ss"))



	


	


