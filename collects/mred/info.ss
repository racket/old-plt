
(module info (lib "infotab.ss" "setup")
  (define doc.txt "doc.txt")
  (define name "MrEd")
  (define version '(200))
  (define post-install-collection "script-installer.ss")

  (define tools '(("private/bd-tool.ss")))
  (define tool-names '("BD"))
  (define tool-icons '((#f))))
