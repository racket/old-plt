(module prefs-file-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide framework:prefs-file^)
  (define-signature framework:prefs-file^
    (get-preferences-filename)))
    