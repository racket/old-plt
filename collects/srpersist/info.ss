;; info.ss for srpersist collection

;; no .zo compilation necessary, since all the
;; real code is in C++

(module info (lib "infotab.ss" "setup")
  (define name "SrPersist")
  (define help-desk-message 
     "Mz/Mr: (require (lib \"invoke-n.m\" \"srpersist\")")
  (define compile-omit-files
    '("info.ss"
      "invoke-1.0.ss"
      "invoke-2.0.ss"
      "invoke-3.0.ss"
      "invoke-3.5.ss"))
  (define blurb
    (list
      "SrPersist is an extension for using ODBC databases from Scheme."))
  (define release-version "200alpha1")
  (define release-iteration "0"))


