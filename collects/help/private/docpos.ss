(module docpos mzscheme
  (provide standard-html-doc-position known-docs)
  
  ;; Define an order on the standard docs.
  (define (standard-html-doc-position d)
    (case (string->symbol d)
  
    [(beginning) -20]
      [(beginning-abbr) -19]
      [(intermediate) -17]
      [(advanced) -15]

      [(r5rs) -10]
      [(mzscheme) -5]
      [(mred) -4]

      [(help) 0]
      [(drscheme) 1]
      [(mzlib) 2]
      [(framework) 4]
      [(misclib) 6]
      [(teach) 8]
      [(mzc) 10]
      [(tools) 30]
      [(insidemz) 50]
      [else 100]))
  
  ; Known manuals:
  (define known-docs
    '(("beginning" . "Beginning Student Language")
      ("beginning-abbr" . "Beginning Student with List Abbreviations Language")
      ("advanced" . "Advanced Student Language")
      ("intermediate" . "Intermediate Student Language")
      ("drscheme" . "PLT DrScheme: Programming Environment Manual")
      ("r5rs" . "Revised(5) Report on the Algorithmic Language Scheme")
      ("mzscheme" . "PLT MzScheme: Language Manual")
      ("mzlib" . "PLT MzLib: Libraries Manual")
      ("mred" . "PLT MrEd: Graphical Toolbox Manual")
      ("framework" . "PLT Framework: GUI Application Framework")
      ("misclib" . "PLT Miscellaneous Libraries: Reference Manual")
      ("mzc" . "PLT mzc: MzScheme Compiler Manual")
      ("insidemz" . "Inside PLT MzScheme")
      ("tools" . "PLT Tools: DrScheme Extension Manual")
      ("t-y-scheme" . "Teach Yourself Scheme in Fixnum Days"))))
