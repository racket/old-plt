(module docpos mzscheme
  (provide standard-html-doc-position known-docs)
  
  ; Define an order on the standard docs:
  (define (standard-html-doc-position d)
    (case (string->symbol d)
      [(help) -10]
      
      [(beginning) -5]
      [(intermediate) -4]
      [(advanced) -3]
      
      [(drscheme) -1]
      [(r5rs) 0]
      [(mzscheme) 1]
      [(mzlib) 2]
      [(mred) 3]
      [(framework) 4]
      [(mrspidey) 5]
      [(misclib) 6]
      [(teach) 8]
      [(mzc) 10]
      [(tools) 30]
      [(insidemz) 50]
      [else 100]))
  
  ; Known manuals:
  (define known-docs
    '(("beginning" . "PLT DrScheme: Beginning Student Language")
      ("advanced" . "PLT DrScheme: Advanced Student Language")
      ("intermediate" . "PLT DrScheme: Intermediate Student Language")
      ("drscheme" . "PLT DrScheme: Programming Environment Manual")
      ("r5rs" . "Revised(5) Report on the Algorithmic Language Scheme")
      ("mzscheme" . "PLT MzScheme: Language Manual")
      ("mzlib" . "PLT MzLib: Libraries Manual")
      ("mred" . "PLT MrEd: Graphical Toolbox Manual")
      ("framework" . "PLT Framework: GUI Application Framework")
      ("mrspidey" . "PLT MrSpidey: Static Debugger Manual")
      ("misclib" . "PLT Miscellaneous Libraries: Reference Manual")
      ("match" . "PLT Match: Pattern Matching for Scheme Manual")
      ("mzc" . "PLT mzc: MzScheme Compiler Manual")
      ("insidemz" . "Inside PLT MzScheme")
      ("tools" . "PLT Tools: DrScheme Extension Manual")
      ("t-y-scheme" . "Teach Yourself Scheme in Fixnum Days"))))
