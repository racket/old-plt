
; Define an order on the standard docs:
(cons
 ;; Order-assigning procedure:
 (lambda (d)
  (case (string->symbol d)
    [(help) -10]
    [(drscheme) -1]
    [(r5rs) 0]
    [(mzscheme) 1]
    [(mred) 2]
    [(framework) 4]
    [(mrspidey) 5]
    [(misclib) 6]
    [(teach) 8]
    [(mzc) 10]
    [(tools) 30]
    [(insidemz) 50]
    [else 100]))
 ;; Known manuals:
 '(("drscheme" . "PLT DrScheme: Programming Environment Manual")
   ("r5rs" . "Revised(5) Report on the Algorithmic Language Scheme")
   ("mzscheme" . "PLT MzScheme: Language Manual")
   ("mred" . "PLT MrEd: Graphical Toolbox Manual")
   ("framework" . "PLT Framework: GUI Application Framework")
   ("mrspidey" . "PLT MrSpidey: Static Debugger Manual")
   ("misclib" . "PLT Miscellaneous Libraries: Reference Manual")
   ("mzc" . "PLT mzc: MzScheme Compiler Manual")
   ("insidemz" . "Inside PLT MzScheme")
   ("tools" . "PLT Tools: DrScheme Extension Manual")))
