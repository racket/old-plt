(module string-constants mzscheme
  (provide string-constants)
  
  (define english-string-constants
    '((case-sensitive?-label "Case sensitive")
      (output-style-label "Output Style")
      (constructor-printing-style "Constructor")
      (quasiquote-printing-style "Quasiquote")
      (write-printing-style "write")
      (sharing-printing?-label "Show sharing in values")
      (pring-rationals-label "Print rationals in whole/part notation")
      (booleans-as-true/false-label "Print booleans as true and false") 
      (use-pretty-printer?-label "Insert newlines in printed values")
      
      ))
  
  ;; use mred resources to save this -- framework prefs won't work
  ;; since this is elaboration time, not run-time of DrScheme
  (define language 'english)
  
  (define string-constants
    (case language
      [(english) english-string-constants])))
