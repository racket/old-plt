(module check-gui mzscheme
  (provide check-version)

  (require "private/gui-defs.ss")
  (require "private/go-check.ss")

  (define check-version
    (case-lambda
     [()
      (go-check 
       #f ; no parent frame
       gui-defs@)]
     [(parent-frame)
      (go-check 
       parent-frame
       gui-defs@)])))


