(module check-gui mzscheme
  (provide check-version)

  (require "private/gui-defs.ss")
  (require "private/go-check.ss")

  (define check-version
    (case-lambda
     [()
      (go-check 
       #f ; no parent frame
       #f ; async
       gui-defs@)]
     [(parent-frame)
      (go-check 
       parent-frame
       #f ; async
       gui-defs@)]
     [(parent-frame sync?)
      (go-check 
       parent-frame
       sync?
       gui-defs@)])))



