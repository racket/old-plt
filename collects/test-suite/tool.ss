(module tool mzscheme
  
  (provide test-case-box-tool@)
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   (lib "snip-lib.ss" "mrlib" "private" "aligned-pasteboard")
   "private/test-case-box.ss")
  
  (define menu-extentions@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^ test-case-box^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define needs-reset? false)
      
      ;; Adds an Insert test case item to the special menu of the drscheme frame
      ;; Updates the needs-reset? when the the program is executed
      (define (test-case-mixin %)
        (class %
          (inherit get-special-menu get-edit-target-object)
          
          (rename [super-execute-callback execute-callback])
          (define/override (execute-callback)
            (super-execute-callback)
            (set! needs-reset? true))
          
          (super-new)
          
          (new menu-item%
            (label "Insert Test Case")
            (parent (get-special-menu))
            (callback
             (lambda (menu event)
               (let ([test-box (new test-case-box%)]
                     [text (get-edit-target-object)])
                 (send text insert test-box)
                 (send test-box resize 600 100)
                 (send text set-caret-owner test-box 'global)))))))
      
      (drscheme:get/extend:extend-unit-frame test-case-mixin)
      
      ;; Adds a hook in the reset-highlighting to clear all of the test-case results when the appropriate
      ;; STATUS: It's better to override reset-highlighting but this after-insert/delete works for now.
      ;(define clear-results-mixin
      ;  (mixin (drscheme:rep:text<%>) ()
      (define (clear-results-mixin %)
        (class %
          (inherit find-first-snip)
          
          (rename [super-after-insert after-insert])
          (define/override (after-insert start len)
            (super-after-insert start len)
            (reset-test-case-boxes))
          
          (rename [super-after-delete after-delete])
          (define/override (after-delete start len)
            (super-after-delete start len)
            (reset-test-case-boxes))
        
          (rename [super-set-modified set-modified])
          (define/override (set-modified b)
            (super-set-modified b)
            (when needs-reset? (reset-test-case-boxes)))
          
          ;; set all of the test-case-boxes in the definitions text to an unevaluated state
          (define/public (reset-test-case-boxes)
            (when needs-reset?
              (set! needs-reset? false)
              (for-each-snip
               (lambda (snip)
                 (when (is-a? snip test-case-box%)
                   (send snip update 'unknown)))
               (find-first-snip))))
            
          (super-new)))
      
      (drscheme:get/extend:extend-definitions-text clear-results-mixin)

      (define require-macro-mixin
        (mixin ((class->interface drscheme:rep:text%)) ()
          (inherit get-user-namespace)
          (rename [super-reset-console reset-console])
          (define/override (reset-console)
            (super-reset-console)
            (parameterize ([current-namespace (get-user-namespace)])
              (namespace-require '(lib "test-case.ss" "test-suite" "private"))))
          (super-new)))
      
      (drscheme:get/extend:extend-interactions-text require-macro-mixin)))
  
  (define test-case-box-tool@
    (compound-unit/sig
     (import (TOOL : drscheme:tool^))
     (link (MENU   : drscheme:tool-exports^ (menu-extentions@ TOOL CASE))
           (CASE   : test-case-box^ (test-case-box@ TOOL)))
     (export (var (MENU phase1))
             (var (MENU phase2)))))
  )