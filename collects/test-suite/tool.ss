(module tool mzscheme
  
  (provide tool@)
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   (lib "string-constant.ss" "string-constants")
   (lib "snip-lib.ss" "mrlib" "private" "aligned-pasteboard")
   "private/test-case-box.ss"
   "private/find-scheme-menu.ss")
  
  (define menu-extentions@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^ test-case-box^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define needs-reset? false)
      
      ;; Adds the test suite tool menu to the Dr. Scheme frame
      ;; Updates the needs-reset? when the the program is executed
      (define (test-case-mixin %)
        (class %
          (inherit get-definitions-text get-edit-target-object get-menu-bar
                   get-special-menu)
          
          (rename [super-execute-callback execute-callback])
          (define/override (execute-callback)
            (send (get-definitions-text) for-each-test-case
                  (lambda (case) (send case reset)))
            (super-execute-callback)
            (set! needs-reset? true))
          
          ;; enable all of the test-cases
          (define (enable enable?)
            (send (get-definitions-text) for-each-test-case
                  (lambda (case) (send case enable enable?))))
          
          ;; this function is copied from the drscheme/private/unit.ss file
          (define (has-editor-on-demand menu-item)
            (let ([edit (get-edit-target-object)])
              (send menu-item enable (and edit (is-a? edit editor<%>)))))
          
          (super-new)
          
          (field
           [test-cases-enabled? true]
           [insert-menu-item
            (new menu-item%
                 (label (string-constant test-case-insert))
                 (parent (get-special-menu))
                 (callback
                  (lambda (menu event)
                    (let ([test-box (new test-case-box% (enabled? test-cases-enabled?))]
                          [text (get-edit-target-object)])
                      (when text
                        (send text begin-edit-sequence)
                        (send text insert test-box)
                        (send test-box take-caret)
                        (send text end-edit-sequence)))))
                 (demand-callback has-editor-on-demand))])
          (let ([parent (find-scheme-menu (get-special-menu))])
            (and parent
                 (new menu-item%
                      (parent parent)
                      (label "Disable All Test Cases")
                      (callback
                       (lambda (menu event)
                         (set! test-cases-enabled? (not test-cases-enabled?))
                         (if test-cases-enabled?
                             (send menu set-label "Disable all Test Cases")
                             (send menu set-label "Enable all Test Cases"))
                         (send (get-definitions-text) for-each-test-case
                               (lambda (tc) (send tc enable test-cases-enabled?))))))))))

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
            (when b (reset-test-case-boxes)))
          
          ;; set all of the test-case-boxes in the definitions text to an unevaluated state
          (define/public (reset-test-case-boxes)
            (when needs-reset?
              (set! needs-reset? false)
              (for-each-test-case (lambda (snip) (send snip reset)))))
          
          ;; executes the given function on each test-case-box
          (define/public (for-each-test-case f)
            (for-each-snip
               (lambda (snip)
                 (when (is-a? snip test-case-box%)
                   (f snip)))
               (find-first-snip)))
          
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
  
  (define tool@
    (compound-unit/sig
     (import (TOOL : drscheme:tool^))
     (link (MENU   : drscheme:tool-exports^ (menu-extentions@ TOOL CASE))
           (CASE   : test-case-box^ (test-case-box@ TOOL)))
     (export (var (MENU phase1))
             (var (MENU phase2)))))
  )
