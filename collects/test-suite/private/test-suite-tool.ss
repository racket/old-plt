(module test-suite-tool mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "signatures.ss"
   "window.ss"
   "window-layout.ss"
   "window-menus.ss"
   "model.ss"
   "case.ss"
   "case-layout.ss"
   "def.ss"
   "expand-program.ss")
  
  (provide tool@)
  
  (define tool-phases@
    (unit/sig tool-phases^
      (import)
      (define (phase1) (void))
      (define (phase2) (void))))
  
  (define drscheme-extentions@
    (unit/sig drscheme-extentions^
      (import drscheme:tool^ window^)
      
      ;; open-test-suite ((union string? false?) . -> . void?)
      ;; opens a test-suite window
      (define (open-test-suite filename)
        (let ([window (instantiate window% ())])
          (send window show true)
          (when filename (send window load-file filename))))
      
      ;; test-suite:frame-basics-mixin mixin-contract?
      ;; a new frame with an open and new file menu option for test-suites
      (define (test-suite:frame-basics-mixin super%)
        (class super%
          
          ;; file-menu:between-new-and-open ((is-a?/c file-menu%) . -> . void?)
          ;; called when contructing the menu between new and open
          (rename [super-file-menu:between-new-and-open
                   file-menu:between-new-and-open])
          (define/override (file-menu:between-new-and-open file-menu)
            (instantiate menu-item% ()
              (label "New Test Suite")
              (parent file-menu)
              (callback
               (lambda (menu event)
                 (open-test-suite false))))
            (super-file-menu:between-new-and-open file-menu))
          
          ;; file-menu:between-open-and-revert ((is-a?/c file-menu%) . -> . void?)
          ;; called when contructing the menu between open and revert
          (rename [super-file-menu:between-open-and-revert
                   file-menu:between-open-and-revert])
          (define/override (file-menu:between-open-and-revert file-menu)
            (instantiate menu-item% ()
              (label "Open Test Suite...")
              (parent file-menu)
              (callback
               (lambda (menu event)
                 (let ([file (get-file)])
                   (when file (open-test-suite file))))))
            (super-file-menu:between-open-and-revert file-menu))
          
          (super-instantiate ())
          ))
      
      (drscheme:get/extend:extend-unit-frame test-suite:frame-basics-mixin)
      ))
  
  (define tool@
    (compound-unit/sig
      (import (TOOL : drscheme:tool^))
      (link (PHASES          : tool-phases^ (tool-phases@))
            (EXTENTIONS      : drscheme-extentions^ (drscheme-extentions@ TOOL WINDOW-MENUS))
            
            (WINDOW          : window^ (window@ TOOL MODEL))
            (WINDOW-LAYOUT   : window^ (window-layout@ TOOL WINDOW))
            (WINDOW-MENUS    : window^ (window-menus@ WINDOW-LAYOUT))
            
            (MODEL           : model^ (model@ TOOL CASE-SNIPCLASS DEF EXPAND-PROGRAM))
            (CASE            : case^ (case@))
            (CASE-LAYOUT     : case^ (case-layout@ CASE))
            (CASE-SNIPCLASS  : case^ (case-snipclass@ CASE-LAYOUT))
            (DEF             : def^ (def@))
            (EXPAND-PROGRAM  : expand-program^ (expand-program@ TOOL))
            )
      (export (var (PHASES phase1))
              (var (PHASES phase2)))
      ))
  )