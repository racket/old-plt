(module test-suite-tool mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   "private/signatures.ss"
   "private/window.ss"
   "private/window-layout.ss"
   "private/window-menus.ss"
   "private/window-language.ss"
   "private/model.ss"
   "private/case.ss"
   "private/case-layout.ss"
   "private/case-tab.ss"
   "private/def.ss"
   "private/helper.ss"
   "private/expand-program.ss"
   ;; The new test-case boxes
   "tool.ss")
  
  (provide tool@)
  
  (define tool-phases@
    (unit/sig tool-phases^
      (import)
      (define (phase1) (void))
      (define (phase2) (void))))
  
  (define test-suite-menu-items@
    (unit/sig test-suite-menu-items^
      (import drscheme:tool^ window^)
      
      ;; test-suite:frame-basics-mixin mixin-contract?
      ;; a new frame with an open and new file menu option for test-suites
      (define (test-suite:frame-basics-mixin super%)
        (class super%
          (inherit get-definitions-text)
          
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
                 (let ([window (instantiate window% ())]
                       [program (send (get-definitions-text) get-filename)])
                   (when program (send window set-program program))
                   (send window show true)))))
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
                 (let ([f (get-file false this)])
                   (when f
                     (let ([w (instantiate window% ())])
                       (if (file-exists? f)
                           (with-handlers
                               ([exn? (lambda (exn?)
                                        (message-box "Error" "Not a valid test-suite")
                                        (send w close))])
                             (send w load-file f)
                             (send w show true))
                           (begin
                             (message-box "Error" "That file does not exist")
                             (send w close)))))))))
            (super-file-menu:between-open-and-revert file-menu))
          
          (super-instantiate ())
          ))
      
      (drscheme:get/extend:extend-unit-frame test-suite:frame-basics-mixin)
      ))
    
  (define tool@
    (compound-unit/sig
      (import (TOOL : drscheme:tool^))
      (link (PHASES          : tool-phases^ (tool-phases@))
            (MENU-ITEMS      : test-suite-menu-items^ (test-suite-menu-items@ TOOL WINDOW-LANGUAGE))
            
            (WINDOW          : window^ (window@ TOOL MODEL))
            (WINDOW-LAYOUT   : window^ (window-layout@ TOOL WINDOW))
            (WINDOW-MENUS    : window^ (window-menus@ WINDOW-LAYOUT))
            (WINDOW-LANGUAGE : window^ (window-language@ TOOL WINDOW-MENUS))
            
            (MODEL           : model^ (model@ TOOL
					      CASE-SNIPCLASS
					      DEF
					      HELPER
					      EXPAND-PROGRAM))
            
	    (HELPER          : helper^ (helper@))

            (CASE            : case^ (case@ TOOL))
            (CASE-LAYOUT     : case^ (case-layout@ CASE))
            (CASE-TAB        : case^ (case-tab@ CASE-LAYOUT))
            (CASE-SNIPCLASS  : case^ (case-snipclass@ CASE-TAB))
            
            (DEF             : def^ (def@))
            (EXPAND-PROGRAM  : expand-program^ (expand-program@ TOOL))
            (TEST-CASE-BOX   : tool-phases^ (test-case-box-tool@ TOOL)))
      (export (var (PHASES phase1))
              (var (PHASES phase2)))
      ))
  )
