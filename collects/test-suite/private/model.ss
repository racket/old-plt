(module model mzscheme
  
  (require
   (lib "unitsig.ss")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "tool.ss" "drscheme")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "private/aligned-pasteboard/snip-lib.ss" "mrlib")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "list.ss")
   "interfaces.ss"
   "signatures.ss"
   "test-text.ss")
  
  (provide model@)
  
  (define program-header-field-name "drscheme:test-suite:program")
  
  (define model@
    (unit/sig model^
      (import drscheme:tool^ case^ def^ expand-program^)
      (define model%
        (class* vertical-pasteboard% (test-suite:model<%>)
          (inherit insert delete find-first-snip begin-write-header-footer-to-file
                   end-write-header-footer-to-file begin-edit-sequence
                   end-edit-sequence)
          
          (init-field window)
          
          (field
           [expander false]
           [tests-showing? false]
           [program (instantiate text% ())]
           [language (preferences:get
                      (drscheme:language-configuration:get-settings-preferences-symbol))]
           [teachpacks empty])
          
          ;; get-program (-> string?)
          ;; the filename of the program to be tested by the test-suite
          (define/public (get-program)
            program)
          
          ;; set-program (string? . -> . void?)
          ;; set the program to the given filename
          (define/public (set-program v)
            (send* program
              (erase)
              (insert v)))
          
          ;; get-teachpacks (-> (listof string?))
          ;; the teachpacks currently installed in the language
          (define/public (get-teachpacks)
            teachpacks)
          
          ;; clear-teachpacks (-> void?)
          ;; set the teachpacks to empty
          (define/public (clear-teachpacks)
            (set! teachpacks empty))
          
          ;; remove-teachpack (string? . -> void?)
          ;; remove a teackpack from the list
          (define/public (remove-teachpack tp)
            (set! teachpacks (remove tp teachpacks)))
          
          ;; add-teachpack (string? . -> . void?)
          ;; add a teachpack
          (define/public (add-teachpack tp)
            (set! teachpacks (append teachpacks (list tp))))
          
          ;; insert-case (-> void?)
          ;; adds a new test case to the test-suite
          (define/public (insert-case)
            (insert (instantiate case% ()
                      (test-showing? tests-showing?))
                    false))
          
          ;; delete-case (-> void?)
          ;; removes the case that currently has focus
          (define/public (delete-case)
            (let ([snip (get-focused-snip this)])
              (when snip (delete snip))))
          
          ;; execute (-> void?)
          ;; runs the test-suite by executing each test-case
          (define/public (execute)
            (send window update-executing true)
            (set-expander)
            (reset-cases)
            (let ([program-filename (send program get-text)])
              (if (string=? program-filename "")
                  (eval-cases)
                  (send expander eval-file
                        program-filename
                        (lambda ()
                          (eval-cases))))))
          
          ;; set-expander (-> void?)
          ;; create a program expander and store it in the field
          (define/private (set-expander)
            (set! expander
                  (instantiate expand-program% ()
                    (language language)
                    (teachpacks teachpacks)
                    (error-handler (send window get-error-handler))
                    (clean-up
                     (lambda ()
                       (send window update-executing false))))))
          
          ;; reset-cases (-> void?)
          ;; reset all the test cases to unknown state
          (define/private (reset-cases)
            (for-each-snip
             (lambda (case)
               (send case reset))
             (find-first-snip)))
          
          ;; eval-cases (-> void?)
          ;; evaluate each case in the test-suite
          (define/private (eval-cases)
            (let ([case (find-first-snip)]
                  [next (lambda ()
                          (send expander done)
                          (send window update-executing false))])
              (if case
                  (send case execute expander next)
                  (next))))
          
          ;; break (-> void?)
          ;; stops execution of the test-suite
          (define/public (break)
            (when expander (send expander break)))
          
          ;; set-language (language? . -> . void?)
          ;; set the language to use to execute the cases
          (define/public (set-language l)
            (set! language l)
            (preferences:set
             (drscheme:language-configuration:get-settings-preferences-symbol)
             l))
          
          ;; get-language (-> language?)
          ;; get the language currently set
          (define/public (get-language)
            language)
          
          ;; show-tests (boolean? . -> . void?)
          ;; show the tests in the display
          (define/public (show-tests show?)
            (set! tests-showing? show?)
            (begin-edit-sequence)
            (for-each-snip
             (lambda (case)
               (send case show-test show?))
             (find-first-snip))
            (end-edit-sequence))
          
          ;; write-headers-to-file ((is-a?/c editor-stream-out%) . -> . boolean?)
          ;; writes the program to be tested to the file header
          (rename [super-write-headers-to-file write-headers-to-file])
          (define/override (write-headers-to-file stream)
            (let ([buffer (box 0)])
              (begin-write-header-footer-to-file stream program-header-field-name buffer)
              (send program write-to-file stream)
              (end-write-header-footer-to-file stream (unbox buffer))
              (super-write-headers-to-file stream)))
          
          ;; read-header-from-file ((is-a?/c editor-stream-in%) string? . -> . boolean?)
          ;; reads a named header from a file
          (rename [super-read-header-from-file read-header-from-file])
          (define/override (read-header-from-file stream name)
            (if (string=? name program-header-field-name)
                (send program read-from-file stream)
                (super-read-header-from-file stream name)))
          
          ;; set-modified (boolean . -> . void?)
          ;; called when the editor is modified
          (rename [super-set-modified set-modified])
          (define/override (set-modified modified?)
            (send window update-modified modified?)
            (super-set-modified modified?))
          
          ;; load-file (boolean? . -> . void?)
          ;; called after a file is loaded into the editor
          (rename [super-after-load-file after-load-file])
          (define/override (after-load-file success?)
            (super-after-load-file success?)
            (when success? (set-modified false)))
          
          (super-instantiate ())
          ))
      
      ;; get-focused-snip: ((is-a?/c editor<%>) . -> . (union (is-a?/c snip%) false?))
      ;; the snip that is currently focused in the editor
      (define (get-focused-snip editor)
        (send editor get-focus-snip))
      ))
  )
