(module test-suite-model mzscheme
  
  (require
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "private/aligned-pasteboard/snip-lib.ss" "mrlib")
   "test-case.ss")
  
  (define program-header-field-name "drscheme:test-suite:program")
  
  (provide test-suite-model%)
  
  (define test-suite-model%
    (class vertical-pasteboard%
      (inherit insert delete find-first-snip begin-write-header-footer-to-file
               end-write-header-footer-to-file)
      
      (init-field window)
      
      (field
       [expand-program-class false]
       [expand-program-instance false])
      
      (field
       [program (instantiate text% ())])
      
      ;; set-expander (class? . -> . void?)
      ;; set the expander class to be used
      (define/public (set-expander c%)
        (set! expand-program-class c%))
      
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
      
      ;; insert-case (-> void?)
      ;; adds a new test case to the test-suite
      (define/public (insert-case)
        (insert (instantiate test-case% ()) false))
      
      ;; delete-case (-> void?)
      ;; removes the case that currently has focus
      (define/public (delete-case)
        (let ([snip (get-focused-snip this)])
          (when snip (delete snip))))
      
      ;; execute (-> void?)
      ;; runs the test-suite by executing each test-case
      (define/public (execute)
        (for-each-snip
         (lambda (case)
           (send case reset))
         (find-first-snip))
        (if expand-program-class
            (let ([expander (instantiate expand-program-class ())]
                  [program-filename (send program get-text)])
              (set! expand-program-instance expander)
              (send window update-executing true)
              (if (string=? program-filename "")
                  (eval-cases expander)
                  (send expander eval-file (send program get-text)
                        (lambda ()
                          (eval-cases expander)))))
            (error 'execute "expand-program% not set")))
      
      ;; break (-> void?)
      ;; stops execution of the test-suite
      (define/public (break)
        (when expand-program-instance
          (send expand-program-instance break)))
      
      ;; eval-case ((is-a?/c expand-program%) . -> . (-> void?))
      ;; evaluate the test cases in the test-suite
      (define/private (eval-cases expander)
        (letrec ([eval-case
                  (lambda (case)
                    (if case
                      (send expander eval-case case
                            (lambda ()
                              (eval-case (send case next))))
                      (begin
                        (send expander done)
                        (send window update-executing false))))])
          (eval-case (find-first-snip))))
      
      ;; write-headers-to-file ((is-a?/c editor-stream-out%) . -> . boolean?)
      ;; writes the program to be tested to the file header
      (rename [super-write-headers-to-file write-headers-to-file])
      (define/override (write-headers-to-file stream)
        (let ([buffer (box 0)])
          (begin-write-header-footer-to-file stream program-header-field-name buffer)
          (send stream put (send program get-text))
          (end-write-header-footer-to-file stream (unbox buffer))
          (super-write-headers-to-file stream)))
      
      ;; read-header-from-file ((is-a?/c editor-stream-in%) string? . -> . boolean?)
      ;; reads a named header from a file
      (rename [super-read-header-from-file read-header-from-file])
      (define/override (read-header-from-file stream name)
        (if (string=? name program-header-field-name)
            (begin
              (send* program
                (erase)
                (insert (send stream get-string)))
              true)
            (super-read-header-from-file stream name)))
      
      ;; set-modified (boolean . -> . void?)
      ;; called when the editor is modified
      (rename [super-set-modified set-modified])
      (define/override (set-modified modified?)
        (send window update-modified modified?)
        (super-set-modified modified?))
      
      (super-instantiate ())
      ))
  
  ;; get-focused-snip: ((is-a?/c editor<%>) . -> . (union (is-a?/c snip%) false?))
  ;; the snip that is currently focused in the editor
  (define (get-focused-snip editor)
    (send editor get-focus-snip))
  )
