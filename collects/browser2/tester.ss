(require-library "macro.ss")
(require-library "function.ss")

(define files (list ;"render-html-test.ss"
                    "render-table-test.ss"))

; test-suite : (listof string) -> (void)
; Loads and executes all test cases for all renderers in the render-html suite.
; ListofFilenames contains the name of the file containing test cases for each renderer.
; EXAMPLE:
; (test-suite (list "render-html-test.ss" "render-table-test.ss")) -> (void)
; (test-suite empty) -> (void)
(define (test-suite ListofFilenames)
  (loop-until (sub1 (length ListofFilenames))
              (lambda (x) (< x 0))
              sub1
              (lambda (i)
                (local [(define a-file (list-ref ListofFilenames i))
                        (define frame (make-object frame% (string-append "OUTPUT for " a-file) #f 800 800))
                        (define text (make-object text%))
                        (define editor-canvas (make-object editor-canvas% frame text))]
                  (send text set-styles-sticky #f)
                  (send text auto-wrap #t)
                  (send frame show #t)
                  (load a-file)
                  (render text)))))
                 
(test-suite files)