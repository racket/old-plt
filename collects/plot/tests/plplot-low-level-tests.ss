; test low level plplot library - does not require mr-ed
(module plplot-low-level-tests mzscheme
  (require 
   "test-helpers.ss"
   (lib "test.ss" "schemeunit")         
   (lib "plplot-low-level-loader.ss" "plot")
   (lib "file.ss")
   (lib "etc.ss"))
  
  (provide plplot-low-level-tests)
  
  ; this test suite can only make some files and check if they are the correct size.
  ; there should be no difference on various platforms (i think)
  
  (define
    plplot-low-level-tests
    (make-test-suite
      "plplot-low-level-tests"
      (make-test-case
        "basic file size check"
        (assert-eq?
         1196 ; just happens to be the size
         (let ((file-name (simplify-path (build-path (this-expression-source-directory) "test.png"))))
           (pl-set-device "png")
           (pl-set-output-file file-name)
           (pl-set-colormap0-index 0 255 255 255) ; white
           (pl-init-plot)
           (pl-set-colormap0-index 0 0 0 0) ; black
           (pl-select-colormap0-index 0) ; black
           (pl-set-plot-environment -5 5 -5 5 1 1)
           (pl-set-labels "x" "y" "a plot")          
           (pl-plot-segment 0 0 1 1)
           (pl-plot-line 5 '(1 2 2.5 (/ 1 4) 3 4) '(1 2 3 4 5))
           (pl-select-colormap0-index 10)
           (pl-plot-segment 1 1 2 2)
           (pl-set-line-width 10)
           (pl-plot-segment 3 3 4 4)
           (pl-finish-plot)
           (begin0
             (file-size file-name)
             (delete-file file-name))))))))
         
  
  
  
  
  
  
  
  
  
  
  
