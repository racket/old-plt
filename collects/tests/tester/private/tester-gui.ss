;; tester-gui.ss: a facade hiding the GUI implementation.

(module tester-gui mzscheme
  (require (lib "tester-controller.ss" "tests" "tester" "private"))
  (require (lib "class.ss"))
  
  (provide get-tester-gui)
  
  ;; get-tester-gui : -> controller%
  ;; to create the GUI and return an interface to it
  (define (get-tester-gui)
    (make-object controller%)))