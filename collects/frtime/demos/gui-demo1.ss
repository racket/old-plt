(module gui-demo1 (lib "frtime.ss" "frtime")
  
  (require (lib "gui.scm" "frtime"))
  (provide (all-defined-except))
  
  (define kinds (list "New York" "Chicago" "California" "Hawaii"))
  (define sizes (list "small" "medium" "large" "Texas"))
  
  (define customer
    (make-text "Customer name:"))
  
  (define kind
    (make-choice "Kind:" kinds))
  
  (define size
    (make-choice "Size:" sizes))
  
  (define button-event
    (make-button "Confirm"))
  
  (make-message
   (hold (snapshot-map-e
          (lambda (_ c k s)
            (string-append c " ordered a "
                           (list-ref sizes s) " "
                           (list-ref kinds k) " pizza."))
          button-event customer kind size))))
