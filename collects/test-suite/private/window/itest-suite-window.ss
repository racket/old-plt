(module itest-suite-window mzscheme
  
  (require
   (lib "class.ss"))
  
  (provide
   test-suite-window<%>)
  
  (define test-suite-window<%>
    (interface ()
      ;; update-modified (boolean? . -> . void?)
      ;; called by the model when it has been modified
      update-modified
      
      ;; update-executing (boolean? . -> . void?)
      ;; called by the model when it is executing
      update-executing
      
      ;; get-error-handler (-> (string? exn? . -> . void?))
      ;; the error handler that is used to display errors to the window
      get-error-handler
      ))
  )