(module interfaces mzscheme
  
  (require
   (lib "class.ss")
   (lib "aligned-pasteboard.ss" "mrlib"))
  
  (provide
   test-suite:window<%>
   test-suite:item<%>
   test-suite:model<%>)
  
  ;; the interface that is implemented by a window that contains a test-suite:model<%>
  (define test-suite:window<%>
    (interface () ;frame<%>?
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
 
  ;; should this have focus-first? It seems to be called on a case!
  
  ;; the interface that is implemented by item in a test-suite
  (define test-suite:item<%>
    (interface (stretchable-snip<%>)
      ;; show-test (boolean? . -> . void?)
      ;; how/hide the test in the display
      show-test

      ;; reset (-> void?)
      ;; resets the result of the test case
      reset

      ;; execute ((is-a?/c expand-program%) ((union (id-s?/c snip%) false?) . -> . void?) . -> . void?)
      ;; execute the item
      execute
      
      ;; lock (boolean? . -> . void?)
      ;; lock or unlock the test case for modification
      lock
    ))
  
  ;; the interface of a model that may be contained in a test-suite:window<%>
  (define test-suite:model<%>
    (interface (aligned-pasteboard<%>)
      ;; get-program (-> string?)
      ;; the filename of the program to be tested by the test-suite
      get-program
      
      ;; set-program (string? . -> . void?)
      ;; set the program to the given filename
      set-program
      
      ;; insert-case (-> void?)
      ;; adds a new test case to the test-suite
      insert-case

      ;; insert-helper (-> void?)
      ;; adds a new helper function definitions section to the test-suite
      insert-helper
      
      ;; delete-case (-> void?)
      ;; removes the case that currently has focus
      delete-case
      
      ;; execute (-> void?)
      ;; runs the test-suite by executing each test-case
      execute
      
       ;; break (-> void?)
      ;; stops execution of the test-suite
      break
      
      ;; set-language (language? . -> . void?)
      ;; set the language to use to execute the cases
      set-language
          
      ;; get-language (-> language?)
      ;; get the language currently set
      get-language
          
      ;; show-tests (boolean? . -> . void?)
      ;; show the tests in the display
      show-tests
          
      ;; get-teachpacks (-> (listof string?))
      ;; the teachpacks currently installed in the language
      get-teachpacks
      
      ;; clear-teachpacks (-> void?)
      ;; set the teachpacks to empty
      clear-teachpacks
      
      ;; remove-teachpack (string? . -> void?)
      ;; remove a teackpack from the list
      remove-teachpack
      
      ;; add-teachpack (string? . -> . void?)
      ;; add a teachpack
      add-teachpack
      ))
  )
