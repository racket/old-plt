(module test-admin mzscheme
  
   (require "admin.scm"
            (lib "class.ss"))
  
  (define a (new admin%))

  (printf "testing admin ... ~n")
  
  (send a test-pnt)
  (send a test-register)
  (send a test-ranking)
  (send a test-run)
  (send a test-final-eval)
  
  (printf "... done~n")
 
  )