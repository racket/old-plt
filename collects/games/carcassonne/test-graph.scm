(module test-graph mzscheme

  (require (file "if.scm")
           (file "graph.scm")
           (file "tiles.scm")
           (file "test-regions.scm")
           "aux.scm"
           (file "Testing/testing.scm")
           (lib "class.ss"))
  
  (define (test comp i exp msg . visual?)
    (define-values (g t) (comp))
    (define (show t)
       (printf "~s @(~s,~s)" (send t snip) (send t get-x) (send t get-y)))
    (test-eq (send g potential-locations-for-tile i) exp (set=? tile=) msg)
    (when (pair? visual?)
      (for-each (lambda (t s)(show t) (show s) (newline))
                (send g potential-locations-for-tile i) exp)))
  
  (test road0 "2" 
        (list 

         (make-tile "2" 2 -1 90)
         (make-tile "2" 2 -1 180)
         (make-tile "2" 2 -1 270)
         
         
         (make-tile "2" 3 0 0)
         (make-tile "2" 3 0 180)
         (make-tile "2" 3 0 270)
         
         (make-tile "2" 3 +1 0)
         (make-tile "2" 3 +1 180)
         (make-tile "2" 3 +1 270)
         (make-tile "2" 0 +2 0)
         (make-tile "2" 0 +2 90)
         (make-tile "2" 0 +2 270)
         
         (make-tile "2" -1 1 0)
         (make-tile "2" -1 1 90) 
         
         (make-tile "2" -1 -1 90)
         (make-tile "2" -1 -1 180)
         (make-tile "2" -1 -1 270)
         
         
         (make-tile "2" -1 1 0)
         (make-tile "2" -1 1 90)
         
         
         (make-tile "2" -2 0 0)
         (make-tile "2" -2 0 90)
         (make-tile "2" -2 0 180)
         (make-tile "2" 1 -1 90)
         (make-tile "2" 1 -1 180)
         (make-tile "2" 1 -1 270)
         )        
        "road0")
                  
   #| INTERNAL TESTS:       |#
  (define graph-test%
    (class* graph% ()
      (inherit potential-locations-for-tile insert-tile insert-tile! reset)
      (super-new)
      (inherit-field last-tile)
        (define/public (ex-test)          
          (define graph0 this)
          (define graph1 (send graph0 insert-tile (make-tile "2" +1 0 90)))
          
          (printf "external testing graph ... ~n")
          
          ;; potential locations for tile 
          (test-eq (send graph0 potential-locations-for-tile "1") 
                   (list
                    (make-tile "1" 0 +1 0)
                    (make-tile "1" 0 +1 90)
                    (make-tile "1" 0 +1 180)
                    (make-tile "1" 0 +1 270))
                   (set=? tile=))
          
          (test-eq (send graph0 potential-locations-for-tile "2") 
                   (list
                    (make-tile "2" +1 0 90)
                    (make-tile "2" 0 +1 0)
                    (make-tile "2" 0 +1 90)
                    (make-tile "2" 0 +1 270)
                    (make-tile "2" -1 0 270))
                   list-tile=)
          
          (test-eq (send graph1 potential-locations-for-tile "2")
                   (list
                    (make-tile "2" +1 -1 90)
                    (make-tile "2" +1 -1 180)                      
                    (make-tile "2" +1 -1 270)
                    
                    (make-tile "2" +2 0 0)
                    (make-tile "2" +2 0 180)
                    (make-tile "2" +2 0 270)
                    
                    
                    (make-tile "2" +1 +1 0)
                    (make-tile "2" +1 +1 90)
                    (make-tile "2" +1 +1 270)
                    
                    (make-tile "2" 0 +1 0)
                    (make-tile "2" 0 +1 90)
                    (make-tile "2" 0 +1 270)
                    
                    (make-tile "2" -1 0 270))
                   (set=? tile=)
                   "graph1: potential locations for tile")
          
          (printf "...done~n"))))
  
    
  ;; --- TESTS ---
  
  (printf "testing inside of graph ...~n")
  (send (new graph%) test)
  (printf "done~n")
  
  (printf "testing external graph ...~n")
  
  (send (new graph-test%) ex-test)
    (printf "done~n")
  )
