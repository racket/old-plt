#cs
(module test-final-score mzscheme 
  (require "if.scm"
           "test-regions.scm"
           "tiles.scm"
           "graph.scm"
           "view.scm"
           "aux.scm"
           (file "Testing/testing.scm")
           (lib "class.ss"))
  
  ;; GraphMaker Listof[(list Follower Number)] String -> Void
  ;; create a graph and test final scores
  (define (test-score gm expected msg . rem)
    (define-values (g x) (gm))
    (test-eq (if (pair? rem) 
                 (send g abbey-regions #t)
                 (send g abbey-regions #t))
             expected 
             (set=? equal?)
             (format "testing final scoring (abbey-regions): ~a"  msg))
    (when (pair? rem)
      (test== (send g abbey-regions) 
              '() 
              (format "testing final scoring (abbey-regions with removal): ~a" msg))))
  
  (printf "testing final scoring ...~n")

  (test-score 
   (let* ([comp road0]
          [comp (place-follower comp -1 0 INNER RED)]
          [comp (place-follower comp +2 0 INNER GREEN)]
          [comp (place-follower comp 0 +1 INNER BLUE)])
     comp)
   `([,RED 2]
     [,GREEN 3]
     [,BLUE 4])
   'road0)
  
  (test-score 
   (let* ([comp road0]
          [comp (place-follower comp -1 0 INNER RED)]
          [comp (place-follower comp +2 0 INNER GREEN)]
          [comp (place-follower comp 0 +1 INNER BLUE)])
     comp)
   `([,RED 2]
     [,GREEN 3]
     [,BLUE 4])
   'road0
   'remove)
  
  (test-score 
   (let* ([comp road1]
          [comp (place-follower comp -1 0 INNER RED)]
          [comp (place-follower comp 0 +1 INNER BLUE)])
     comp)
   `([,RED 2]
     [,BLUE 3])
   'road1)
  
  (test-score 
   (let* ([comp road1]
          [comp (place-follower comp -1 0 INNER RED)]
          [comp (place-follower comp 0 +1 INNER BLUE)])
     comp)
   `([,RED 2]
     [,BLUE 3])
   'road1
   'remove)
  
  (test-score 
   (let* ([comp road2]
          [comp (place-follower comp -1 0 INNER RED)]
          [comp (place-follower comp 0 +1 INNER BLUE)])
     comp)
   `([,RED 2]
     [,BLUE 6])
   'road2)
  
  (test-score 
   (let* ([comp road2]
          [comp (place-follower comp -1 0 INNER RED)]
          [comp (place-follower comp 0 +1 INNER BLUE)])
     comp)
   `([,RED 2]
     [,BLUE 6])
   'road2
   'remove)
  
  (test-score road3 '() 'road3)
  
  (printf ".. .done~n")

  )
