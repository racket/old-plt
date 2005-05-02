;; testing the basic tile methods 
#cs
(module test-tiles mzscheme 
  (require (file "if.scm")
           (file "tiles.scm")
           (file "Testing/testing.scm")
           (lib "class.ss"))
  
  (printf "external testing pieces ... ~n")
  
  (test== (send (make-tile "00" 0 0 0) match NORTH (make-tile "1" 0 0 0)) #f "non-match")
  (test== (send (make-tile "00" 0 0 0) match SOUTH (make-tile "1" 0 0 0)) #t "g-g match")
  (test== (send (make-tile "00" 0 0 0) match EAST  (make-tile "1" 0 0 0)) #f "non-match")
  (test== (send (make-tile "00" 0 0 0) match NORTH (make-tile "3" 0 0 0)) #t "c-c match")
  (test== (send (make-tile "00" 0 0 0) match NORTH (make-tile "3" 0 0 90)) #t "c-c match 90")
  (test== (send (make-tile "00" 0 0 0) match NORTH (make-tile "3" 0 0 180)) #t "c-c match 180")
  (test== (send (make-tile "00" 0 0 0) match NORTH (make-tile "3" 0 0 270)) #t "c-c match 270")
  (test== (send (make-tile "00" 0 0 0) match EAST  (make-tile "10" 0 0 180)) #f "r-r match")
  
  (test== (send (make-tile "2" 0 0 180) match SOUTH (make-tile "00" 0 0 0)) #f "r-c match 180")
  (test== (send (make-tile "2" 0 0  90) match SOUTH (make-tile "00" 0 0 0)) #f "g-c match 90")
  
  (test== (send (make-tile "00" 0 0 0) abbey?) #f)
  (test== (send (make-tile "1" 0 0 0) abbey?) #t)
  
  (test== (send (make-tile "00" 0 0 0) end-of-road?) #f "eor00")
  (test== (send (make-tile "2" 0 0 0) end-of-road? ) #t "eor2")
  (test== (send (make-tile "6" 0 0 0) end-of-road? ) #t "eor6")
  (test== (send (make-tile "7" 0 0 0) end-of-road? ) #t "eor7")
  (test== (send (make-tile "11" 0 0 0) end-of-road? ) #f "eor11")
  (test== (send (make-tile "19" 0 0 0) end-of-road?) #t "eor19")
  (test== (send (make-tile "23" 0 0 0) end-of-road?) #t "eor23")
  (test== (send (make-tile "24" 0 0 0) end-of-road?) #t "eor24")
  
  (test== (send (make-tile "19" 0 0 0) roads) (list EAST SOUTH WEST))
  (test== (send (make-tile "11" 0 0 0) roads) (list SOUTH WEST))
  
  (test== (send (make-tile "00" 0 0 0) castle?) #f)
  (test== (send (make-tile "3" 0 0 0) castle?) #t "3")
  (test== (send (make-tile "4" 0 0 0) castle?) #t)
  (test== (send (make-tile "5" 0 0 0) castle?) #t)
  (test== (send (make-tile "6" 0 0 0) castle?) #t)
  (test== (send (make-tile "7" 0 0 0) castle?) #t)
  (test== (send (make-tile "8" 0 0 0) castle?) #t)
  (test== (send (make-tile "9" 0 0 0) castle?) #t)
  (test== (send (make-tile "11" 0 0 0) castle?) #t "11")
  (test== (send (make-tile "12" 0 0 0) castle?) #t)
  (test== (send (make-tile "13" 0 0 0) castle?) #t)
  
  (test== (send (make-tile "4" 0 0 0) castles) (list NORTH EAST WEST))
  
  (test==
   (send (make-tile "00" 0 0 0) follower-places)
   (list NORTH EAST)
   "followers on 0")
  
  (test==
   (send (make-tile "1" 0 0 0) follower-places)
   (list INNER)
   "followers on 1")
  
  (test==
   (send (make-tile "2" 0 0 0) follower-places)
   (list INNER SOUTH)
   "followers on 2")
  
  (test==
   (send (make-tile "3" 0 0 0) follower-places)
   (list INNER)
   "followers on 3")
  
  (test==
   (send (make-tile "4" 0 0 0) follower-places)
   (list INNER)
   "followers on 4")
  
  (test==
   (send (make-tile "5" 0 0 0) follower-places)
   (list INNER)
   "followers on 5")
  
  (test==
   (send (make-tile "6" 0 0 0) follower-places)
   (list INNER SOUTH)
   "followers on 6")
  
  (test==
   (send (make-tile "7" 0 0 0) follower-places)
   (list INNER SOUTH)
   "followers on 7")
  
  (test==
   (send (make-tile "8" 0 0 0) follower-places)
   (list INNER)
   "followers on 8")
  
  (test==
   (send (make-tile "9" 0 0 0) follower-places)
   (list INNER)
   "followers on 9")
  
  (test==
   (send (make-tile "10" 0 0 0) follower-places)
   (list NORTH SOUTH)
   "followers on 10")
  
  (test==
   (send (make-tile "11" 0 0 0) follower-places)
   (list INNER SOUTH)
   "followers on 11")
  
  (test==
   (send (make-tile "12" 0 0 0) follower-places)
   (list INNER)
   "followers on 12")
  
  (test==
   (send (make-tile "13" 0 0 0) follower-places)
   (list INNER)
   "followers on 13")
  
  (test==
   (send (make-tile "14" 0 0 0) follower-places)
   (list NORTH EAST)
   "followers on 14")
  
  (test==
   (send (make-tile "15" 0 0 0) follower-places)
   (list NORTH SOUTH)
   "followers on 15")
  
  (test==
   (send (make-tile "16" 0 0 0) follower-places)
   (list NORTH)
   "followers on 16")
  
  (test==
   (send (make-tile "17" 0 0 0) follower-places)
   (list NORTH EAST)
   "followers on 17")
  
  (test==
   (send (make-tile "18" 0 0 0) follower-places)
   (list NORTH SOUTH)
   "followers on 18")
  
  (test==
   (send (make-tile "19" 0 0 0) follower-places)
   (list NORTH EAST SOUTH WEST)
   "followers on 19")
  
  (test==
   (send (make-tile "20" 0 0 0) follower-places)
   (list NORTH EAST)
   "followers on 20")
  
  (test==
   (send (make-tile "21" 0 0 0) follower-places)
   (list NORTH)
   "followers on 21")
  
  (test==
   (send (make-tile "22" 0 0 0) follower-places)
   (list EAST)
   "followers on 22")
  
  (test==
   (send (make-tile "23" 0 0 0) follower-places)
   (list EAST SOUTH WEST)
   "followers on 23")
  
  (test==
   (send (make-tile "24" 0 0 0) follower-places)
   (list NORTH EAST SOUTH WEST)
   "followers on 24")
  
  (define t 
    (let* ([t (make-tile "24" 0 0 0)]
           [t (send t place-follower NORTH RED)]
           [t (send t place-follower EAST  BLACK)]
           [t (send t place-follower SOUTH BLUE)]
           [t (send t place-follower WEST  WHITE)])
      t))
  
  (printf ">>> ~s~n" (send t snip))
  
  (define t2 (send (make-tile "1" 0 0 0) place-follower INNER RED))
  
  (printf ">>> ~s~n" (send t2 snip))

  (test== (send t follower@ NORTH) `(,RED) "follower north")
  (test== (send t follower@ SOUTH) `(,BLUE) "follower south")
  (test== (send (make-tile "24" 0 0 0) follower@ NORTH) '())
  
  (send t remove-follower NORTH)
  (send t remove-follower SOUTH)
  
  (test== (send t follower@ NORTH) '() "follower north removed")
  (test== (send t follower@ EAST) `(,BLACK) "follower east remains")
  
  (printf ">>> ~s~n" (send t snip))
  
  (printf "done ... ~n")
  )
