(module board mzscheme
  
  (require "array2d.ss"
	   "client-parameters.ss"
           (lib "class.ss")
           (lib "list.ss")
           (lib "lex.ss" "parser-tools")
           (lib "yacc.ss" "parser-tools"))

  (provide get-type get-robot get-valid set-valid set-invalid get-weight set-weight
           get-spot set-spot get-player-x get-player-y
           (struct command (bid command arg))
           (struct package (id x y weight))
	   package->string
           (struct robot (id x y))
	   num-robots
           read-board! read-response!
           fix-home!)
           
  ;; empty = 0
  ;; water = 1
  ;; wall = 2
  ;; home = 3
  
  ;; The board is a vector of fixnums with this it pattern
  ;; 1 2 1 25
  ;; robot? type valid? weight

  (define-syntax get-type
    (syntax-rules ()
      ((_ n)
       (bitwise-and #b11 (arithmetic-shift n -26)))))
  
  (define-syntax set-empty
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b10011111111111111111111111111))))
  
  (define-syntax set-water
    (syntax-rules ()
      ((_ n)
       (bitwise-ior
        (bitwise-and n #b10011111111111111111111111111)
        #b00100000000000000000000000000))))
  
  (define-syntax set-wall
    (syntax-rules ()
      ((_ n)
       (bitwise-ior
        (bitwise-and n #b10011111111111111111111111111)
        #b01000000000000000000000000000))))
  
  (define-syntax set-home
    (syntax-rules ()
      ((_ n)
       (bitwise-ior n #b01100000000000000000000000000))))
  
  (define-syntax get-robot
    (syntax-rules ()
      ((_ n)
       (arithmetic-shift n -28))))
  

  (define-syntax set-robot
    (syntax-rules ()
      ((_ n)
       (bitwise-ior n #b10000000000000000000000000000))))

  (define-syntax set-robot-empty
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b01111111111111111111111111111))))
      
  (define-syntax get-valid
    (syntax-rules ()
      ((_ n)
       (bitwise-and #b1 (arithmetic-shift n -25)))))
  
  (define-syntax set-valid
    (syntax-rules ()
      ((_ n)
       (bitwise-ior n #b00010000000000000000000000000))))
  
  (define-syntax set-invalid
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b11101111111111111111111111111))))
  
  (define-syntax get-weight
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b00001111111111111111111111111))))
  
  (define-syntax set-weight
    (syntax-rules ()
      ((_ n w)
       (bitwise-ior (bitwise-and n #b11110000000000000000000000000)
                    w))))

  (define-syntax get-spot
    (syntax-rules ()
      ((_ board x y)
       (let ((x1 x)
             (y1 y))
         (cond
           ((and (> x1 0) (> y1 0) (<= x1 (board-width)) (<= y1 (board-height)))
            (array2d-ref board (sub1 y1) (sub1 x1)))
           (else
            #b01000000000000000000000000000))))))
          
  (define-syntax set-spot
    (syntax-rules ()
      ((_ board x y new-val)
       (let ((x1 x)
             (y1 y))
         (cond
           ((and (> x1 0) (> y1 0) (<= x1 (board-width)) (<= y1 (board-height)))
            (array2d-set! board (sub1 y1) (sub1 x1) new-val)))))))
  
  (define-syntax get-player-x
    (syntax-rules ()
      ((_) (robot-x (hash-table-get (robot-table) (player-id))))))

  (define-syntax get-player-y
    (syntax-rules ()
      ((_) (robot-y (hash-table-get (robot-table) (player-id))))))
  
  
  ;; (make-command num `(s n e w p d) list-of-package)
  (define-struct command (bid command arg) (make-inspector))
  
  ;; (make-package num num num num)
  (define-struct package (id x y weight) (make-inspector))

  (define (package->string p)
    (format "~a (~a, ~a) - weight ~a"
	    (package-id p)
	    (package-x p)
	    (package-y p)
	    (package-weight p)))

  ;; (make-robot num num num)
  (define-struct robot (id x y))

  ;; robot-table: (num robot hash-table)
  (define robot-table (make-parameter #f))

  ;; robot-indexes: num list
  (define robot-indexes (make-parameter null))
  (define (num-robots) (length (robot-indexes)))

  (define-struct response (id name arg) (make-inspector))
  
  (define-tokens rt (NUM))
  (define-empty-tokens ert (^ X Y N S E W P D EOF Robot died.))
  
  (define response-parser
    (parser
     (tokens rt ert)
     (start start)
     (end EOF)
     (error (lambda (a b c) (error 'parser)))
     (grammar
      (start ((death) $1)
	     ((response-list) $1))
      (death ((Robot NUM died.) (raise 'dead-robot)))
      (response ((^ NUM com-list) 
                 (map (lambda (com)
                        (make-response $2 (car com) (cdr com)))
                      $3))
                ((^ NUM) (list (make-response $2 'nothing 'nothing))))
      (response-list ((response) (list $1))
                     ((response response-list) (cons $1 $2)))
      (com ((X NUM Y NUM) (cons 'X (cons $2 $4)))
           ((N) (cons 'N null))
           ((S) (cons 'S null))
           ((E) (cons 'E null))
           ((W) (cons 'W null))
           ((P NUM) (cons 'P $2))
           ((D NUM) (cons 'D $2)))
      (com-list ((com) (list $1))
                ((com com-list) (cons $1 $2))))))
       
  
  (define (read-response in)
    (let* ((t (regexp-replace* "#" (read-line in) " ^ "))
           (s (open-input-string t)))
      (with-handlers ((exn:user? (lambda (ex)
                                   (printf "bad-string ~a~n" t)
                                   (raise ex))))
        (response-parser (lambda ()
                           (let ((i (read s)))
                             (cond
                               ((memq i `(^ X Y N S E W P D Robot died.)) i)
                               ((number? i) (token-NUM i))
                               (else 'EOF))))))))
          
  (define (read-initial-response! in)
    (let* ((responses
            (filter (lambda (x)
                      (eq? 'X (response-name x)))
                    (apply append (read-response in))))
           (robots (map (lambda (response)
                          (make-robot (response-id response)
                                      (car (response-arg response))
                                      (cdr (response-arg response))))
                        responses)))
      (for-each (lambda (robot)
                  (let ((x (robot-x robot))
                        (y (robot-y robot)))
                    (set-spot (board) x y (set-robot (get-spot (board) x y))))
                  (hash-table-put! (robot-table) (robot-id robot) robot))
                robots)
      (robot-indexes (remove-dups (map robot-id robots)))))
    
  (define (remove-dups loi)
    (let loop ((l (mergesort loi >))
               (r null))
      (cond
        ((null? l) r)
        ((null? r) (loop (cdr l) (list (car l))))
        ((= (car l) (car r)) (loop (cdr l) r))
        (else
         (loop (cdr l) (cons (car l) r))))))


  ;; alive and all are sorted lists of integers (least to greatest)
  ;; return all integers in all that are not in alive
  (define (find-dead alive all)
    (cond
      ((null? alive) all)
      ((null? all) null)
      ((= (car alive) (car all))
       (find-dead (cdr alive) (cdr all)))
      ((< (car alive) (car all))
       (find-dead (cdr alive) all))
      (else
       (cons (car all)
             (find-dead alive (cdr all))))))
  
  
  (define (read-response! add-score packages in)
    (let* ((responses (read-response in))
           (flat-responses (apply append responses))
           (alive-robots (remove-dups (map (lambda (x) 
                                             (response-id (car x)))
                                           responses)))
           (dead-robots
            (find-dead alive-robots (robot-indexes)))
           (package-table (make-hash-table)))
      ;(printf "~a~n" responses)
	
      (for-each
       (lambda (p)
	 (hash-table-put! package-table
                          (package-id p)
                          p))
       packages)
      (for-each
       (lambda (id)
         (let* ((r (hash-table-get (robot-table) id))
                (x (robot-x r))
                (y (robot-y r)))
           (set-spot (board) x y (set-robot-empty (get-spot (board) x y)))
           (hash-table-remove! (robot-table) id)))
       dead-robots)

      (map
       (lambda (r)
	 (let* ((old-robot (hash-table-get (robot-table) (response-id r) (lambda () #f)))
                (old-robot (cond
                             (old-robot old-robot)
                             (else (make-robot (response-id r) 1 1))))
		(new-robot
		 (make-robot
		  (robot-id old-robot)
		  (robot-x old-robot)
		  (robot-y old-robot))))
	   (case (response-name r)
	     ((nothing) (void))
	     ((P)
	      (cond
	       ((= (response-id r) (player-id))
                (let ((p (hash-table-get package-table (response-arg r))))
                  (cond
                    ((gui) (send (gui) log (format "Picked up package: ~a" (package->string p)))))
                  (packages-held
                   (cons p (packages-held)))))))
	     ((D)
	      (cond
	       ((= (response-id r) (player-id))
		(let ((drop (response-arg r)))
		  (for-each (lambda (p)
			      (cond
			       ((and (= (package-id p) drop)
				     (= (get-player-x) (package-x p))
				     (= (get-player-y) (package-y p)))
				(cond
                                  ((gui) (send (gui) log
                                               (format "Dropped package: ~a" (package->string p)))))
                                (add-score (package-weight p)))))
			    (packages-held))
		  (packages-held
		   (filter (lambda (p)
			     (not (= drop (package-id p))))
			   (packages-held)))))))
	     ((N) (set-robot-y! new-robot (add1 (robot-y new-robot))))
	     ((S) (set-robot-y! new-robot (sub1 (robot-y new-robot))))
	     ((E) (set-robot-x! new-robot (add1 (robot-x new-robot))))
	     ((W) (set-robot-x! new-robot (sub1 (robot-x new-robot))))
	     ((X)
	      (set-robot-x! new-robot (car (response-arg r)))
	      (set-robot-y! new-robot (cdr (response-arg r)))))
	   (set-spot (board)
		     (robot-x old-robot)
		     (robot-y old-robot)
		     (set-robot-empty (get-spot (board)
						(robot-x old-robot)
						(robot-y old-robot))))
	   (set-spot (board)
		     (robot-x new-robot)
		     (robot-y new-robot)
		     (set-robot (get-spot (board)
					  (robot-x new-robot)
					  (robot-y new-robot))))
	   (hash-table-put! (robot-table) (robot-id new-robot) new-robot)))
       flat-responses)
      (robot-indexes alive-robots)
      (let ((robots (map (lambda (id) (hash-table-get (robot-table) id))
                         (robot-indexes))))
        (cond
          ((gui)
           (send (gui) set-robots 
                 (map (lambda (robot)
                        (list (robot-id robot)
                              (robot-x robot)
                              (robot-y robot)))
                      robots)
                 (player-id)
                 (player-money)
                 (score)
                 (packages-held))))
	(for-each (lambda (robot)
		    (cond
		     ((not (= 1 (get-robot (get-spot (board) 
                                                     (robot-x robot) (robot-y robot)))))
		      (printf "Robot at (~a,~a) not on board~n" 
                              (robot-x robot) (robot-y robot))
                      (printf "~a~n"
                              (get-robot (get-spot (board) 
						   (robot-x robot) (robot-y robot)))))))
		  robots)
        robots)))
  
  (define (read-good-char in)
    (let ((c (read-char in)))
      (case c
        ((#\. #\~ #\# #\@) c)
        (else (read-good-char in)))))

        
  (define (compute-gui-board b)
    (let loop ((i 1))
      (cond
        ((> i (board-height)) null)
        (else
         (cons
          (let ((s (make-string (board-width))))
            (let loop ((j 1))
              (cond
                ((> j (board-width)) s)
                (else
                 (string-set! s 
                              (sub1 j)
                              (case (get-type (get-spot b j i))
                                ((0) #\.)
                                ((1) #\~)
                                ((2) #\#)
                                ((3) #\@)))
                 (loop (add1 j))))))
          (loop (add1 i)))))))
  
  (define (read-board! input gui?)
    (robot-indexes null)
    (robot-table (make-hash-table))
    (board-width (read input))
    (board-height (read input))
    (board (make-array2d (board-height) (board-width) 0))
    (let loop ((i 1)
               (j 1))
      (cond
        ((> i (board-height)) (void))
        ((> j (board-width)) (loop (add1 i) 1))
        (else
         (let ((c (read-good-char input)))
           (set-spot (board) j i
                     (case c
                       ((#\~) (set-water 0))
                       ((#\#) (set-wall 0))
                       ((#\@) (begin (home-list (cons (cons j i) (home-list)))
                                     (set-home 0)))
                       (else 0))))
         (loop i (add1 j)))))
    (player-id (read input))
    (player-capacity (read input))
    (player-money (read input))
    (player-initial-money (player-money))
    (cond
      (gui? (gui (make-object (dynamic-require "simple-gui.ss" 'gui%)
                   (compute-gui-board (board)) (board-width) (board-height)))))
    (read-line input)
    (read-initial-response! input))
    
  (define (fix-home!)
    (let* ((px (get-player-x))
	   (py (get-player-y))
	   (spot (get-spot (board) px py)))
      (cond
       ((= 3 (get-type spot))
	(cond 
	 ((gui)
	  (send (gui) change-board px py #\.)))
	(home-list (remove-home px py (home-list)))
	(set-spot (board) px py (set-empty spot))))))
  
  (define (remove-home x y hl)
    (cond
     ((null? hl) null)
     ((and (= x (caar hl)) (= y (cdar hl)))
      (cdr hl))
     (else
      (cons (car hl) (remove-home x y (cdr hl))))))
  
  )