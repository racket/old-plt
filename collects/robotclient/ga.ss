(require (lib "list.ss" "mzlib"))
(define GREATEST-POSSIBLE-INTEGER 10000)
(define LOWEST-POSSIBLE-INTEGER  -10000)

(define print-every-n-generations 2)
(define run-n-generations 10)
(define output-port (current-output-port))
(define boards '((3 "board1" "packages1")
		 (3 "board2" "packages1")
		 (5 "board3" "packages2")
		 (4 "board4" "packages2")
		 (10 "board5" "packages3")))
(define initial-set
  `((0 0 0 0 0 0 0 0 0 0 0 0)
    (1 1 1 1 1 1 1 1 1 1 1 1)
    (2 2 2 2 2 2 2 2 2 2 2 2)))

;; mutation-factor : -> number
;; Randomly determines how much to multiply the random mutation
;; value by for a particular gene
(define (mutation-factor)
  (let ([num (random 100)])
    (cond
      [(< num 75) 0]
      [(< num 85) 1]
      [(< num 95) 10]
      [else 100])))

;; breed : gene-seq gene-seq -> gene-seq
(define (breed gs1 gs2)
  (let ([base (map (lambda (x y) (ceiling (/ (+ x y) 2))) gs1 gs2)])
    (map (lambda (x)
	   (let* ([mutation (* (- (random 20) 10) (mutation-factor))]
		  [newval (+ x mutation)])
	     (if (or (>= newval GREATEST-POSSIBLE-INTEGER)
		     (<= newval LOWEST-POSSIBLE-INTEGER))
		 x newval)))
      base)))

;; n-copies : T -> list-of-T
(define (n-copies n x)
  (cond
    [(= 0 n) '()]
    [else (cons x (n-copies (sub1 n) x))]))

;; spawn-next-generation : list-of-gene-seqs -> list-of-gene-seqs
(define (spawn-next-generation bestls)
  (let ([base-set (apply append (map (lambda (x) (n-copies 3 x)) bestls))])
    (let loop ((ls base-set) (acc '()))
      (cond
	[(null? (cdr ls)) (append bestls acc)]
	[else (loop (cdr ls)
		(append (map (lambda (x) (breed (car x) (cdr x)))
			  (map (lambda (x) (cons (car ls) x)) (cdr ls)))
		        acc))]))))

;; play-board : vector(num) string string vector(gene-seq) num -> void
(define (play-board scoreboard board packages player-vec players)
  (let ([board-file (string-append "boards/" board)]
	[packages-file (string-append "boards/" packages)])
    ;; play the board, get the results in 'results'
    (for-each (lambda (player score)
		(vector-set! scoreboard player
		  (+ (vector-ref scoreboard player) score)))
      players results)))

;; generate-results : list-of-gene-seqs -> list-of-gene-seqs
(define (generate-results ls boards)
  (let ([scoreboard (make-vector (length ls) 0)]
	[players (list->vector ls)]
	[num-players (length ls)])
    (let board-loop ((board-ls boards))
      (cond
	[(null? board-ls) (map cons (vector->list scoreboard)
			    (vector->list players))]
	[else (let tourny-loop ([i 0])
		(cond
		  [(= i num-players) (board-loop (cdr board-ls))]
		  [else (let ([need-players (sub1 (caar board-ls))]
			      [board-file (cadar board-ls)]
			      [packages-file (caddar board-ls)])
			  (let other-players ([player i] [acc '()])
			    (cond
			      [(= (length acc) need-players)
			       (play-board scoreboard board-file
				 packages-file players acc)
			       (tourny-loop (add1 i))]
			      [else (other-players
				      (modulo (add1 i) num-players)
				      (cons i acc))])))]))]))))
  
;; chop-list! : num list -> void
(define (chop-list! n ls)
  (let loop ((workls ls) (n (sub1 n)))
    (cond
      ((= n 0) (set-cdr! workls '()))
      (else (loop (cdr workls) (sub1 n))))))

;; run-generation : list-of-gene-seqs -> list-of-gene-seqs
(define (run-generation ls print)
  (let* ([results (generate-results ls)]
	 [results (mergesort results (lambda (x y) (> (car x) (car y))))])
    (chop-list! 3 results)
    (when (output-port? print)
      (fprintf print "(Score ~a) ~a~n" (caar results) (cdar results))
      (fprintf print "(Score ~a) ~a~n" (caadr results) (cdadr results))
      (fprintf print "(Score ~a) ~a~n" (caaddr results) (cdaddr results))
      (flush-output print))
    (spawn-next-generation (map cdr results))))

;; run-genetic-algorithm : list-of-gene-seqs output-port num num -> void
(define (run-genetic-algorithm initial-set output-port modnum endat)
  (let loop ((i 0) (group (spawn-next-generation initial-set)))
    (cond
      [(= i endat)
       (fprintf output-port "---------------FINAL--------------------------~n")
       (run-generation group output-port)]
      [(= (modulo i modnum) 0)
       (fprintf output-port "----------Generation ~a------------------~n" i)
       (loop (add1 i) (run-generation group output-port))]
      [else (loop (add1 i) (run-generation group #f))]))
  (void))

(run-genetic-algorithm
  initial-set
  output-port
  print-every-n-generations
  run-n-generations)

    