(module client-parameters mzscheme

  (provide (all-defined))
	
  (define score (make-parameter 0))

  (define home-list (make-parameter null))  

  (define board-height (make-parameter 0))
  (define board-width (make-parameter 0))
  (define board (make-parameter #f))

  (define player-id (make-parameter 0))
  (define player-money (make-parameter 0))
  (define player-initial-money (make-parameter 0))
  (define player-capacity (make-parameter 0))
  (define packages-held (make-parameter null))
  
  (define gui (make-parameter #f))
  
  (define path-loc (make-parameter (cons 0 0)))

  (define (init-parameters)
    (score 0)
    (home-list null)
    (board-height 0)
    (board-width 0)
    (board #f)
    (player-id 0)
    (player-money 0)
    (player-initial-money 0)
    (packages-held null)
    (gui #f)
    (path-loc (cons 0 0))
    ))