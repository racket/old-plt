(module client-parameters mzscheme

  (provide (all-defined))
	
  (define score (make-parameter 0))

  (define board-height (make-parameter 0))
  (define board-width (make-parameter 0))
  (define board (make-parameter #f))

  (define player-id (make-parameter 0))
  (define player-money (make-parameter 0))
  (define player-initial-money (make-parameter 0))
  (define player-capacity (make-parameter 0))
  (define packages-held (make-parameter null))
  
  (define (init-parameters)
    (score 0)
    (board-height 0)
    (board-width 0)
    (board #f)
    (player-id 0)
    (player-money 0)
    (player-initial-capacity 0)
    (packages-held null)
    ))