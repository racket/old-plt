(module client-parameters mzscheme
  ;; thread variables

  (provide (all-defined))

  ;; The client's current score
  (define score (make-parameter 0))

  ;; The list of home bases
  ;; home-list: (cons int int) list
  (define home-list (make-parameter null))  
  
  ;; The board - see board.ss for board representation
  (define board-height (make-parameter 0))
  (define board-width (make-parameter 0))
  (define board (make-parameter #f))

  ;; Information about the client robot
  (define player-id (make-parameter 0))
  (define player-money (make-parameter 0))
  (define player-initial-money (make-parameter 0))
  (define player-capacity (make-parameter 0))
  (define packages-held (make-parameter null))
  
  ;; A gui object or #f for no gui (see simple-client.ss)
  (define gui (make-parameter #f))
  
  ;; The next location the robot should go to
  ;; according to the baseline client
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