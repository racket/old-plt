
(define-signature cards^
  (make-table        ; -> table
   make-deck         ; -> list of cards
   shuffle-list      ; list shuffle-count -> list
   (struct region (x y w h label callback) -setters)
   set-region-callback!
   make-button-region))
