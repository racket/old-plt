
(define-signature cards:classes^
  (pasteboard%
   table%))

(define-signature cards:card-class^
  (card%))

(define-signature cards:make-cards^
  (back
   deck-of-cards))

(define-signature cards:main^
  (make-table        ; -> table
   make-deck))       ; -> (list-of cards)
   
(define-signature cards:constants^
  (ANIMATION-STEPS
   ANIMATION-TIME

   PRETTY-CARD-SEP-AMOUNT

   red-brush
   nice-font))

(define-signature cards:util^
  (shuffle-list)) ; (list-of X) int -> (list-of X)

(define-signature cards:snipclass^
  (sc))

(define-signature cards:region^
  ((struct region (x y w h label callback) -setters)
   set-region-callback!
   make-button-region))

(define-signature cards:region-local^
  ((struct region (x y w h label callback button? hilite? decided-start? can-select?))
   make-button-region))

(define-signature cards^
  ((open cards:main^)
   (open cards:util^)
   (open cards:region^)))
