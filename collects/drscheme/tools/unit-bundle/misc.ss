(unit/sig drscheme:bundle:misc^
  (import mred^)
  
  (define (set-box/f! b/f contents)
    (when (box? b/f)
      (set-box! b/f contents)))
  (define (set-dc-pen dc color width style)
    (send dc set-pen (send the-pen-list find-or-create-pen color width style)))
  (define (set-dc-brush dc color style)
    (send dc set-brush (send the-brush-list find-or-create-brush color style)))
  (define white (make-object color% "WHITE"))
  (define black (make-object color% "BLACK")))
