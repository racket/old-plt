;; Pages and the transitions between them. A page is a function that produces
;; a xexpr/callback. The embedded procedures in these xexpr/callbacks are
;; transitions. A transition is a procedure that consumes a request and
;; produces a xexpr/callback; it does this by calling send/suspend/callback
;; on a page function.
(module pages-transitions mzscheme
  (require (lib "unitsig.ss")
           "pages.ss" "transitions.ss"
           "sigs.ss")

  (define-signature pages-transitions^ ((open transitions^) (open pages^)))

  (define-values/invoke-unit/sig
    pages-transitions^
    (compound-unit/sig
      (import)
      (link (T : transitions^ (transitions@ P))
            (P : pages^ (pages@ T)))
      (export (open P)
              (open T))))

  (provide-signature-elements pages-transitions^)

  )
