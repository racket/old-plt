(require-library "framework.ss" "framework")

(define drjava-frame%
  (class frame:text-info-file% (filename)
    (sequence (super-init (string-append (or filename "Untitled") "*")))
    (inherit get-canvas% get-area-container make-editor)
    (private
      (can% (get-canvas%))
      (ac (get-area-container)))
    (sequence
      (send (make-object can% ac) set-editor (make-editor)))))

;(define f (make-object drjava-frame% "Hi"))
;(define can% (send f get-canvas%))
;(define can (make-object can% (send f get-area-container)))
;(send can set-editor (send f make-editor))

;; open : String -> Void
(define (open name)
  (send (make-object drjava-frame% name) show #t))

(handler:insert-format-handler "drjava" void open)
(send (make-object drjava-frame% #f) show #t)