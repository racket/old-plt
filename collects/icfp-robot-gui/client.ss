
(module client mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "unit.ss")
           (lib "list.ss")
           "draw.ss"
           "io.ss")

  ;; ---------- Configuration -----------
  
  (define server-name "localhost")
  (define server-port 4004)
  
  ;; -----------------------------------
  
  (define-values (session board me robots packages)
    (connect server-name server-port))
  
  (define width (vector-length (vector-ref board 0)))
  (define height (vector-length board))
               
  (define f (instantiate frame% ("Robot")))
  (define drawn (instantiate board-panel% (f width height board)))
  
  (send drawn install-robots&packages robots packages)
  
  (define bottom (make-object horizontal-panel% f))
  
  (make-object message% (format "Robot ~a" me) bottom)
  
  (let ([v (instantiate vertical-panel% (bottom) [stretchable-height #f] [stretchable-width #f])])
    (make-object button% "North" v (lambda (b e) (make-move 'n)))
    (let ([h (make-object horizontal-panel% v)])
      (make-object button% "West" h (lambda (b e) (make-move 'w)))
      (make-object button% "East" h (lambda (b e) (make-move 'e))))
    (make-object button% "South" v (lambda (b e) (make-move 's))))
  
  (define (make-move m)
    (let-values ([(rs ps) (move 1 session m me robots packages)])
      (set! robots rs)
      (set! packages ps)
      (send drawn install-robots&packages rs ps)
      (list-available-packages)))
  
  (define pickup-panel (make-object vertical-panel% bottom))
  (send pickup-panel set-alignment 'left 'center)
  (define pickup-button (make-object button% "Pickup:" pickup-panel (lambda (b e)
                                                                      (do-list-action pickup-list 'pick))))
  (define pickup-list (make-object list-box% #f null pickup-panel 
                        (lambda (l e) (enable-list-buttons))
                        '(multiple)))
  
  (define drop-panel (make-object vertical-panel% bottom))
  (send drop-panel set-alignment 'left 'center)
  (define drop-button (make-object button% "Drop:" drop-panel (lambda (b e)
                                                                (do-list-action drop-list 'drop))))
  (define drop-list (make-object list-box% #f null drop-panel 
                      (lambda (l e) (enable-list-buttons))
                      '(multiple)))
  
  (define (list-available-packages)
    (send pickup-list clear)
    (send drop-list clear)
    (let ([r (assoc me robots)])
      (for-each (lambda (pack)
                  (send drop-list append (format "~a" pack)))
                (list-ref r 5))
      (for-each (lambda (pack)
                  (when (and (= (list-ref pack 1) (list-ref r 1))
                             (= (list-ref pack 2) (list-ref r 2))
                             (not (member (car pack) (list-ref r 5))))
                    (send pickup-list append (format "~a" (car pack)))))
                packages))
    (enable-list-buttons))
  
  (define (enable-list-buttons)
    (send pickup-button enable (pair? (send pickup-list get-selections)))
    (send drop-button enable (pair? (send drop-list get-selections))))
  
  (define (do-list-action list what)
    (make-move `(,what ,@(map (lambda (pos)
                                (string->number (send list get-string pos)))
                              (send list get-selections)))))
  
  (list-available-packages)
  
  (send f show #t))
