(module widgets mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (provide commit-text-field% tabbed-panel%)
  
  (define commit-text-field%
    (class text-field% 
      
      (rename (super-set-value set-value))
      (override)
      (inherit get-value)
      
      (public get-committed-value)
      
      (init-field convert)
      (init label parent callback (init-value ""))
      
      
      (define committed-string init-value)
      (define committed-value 
        (let ((v (convert committed-string)))
          (cond
            (v v)
            (else
             (set! committed-string "")
             #f))))
      (define (get-committed-value) committed-value)
      
      (define user-callback callback)
      
;      (define/override (set-value v)
;        (super-set-value v)
;        (try-commit))
      
      (define (cb tf ce)
        (if (eq? 'text-field-enter (send ce get-event-type))
            (try-commit)))
      
      (define (try-commit)
        (let ((v (convert (get-value))))
          (cond
            (v
             (set! committed-value v)
             (set! committed-string (get-value))
             (user-callback v)))))
      
      (super-instantiate (label parent cb committed-string))))
  
  (define window-button%
    (class button%
      (init-field window)
      (define/public (get-window) window)
      (super-instantiate ())))
  
  
  (define tabbed-panel%
    (class vertical-panel%
      (super-instantiate ())
      
      (public close-current set-button-label)
      
      (inherit add-child delete-child change-children)
      
      (define lock #t)
      (define tab-bar (instantiate horizontal-panel% (this)
                        (stretchable-height #f)))
      (delete-child tab-bar)
      (define current-child #f)
      (define current-button #f)
      (define num-children 0)
      (define button-table (make-hash-table 'weak))
      (set! lock #f)
      
      (define (set-current-child! child)
        (if current-child (delete-child current-child))
        (set! current-child child)
        (send child activate))
      
      (define (set-current-button! b)
        (if current-button (send current-button enable #t))
        (set! current-button b)
        (send current-button enable #f))
      
      (define (make-button-callback child)
        (lambda (a b)
          (set-current-child! child)
          (set-current-button! a)
          (add-child current-child)))

      (define/override (after-new-child child)
        (cond
          ((not lock)
           (set-current-child! child)
           (if (= num-children 1) 
               (begin
                 (add-child tab-bar)
                 (change-children (lambda (c)
                                    (cons (cadr c) (cons (car c) null))))))
           (let ((b (make-object window-button%
                      child "tab button" tab-bar (make-button-callback child))))
             (set-current-button! b)
             (hash-table-put! button-table child b))
           (set! num-children (add1 num-children)))))
      
      (define (set-button-label c l)
        (let* ((b (hash-table-get button-table c))
               (new-b (make-object window-button% 
                        (send b get-window) l tab-bar 
                        (make-button-callback (send b get-window)))))
          (hash-table-put! button-table c new-b)
          (if (eq? current-button b)
              (begin
                (set! current-button new-b)
                (send new-b enable #f)))
          (send tab-bar change-children
                (lambda (c)
                  (let loop ((buttons c))
                    (cond
                      ((null? buttons) null)
                      ((eq? (car buttons) b) (cons new-b (loop (cdr buttons))))
                      ((eq? (car buttons) new-b) (loop (cdr buttons)))
                      (else (cons (car buttons) (loop (cdr buttons))))))))))
              
      
      (define (close-current)
        (cond
          ((> num-children 0)
           (if (= 2 num-children)
               (delete-child tab-bar))
           (send tab-bar delete-child current-button)
           (cond
             ((= 1 num-children)
              (set! current-button #f)
              (delete-child current-child)
              (set! current-child #f))
             (else
              (set-current-button! (car (send tab-bar get-children)))
              (send current-button command
                    (make-object control-event% 'button (current-milliseconds)))))
           (set! num-children (sub1 num-children)))))
        
      
      
      ))
      
)