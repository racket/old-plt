(module data mzscheme
  
  (define (caddddr l)
    (caddr (cddr l)))
  (define (cadddddr l)
    (caddr (cdddr l)))
  
  (define (make-bot id x y money max-lift packages)
    (list id x y money max-lift packages))
  (define bot-id car)
  (define bot-x cadr)
  (define bot-y caddr)
  (define bot-money cadddr)
  (define bot-max-lift caddddr)
  (define bot-packages cadddddr)
  
  (define (make-pkg id x y dest-x dext-y weight)
    (list id x y dest-x dext-y weight))
  (define pkg-id car)
  (define pkg-x cadr)
  (define pkg-y caddr)
  (define pkg-dest-x cadddr)
  (define pkg-dest-y caddddr)
  (define pkg-weight cadddddr)
  
  ;; 0-based!
  (define (get-cell board i j)
    (vector-ref (vector-ref board j) i))
  (define (set-cell! board i j v)
    (vector-set! (vector-ref board j) i v))
  
  (provide (all-defined)))

  