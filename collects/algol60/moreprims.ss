;; Primitives that aren't needed by the parser or
;; simplifier, and that tend to collide with MzScheme names
(module moreprims "cmzscheme.ss"
  (require (rename mzscheme mz:abs abs)
           (rename mzscheme mz:sqrt sqrt)
           (rename mzscheme mz:exp exp)
           (rename mzscheme mz:cos cos)
           (rename mzscheme mz:sin sin))
  (provide sin
           cos
           arctan
           sqrt
           abs
           ln
           exp)
  
  (define (abs k v)
    (k (mz:abs (v))))
  
  (define (sqrt k v)
    (k (mz:sqrt (v))))
  
  (define (sin k v)
    (k (mz:sin (v))))
  
  (define (cos k v)
    (k (mz:cos (v))))
  
  (define (exp k v)
    (k (mz:exp (v))))
  
  (define (arctan k v)
    (k (atan (v))))
  
  (define (ln k v)
    (k (log (v)))))