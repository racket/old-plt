  (unit/sig mzlib:print-convert-hooks^
    (import)

    (define before-test? (lambda (expr) (is-a? expr wx:image-snip%)))
    (define before-convert (lambda (expr recur) expr))
    
    (define build-share-hook (lambda (x b) 'no-share))
    (define build-share-name-hook  (lambda (x) #f))
    (define print-convert-hook (lambda (x p) x)))
