(module util mzscheme
  (require (lib "string.ss")
           (lib "url.ss" "net")
           )
  (provide (struct virtual-path (directory-part file-part))
           lowercase-symbol!
           url->virtual-path)

  (define-struct virtual-path (directory-part file-part) (make-inspector))
  
  ; lowercase-symbol! : (union string bytes) -> symbol
  (define (lowercase-symbol! s)
    (let ([s (if (bytes? s)
                 (bytes->string/utf-8 s)
                 s)])
      (string-lowercase! s)
      (string->symbol s)))


  ;; url->virtual-path: url -> (union #f string)
  ;; create a relative path from the url-path
  ;; the relative path will have no dots in it.
  (define (url->virtual-path a-url)
    (let build-virtual-path ([p (url-path a-url)])
      (cond
       [(null? p) (make-virtual-path '() "")]
       [(path/param? (car p))
        (build-virtual-path (cons (path/param-path (car p)) (cdr p)))]
       [(null? (cdr p))
        (let ([first (car p)])
          (cond
           [(string=? first ".") (make-virtual-path '() "")]
           [(string=? first "..") (make-virtual-path '("..") "")]
           [else
            (make-virtual-path '() first)]))]
       [else
        (let ([first (car p)]
              [rest (build-virtual-path (cdr p))])
          (cond
           [(or (string=? "" first)
                (string=? "." first)) rest]
           [(string=? ".." first)
            (make-virtual-path
             (cons first (virtual-path-directory-part rest))
             (virtual-path-file-part rest))]
           [(null? (virtual-path-directory-part rest))
            (make-virtual-path
             (cons first (virtual-path-directory-part rest))
             (virtual-path-file-part rest))]
           [(string=? ".." (car (virtual-path-directory-part rest)))
            (make-virtual-path
             (cdr (virtual-path-directory-part rest))
             (virtual-path-file-part rest))]
           [else
            (make-virtual-path
             (cons first (virtual-path-directory-part rest))
             (virtual-path-file-part rest))]))])))
  )
