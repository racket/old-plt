(unit/sig help:help^
  (import help:search^)

  (define re:html (regexp "([.*]plt/collects/doc/)(.*)"))

  (define (add-doc-section name ckey)
    (void))
  (define (add-kind-section name ckey)
    (void))

  (define (build-url page label)
    (cond
     [(regexp-match re:html page)
      =>
      (lambda (m)
	(let ([fst (cadr m)]
	      [snd (caddr m)])
	  (format "http://www.cs.rice.edu/CS/PLT/unreleased/~a#~a" snd label)))]
     [else (format "file:/~a" page)]))

  (define (add-choice key name title page label ckey)
    (printf "<a href=\"~a\">~a</a> in ~a~n"
	    (build-url page label)
	    name
	    title))

  (define (output . x)
    (let ([s (apply format x)])
      (display s)
      (newline)))

  (define regexp? #f)
  (define exact? #f)
  (define search-level 1)
  (define given-find "set-alignment")

  (let/ec k
    (do-search
     given-find
     search-level
     regexp?
     exact?
     (gensym)
     (lambda ()
       (output "(maximum searches reached)")
       (k (void))))))