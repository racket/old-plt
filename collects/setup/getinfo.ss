
(module getinfo mzscheme

  (require (lib "match.ss")
           (lib "list.ss"))

  (provide get-info
           get-info/full
	   find-relevant-collections)

  (define (get-info coll-path)
    (let* ([coll-path (map (lambda (x) (if (path? x) (path->string x) x)) coll-path)]
	   [dir (apply collection-path coll-path)])
      (get-info/full dir)))
  
  (define (get-info/full dir)
    (let ([file (build-path dir "info.ss")])
      (if (file-exists? file)
	  (begin
	    (with-input-from-file file
	      (lambda ()
		(let ([r (read)])
		  (unless (eof-object? (read))
		    (error "info.ss file has multiple expressions in ~a" dir))
		  (match r
		    [('module 'info '(lib "infotab.ss" "setup")
		       expr ...)
		     'ok]
		    [else (error 
			   'get-info
			   "info file does not contain a module of the right shape: \"~a\""
			   file)]))))
	    (dynamic-require file '#%info-lookup))
	  #f)))

  (define relevant-ht #f)
  (define relevant-collection-paths #f)

  (define (populate-relevant-table!)
    ;; Use the colls ht because a collection might be in multiple
    ;;  collection paths, and we only want one
    (let ([colls (make-hash-table 'equal)])
      (for-each (lambda (p)
		  (let ([f (build-path p "info-domain" "compiled" "cache.ss")])
		    (when (file-exists? f)
		      (for-each (lambda (i)
				  (unless (and (list? i)
					       (= (length i) 2)
					       (list? (car i))
					       (andmap path-string? (car i))
					       (list? (cadr i))
					       (andmap symbol? (cadr i)))
				    (error 'find-relevant-collections
					   "bad info-domain cache entry: ~e in: ~a" 
					   i
					   f))
				  (hash-table-put! colls (car i) (cadr i)))
				(let ([l (with-input-from-file f read)])
				  (unless (list? l)
				    (error 'find-relevant-collections
					   "bad info-domain cache file: ~a" 
					   f))
				  l)))))
		(reverse relevant-collection-paths))
      ;; For each coll, invert the mapping, adding the col name to the list for each sym: 
      (hash-table-for-each colls
			   (lambda (coll syms)
			     (for-each (lambda (sym)
					 (hash-table-put! 
					  relevant-ht
					  sym
					  (cons coll (hash-table-get relevant-ht sym (lambda () null)))))
				       syms)))))

  (define (find-relevant-collections syms)
    (unless (equal? relevant-collection-paths
		    (current-library-collection-paths))
      (set! relevant-ht (make-hash-table))
      (set! relevant-collection-paths (current-library-collection-paths))
      (populate-relevant-table!))
    (let ([unsorted (if (= (length syms) 1)
                        ;; Simple case: look up in table
                        (hash-table-get relevant-ht (car syms) (lambda () null))
                        ;; Use a hash table, because the same collection might work for multiple syms
                        (let ([result (make-hash-table 'equal)])
                          (for-each (lambda (sym)
                                      (let ([l (hash-table-get relevant-ht sym (lambda () null))])
                                        (for-each (lambda (c) (hash-table-put! result c #t))
                                                  l)))
                                    syms)
                          ;; Extract the relevant collections:
                          (hash-table-map result (lambda (k v) k))))])
      (quicksort unsorted compare-collection-lists)))
  
  (define (compare-collection-lists a b)
    (let loop ([a a]
               [b b])
      (cond
        [(and (null? a) (null? b)) #t]
        [(null? a) #t]
        [(null? b) #f]
        [else (cond
                [(string=? (car a) (car b))
                 (loop (cdr a) (cdr b))]
                [else (string<=? (car a) (car b))])]))))
