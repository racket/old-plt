#!/bin/sh
string=? ; exec ${PLTHOME}/bin/mzscheme -mgrq $0 "$@"

;; See also gettitle.ss, which contains regexps that are sensitive to
;; the precise output of tex2page.

(require (lib "cmdline.ss")
	 (lib "list.ss")
	 (lib "string.ss")
	 (lib "xml.ss" "xml")
	 (lib "html.ss" "html")
	 (lib "pretty.ss"))

(define dir-to-read #f)
(define doc-to-read #f)
(define split-out-keywords? #f)

(command-line
 "makekeywords.ss" argv
 (once-each
  [("--keywords") "Extract R5RS keywords"
   (set! split-out-keywords? #t)]
  [("--docname") name "Document name (when not the same as the directory)"
   (set! doc-to-read name)])
 (args (dir)
       (set! dir-to-read dir)))

(unless doc-to-read
  (set! doc-to-read dir-to-read))

(define re:iname (regexp "<a href=\"([-a-z25]*-Z-H-[0-9]*[.]html)#node_index_start\">index</a>"))
(define re:ientry (regexp "^((&nbsp;)*)(<a href.*</a>)<br>$"))
(define re:ilink (regexp "(.*)<a href=\"(mzscheme-Z-H-[0-9]*[.]html)#(node_idx_[0-9]*)\">(.*)</a>"))
(define re:isubentry-start (regexp "^<br>$"))
(define re:ientry-top (regexp "<DT>(.*)<DD><DL>(.*)"))
(define re:isubentry-end (regexp "^</DL>"))
(define re:isee (regexp "^<DT>(.*)<DD>see (.*)"))

(use-html-spec #f)

(define index-file 
  (with-input-from-file (build-path dir-to-read (format "~a.html" doc-to-read))
    (lambda ()
      (let loop ()
	(let ([r (read-line)])
	  (cond
	   [(eof-object? r) #f]
	   [(regexp-match re:iname r)
	    =>
	    (lambda (m) (cadr m))]
	   [else (loop)]))))))

(unless index-file
  (error 'makehdindex 
	 "Could not discover the HTML index file for ~a by reading ~a.html" 
	 dir-to-read doc-to-read))

(define doc (map
	     xml->xexpr 
	     (with-input-from-file (build-path dir-to-read index-file) 
	       (lambda () (read-html-as-xml)))))

;; dive into 'html, then 'body:
(define (go-in tag)
  (set! doc
	(ormap (lambda (i)
		 (and (pair? i)
		      (eq? (car i) tag)
		      (cddr i)))
	       doc)))
(go-in 'html)
(go-in 'body)

;; flatten paragraphs:
(set! doc
      (let loop ([d doc])
	(if (null? d)
	    null
	    (if (and (pair? (car d))
		     (eq? 'p (caar d)))
		(loop (append (cddar d) (cdr d)))
		(cons (car d) (loop (cdr d)))))))

;; lift <br>
(set! doc
      (let loop ([d doc])
	(if (null? d)
	    null
	    (if (pair? (car d))
		(if (eq? 'br (caar d))
		    (cons '(br ()) (loop (append (cddar d) (cdr d))))
		    (cons (list* (caar d) (cadar d) (loop (cddar d)))
			  (loop (cdr d))))
		(cons (car d) (loop (cdr d)))))))

;; Skip to "node_index_start" anchor:
(set! doc
      (let loop ([d doc])
	(if (null? d)
	    (error 'makehdindex "index start not found on index page ~s"
		   index-file)
	    (if (and (pair? (car d))
		     (eq? 'a (caar d))
		     (equal? '(name "node_index_start") (assq 'name (cadar d))))
		(cdr d)
		(loop (cdr d))))))

(define spaces (string #\space #\tab #\newline #\return))

(define whitespace?
  (let ([re (regexp (format "^[~a]*$" spaces))])
    (lambda (s) (regexp-match re s))))
  
(define re:pre-space (format "^[~a]*" spaces))
(define re:post-space (format "[~a]*$" spaces))
(define re:multi-space (format "[~a][~a]+" spaces spaces))
(define (extract-string n)
  (let ([n
	 (let loop ([n n])
	   (if (string? n)
	       n
	       (if (eq? n 'nbsp)
		   " "
		   (apply string-append (map loop (cddr n))))))])
    (regexp-replace 
     re:multi-space
     (regexp-replace 
      re:post-space
      (regexp-replace
       re:pre-space
       n
       "")
      "")
     " ")))

(define (build-prefix nesting)
  (string-append
   (if (null? nesting)
       ""
       (string-append
	(let loop ([l nesting])
	  (if (null? (cdr l))
	      (car l)
	      (string-append (loop (cdr l))
			     ", "
			     (car l))))
	", "))))

(define (nb-whitespace? x)
  (or (eq? x 'nbsp)
      (and (string? x) (whitespace? x))))

;; Parse each line in the index. An index entry is of the form
;;   (br ()) [whitespace|nbsp]* name ["," name]*
;; Every four nbsps indicate a level o nesting
;; Each name is a link when its of the form (a ((href ...)) ...)
;;   If there's more then one name, the extras are links,
;;   and they refer to the same name as earlier items:

;; rough-index is a list of (cons name link), where the
;;  link is "page%anchor"
(define rough-index null)

;; xrefs is a list of (cons name name)
(define xrefs null)

(let loop ([nestings (list "???")][d doc])
  (if (null? d)
      null
      (let ([l (car d)])
	(if (not (pair? l))
	    (loop nestings (cdr d))
	    (cond
	     [(eq? 'br (car l))
	      ;; Start of an entry
	      (let ([d (cdr d)])
		(let-values ([(d depth)
			      (let loop ([d d][n 0])
				(if (nb-whitespace? (car d))
				    (loop (cdr d) (add1 n))
				    (if (and (string? (car d))
					     (whitespace? (car d)))
					(loop (cdr d) n)
					(values d n))))])
		  (let* ([id (car d)]
			 [anchor? (and (pair? id)
				       (eq? 'a (car id)))]
			 [name (extract-string id)]
			 [nested (list-tail nestings 
					    (- (length nestings)
					       (quotient depth 4)))])
		    ;; Output links until we find a br:
		    (let xloop ([d d])
		      (unless (null? d)
			(let ([l (car d)])
			  (cond
			   [(and (pair? l) (eq? 'br (car l)))
			    ;; Done with this line
			    ;; Extend/replace nestings
			    (loop (cons name nested)
				  d)]
			   [(and (pair? l)
				 (eq? 'a (car l)))
			    ;; Found a link
			    (set! rough-index
				  (cons
				   (cons
				    (format "~a~a"
					    (build-prefix nested) name)
				    (cadr (assq 'href (cadr l))))
				   rough-index))
			    (xloop (cdr d))]
			   [(and (pair? l)
				 (eq? 'em (car l))
				 (equal? "see" (caddr l)))
			    ;; Found an X-ref
			    (let* ([d (let loop ([d (cdr d)])
					(if (and (string? (car d))
						 (whitespace? (car d)))
					    (loop (cdr d))
					    d))]
				   [target (extract-string (car d))])
			      (set! xrefs (cons
					   (cons (format "~a~a"
							 (build-prefix nested) 
							 ;; Drop trailing comma:
							 (substring name
								    0
								    (sub1 (string-length name))))
						 target)
					   xrefs))
			      (xloop (cdr d)))]
			   [else
			    (xloop (cdr d))])))))))]
	     [else
	      (loop nestings (cdr d))])))))

(fprintf (current-error-port) "  Copying `see' entries~n")

; Copy entries for each "see" xref
(define full-rough-index
  (apply
   append
   rough-index
   (map
    (lambda (see)
      (let* ([dest (car see)]
	     [src (cdr see)]
	     [re (regexp (string-append "^" (regexp-quote src) "(|,.*)$"))])
	(let loop ([l rough-index])
	  (cond
	   [(null? l) null]
	   [(regexp-match re (caar l))
	    => (lambda (m)
		 (cons (cons (string-append dest (cadr m))
			     (cdar l))
		       (loop (cdr l))))]
	   [else (loop (cdr l))]))))
    xrefs)))

(define re:split (regexp "^([^#]*)#(.*)$"))

;; Split HTML page and anchor name:
(define full-index-without-titles
  (map (lambda (i)
	 (let ([m (regexp-match re:split (cdr i))])
	   (list (car i)
		 (cadr m)
		 (caddr m))))
       full-rough-index))

(load-relative "gettitle.ss")

(fprintf (current-error-port) "  Getting titles~n")

;; Get title in each case:
(define full-index
  (map (lambda (i)
	 (list (car i)
	       (cadr i)
	       (caddr i)
	       (clean-up (get-title (cadr i) (caddr i)))))
       full-index-without-titles))

;; If split-out-keywords?, then figure out which index items
;; should actually be keyword entries:
(when split-out-keywords?
  (fprintf (current-error-port) "  Finding keywords~n")
  (with-output-to-file (build-path dir-to-read "keywords")
    (lambda ()
      (printf "(~n")
      (for-each (lambda (i)
		  (let ([file (cadr i)]
			[anchor (caddr i)])
		    (let ([re (regexp (format "(procedure:).*([(]<a name=\"~a\">[^)]*[)])"
					      (regexp-quote anchor)))])
		    (with-input-from-file (build-path dir-to-read file)
		      (lambda ()
			(let loop ()
			  (let ([l (read-line)])
			    (unless (eof-object? l)
			      (let ([m (regexp-match re l)])
				(if m
				    ;; keyword entry:
				    (printf "(~s ~s ~s ~s ~s)~n"
					    (car i)
					    (clean-up (caddr m))
					    (cadr i)
					    (caddr i)
					    (cadddr i))
				    (loop)))))))))))
		full-index)
      (printf ")~n"))
    'truncate/replace))
		

(fprintf (current-error-port) "  Filtering keywords~n")

; Remove anything already covered as a keyword (i.e., ref to same section)
(define keywords (with-handlers ([void (lambda (x) null)])
		   (with-input-from-file (build-path dir-to-read "keywords") read)))
(define ht (make-hash-table))
(for-each
 (lambda (k)
   (let* ([s (string->symbol (car k))]
	  [l (hash-table-get ht s (lambda () null))])
     (hash-table-put! ht s (cons (list-ref k 4) l))))
 keywords)
(define filtered-index
  (filter
   (lambda (i)
     (not (member (list-ref i 3)
		  (hash-table-get ht 
				  (string->symbol (car i)) 
				  (lambda () null)))))
   full-index))

(with-output-to-file (build-path dir-to-read "hdindex")
  (lambda ()
    (printf "(~n")
    (for-each
     (lambda (s)
       (printf "~s~n" s))
     ; Resort because we may have added extra entries
     (quicksort filtered-index
		(lambda (a b)
		  (string-ci<? (car a) (car b)))))
    (printf ")~n"))
  'truncate)
