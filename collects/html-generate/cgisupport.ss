(require-library "html.ss" "html-generate")

(read-case-sensitive #t)

(define emit-page-to-file
  (lambda (page filename)
    (let ([port (open-output-file filename 'truncate/replace)])
      (raw-emit-html-top-level page port)
      (close-output-port port))))

(define guarantee-list
  (lambda (expr)
    (if (list? expr) expr (list expr))))

(define map-append
  (lambda (proc lst)
    (if (null? lst) null
	(let ([car-result (proc (car lst))])
	  (append car-result (map-append proc (cdr lst)))))))

(define replace
  (lambda (lst item1 item2)
    (cond [(null? lst) lst]
	  [(equal? item1 (car lst))
	   (cons item2 (replace (cdr lst) item1 item2))]
	  [else (cons item1 (replace (cdr lst) item1 item2))])))

(define augment-str-in-list
  (lambda (lst item aug-info)
    (if (string=? aug-info "") lst
	(replace lst item (sprintf "~a (~a)" item aug-info)))))

(define make-bold [html-bold])

;; use args for align, bold
(define create-row
  (lambda (data . args)
    (let* ([args-length (length args)]
	   [row-align (if (= args-length 0) 'center (list-ref args 0))]
	   [bold? (if (> args-length 1) (list-ref args 1) #f)]
	   [data-func [html-table-data `(align ,row-align)]])
      (apply [html-table-row]
	     (map (lambda (datum)
		    (if (list? datum)
			(apply data-func datum)
			(data-func (if (equal? "" datum) "n/a" datum))))
		  (if bold? (cons ([html-bold] (car data)) (cdr data)) data))))))

;; args has formatting commands -- table-align, row-align, border size
(define create-table
  (lambda (col-headers rowdata . args)
    (let* ([args-length (length args)]
	   [table-align (if (= args-length 0) 'center (list-ref args 0))]
	   [row-align (if (> args-length 1) (list-ref args 1) 'center)]
	   [border-size (if (> args-length 2) (list-ref args 2) 0)]
	   [create-row-fun (lambda (datum) (create-row datum row-align))])
      (apply [html-table `(border ,border-size)]
	     (if (null? col-headers)
		 (map create-row-fun rowdata)
		 (cons (create-row (map make-bold col-headers))
		       (map create-row-fun rowdata)))))))

(define make-text-input
  (lambda (name value size)
    [html-input `(type text) `(name ,name) `(size ,size) `(value ,value)]))
    
(define make-empty-text-input
  (lambda (name size)
    (make-text-input name "" size)))

(define make-checkbox
  (lambda (name value)
    [html-input `(type checkbox) `(name ,name) `(value ,value)]))

(define convert
  (lambda (value codes)
    (if (string? value) value
	(let ([code (assq value codes)])
	  (if code
	      (cdr code)
	      (symbol->string value))))))

(define insert-commas
  (lambda (lst)
    (cond [(null? lst) null]
	  [(= 1 (length lst)) lst]
	  [else (let loop ([l lst])
		  (if (null? (cdr l)) l
		      (cons (car l)
			    (cons ", " (loop (cdr l))))))])))

(define list->comma-sep-string
  (lambda (strlist)
    (apply string-append (insert-commas strlist))))

(define convert-to-crossref
  (lambda (name crossrefs)
    (if (string? name) name
	(let ([link (assq name crossrefs)])
	  (if link
	      ([html-anchor `(href ,(cdr link))] (symbol->string (car link)))
	      (symbol->string name))))))

(define convert-strlist
  (lambda (converter strlist)
    (list->comma-sep-string (map converter strlist))))

(define remove-spaces
  (lambda (str)
    (regexp-replace* " " str "")))

(define place-tag
  (lambda (tag)
    (if (string=? "" tag) ""
        ([html-anchor `(name ,tag)]))))

(define create-link
  (lambda (text url)
    (if (string=? "" url) text
	([html-anchor `(href ,url)] text))))

(define convert-to-string
  (lambda (x)
    (cond ((string? x) x)
	  ((number? x) (number->string x))
	  ((symbol? x) (symbol->string x))
	  (else (error 'convert-to-string "Can't handle ~a" x)))))

(define make-entry-boxes
  (lambda (field-values . format-name)
    (let ([format-proc (if (null? format-name) (lambda (x) x) (car format-name))])
      (map (lambda (field)
	     (let ([name (ivar (ivar field of-spec-type) name)]
		   [value-str (convert-to-string (ivar field value))])
	       (list (format-proc name) " : "
		     (make-text-input name value-str 10)
		     ([html-paragraph]))))
	   field-values))))

(define make-option
  (lambda (name)
    (list [html-option `(value ,name)] name)))

(define pad-list
  (lambda (lst to-length with-item)
    (let ([pad-length (- to-length (length lst))])
      (append lst (vector->list (make-vector pad-length with-item))))))

(define split-list-into-sublists
  (lambda (lst sublist-length . padding)
    (let loop ([split lst] [count 0] [curr-sublist null] [sublists null])
      (cond [(null? split)
	     (cond [(null? curr-sublist) (reverse sublists)]
		   [(null? padding)
		    (reverse (cons (reverse curr-sublist) sublists))]
		   [else (let ([last (pad-list (reverse curr-sublist)
					       sublist-length (car padding))])
			   (reverse (cons last sublists)))])]
	    [(= count sublist-length)
	     (loop (cdr split) 1 (list (car split))
		   (cons (reverse curr-sublist) sublists))]
	    [else
	     (loop (cdr split) (add1 count) (cons (car split) curr-sublist)
		   sublists)]))))

(define format-list-as-table
  (lambda (lst width)
    (create-table '() (split-list-into-sublists lst width) 'left 'left)))

(define gen-sec
  (lambda (recs formatter tag heading)
    (if (null? recs) (list "")
	(let ([cmds (map-append formatter recs)])
	  (list
	   ([html-anchor `(name ,tag)]
	    ([html-h3] heading))
	   (apply [html-ul] cmds))))))

(define gen-page
  (lambda (title body)
    (make-page
     ([html-head] ([html-title] title))
     #f
     (apply [html-body `(bgcolor "white")] (guarantee-list body)))))

(define gen-common-header
  (lambda (pagetitle)
    (list ([html-center] ([html-h1] "Formal Methods Education Resources"))
	  ([html-paragraph])
	  ([html-center] ([html-h1] pagetitle))
	  ([html-paragraph])
	  ([html-center]
	   ([html-anchor `(href "../")] "Main Page")
	   " | "
	   ([html-anchor `(href "../")] "Courses")
	   " | "
	   ([html-anchor `(href "../")] "Examples")
	   " | "
	   ([html-anchor `(href "../")] "Readings")
	   " | "
	   ([html-anchor `(href "../")] "Position Papers")
	   " | "
	   ([html-anchor `(href "../")] "Tools")
	   " | "
	   ([html-anchor `(href "../")] "Submit"))
	  ([html-paragraph])
	  ([html-paragraph])
	  ([html-hrule]))))

(define gen-common-footer
  (lambda ()
    (list ([html-hrule])
	  ([html-table `(width "100%")]
	   ([html-table-row]
	    ([html-table-data `(align right)]
	     ([html-small]
	      "This site is currently maintained by "
	      ([html-anchor `(href "http://www.cs.rice.edu/~kfisler/")]
	       "Kathi Fisler")
	      [html-break]
	      "Last updated on "
	      [html-verbatim-comment "#echo var=\"LAST_MODIFIED\""]
	      [html-break]
	      ([html-anchor `(href "../acks.shtml")] "Acknowledgements")
	      )))))))


