(unit
  (import)
  (export)

  (define (translations . t)
    (let loop ([t t])
      (if (null? t)
	  null
	  (let ([pattern (car t)]
		[result (cadr t)])
	    (cons (cons (regexp (format "^(~a)" pattern))
			result)
		  (loop (cddr t)))))))

  (define seq string-append)
  (define startseq seq)
  (define (arbno s) (format "(~a)*" s))
  (define (one+ s) (format "(~a)+" s))
  (define (maybe s) (format "(~a)?" s))
  (define (alt a b) (format "~a|~a" a b))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (comment s p)
    (let loop ([p (+ p 2)])
      (if (and (eq? #\* (string-ref s p))
	       (eq? #\/ (string-ref s (add1 p))))
	  (+ p 2)
	  (loop (add1 p)))))

  (define (line-comment s p)
    (let loop ([p (add1 p)])
      (let ([c (string-ref s p)])
	(if (or (eq? c #\newline)
		(eq? c #\return))
	    (add1 p)
	    (loop (add1 p))))))

  (define re:line (regexp "^#line ([0-9]*) \"([^\"]*)\""))
  (define (cpp s p)
    (let ([m (regexp-match re:line s p)])
      (when m
	(set! source-line (string->number (cadr m)))
	(set! source-file (caddr m))))
    (line-comment s p))

  (define (result s)
    (list s
	  source-file   ; file
	  source-line 0)) ; line col

  (define (symbol s)
    (result (string->symbol s) ))

  (define re:octal (regexp "0[0-9]+"))
  (define re:int (regexp "[0-9]*"))
  (define (number s)
    (result
     (cond
      [(regexp-match re:octal s) 
       (format "#o~a" s)]
      [(regexp-match re:int s)
       (string->number s)]
      [else (string->symbol s)])))

  (define (character s)
    (count-newlines s)
    (symbol s))

  (define (string s)
    (count-newlines s)
    (result s))

  (define (start s)
    'start)

  (define (stop s)
    #f)

  (define (count-newlines s)
    (let loop ([p (sub1 (string-length s))])
      (unless (= p -1)
	(when (eq? #\newline (string-ref s p))
	  (set! source-line (add1 source-line)))
	(loop (sub1 p)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define D "[0-9]")
  (define L "[a-zA-Z_]")
  (define H "[a-fA-F0-9]")
  (define E "[Ee][+-]?{D}+")
  (define FS "(f|F|l|L)")
  (define IS "(u|U|l|L)*")

  (define comments
    (translations
     "/[*]" comment
     "//" line-comment
     "#line" cpp
     "#pragma" cpp))

  (define complexes
    (translations
     (seq L (arbno (alt L D)))  symbol

     (seq "0" "[xX]" (one+ H) (maybe IS)) number

     (seq "0" (one+ D) (maybe IS)) number
     (seq (one+ D) (maybe IS)) number
     (seq (maybe L) "'([^\\']|\\\\.)+'") character
     
     (seq (one+ D) E (maybe FS)) number
     (seq (arbno D) "[.]" (one+ D) (maybe E) (maybe FS)) number
     (seq (one+ D) "[.]" (arbno D) (maybe E) (maybe FS)) number

     (seq (maybe L) "\"([^\\\"]|\\\\.)*\"") string))

  (define simple-table (make-vector 256 #f))

  (define (simple-translations . l)
    (let loop ([l l])
      (unless (null? l)
	(loop (cddr l))
	(let* ([pattern (car l)]
	       [result (cadr l)]
	       [n (char->integer (string-ref pattern 0))])
	  (vector-set! simple-table
		       n
		       (cons
			(list* pattern (string-length pattern) 
			       result)
			(or
			 (vector-ref simple-table n)
			 null)))))))

  (simple-translations
   "#" symbol
   "##" symbol
   "..." symbol
   ">>=" symbol
   "<<=" symbol
   "+=" symbol
   "-=" symbol
   "*=" symbol
   "/=" symbol
   "%=" symbol
   "&=" symbol
   "^=" symbol
   "|=" symbol
   ">>" symbol
   "<<" symbol
   "++" symbol
   "--" symbol
   "->" symbol
   "&&" symbol
   "||" symbol
   "<=" symbol
   ">=" symbol
   "==" symbol
   "!=" symbol
   ";" symbol
   "{" start
   "}" stop
   "," symbol
   "::" symbol
   ":" symbol
   "=" symbol
   "(" start
   ")" stop
   "[" start
   "]" stop
   "." symbol
   "&" symbol
   "!" symbol
   "~" symbol
   "-" symbol
   "+" symbol
   "*" symbol
   "/" symbol
   "%" symbol
   "<" symbol
   ">" symbol
   "^" symbol
   "|" symbol
   "?" symbol)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define source-file #f)
  (define source-line 0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (read-all p)
    (let loop ([l null])
      (let ([s (read-string 4096 p)])
	(if (eof-object? s)
	    (apply string-append (reverse! l))
	    (loop (cons s l))))))

  (define tokenized
    (let* ([s (read-all (current-input-port))]
	   [len (string-length s)])
      (let loop ([p 0][result null])
	(if (= p len)
	    (cons (reverse! result) p)
	    (let ([char (string-ref s p)])
	      (when (eq? char #\newline)
		(set! source-line (add1 source-line)))
	      (cond
	       [(char-whitespace? char)
		(loop (add1 p) result)]
	       [(and (memq (string-ref s p) '(#\# #\/))
		     (ormap (lambda (t)
			      (and (regexp-match-positions (car t) s p)
				   (cdr t)))
			    comments))
		=> (lambda (cmt) (loop (cmt s p) result))]
	       [else
		(let ([simple (let ([sl (vector-ref simple-table (char->integer char))])
				(and sl
				     (ormap 
				      (lambda (t)
					(and (or (= 1 (cadr t))
						 (string=? (car t) (substring s p (+ p (cadr t)))))
					     (cons ((cddr t) (car t))
						   (+ p (cadr t)))))
				      sl)))])
		  (cond
		   [(not simple)
		    (let ([complex (ormap
				    (lambda (t)
				      (let ([m (regexp-match-positions (car t) s p)])
					(and m
					     (cons ((cdr t) (substring s (caar m) (cdar m)))
						   (cdar m)))))
				    complexes)])
		      (cond
		       [(not complex)
			(error 'c-tokenize "strange: ~e ~e" p (substring s p (min len (+ p 100))))]
		       [(car complex)
			(loop (cdr complex) (cons (car complex) result))]))]
		   [(not (car simple))
		    (cons (reverse! result) (cdr simple))]
		   [(eq? (car simple) 'start)
		    (let ([sf source-file]
			  [sl source-line]
			  [sub (loop (cdr simple) null)])
		      (loop (cdr sub) (cons (list*
					     (list (substring s p (add1 p)))
					     sf
					     sl 0
					     (car sub))
					    result)))]
		   [simple
		    (loop (cdr simple) (cons (car simple) result))]))]))))))

  tokenized)
