
;; Reads pre-processed C input from the current input port, and
;; tokenizes it, with paren/bracket/brace matching. The result is a
;; list of "elements", where the client defines the representation of
;; elements.

;; The `make-element' imported function must take 3 arguments: a single
;; token (symbol, number, or string), a filename, and a line number.

;; The `make-seq-element' imported function must take 4 arguments: a character
;; (#\(, #\[, or #\{), a file, a line number, and a list of elements
;; (for the syntax between the parens/brackets/braces).

;; The token patterns are based on a standard C .lex description.

(unit
  (import make-element make-seq-element)
  (export)

  (define (trans pattern)
    (regexp (format "^(~a)" pattern)))

  (define (translations . t)
    (let loop ([t t])
      (if (null? t)
	  null
	  (let ([pattern (car t)]
		[result (cadr t)])
	    (cons (cons (trans pattern)
			result)
		  (loop (cddr t)))))))

  (define (a-regexp-match-positions re s p)
    (regexp-match-positions re s p))

  (define seq string-append)
  (define startseq seq)
  (define (arbno s) (format "(~a)*" s))
  (define (arbno/ s) (format "~a*" s))
  (define (one+ s) (format "(~a)+" s))
  (define (one+/ s) (format "~a+" s))
  (define (maybe s) (format "(~a)?" s))
  (define (maybe/ s) (format "~a?" s))
  (define (alt a b) (format "~a|~a" a b))
  (define (alt* . l)
    (let loop ([l l])
      (if (null? (cdr l))
	  (format "(~a)" (car l))
	  (format "(~a)|~a" (car l) (loop (cdr l))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (line-comment s p)
    (let loop ([p (add1 p)])
      (let ([c (string-ref s p)])
	(if (or (eq? c #\newline)
		(eq? c #\return))
	    (add1 p)
	    (loop (add1 p))))))

  (define re:line (regexp (format "^#[^~a~a]* ([0-9]+) \"([^\"]*)\"" 
				  #\newline #\return)))
  (define (cpp s p)
    (let ([m (regexp-match re:line s p)])
      (when m
	(set! source-line (string->number (cadr m)))
	(set! source-file (caddr m))))
    (line-comment s p))

  (define (result s)
    (make-element
     s
     source-file   ; file
     source-line)) ; line

  (define (symbol s)
    (result (string->symbol s)))

  (define re:octal (regexp "^0[0-9]+$"))
  (define re:int (regexp "^[0-9]*$"))
  (define (number s)
    (result
     (cond
      [(regexp-match-positions re:octal s) 
       (string->number s 8)]
      [(regexp-match-positions re:int s)
       (string->number s)]
      [else (string->symbol s)])))

  (define (character s)
    (count-newlines s)
    (symbol s))

  (define (string s)
    (count-newlines s)
    (result (substring s 1 (sub1 (string-length s)))))

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
  (define E (format "[Ee][+-]?~a+" D))
  (define FS "(f|F|l|L)")
  (define IS "(u|U|l|L)*")

  (define comments
    (translations
     "#" cpp))

  (define symbol-complex (trans (seq L (arbno (alt L D)))))

  (define number-complex
    (trans (alt*
	    (seq (arbno/ D) "[.]" (one+/ D) (maybe E) (maybe/ FS))
	    (seq (one+/ D) "[.]" (arbno D) (maybe E) (maybe/ FS))
	    (seq (one+/ D) E (maybe/ FS))

	    (seq "0" "[xX]" (one+/ H) IS) ;; hex
	    (seq "0" (one+/ D) IS) ;; octal
	    (seq (one+/ D) IS)))) ;; integer

  (define char-complex (trans (seq (maybe L) "'([^\\']|\\\\.)+'")))
  (define string-complex (trans (seq (maybe L) "\"([^\\\"]|\\\\.)*\"")))

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
   "." #f ; => symbol/num
   "&" symbol
   "!" symbol
   "~" symbol
   "-" #f ; => symbol/num
   "+" #f ; => symbol/num
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
	       [(eq? char '#\#) ;; We assume only #-based preprocessor left
		(loop (cpp s p) result)]
	       [else
		(let ([simple (let ([sl (vector-ref simple-table (char->integer char))])
				(and sl
				     (ormap 
				      (lambda (t)
					(and (or (= 1 (cadr t))
						 (string=? (car t) (substring s p (+ p (cadr t)))))
					     (let ([f (cddr t)])
					       (if f
						   (cons (f (car t))
							 (+ p (cadr t)))
						   (let ([m (regexp-match-positions number-complex s p)])
						     (if m
							 (cons (number (substring s (caar m) (cdar m)))
							       (cdar m))
							 (cons (symbol (car t))
							       (+ p (cadr t)))))))))
				      sl)))])
		  (cond
		   [(not simple)
		    (cond
		     [(regexp-match-positions symbol-complex s p)
		      => (lambda (m)
			   (loop (cdar m)
				 (cons (symbol (substring s (caar m) (cdar m)))
				       result)))]
		     [(regexp-match-positions number-complex s p)
		      => (lambda (m)
			   (loop (cdar m)
				 (cons (number (substring s (caar m) (cdar m)))
				       result)))]
		     [(regexp-match-positions char-complex s p)
		      => (lambda (m)
			   (loop (cdar m)
				 (cons (character (substring s (caar m) (cdar m)))
				       result)))]
		     [(regexp-match-positions string-complex s p)
		      => (lambda (m)
			   (loop (cdar m)
				 (cons (string (substring s (caar m) (cdar m)))
				       result)))]
		     [else
		      (error 'c-tokenize "strange: ~e ~e" p (substring s p (min len (+ p 100))))])]
		   [(not (car simple))
		    (cons (reverse! result) (cdr simple))]
		   [(eq? (car simple) 'start)
		    (let ([sf source-file]
			  [sl source-line]
			  [sub (loop (cdr simple) null)])
		      (loop (cdr sub) (cons (make-seq-element
					     (string-ref s p)
					     sf
					     sl
					     (car sub))
					    result)))]
		   [simple
		    (loop (cdr simple) (cons (car simple) result))]))]))))))

  tokenized)
