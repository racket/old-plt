(module built-in mzscheme
	(require (lib "math.ss")
		 "basic.ss")
	(provide <pervasive-funcs>)

		;; Curried primitives


	;; 18.1.2 Exceptions

	;; raise (raise)


	;; 18.1.3 Comparisons
	(define (<=> a)
	  (lambda (b)
	    (equal? a b)))

	
	(define (<> a)
	  (lambda (b)
	    (not (equal? a b))))

	(define (boolean-to-number a)
	  (if a
	      1
	      0))

	(define (<lt> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (< a b)]
	     [(boolean? a)
	      (< (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string<? a b)]
	     [else
	      "Uncaught exception: Invalid_argument"])))

	(define (<le> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (<= a b)]
	     [(boolean? a)
	      (<= (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string<=? a b)]
	     [else
	      "Uncaught exception: Invalid_argument"])))

	(define (<gt> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (> a b)]
	     [(boolean? a)
	      (> (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string>? a b)]
	     [else
	      "Uncaught exception: Invalid_argument"])))

	(define (<ge> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (>= a b)]
	     [(boolean? a)
	      (>= (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string>=? a b)]
	     [else
	      "Uncaught exception: Invalid_argument"])))

	(define (<compare> a)
	  (lambda (b)
	    (cond
	     [(and (number? a) (number? b)) (- a b)]
	     [(<lt> a b) -1]
	     [(<gt> a b) 1]
	     [else 0])))


	(define (<min> x)
	  (lambda (y)
	    (cond
	     [(number? x) (if (< x y) x y)]
	     [(string? x) (if (string<? x y) x y)]
	     [(boolean? x) (if x y x)])))

	(define (<max> x)
	  (lambda (y)
	    (cond
	     [(number? x) (if (> x y) x y)]
	     [(string? x) (if (string>? x y) x y)]
	     [(boolean? x) (if x x y)])))
	
	(define (<==> a)
	  (lambda (b)
	    (eq? a b)))

	(define (<!=> a)
	  (lambda (b)
	    (not (eq? a b))))


	;; 18.1.4 Boolean operations

	(define (<or> a)
	  (lambda (b)
	    (or a b)))

	(define (<and> a)
	  (lambda (b)
	    (and a b)))

	;; 18.1.5 Integer arithmetic

	(define (<~-> a)
	  (- a))

	;;succ (add1)

	;;pref (sub1)

	(define (<+> a)
	  (lambda (b)
	    (+ a b)))

	(define (<-> a)
	  (lambda (b)
	    (- a b)))
	
	(define (<*> a)
	  (lambda (b)
	    (* a b)))

	(define (</> a)
	  (lambda (b)
	    (/ a b)))

	(define (<mod> a)
	  (lambda (b)
	    (modulo a b)))

	;;abs (abs)

	;; Bitwise operations
	(define (<land> a)
	  (lambda (b)
	    (bitwise-and a b)))

	(define (<lor> a)
	  (lambda (b)
	    (bitwise-ior a b)))

	(define (<lxor> a)
	  (lambda (b)
	    (bitwise-xor a b)))
	
	;; lnot (bitwise-not

	;;18.1.6 Floating Point arithmetic
	
	;; ~- (- a)

	;; +. (<+>)

	;; -. (<->)

	;; *. (<*>)

	;; /. (</>)

	(define (<**> a)
	  (lambda (b)
	    (expt a b)))
	
	;; sqrt (sqrt)

	;; exp (exp)

	;; log (log)
	
	(define (<log10> a)
	  (/ (log a) (log 10)))

	;; cos (cos)

	;; sin (sin)

	;; tan (tan)

	;; acos (acos)

	;; asin (asin)

	;; atan (atan)

	(define (<atan2> a)
	  (lambda (b)
	    (atan a b)))

	;; cosh (cosh)

	;; sinh (sinh)

	;; tanh (tanh)

	;; ceiling (ceiling)
	
	;; floor (floor)

	;; abs_float (abs)

	(define (<mod_float> a)
	  (lambda (b)
	    (- a (* (floor (/ a b)) b))))

;;	(define (<frexp> a)
;;	  (if (= a 0)
;;	      (make-<tuple> 0 0)
	      
	(define (<ldexp> a)
	  (lambda (b)
	    (* a (expt 2 b))))

	(define (<modf> a)
	  (if (> a 0)
	      (make-<tuple> (- a (floor a)) (floor a))
	      (make-<tuple> (- a (ceiling a)) (ceiling a))))

	;; float (truncate)

	;; float_of_int (truncate)

	;; truncate (truncate)

	;; int_of_float (no-op)

	;; ignore 

	;;18.1.7 String Operations

	(define (<string-append> a)
	  (lambda (b)
	    (string-append a b)))

	;;18.1.8 Character operations

	;; int_of_char (char->integer)

	;; char_of_int (integer->char)

	;;18.1.9 Unit operations

	(define (<ignore> a)
	  (make-<unit>))

	;;18.1.10 String conversion functions

	(define (<string_of_bool> a)
	  (if a
	      "true"
	      "false"))

	(define (<bool_of_string> a)
	  (cond
	   [(equal? a "true") #t]
	   [(equal? a "false") #f]
	   [else "Invalid_Argument"]))

	(define (<string_of_int> a)
	  (format "~a" a))

	(define (<int_of_string> a)
	  (string->number a))
	  
	(define <string_of_float> <string_of_int>)

	(define <float_of_string> <int_of_string>)

	;;18.1.11 Pair operations
	  
	(define (<fst> a)
	  (car (<tuple>-list a)))

	(define (<snd> a)
	  (cadr (<tuple>-list a)))

	;;18.1.12 List operations

	(define (<append> a)
	  (lambda (b)
	    (append a b)))

	;;18.1.13 Input/output

	;; stdin (current-input-port)
	;; stdout (current-output-port)
	;; stderr (current-error-port)

	;; print_char (display)
	;; print_string (display)
	;; print_int (display)
	;; print_float (display)
	;; print_endline (display)
	;; print_newline (newline)

	(define (<display-error> a)
	  (display a (current-error-port)))
	
	;; prerr_char (<display-error>)
	;; prerr_string (<display-error>)
	;; prerr_int (<display-error>)
	;; prerr_float (<display-error>)
	;; prerr_endline (<display-error>)
	
	(define (<prerr_newline> a)
	  (newline (current-error-port)))

	(define (<read_line> a)
	  (read-line))
	;; read_line (read-line)
	
	(define (<read_int>)
	  (let ([res (read)])
	    (if (integer? res)
		res
		"Failure int_of_string")))

	(define (<read_float>)
	  (let ([res (read)])
	    (if (number? res)
		res
		"Failure: float_of_string")))

	
	(define (<open_out> f)
	  (open-output-file f 'text))
	
	;; open_out_bin (open-output-file)

	(define (<flush> port)
	  (begin
	    (flush-output port)
	    (make-<unit>)))

	(define (<output_char> port)
	  (lambda (char)
	    (begin
	      (write char port)
	      (make-<unit>))))

	(define (<output_string> port)
	  (lambda (string)
	    (begin
	      (write string port)
	      (make-<unit>))))

	(define (<output> port)
	  (lambda (string) 
	    (lambda (pos) 
	      (lambda (len)
		(cond
		 [(> len 1) (begin 
			      (write (string-ref string pos))
			      (<output> port string (+ 1 pos) (- 1 len)))]
		 [(= len 1) (begin
			      (write (string-ref string pos))
			      (make-<unit>))]
		 [else "Invalid_argument: output"])))))

	(define (<output_value> port)
	  (lambda (item)
	    (begin
	      (write item port)
	      (make-<unit>))))

	(define (<seek_out> port) 
	  (lambda (k)
	    (begin
	      (file-position port k)
	      (make-<unit>))))

	(define (<pos_out> port)
	  (file-position port))

	
	(define (<close_out> port)
	  (begin
	    (close-output-port port)
	    (make-<unit>)))

	(define (<open_in> f)
	  (open-input-file f 'text))

	;; open_in_bin (open-input-file)

	(define (<input_char> p)
	  (read-char p))

	(define (<input_line> p)
	  (read-line p))

	(define (<input_value> p)
	  (read p))
	
	(define (<seek_in> p)
	  (lambda (n)
	    (begin
	      (file-position p n)
	      (make-<unit>))))

	(define (<pos_in> p)
	  (file-position p))

	(define (<close_in> port)
	  (begin
	    (close-input-port port)
	    (make-<unit>)))
	



	;; ref (box)

	;; ! (unbox)

	(define (<:=> b)
	  (lambda (v)
	    (begin (set-box! b v)
		   (make-<unit>))))

	(define (<incr> a)
	  (set-box! a (+ 1 (unbox a))))

	(define (<decr> a)
	  (set-box! a (- (unbox a) 1)))


	(define <pervasive-funcs> (make-hash-table 'equal))
	(hash-table-put! <pervasive-funcs> "raise" (cons (make-arrow (list "exception") (make-tvar "'a")) raise))
	(hash-table-put! <pervasive-funcs> "=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <=>))
	(hash-table-put! <pervasive-funcs> "<>" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <>))

	(hash-table-put! <pervasive-funcs> "<" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <lt>))

	(hash-table-put! <pervasive-funcs> ">" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <gt>))

	(hash-table-put! <pervasive-funcs> "<=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <le>))

	(hash-table-put! <pervasive-funcs> ">=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <ge>))

	(hash-table-put! <pervasive-funcs> "compare" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool") ) <compare>))

	(hash-table-put! <pervasive-funcs> "min" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) (make-tvar "'a")) ) <min>))

	(hash-table-put! <pervasive-funcs> "max" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) (make-tvar "'a")) ) <max> ))

	(hash-table-put! <pervasive-funcs> "==" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <==> ))

	(hash-table-put! <pervasive-funcs> "!=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <!=>))

	(hash-table-put! <pervasive-funcs> "not" (cons (make-arrow (list "bool") "bool")  not))

	(hash-table-put! <pervasive-funcs> "&&" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <and>))

	(hash-table-put! <pervasive-funcs> "&" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <and>))


	(hash-table-put! <pervasive-funcs> "||" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <or>))

	(hash-table-put! <pervasive-funcs> "or" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <or>))

	(hash-table-put! <pervasive-funcs> "~-" (cons (make-arrow (list "int") "int") -))

	(hash-table-put! <pervasive-funcs> "succ" (cons (make-arrow (list "int") "int") add1))

	(hash-table-put! <pervasive-funcs> "pred" (cons (make-arrow (list "int") "int") sub1))

	(hash-table-put! <pervasive-funcs> "+" (cons (make-arrow (list "int") (make-arrow (list "int") "int") ) <+>))

	(hash-table-put! <pervasive-funcs> "-" (cons (make-arrow (list "int") (make-arrow (list "int") "int") ) <->))

	(hash-table-put! <pervasive-funcs> "*" (cons (make-arrow (list "int") (make-arrow (list "int") "int") ) <*>))

	(hash-table-put! <pervasive-funcs> "/" (cons (make-arrow (list "int") (make-arrow (list "int") "int") ) </>))

	(hash-table-put! <pervasive-funcs> "mod" (cons (make-arrow (list "int") (make-arrow (list "int") "int") ) <mod>))

	(hash-table-put! <pervasive-funcs> "abs" (cons (make-arrow (list "int") "int") abs))

	(hash-table-put! <pervasive-funcs> "land" (cons (make-arrow (list "int") (make-arrow (list "int") "int") ) <land>))

	(hash-table-put! <pervasive-funcs> "lor" (cons (make-arrow (list "int") (make-arrow (list "int") "lor") ) <mod>))

	(hash-table-put! <pervasive-funcs> "lxor" (cons (make-arrow (list "int") (make-arrow (list "int") "int") ) <lxor>))

	(hash-table-put! <pervasive-funcs> "lnot" (cons (make-arrow (list "int") "int") bitwise-not))

	(hash-table-put! <pervasive-funcs> "~-." (cons (make-arrow (list "float") "float") -))

	(hash-table-put! <pervasive-funcs> "+." (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) <+>))

	(hash-table-put! <pervasive-funcs> "-." (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) <->))

	(hash-table-put! <pervasive-funcs> "*." (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) <*>))

	(hash-table-put! <pervasive-funcs> "/." (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) </>))

	(hash-table-put! <pervasive-funcs> "**" (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) <**>))

	(hash-table-put! <pervasive-funcs> "sqrt" (cons (make-arrow (list "float") "float") sqrt))
	
	(hash-table-put! <pervasive-funcs> "exp" (cons (make-arrow (list "float") "float") exp))

	(hash-table-put! <pervasive-funcs> "log" (cons (make-arrow (list "float") "float") log))

	(hash-table-put! <pervasive-funcs> "log10" (cons (make-arrow (list "float") "float") <log10>))

	(hash-table-put! <pervasive-funcs> "cos" (cons (make-arrow (list "float") "float") cos))

	(hash-table-put! <pervasive-funcs> "sin" (cons (make-arrow (list "float") "float") sin))

	(hash-table-put! <pervasive-funcs> "tan" (cons (make-arrow (list "float") "float") tan))

	(hash-table-put! <pervasive-funcs> "acos" (cons (make-arrow (list "float") "float") acos))

	(hash-table-put! <pervasive-funcs> "asin" (cons (make-arrow (list "float") "float") asin))

	(hash-table-put! <pervasive-funcs> "atan" (cons (make-arrow (list "float") "float") atan))


	(hash-table-put! <pervasive-funcs> "atan2" (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) <atan2>))

	

	(hash-table-put! <pervasive-funcs> "cosh" (cons (make-arrow (list "float") "float") cosh))


	(hash-table-put! <pervasive-funcs> "sinh" (cons (make-arrow (list "float") "float") sinh))


;	(hash-table-put! <pervasive-funcs> "tanh" (cons (make-arrow (list "float") "float") tanh))


	(hash-table-put! <pervasive-funcs> "ceil" (cons (make-arrow (list "float") "float") ceiling))


	(hash-table-put! <pervasive-funcs> "floor" (cons (make-arrow (list "float") "float") floor))


	(hash-table-put! <pervasive-funcs> "abs_float" (cons (make-arrow (list "float") "float") abs))

	(hash-table-put! <pervasive-funcs> "mod_float" (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) <mod_float>))

	(hash-table-put! <pervasive-funcs> "ldexp" (cons (make-arrow (list "float") (make-arrow (list "float") "float" )) <ldexp>))

	(hash-table-put! <pervasive-funcs> "modf" (cons (make-arrow (list "float") (make-<tuple> (list "float" "float"))) <modf>))

	(hash-table-put! <pervasive-funcs> "float" (cons (make-arrow (list "int") "float") (lambda (x) x)))
			 
	(hash-table-put! <pervasive-funcs> "float_of_int" (cons (make-arrow (list "int") "float") (lambda (x) x)))

	(hash-table-put! <pervasive-funcs> "truncate" (cons (make-arrow (list "float") "int") truncate))

	(hash-table-put! <pervasive-funcs> "int_of_float" (cons (make-arrow (list "float") "int") truncate))

	(hash-table-put! <pervasive-funcs> "^" (cons (make-arrow (list "string") (make-arrow (list "string") "string")) <string-append>))

	(hash-table-put! <pervasive-funcs> "int_of_char" (cons (make-arrow (list "char") "int") char->integer))

	(hash-table-put! <pervasive-funcs> "char_of_int" (cons (make-arrow (list "int") "char") integer->char))

	(hash-table-put! <pervasive-funcs> "ignore" (cons (make-arrow (list (make-tvar "'a")) "unit") <ignore>))
	

	(hash-table-put! <pervasive-funcs> "string_of_bool" (cons (make-arrow (list "bool") "string") <string_of_bool>))

	(hash-table-put! <pervasive-funcs> "bool_of_string" (cons (make-arrow (list "string") "bool") <bool_of_string>))

	(hash-table-put! <pervasive-funcs> "string_of_int" (cons (make-arrow (list "int") "string") <string_of_int>))
	
	(hash-table-put! <pervasive-funcs> "int_of_string" (cons (make-arrow (list "string") "int") <int_of_string>))

	(hash-table-put! <pervasive-funcs> "string_of_float" (cons (make-arrow (list "float") "string") <string_of_float>))

	(hash-table-put! <pervasive-funcs> "float_of_string" (cons (make-arrow (list "string") "float") <int_of_string>))

	(hash-table-put! <pervasive-funcs> "fst" (cons (make-arrow (list (make-<tuple> (list (make-tvar "'a") (make-tvar "'b")))) (make-tvar "'a")) <fst>))

	(hash-table-put! <pervasive-funcs> "snd" (cons (make-arrow (list (make-<tuple> (list (make-tvar "'a") (make-tvar "'b")))) (make-tvar "'b")) <snd>))

	(hash-table-put! <pervasive-funcs> "@" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-arrow (list (make-tlist (make-tvar "'a'"))) (make-tlist (make-tvar "'a")))) <append>))

	(hash-table-put! <pervasive-funcs> "print_char" (cons (make-arrow (list "char") "unit") display))

	(hash-table-put! <pervasive-funcs> "print_string" (cons (make-arrow (list "string") "unit") display))

	(hash-table-put! <pervasive-funcs> "print_int" (cons (make-arrow (list "int") "unit") display))

	(hash-table-put! <pervasive-funcs> "print_float" (cons (make-arrow (list "float") "unit") display))

	(hash-table-put! <pervasive-funcs> "print_endline" (cons (make-arrow (list "string") "unit") display))

	(hash-table-put! <pervasive-funcs> "print_newline" (cons (make-arrow (list "unit") "unit") newline))

		(hash-table-put! <pervasive-funcs> "prerr_char" (cons (make-arrow (list "char") "unit") <display-error>))

	(hash-table-put! <pervasive-funcs> "prerr_string" (cons (make-arrow (list "string") "unit") <display-error>))

	(hash-table-put! <pervasive-funcs> "prerr_int" (cons (make-arrow (list "int") "unit") <display-error>))

	(hash-table-put! <pervasive-funcs> "prerr_float" (cons (make-arrow (list "float") "unit") <display-error>))

	(hash-table-put! <pervasive-funcs> "prerr_endline" (cons (make-arrow (list "string") "unit") <display-error>))

	(hash-table-put! <pervasive-funcs> "prerr_newline" (cons (make-arrow (list "unit") "unit") <prerr_newline>))

	(hash-table-put! <pervasive-funcs> "read_line" (cons (make-arrow (list "unit") "string") read-line))

	(hash-table-put! <pervasive-funcs> "read_int" (cons (make-arrow (list "unit") "int") <read_int>))

	(hash-table-put! <pervasive-funcs> "read_float" (cons (make-arrow (list "unit") "float") <read_float>))

	(hash-table-put! <pervasive-funcs> "open_out" (cons (make-arrow (list "string") "out_channel") <open_out>))

	(hash-table-put! <pervasive-funcs> "open_out_bin" (cons (make-arrow (list "string") "out_channel") open-output-file))

	(hash-table-put! <pervasive-funcs> "flush" (cons (make-arrow (list "out_channel") "unit") <flush>))

	(hash-table-put! <pervasive-funcs> "output_char" (cons (make-arrow (list "out_channel") (make-arrow (list "char") "unit")) <output_char>))

	(hash-table-put! <pervasive-funcs> "output_string" (cons (make-arrow (list "out_channel") (make-arrow (make-tlist "string") "unit")) <output_string>))

	(hash-table-put! <pervasive-funcs> "output" (cons (make-arrow (list "out_channel") (make-arrow (list "string") (make-arrow (list "int") (make-arrow (list "int") "unit")))) <output>))
	
	(hash-table-put! <pervasive-funcs> "output_value" (cons (make-arrow (list "out_channel") (make-arrow (list (make-tvar "'a")) "unit")) <output_value>))

	(hash-table-put! <pervasive-funcs> "seek_out" (cons (make-arrow (list "out_channel") (make-arrow (list "int") "unit")) <seek_out>))

	(hash-table-put! <pervasive-funcs> "pos_out" (cons (make-arrow (list "out_channel") "int") <pos_out>))

	(hash-table-put! <pervasive-funcs> "close_out" (cons (make-arrow (list "out_channel") "unit") <close_out>))

	(hash-table-put! <pervasive-funcs> "open_in" (cons (make-arrow (list "string") "in_channel") <open_in>))

	(hash-table-put! <pervasive-funcs> "open_in_bin" (cons (make-arrow (list "string") "unit") open-input-file))
	
	(hash-table-put! <pervasive-funcs> "input_char" (cons (make-arrow (list "in_channel") "char") <input_char>))
	
	(hash-table-put! <pervasive-funcs> "input_line" (cons (make-arrow (list "in_channel") "string") <input_line>))

	(hash-table-put! <pervasive-funcs> "input_value" (cons (make-arrow (list "in_channel") (make-tvar "'a")) <input_value>))

	(hash-table-put! <pervasive-funcs> "seek_in" (cons (make-arrow (list "in_channel") (make-arrow (list "int") "unit")) <seek_in>))

	(hash-table-put! <pervasive-funcs> "pos_in" (cons (make-arrow (list "in_channel") "int") <pos_in>))

	(hash-table-put! <pervasive-funcs> "close_in" (cons (make-arrow (list "in_channel") "unit") <close_in>))

	(hash-table-put! <pervasive-funcs> "ref" (cons (make-arrow (list (make-tvar "'a")) (make-ref (make-tvar "'a"))) box))

	(hash-table-put! <pervasive-funcs> "!" (cons (make-arrow (list (make-ref (make-tvar "'a"))) (make-tvar "'a")) unbox))

	(hash-table-put! <pervasive-funcs> ":=" (cons (make-arrow (list (make-ref (make-tvar "'a"))) (make-arrow (list (make-tvar "'a")) "unit")) <:=>))

	(hash-table-put! <pervasive-funcs> "incr" (cons (make-arrow (list (make-ref "int")) "unit") <incr>))

	(hash-table-put! <pervasive-funcs> "decr" (cons (make-arrow (list (make-ref "int")) "unit") <decr>))


)



	