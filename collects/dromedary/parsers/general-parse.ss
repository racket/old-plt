#cs
(module general-parse mzscheme
	(require "ml-lex.ss"
		 (lib "lex.ss" "parser-tools")
		 (lib "yacc.ss" "parser-tools")
		 (lib "readerr.ss" "syntax")
		 (prefix ast: "../ast.ss"))
	
	(provide parse-offset mktailpat mktailexp mkinfix mkexp mkpat mktyp 
		 build-src build-syn-list build-src-list ml-split parse-ml-port
		 parse-ml-file
		 (all-from "ml-lex.ss")
		 (all-from (lib "lex.ss" "parser-tools"))
		 (all-from (lib "yacc.ss" "parser-tools"))
		 (all-from (lib "readerr.ss" "syntax")))

	(define parse-offset (make-parameter 0))
	
	(define (mktailexp taillist src)
  (if (null? taillist)
      (ast:make-expression (ast:make-pexp_construct (ast:make-lident (datum->syntax-object #f "[]" #f)) null #f) src)
      (let ([exp_el (mktailexp (cdr taillist) src)]
	    [lsrc (ast:expression-pexp_src (car taillist))])
	(ast:make-expression (ast:make-pexp_construct (ast:make-lident (datum->syntax-object #f "::" #f)) (ast:make-expression (ast:make-pexp_tuple (list (car taillist) exp_el)) lsrc) #f) lsrc))))

(define (mktailpat taillist src)
  (if (null? taillist)
      (ast:make-pattern (ast:make-ppat_construct (ast:make-lident (datum->syntax-object #f "[]" #f)) null #f) src)
      (let ([pat_pl (mktailpat (cdr taillist) src)]
	    [lsrc (ast:pattern-ppat_src (car taillist))])
	(ast:make-pattern (ast:make-ppat_construct (ast:make-lident (datum->syntax-object #f "::" #f)) (ast:make-pattern (ast:make-ppat_tuple (list (car taillist) pat_pl)) lsrc) #f) lsrc))))
			

(define-syntax mkinfix
  (syntax-rules ()
		  ([_ lhs op rhs] 
		   (syntax [ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident op)) (build-src 2 2)) (list (cons "" lhs) (cons "" rhs))) (build-src 3)]))))
  
  (define-syntax mkexp
    (syntax-rules ()
		  ([_ exp] 
		   (syntax [ast:make-expression exp (build-src 1)]))))
  
  (define-syntax mkpat
    (syntax-rules ()
		   ([_ pat] 
		    (syntax [ast:make-pattern pat (build-src 1)]))))
  
  (define-syntax mktyp
    (syntax-rules ()
		   ([_ typ] 
		    (syntax [ast:make-core_type typ (build-src 1)]))))

  (define-syntax (build-src stx)
    (syntax-case stx ()
		 ((_ end)
		  (syntax (build-src 1 end)))
		 ((_ start end)
		  (with-syntax ((start-pos (datum->syntax-object 
					    (syntax end)
					    (string->symbol 
					     (format "$~a-start-pos"
						     (syntax-object->datum (syntax start))))))
				(end-pos (datum->syntax-object 
					  (syntax end)
					  (string->symbol 
					   (format "$~a-end-pos"
						   (syntax-object->datum (syntax end)))))))
			       (syntax
				(ast:make-src
				 (position-line start-pos)
					      (position-col start-pos)
					      (+ (position-offset start-pos) (parse-offset))
					      (- (position-offset end-pos)
						 (position-offset start-pos))))))))
  
 (define (build-syn-list syn . others)
   (if (null? others)
       (list #f
	     (syntax-line syn)
	     (syntax-column syn)
	     (+ (syntax-position syn) (parse-offset))
	     (syntax-span syn))
       (list #f
	     (syntax-line syn)
	     (syntax-column syn)
	     (+ (syntax-position syn) (parse-offset))
	     (- (car others)
		(syntax-column syn)))))

 (define-syntax build-src-list
   (syntax-rules ()
		 ((_ end)
		  (syntax (build-src-list end end)))
		 ((_ start end)
		  (with-syntax ((start-pos (datum->syntax-object
					    (syntax end)
					    (string->symbol
					     (format "$~a-start-pos"
						     (syntax-object->datum (syntax start))))))
				(end-pos (datum->syntax-object 
					  (syntax end)
					  (string->symbol 
					   (format "$~a-end-pos"
						   (syntax-object->datum (syntax end)))))))
			       (syntax
				(list #f
				      (position-line start-pos)
				      (position-col start-pos)
				      (+ (position-offset start-pos) (parse-offset))
				      (- (position-offset end-pos)
					 (position-offset start-pos))))))))
  
  (define (ml-split listtosplit)
      (ml-split-helper listtosplit (list null null)))
  
  (define (ml-split-helper listtosplit newlist)
    (if (null? listtosplit)
	newlist
	(ml-split-helper (cdr listtosplit) (list (append (car newlist) (list (car (car listtosplit)))) (append (cadr newlist) (list (cdr (car listtosplit))))))))


  (define (parse-ml-port port file offset parser)
    (parse-offset offset)
    (let ([lexer (ml-lex file)])
      (port-count-lines! port)
      (parser
       (lambda ()
	 (let loop ()
	   (let ([v (lexer port)])
	     (if (void? (car v))
		 (loop)
		 v)))))))
  
  (define (parse-ml-file file parser)
    (with-input-from-file file
      (lambda ()
	(parse-ml-port (current-input-port)
			(path->complete-path file)
			0
			parser))))
)