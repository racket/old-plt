; stepper.ss 

(require-library "letplsrc.ss")

(unit/sig stepper^
  (import [e : stepper:error^]
	  mzlib:function^
	  [z : zodiac:system^])
  
  (define (stepper-start string)
    (source-table-build (list ((z:read (open-input-string string)))))
    source-table-lookup)
  
  (define (stepper-step)
    null)
  
  (define (stepper-stop)
    null)
  
  ; HASH SOURCE LOCATIONS AND CLOSURES: 
  
  (define-values (source-table-build source-table-lookup)
    (let ([source-exprs #f])
      (values
       (lambda (read-exprs)
	 (set! source-exprs read-exprs))
       (lambda (offset)
	 (let search-exprs ([exprs source-exprs])
	   (let ([expr 
		  (car (filter 
			(lambda (expr) 
			  (< offset (z:location-offset (z:zodiac-finish expr))))
			exprs))])
	     (if (= offset (z:location-offset (z:zodiac-start expr)))
		 expr
		 (cond
		   ((z:scalar? expr) (e:static-error "starting offset inside scalar:" offset))
		   ((z:sequence? expr) 
		    (let ([object (z:read-object expr)])
		      (cond
			((z:list? expr) (search-exprs object))
			((z:vector? expr) (search-exprs (vector->list object))) ; can source exprs be here?
			((z:improper-list? expr)
			 (search-exprs (search-exprs object)))
			(else (e:static-error "unknown expression type in sequence" expr)))))
		   (else (e:static-error "unknown read type" expr))))))))))

  (define-values (closure-table-put! closure-table-lookup)
    (let ([closure-table (make-hash-table-weak)])
      (values
       (lambda (key value)
	 (hash-table-put! closure-table key value))
       (lambda (key)
	 (hash-table-get closure-table key)))))
  
  ; SOURCE RECONSTRUCTION
  
  

  
  
  
  
  

  ; DOWN HERE NOT FINISHED YET
  
  (define break-info #f)
  (define break-sema (make-semaphore))
  (define resume-sema (make-semaphore))
  
  (define (break)
    (set! break-info (current-continuation-marks debug-key))
    (semaphore-post break-sema)
    (semaphore-wait resume-sema))
  
  '(define (step)
    (semaphore-post resume-sema)
    (semaphore-wait break-sema)
    (display-break-info))
  
  '(define (annotate-wrapper expr)
    (let-values (((annotated dont-care dont-care-2) (annotate (my-expand-defmacro expr) null #t)))
    annotated))
  
  '(define (display-break-info)
    (for-each (lambda (frame)
		(let ([debug-info (frame)])
		  (printf "expr: ~a~n" (car debug-info))
		  (if (not (null? (cdr debug-info)))
		      (printf " bound: ~a~n" (cdr debug-info)))))
	      break-info))
  
  '(define (step-prog expr)
    (initialize-closure-table)
    (thread (lambda () 
	      (printf "result: ~a~n" (eval (annotate-wrapper expr)))
	      (semaphore-post break-sema)))
    (semaphore-wait break-sema)
    (display-break-info))
  
  
  
  )