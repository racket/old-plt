(module fit mzscheme
  (require (lib "list.ss"))
  (require (lib "fit-low-level.ss" "plot"))
  (require (lib "etc.ss"))
  
        
  ; a structure containing a function to be fitted
  ; and information about it's parameters
  (define-struct fit-fun  (function
                             vars 
                             params))
 
  ; allows creation of fit-function structures
  (define-syntax fit-lambda 
    (syntax-rules ()
      [(_ (a ...) (b ...) body)
       (make-fit-fun
        (lambda (a ... b ...) body)
        (list (quote a) ...)
        (list (quote b) ...))]))
         
  ; a structure contain a the results of a curve-fit
  (define-struct fit-result (
                             function
                             final-params
                             ;rms
                             ;variance                             
                             ;limit                             
                             ;correlation
                             ;std-err
                             ))
  
  
  
  ; expects : fit-fun (asslist) (listof num) [(listof num)] (listof num) [(listof-num) u 1]
  (define (fit function guesses x-data . rest)    
    (let* ((num-vars (length (fit-fun-vars function)))           
           (y-data (if (= num-vars 2)
                       (car rest)
                       x-data))
           (z-data (if (= num-vars 2)
                       (cadr rest)
                       (car rest)))
           (error-given (if (= num-vars 2)
                            (list-ref rest 2)
                            (list-ref rest 1)))
           (err-data (if (list? error-given)
                      error-given
                      (build-list (length x-data) (lambda (x) 1))))
           (final-function (if (= num-vars 2)
                               (fit-fun-function  function)
                               (lambda (x y . rest)
                                 (apply (fit-fun-function function) x rest))))
           (ordered-guesses (setup-guesses guesses (fit-fun-params function)))
           (result-params (fit-internal final-function x-data y-data z-data err-data ordered-guesses)))
      (if (null? result-params)
          null         
          (make-fit-result
           (lambda args (apply (fit-fun-function function) (append args result-params)))
           (map list (fit-fun-params function) result-params)))))
  
  ; setup-guesses : asslist (listof symbol) -> (listof num)
  ; generates a list of initial guesses given the given guesses and symbols
  (define (setup-guesses given-values all-values)
    (cond 
      [(empty? all-values) empty]
      [else
       (cons 
        (cond [(assq (car all-values) given-values) => cadr]
              [else 1])
        (setup-guesses given-values (cdr all-values)))]))
  
   (provide fit fit-lambda (struct fit-result (
                             function
                             final-params) )))