; misc exn

(module arg-mismatch-exn mzscheme
  (require
   (lib "specs.ss" "framework") ; contracts
   )
  
  (provide/contract
   (raise-arg-mismatch-exn (string? any? any? . -> . void?))
   )
  
  ; string top top -> void
  (define (raise-arg-mismatch-exn fct-name type value)
    (raise (make-exn:application:mismatch
            (format "~a: expects argument of type <~a>; given ~a" fct-name type value)
            (current-continuation-marks)
            value)))
  
  )
