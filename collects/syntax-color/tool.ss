(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "class.ss"))

  (provide tool@)
  
  (define (mixin-definition %)
    (class %
      (define tokens #f)
      (define/public (get-tokens) tokens)
      (define/public (set-tokens t) (set! tokens t))
      (define/public (get-buffer-start) 0)
      (super-instantiate ())))

  (define (mixin-interaction %)
    (class %
      (define tokens #f)
      (define current-pos 0)
      (inherit get-prompt-position)
      (define/public (get-tokens)
        (unless (= current-pos (get-prompt-position))
          (set! current-pos (get-prompt-position))
          (set! tokens #f))
        tokens)
      (define/public (set-tokens t) (set! tokens t))
      (define/public (get-buffer-start) (get-prompt-position))
      (super-instantiate ())))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (drscheme:get/extend:extend-interactions-text mixin-interaction)
      (drscheme:get/extend:extend-definitions-text mixin-definition)
      
      (define (phase1) (void))
      (define (phase2) (void)))))
      

