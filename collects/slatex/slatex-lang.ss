
(module slatex-lang mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred"))
  (provide tool@)
  
  (define o (current-output-port))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (add-slatex-language))
      
      (define (printf . args) (apply fprintf o args))
      
      (define (add-slatex-language)
        (let* ([extras-mixin
                (lambda (mred-launcher? one-line-summary)
                  (lambda (%)
                    (class %
                      (define/override (get-one-line-summary) one-line-summary)
                      (define/override (use-namespace-require/copy?) #t)
                      
                      (define/override (front-end input settings) (slatex-front-end input))
                      
                      (inherit get-module get-transformer-module get-init-code)
                      (define/override (create-executable setting parent program-filename)
                        (let ([executable-fn
			       (drscheme:language:put-executable
				parent
				program-filename
				mred-launcher?
				#t
				(if mred-launcher?
				    (string-constant save-a-mred-launcher)
				    (string-constant save-a-mzscheme-launcher)))])
                          (when executable-fn
                            (drscheme:language:create-module-based-launcher
                             program-filename
                             executable-fn
                             (get-module)
                             (get-transformer-module)
                             (get-init-code setting)
                             mred-launcher?
                             (use-namespace-require/copy?)))))
                      (super-instantiate ()))))]
               [make-simple
                (lambda (module position numbers mred-launcher? one-line-summary)
                  (let ([%
                         ((extras-mixin mred-launcher? one-line-summary)
                          ((drscheme:language:get-default-mixin)
                           (drscheme:language:module-based-language->language-mixin
                            (drscheme:language:simple-module-based-language->module-based-language-mixin
                             drscheme:language:simple-module-based-language%))))])
                    (instantiate % ()
                      (module module)
                      (language-position position)
                      (language-numbers numbers))))])
          (drscheme:language-configuration:add-language
           (make-simple '(lib "plt-pretty-big.ss" "lang")
                        (list (string-constant plt)
                              "SLaTeX")
                        (list -10 9)
                        #t
                        "evaluates code between \\begin{schemedisplay} and \\end{schemedisplay}"))))
      
      ;; slatex-front-end : text/pos -> (-> (union eof syntax))
      (define (slatex-front-end input)
        (let* ([text (drscheme:language:text/pos-text input)]
               [frame (send (send text get-canvas) get-top-level-window)])
          (let-values ([(port source offset line col)
                        (cond
                          [(eq? text (send frame get-definitions-text))
                           (get-slatex-removed-stuff input)]
                          [else
                           (get-regular-stuff input)])])
            (port-count-lines! port)
            (let ([closed? #f])
              (lambda ()
                (if closed?
                    eof
                    (let ([result (read-syntax source port (list line col offset))])
                      (if (eof-object? result)
                          (begin
                            (set! closed? #t)
                            (close-input-port port)
                            eof)
                          result))))))))

      ;; get-regular-stuff : ...
      (define (get-regular-stuff input)
        (let* ([text (drscheme:language:text/pos-text input)]
               [start (drscheme:language:text/pos-start input)]
               [end (drscheme:language:text/pos-end input)]
               [start-line (send text position-paragraph start)]
               [start-col (- start (send text paragraph-start-position start-line))])
          (let ([port (open-input-text-editor text start end)])
            (values port
                    text
                    start
                    start-line
                    start-col))))
      
      (define begin-scheme-display "\\begin{schemedisplay}")
      (define end-scheme-display "\\end{schemedisplay}")
      (define (get-slatex-removed-stuff input)
        (let* ([text (drscheme:language:text/pos-text input)]
               [start (drscheme:language:text/pos-start input)]
               [end (drscheme:language:text/pos-end input)]
               [next-end-position #f]
               [find-next-begin-scheme-region
                (lambda (pos)
                  (let ([start (send text find-string begin-scheme-display 'forward pos 'eof #f)])
                    (and start
                         (let ([end (send text find-string end-scheme-display 'forward start 'eof #t)])
                           (and end
                                (begin
                                  (send text split-snip start)
                                  (send text split-snip end)
                                  (set! next-end-position end)
                                  (send text find-snip start 'after-or-none)))))))]
               [choose-next-snip
                (lambda (snip)
                  (cond
                    [next-end-position
                     (let ([next-candidate (send snip next)])
                       (if next-candidate
                           (let ([candidate-pos (send text get-snip-position next-candidate)])
                             (if (candidate-pos . < . next-end-position)
                                 next-candidate
                                 (find-next-begin-scheme-region next-end-position)))
                           #f))]
                    [else (find-next-begin-scheme-region start)]))]
               [end (drscheme:language:text/pos-end input)]
               [start-line (send text position-paragraph start)]
               [start-col (- start (send text paragraph-start-position start-line))])
          (values
           (open-input-text/choose-snips text choose-next-snip)
           text
           start
           start-line
           start-col)))
  
      ;; open-input-text/choose-snips : (instanceof text%) 
      ;;                                ((union #f snip) -> (union #f snip))
      ;;                             -> input-port
      ;; creates a user port whose input is taken from the text%,
      ;; starting at position `start-in' (taking into account #!)
      ;; and ending at position `end'.
      (define (open-input-text/choose-snips text choose-next-snip)
        (let* ([snip (choose-next-snip #f)]
               [str #f]
               [pos 0]
               [update-str-to-snip
                (lambda ()
                  (cond
                    [(not snip)
                     (set! str #f)]
                    [(is-a? snip string-snip%)
                     (set! str (send snip get-text 0 (send snip get-count)))]
                    [else
                     (set! str 'snip)]))]
               [next-snip
                (lambda ()
                  (set! snip (choose-next-snip snip))
                  (set! pos 0)
                  (update-str-to-snip))]
               [read-char (lambda ()
                            (cond
                              [(not str) eof]
                              [(string? str)
                               (begin0 (string-ref str pos)
                                       (set! pos (+ pos 1))
                                       (when ((string-length str) . <= . pos)
                                         (next-snip)))]
                              [(eq? str 'snip)
                               (begin0
                                 (let ([the-snip snip])
                                   (lambda (file line col pos)
                                     (values (send the-snip copy) 1)))
                                 (next-snip))]))]
               [char-ready? (lambda () #t)]
               [close (lambda () (void))]
               [port (make-custom-input-port 
                      #f
                      (lambda (s)
                        (let ([c (read-char)])
                          (if (char? c)
                              (begin
                                (string-set! s 0 c)
                                1)
                              c)))
                      #f ; no peek
                      close)])
          (update-str-to-snip)
          port)))))