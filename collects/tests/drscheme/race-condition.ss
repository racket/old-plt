
(module race-condition mzscheme
  (require "drscheme-test-util.ss"
           (lib "class.ss")
           (lib "list.ss")
           (lib "framework.ss" "framework")
           (prefix fw: (lib "framework.ss" "framework")))
  
  (provide run-test)

  (define language (make-parameter "<<not a language>>"))
  
  (define (check-top-of-repl)
    (let ([drs (wait-for-drscheme-frame)])
      (set-language #t)
      (do-execute drs)
      (let* ([interactions (send drs get-interactions-text)]
             [short-lang (car (last-pair (language)))]
             [get-line (lambda (n) (send interactions get-text 
                                         (send interactions paragraph-start-position n)
                                         (send interactions paragraph-end-position n)))]
             [line0-expect (format "Welcome to DrScheme, version ~a." (version:version))]
             [line1-expect 
              (if (string? short-lang)
                  (format "Language: ~a." short-lang)
                  short-lang)]
             [line0-got (get-line 0)]
             [line1-got (get-line 1)])
        (unless (and (string=? line0-expect line0-got)
                     (if (string? short-lang)
                         (string=? line1-expect line1-got)
                         (regexp-match line1-expect line1-got)))
          (printf "expected lines: ~n  ~a~n  ~a~ngot lines:~n  ~a~n  ~a~n" 
                  line0-expect line1-expect
                  line0-got line1-got)
          (error 'race-condition.ss "failed get top of repl test")))))
  
  ;; set-language : boolean -> void
  (define (set-language close-dialog?)
    (set-language-level! (language) close-dialog?))

  (define (run-test)
    ;; clear teachpack
    (let ([drs (wait-for-drscheme-frame)])
      (fw:test:menu-select "Language" "Clear All Teachpacks"))
    
    ;; this exposes the race condition, after it runs for a while.
    (let loop ()
      (parameterize ([language (list "PLT" (regexp "Graphical"))])
        (check-top-of-repl))
      (parameterize ([language (list "PLT" (regexp "Textual"))])
        (check-top-of-repl))
      (loop))))