
(module sample-solutions-one-window mzscheme
  (require "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "class.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (prefix fw: (lib "framework.ss" "framework")))
  
  (provide run-test)
  
  (define (section->language section)
    (cond
      [(section . <= . 12) '("How to Design Programs" "Beginning Student")]
      [(section . <= . 29) '("How to Design Programs" "Intermediate Student")]
      [else '("How to Design Programs" "Advanced Student")]))

  (define sample-solutions-teachpack-filename
    (build-path (collection-path "tests" "drscheme") "sample-solutions-testsuite-tp.scm"))
  
  (define sample-solutions-dir
    (let ([try1
           (collection-path "solutions")]
          [try2
           (build-path (collection-path "mzlib")
                       'up
                       'up
                       'up
                       "robby"
                       "collects"
                       "solutions")])
      (cond
        [(directory-exists? try1) try1]
        [else try2])))
  
  (unless (directory-exists? sample-solutions-dir)
    (error 'sample-solutions.ss "expected directory ~s to exist" sample-solutions-dir))
  
  (set! sample-solutions-dir (normalize-path sample-solutions-dir))
  
  (define toc (call-with-input-file (build-path sample-solutions-dir "toc.ss") read))
  (define default-toc-entry '(#f ()))
  
  (define labels
    (let* ([all-info (call-with-input-file (build-path (collection-path "solutions") 
                                                       'up 'up "proj" "book" "solutions"
                                                       "labels.scm") read)]
           [ex-labels (filter (lambda (x) (and (string=? (substring (car x) 0 3) "ex:")
                                               (> (string-length (car x)) 3)))
                              all-info)])
      (map (lambda (x) 
             (cons (string-append (substring (car x) 3 (string-length (car x))) ".scm")
                   (cdr x)))
           ex-labels)))
  
  (define sample-solutions
    (filter (lambda (x) (and 
                         (> (string-length x) 3)
                         (string=? "scm" (substring x (- (string-length x) 3) (string-length x)))
                         (memf (lambda (y) (string=? (car y) x)) labels)))
            (directory-list sample-solutions-dir)))

  (define separator-sexp "should be")
  
  (define (test-single-file filename)
    (let* ([toc-entry (let ([lookup (assoc (string->symbol filename) toc)])
                        (if lookup
                            (cdr lookup)
                            default-toc-entry))]
           [label (car (memf (lambda (x) (string=? (car x) filename)) labels))]
           [language (section->language (car (cadr label)))]
           [errors-ok? (car toc-entry)]
           [teachpacks (cadr toc-entry)])
      
      (let* ([drs-frame (wait-for-drscheme-frame)]
             [definitions-text (send drs-frame get-definitions-text)]
             [interactions-text (send drs-frame get-interactions-text)])
        
        ;; update the program (cheat to hack around gc bug -- should really use file|open)
        (send definitions-text load-file (build-path sample-solutions-dir filename))
        
        ;; only bother changing the language dialog when necessary.
        (unless (string=?
                 (send interactions-text get-text
                       (send interactions-text paragraph-start-position 1)
                       (send interactions-text paragraph-end-position 1))
                 (format "Language: ~a." (car (last-pair language))))
          (set-language-level! language))
        
        ;; only bother changing the teachpacks when necessary.
        (let* ([get-full-path
                (lambda (teachpack)
                  (cond
                    [(absolute-path? teachpack)
                     (normal-case-path 
                      (normalize-path
                       teachpack))]
                    [else
                     (normal-case-path
                      (normalize-path
                       (build-path
                        (collection-path "mzlib") 'up 'up
                        "teachpack" "htdp" teachpack)))]))]
               [teachpack-should-be
                (apply string-append 
                       (map (lambda (tp) (format "Teachpack: ~a.~n" (get-full-path tp)))
                            (cons
                             sample-solutions-teachpack-filename
                             teachpacks)))]
               [teachpack-is
                (send interactions-text get-text
                      (send interactions-text paragraph-start-position 2)
                      (send interactions-text paragraph-start-position 
                            (+ 2 (length teachpacks) 1)))] ;; add 1 for the always there teachpack
               [teachpacks-already-set? (string=? teachpack-should-be teachpack-is)])
          (unless teachpacks-already-set?
            (fw:test:menu-select "Language" "Clear All Teachpacks")
            (use-get/put-dialog
             (lambda ()
               (fw:test:menu-select "Language" "Add Teachpack..."))
             sample-solutions-teachpack-filename)
            (for-each (lambda (teachpack)
                        (use-get/put-dialog
                         (lambda ()
                           (fw:test:menu-select "Language" "Add Teachpack..."))
                         (get-full-path teachpack)))
                      teachpacks)))
        
        (do-execute drs-frame)
        
        ;; kill the program, but only when it opens windows
        (unless (eq? drs-frame (get-top-level-focus-window))
          (custodian-shutdown-all (send interactions-text get-user-custodian))
          (let ([wait-for-kill-window
                 (lambda ()
                   (let ([f (get-top-level-focus-window)])
                     (and f (equal? (send f get-label)
                                    "Evaluation Terminated"))))])
            (poll-until wait-for-kill-window)
            (fw:test:button-push "OK")
            (wait-for-drscheme-frame #f)))
        
        ;; still check equal pairs when there is a sanctioned error.
        (cond
          [(and (not errors-ok?)
                (has-error? drs-frame))
           =>
           (lambda (err-msg)
             (printf "ERROR: ~a: found error, but should be no errors:~n  ~a\n  teachpacks: ~a\n"
                     filename
                     err-msg
                     teachpacks))]
          [else
           (let* ([output (fetch-output drs-frame)]
                  [port (open-input-string output)])
             (let loop ([last #f]
                        [equal-count 1])
               (let ([sexp (with-handlers ([(lambda (exn) #t)
                                            (lambda (exn) exn)])
                             (read port))])
                 (unless (eof-object? sexp)
                   (cond
                     [(and (not last) (equal? sexp separator-sexp))
                      (printf "ERROR: ~a: found = as first sexp~n" filename)]
                     [(and last (equal? separator-sexp sexp))
                      (let ([after (with-handlers ([(lambda (exn) #t)
                                                    (lambda (exn) exn)])
                                     (read port))])
                        (unless (equal? after last)
                          (printf "ERROR: ~a: pair #~a mismatched.\n     got ~s\nexpected ~s\nteachpacks: ~a\n"
                                  filename equal-count 
                                  (if (exn? last) (exn-message last) last)
                                  (if (exn? after) (exn-message after) after)
                                  teachpacks))
                        (loop after (+ equal-count 1)))]
                     [else (loop sexp equal-count)])))))]))))
  (define (run-test)
    (for-each test-single-file sample-solutions)))