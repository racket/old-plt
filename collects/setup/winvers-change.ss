;; This thing will crawl over a given tree (on the command-line, defaults to
;; the current directory) , and look for "xxxxxxx" things to replace by the
;; version number, which should be done on a Windows *binary* tree before
;; packing a distribution.  It will rename files with "xxxxxxx" in their name
;; by a "NNN_NNN" version number, and will also do this rewrite in all files.
;; (Actually looking for a few known string templates, to be safe.)  Note that
;; this is done *in-place*, so it will not work from a running MzScheme.exe on
;; Windows -- "winvers.ss" uses a trick of making a copy of the binary and
;; restarting that copy for the actual change.

(module winvers-change mzscheme

  (define verbose? #t)
  (define binary-extensions '(exe dll lib obj o so def exp))
  (define x-len 7)
  (define x-re          (format "lib(?:mzsch|mzgc|mred)(?:|3m)(~a)"
                                (make-string x-len #\x)))
  (define renaming      (regexp (format "^~a[.](?:dll|lib)$" x-re)))
  (define substitutions (map (lambda (s) (format s x-re))
                             '("~a[.](?:dll|lib)\0"
                               "~a_NULL_THUNK_DATA\0"
                               "__IMPORT_DESCRIPTOR_~a\0"
                               "__head_~a_lib\0"
                               "__~a_lib_iname\0")))

  (define version-string
    (cond [(regexp-match "^([0-9]+)(?:[.]([0-9]+))?$" (version)) =>
           (lambda (m)
             (let ([major (cadr m)] [minor (or (caddr m) "")])
               (string-append (cadr m) "_"
                              (make-string (- x-len 1
                                              (string-length major)
                                              (string-length minor))
                                           #\0)
                              minor)))]
          [else (error 'winvers-change "unexpected version string: ~s"
                       (version))]))

  (define (string-downcase str)
    (list->string (map char-downcase (string->list str))))

  (define (binary-file? filename)
    (cond [(regexp-match #rx"[.]([^.]+)$" filename) =>
           (lambda (m)
             (memq (string->symbol (string-downcase (cadr m)))
                   binary-extensions))]
          [else #f]))

  (define (do-file file)
    (when (binary-file? file)
      (let ([dfile (string-downcase file)])
        (cond [(regexp-match-positions renaming dfile) =>
               (lambda (m)
                 (let ([new (string-append (substring dfile 0 (caadr m))
                                           version-string
                                           (substring dfile (cdadr m)))])
                   (when verbose?
                     (printf "Renaming: ~a/~a -> ~a\n"
                             (current-directory) file new))
                   (rename-file-or-directory file new)
                   (set! file new)))]))
      (let-values ([(i o)    (open-input-output-file file 'update)]
                   [(print?) verbose?])
        (for-each (lambda (subst)
                    (file-position i 0)
                    (let loop ([pos 0])
                      (cond [(regexp-match-positions subst i) =>
                             (lambda (m)
                               (when print?
                                 (printf "Changing: ~a/~a\n"
                                         (current-directory) file)
                                 (set! print? #f))
                               (file-position o (+ pos (caadr m)))
                               (display version-string o)
                               (flush-output o)
                               (file-position i (+ pos (cdar m)))
                               (loop (+ pos (cdar m))))])))
                  substitutions)
        (close-input-port i)
        (close-output-port o))))

  (parameterize ([current-directory
                  (let ([args (vector->list
                               (current-command-line-arguments))])
                    (if (null? args) "." (car args)))])
    (let loop ([files '(".")])
      (when (pair? files)
        (cond [(file-exists? (car files)) (do-file (car files))]
              [(directory-exists? (car files))
               (parameterize ([current-directory (car files)])
                 (loop (directory-list)))])
        (loop (cdr files)))))

)
