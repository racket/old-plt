#!/bin/sh

string=? ; exec mzscheme/mzscheme -qr $0 "$@"

(require (lib "cmdline.ss"))

;; from-remote-host : (union string #f)
(define from-remote-host #f)

;; to-remote-host : (union string #f)
(define to-remote-host #f)

;; src-plt-home : string
;; (must be initialized by commandline parsing)
(define src-plt-home #f)

;; dest-plt-home : string
;; (must be initialized by commandline parsing)
(define dest-plt-home #f)

(let ([trh (lambda (x) (set! to-remote-host x))]
      [frh (lambda (x) (set! from-remote-host x))])
  (command-line
   "remote-install.ss"
   argv
   (once-any
    (("-f" "--from-remote") remote-host
     "the remote host to install the binaries from"
     (frh remote-host))
    (("-t" "--to-remote") remote-host
     "the remote host to install the binaries on"
     (trh remote-host)))
   (args (src-plt-home-directory dest-plt-home-directory)
         (set! src-plt-home src-plt-home-directory)
         (set! dest-plt-home dest-plt-home-directory))))

(define-struct pr (from to))

(define plt-relative-files
  (case (system-type)
    [(macosx) (list (make-pr (build-path "mred" "mred")
                             (build-path "bin"))
                    (make-pr (build-path "mred" "mred3m")
                             (build-path "bin"))
                    (make-pr (build-path "mred" "Starter.app")
                             (build-path "collects" "launcher"))
                    (make-pr (build-path "mzscheme" "mzscheme")
                             (build-path "bin"))
                    (make-pr (build-path "mzscheme" "mzscheme3m")
                             (build-path "bin"))
                    (make-pr (build-path "mzscheme" "dynsrc" "mzdyn.c")
                             (build-path "collects" "mzscheme" "lib")))]
    [else (error 'remote-install.ss "only works for macos x")]))

(define home-directory-relative-files
  (case (system-type)
    [(macosx) (list (make-pr (build-path "mred"
                                         "PLT_MrEd.framework"
                                         "Versions"
                                         (version))
                             (build-path "Library"
                                         "Frameworks"
                                         "PLT_MrEd.framework"
                                         "Versions"))
                    (make-pr (build-path "mzscheme"
                                         "PLT_MzScheme.framework"
                                         "Versions"
                                         (version))
                             (build-path "Library"
                                         "Frameworks"
                                         "PLT_MzScheme.framework"
                                         "Versions")))]
    [else '()]))

(define (do-copy src-rel dest-rel)
  (lambda (pr)
    (printf
     "~a"
     (format "scp -r ~a~a ~a~a"
             (if from-remote-host (format "~a:" from-remote-host) "")
             (build-path src-rel (pr-from pr))
             (if to-remote-host (format "~a:" to-remote-host) "")
             (build-path dest-rel (pr-to pr))))
    (newline)))

(for-each (do-copy src-plt-home dest-plt-home) plt-relative-files)
(for-each (do-copy "~" "~") home-directory-relative-files)
