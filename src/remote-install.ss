#!/bin/sh

string=? ; exec "$PLTHOME/bin/mzscheme" -qr $0 "$@"

(use-compiled-file-kinds 'none)
(require (lib "cmdline.ss")
	 (lib "process.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  copying from other installation
;;

(define (copy-from-other)
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

(define (get-remote-version)
  (let ([cmd
         (format
          "ssh ~a 'setenv PLTHOME ~a ; $PLTHOME/bin/mzscheme -qmve \"(write (version))\"'"
          from-remote-host
          src-plt-home)])
    (let-values ([(out in pid err status-proc) (apply values (process cmd))])
      (begin0
        (read out)
        (close-output-port in)
        (close-input-port out)
        (close-input-port err)))))
  
  (define stupid-internal-define-syntax2
    (command-line
     "remote-install.ss installation"
     (list->vector (cdr (vector->list argv)))
     (once-any
      (("-f" "--from-remote") remote-host
       "the remote host to install the binaries from"
       (set! to-remote-host remote-host))
      (("-t" "--to-remote") remote-host
       "the remote host to install the binaries on"
       (set! from-remote-host remote-host)))
     (args (src-plt-home-directory dest-plt-home-directory)
           (set! src-plt-home src-plt-home-directory)
           (set! dest-plt-home dest-plt-home-directory))))
  
  ;; vers : string
  (define vers
    (if from-remote-host
        (get-remote-version)
        (version)))
  
  (define stupid-internal-define-syntax1
    (printf "Installation version ~a\n" vers))
  
  (define-struct pr (from to))
  
  (define plt-relative-files
    (case (system-type)
      [(macosx) (list (make-pr (build-path "MrEd.app")
                               (build-path 'same))
                      (make-pr (build-path "MrEd3m.app")
                               (build-path 'same))
                      (make-pr (build-path "collects" "launcher" "Starter.app")
                               (build-path "collects" "launcher"))
                      (make-pr (build-path "collects" "launcher" "Starter3m.app")
                               (build-path "collects" "launcher"))
                      (make-pr (build-path "bin" "mzscheme")
                               (build-path "bin"))
                      (make-pr (build-path "bin" "mzscheme3m")
                               (build-path "bin"))
                      (make-pr (build-path "bin" "mred")
                               (build-path "bin"))
                      (make-pr (build-path "bin" "mred3m")
                               (build-path "bin"))
                      (make-pr (build-path "lib" "mzdyn.o")
                               (build-path "lib"))
                      (make-pr (build-path "lib" "libmzgc.a")
                               (build-path "lib"))
                      (make-pr (build-path "lib" "libmzscheme.a")
                               (build-path "lib"))
                      (make-pr (build-path "lib" "mzdyn3m.o")
                               (build-path "lib")))]
      [else (error 'remote-install.ss "only works for macos x")]))
  
  (define home-directory-relative-files
    (case (system-type)
      [(macosx) (list (make-pr (build-path "Library"
                                           "Frameworks"
                                           "PLT_MrEd.framework"
                                           "Versions"
                                           vers)
                               (build-path "Library"
                                           "Frameworks"
                                           "PLT_MrEd.framework"
                                           "Versions"))
                      (make-pr (build-path "Library"
                                           "Frameworks"
                                           "PLT_MrEd.framework"
                                           "Versions"
                                           (string-append vers "_3m"))
                               (build-path "Library"
                                           "Frameworks"
                                           "PLT_MrEd.framework"
                                           "Versions"))
                      (make-pr (build-path "Library"
                                           "Frameworks"
                                           "PLT_MzScheme.framework"
                                           "Versions"
                                           (string-append vers "_3m"))
                               (build-path "Library"
                                           "Frameworks"
                                           "PLT_MzScheme.framework"
                                           "Versions"))
                      (make-pr (build-path "Library"
                                           "Frameworks"
                                           "PLT_MzScheme.framework"
                                           "Versions"
                                           vers)
                               (build-path "Library"
                                           "Frameworks"
                                           "PLT_MzScheme.framework"
                                           "Versions")))]
      [else '()]))
  
  (define (do-copy src-rel dest-rel)
    (lambda (pr)
      (unless to-remote-host
        (let ([path (build-path dest-rel (pr-to pr))])
          (unless (directory-exists? path)
            (printf "(make-directory ~s)\n" path)
            (make-directory path))))
      (let ([cmd
             (format "scp -C -r ~a~a ~a~a"
                     (if from-remote-host
			 (format "~a:" from-remote-host)
			 "")
                     (path->string (build-path src-rel (pr-from pr)))
                     (if to-remote-host (format "~a:" to-remote-host) "")
                     (path->string (build-path dest-rel (pr-to pr))))])
        (printf "~a\n" cmd)
        (system cmd))))
  
  (for-each (do-copy src-plt-home dest-plt-home) plt-relative-files)
  
  (when (or from-remote-host to-remote-host)
    (for-each (do-copy "~" "~") home-directory-relative-files)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; copying from download.plt-scheme.org
;;

(define libraries/frameworks
  "http://download.plt-scheme.org/scheme/binaries/ppc-osx-mac/plt-ppc-osx-mac-frameworks.tgz")

(define binaries
  "http://download.plt-scheme.org/scheme/binaries/ppc-osx-mac/plt-ppc-osx-mac-binaries.tgz")

(define (copy-from-download.plt-scheme.org)
  (define plt-home #f)
  
  (command-line
   "remote-install.ss download"
   (list->vector (cdr (vector->list argv)))
   (args (plt-home-directory)
         (let-values ([(base name dir?) (split-path plt-home-directory)])
           (unless (equal? name "plt")
             (error 'remote-install.ss "expected plt home to end with `plt', got ~s" name))
           (set! plt-home base))))
  
  (system (format "wget -O - ~a | tar xzf - -C ~~" 
                  libraries/frameworks))
  (system (format "wget -O - ~a | tar xzf - -C ~a" 
                  binaries
                  plt-home)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; go
;;

(define legal-starts `("download" "installation"))


(cond
  [(eq? (vector) argv) 
   (error 'remote-install.ss "expected one of ~s as first argument" legal-starts)]
  [(equal? (vector-ref argv 0) "installation")
   (copy-from-other)]
  [(equal? (vector-ref argv 0) "download")
   (copy-from-download.plt-scheme.org)]
  [else
   (error 'remote-install.ss "expected one of ~s as first argument" legal-starts)])
