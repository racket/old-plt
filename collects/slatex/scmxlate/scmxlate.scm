
(cond ((not 'nil)
       ;Common Lisp
       (defvar *using-scmxlate-p* t)
       (load
        (merge-pathnames
          (make-pathname :type "cl")
          *load-pathname*)))) 

;(require (lib "trace.ss"))

(define *scmxlate-version* "0m")
(define *dialect* #f)
(define *operating-systems-supported* '())
(define *operating-system* #f)
(define *compile?* #f)
(define *shell-script?* #f)
(define *disable-main?* #f)
(define *names-defined* '())
;(define *names-ignored* '())
;(define *names-disabled* '())
(define *aliases* '())
;(define *predefined-aliases* '())
(define *target-file* #f)

(define *cr* #f)
(define *lf* #f)

'(define string->lower-case
  (lambda (s)
    (list->string
     (map char-downcase
          (string->list s)))))

;string->list doesn't work in scsh!

(define string->lower-case
  (lambda (s)
    (let loop ((i (- (string-length s) 1)) (r '()))
      (if (< i 0) (list->string r)
          (loop (- i 1)
            (cons (char-downcase (string-ref s i)) r))))))

(define read-a-line
  (lambda (i)
    (list->string
     (let loop ()
       (let ((c (read-char i)))
         (if (or (eof-object? c)
                 (char=? c #\newline))
             '()
             (cons c (loop))))))))

(define resolve-aliases
  (lambda (e)
    (cond ((pair? e)
           (cons (resolve-aliases (car e))
                 (resolve-aliases (cdr e))))
          ((symbol? e)
           (cond ((assv e *aliases*) => cdr)
                 (else e)))
          (else e))))

(define copy-port-to-port
  (lambda (i o)
    (let loop ()
      (let ((c (read-char i)))
        (if (not (eof-object? c))
          (begin
            (if (eqv? *operating-system* 'windows)
                (cond ((char=? c *cr*) 'skip)
                      ((char=? c *lf*) (display *cr* o)
                                       (display *lf* o))
                      (else (display c o)))
                (display c o))
            (loop)))))))

(define copy-file-to-port
  (lambda (f o)
    (call-with-input-file f
      (lambda (i)
        (copy-port-to-port i o)))))




(define *files-to-be-ported*
  (call-with-input-file "dialects/files-to-be-ported.scm"
    (lambda (i)
      (let loop ()
        (let ((f (read i)))
          (if (eof-object? f) '()
              (cons (string->lower-case (symbol->string f))
                    (loop))))))))

(define *dialects-supported*
  (call-with-input-file "dialects/dialects-supported.scm"
    (lambda (i)
      (let loop ()
        (let ((d (read i)))
          (if (eof-object? d) '()
              (cons d (loop))))))))

(set! *dialect*
  (begin
   (display "What is your Scheme dialect?")
   (newline) (display "     ") (display "(")
   (let loop ((dd *dialects-supported*) (i 0))
     (cond ((null? dd) 
            (display "other)")
            (newline))
           (else
            (if (>= i 5) 
                (begin (set! i 0) 
                       (newline)
                       (display "     ")
                       (display " ")))
            (display (car dd)) (display " ")
            (loop (cdr dd) (+ i 1)))))
   (read)))

(if (eqv? *dialect* 'sxm)
    ;sxm issues warnings for forward-refs,
    ;which can't all be removed anyway
    (warning-handler (lambda zzz #f)))

(if (eqv? *dialect* 'scheme48)
    (begin
      (display "Structures") (newline)
      (for-each
        (lambda (str)
          (display "  ") (display str) (newline))
        '(c-system-function extended-ports posix-files posix-process-data))
      (display "must be open before you configure or run tex2page.")
      (newline)
      (display "If they aren't open, please open and retry.")
      (newline)))

(define exists-file?
  (case *dialect*
    ((bigloo chez gambit guile kawa mitscheme mzscheme petite plt scm scsh stk 
             stklos sxm umbscheme)
     file-exists?)
    ((scheme48)
     (lambda (f)
       (accessible? f (access-mode read))))
    ((pscheme)
     (lambda (f)
       (with-handlers (((lambda (x) #t) (lambda (x) #f)))
         (close-input-port (open-input-file f))
         #t)))
    (else (lambda (f) #t))))

(define obliterate-file
  (case *dialect*
    ((bigloo chez guile kawa mitscheme mzscheme petite plt pscheme 
             scsh scm sxm umbscheme) 
     delete-file)
    ((scheme48) unlink)
    ((stk stklos)
     (lambda (f)
       (system (string-append "rm " f))))
    ((gambit)
     ;## causes problems with other Scheme dialects
     (lambda (f)
       ((eval (call-with-input-string "##shell-command" read))
        (string-append "rm " f))))
    (else (lambda (f) #t))))

(define ensure-file-deleted
  (lambda (f)
    (if (exists-file? f) (obliterate-file f))))

(if (exists-file? "dialects/operating-systems-supported.scm")
    (set! *operating-systems-supported*
      (call-with-input-file "dialects/operating-systems-supported.scm"
        (lambda (i)
          (let loop ()
            (let ((s (read i)))
              (if (eof-object? s) '()
                  (cons s (loop)))))))))

(set! *operating-system*
  (case (length *operating-systems-supported*)
    ((0) 
     ;if no OSes mentioned, assume unix
     'unix)
    ((1) 
     ;if only one OS mentioned, choose it right away
     (car *operating-systems-supported*))
    (else
     (case *dialect*
       ((bigloo chez gambit guile mzscheme petite plt 
                scm scsh stk stklos sxm)
        (cond ((getenv "COMSPEC") 'windows)
              (else 'unix)))
       ((scheme48)
        (cond ((lookup-environment-variable "COMSPEC") 'windows)
              (else 'unix)))
       ((mitscheme)
        (cond ((get-environment-variable "COMSPEC") 'windows)
              (else 'unix)))
       ((pscheme) 'windows)
       ((umbscheme) 'unix)
       (else
        (display "What is your operating system? (")
        (let ((first? #t))
          (for-each
           (lambda (os)
             (if first? (set! first? #f)
                 (display " "))
             (display os))
           *operating-systems-supported*))
        (display ")")
        (newline)
        (read))))))

(define integer-to-char
  (lambda (n)
    (integer->char
     (if (memv *dialect* '(scheme48 scsh))
         (+ 1000 n)
         n))))

(if (eqv? *operating-system* 'windows)
    (begin
     (set! *cr* (integer-to-char 13))
     (set! *lf* (integer-to-char 10))))

;get "system" for PLT Scheme

(if (memv *dialect* '(mzscheme plt))
  (if (char=? (string-ref (version) 0) #\2)
    (eval '(require (lib "process.ss")))))

;get "pretty-print" for PLT Scheme 

(if (memv *dialect* '(mzscheme plt))
  (if (char=? (string-ref (version) 0) #\2)
    (eval '(require (lib "pretty.ss")))))

;get compiler for MzScheme 

(if (eqv? *dialect* 'mzscheme)
    (eval
      (if (char=? (string-ref (version) 0) #\2)
        '(require (lib "compile.ss"))
        '(require-library "compile.ss"))))

(define *dialect-s*
  (string->lower-case (symbol->string *dialect*)))

(define *operating-system-s*
  (if (null? *operating-systems-supported*) ""
      (string-append
       "-"
       (string->lower-case
        (symbol->string *operating-system*)))))

(define writeln
  (lambda (e o)
    (write (resolve-aliases e) o)
    (newline o)
    (newline o)))

;redefine writeln to use pretty-printer
;for dialects that have it

(if (memv *dialect* '(gambit mzscheme petite plt sxm))
    (set! writeln
      (lambda (e o)
        (pretty-print (resolve-aliases e) o)
        (newline o))))

(define bigloo-sanitize-colons
  (lambda (s)
    (let ((n (string-length s)))
      ;change trailing :'s to $'s
      (let loop ((i (- n 1)) (r '()))
        (if (< i 0) (list->string (reverse! r))
            (let ((c (string-ref s i)))
              (if (char=? c #\:)
                  (loop (- i 1) (cons #\$ r))
                  (string-append
                   (substring s 0 (+ i 1))
                   (list->string (reverse! r))))))))))

(define names-defined
  (lambda (x)
    (let ((r '()))
      (let loop ((x x))
        (if (pair? x)
            (let ((n (length x)))
              (cond ((and (> n 2) 
                          (memv (car x) 
                                '(define define-macro define-syntax 
                                   defmacro
                                   defstruct)))
                     (set! r
                       (cons
                         (let ((name (cadr x)))
                           (if (pair? name) (car name) name))
                         r)))
                    ((and (>= n 2) (eqv? (car x) 'scmxlate-ignore))
                     (set! r
                       (append (cdr x) r)))
                    ((and (>= n 2) (eqv? (car x) 'scmxlate-rename-define))
                     (set! r
                       (append (map car (cdr x)) r)))
                    ((and (> n 3) (eqv? (car x) 'syntax-table-define))
                     (set! r
                       (cons
                         (cadr (caddr x)) 
                         r)))
                    ((and (> n 2) (memv (car x) '(begin if when unless)))
                     (for-each loop (cddr x)))))))
      r)))

(define translate-port-to-port
  (lambda (i o)
    (if (char=? (peek-char i) #\#)
        (begin
         (set! *shell-script?* #t)
         (read-a-line i)
         (display "; ensure shell-magic above" o)
         (newline o)
         ))
    (let loop ()
      (let* ((x (read i))
             (a (if (pair? x) (car x) #f))
             (names (names-defined x))
             (name (and (pair? names) (car names))))
        (if (not (eof-object? x))
            (begin
             (cond ((not (pair? x)) 'skip)
                   ((and (not (memv a '(scmxlate-ignore
                                        scmxlate-rename-define)))
                             (memv name *names-defined*))
                    'skip)
                   (else
                    (if (pair? names)
                        (set! *names-defined*
                          (append names *names-defined*)))
                    (case a 
                      ((scmxlate-rename scmxlate-rename-define)
                       (for-each
                        (lambda (x-y)
                          (let ((x (car x-y)))
                            ;(set! *names-defined* (cons x *names-defined*))
                            (set! *aliases*
                              (cons (cons x (cadr x-y))
                                    *aliases*))))
                        (cdr x)))
                      ((scmxlate-include)
                       (translate-file-to-port
                        (string-append "dialects/"
                                       (cadr x)) o))
                      ((scmxlate-prefix)
                       (let ((pfx (cadr x))) 
                         (if (eqv? *dialect* 'bigloo)
                             (set! pfx
                               (bigloo-sanitize-colons pfx)))
                         (for-each
                          (lambda (id)
                            (set! *aliases*
                              (cons (cons id
                                         (string->symbol
                                          (string-append pfx
                                           (symbol->string id))))
                                    *aliases*)))
                          (cddr x))))
                      ((scmxlate-target-file)
                       (set! *target-file* (cadr x)))
                      ((scmxlate-compile?)
                       (set! *compile?* (cadr x)))
                      ((scmxlate-disable-main)
                       (set! *disable-main?* #t)
                       (set! *names-defined*
                         (cons 'main *names-defined*)))
                      ((scmxlate-ignore) #f)
                      ((eval-if-mzscheme) #f)
                      ((define-macro) 
                       (writeln (translate-define-macro x) o))
                      ((define-syntax)
                       (writeln (translate-define-syntax x) o))
                      ((main)
                       (if (not *disable-main?*) (writeln x o)))
                      (else
                       (writeln x o)))))
             (loop)))))))

(define translate-file-to-port
  (lambda (f o)
    (call-with-input-file f
      (lambda (i)
        (translate-port-to-port i o)))))

(define translate-define-syntax
  (case *dialect*
    ((stk stklos)
     (lambda (e)
       `(define-macro (,(cadr e) . _args)
          (let ((datum->syntax-object (lambda (x y) y))
                (syntax-object->datum (lambda (x) x)))
            (,(caddr e) (cons ',(cadr e) _args))))))
    ((mitscheme)
     (lambda (e)
       `(syntax-table-define system-global-syntax-table ',(cadr e)
          (macro _args
                 (let ((datum->syntax-object (lambda (x y) y))
                       (syntax-object->datum (lambda (x) x)))
                   (,(caddr e) (cons ',(cadr e) _args)))))))
    ((scm kawa umbscheme)
     (lambda (e)
       `(defmacro ,(cadr e) _args
          (let ((datum->syntax-object (lambda (x y) y))
                (syntax-object->datum (lambda (x) x)))
            (,(caddr e) (cons ',(cadr e) _args))))))
    ((scheme48 scsh)
     (lambda (e)
       `(define-syntax ,(cadr e)
          (lambda (__form __rename __compare)
            (let ((datum->syntax-object (lambda (x y) y))
                  (syntax-object->datum (lambda (x) x)))
              (,(caddr e) __form))))))
    ((gambit guile bigloo pscheme)
     (lambda (e)
       (let ((e-t `(define-macro ,(cadr e)
                     (lambda _args
                       (let ((datum->syntax-object (lambda (x y) y))
                             (syntax-object->datum (lambda (x) x)))
                         (,(caddr e) (cons ',(cadr e) _args)))))))
         (if (eqv? *dialect* 'gambit)
             `(begin ,e-t
                     (eval ',e-t))
             e-t))))
    ((chez petite sxm)
     ;unlike Mz, these dialects don't allow 
     ;a general syntax object as datum->syntax-object's
     ;first arg; it's got to be an identifier 
     (lambda (e)
     `(define-syntax ,(cadr e)
        (let* ((old-datum->syntax-object datum->syntax-object)
               (datum->syntax-object
                 (lambda (so output)
                   (old-datum->syntax-object
                     (syntax-case so ()
                       ((k . stuff) (syntax k)))
                     output))))
          ,(caddr e)))))
    ((mzscheme plt)
     (lambda (e)
       (if *compile?* (eval e))
       e))
    (else 
      (lambda (e) e))))

(define translate-define-macro
  (case *dialect*
    ((stk stklos)
     (lambda (e)
       `(define-macro (,(cadr e) ,@(cadr (caddr e)))
          ,@(cddr (caddr e)))))
    ((mitscheme)
     (lambda (e)
       `(syntax-table-define system-global-syntax-table ',(cadr e)
          (macro ,@(cdr (caddr e))))))
    ((scm kawa umbscheme)
     (lambda (e)
       `(defmacro ,(cadr e) ,(cadr (caddr e))
          ,@(cddr (caddr e)))))
    ((scheme48 scsh)
     (lambda (e)
       `(define-syntax ,(cadr e)
          (lambda (__form __renamee __compare)
            (apply ,(caddr e) (cdr __form))))))
    ((gambit)
     (lambda (e)
       `(begin ,e
          (eval ',e))))
    ((chez petite sxm)
     (lambda (e)
       `(define-syntax ,(cadr e)
          (lambda (x)
            (syntax-case x ()
              ((k . stuff)
               (datum->syntax-object (syntax k)
                 (apply ,(caddr e)
                   (cdr (syntax-object->datum x))))))))))
    ((mzscheme plt)
     (lambda (e)
       (if *compile?* (eval e))
       e))
    (else ;guile, bigloo, pscheme, etc
      (lambda (e) e))))

(define *predefined-aliases*
  (case *dialect*
    ((guile)
     '((load . primitive-load)
       (flush-output . force-output)))
    ((mitscheme)
     '((getenv . get-environment-variable)
       (open-input-string . string->input-port)
       (system . run-shell-command)
       ))
    ((chez petite sxm)
     '((flush-output . flush-output-port)
       ))
    ((gambit)
     `((get-output-string . close-output-port)
       (system . ,(call-with-input-string "##shell-command" read))
       ))
    ((scheme48)
     '((delete-file . unlink)
       (getenv . lookup-environment-variable)
       (open-output-string . make-string-output-port)
       (get-output-string . string-output-port-output)
       ))
    ((scsh)
     '((flush-output . force-output)
       (get-output-string . string-output-port-output)
       (open-output-string . make-string-output-port)))
    (else '())))

(cond ((not (memv *dialect* *dialects-supported*))
       (display "Sorry, dialect ")
       (display *dialect*)
       (display " is not supported. :-<")
       (newline))
      ((not (or (memv *operating-system* '(unix windows)) 
                (memv *operating-system* *operating-systems-supported*)))
       (display "Sorry, operating system ")
       (display *operating-system*)
       (display " is not supported.")
       (newline))
      (else
       (for-each
        (lambda (file-to-be-ported)
          (display "Porting ")
          (display file-to-be-ported)
          (display "...")
          (newline)
          (set! *shell-script?* #f)
          (set! *compile?* 'ask)
          (set! *disable-main?* #f)
          (set! *names-defined* '(eval-if-mzscheme))
          (set! *aliases* *predefined-aliases*) 
          (let ((dialect-start-file
                 (string-append "dialects/" *dialect-s*
                                "-start-"
                                file-to-be-ported))
                (dialect-os-start-file
                 (string-append "dialects/" *dialect-s*
                                *operating-system-s*
                                "-start-"
                                file-to-be-ported))
                (dialect-preamble-file
                 (string-append "dialects/" 
                                *dialect-s* 
                                "-preamble-"
                                file-to-be-ported))
                (dialect-os-preamble-file
                 (string-append "dialects/" 
                                *dialect-s* 
                                *operating-system-s*
                                "-preamble-"
                                file-to-be-ported))
                (user-override-file
                 (string-append "scmxlate-" file-to-be-ported))
                (dialect-override-file
                 (string-append "dialects/" *dialect-s* 
                                "-procs-"
                                file-to-be-ported))
                (dialect-os-override-file
                 (string-append "dialects/" *dialect-s*
                                *operating-system-s*
                                "-procs-"
                                file-to-be-ported))
                (dialect-postamble-file
                 (string-append "dialects/" *dialect-s*
                                "-postamble-" file-to-be-ported))
                (dialect-os-postamble
                 (string-append "dialects/" *dialect-s* 
                                *operating-system-s*
                                "-postamble-"
                                file-to-be-ported))
                (dialect-finish-file
                 (string-append "dialects/" *dialect-s*
                                "-finish-"
                                file-to-be-ported))
                (dialect-os-finish-file
                 (string-append "dialects/" *dialect-s*
                                *operating-system-s*
                                "-finish-"
                                file-to-be-ported)))
            (let ((start-file
                   (cond ((exists-file? dialect-os-start-file)
                          dialect-os-start-file)
                         ((exists-file? dialect-start-file)
                          dialect-start-file)
                         (else #f)))
                  (preamble-file
                   (cond ((exists-file? dialect-os-preamble-file)
                          dialect-os-preamble-file)
                         ((exists-file? dialect-preamble-file) 
                          dialect-preamble-file)
                         (else #f)))
                  (user-override-file
                   (if (exists-file? user-override-file) 
                       user-override-file #f))
                  (dialect-override-file
                   (if (exists-file? dialect-override-file) 
                       dialect-override-file #f))
                  (dialect-os-override-file
                   (if (exists-file? dialect-os-override-file) 
                       dialect-os-override-file #f))
                  (postamble-file
                   (cond ((exists-file? dialect-os-postamble)
                          dialect-os-postamble)
                         ((exists-file? dialect-postamble-file) 
                          dialect-postamble-file)
                         (else #f)))
                  (finish-file
                   (cond ((exists-file? dialect-os-finish-file) 
                          dialect-os-finish-file)
                         ((exists-file? dialect-finish-file) 
                          dialect-finish-file)
                         (else #f)))
                  (target-file
                   (string-append "my-"
                                  file-to-be-ported)))
              (ensure-file-deleted target-file)
              (if start-file
                  ((if (eqv? *dialect* 'guile)
                       primitive-load
                       load)
                   start-file))
              (call-with-output-file target-file
                (lambda (o)
                  (if  preamble-file
                      (call-with-input-file preamble-file
                        (lambda (i)
                          ;what abt shellmagic line if
                          ;compiling?
                          (copy-port-to-port i o)
                          (newline o))))
                  ;set names-defined nil?
                  (for-each
                   (lambda (f)
                     (if f (translate-file-to-port f o)))
                   (list user-override-file 
                         dialect-override-file 
                         dialect-os-override-file))
                  (newline o)
                  (display ";Configured for Scheme dialect " o)
                  (display *dialect* o)
                  (display " by scmxlate, v " o)
                  (display *scmxlate-version* o)
                  (display "," o) (newline o)
                  (display ";(c) Dorai Sitaram, " o) (newline o)
                  (display ";http://www.ccs.neu.edu/~dorai/" o)
                  (display "scmxlate/scmxlate.html" o)
                  (newline o) (newline o)
                  (translate-file-to-port file-to-be-ported o)
                  (newline o)
                  (if postamble-file
                      (copy-file-to-port postamble-file o))
                  (newline o)))
               ;compile?
               (if (eqv? *compile?* 'ask)
                   (set! *compile?*
                   (case *operating-system*
                     ((unix) (case *dialect*
                               ((mzscheme)
                                (display "Compile? [#t #f]")
                                (newline) (read))
                               (else #f)))
                     (else #f))))
              (if *compile?*
                  (case *operating-system*
                    ((unix) (case *dialect*
                              ((mzscheme) 'ok)
                              (else (set! *compile?* #f))))
                    (else (set! *compile?* #f))))
              (if *compile?*
                  (let ((target-file-so
                         (string-append target-file ".so")))
                    (display "Compiling...")
                    (newline)
                    (display "(If this fails, try without compile option.)")
                    (newline)
                    (case *dialect*
                      ((mzscheme)
                       (compile-file target-file target-file-so)
                       (delete-file target-file)
                       (call-with-output-file target-file
                         (lambda (o)
                           (if preamble-file
                               (call-with-input-file preamble-file
                                 (lambda (i)
                                   (let loop ()
                                   (if (memv (peek-char i) '(#\# #\"))
                                       (begin
                                        (display (read-a-line i) o)
                                        (newline o)
                                          (loop)))))))
                           (copy-file-to-port target-file-so o)))))))
              (if (and *shell-script?*
                       (eqv? *operating-system* 'unix))
                  (let ((chmod-cmd
                         (string-append "chmod +x " target-file)))
                    (case *dialect*
                      ((chez bigloo guile mzscheme petite plt 
			     scheme48 
			     scm stk stklos sxm umbscheme)
                       (system chmod-cmd))
                      ((gambit)
                       ((eval (call-with-input-string "##shell-command" 
                                                      read))
                        chmod-cmd))
                      ((mitscheme)
                       (run-shell-command chmod-cmd))
                      ((scsh)
                       (eval (let* ((i (make-string-input-port
                                        (string-append
                                         "(run ("
                                         chmod-cmd "))")))
                                    (e (read i)))
                               (close-input-port i)
                               e)
                             (interaction-environment)))
		      (else
		       (display "Do") (newline)
		       (display "  ") (display chmod-cmd) (newline)))))
              (cond (finish-file
                     ((if (eqv? *dialect* 'guile) primitive-load load)
                      finish-file))
                    (else
                     (display "Resulting file is `")
                     (display target-file)
                     (display "'.") (newline)
                     (display "You may want to rename it.") (newline)))
              )))
        *files-to-be-ported*)))

(if (eqv? *dialect* 'scsh) (force-output))

;exit Scheme if possible

(case *dialect*
  ((bigloo) (exit 0))
  ((chez gambit guile mzscheme petite plt pscheme
	 scsh stk stklos sxm) (exit))
  ((mitscheme) (%exit))
  ((scm) (quit))
  (else (display "You may exit Scheme now!")
        (newline)))
