#| planet-shared.ss -- shared client/server utility functions

Various common pieces of code that both the client and server need to access

==========================================================================================
THINGS TO DO FOR PLANET IN GENERAL
----------------------------------------

- the syntax may be too verbose
- there's no way to upload files
- maybe the archive should verify that the .plt file works
- test non-Scheme-only modules
- different versions of the same file being loaded at once are not detected
- downloading is suboptimal -- test to see if this is really a problem
- need a utility (tool for drscheme?) to bundle .plt files and upload them automatically
- need an automatic index of the files in the PLaneT server archives for the web
- Much more testing is needed
- give better error messages
- different modes, install vs no-install
- threaded server -- warning -- need i/o lock to write or we'll get garbage. Is it
  really worth threading at all? If not, why not just use a sequential protocol?
- utility to download directory tree skeleton?

- LANGUAGE VERSION! 299 vs 206 makes this an emminent threat

- To help out people behind firewalls we may want to consider HTTP as transport
    protocol, though this is skanky
--------------------------------------------------

- TODO:

- test suite
- a command-line tool to update the require->module list
- a GUI/drscheme tool to do the same thing
- a module exploration/debugging tool (is this the same as the current module browser?)
- a non-executing installation tool
- more efficient recursive-dependency handling. this is necessary if we want the previous
  item. I think a minor addition to the protocol would do the trick, though packaging
  becomes harder (you can't just use mzc --plt <file.plt> archive). We probably need to
  make improvements to the .plt system anyway. Something to talk to Robby and Matthew
  about.
- thread-safety, kill-safety.
==========================================================================================

|#

(module planet-shared mzscheme
  
  (require (lib "list.ss"))
  
  (provide (all-defined))
  
  
  (define-syntax (define-parameters stx)
    (syntax-case stx ()
      [(_ (name val) ...)
       (andmap identifier? (syntax-e #'(name ...)))
       #'(begin
           (provide name ...)
           (define name (make-parameter val)) ...)]))
  
  
  ; exn:i/o:protocol: exception indicating that a protocol error occured
  (define-struct (exn:i/o:protocol exn:i/o) ())
 
  
  ; ==========================================================================================
  ; CACHE LOGIC
  ; Handles checking the cache for an appropriate module
  ; ==========================================================================================
    
  ; lookup-package : FULL-PKG-SPEC string[dirname] -> PKG | #f
  ; returns the directory pointing to the appropriate package in the cache, or #f if the given package
  ; isn't in the cache
  (define (lookup-package pkg cache-dir)
    (let ((pkg-dir (build-path (apply build-path cache-dir (pkg-spec-path pkg)) (pkg-spec-name pkg))))
      (if (directory-exists? pkg-dir)
          (get-best-match pkg pkg-dir)
          #f)))
  
  ; get-best-match :FULL-PKG-SPEC (listof string[directory-name]) -> PKG | #f
  ; gets the best version in the given subdirectory in the specified low and high version range
  ; or #f if there is no appropriate version
  (define (get-best-match pkg-spec path)
    (let ((major-version (if (pkg-spec-maj pkg-spec)
                             (let ((specified-number (number->string (pkg-spec-maj pkg-spec))))
                               (if (directory-exists? (build-path path specified-number))
                                   specified-number
                                   #f))
                             (get-highest-numbered-subdir path #f #f))))
      (if major-version
          (let ((minor-version (get-highest-numbered-subdir 
                                (build-path path major-version)
                                (pkg-spec-minor-lo pkg-spec)
                                (pkg-spec-minor-hi pkg-spec))))
            (if minor-version
                (make-pkg
                 (pkg-spec-name pkg-spec)
                 (pkg-spec-path pkg-spec)
                 (string->number major-version) 
                 (string->number minor-version)
                 (build-path path major-version minor-version))
                #f))
          #f)))
  
  ; get-highest-numbered-subdir : string (Nat | #f) (Nat | #f) -> string[subdir] | #f
  ; given a path, returns the subdirectory of that path with the highest numeric name or #f if
  ; none exists. Does not return the full path.
  (define (get-highest-numbered-subdir path lo hi)
    (define (valid-dir? d) 
      (and 
       (directory-exists? (build-path path d)) 
       (let ((n (string->number d)))
         (and n
              (or (not lo) (>= n lo))
              (or (not hi) (<= n hi))))))
    
    (unless (directory-exists? path) 
      (raise (make-exn:module 
              "Internal PLaneT error: inconsistent cache, directory does not exist" 
              (current-continuation-marks))))
    (max-string (filter valid-dir? (directory-list path))))
  
  ; FULL-PKG-SPEC : (make-pkg-spec string (Nat | #f) (Nat | #f) (Nat | #f) (listof string) (syntax | #f))
  (define-struct pkg-spec (name maj minor-lo minor-hi path stx) (make-inspector))
  ; PKG : string Nat Nat path
  (define-struct pkg (name route maj min path))
  
  ; ==========================================================================================
  ; UTILITY
  ; Miscellaneous utility functions
  ; ==========================================================================================
  
  ; max-string : listof string[digits] -> string | #f
  ; this odd little guy takes a list of strings that represent a number and returns the string
  ; that represents the maximum number among them, or #f if there were no numbers at all 
  (define (max-string strs)
    (if (null? strs)
        #f
        (let loop ((biggest (car strs))
                   (big-n (string->number (car strs)))
                   (rest (cdr strs)))
          (cond
            [(null? rest) biggest]
            [else
             (let* ([candidate (car rest)]
                    [test-n    (string->number candidate)])
               (if (> test-n big-n)
                   (loop candidate test-n (cdr rest))
                   (loop biggest big-n (cdr rest))))]))))
  
  ; write-line : X output-port -> void
  ; writes the given value followed by a newline to the given port
  (define (write-line obj p)
    (write obj p)
    (newline p))
  
  ; for-each/n (X Nat -> Y) (listof X) -> void
  ; calls the input function on each element of the input list in order,
  ; also providing the element's zero-based index in the list
  (define (for-each/n f l)
    (let loop ((l l) (n 0))
      (cond
        [(null? l) (void)]
        [else
         (f (car l) n)
         (loop (cdr l) (add1 n))])))
      
  ; nat? : TST -> bool
  ; determines if the given scheme value is a natural number
  (define (nat? obj) (and (integer? obj) (>= obj 0)))
    
  ; read-n-chars-to-file : Nat input-port string[filename] -> void
  ; copies exactly n chars to the given file from the given port. Raises an exception
  ; if the given number of characters are not available.
  (define (read-n-chars-to-file n ip file)
    (let ((op (open-output-file file 'truncate)))
      (copy-n-chars n ip op)
      (close-output-port op)))
  
  ; copy-n-chars : Nat input-port output-port -> void
  ; copies exactly n characters from the input to the output. Raises an exception
  ; if this is not possible.
  (define (copy-n-chars n ip op)
    (let* ((bufsize (expt 2 16))
           (buf (make-string bufsize)))
      (let loop ((chars-to-read n))
        (cond
          [(= chars-to-read 0) (void)]
          [else
           (let ((chars-read (read-string-avail! buf ip 0 (min chars-to-read bufsize))))
             (cond
               [(eof-object? chars-read) (raise (make-exn:i/o:port:read "Not enough chars on input" (current-continuation-marks) ip))]
               [else 
                (write-string-avail buf op 0 chars-read)
                (loop (- chars-to-read chars-read))]))]))))
  
  ; repeat-forever : (-> void) -> [diverges]
  ; repeatedly invokes the given thunk forever
  (define (repeat-forever thunk) (let loop () (thunk) (loop)))
  
  
  ; build-hash-table : listof (list X Y) -> equal-hash-table[X -> Y]
  ; builds a new hash-table mapping all given X's to their appropriate Y values
  (define (build-hash-table asl)
    (let ((ht (make-hash-table 'equal)))
      (for-each (lambda (item) (hash-table-put! ht (car item) (cadr item))) asl)
      ht))
  
  (define (categorize f l)
    (let ((h (make-hash-table)))
      (begin
        (for-each
         (lambda (x) 
           (let ((key (f x)))
             (hash-table-put! h
                              key
                              (cons x (hash-table-get h key (lambda () null))))))
         l)
        (hash-table-map h list))))
  
  (define (drop-last l) (reverse (cdr (reverse l))))
  
  (define read-all
    (case-lambda
      [() (read-all (current-input-port))]
      [(ip)
       (let ((sexpr (read ip)))
         (cond
           [(eof-object? sexpr) '()]
           [else (cons sexpr (read-all ip))]))]))

  (define (wrap x) (begin (write x) (newline) x))
  
  
  ;; ============================================================
  ;; TREE STUFF
  ;; ============================================================
  
  ;; tree[X] ::= (make-branch X (listof tree[X])
  (define-struct branch (node children))
  
  ;; directory->tree : directory (string -> bool) -> tree[string]
  (define (directory->tree directory valid-dir?)
    (let-values ([(path name _) (split-path directory)])
      (let* ((files (directory-list directory))
             (files (map (lambda (d) (build-path directory d)) files))
             (files (filter (lambda (d) (and (directory-exists? d) (valid-dir? d))) files)))
        (make-branch name (map (lambda (d) (directory->tree d valid-dir?)) files)))))

  ;; filter-tree-by-pattern : tree[x] (listof (x -> y)) -> tree[y]
  ;; constraint: depth of the tree <= length of the list
  ;; converts the tree by applying to each depth the function at that position in the list
  (define (filter-tree-by-pattern tree pattern)
    (cond
      [(null? pattern) (error 'filter-tree-by-pattern "Tree too deep")]
      [else
       (make-branch ((car pattern) (branch-node tree))
                    (map 
                     (lambda (x) (filter-tree-by-pattern x (cdr pattern)))
                     (branch-children tree)))]))

  ;; sexp-tree[x] ::= (cons x (listof sexp-tree[x]))
  
  ;; tree->list : tree[x] -> sexp-tree[x]
  (define (tree->list tree)
    (cons (branch-node tree) (map tree->list (branch-children tree))))
  
  
  
  
  )