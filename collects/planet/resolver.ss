#| resolver.ss -- PLaneT client

1. Introduction

The PLaneT system is a method for automatically sharing code packages, both as libraries and
as full applications, that gives every user of a PLaneT client the illusion of having a local copy
of every code package on the server, but is parsimonious in its transmission. It consists of a 
centralized server that holds all packages and individual clients that hold some portion of the 
archive locally. Maintenance of that archive should be transparent, and is the complete 
responsibility of the PLaneT client.

2. Client behavior

The PLaneT client receives user requests (i.e., the "(require (planet ...))" forms) and loads the
appropriate modules in response. In the course of handling these requests it may download new code
packages from the PLaneT server.

2.1 User interface

The structure of user PLaneT invocations is listed below.

PLANET-REQUEST ::= (planet FILE-NAME PKG-SPEC [PATH ...]?) 
FILE-NAME      ::= string
PKG-SPEC       ::= string | (FILE-PATH ... PKG-NAME) | (FILE-PATH ... PKG-NAME VER-SPEC)
VER-SPEC       ::= Nat | (Nat MINOR) 
MINOR          ::= Nat | (Nat Nat) | (= Nat) | (+ Nat) | (- Nat)
FILE-PATH      ::= string
PKG-NAME       ::= string
OWNER-NAME     ::= string
PATH           ::= string

All strings must be legal filename strings.

When encountered, a planet-request is interpreted as requiring the given file name from the given
logical package, specified by the package spec and the collection specification, if given. If no
VER-SPEC is provided, the most recent version is assumed. If no owner-name/path ... clause is
provided, the default package is assumed.

2. PLaneT protocol

PLaneT clients communicate request PLaneT servers over a TCP connection using a specialized 
protocol. The protocol is described below.

2.1 Overview

1. PLaneT client establishes TCP connection to PLaneT server.
2. Client transmits a version specifier.
3. Server either refuses that version and closes connection or accepts.
4. Client transmits a sequence of requests terminated by a special end-of-request marker.
    Simultaneously, server transmits responses to those requests.
5. Once the server has handled every request, it closes the connection.


I am concerned about the overhead of opening and closing TCP connections for a large 
program with many requires, so I want to allow many requests and responses over the same
connection. Unfortunately there's a wrinkle: the standard client, implemented the obvious
way, would be unable to send more than one request at a time because it gets invoked purely 
as a response to a require form and must load an appropriate file before it returns. This
means I can't batch up multiple requires, at least not with an obvious implementation.
  
A possible solution would be to implement an install program that walks over the syntax
tree of a program and gathers all requires, then communicates with the server and learns
what additional packages would be necessary due to those requires, and then downloads all
of them at once. We would have to implement both methods simultaneously, though, to allow
for REPL-based PLaneT use and dynamic-require (unless we want it to be a runtime exception
to use PLaneT from the REPL or via dynamic-require, something I'd rather not do), so I
want a protocol that will allow both forms of access easily. This protocol does that, and
doesn't require too much additional overhead in the case that the client only takes one
package at a time.

2.2 Communication Details

After a TCP connection is established, the client transmits a VERSION-SPECIFIER:

VERSION-SPECIFIER ::= "PLaneT/1.0\n"

The server responds with a VERSION-RESPONSE:

VERSION-RESPONSE ::=
  'ok "\n"
| ('invalid string) "\n" 

where the string in the invalid case is descriptive text intended for display to the user
that may indicate some specific message about the nature of the error.

If the server sends 'invalid, the server closes the connection. Otherwise, the client may 
send any number of requests, followed by an end-of-request marker:

REQUESTS ::= { REQUEST "\n"}* 'end "\n"
REQUEST  ::= (SEQ-NO 'get PKG-LANG PKG-NAME (Nat | #f) (Nat | #f) (Nat | #f) [OWNER-NAME PATH ...]?)
PKG-LANG ::= String
SEQ-NO   ::= Nat
 
The fields in a request are a uniquely identifying sequence number, the literal symbol 'get,
the name of the package to receive, the required major version and the lowest and highest
acceptable version (with #f meaning that there is no constraint for that field, and a #f in
major-version field implying that both other fields must also be #f), and the package path.

As the client is transmitting a REQUESTS sequence, the server begins responding to it with
RESPONSE structures, each with a sequence number indicating to which request it is 
responding (except in the case of input too garbled to extract a sequence number):

RESPONSE ::=
| ('error 'malformed-input string) "\n"
| (SEQ-NO 'error 'malformed-request string) "\n" 
| (SEQ-NO 'bad-language string) "\n"
| (SEQ-NO 'get 'ok Nat Nat Nat) "\n" BYTE-DATA
| (SEQ-NO 'get 'error ERROR-CODE string) "\n"

ERROR-CODE ::= 'not-found

If the server receives a malformed request, it may close connection after sending a
malformed-request response without processing any other requests. Otherwise it must
process all requests even in the event of an error. On a successful get, the three numbers
the server returns are the matched package's major version, the matched package's minor version,
and the number of bytes in the package.

3 Client Download Policies

Mzscheme invokes the PLaneT client once for each instance of a require-planet form in a program
being run (i.e., the transitive closure of the "requires" relation starting from some specified
root module; this closure is calculable statically). At each of these invocations, the client
examines its internal cache to see if an appropriate module exists that matches the specification
given by the user (for details see the next section). If one does, the client loads that module
and returns. If none does, it initiates a transaction with the server using the PLaneT protocol
described in the previous subsection and sends a single request consisting of the user's request. 
It installs the resulting .plt file and then loads the appropriate file.

The client keeps a cache of downloaded packages locally. It does so in the
$PLTCOLLECTS/planet/cache/ directory and subdirectories, in an intuitive manner: each item in
the package's path in the PLaneT require line correspond to a subdirectory in the cache 
directory, starting with the owner name. (They should be unpacked relative to some user-specific
rather than installation-specific place, possibly, but that's difficult to do so we won't do it
yet).

To check whether a package is installed when attempting to satisfy a requirement, the client 
checks its cache to see if an appropriate entry exists in its link-table for that require line.
If one exists, it uses the named package directly. If none exists, it checks to see if there is
an appropriate subdirectory.

||#
(module resolver mzscheme
  
  (require (lib "match.ss")
           (lib "file.ss")
           (lib "plt-single-installer.ss" "setup")
           "config.ss"
           "private/planet-shared.ss"
           "private/linkage.ss")

  (provide (rename resolver planet-module-name-resolver))

  (define install? (make-parameter #t)) ;; if #f, will not install packages and instead give an error
  
  ;; ensure these directories exist
  (make-directory* (PLANET-DIR))
  (make-directory* (CACHE-DIR))
  
  (define (resolver spec module-path stx)
    (establish-diamond-property-monitor)
    (cond
      [(or spec stx) (planet-resolve spec module-path stx)]
      [else module-path]))
  
  ; ==========================================================================================
  ; DIAMOND PROPERTY STUFF
  ; make sure a module isn't loaded twice with two different versions
  ; ==========================================================================================
  (define VER-CACHE-NAME #f)
  
  (define (establish-diamond-property-monitor)
    (unless VER-CACHE-NAME (set! VER-CACHE-NAME (gensym)))
    (unless (namespace-variable-value VER-CACHE-NAME #t (lambda () #f))
      (namespace-set-variable-value! VER-CACHE-NAME (make-hash-table 'equal))))
      
  (define (the-version-cache) (namespace-variable-value VER-CACHE-NAME))
  
  (define (pkg->diamond-key pkg)
    (cons (pkg-name pkg) (pkg-route pkg)))
  
  (define (add-pkg-to-diamond-registry! pkg)
    (let ((orig (hash-table-get (the-version-cache)
                                (pkg->diamond-key pkg)
                                (lambda () #f))))
      (cond
        [(not orig) (hash-table-put! (the-version-cache) (pkg->diamond-key pkg) pkg)]
        [(and (eq? (pkg-maj pkg) (pkg-maj orig))
              (eq? (pkg-min pkg) (pkg-min orig)))
         (void)]
        [else (raise (make-exn:module (format 
                                       "Package ~a loaded twice with multiple versions: 
attempted to load version ~a.~a while version ~a.~a was already loaded" 
                                       (pkg-name pkg) 
                                       (pkg-maj pkg)
                                       (pkg-min pkg)
                                       (pkg-maj orig)
                                       (pkg-min orig)) 
                                      (current-continuation-marks)))])))
    
  ; ==========================================================================================
  ; MAIN LOGIC
  ; Handles the overall functioning of the resolver
  ; ==========================================================================================
  
  ; planet-resolve : PLANET-REQUEST symbol syntax[PLANET-REQUEST] -> symbol
  ; resolves the given request. Returns a name corresponding to the module in the correct
  ; environment
  (define (planet-resolve spec module-path stx)
    (match-let* ([(file-name pkg-spec path ...) (cdr spec)]
                 [pspec (pkg-spec->full-pkg-spec pkg-spec stx)]
                 [pkg (or (get-linkage module-path pspec)
                          (add-linkage! module-path pspec
                                        (or
                                         (get-package-from-cache pspec)
                                         (get-package-from-server pspec)
                                         (raise (make-exn:module 
                                                 "Could not find matching package"
                                                 (current-continuation-marks))))))])
      (add-pkg-to-diamond-registry! pkg)
      (do-require file-name path module-path stx pkg)))
  
  ; pkg-spec->full-pkg-spec : PKG-SPEC syntax -> FULL-PKG-SPEC
  (define (pkg-spec->full-pkg-spec spec stx)
    (define (pkg name maj lo hi path) (make-pkg-spec name maj lo hi path stx))
    (match spec
      [(? string?)            (pkg spec #f #f #f '())]
      [((? string? path) ... (? number? maj)) (pkg (last path) maj 0 #f (drop-last path))]
      [((? string? path) ... (? number? maj) min-spec)
       (let ((pkg (lambda (min max) (pkg (last path) maj min max (drop-last path)))))
         (match min-spec
           [(? number? min)                 (pkg min #f)]
           [((? number? lo) (? number? hi)) (pkg lo  hi)]
           [('= (? number? min))            (pkg min min)]
           [('+ (? number? min))            (pkg min #f)]
           [('- (? number? min))            (pkg 0   min)]))]
      [_ (raise-syntax-error 'require (format "Invalid PLaneT package specifier: ~e" spec) stx)]))
  
  
    
  ; ==========================================================================================
  ; PHASE 2: CACHE SEARCH
  ; If there's no linkage, there might still be an appropriate cached module.
  ; ==========================================================================================

  ; get-package-from-cache : FULL-PKG-SPEC -> PKG | #f
  (define (get-package-from-cache pkg-spec) 
    (lookup-package pkg-spec (CACHE-DIR)))
  
  ; ==========================================================================================
  ; PHASE 3: SERVER RETRIEVAL
  ; Ask the PLaneT server for an appropriate package if we don't have one locally.
  ; ==========================================================================================
                 
  ; get-package-from-server : FULL-PKG-SPEC -> PKG | #f
  ; downloads and installs the given package from the PLaneT server and installs it in the cache,
  ; then returns a path to it
  (define (get-package-from-server pkg)
    (with-handlers
        ([exn:i/o? (lambda (e) (raise (make-exn:module 
                                       (format 
                                        "Error downloading module from PLaneT server: ~a"
                                        (exn-message e))
                                       (exn-continuation-marks e))))])
      (match (download-package pkg)
        [(#t str maj min) (install-pkg pkg str maj min)]
        [(#f str) #f])))
  
  (require (lib "date.ss"))
  (define (current-time) 
    (let ((date (seconds->date (current-seconds))))
      (parameterize ((date-display-format 'rfc2822))
        (format "~a ~a:~a:~a" 
                (date->string date)
                (date-hour date)
                (date-minute date)
                (date-second date)))))
  
  ; install-pkg : FULL-PKG-SPEC string Nat Nat -> PKG
  ; install the given pkg to the planet cache and return a path to it as a string
  (define (install-pkg pkg str maj min)
    (let ((the-dir 
           (apply 
            build-path 
            (CACHE-DIR) 
            (append (pkg-spec-path pkg) 
                    (list (pkg-spec-name pkg) (number->string maj) (number->string min))))))
      (if (directory-exists? the-dir)
          (raise (make-exn:module 
                  "Internal PLaneT error: trying to install already-installed package" 
                  (current-continuation-marks)))
          (begin
            (make-directory* the-dir)
            (let* ((null-out (make-custom-output-port 
                              #f 
                              (lambda (s start end buffer-ok?) (- end start))
                              void void))
                   (outport
                    (if (LOG-FILE)
                        (with-handlers ((exn:i/o? (lambda (e) null-out)))
                          (open-output-file (LOG-FILE) 'append))
                        null-out)))
              (parameterize ((current-output-port outport))
                (printf "\n============= Installing ~a on ~a =============\n" 
                        (pkg-spec-name pkg)
                        (current-time))
                (run-single-installer str (lambda () the-dir))))
            (make-pkg (pkg-spec-name pkg) (pkg-spec-path pkg) maj min the-dir)))))
         
  ; download-package : FULL-PKG-SPEC -> RESPONSE
  ; RESPONSE ::= (list #f string) | (list #t string Nat Nat)
  ; downloads the given package and returns (list bool string): if bool is #t,
  ; the string is the name of a file that contains the package. If bool is #f, the package
  ; didn't exist and the string is the server's informative message.
  ; raises an exception if some protocol failure occurs in the download process
  (define (download-package pkg)
  
    (define-values (ip op) (tcp-connect (PLANET-SERVER-NAME) (PLANET-SERVER-PORT)))
    (define (close-ports)
      (close-input-port ip)
      (close-output-port op))
    
    (define (request-pkg-list pkgs)
      (for-each/n (lambda (pkg seqno) 
                    (write-line (list* seqno 'get 
                                   (DEFAULT-PACKAGE-LANGUAGE)    
                                   (pkg-spec-name pkg) 
                                   (pkg-spec-maj pkg) 
                                   (pkg-spec-minor-lo pkg)
                                   (pkg-spec-minor-hi pkg)
                                   (pkg-spec-path pkg))
                             op))
                  pkgs)
      (write-line 'end op))
    
    (define (state:initialize)
      (fprintf op "PLaneT/1.0\n")
      (match (read ip)
        ['ok                        (state:send-pkg-request)]
        [('invalid (? string? msg)) (state:abort (string-append "protocol version error: " msg))]
        [bad-msg                    (state:abort (format "server protocol error (received invalid response): ~a" bad-msg))]))
         
    (define (state:send-pkg-request)
      (request-pkg-list (list pkg))
      (state:receive-package))
        
    (define (state:receive-package)
      (match (read ip)
        [(_ 'get 'ok (? nat? maj) (? nat? min) (? nat? bytes))
         (let ((filename (make-temporary-file "planettmp~a.plt")))
           (read-char ip) ; throw away newline that must be present
           (read-n-chars-to-file bytes ip filename)
           (list #t filename maj min))]
        [(_ 'error 'malformed-request (? string? msg)) (state:abort (format "Internal error (malformed request): ~a" msg))]
        [(_ 'get 'error 'not-found (? string? msg)) (state:failure (format "Server had no matching package: ~a" msg))]
        [(_ 'get 'error (? symbol? code) (? string? msg))
         (state:abort (format "Unknown error ~a receiving package: ~a" code msg))]
        [bad-response  (state:abort (format "Server returned malformed message: ~e" bad-response))]))
    
    (define (state:abort msg) (raise (make-exn:i/o:protocol msg (current-continuation-marks))))
    (define (state:failure msg) (list #f msg))
    
    (with-handlers ([void (lambda (e) (close-ports) (raise e))])
      (begin0
        (state:initialize)
        (close-ports))))
  
  ; ==========================================================================================
  ; MODULE MANAGEMENT
  ; Handles interaction with the module system
  ; ==========================================================================================
  
  ; do-require : string string syntax PKG -> symbol
  ; requires the given filename, which must be a module, in the given path.
  (define (do-require file path module-path stx pkg)
    (parameterize ((current-load-relative-directory (pkg-path pkg)))
      ((current-module-name-resolver) 
       `(file ,(apply build-path (pkg-path pkg) (append path (list file))))
       module-path
       stx)))


; ============================================================
; UTILITY
; A few small utility functions

(define (last l) (car (reverse l))))