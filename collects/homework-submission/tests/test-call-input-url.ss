;; call/input-url doesn't seem to be working perfectly.
(require (lib "etc.ss")
         (lib "url.ss" "net")
         (lib "xml.ss" "xml")
         (lib "send-assertions.ss" "web-server" "tools"))

(define *THE-SERVER* "http://syrma:8125")
(define *THE-URL*
  (string->url (string-append *THE-SERVER* "/servlets/submit.ss")))

(define (id-display x)
  (printf "~v~n" x) x)

(define (pre-process-page p)
  (xml->xexpr (read-xml/element p)))

(define (post-process-page p)
  (string->url (string-append *THE-SERVER* p)))

(printf "With explicit bindings~n")

(call/input-url
  *THE-URL*
  get-pure-port
  (compose id-display post-process-page
           id-display form->k-url
           id-display pre-process-page)
  '())

(printf "Without explicit bindings~n")

(call/input-url
  *THE-URL*
  get-pure-port
  (compose id-display post-process-page
           id-display form->k-url
           id-display pre-process-page))
