(require-library "xml.ss" "xml")
(require-library "function.ss")

;TEST CASES are in comma-delimited-text-test.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; comma-delimited-text-parser : string -> (listof string)
; Accepts a-string to parse using commas to separate fields, 
; ignoring line feeds and leading or trailing white space, 
; replacing each carriage return or tab with a single space, .
; Returns the list of fields in a-string as a list of strings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLES:
; (comma-delimited-text-parser "Arial, Courier, Times New Roman") ->
;   ("Arial" "Courier" "Times New Roman")
; (comma-delimeted-text-parser "Arial,Courier,New Roman") -> 
;   ("Arial" "Courier" "New Roman")
; (comma-delimited-text-parser "") -> ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (comma-delimited-text-parser a-string)
  (separate (filter-list (string->list a-string)) empty empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; separate : (listof char) (listof string) (listof char)
; To separate a pre-filtered string along comma delimited fields
; into a list of substrings, each string of which represents one
; field of data.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (separate filteredlochars losubstrings lochars)
  (cond [(and (not (empty? lochars)) (empty? filteredlochars))
         (reverse (cons (list->string (reverse lochars)) losubstrings))]
        [(empty? filteredlochars) losubstrings]
        [(and (not (empty? lochars)) (char=? (first filteredlochars) #\,))
         (separate (rest filteredlochars)
                   (cons (list->string (reverse lochars)) losubstrings)
                   empty)]
        [(and (empty? lochars) (char=? (first filteredlochars) #\,))
         (separate (rest filteredlochars)
                   losubstrings
                   empty)]
        [else (separate (rest filteredlochars)
                        losubstrings
                        (cons (first filteredlochars) lochars))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; comma? : char -> bool
; Is a-char a comma?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (comma? a-char)
  (char=? a-char #\,))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-list : (listof char) -> (listof char)
; To filter leading and trailing whitespace and to collapse 
; unused whitespace between data and commas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter-list a-lochars)
  (reverse
   (filter-white-space-after-commas
    (reverse 
     (filter-white-space-after-commas
      (filter-trailing-white-space
       (filter-leading-white-space a-lochars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-leading-white-space : (listof char) -> (listof char)
; To filter leading whitespace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter-leading-white-space a-lochars)
  (cond [(empty? a-lochars) empty]
        [(char-whitespace? (first a-lochars))
         (filter-leading-white-space (rest a-lochars))]
        [else a-lochars]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-trailing-white-space : (listof char) -> (listof char)
; To filter trailing whitespace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter-trailing-white-space a-lochars)
  (reverse (filter-leading-white-space (reverse a-lochars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-white-space-after-commas : (listof char) -> (listof char)
; To  remove spaces after commas and before non-spaces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter-white-space-after-commas a-lochars)
  (cond [(empty? a-lochars) empty]
        [(not (comma? (first a-lochars))) 
         (cons (first a-lochars) (filter-white-space-after-commas (rest a-lochars)))]
        [else (cons (first a-lochars)
                    (filter-white-space-after-commas (filter-leading-white-space (rest a-lochars))))]))