;;;
;;; <qp-unit.ss> ---- Quoted Printable Implementation
;;;
;;; Copyright (C) 2002 by PLT. 
;;; Copyright (C) 2001 by Francisco Solsona. 
;;;
;;; This file is part of mime-plt.

;;; mime-plt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; mime-plt is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with mime-plt; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

(module qp-unit mzscheme
  (require "qp-sig.ss"
           (lib "unitsig.ss")
           (lib "etc.ss"))

  (provide net:qp@)
  (define net:qp@
    (unit/sig net:qp^
      (import)
      
      ;; Exceptions:
      ;; String or input-port expected:
      (define-struct qp-error ())
      (define-struct (qp-wrong-input qp-error) ())
      (define-struct (qp-wrong-line-size qp-error) (size))
      
      ;; qp-encode : string -> string
      ;; returns the quoted printable representation of STR.
      (define qp-encode
        (lambda (str)
          (let ((out (open-output-string)))
            (qp-encode-stream (open-input-string str) out)
            (get-output-string out))))
      
      ;; qp-decode : string -> string
      ;; returns STR unqp.
      (define qp-decode
        (lambda (str)
          (let ((out (open-output-string)))
            (qp-decode-stream (open-input-string str) out)
            (get-output-string out))))
      
      (define qp-decode-stream
        (lambda (in out)
          (let ((iport (cond ((input-port? in) in)
                             ((string? in) (open-input-string in))
                             (else
                              (raise (make-qp-wrong-input))))))
            (let loop ((ln (read-line iport 'return-linefeed)))
              (cond ((eof-object? ln) (void)) ;; done reading
                    (else
                     (when (> (string-length ln) 76)
                       (warning "quoted-printable line is too long: ~a" (string-length ln)))
                     (quoted-printable-decode-line ln out)
                     (loop (read-line iport 'return-linefeed))))))))
      
      (define quoted-printable-decode-line
        (lambda (line out)
          (let ((in (open-input-string line)))
            (let loop ((ch (read-char in)))
              (if (eof-object? ch)
		  (newline out) ;; preserve linefeed
		  (case ch
		    ((#\=);; quoted-printable stuff
		     (let ((next (read-char in)))
		       (cond ((eof-object? next);; end of qp-line
			      null)
			     ((hex-digit? next)
			      (let ((next-next (read-char in)))
				(cond ((eof-object? next-next)
				       (warning "Illegal qp sequence: `=~a'" next)
				       (display "=" out)
				       (display next out))
				      ((hex-digit? next-next)
				       ;; qp-encoded
				       (display (hex-octet->char
						 (format "~a~a" next next-next))
						out))
				      (else
				       (warning "Illegal qp sequence: `=~a~a'" next next-next)
				       (display "=" out)
				       (display next out)
				       (display next-next out)))))
			     (else
			      ;; Warning: invalid
			      (warning "Illegal qp sequence: `=~a'" next)
			      (display "=" out)
			      (display next out)))
		       (unless (eof-object? next) ;; eol is effectively consumed by =
			 (loop (read-char in)))))
		    (else
		     (display ch out)
		     (loop (read-char in)))))))))
      
      (define warning
        (lambda (msg . args)
          (fprintf (current-error-port)
                   (apply format msg args))
          (newline (current-error-port))))
      
      (define hex-digit?
        (lambda (char)
          (regexp-match (regexp "[0-9abcdefABCDEF]")
                        (string char))))
      
      (define hex-octet->char
        (lambda (str)
          (integer->char (string->number str 16))))
      
      (define qp-blank?
        (lambda (char)
          (or (char=? char #\space)
              (char=? char #\tab))))
      
      (define qp-newline
        (lambda (port)
          (display #\return port)
          (display #\linefeed port)))
      
      (define qp-uppercase
        (lambda (hex-octet)
          (list->string (map char-upcase (string->list hex-octet)))))
      
      (define char->hex-octet
        (lambda (char)
          (let* ((ans (qp-uppercase
                       (number->string (char->integer char) 16)))
                 (padding? (< (string-length ans) 2)))
            (if padding?
                (format "=0~a" ans)
                (format "=~a" ans)))))
      
      (define display-qp-encoded
        (lambda (line out)
          (let* ((blanks (regexp "[ \t]+$"))
                 (pos (regexp-match-positions blanks line))
                 (col (caar pos))
                 (rest-of-line (substring line col (string-length line))))
            ;; Print everything up to the last non-blank char in line.
            (display (substring line 0 col) out)
            ;; hex-encode the following blanks
            (let loop ((str rest-of-line) (len (string-length rest-of-line)) (column (add1 col)))
              (cond  ((= column 76)
                      ;; Add CRLF to output
                      (qp-newline out)
                      ;; return the remainder blanks
                      str)
                     ;; Done, the whole line fitted on 76 chars.
                     ((zero? len) "")
                     ((<= column 73)
                      (display (char->hex-octet (string-ref str 0)) out)
                      (loop (substring str 1 len)
                            (sub1 len)
                            (+ column 3)))
                     (else
                      (display "=" out);; soft line break
                      (qp-newline out)
                      ;; return the remainder blanks
                      str))))))
      
      (define qp-encode-stream
        (lambda (in out)
          (let ((iport (cond ((input-port? in) in)
                             ((string? in) (open-input-string in))
                             (else
                              (raise (make-qp-wrong-input))))))
            (let loop ((c (read-char iport)) (line "") (column 0))
              (cond ((eof-object? c)
                     (if (qp-blank? (string-ref line (sub1 column)))
                         (let loop ((rem (display-qp-encoded line out)))
                           (unless (string=? rem "")
                             (loop (display-qp-encoded rem out))))
                         (display line out)))
                    ((= column 76);; Only 76 chars per line
                     (if (qp-blank? (string-ref line (sub1 column)))
                         ;; line ends in blank, we 8-bit encode blanks,
                         ;; print them out, and the remaining is pass to the
                         ;; following line.
                         (let ((rem (display-qp-encoded line out)))
                           (loop c rem (string-length rem)))
                         (begin
                           (display line out)
                           (qp-newline out)
                           (loop c "" 0))))
                    ((or (safe-char? c) (qp-blank? c))
                     (loop (read-char iport)
                           (string-append line (string c))
                           (add1 column)))
                    ;; OK octet is greater than 127.
                    ((<= column 72)
                     (loop (read-char iport)
                           (string-append line (char->hex-octet c))
                           (+ column 3)))
                    (else
                     (display line out);; is shorter that 76! (and greater that 72)
                     (display "=" out);; soft line break
                     (qp-newline out)
                     (loop (read-char iport) "" 0)))))))
      
      ;; safe-char := <any octet with decimal value of 33 through
      ;; 		     60 inclusive, and 62 through 126>
      ;; 		; Characters not listed as "mail-safe" in
      ;; 		; RFC 2049 are also not recommended.
      (define safe-char?
        (lambda (octet)
          (let ((dec (char->integer octet)))
            (or (and (<= 33 dec) (<= dec 60))
                (and (<= 62 dec) (<= dec 126)))))))))

;;; qp-unit.ss ends here
