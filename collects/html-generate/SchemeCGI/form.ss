;;; -*- Scheme -*-
;;;
;;; FORM-LAMBDA
;;;
;;; Copyright (C) 1996 Emergent Technologies Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;;; MA 02139, USA.

;;; Revision History
;;;
;;; September 5, 1996 - Version 0.1
;;;  Created and released initial version.

(require-library "formpars.ss" "SchemeCGI")

;;; Some helper procedures
(define (xform-string-copy modifier)
  (lambda (string)
    (let ((new (string-copy string)))
      (modifier new)
      new)))

(define string-uppercase (xform-string-copy string-uppercase!))

(define string-lowercase (xform-string-copy string-lowercase!))

;;; The fundamental idea here is that a form is a closure with an
;;; extremely complicated printed representation, and that submitting
;;; a form is simply a remote procedure call.
;;;
;;; I'm not ready to supply the full implementation, but the basic
;;; syntax is this:
;;;
;;; FORM-LAMBDA (<binding> ...) <body>     Syntax
;;;
;;; This creates a closure much like you would expect.  
;;;
;;; The form-description is a hint to the HTTP client as to how to
;;; display the form.
;;;
;;; Each binding is a list of two objects, an identifier and a type
;;; specifier.  The type specifier is expected to evaluate to a
;;; procedure that will convert the string provided by the http client
;;; into the appropriate Scheme object, or to raise an exception.
;;;
;;; In theory, we should be able to return a FORM-LAMBDA to a client,
;;; but in practice, this is rather difficult to do because of the
;;; lexical environment.  We could build a way to deal with this using
;;; some persistant object scheme, but that is just too hard right
;;; now.  But it is the ``right thing''.
;;; 
;;; As a temporary measure, we supply INVOKE-FORM-LAMBDA.  A procedure
;;; of one argument that applies the form-lambda to the currently
;;; supplied form input.

(define-struct FORM-LAMBDA
  (binding-names
   binding-specs
   closure
   ))

(define-macro form-lambda
  (lambda (bindings . body)
    (let ((names (map car bindings))
	  (specs (map cadr bindings)))
    `(MAKE-FORM-LAMBDA 
      (QUOTE ,names)
      (LIST ,@specs)
      (LAMBDA (,@names) ,@body)))))

(define (form-fetch key form-input-alist)
  (let ((thing (assoc key form-input-alist)))
    (if thing 
	(cdr thing)
	(client-error "Form did not contain a ~a field." key))))

(define (invoke-form-lambda form-lambda)
  (let ((form-input-alist (parse-current-form-input)))
    (apply (form-lambda-closure form-lambda)
	   (map (lambda (name converter)
		  (converter name (form-fetch name form-input-alist)))
		(form-lambda-binding-names form-lambda)
		(form-lambda-binding-specs form-lambda)))))
    
;;; Form entry specifiers
;;;
;;; These procedures are expected to turn a printed representation
;;; into an object or to fail.

(define (form-entry-specifier type convert-and-check)
  (lambda (name string)
    (convert-and-check string 
      (lambda (value) value)
      (lambda (bogus-value)
	(client-error "The value of entry ~a, ~a, is not ~a."
		      name
		      bogus-value
		      type)))))

(define (form-convert-string conversion)
  (lambda ()
    (lambda (name value)
      (conversion value))))

(define form-string
  (form-convert-string (lambda (x) x)))	;no conversion

(define form-upcase-string 
  (form-convert-string string-uppercase))

(define form-lowercase-string
  (form-convert-string string-lowercase))

(define form-symbol
  (form-convert-string read-string))

(define (form-number name numeric-type)
  (form-entry-specifier name
   (lambda (string success failure)
     (let ((number (string->number string)))
       (cond ((numeric-type number) (success number))
	     ((eq? number   #f)     (failure string))
	     (else                  (failure number)))))))

(define form-int  (form-number "an integer" integer?))
(define form-real (form-number "a real"     real?))

;; Just like number, but optional % suffix is stripped.

(define form-percent
  (form-entry-specifier "a percentage"
    (lambda (string success failure)

      (define (do-it stripped)
	(let ((number (string->number stripped)))
	  (cond ((real? number) (success number))
		(number         (failure number))
		(else           (failure string)))))

     (if (char=? (string-right-ref string 0) #\%)
	 (do-it (substring string 0 (- (string-length string) 1)))
	 (do-it string)))))

(define (form-range base-type)
  (lambda (minimum maximum)
    (lambda (name string)
      (let ((number (base-type name string)))
	(if (or (<  number minimum)
		(>= number maximum))
	    (client-error "The value of entry ~a, ~a, does not lie in the range [~a,~a)." name number minimum maximum)
	    number)))))

;;; Accepts things that look like dollar amounts.
;;; Completely ad-hoc.
;;; Rule 1.  Leftmost char may be a $.
;;; Rule 2.  Rightmost char may be a .
;;; Rule 3.  Third from right char may be a . if other two chars are digits.
;;; Rule 4.  If there are any commas, they must be every 4 chars to
;;;          the left of the decimal, and the number may not start
;;;          with a comma.

(define http-us-dollars
  (form-entry-specifier
   "a dollar amount"
   (lambda (original-string success failure)

     ;; Strip optional leading $ sign.
     (define (strip-dollar-sign string)
       (if (char=? (string-ref string 0) #\$)
	   (substring string 1 (string-length string))
	   string))

     ;; Trailing decimal, or decimal and 2 digits allowed.
     (define (strip-cents string receiver)
       (cond ((char=? (string-right-ref string 0) #\.) ;ends in period
	      (if (= (string-length string) 1)
		  (failure original-string) ;single dot means nothing
		  (receiver (substring string 0 (- (string-length string) 1)) 0)))
	     ((< (string-length string) 3) (receiver string 0))	;short strings can't win
	     ((char=? (string-right-ref string 2) #\.) ;has 2 digits
	      (let ((dollars (substring string 0 (- (string-length string) 3)))
		    (cents   (string->number (substring string
							(- (string-length string) 2)
							(string-length string)))))
		(if (and cents (integer? cents))
		    (receiver dollars (/ cents 100))
		    (failure original-string))))
	     (else (receiver string 0))))

     (define (simple-amount dollar-part accumulate)
       (let ((dollars (string->number dollar-part)))
	 (if (and dollars (integer? dollars))
	     (success (+ dollars accumulate))
	     (failure original-string))))

     (define (deal-with-commas dollar-part multiplier accumulate)
       ;;(display " ") (display dollar-part)
       (let ((sl (string-length dollar-part)))
	 (cond ((= sl 0) (failure original-string))
	       ((< sl 4) (let ((x (string->number dollar-part)))
			   (if (and x (integer? x))
			       (success (+ (* x multiplier)
					   accumulate))
			       (failure original-string))))
	       ((= sl 4) (failure original-string))
	       ((not (char=? (string-right-ref dollar-part 3) #\,))
		(failure original-string))
	       (else (let ((x (string->number (substring dollar-part (- sl 3) sl))))
		       (if (and x (integer? x))
			   (deal-with-commas
			    (substring dollar-part 0 (- sl 4))
			    (* multiplier 1000)
			    (+ accumulate (* multiplier x)))
			   (failure original-string)))))))

     (strip-cents (strip-dollar-sign original-string)
       (lambda (dollar-part accumulate)
	 (let ((sl (string-length dollar-part)))
	   (cond ((= sl 0) (success accumulate)) ; only pennies
		 ((= sl 1) (simple-amount dollar-part accumulate))
		 ((not (member (string-ref dollar-part 0) 
			       '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
		  (failure original-string))
		 ((< sl 4) (simple-amount dollar-part accumulate))
		 ((not (char=? (string-right-ref dollar-part 3) #\,))
		  (simple-amount dollar-part accumulate))
		 (else (deal-with-commas dollar-part 1 accumulate)))))))))










