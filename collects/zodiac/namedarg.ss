; $Id: namedarg.ss,v 1.9 1998/01/14 15:04:20 mflatt Exp $

(begin-construction-time (reference-library "match.ss"))
(begin-construction-time (reference-library "macro.ss"))

(begin-construction-time 
 (invoke-open-unit
  (reference-relative-library "namedargr.ss")))

(define-macro define-argument-list
  (lambda args
    `(begin-construction-time
       (-:define-argument-list ,@(map (lambda (x) `',x) args)))))

(define-macro lambda/nal
  (begin-construction-time lambda/nal))

(define-macro call/nal
  (begin-construction-time call/nal))

#|

(define-argument-list m3-elaborator
  expr foo (kwd attributes: attr) (kwd vocabulary: vocab)
  (opt (kwd parameterization: params) (current-parameterization))
  (opt other 3))

(define-argument-list other
  expr foo (kwd attributes: attr) (kwd vocabulary: vocab)
  (opt (kwd parameterization: params) (current-parameterization))
  (opt other 3))

(define f (lambda/nal m3-elaborator
	    (values expr foo attr vocab params other)))

(call/nal m3-elaborator f
  (parameterization: 'pahram)
  (vocabulary: 'vohcab )
  'exper
  'fuh
  'udder
  (attributes: 'atter))

|#
