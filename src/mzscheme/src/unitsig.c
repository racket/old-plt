/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "schminc.h"

Scheme_Object *scheme_init_unitsig(void)
{
  Scheme_Env *env;
  Scheme_Object *unitsig_macros = NULL;

  env = scheme_get_env(scheme_config);

#define EVAL_ONE_STR(str) unitsig_macros = scheme_eval_string(str, env)
#define EVAL_ONE_SIZED_STR(str, len) unitsig_macros = scheme_eval_compiled_sized_string(str, len, env)
#define JUST_DEFINED(x) /**/

#if USE_COMPILED_MACROS
#include "cunitsig.inc"
#else
#include "unitsig.inc"
#endif

  return unitsig_macros;
}


(define verify-linkage-signature-match
  (let ([make-exn make-exn:unit]
	  [p-suffix (lambda (pos) (case pos [(1) 'st][(2) 'nd][(3) 'rd][else 'th]))])
    (lambda (who tags units esigs isigs)
      (for-each
       (lambda (u tag)
	  (unless (unit-with-signature? u)
	     (raise
	      (make-exn
	       (string->immutable-string
		(format
		 "~s: expression for \"~s\" is not a signed unit: ~e"
		 who tag u))
	       (current-continuation-marks)))))
       units tags)
      (for-each
       (lambda (u tag esig)
	 (verify-signature-match
	  who #f
	  (format "specified export signature for ~a" tag)
	  esig
	  (format "export signature for actual ~a sub-unit" tag)
	  (unit-with-signature-exports u)))
       units tags esigs)
      (for-each
       (lambda (u tag isig)
	 (let ([n (length (unit-with-signature-imports u))]
		 [c (length isig)])
	   (unless (= c n)
	      (raise
	       (make-exn
		(string->immutable-string
		 (format
		  "~s: ~a unit imports ~a units, but ~a units were provided"
		  who tag n c))
		(current-continuation-marks))))))
       units tags isigs)
      (for-each
       (lambda (u tag isig)
	 (let loop ([isig isig][expecteds (unit-with-signature-imports u)][pos 1])
	   (unless (null? isig)
	     (let ([expected (car expecteds)]
		     [provided (car isig)])
	       (verify-signature-match
		who #t
		(format "~a unit's ~s~s import (which is ~a)" tag
			  pos (p-suffix pos)
			  (car expected))
		(cdr expected)
		(format "~a's ~s~s linkage (which is ~a)"
			  tag
			  pos (p-suffix pos)
			  (car provided))
		(cdr provided))
	       (loop (cdr isig) (cdr expecteds) (add1 pos))))))
       units tags isigs))))


; Extra names:
(define-values (unit/sig? unit-with-signature->unit unit/sig->unit)
       (values unit-with-signature?
	       unit-with-signature-unit
	       unit-with-signature-unit))
