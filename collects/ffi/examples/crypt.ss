#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

(require (lib "crypt.ss" "ffi"))

(define passwd #"foo")
(define salt #"xz")
(printf ">>> crypt(~s,~s) = ~s\n" passwd salt (crypt passwd salt))

;; md5-based version
(set! salt #"$1$somesalt$")
(printf ">>> crypt(~s,~s) = ~s\n" passwd salt (crypt passwd salt))

(newline)
(define foo #"foo bar")
(define key #"my key")
(printf ">>> ~s --encrypt--> ~s --decrypt--> ~s\n"
        foo (encrypt foo key) (decrypt (encrypt foo key) key))
