;; TeachPack : guess-gui.ss
;; Language: Advanced

(connect (lambda (e b)
           (printf "0th digit: ~s~n" (control 0))
           (view (control 0))))