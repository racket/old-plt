(let ([bw? (and (defined? 'mred:get-display-depth)
                (< (mred:get-display-depth) 8))])
  `((keyword (#x99 00 00) bold)
    (unbound-variable 
     ,@(if bw?
           `("black" slant)
           `("red")))
    (bound-variable 
     ,@(if bw?
           `("black" underline)
           `("navy")))
    (primitive "navy")

    ;; netscape and drscheme's notions of "green" are different
    (constant (51 135 39))

    (comment "teal")
    (base "brown")))
