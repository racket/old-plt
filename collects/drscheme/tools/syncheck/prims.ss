(unit/sig
  drscheme:syncheck:prims^
  (import)
  (define beginning (make-hash-table))
  (define intermediate (make-hash-table))
  (define advanced (make-hash-table))
  (define (initialize-tables)
    (set! initialize-tables void)
    (hash-table-put!
      beginning
      'number?
      '("number?: (any -> boolean)"
        "         to determine whether some value is a number"))
    (hash-table-put!
      beginning
      '=
      '("=: (num num num ... -> boolean)"
        "   to compare two numbers for equality"))
    (hash-table-put!
      beginning
      '<
      '("<: (real real real ... -> boolean)"
        "   to compare two real numbers for less-than"))
    (hash-table-put!
      beginning
      '>
      '(">: (real real real ... -> boolean)"
        "   to compare two real numbers for greater-than"))
    (hash-table-put!
      beginning
      '<=
      '("<=: (real real real ... -> boolean)"
        "    to compare two real numbers for less-than or equality"))
    (hash-table-put!
      beginning
      '>=
      '(">=: (real real -> boolean)"
        "    to compare two real numbers for greater-than or equality"))
    (hash-table-put!
      beginning
      '+
      '("+: (num num num ... -> num)"
        "   to compute the sum of the input numbers"))
    (hash-table-put!
      beginning
      '-
      '("-: (num num ... -> num)"
        "   to compute the difference between the input numbers or to negate a number, if there is only one input"))
    (hash-table-put!
      beginning
      '*
      '("*: (num num num ... -> num)"
        "   to compute the product of all of the input numbers"))
    (hash-table-put!
      beginning
      '/
      '("/: (num num num ... -> num)"
        "   to compute the rational quotient of its input. 
              None but the first number can be zero."))
    (hash-table-put!
      beginning
      'max
      '("max: (num num ... -> num)" "     to determine the largest number"))
    (hash-table-put!
      beginning
      'min
      '("min: (num num ... -> num)" "     to determine the smallest number"))
    (hash-table-put!
      beginning
      'quotient
      '("quotient: (int int -> int)"
        "          to compute the quotient of two integers"))
    (hash-table-put!
      beginning
      'remainder
      '("remainder: (int int -> int)"
        "           to compute the remainder of two integers"))
    (hash-table-put!
      beginning
      'modulo
      '("modulo: (int int -> int)"
        "        to compute the modulo of two integers"))
    (hash-table-put!
      beginning
      'square
      '("square: (num -> num)" "        to compute the square of a number"))
    (hash-table-put!
      beginning
      'sqrt
      '("sqrt: (num -> num)" "      to compute the square root of a number"))
    (hash-table-put!
      beginning
      'expt
      '("expt: (num num -> num)"
        "      to compute the exponent of two numbers"))
    (hash-table-put!
      beginning
      'abs
      '("abs: (real -> real)"
        "     to compute the absolute value of a real number"))
    (hash-table-put!
      beginning
      'exp
      '("exp: (num -> num)" "     to compute e raised to a number"))
    (hash-table-put!
      beginning
      'log
      '("log: (num -> num)"
        "     to compute the base-e logarithm of a number"))
    (hash-table-put!
      beginning
      'sin
      '("sin: (num -> num)" "     to compute the sine of a number (radians)"))
    (hash-table-put!
      beginning
      'cos
      '("cos: (num -> num)"
        "     to compute the cosine of a number (radians)"))
    (hash-table-put!
      beginning
      'tan
      '("tan: (num -> num)"
        "     to compute the tangent of a number (radians)"))
    (hash-table-put!
      beginning
      'asin
      '("asin: (num -> num)"
        "      to compute the arcsine (inverse of sin) of a number"))
    (hash-table-put!
      beginning
      'acos
      '("acos: (num -> num)"
        "      to compute the arccosine (inverse of cos) of a number"))
    (hash-table-put!
      beginning
      'atan
      '("atan: (num -> num)"
        "      to compute the arctan (inverse of tan) of a number"))
    (hash-table-put!
      beginning
      'sinh
      '("sinh: (num -> num)"
        "      to compute the hyperbolic cosine of a number"))
    (hash-table-put!
      beginning
      'cosh
      '("cosh: (num -> num)"
        "      to compute the hyperbolic cosine of a number"))
    (hash-table-put!
      beginning
      'exact?
      '("exact?: (num -> bool)"
        "        to determine whether some number is exact"))
    (hash-table-put!
      beginning
      'integer?
      '("integer?: (any -> bool)"
        "          to determine whether some value is an integer (exact or inexact)"))
    (hash-table-put!
      beginning
      'zero?
      '("zero?: (number -> bool)"
        "       to determine if some value is zero or not"))
    (hash-table-put!
      beginning
      'odd?
      '("odd?: (integer -> bool)"
        "      to determine if some value is odd or not"))
    (hash-table-put!
      beginning
      'even?
      '("even?: (integer -> bool)"
        "       to determine if some value is even or not"))
    (hash-table-put!
      beginning
      'add1
      '("add1: (number -> number)"
        "      to compute a number one larger than a given number"))
    (hash-table-put!
      beginning
      'sub1
      '("sub1: (number -> number)"
        "      to compute a number one smaller than a given number"))
    (hash-table-put!
      beginning
      'lcm
      '("lcm: (int int ... -> int)"
        "     to compute the least common multiple of two integers"))
    (hash-table-put!
      beginning
      'gcd
      '("gcd: (int int ... -> int)"
        "     to compute the greatest common divisior"))
    (hash-table-put!
      beginning
      'rational?
      '("rational?: (any -> bool)"
        "           to determine whether some value is rational number"))
    (hash-table-put!
      beginning
      'numerator
      '("numerator: (rat -> int)"
        "           to compute the numerator of a rational"))
    (hash-table-put!
      beginning
      'denominator
      '("denominator: (rat -> int)"
        "             to compute the denominator of a rational"))
    (hash-table-put!
      beginning
      'inexact?
      '("inexact?: (num -> bool)"
        "          to determine whether some number is inexact"))
    (hash-table-put!
      beginning
      'real?
      '("real?: (any -> bool)"
        "       to determine whether some value is a real number"))
    (hash-table-put!
      beginning
      'floor
      '("floor: (real -> int)"
        "       to determine the closest integer below a real number"))
    (hash-table-put!
      beginning
      'ceiling
      '("ceiling: (real -> int)"
        "         to determine the closest integer above a real number"))
    (hash-table-put!
      beginning
      'round
      '("round: (real -> int)" "       to round a real number to an integer"))
    (hash-table-put!
      beginning
      'complex?
      '("complex?: (any -> bool)"
        "          to determine whether some value is complex"))
    (hash-table-put!
      beginning
      'make-polar
      '("make-polar: (real real -> num)"
        "            to create a complex from a magnitude and angle"))
    (hash-table-put!
      beginning
      'real-part
      '("real-part: (num -> real)"
        "           to extract the real part from a complex number"))
    (hash-table-put!
      beginning
      'imag-part
      '("imag-part: (num -> real)"
        "           to extract the imaginary part from a complex"))
    (hash-table-put!
      beginning
      'magnitude
      '("magnitude: (num -> real)"
        "           to determine the magnitude of a complex number"))
    (hash-table-put!
      beginning
      'angle
      '("angle: (num -> real)"
        "       to extract the angle from a complex number"))
    (hash-table-put!
      beginning
      'conjugate
      '("conjugate: (num -> num)"
        "           to compute the conjugate of a complex number"))
    (hash-table-put!
      beginning
      'exact->inexact
      '("exact->inexact: (num -> num)"
        "                to convert an exact number to an inexact one"))
    (hash-table-put!
      beginning
      'inexact->exact
      '("inexact->exact: (num -> num)"
        "                to approximate an inexact number by an exact one"))
    (hash-table-put!
      beginning
      'number->string
      '("number->string: (num -> string)"
        "                to convert a number to a string"))
    (hash-table-put!
      beginning
      'integer->char
      '("integer->char: (int -> char)"
        "               to lookup the character that corresponds to the<NL>given integer in the ASCII table (if any)"))
    (hash-table-put!
      beginning
      'random
      '("random: (int -> int)"
        "        to generate a random natural number <NL> less than some given integer"))
    (hash-table-put!
      beginning
      'current-seconds
      '("current-seconds: (-> int)"
        "                 to compute the current time in seconds elapsed <NL> (since a platform-specific starting date)"))
    (hash-table-put! beginning 'e '("e: real" "   Euler's number"))
    (hash-table-put!
      beginning
      'pi
      '("pi: real"
        "    the ratio of a circle's circumference to its diameter"))
    (hash-table-put!
      beginning
      'boolean?
      '("boolean?: (any -> boolean)"
        "          to determine whether some value is a boolean"))
    (hash-table-put!
      beginning
      'boolean=?
      '("boolean=?: (boolean boolean -> boolean)"
        "           to determine whether two booleans are equal"))
    (hash-table-put!
      beginning
      'not
      '("not: (boolean -> boolean)"
        "     to compute the negation of a boolean value"))
    (hash-table-put!
      beginning
      'symbol?
      '("symbol?: (any -> boolean)"
        "         to determine whether some value is a symbol"))
    (hash-table-put!
      beginning
      'symbol=?
      '("symbol=?: (symbol symbol -> boolean)"
        "          to determine whether two symbols are equal"))
    (hash-table-put!
      beginning
      'cons?
      '("cons?: (any -> boolean)"
        "       to determine whether some value is a constructed list"))
    (hash-table-put!
      beginning
      'pair?
      '("pair?: (any -> boolean)"
        "       to determine whether some value is a constructed list"))
    (hash-table-put!
      beginning
      'empty?
      '("empty?: (any -> boolean)"
        "        to determine whether some value is the empty list"))
    (hash-table-put!
      beginning
      'null?
      '("null?: (any -> boolean)"
        "       to determine whether some value is the empty list"))
    (hash-table-put!
      beginning
      'list?
      '("list?: (any -> boolean)"
        "       to determine whether some value is a list"))
    (hash-table-put!
      beginning
      'cons
      '("cons: (x (listof x) -> (listof x))" "      to construct a list"))
    (hash-table-put!
      beginning
      'first
      '("first: ((cons y (listof x)) -> y)"
        "       to select the first item of a non-empty list"))
    (hash-table-put!
      beginning
      'car
      '("car: ((cons y (listof x)) -> y)"
        "     to select the first item of a non-empty list"))
    (hash-table-put!
      beginning
      'rest
      '("rest: ((cons y (listof x)) -> (listof x))"
        "      to select the rest of a non-empty list"))
    (hash-table-put!
      beginning
      'cdr
      '("cdr: ((cons y (listof x)) -> (listof x))"
        "     to select the rest of a non-empty list"))
    (hash-table-put!
      beginning
      'second
      '("second: ((cons z (cons y (listof x))) -> y)"
        "        to select the second item of a non-empty list"))
    (hash-table-put!
      beginning
      'cadr
      '("cadr: ((cons z (cons y (listof x))) -> y)"
        "      to select the second item of a non-empty list"))
    (hash-table-put!
      beginning
      'third
      '("third: ((cons w (cons z (cons y (listof x)))) -> y)"
        "       to select the third item of a non-empty list"))
    (hash-table-put!
      beginning
      'caddr
      '("caddr: ((cons w (cons z (cons y (listof x)))) -> y)"
        "       to select the third item of a non-empty list"))
    (hash-table-put!
      beginning
      'fourth
      '("fourth: ((listof y) -> y)"
        "        to select the fourth item of a non-empty list"))
    (hash-table-put!
      beginning
      'cadddr
      '("cadddr: ((listof y) -> y)"
        "        to select the fourth item of a non-empty list"))
    (hash-table-put!
      beginning
      'fifth
      '("fifth: ((listof y) -> y)"
        "       to select the fifth item of a non-empty list"))
    (hash-table-put!
      beginning
      'sixth
      '("sixth: ((listof y) -> y)"
        "       to select the sixth item of a non-empty list"))
    (hash-table-put!
      beginning
      'seventh
      '("seventh: ((listof y) -> y)"
        "         to select the seventh item of a non-empty list"))
    (hash-table-put!
      beginning
      'eighth
      '("eighth: ((listof y) -> y)"
        "        to select the eighth item of a non-empty list"))
    (hash-table-put!
      beginning
      'list-ref
      '("list-ref: ((listof x) number -> x)"
        "          to extract the i-th item from a list"))
    (hash-table-put!
      beginning
      'list
      '("list: (any ... -> (listof any))"
        "      to construct a list of its arguments"))
    (hash-table-put!
      beginning
      'append
      '("append: ((listof any) ... -> (listof any))"
        "        to create a single list from several, by juxtaposition of the items"))
    (hash-table-put!
      beginning
      'length
      '("length: (list -> number)"
        "        to compute the number of items on a list"))
    (hash-table-put!
      beginning
      'memq
      '("memq: (any list -> (union false list))"
        "      to determine whether some value is on some list <NL> (comparing values with eq?)"))
    (hash-table-put!
      beginning
      'memv
      '("memv: (any list -> (union false list))"
        "      to determine whether some value is on the list <NL> (comparing values with eqv?)"))
    (hash-table-put!
      beginning
      'member
      '("member: (any list -> (union false list))"
        "        to determine whether some value is on the list <NL> (comparing values with equal?)"))
    (hash-table-put!
      beginning
      'reverse
      '("reverse: (list -> list)"
        "         to create a reversed version of a list"))
    (hash-table-put!
      beginning
      'assq
      '("assq: (x (listof (cons x y)) -> (union false (cons x y)))"
        "      to determine whether some item is the first item of a pair <NL> in a list of pairs"))
    (hash-table-put!
      beginning
      'equal?
      '("equal?: (list list -> boolean)"
        "        to determine whether two lists are equal"))
    (hash-table-put! beginning 'char? '("char?: (any -> boolean)" "        "))
    (hash-table-put!
      beginning
      'char=?
      '("char=?: (char char ... -> boolean)"
        "        to determine whether two characters are equal"))
    (hash-table-put!
      beginning
      'char<?
      '("char<?: (char char ... -> boolean)"
        "        to determine whether a character precedes another"))
    (hash-table-put!
      beginning
      'char>?
      '("char>?: (char char ... -> boolean)"
        "        to determine whether a character succeeds another"))
    (hash-table-put!
      beginning
      'char<=?
      '("char<=?: (char char ... -> boolean)"
        "         to determine whether a character precedes another <NL>(or is equal to it)"))
    (hash-table-put!
      beginning
      'char>=?
      '("char>=?: (char char ... -> boolean)"
        "         to determine whether a character succeeds another <NL>(or is equal to it)"))
    (hash-table-put!
      beginning
      'char-ci=?
      '("char-ci=?: (char char ... -> boolean)"
        "           to determine whether two characters are equal <NL>in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'char-ci<?
      '("char-ci<?: (char char ... -> boolean)"
        "           to determine whether a character precedes another <NL>in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'char-ci>?
      '("char-ci>?: (char char ... -> boolean)"
        "           to determine whether a character succeeds another <NL>in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'char-ci<=?
      '("char-ci<=?: (char char ... -> boolean)"
        "            to determine whether a character precedes another <NL>(or is equal to it) in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'char-ci>=?
      '("char-ci>=?: (char char ... -> boolean)"
        "            to determine whether a character succeeds another <NL>(or is equal to it) in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'char-numeric?
      '("char-numeric?: (char -> boolean)"
        "               to determine whether a character represents a digit"))
    (hash-table-put!
      beginning
      'char-alphabetic?
      '("char-alphabetic?: (char -> boolean)"
        "                  to determine whether a character represents <NL> an alphabetic character"))
    (hash-table-put!
      beginning
      'char-whitespace?
      '("char-whitespace?: (char -> boolean)"
        "                  to determine whether a character represents space"))
    (hash-table-put!
      beginning
      'char-upper-case?
      '("char-upper-case?: (char -> boolean)"
        "                  to determine whether a character is an <NL>upper-case character"))
    (hash-table-put!
      beginning
      'char-lower-case?
      '("char-lower-case?: (char -> boolean)"
        "                  to determine whether a character is a <NL>lower-case character"))
    (hash-table-put!
      beginning
      'char-upcase
      '("char-upcase: (char -> char)"
        "             to determine the equivalent upper-case character"))
    (hash-table-put!
      beginning
      'char-downcase
      '("char-downcase: (char -> char)"
        "               to determine the equivalent lower-case character"))
    (hash-table-put!
      beginning
      'char->integer
      '("char->integer: (char -> integer)"
        "               to lookup the number that corresponds to the<NL>given character in the ASCII table (if any)"))
    (hash-table-put!
      beginning
      'string?
      '("string?: (any -> boolean)"
        "         to determine whether a value is a string"))
    (hash-table-put!
      beginning
      'string-length
      '("string-length: (string -> nat)"
        "               to determine the length of a string"))
    (hash-table-put!
      beginning
      'make-string
      '("make-string: (nat char -> string)"
        "             to produce a string of given length<NL>from a single given character"))
    (hash-table-put!
      beginning
      'string-ref
      '("string-ref: (string nat -> char)"
        "            to extract the i-the character from a string"))
    (hash-table-put!
      beginning
      'substring
      '("substring: (string nat nat -> string)"
        "           to extract the substring starting at a 0-based index,<NL> consisting of a given number of characters"))
    (hash-table-put!
      beginning
      'string-copy
      '("string-copy: (string -> string)" "             to copy a string"))
    (hash-table-put!
      beginning
      'string-append
      '("string-append: (string ... -> string)"
        "               to juxtapose the characters of several strings"))
    (hash-table-put!
      beginning
      'string=?
      '("string=?: (string string ... -> boolean)"
        "          to compare two strings character-wise"))
    (hash-table-put!
      beginning
      'string<?
      '("string<?: (string string ... -> boolean)"
        "          to determine whether one string alphabetically<NL>precedes another"))
    (hash-table-put!
      beginning
      'string>?
      '("string>?: (string string ... -> boolean)"
        "          to determine whether one string alphabetically<NL>succeeds another"))
    (hash-table-put!
      beginning
      'string<=?
      '("string<=?: (string string ... -> boolean)"
        "           to determine whether one string alphabetically<NL>precedes another (or is equal to it)"))
    (hash-table-put!
      beginning
      'string>=?
      '("string>=?: (string string ... -> boolean)"
        "           to determine whether one string alphabetically<NL>succeeds another (or is equal to it)"))
    (hash-table-put!
      beginning
      'string-ci=?
      '("string-ci=?: (string string ... -> boolean)"
        "             to compare two strings character-wise <NL>in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'string-ci<?
      '("string-ci<?: (string string ... -> boolean)"
        "             to determine whether one string alphabetically<NL>precedes another in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'string-ci>?
      '("string-ci>?: (string string ... -> boolean)"
        "             to determine whether one string alphabetically<NL>succeeds another in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'string-ci<=?
      '("string-ci<=?: (string string ... -> boolean)"
        "              to determine whether one string alphabetically<NL>precedes another (or is equal to it)<NL>in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'string-ci>=?
      '("string-ci>=?: (string string ... -> boolean)"
        "              to determine whether one string alphabetically<NL>succeeds another (or is equal to it)<NL>in a case-insensitive manner"))
    (hash-table-put!
      beginning
      'string->number
      '("string->number: (string -> (union number false))"
        "                to convert a string into a number,<NL>produce false if impossible"))
    (hash-table-put!
      beginning
      'string->list
      '("string->list: (string -> (listof char))"
        "              to convert a string into a list of characters"))
    (hash-table-put!
      beginning
      'list->string
      '("list->string: ((listof char) -> string)"
        "              to convert a s list of characters into a string"))
    (hash-table-put!
      beginning
      'format
      '("format: (string any ... -> string)"
        "        to format a string, possibly embedding values"))
    (hash-table-put!
      beginning
      'make-posn
      '("make-posn: (number number -> posn)" "           to construct a posn"))
    (hash-table-put!
      beginning
      'posn?
      '("posn?: (anything -> boolean)"
        "       to determine if its input is a posn"))
    (hash-table-put!
      beginning
      'posn-x
      '("posn-x: (posn -> number)"
        "        to extract the x component of a posn"))
    (hash-table-put!
      beginning
      'posn-y
      '("posn-y: (posn -> number)"
        "        to extract the y component of a posn"))
    (hash-table-put!
      beginning
      'error
      '("error: (symbol string -> void)" "       to signal an error"))
    (hash-table-put!
      beginning
      'struct?
      '("struct?: (any -> boolean)"
        "         to determine whether some value is a structure"))
    (hash-table-put!
      beginning
      'equal?
      '("equal?: (any any -> boolean)"
        "        to compare two values, component by component"))
    (hash-table-put!
      beginning
      'eq?
      '("eq?: (any any -> boolean)"
        "     to compare two values based on when they were created"))
    (hash-table-put!
      beginning
      'exit
      '("exit: (-> void)" "      to exit the running program"))
    (hash-table-put!
      beginning
      'eval
      '("eval: (any -> any)" "      to evaluate an sexpression"))
    (hash-table-put!
      beginning
      'procedure?
      '("procedure?: (any -> boolean)" "            to recognize a procedure"))
    (hash-table-put!
      beginning
      'time-apply
      '("time-apply: ((any ... -> any) any ... -> any)"
        "            applies the first argument to the rest of the arguments and time the function call"))
    (hash-table-put!
      beginning
      'apply
      '("apply: ((any ... -> any) any ... -> any)"
        "       to apply the first argument to the rest of the arguments"))
    (hash-table-put!
      beginning
      'values
      '("values: (any ... -> (values any ...))"
        "        to return multiple values"))
    (hash-table-put!
      beginning
      'void
      '("void: (-> void)" "      to construct the void value"))
    (hash-table-put!
      beginning
      'void?
      '("void?: (any -> boolean)" "       to recognize the void value"))
    (hash-table-put!
      beginning
      'raise
      '("raise: (any -> void)" "       to raise an exception"))
    (hash-table-put!
      beginning
      'make-exn:else
      '("make-exn:else: (string continuation-mark-set -> exn)"
        "               to construct an else exception"))
    (hash-table-put!
      beginning
      'current-continuation-marks
      '("current-continuation-marks: (-> continuation-mark-set)"
        "                            to return the current set of continuation marks"))
    (hash-table-put!
      beginning
      'require-library/proc
      '("require-library/proc: (string string -> void)"
        "                      to load a library"))
    (hash-table-put!
      intermediate
      'number?
      '("number?: (any -> boolean)"
        "         to determine whether some value is a number"))
    (hash-table-put!
      intermediate
      '=
      '("=: (num num num ... -> boolean)"
        "   to compare two numbers for equality"))
    (hash-table-put!
      intermediate
      '<
      '("<: (real real real ... -> boolean)"
        "   to compare two real numbers for less-than"))
    (hash-table-put!
      intermediate
      '>
      '(">: (real real real ... -> boolean)"
        "   to compare two real numbers for greater-than"))
    (hash-table-put!
      intermediate
      '<=
      '("<=: (real real real ... -> boolean)"
        "    to compare two real numbers for less-than or equality"))
    (hash-table-put!
      intermediate
      '>=
      '(">=: (real real -> boolean)"
        "    to compare two real numbers for greater-than or equality"))
    (hash-table-put!
      intermediate
      '+
      '("+: (num num num ... -> num)"
        "   to compute the sum of the input numbers"))
    (hash-table-put!
      intermediate
      '-
      '("-: (num num ... -> num)"
        "   to compute the difference between the input numbers or to negate a number, if there is only one input"))
    (hash-table-put!
      intermediate
      '*
      '("*: (num num num ... -> num)"
        "   to compute the product of all of the input numbers"))
    (hash-table-put!
      intermediate
      '/
      '("/: (num num num ... -> num)"
        "   to compute the rational quotient of its input. 
              None but the first number can be zero."))
    (hash-table-put!
      intermediate
      'max
      '("max: (num num ... -> num)" "     to determine the largest number"))
    (hash-table-put!
      intermediate
      'min
      '("min: (num num ... -> num)" "     to determine the smallest number"))
    (hash-table-put!
      intermediate
      'quotient
      '("quotient: (int int -> int)"
        "          to compute the quotient of two integers"))
    (hash-table-put!
      intermediate
      'remainder
      '("remainder: (int int -> int)"
        "           to compute the remainder of two integers"))
    (hash-table-put!
      intermediate
      'modulo
      '("modulo: (int int -> int)"
        "        to compute the modulo of two integers"))
    (hash-table-put!
      intermediate
      'square
      '("square: (num -> num)" "        to compute the square of a number"))
    (hash-table-put!
      intermediate
      'sqrt
      '("sqrt: (num -> num)" "      to compute the square root of a number"))
    (hash-table-put!
      intermediate
      'expt
      '("expt: (num num -> num)"
        "      to compute the exponent of two numbers"))
    (hash-table-put!
      intermediate
      'abs
      '("abs: (real -> real)"
        "     to compute the absolute value of a real number"))
    (hash-table-put!
      intermediate
      'exp
      '("exp: (num -> num)" "     to compute e raised to a number"))
    (hash-table-put!
      intermediate
      'log
      '("log: (num -> num)"
        "     to compute the base-e logarithm of a number"))
    (hash-table-put!
      intermediate
      'sin
      '("sin: (num -> num)" "     to compute the sine of a number (radians)"))
    (hash-table-put!
      intermediate
      'cos
      '("cos: (num -> num)"
        "     to compute the cosine of a number (radians)"))
    (hash-table-put!
      intermediate
      'tan
      '("tan: (num -> num)"
        "     to compute the tangent of a number (radians)"))
    (hash-table-put!
      intermediate
      'asin
      '("asin: (num -> num)"
        "      to compute the arcsine (inverse of sin) of a number"))
    (hash-table-put!
      intermediate
      'acos
      '("acos: (num -> num)"
        "      to compute the arccosine (inverse of cos) of a number"))
    (hash-table-put!
      intermediate
      'atan
      '("atan: (num -> num)"
        "      to compute the arctan (inverse of tan) of a number"))
    (hash-table-put!
      intermediate
      'sinh
      '("sinh: (num -> num)"
        "      to compute the hyperbolic cosine of a number"))
    (hash-table-put!
      intermediate
      'cosh
      '("cosh: (num -> num)"
        "      to compute the hyperbolic cosine of a number"))
    (hash-table-put!
      intermediate
      'exact?
      '("exact?: (num -> bool)"
        "        to determine whether some number is exact"))
    (hash-table-put!
      intermediate
      'integer?
      '("integer?: (any -> bool)"
        "          to determine whether some value is an integer (exact or inexact)"))
    (hash-table-put!
      intermediate
      'zero?
      '("zero?: (number -> bool)"
        "       to determine if some value is zero or not"))
    (hash-table-put!
      intermediate
      'odd?
      '("odd?: (integer -> bool)"
        "      to determine if some value is odd or not"))
    (hash-table-put!
      intermediate
      'even?
      '("even?: (integer -> bool)"
        "       to determine if some value is even or not"))
    (hash-table-put!
      intermediate
      'add1
      '("add1: (number -> number)"
        "      to compute a number one larger than a given number"))
    (hash-table-put!
      intermediate
      'sub1
      '("sub1: (number -> number)"
        "      to compute a number one smaller than a given number"))
    (hash-table-put!
      intermediate
      'lcm
      '("lcm: (int int ... -> int)"
        "     to compute the least common multiple of two integers"))
    (hash-table-put!
      intermediate
      'gcd
      '("gcd: (int int ... -> int)"
        "     to compute the greatest common divisior"))
    (hash-table-put!
      intermediate
      'rational?
      '("rational?: (any -> bool)"
        "           to determine whether some value is rational number"))
    (hash-table-put!
      intermediate
      'numerator
      '("numerator: (rat -> int)"
        "           to compute the numerator of a rational"))
    (hash-table-put!
      intermediate
      'denominator
      '("denominator: (rat -> int)"
        "             to compute the denominator of a rational"))
    (hash-table-put!
      intermediate
      'inexact?
      '("inexact?: (num -> bool)"
        "          to determine whether some number is inexact"))
    (hash-table-put!
      intermediate
      'real?
      '("real?: (any -> bool)"
        "       to determine whether some value is a real number"))
    (hash-table-put!
      intermediate
      'floor
      '("floor: (real -> int)"
        "       to determine the closest integer below a real number"))
    (hash-table-put!
      intermediate
      'ceiling
      '("ceiling: (real -> int)"
        "         to determine the closest integer above a real number"))
    (hash-table-put!
      intermediate
      'round
      '("round: (real -> int)" "       to round a real number to an integer"))
    (hash-table-put!
      intermediate
      'complex?
      '("complex?: (any -> bool)"
        "          to determine whether some value is complex"))
    (hash-table-put!
      intermediate
      'make-polar
      '("make-polar: (real real -> num)"
        "            to create a complex from a magnitude and angle"))
    (hash-table-put!
      intermediate
      'real-part
      '("real-part: (num -> real)"
        "           to extract the real part from a complex number"))
    (hash-table-put!
      intermediate
      'imag-part
      '("imag-part: (num -> real)"
        "           to extract the imaginary part from a complex"))
    (hash-table-put!
      intermediate
      'magnitude
      '("magnitude: (num -> real)"
        "           to determine the magnitude of a complex number"))
    (hash-table-put!
      intermediate
      'angle
      '("angle: (num -> real)"
        "       to extract the angle from a complex number"))
    (hash-table-put!
      intermediate
      'conjugate
      '("conjugate: (num -> num)"
        "           to compute the conjugate of a complex number"))
    (hash-table-put!
      intermediate
      'exact->inexact
      '("exact->inexact: (num -> num)"
        "                to convert an exact number to an inexact one"))
    (hash-table-put!
      intermediate
      'inexact->exact
      '("inexact->exact: (num -> num)"
        "                to approximate an inexact number by an exact one"))
    (hash-table-put!
      intermediate
      'number->string
      '("number->string: (num -> string)"
        "                to convert a number to a string"))
    (hash-table-put!
      intermediate
      'integer->char
      '("integer->char: (int -> char)"
        "               to lookup the character that corresponds to the<NL>given integer in the ASCII table (if any)"))
    (hash-table-put!
      intermediate
      'random
      '("random: (int -> int)"
        "        to generate a random natural number <NL> less than some given integer"))
    (hash-table-put!
      intermediate
      'current-seconds
      '("current-seconds: (-> int)"
        "                 to compute the current time in seconds elapsed <NL> (since a platform-specific starting date)"))
    (hash-table-put! intermediate 'e '("e: real" "   Euler's number"))
    (hash-table-put!
      intermediate
      'pi
      '("pi: real"
        "    the ratio of a circle's circumference to its diameter"))
    (hash-table-put!
      intermediate
      'boolean?
      '("boolean?: (any -> boolean)"
        "          to determine whether some value is a boolean"))
    (hash-table-put!
      intermediate
      'boolean=?
      '("boolean=?: (boolean boolean -> boolean)"
        "           to determine whether two booleans are equal"))
    (hash-table-put!
      intermediate
      'not
      '("not: (boolean -> boolean)"
        "     to compute the negation of a boolean value"))
    (hash-table-put!
      intermediate
      'symbol?
      '("symbol?: (any -> boolean)"
        "         to determine whether some value is a symbol"))
    (hash-table-put!
      intermediate
      'symbol=?
      '("symbol=?: (symbol symbol -> boolean)"
        "          to determine whether two symbols are equal"))
    (hash-table-put!
      intermediate
      'cons?
      '("cons?: (any -> boolean)"
        "       to determine whether some value is a constructed list"))
    (hash-table-put!
      intermediate
      'pair?
      '("pair?: (any -> boolean)"
        "       to determine whether some value is a constructed list"))
    (hash-table-put!
      intermediate
      'empty?
      '("empty?: (any -> boolean)"
        "        to determine whether some value is the empty list"))
    (hash-table-put!
      intermediate
      'null?
      '("null?: (any -> boolean)"
        "       to determine whether some value is the empty list"))
    (hash-table-put!
      intermediate
      'list?
      '("list?: (any -> boolean)"
        "       to determine whether some value is a list"))
    (hash-table-put!
      intermediate
      'cons
      '("cons: (x (listof x) -> (listof x))" "      to construct a list"))
    (hash-table-put!
      intermediate
      'first
      '("first: ((cons y (listof x)) -> y)"
        "       to select the first item of a non-empty list"))
    (hash-table-put!
      intermediate
      'car
      '("car: ((cons y (listof x)) -> y)"
        "     to select the first item of a non-empty list"))
    (hash-table-put!
      intermediate
      'rest
      '("rest: ((cons y (listof x)) -> (listof x))"
        "      to select the rest of a non-empty list"))
    (hash-table-put!
      intermediate
      'cdr
      '("cdr: ((cons y (listof x)) -> (listof x))"
        "     to select the rest of a non-empty list"))
    (hash-table-put!
      intermediate
      'second
      '("second: ((cons z (cons y (listof x))) -> y)"
        "        to select the second item of a non-empty list"))
    (hash-table-put!
      intermediate
      'cadr
      '("cadr: ((cons z (cons y (listof x))) -> y)"
        "      to select the second item of a non-empty list"))
    (hash-table-put!
      intermediate
      'third
      '("third: ((cons w (cons z (cons y (listof x)))) -> y)"
        "       to select the third item of a non-empty list"))
    (hash-table-put!
      intermediate
      'caddr
      '("caddr: ((cons w (cons z (cons y (listof x)))) -> y)"
        "       to select the third item of a non-empty list"))
    (hash-table-put!
      intermediate
      'fourth
      '("fourth: ((listof y) -> y)"
        "        to select the fourth item of a non-empty list"))
    (hash-table-put!
      intermediate
      'cadddr
      '("cadddr: ((listof y) -> y)"
        "        to select the fourth item of a non-empty list"))
    (hash-table-put!
      intermediate
      'fifth
      '("fifth: ((listof y) -> y)"
        "       to select the fifth item of a non-empty list"))
    (hash-table-put!
      intermediate
      'sixth
      '("sixth: ((listof y) -> y)"
        "       to select the sixth item of a non-empty list"))
    (hash-table-put!
      intermediate
      'seventh
      '("seventh: ((listof y) -> y)"
        "         to select the seventh item of a non-empty list"))
    (hash-table-put!
      intermediate
      'eighth
      '("eighth: ((listof y) -> y)"
        "        to select the eighth item of a non-empty list"))
    (hash-table-put!
      intermediate
      'list-ref
      '("list-ref: ((listof x) number -> x)"
        "          to extract the i-th item from a list"))
    (hash-table-put!
      intermediate
      'list
      '("list: (any ... -> (listof any))"
        "      to construct a list of its arguments"))
    (hash-table-put!
      intermediate
      'append
      '("append: ((listof any) ... -> (listof any))"
        "        to create a single list from several, by juxtaposition of the items"))
    (hash-table-put!
      intermediate
      'length
      '("length: (list -> number)"
        "        to compute the number of items on a list"))
    (hash-table-put!
      intermediate
      'memq
      '("memq: (any list -> (union false list))"
        "      to determine whether some value is on some list <NL> (comparing values with eq?)"))
    (hash-table-put!
      intermediate
      'memv
      '("memv: (any list -> (union false list))"
        "      to determine whether some value is on the list <NL> (comparing values with eqv?)"))
    (hash-table-put!
      intermediate
      'member
      '("member: (any list -> (union false list))"
        "        to determine whether some value is on the list <NL> (comparing values with equal?)"))
    (hash-table-put!
      intermediate
      'reverse
      '("reverse: (list -> list)"
        "         to create a reversed version of a list"))
    (hash-table-put!
      intermediate
      'assq
      '("assq: (x (listof (cons x y)) -> (union false (cons x y)))"
        "      to determine whether some item is the first item of a pair <NL> in a list of pairs"))
    (hash-table-put!
      intermediate
      'equal?
      '("equal?: (list list -> boolean)"
        "        to determine whether two lists are equal"))
    (hash-table-put!
      intermediate
      'char?
      '("char?: (any -> boolean)" "        "))
    (hash-table-put!
      intermediate
      'char=?
      '("char=?: (char char ... -> boolean)"
        "        to determine whether two characters are equal"))
    (hash-table-put!
      intermediate
      'char<?
      '("char<?: (char char ... -> boolean)"
        "        to determine whether a character precedes another"))
    (hash-table-put!
      intermediate
      'char>?
      '("char>?: (char char ... -> boolean)"
        "        to determine whether a character succeeds another"))
    (hash-table-put!
      intermediate
      'char<=?
      '("char<=?: (char char ... -> boolean)"
        "         to determine whether a character precedes another <NL>(or is equal to it)"))
    (hash-table-put!
      intermediate
      'char>=?
      '("char>=?: (char char ... -> boolean)"
        "         to determine whether a character succeeds another <NL>(or is equal to it)"))
    (hash-table-put!
      intermediate
      'char-ci=?
      '("char-ci=?: (char char ... -> boolean)"
        "           to determine whether two characters are equal <NL>in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'char-ci<?
      '("char-ci<?: (char char ... -> boolean)"
        "           to determine whether a character precedes another <NL>in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'char-ci>?
      '("char-ci>?: (char char ... -> boolean)"
        "           to determine whether a character succeeds another <NL>in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'char-ci<=?
      '("char-ci<=?: (char char ... -> boolean)"
        "            to determine whether a character precedes another <NL>(or is equal to it) in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'char-ci>=?
      '("char-ci>=?: (char char ... -> boolean)"
        "            to determine whether a character succeeds another <NL>(or is equal to it) in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'char-numeric?
      '("char-numeric?: (char -> boolean)"
        "               to determine whether a character represents a digit"))
    (hash-table-put!
      intermediate
      'char-alphabetic?
      '("char-alphabetic?: (char -> boolean)"
        "                  to determine whether a character represents <NL> an alphabetic character"))
    (hash-table-put!
      intermediate
      'char-whitespace?
      '("char-whitespace?: (char -> boolean)"
        "                  to determine whether a character represents space"))
    (hash-table-put!
      intermediate
      'char-upper-case?
      '("char-upper-case?: (char -> boolean)"
        "                  to determine whether a character is an <NL>upper-case character"))
    (hash-table-put!
      intermediate
      'char-lower-case?
      '("char-lower-case?: (char -> boolean)"
        "                  to determine whether a character is a <NL>lower-case character"))
    (hash-table-put!
      intermediate
      'char-upcase
      '("char-upcase: (char -> char)"
        "             to determine the equivalent upper-case character"))
    (hash-table-put!
      intermediate
      'char-downcase
      '("char-downcase: (char -> char)"
        "               to determine the equivalent lower-case character"))
    (hash-table-put!
      intermediate
      'char->integer
      '("char->integer: (char -> integer)"
        "               to lookup the number that corresponds to the<NL>given character in the ASCII table (if any)"))
    (hash-table-put!
      intermediate
      'string?
      '("string?: (any -> boolean)"
        "         to determine whether a value is a string"))
    (hash-table-put!
      intermediate
      'string-length
      '("string-length: (string -> nat)"
        "               to determine the length of a string"))
    (hash-table-put!
      intermediate
      'make-string
      '("make-string: (nat char -> string)"
        "             to produce a string of given length<NL>from a single given character"))
    (hash-table-put!
      intermediate
      'string-ref
      '("string-ref: (string nat -> char)"
        "            to extract the i-the character from a string"))
    (hash-table-put!
      intermediate
      'substring
      '("substring: (string nat nat -> string)"
        "           to extract the substring starting at a 0-based index,<NL> consisting of a given number of characters"))
    (hash-table-put!
      intermediate
      'string-copy
      '("string-copy: (string -> string)" "             to copy a string"))
    (hash-table-put!
      intermediate
      'string-append
      '("string-append: (string ... -> string)"
        "               to juxtapose the characters of several strings"))
    (hash-table-put!
      intermediate
      'string=?
      '("string=?: (string string ... -> boolean)"
        "          to compare two strings character-wise"))
    (hash-table-put!
      intermediate
      'string<?
      '("string<?: (string string ... -> boolean)"
        "          to determine whether one string alphabetically<NL>precedes another"))
    (hash-table-put!
      intermediate
      'string>?
      '("string>?: (string string ... -> boolean)"
        "          to determine whether one string alphabetically<NL>succeeds another"))
    (hash-table-put!
      intermediate
      'string<=?
      '("string<=?: (string string ... -> boolean)"
        "           to determine whether one string alphabetically<NL>precedes another (or is equal to it)"))
    (hash-table-put!
      intermediate
      'string>=?
      '("string>=?: (string string ... -> boolean)"
        "           to determine whether one string alphabetically<NL>succeeds another (or is equal to it)"))
    (hash-table-put!
      intermediate
      'string-ci=?
      '("string-ci=?: (string string ... -> boolean)"
        "             to compare two strings character-wise <NL>in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'string-ci<?
      '("string-ci<?: (string string ... -> boolean)"
        "             to determine whether one string alphabetically<NL>precedes another in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'string-ci>?
      '("string-ci>?: (string string ... -> boolean)"
        "             to determine whether one string alphabetically<NL>succeeds another in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'string-ci<=?
      '("string-ci<=?: (string string ... -> boolean)"
        "              to determine whether one string alphabetically<NL>precedes another (or is equal to it)<NL>in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'string-ci>=?
      '("string-ci>=?: (string string ... -> boolean)"
        "              to determine whether one string alphabetically<NL>succeeds another (or is equal to it)<NL>in a case-insensitive manner"))
    (hash-table-put!
      intermediate
      'string->number
      '("string->number: (string -> (union number false))"
        "                to convert a string into a number,<NL>produce false if impossible"))
    (hash-table-put!
      intermediate
      'string->list
      '("string->list: (string -> (listof char))"
        "              to convert a string into a list of characters"))
    (hash-table-put!
      intermediate
      'list->string
      '("list->string: ((listof char) -> string)"
        "              to convert a s list of characters into a string"))
    (hash-table-put!
      intermediate
      'format
      '("format: (string any ... -> string)"
        "        to format a string, possibly embedding values"))
    (hash-table-put!
      intermediate
      'make-posn
      '("make-posn: (number number -> posn)" "           to construct a posn"))
    (hash-table-put!
      intermediate
      'posn?
      '("posn?: (anything -> boolean)"
        "       to determine if its input is a posn"))
    (hash-table-put!
      intermediate
      'posn-x
      '("posn-x: (posn -> number)"
        "        to extract the x component of a posn"))
    (hash-table-put!
      intermediate
      'posn-y
      '("posn-y: (posn -> number)"
        "        to extract the y component of a posn"))
    (hash-table-put!
      intermediate
      'error
      '("error: (symbol string -> void)" "       to signal an error"))
    (hash-table-put!
      intermediate
      'struct?
      '("struct?: (any -> boolean)"
        "         to determine whether some value is a structure"))
    (hash-table-put!
      intermediate
      'equal?
      '("equal?: (any any -> boolean)"
        "        to compare two values, component by component"))
    (hash-table-put!
      intermediate
      'eq?
      '("eq?: (any any -> boolean)"
        "     to compare two values based on when they were created"))
    (hash-table-put!
      intermediate
      'exit
      '("exit: (-> void)" "      to exit the running program"))
    (hash-table-put!
      intermediate
      'eval
      '("eval: (any -> any)" "      to evaluate an sexpression"))
    (hash-table-put!
      intermediate
      'procedure?
      '("procedure?: (any -> boolean)" "            to recognize a procedure"))
    (hash-table-put!
      intermediate
      'time-apply
      '("time-apply: ((any ... -> any) any ... -> any)"
        "            applies the first argument to the rest of the arguments and time the function call"))
    (hash-table-put!
      intermediate
      'apply
      '("apply: ((any ... -> any) any ... -> any)"
        "       to apply the first argument to the rest of the arguments"))
    (hash-table-put!
      intermediate
      'values
      '("values: (any ... -> (values any ...))"
        "        to return multiple values"))
    (hash-table-put!
      intermediate
      'void
      '("void: (-> void)" "      to construct the void value"))
    (hash-table-put!
      intermediate
      'void?
      '("void?: (any -> boolean)" "       to recognize the void value"))
    (hash-table-put!
      intermediate
      'raise
      '("raise: (any -> void)" "       to raise an exception"))
    (hash-table-put!
      intermediate
      'make-exn:else
      '("make-exn:else: (string continuation-mark-set -> exn)"
        "               to construct an else exception"))
    (hash-table-put!
      intermediate
      'current-continuation-marks
      '("current-continuation-marks: (-> continuation-mark-set)"
        "                            to return the current set of continuation marks"))
    (hash-table-put!
      intermediate
      'require-library/proc
      '("require-library/proc: (string string -> void)"
        "                      to load a library"))
    (hash-table-put!
      advanced
      'number?
      '("number?: (any -> boolean)"
        "         to determine whether some value is a number"))
    (hash-table-put!
      advanced
      '=
      '("=: (num num num ... -> boolean)"
        "   to compare two numbers for equality"))
    (hash-table-put!
      advanced
      '<
      '("<: (real real real ... -> boolean)"
        "   to compare two real numbers for less-than"))
    (hash-table-put!
      advanced
      '>
      '(">: (real real real ... -> boolean)"
        "   to compare two real numbers for greater-than"))
    (hash-table-put!
      advanced
      '<=
      '("<=: (real real real ... -> boolean)"
        "    to compare two real numbers for less-than or equality"))
    (hash-table-put!
      advanced
      '>=
      '(">=: (real real -> boolean)"
        "    to compare two real numbers for greater-than or equality"))
    (hash-table-put!
      advanced
      '+
      '("+: (num num num ... -> num)"
        "   to compute the sum of the input numbers"))
    (hash-table-put!
      advanced
      '-
      '("-: (num num ... -> num)"
        "   to compute the difference between the input numbers or to negate a number, if there is only one input"))
    (hash-table-put!
      advanced
      '*
      '("*: (num num num ... -> num)"
        "   to compute the product of all of the input numbers"))
    (hash-table-put!
      advanced
      '/
      '("/: (num num num ... -> num)"
        "   to compute the rational quotient of its input. 
              None but the first number can be zero."))
    (hash-table-put!
      advanced
      'max
      '("max: (num num ... -> num)" "     to determine the largest number"))
    (hash-table-put!
      advanced
      'min
      '("min: (num num ... -> num)" "     to determine the smallest number"))
    (hash-table-put!
      advanced
      'quotient
      '("quotient: (int int -> int)"
        "          to compute the quotient of two integers"))
    (hash-table-put!
      advanced
      'remainder
      '("remainder: (int int -> int)"
        "           to compute the remainder of two integers"))
    (hash-table-put!
      advanced
      'modulo
      '("modulo: (int int -> int)"
        "        to compute the modulo of two integers"))
    (hash-table-put!
      advanced
      'square
      '("square: (num -> num)" "        to compute the square of a number"))
    (hash-table-put!
      advanced
      'sqrt
      '("sqrt: (num -> num)" "      to compute the square root of a number"))
    (hash-table-put!
      advanced
      'expt
      '("expt: (num num -> num)"
        "      to compute the exponent of two numbers"))
    (hash-table-put!
      advanced
      'abs
      '("abs: (real -> real)"
        "     to compute the absolute value of a real number"))
    (hash-table-put!
      advanced
      'exp
      '("exp: (num -> num)" "     to compute e raised to a number"))
    (hash-table-put!
      advanced
      'log
      '("log: (num -> num)"
        "     to compute the base-e logarithm of a number"))
    (hash-table-put!
      advanced
      'sin
      '("sin: (num -> num)" "     to compute the sine of a number (radians)"))
    (hash-table-put!
      advanced
      'cos
      '("cos: (num -> num)"
        "     to compute the cosine of a number (radians)"))
    (hash-table-put!
      advanced
      'tan
      '("tan: (num -> num)"
        "     to compute the tangent of a number (radians)"))
    (hash-table-put!
      advanced
      'asin
      '("asin: (num -> num)"
        "      to compute the arcsine (inverse of sin) of a number"))
    (hash-table-put!
      advanced
      'acos
      '("acos: (num -> num)"
        "      to compute the arccosine (inverse of cos) of a number"))
    (hash-table-put!
      advanced
      'atan
      '("atan: (num -> num)"
        "      to compute the arctan (inverse of tan) of a number"))
    (hash-table-put!
      advanced
      'sinh
      '("sinh: (num -> num)"
        "      to compute the hyperbolic cosine of a number"))
    (hash-table-put!
      advanced
      'cosh
      '("cosh: (num -> num)"
        "      to compute the hyperbolic cosine of a number"))
    (hash-table-put!
      advanced
      'exact?
      '("exact?: (num -> bool)"
        "        to determine whether some number is exact"))
    (hash-table-put!
      advanced
      'integer?
      '("integer?: (any -> bool)"
        "          to determine whether some value is an integer (exact or inexact)"))
    (hash-table-put!
      advanced
      'zero?
      '("zero?: (number -> bool)"
        "       to determine if some value is zero or not"))
    (hash-table-put!
      advanced
      'odd?
      '("odd?: (integer -> bool)"
        "      to determine if some value is odd or not"))
    (hash-table-put!
      advanced
      'even?
      '("even?: (integer -> bool)"
        "       to determine if some value is even or not"))
    (hash-table-put!
      advanced
      'add1
      '("add1: (number -> number)"
        "      to compute a number one larger than a given number"))
    (hash-table-put!
      advanced
      'sub1
      '("sub1: (number -> number)"
        "      to compute a number one smaller than a given number"))
    (hash-table-put!
      advanced
      'lcm
      '("lcm: (int int ... -> int)"
        "     to compute the least common multiple of two integers"))
    (hash-table-put!
      advanced
      'gcd
      '("gcd: (int int ... -> int)"
        "     to compute the greatest common divisior"))
    (hash-table-put!
      advanced
      'rational?
      '("rational?: (any -> bool)"
        "           to determine whether some value is rational number"))
    (hash-table-put!
      advanced
      'numerator
      '("numerator: (rat -> int)"
        "           to compute the numerator of a rational"))
    (hash-table-put!
      advanced
      'denominator
      '("denominator: (rat -> int)"
        "             to compute the denominator of a rational"))
    (hash-table-put!
      advanced
      'inexact?
      '("inexact?: (num -> bool)"
        "          to determine whether some number is inexact"))
    (hash-table-put!
      advanced
      'real?
      '("real?: (any -> bool)"
        "       to determine whether some value is a real number"))
    (hash-table-put!
      advanced
      'floor
      '("floor: (real -> int)"
        "       to determine the closest integer below a real number"))
    (hash-table-put!
      advanced
      'ceiling
      '("ceiling: (real -> int)"
        "         to determine the closest integer above a real number"))
    (hash-table-put!
      advanced
      'round
      '("round: (real -> int)" "       to round a real number to an integer"))
    (hash-table-put!
      advanced
      'complex?
      '("complex?: (any -> bool)"
        "          to determine whether some value is complex"))
    (hash-table-put!
      advanced
      'make-polar
      '("make-polar: (real real -> num)"
        "            to create a complex from a magnitude and angle"))
    (hash-table-put!
      advanced
      'real-part
      '("real-part: (num -> real)"
        "           to extract the real part from a complex number"))
    (hash-table-put!
      advanced
      'imag-part
      '("imag-part: (num -> real)"
        "           to extract the imaginary part from a complex"))
    (hash-table-put!
      advanced
      'magnitude
      '("magnitude: (num -> real)"
        "           to determine the magnitude of a complex number"))
    (hash-table-put!
      advanced
      'angle
      '("angle: (num -> real)"
        "       to extract the angle from a complex number"))
    (hash-table-put!
      advanced
      'conjugate
      '("conjugate: (num -> num)"
        "           to compute the conjugate of a complex number"))
    (hash-table-put!
      advanced
      'exact->inexact
      '("exact->inexact: (num -> num)"
        "                to convert an exact number to an inexact one"))
    (hash-table-put!
      advanced
      'inexact->exact
      '("inexact->exact: (num -> num)"
        "                to approximate an inexact number by an exact one"))
    (hash-table-put!
      advanced
      'number->string
      '("number->string: (num -> string)"
        "                to convert a number to a string"))
    (hash-table-put!
      advanced
      'integer->char
      '("integer->char: (int -> char)"
        "               to lookup the character that corresponds to the<NL>given integer in the ASCII table (if any)"))
    (hash-table-put!
      advanced
      'random
      '("random: (int -> int)"
        "        to generate a random natural number <NL> less than some given integer"))
    (hash-table-put!
      advanced
      'current-seconds
      '("current-seconds: (-> int)"
        "                 to compute the current time in seconds elapsed <NL> (since a platform-specific starting date)"))
    (hash-table-put! advanced 'e '("e: real" "   Euler's number"))
    (hash-table-put!
      advanced
      'pi
      '("pi: real"
        "    the ratio of a circle's circumference to its diameter"))
    (hash-table-put!
      advanced
      'boolean?
      '("boolean?: (any -> boolean)"
        "          to determine whether some value is a boolean"))
    (hash-table-put!
      advanced
      'boolean=?
      '("boolean=?: (boolean boolean -> boolean)"
        "           to determine whether two booleans are equal"))
    (hash-table-put!
      advanced
      'not
      '("not: (boolean -> boolean)"
        "     to compute the negation of a boolean value"))
    (hash-table-put!
      advanced
      'symbol?
      '("symbol?: (any -> boolean)"
        "         to determine whether some value is a symbol"))
    (hash-table-put!
      advanced
      'symbol=?
      '("symbol=?: (symbol symbol -> boolean)"
        "          to determine whether two symbols are equal"))
    (hash-table-put!
      advanced
      'cons?
      '("cons?: (any -> boolean)"
        "       to determine whether some value is a constructed list"))
    (hash-table-put!
      advanced
      'pair?
      '("pair?: (any -> boolean)"
        "       to determine whether some value is a constructed list"))
    (hash-table-put!
      advanced
      'empty?
      '("empty?: (any -> boolean)"
        "        to determine whether some value is the empty list"))
    (hash-table-put!
      advanced
      'null?
      '("null?: (any -> boolean)"
        "       to determine whether some value is the empty list"))
    (hash-table-put!
      advanced
      'list?
      '("list?: (any -> boolean)"
        "       to determine whether some value is a list"))
    (hash-table-put!
      advanced
      'cons
      '("cons: (x (listof x) -> (listof x))" "      to construct a list"))
    (hash-table-put!
      advanced
      'first
      '("first: ((cons y (listof x)) -> y)"
        "       to select the first item of a non-empty list"))
    (hash-table-put!
      advanced
      'car
      '("car: ((cons y (listof x)) -> y)"
        "     to select the first item of a non-empty list"))
    (hash-table-put!
      advanced
      'rest
      '("rest: ((cons y (listof x)) -> (listof x))"
        "      to select the rest of a non-empty list"))
    (hash-table-put!
      advanced
      'cdr
      '("cdr: ((cons y (listof x)) -> (listof x))"
        "     to select the rest of a non-empty list"))
    (hash-table-put!
      advanced
      'second
      '("second: ((cons z (cons y (listof x))) -> y)"
        "        to select the second item of a non-empty list"))
    (hash-table-put!
      advanced
      'cadr
      '("cadr: ((cons z (cons y (listof x))) -> y)"
        "      to select the second item of a non-empty list"))
    (hash-table-put!
      advanced
      'third
      '("third: ((cons w (cons z (cons y (listof x)))) -> y)"
        "       to select the third item of a non-empty list"))
    (hash-table-put!
      advanced
      'caddr
      '("caddr: ((cons w (cons z (cons y (listof x)))) -> y)"
        "       to select the third item of a non-empty list"))
    (hash-table-put!
      advanced
      'fourth
      '("fourth: ((listof y) -> y)"
        "        to select the fourth item of a non-empty list"))
    (hash-table-put!
      advanced
      'cadddr
      '("cadddr: ((listof y) -> y)"
        "        to select the fourth item of a non-empty list"))
    (hash-table-put!
      advanced
      'fifth
      '("fifth: ((listof y) -> y)"
        "       to select the fifth item of a non-empty list"))
    (hash-table-put!
      advanced
      'sixth
      '("sixth: ((listof y) -> y)"
        "       to select the sixth item of a non-empty list"))
    (hash-table-put!
      advanced
      'seventh
      '("seventh: ((listof y) -> y)"
        "         to select the seventh item of a non-empty list"))
    (hash-table-put!
      advanced
      'eighth
      '("eighth: ((listof y) -> y)"
        "        to select the eighth item of a non-empty list"))
    (hash-table-put!
      advanced
      'list-ref
      '("list-ref: ((listof x) number -> x)"
        "          to extract the i-th item from a list"))
    (hash-table-put!
      advanced
      'list
      '("list: (any ... -> (listof any))"
        "      to construct a list of its arguments"))
    (hash-table-put!
      advanced
      'append
      '("append: ((listof any) ... -> (listof any))"
        "        to create a single list from several, by juxtaposition of the items"))
    (hash-table-put!
      advanced
      'length
      '("length: (list -> number)"
        "        to compute the number of items on a list"))
    (hash-table-put!
      advanced
      'memq
      '("memq: (any list -> (union false list))"
        "      to determine whether some value is on some list <NL> (comparing values with eq?)"))
    (hash-table-put!
      advanced
      'memv
      '("memv: (any list -> (union false list))"
        "      to determine whether some value is on the list <NL> (comparing values with eqv?)"))
    (hash-table-put!
      advanced
      'member
      '("member: (any list -> (union false list))"
        "        to determine whether some value is on the list <NL> (comparing values with equal?)"))
    (hash-table-put!
      advanced
      'reverse
      '("reverse: (list -> list)"
        "         to create a reversed version of a list"))
    (hash-table-put!
      advanced
      'assq
      '("assq: (x (listof (cons x y)) -> (union false (cons x y)))"
        "      to determine whether some item is the first item of a pair <NL> in a list of pairs"))
    (hash-table-put!
      advanced
      'equal?
      '("equal?: (list list -> boolean)"
        "        to determine whether two lists are equal"))
    (hash-table-put!
      advanced
      'set-first!
      '("set-first!: ((cons y (listof x)) y -> void)"
        "            to update the first item of a non-empty list"))
    (hash-table-put!
      advanced
      'set-rest!
      '("set-rest!: ((cons y (listof x)) (listof x) -> void)"
        "           to update the rest of a non-empty list"))
    (hash-table-put!
      advanced
      'set-car!
      '("set-car!: ((cons y (listof x)) y -> void)"
        "          to update the first item of a non-empty list"))
    (hash-table-put!
      advanced
      'set-cdr!
      '("set-cdr!: ((cons y (listof x)) (listof x) -> void)"
        "          to update the rest of a non-empty list"))
    (hash-table-put! advanced 'char? '("char?: (any -> boolean)" "        "))
    (hash-table-put!
      advanced
      'char=?
      '("char=?: (char char ... -> boolean)"
        "        to determine whether two characters are equal"))
    (hash-table-put!
      advanced
      'char<?
      '("char<?: (char char ... -> boolean)"
        "        to determine whether a character precedes another"))
    (hash-table-put!
      advanced
      'char>?
      '("char>?: (char char ... -> boolean)"
        "        to determine whether a character succeeds another"))
    (hash-table-put!
      advanced
      'char<=?
      '("char<=?: (char char ... -> boolean)"
        "         to determine whether a character precedes another <NL>(or is equal to it)"))
    (hash-table-put!
      advanced
      'char>=?
      '("char>=?: (char char ... -> boolean)"
        "         to determine whether a character succeeds another <NL>(or is equal to it)"))
    (hash-table-put!
      advanced
      'char-ci=?
      '("char-ci=?: (char char ... -> boolean)"
        "           to determine whether two characters are equal <NL>in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'char-ci<?
      '("char-ci<?: (char char ... -> boolean)"
        "           to determine whether a character precedes another <NL>in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'char-ci>?
      '("char-ci>?: (char char ... -> boolean)"
        "           to determine whether a character succeeds another <NL>in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'char-ci<=?
      '("char-ci<=?: (char char ... -> boolean)"
        "            to determine whether a character precedes another <NL>(or is equal to it) in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'char-ci>=?
      '("char-ci>=?: (char char ... -> boolean)"
        "            to determine whether a character succeeds another <NL>(or is equal to it) in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'char-numeric?
      '("char-numeric?: (char -> boolean)"
        "               to determine whether a character represents a digit"))
    (hash-table-put!
      advanced
      'char-alphabetic?
      '("char-alphabetic?: (char -> boolean)"
        "                  to determine whether a character represents <NL> an alphabetic character"))
    (hash-table-put!
      advanced
      'char-whitespace?
      '("char-whitespace?: (char -> boolean)"
        "                  to determine whether a character represents space"))
    (hash-table-put!
      advanced
      'char-upper-case?
      '("char-upper-case?: (char -> boolean)"
        "                  to determine whether a character is an <NL>upper-case character"))
    (hash-table-put!
      advanced
      'char-lower-case?
      '("char-lower-case?: (char -> boolean)"
        "                  to determine whether a character is a <NL>lower-case character"))
    (hash-table-put!
      advanced
      'char-upcase
      '("char-upcase: (char -> char)"
        "             to determine the equivalent upper-case character"))
    (hash-table-put!
      advanced
      'char-downcase
      '("char-downcase: (char -> char)"
        "               to determine the equivalent lower-case character"))
    (hash-table-put!
      advanced
      'char->integer
      '("char->integer: (char -> integer)"
        "               to lookup the number that corresponds to the<NL>given character in the ASCII table (if any)"))
    (hash-table-put!
      advanced
      'string?
      '("string?: (any -> boolean)"
        "         to determine whether a value is a string"))
    (hash-table-put!
      advanced
      'string-length
      '("string-length: (string -> nat)"
        "               to determine the length of a string"))
    (hash-table-put!
      advanced
      'make-string
      '("make-string: (nat char -> string)"
        "             to produce a string of given length<NL>from a single given character"))
    (hash-table-put!
      advanced
      'string-ref
      '("string-ref: (string nat -> char)"
        "            to extract the i-the character from a string"))
    (hash-table-put!
      advanced
      'substring
      '("substring: (string nat nat -> string)"
        "           to extract the substring starting at a 0-based index,<NL> consisting of a given number of characters"))
    (hash-table-put!
      advanced
      'string-copy
      '("string-copy: (string -> string)" "             to copy a string"))
    (hash-table-put!
      advanced
      'string-append
      '("string-append: (string ... -> string)"
        "               to juxtapose the characters of several strings"))
    (hash-table-put!
      advanced
      'string=?
      '("string=?: (string string ... -> boolean)"
        "          to compare two strings character-wise"))
    (hash-table-put!
      advanced
      'string<?
      '("string<?: (string string ... -> boolean)"
        "          to determine whether one string alphabetically<NL>precedes another"))
    (hash-table-put!
      advanced
      'string>?
      '("string>?: (string string ... -> boolean)"
        "          to determine whether one string alphabetically<NL>succeeds another"))
    (hash-table-put!
      advanced
      'string<=?
      '("string<=?: (string string ... -> boolean)"
        "           to determine whether one string alphabetically<NL>precedes another (or is equal to it)"))
    (hash-table-put!
      advanced
      'string>=?
      '("string>=?: (string string ... -> boolean)"
        "           to determine whether one string alphabetically<NL>succeeds another (or is equal to it)"))
    (hash-table-put!
      advanced
      'string-ci=?
      '("string-ci=?: (string string ... -> boolean)"
        "             to compare two strings character-wise <NL>in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'string-ci<?
      '("string-ci<?: (string string ... -> boolean)"
        "             to determine whether one string alphabetically<NL>precedes another in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'string-ci>?
      '("string-ci>?: (string string ... -> boolean)"
        "             to determine whether one string alphabetically<NL>succeeds another in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'string-ci<=?
      '("string-ci<=?: (string string ... -> boolean)"
        "              to determine whether one string alphabetically<NL>precedes another (or is equal to it)<NL>in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'string-ci>=?
      '("string-ci>=?: (string string ... -> boolean)"
        "              to determine whether one string alphabetically<NL>succeeds another (or is equal to it)<NL>in a case-insensitive manner"))
    (hash-table-put!
      advanced
      'string->number
      '("string->number: (string -> (union number false))"
        "                to convert a string into a number,<NL>produce false if impossible"))
    (hash-table-put!
      advanced
      'string->list
      '("string->list: (string -> (listof char))"
        "              to convert a string into a list of characters"))
    (hash-table-put!
      advanced
      'list->string
      '("list->string: ((listof char) -> string)"
        "              to convert a s list of characters into a string"))
    (hash-table-put!
      advanced
      'format
      '("format: (string any ... -> string)"
        "        to format a string, possibly embedding values"))
    (hash-table-put!
      advanced
      'make-posn
      '("make-posn: (number number -> posn)" "           to construct a posn"))
    (hash-table-put!
      advanced
      'posn?
      '("posn?: (anything -> boolean)"
        "       to determine if its input is a posn"))
    (hash-table-put!
      advanced
      'posn-x
      '("posn-x: (posn -> number)"
        "        to extract the x component of a posn"))
    (hash-table-put!
      advanced
      'posn-y
      '("posn-y: (posn -> number)"
        "        to extract the y component of a posn"))
    (hash-table-put!
      advanced
      'set-posn-x!
      '("set-posn-x!: (posn number -> void)"
        "             to update the x component of a posn"))
    (hash-table-put!
      advanced
      'set-posn-y!
      '("set-posn-y!: (posn number -> void)"
        "             to update the x component of a posn"))
    (hash-table-put!
      advanced
      'error
      '("error: (symbol string -> void)" "       to signal an error"))
    (hash-table-put!
      advanced
      'struct?
      '("struct?: (any -> boolean)"
        "         to determine whether some value is a structure"))
    (hash-table-put!
      advanced
      'equal?
      '("equal?: (any any -> boolean)"
        "        to compare two values, component by component"))
    (hash-table-put!
      advanced
      'eq?
      '("eq?: (any any -> boolean)"
        "     to compare two values based on when they were created"))
    (hash-table-put!
      advanced
      'exit
      '("exit: (-> void)" "      to exit the running program"))
    (hash-table-put!
      advanced
      'eval
      '("eval: (any -> any)" "      to evaluate an sexpression"))
    (hash-table-put!
      advanced
      'procedure?
      '("procedure?: (any -> boolean)" "            to recognize a procedure"))
    (hash-table-put!
      advanced
      'time-apply
      '("time-apply: ((any ... -> any) any ... -> any)"
        "            applies the first argument to the rest of the arguments and time the function call"))
    (hash-table-put!
      advanced
      'apply
      '("apply: ((any ... -> any) any ... -> any)"
        "       to apply the first argument to the rest of the arguments"))
    (hash-table-put!
      advanced
      'values
      '("values: (any ... -> (values any ...))"
        "        to return multiple values"))
    (hash-table-put!
      advanced
      'void
      '("void: (-> void)" "      to construct the void value"))
    (hash-table-put!
      advanced
      'void?
      '("void?: (any -> boolean)" "       to recognize the void value"))
    (hash-table-put!
      advanced
      'raise
      '("raise: (any -> void)" "       to raise an exception"))
    (hash-table-put!
      advanced
      'make-exn:else
      '("make-exn:else: (string continuation-mark-set -> exn)"
        "               to construct an else exception"))
    (hash-table-put!
      advanced
      'current-continuation-marks
      '("current-continuation-marks: (-> continuation-mark-set)"
        "                            to return the current set of continuation marks"))
    (hash-table-put!
      advanced
      'require-library/proc
      '("require-library/proc: (string string -> void)"
        "                      to load a library"))
    (hash-table-put!
      advanced
      'force
      '("force: (delay -> any)"
        "       to find the delayed value. See also delay."))
    (hash-table-put!
      advanced
      'promise?
      '("promise?: (any -> boolean)"
        "          to determine if a value is delayed."))
    (hash-table-put!
      advanced
      'make-promise
      '("make-promise: ((-> any) -> boolean)"
        "              to construct a delay."))
    (hash-table-put!
      advanced
      'map
      '("map: ((x ... -> z) (listof x) ... -> (listof z))"
        "     to construct a new list by applying a function to each item on an existing existing"))
    (hash-table-put!
      advanced
      'for-each
      '("for-each: ((any ... -> any) (listof any) ... -> void)"
        "          to apply a function to each item on a list for effect only"))
    (hash-table-put!
      advanced
      'filter
      '("filter: ((x -> boolean) (listof x) -> (listof x))"
        "        to construct a list from all those items on  a list for which the predicate holds"))
    (hash-table-put!
      advanced
      'foldr
      '("foldr: ((x y -> z) y (listof x) -> z)"
        "       (foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))"))
    (hash-table-put!
      advanced
      'foldl
      '("foldl: ((x y -> z) y (listof x) -> z)"
        "       (foldl f base (list x-1 ... x-n)) = (f x-n ... (f x-1 base))"))
    (hash-table-put!
      advanced
      'build-list
      '("build-list: (nat (nat -> x) -> (listof x))"
        "            (build-list n f) = (list (f 0) ... (f (- n 1)))"))
    (hash-table-put!
      advanced
      'quicksort
      '("quicksort: ((x x -> boolean) (listof x) -> (listof x))"
        "           to construct a list from all items on a list in an order according to a predicate"))
    (hash-table-put!
      advanced
      'andmap
      '("andmap: ((x -> boolean) (listof x) -> boolean)"
        "        (andmap p (list x-1 ... x-n)) = (and (p x-1) (and ... (p x-n)))"))
    (hash-table-put!
      advanced
      'ormap
      '("ormap: ((x -> boolean) (listof x) -> boolean)"
        "       (ormap p (list x-1 ... x-n)) = (or (p x-1) (and ... (p x-n)))"))
    (hash-table-put!
      advanced
      'vector
      '("vector: (x ... -> (vector x ...))" "        to construct a vector"))
    (hash-table-put!
      advanced
      'make-vector
      '("make-vector: (number x -> (vectorof x))"
        "             to construct a vector"))
    (hash-table-put!
      advanced
      'build-vector
      '("build-vector: (nat (nat -> x) -> (vectorof x))"
        "              to construct a vector"))
    (hash-table-put!
      advanced
      'vector-ref
      '("vector-ref: ((vector x) nat -> x)"
        "            to extract an element from a vector"))
    (hash-table-put!
      advanced
      'vector-length
      '("vector-length: ((vector x) -> nat)"
        "               to determine the length of a vector"))
    (hash-table-put!
      advanced
      'vector-set!
      '("vector-set!: ((vectorof x) nat x -> (vectorof x))"
        "             to update a vector"))
    (hash-table-put!
      advanced
      'vector?
      '("vector?: (any -> boolean)"
        "         to determine if a value is a vector"))
    (hash-table-put!
      advanced
      'box
      '("box: (any -> box)" "     to construct a box"))
    (hash-table-put!
      advanced
      'unbox
      '("unbox: (box -> any)" "       to extract the boxed value"))
    (hash-table-put!
      advanced
      'set-box!
      '("set-box!: (box any -> void)" "          to update a box"))
    (hash-table-put!
      advanced
      'box?
      '("box?: (any -> boolean)" "      to determine if a value is a box"))
    (hash-table-put!
      advanced
      'call/cc
      '("call/cc: ((cont -> any) -> any)"
        "         to capture the current continuation"))
    (hash-table-put!
      advanced
      'call-with-current-continuation
      '("call-with-current-continuation: ((cont -> any) -> any)"
        "                                to capture the current continuation"))
    (hash-table-put!
      advanced
      'turtles
      '("turtles: (-> void)"
        "         to toggle the view of the turtles window"))
    (hash-table-put!
      advanced
      'turtles
      '("turtles: (bool -> void)"
        "         to open or close the turtles window, based on the argument"))
    (hash-table-put!
      advanced
      'turn
      '("turn: (number -> void)"
        "      to turn the turtles by a number of degrees"))
    (hash-table-put!
      advanced
      'turn/radians
      '("turn/radians: (number -> void)"
        "              to turn the turtles by a number of radians"))
    (hash-table-put!
      advanced
      'move
      '("move: (number -> void)" "      to move the turtles forward"))
    (hash-table-put!
      advanced
      'erase
      '("erase: (number -> void)"
        "       to erase the path in front of each turtle"))
    (hash-table-put!
      advanced
      'move-offfset
      '("move-offfset: (number number -> void)"
        "              to move by a delta-x and delta-y"))
    (hash-table-put!
      advanced
      'draw-offfset
      '("draw-offfset: (number number -> void)"
        "              to draw by a delta-x and delta-y"))
    (hash-table-put!
      advanced
      'erase-offfset
      '("erase-offfset: (number number -> void)"
        "               to erase by a delta-x and delta-y"))
    (hash-table-put!
      advanced
      'clear
      '("clear: (-> void)"
        "       to erase the drawing of the turtles entirely"))
    (hash-table-put!
      advanced
      'save-turtle-bitmap
      '("save-turtle-bitmap: (string (union (quote xbm) (quote xpm) (quote pict)) -> void)"
        "                    to save the current turtles to a platform-specific filetype"))
    (hash-table-put!
      advanced
      'splitfn
      '("splitfn: ((-> void) -> void)"
        "         to split the turtles (used in the expansion of `split')"))
    (hash-table-put!
      advanced
      'split*fn
      '("split*fn: ((listof (-> void)) -> void)"
        "          to split the turtles (used in the expansion of `split*')"))
    (hash-table-put!
      advanced
      'tpromptfn
      '("tpromptfn: ((-> void) -> void)"
        "           to set a turtle prompt (used in the expansion of `tprompt')"))))
