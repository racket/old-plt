(module gl-vectors mzscheme
  (require "gl-vectors/gl-double-vector.ss"
           "gl-vectors/gl-float-vector.ss"
           "gl-vectors/gl-uint-vector.ss"
           "gl-vectors/gl-ushort-vector.ss"
           "gl-vectors/gl-ubyte-vector.ss"
           "gl-vectors/gl-int-vector.ss"
           "gl-vectors/gl-short-vector.ss"
           "gl-vectors/gl-byte-vector.ss"
           "gl-vectors/gl-boolean-vector.ss")
  
  (provide (all-from "gl-vectors/gl-double-vector.ss")
           (all-from "gl-vectors/gl-float-vector.ss")
           (all-from "gl-vectors/gl-uint-vector.ss")
           (all-from "gl-vectors/gl-ushort-vector.ss")
           (all-from "gl-vectors/gl-ubyte-vector.ss")
           (all-from "gl-vectors/gl-int-vector.ss")
           (all-from "gl-vectors/gl-short-vector.ss")
           (all-from "gl-vectors/gl-byte-vector.ss")
           (all-from "gl-vectors/gl-boolean-vector.ss")))
