;; Every request contains a URL which contains a path. The purpose of the URL
;; is to identify a resource on the server. The path encoded in the URL is a
;; "virtual" path in the sense that it does not refer directly to the file
;; system or other storage medium on the server. The purpose of the resolver
;; is to map virtual paths to real paths. The process of resolving virtual
;; paths will be called "resource resolution".

;; The user will specify a resource map as a relation between the virtual file
;; system and the server's file system. The domain of the relation will be the
;; virtual file system, while the range is the server file system. It is
;; acceptible for the relation to be many-to-one, as multiple virtual paths may
;; be aliases for the same real resource. The relation must be a function,
;; otherwise it is ambiguous which path real path corresponds to a given
;; virtual path.

;; The user will specify the resource map by providing pairs of paths. The
;; first path in each pair will refer to the virtual file system, while the
;; second path refers to the server file system. The following data definition
;; is given for paths:

;; a path consists of a file-part and a directory-part
;; -- The file-part is just a string. Either "" or something non-trivial
;;    E.g. "", "bunny.gif", "homework-servlet.ss"
;; -- The directory-part is a (listof string)

;; A path can be thought of as a sequence of tokens, where the tokens are drawn
;; from the alphabet of files and directories. The file-part of a path is just
;; the last token in the sequence. The file-part is provided separately to
;; distinguish between paths which identify directories and paths which
;; identify files.

;; The user must specify a possibly infinite number of pairs in the resource
;; map using a finite specification. The full resource map, R, is established
;; inductively:
;; BASE: Every pair in the specification is automatically in R. (Note, that a
;; valid specification must already be a function.)
;;
;; STEP: For virtual-path, v, suppose there exists a pair (v', s') in R, such
;; that v' is a prefix of v. Let (v'', s'') in R, be such that v'' is the
;; longest prefix of v. Now v can be written v = v''w for some suffix w. Deduce
;; that (v, s''w) in R.

;; Resources can be either static or dynamic. Static resources are the simplest
;; to understand and refer most directly to the actual contents of a file or
;; directory on the server. All other resources are dynamic, and require
;; executing a script to generate the resource "on the fly." Depending on the
;; type of the paths involved, and the type of the resource being accessed, not
;; every pair of paths has an interpretation in the resource map. The following
;; table sumarizes which pairs are valid and which are not:

;; -------------------------------------------------------------
;;              |                      Real Path
;; -------------------------------------------------------------
;; virtual-path | static-file | static-directory | dynamic-file
;; -------------------------------------------------------------
;;     file     |      OK     |      invalid     |    valid
;; -------------------------------------------------------------
;;   directory  |   invalid   |        OK        |     OK

;; When specifying a pair in the base resource map, the user must specify
;; whether it is a static or dynamic resource. Whether a resource is static or
;; dynamic is preserved by the induction. I.e. if in the inductive step (see
;; above), s'' is a static (dynamic) resource, then deduce that s''w must also
;; be a static (dynamic) resource.

;; resource resolution proceeds in three steps:

;; 1. Removing relative references. I.e. getting rid of dots such as "." and
;;    ".." I have observed that most browsers take care of this before the
;;    request is sent, however for security reasons it is wise to do this on
;;    the server as well. If you don't do this, then an attacker could
;;    potentially exploit path chasing to gain access to a file that is
;;    otherwise not accessible.
;;       The resulting path may be invalid, i.e. it may have ".."s in it which
;;    go up above the root of the virtual file system. Such requests are
;;    discarded and an error is reported to the browser. A resulting valid path
;;    will be referred to as an "absolute" path. The domain of the resource map
;;    is drawn only from the set of absolute paths.

;; 2. Once the absolute virtual path is established, apply the resource map to
;;    obtain a real path.

;; 3. Case virtual-path is a file and the real-path is a static-file:
;;       Serve the static file to the user.
;;    Case virtual-path is a file and the real-path is a static-directory:
;;       This is an invalid combination, however note that sometimes a user may
;;       place neglect putting a slash at the end of a URL so that a
;;       directory-path is interpreted as a file-path. This should be detected
;;       and a 301 should be sent back to the browser redirecting to the
;;       correct directory version.
;;    Case virtual-path is a file and the real-path is a dynamic-file:
;;       Load the servlet, or find the servlet instance and invoke.
;;    Case virtual-path is a directory and the real-path is a static-directory:
;;       Apply the indices, i.e. look for index.html.
;;    Case virtual-path is a directory and the real-path is a dynamic-file:
;;       Load the servlet, or find the servlet instance and invoke.

;; Note: it may be useful to have dynamic-directories. For a dynamic-directory
;; the server would look for some default "index.ss" to load.


