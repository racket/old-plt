;sxm issues warnings for forward-references,
;which can't all be removed anyway

(warning-handler (lambda zzz #f))

(scmxlate-include "make-qualified-names.scm")

(scmxlate-rename
 (flush-output flush-output-port)
 )


