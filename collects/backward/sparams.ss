(define-signature plt:parameters^
  (case-sensitive?
   allow-one-armed-if?
   allow-set!-on-undefined?
   allow-internal-defines?
   allow-improper-lists?
   allow-improper-lists-in-lambda?
   unmatched-cond/case-is-error?
   check-syntax-level)) ; 'core, 'structured 'side-effecting, or 'advanced
