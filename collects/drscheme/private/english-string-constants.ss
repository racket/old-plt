(
 (drscheme "DrScheme")

 ;; Language dialog
 (language-dialog-title "Configure Language")
 (case-sensitive-label "Case sensitive")
 (output-style-label "Output Style")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insert newlines in printed values")
 (input-syntax "Input Syntax")
 (output-syntax "Output Syntax")
 
 ;; repl stuff
 (evaluation-terminated "Evaluation Terminated")
 (evaluation-terminated-explanation
  "The evaluation thread is no longer running, so no evaluation can take place until the next execution.")
 (last-stack-frame "show the last stack frame")
 (more-stack-frames "show the ~a ~a stack frames")
 
 ;; welcoming message in repl
 (language "Language")
 (custom "custom")
 (teachpack "Teachpack")
 (welcome-to "Welcome to")
 (version "version")
 
 ;; kill evaluation dialog
 (kill-evaluation? "Do you want to kill the evaluation?")
 (just-break "Just Break")
 (kill "Kill")
 (kill? "Kill?")
 )
