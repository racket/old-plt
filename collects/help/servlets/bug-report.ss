(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/hd-css.ss")
(require "private/util.ss")
(require "private/synthesize.ss")
(require "private/external.ss")

(unit/sig ()
  (import servlet^)

  (define external? (unbox external-box))

  (define (get-pref/external sym)
    (if external?	
	""
	(get-pref/default sym "")))

  (define name (get-pref/external 'user-name))
  (define email (get-pref/external 'user-email))

  (define (make-option s)
    (if (string=? (car s) "*")
        `(OPTION ((SELECTED "true")
                  (VALUE ,(caddr s))) ,(cadr s))
        `(OPTION ((VALUE ,(cadr s))) ,(car s))))
  
  ; starred one is default
  (define available-severities
    '(("critical" "critical")
      ("*" "serious" "serious")
      ("non-critical" "non-critical")))

  (define severity-option-links
    (map make-option available-severities))

  ; starred one is default
  (define available-bug-classes
    '(("*" "software bug" "sw-bug")
      ("documentation bug" "doc-bug")
      ("change request" "change-request")
      ("support" "support")))

  (define bug-class-option-links
    (map make-option available-bug-classes))

  ; starred one is default
  (define available-priorities
    '(("high" "high")
      ("*" "medium" "medium")
      ("low" "low")))

  (define priority-option-links 
    (map make-option available-priorities))

  `(HTML 
    (HEAD ,hd-css)	
    (BODY 
     (TABLE ((CELLPADDING "0")
	     (CELLSPACING "0")
	     (COLS "2")
	     (WIDTH "85%"))
	    (TR 
	     (TD (FONT ((SIZE "+2")) (B "Send bug report to PLT")))
	     (TD ((ALIGN "right")) 
		 ,home-page)))
     (P)
     (FORM ((ACTION "/servlets/confirm-bug.ss")
	    (METHOD "POST"))
	   (B  "Name:") (BR)
	   (INPUT ((TYPE "hidden")
		   (NAME "synth-info")
		   (VALUE ,(format "~s" (get-synthesized-info)))))
	   (INPUT ((TYPE "text")
		   (NAME "originator")
		   (VALUE ,name)
		   (SIZE "60")))
	   (BR)
	   (B  "E-mail address:") (BR)
	   (INPUT ((TYPE "text")
		   (NAME "reply-to")
		   (VALUE ,email)
		   (SIZE "60")))
	   (BR)
	   (B  "Summary of the problem:") (BR)
	   (INPUT ((TYPE "text")
		   (NAME "subject")
		   (VALUE "")
		   (SIZE "70")))
	   (BR)
	   (TABLE 
	    (TR 
	     (TD  (B  "Severity:"))
	     (TD  (SELECT ((NAME "severity"))
			  ,@severity-option-links))
	     (TD 'nbsp 'nbsp) 
	     (TD  (B  "Class:"))
	     (TD  
	      (SELECT ((NAME "class"))
		      ,@bug-class-option-links))
	     (TD 'nbsp 'nbsp)
	     (TD  (B  "Priority:"))
	     (TD 
	      (SELECT ((NAME "priority"))
		      ,@priority-option-links))))
	   (P)
	   (B  "Description of the problem:") (BR)
	   (TEXTAREA ((NAME "description")
		      (ROWS "6")
		      (COLS "60")
		      (STYLE "font-family:monospace"))
		     "")
	   (P)
	   (B    
	    "Please give a short sequence of steps "
	    "to reproduce the problem:") (BR)
	    (TEXTAREA ((NAME "how-to-repeat")
		       (ROWS "6")
		       (COLS "60")
		       (STYLE "font-family:monospace"))
		      "")
	    (BR)
	    (INPUT ((TYPE "submit")
		    (VALUE "Send")))
	    'nbsp 'nbsp
	    (INPUT ((TYPE "reset")
		    (VALUE "Reset")))))))
