(module framework-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred"))

  (require "framework-sig.ss"
	   "private/sig.ss"

           "private/application.ss"
	   "private/version.ss"
	   "private/color-model.ss"
	   "private/exn.ss"
	   "private/exit.ss"
	   "private/menu.ss"
	   "private/preferences.ss"
	   "private/autosave.ss"
	   "private/handler.ss" 
	   "private/keymap.ss"
	   "private/match-cache.ss"
	   "private/paren.ss"
	   "private/scheme-paren.ss"
	   "private/path-utils.ss"
	   "private/icon.ss"
	   "private/editor.ss"
	   "private/pasteboard.ss"
	   "private/text.ss"
	   
	   "private/finder.ss"
	   "private/group.ss"
	   "private/canvas.ss"
	   "private/panel.ss"
	   "private/frame.ss"
	   "private/scheme.ss"
	   "private/main.ss")

  (provide framework@)

  (define framework@
    (compound-unit/sig
      (import [mred : mred^])
      (link [application : framework:application^ (application@)]
	    [version : framework:version^ (version@)]
	    [color-model : framework:color-model^ (color-model@ )]
	    [exn : framework:exn^ (exn@)]
	    [exit : framework:exit^ (exit@ mred preferences)]
	    [menu : framework:menu^ (menu@ mred preferences)]
	    [preferences : framework:preferences^
			 (preferences@ mred exn exit panel)]
	    [autosave : framework:autosave^ (autosave@ mred exit preferences)]
	    [match-cache : framework:match-cache^ (match-cache@)]
	    [paren : framework:paren^ (paren@)]
	    [scheme-paren : framework:scheme-paren^ (scheme-paren@ paren)]
	    [path-utils : framework:path-utils^ (path-utils@)]
	    [icon : framework:icon^ (icon@ mred)]

	    [keymap : framework:keymap^
		    (keymap@ mred preferences finder handler scheme-paren frame editor)]
	    [editor : framework:editor^
		    (editor@ mred autosave finder path-utils keymap icon
			     preferences text pasteboard frame handler)]
	    [pasteboard : framework:pasteboard^ (pasteboard@ mred editor)]
	    [text : framework:text^
		  (text@ mred icon editor preferences keymap color-model frame scheme)]
	    [finder : framework:finder^ (finder@ mred preferences keymap)]
	    [group : framework:group^ 
                   (group@ mred application frame preferences text canvas menu)]
	    [canvas : framework:canvas^ (canvas@ mred preferences frame)]
	    [panel : framework:panel^ (panel@ icon mred)]
	    [frame : framework:frame^ 
		   (frame@ mred group preferences icon handler application panel
			   exit finder keymap text pasteboard editor canvas menu)]
	    [handler : framework:handler^
		     (handler@ mred finder group text preferences frame)]

	    [scheme : framework:scheme^ 
		    (scheme@ mred preferences match-cache paren
			     scheme-paren icon keymap text editor frame)]
	    [main : framework:main^ (main@ mred preferences exit group)])
      (export
       (unit menu)
       (unit application)
       (unit version)
       (unit color-model)
       (unit exn)
       (unit exit)
       (unit preferences)
       (unit autosave)
       (unit handler) 
       (unit keymap)
       (unit match-cache)
       (unit paren)
       (unit scheme-paren)
       (unit path-utils)
       (unit icon)
       (unit editor)
       (unit pasteboard)
       (unit text)
       (unit finder)
       (unit group)
       (unit canvas)
       (unit panel)
       (unit frame)
       (unit scheme)
       (unit main)))))
