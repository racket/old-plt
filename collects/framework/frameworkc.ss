;;
;; $Id: frameworkc.ss,v 1.18 2000/01/25 22:23:02 robby Exp $
;;

(compound-unit/sig (import [core:string : mzlib:string^]
			   [core:function : mzlib:function^]
			   [core:pretty-print : mzlib:pretty-print^]
			   [core:file : mzlib:file^]
			   [core:thread : mzlib:thread^]
			   [mred : mred^]
			   [keys : framework:keys^]
			   [test : framework:test^])
  (link [application : framework:application^ ((require-relative-library "app.ss"))]
	[version : framework:version^ ((require-relative-library "version.ss")
                                       core:string core:function)]
        [color-model : framework:color-model^ ((require-relative-library "color-model.ss") 
                                               core:function)]
	[exn : framework:exn^ ((require-relative-library "exn.ss"))]
	[exit : framework:exit^ ((require-relative-library "exit.ss") mred preferences gui-utils)]
	[preferences : framework:preferences^
		     ((require-relative-library "prefs.ss")
		      mred
		      exn exit panel core:pretty-print core:function)]
	[autosave : framework:autosave^
		  ((require-relative-library "autosave.ss") mred exit preferences)]
	[handler : framework:handler^
		 ((require-relative-library "handler.ss")
		  mred
		  gui-utils finder group  text preferences frame
		  core:file)] 
	[keymap : framework:keymap^
		((require-relative-library "keymap.ss")
		 mred keys preferences finder handler scheme-paren frame)]
	[match-cache : framework:match-cache^ ((require-relative-library "mcache.ss"))]
	[paren : framework:paren^ ((require-relative-library "paren.ss"))]
	[scheme-paren : framework:scheme-paren^
		      ((require-relative-library "sparen.ss") paren)]
	[path-utils : framework:path-utils^ ((require-relative-library "fileutil.ss"))]
	[icon : framework:icon^ ((require-relative-library "icon.ss") mred)]

	[editor : framework:editor^ ((require-relative-library "editor.ss")
				     mred
				     autosave finder path-utils keymap icon
				     preferences text pasteboard frame
				     core:file)]
	[pasteboard : framework:pasteboard^ ((require-relative-library "pasteboard.ss")
					     mred editor)]
	[text : framework:text^ ((require-relative-library "text.ss")
				 mred icon editor preferences keymap
				 gui-utils color-model frame
				 core:function)]

	[gui-utils : framework:gui-utils^ ((require-relative-library "guiutils.ss") mred)]

	[finder : framework:finder^
		((require-relative-library "finder.ss")
		 mred
		 preferences gui-utils keymap
		 core:string core:function core:file)]

	[group : framework:group^ 
	       ((require-relative-library "group.ss")
		mred
		application frame preferences
		core:function core:file)]

	[canvas : framework:canvas^ ((require-relative-library "canvas.ss")
				     mred preferences frame)]

	[panel : framework:panel^ ((require-relative-library "panel.ss")
				   mred core:function)]

	[frame : framework:frame^ 
	       ((require-relative-library "frame.ss")
		mred
		group preferences icon handler application panel
		gui-utils exit finder keymap text pasteboard editor canvas
		core:function
		core:file)]
	[scheme : framework:scheme^ 
		((require-relative-library "scheme.ss")
		 mred preferences match-cache paren
		 scheme-paren icon keymap text frame
		 core:thread
		 core:function)]
	[main : framework:main^ ((require-relative-library "main.ss")
				 mred
				 preferences exit group)])
  (export
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
   (unit gui-utils)
   (unit finder)
   (unit group)
   (unit canvas)
   (unit panel)
   (unit frame)
   (unit scheme)
   (unit main)))