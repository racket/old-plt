;; loadu.ss - loads graphic files into a unit
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------
; ported to MrEd 100 by Paul Steckler 

(require-relative-library "sigs.ss")

(define mrspidey-gui@
  (unit/sig mrspidey-gui^
    (import mred^
	    framework^
	    browser^ 
	    mrspidey:mzlib:function^
	    mzlib:pretty-print^
	    mzlib:file^
	    mzlib:string^
	    mrspidey:sba^
	    mrspidey:interaction^
	    (zodiac : zodiac:system^))
    
    (include "deltas.ss")
    (include "statedit.ss")
    (include "dyn-edit.ss")
    (include "graphics.ss")
    (include "arrow.ss")
    (include "annotat.ss")
    ;(include "paramenu.ss")
    ;(include "option.ss")
    (include "prefs.ss")
    (include "Tframe.ss")
    (include "main.ss")))
