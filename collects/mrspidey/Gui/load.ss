;; load.ss - loads gui files
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

(require-library "frameworks.ss" "framework")

(load-relative "deltas.ss")
;(printf "Loaded deltas.ss~n")
(load-relative "statedit.ss")
;(printf "Loaded statedit.ss~n")
(load-relative "dyn-edit.ss")
;(printf "Loaded dyn-edit.ss~n")
(load-relative "graphics.ss")
;(printf "Loaded graphics.ss~n")
(load-relative "arrow.ss")
;(printf "Loaded arrow.ss~n")
(load-relative "annotat.ss")
;(printf "Loaded annotat.ss~n")
;(load-relative "paramenu.ss")
;(printf "Loaded paramenu.ss~n")
;(load-relative "option.ss")
;(printf "Loaded option.ss~n")
(load-relative "prefs.ss")
;(printf "Loaded prefs.ss~n")
(load-relative "Tframe.ss")
;(printf "Loaded Tframe.ss~n")
(load-relative "main.ss")
;(printf "Loaded main.ss~n")
