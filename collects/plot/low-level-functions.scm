

; the scheme interface to the low-level functions (exculuding book-keeping functions)

; IMPLEMENTED FUNCTIONS
;
; pl-init-plot
; void -> void
; initializes plplot internal state
;
; pl-finish-plot
; void -> void
; finishes plot, finalizes open device streams
;
; pl-set-plot-environment
; number number number number number number -> void
; sets up a 2d viewport given x-min x-max y-min y-max, a 'scaling' and 'axes-type' parameters
;
; pl-set-labels
; string string string -> void
; set the x label, y label and title of the plot
;
; pl-plot-line
; int alon alon -> void
; plots a line, given the number of x-values, list of x-values and y-values
;
; pl-plot-segement
; number number number number -> void
; plots a single line segement 

; pl-set-background-color
; int int int -> void
; sets the background color to an rgb value. Must be called before pl-init

; pl-select-colormap0-index
; int -> void
; selects a color from colormap0

; pl-set-colormap0-index
; int int int int -> void
; set a color at an index

; pl-set-line-width
; number -> void
; sets the line width to the width specified.

; pl-write-text
; number number number number number string
; write text on a line parellel to x,y dx,dy (args 1 -  4)
  ; justification is fraction where the string is to be placed
  ; 0 is left, .5 is center, etc.

; pl-2d-contour-plot
; listoflistof-number listof-number listof-number listof-number
; makes a 2d contour plot given the z-grid, list of x-values, list of y values, and desired levels

; pl-2d-shade-plot
; listoflistof-number listof-number listof-number listof-number
; makes a 2d shade plot given the z-grid, list of x-values, list of y values, and desired levels

;; UNIMPLEMENTED

; pl-area-fill 
; number  listof-number listof-number
; fills an area described by the given points with the current fill style

; plpat
; create a fill pattern

; plppat
; select a predefined pattern

 
; plot-x-error -> plerrx( n,xmin,xmax,y)
; plot-y-error -> plerry( n,ymin,ymax,n)

; line style functions

; plstyl
; set the line style using arrays to specify pen lifts and drop intervals

; pllsty
; set the line style to a predefined style - undocumented

; plfill3 (n x y z)
; fills a polygon in 3 space

; plgcol0, (gets rgb value)
; returns the rgb value for a specific color in colormap 0

; plgcolbg (gets bg rgb value)
; returns the rgb value for the background color - same as plgcol0 on 0

; plw3d
; set up a window for 3d plotting
; arguments are :
; basex, basey -> flat size of box
; height - z coordinate size
; xmin xmax ymin ymax zmin zmax - latergest/smallest user entereable values
; alt az , the altitute and aziumth of viewing

; plbox3
; draw a 3d viewing box
; for each coordinate takes :
; opt (how to draw axie) ,label, ticks, number of ticks

; plline3 (n x y z) 
; draw a line in 3 space
; x y z are arrays of doubles
; n is length of those ararys

; plpoin ( x y char
; draw a point in 2 space wit
; plpoin3 (n x y z, code)

; plmesh(c) (x y z nx  ny , opt) 
; draws a mesh in 3d
; has options, linex , liny linexy, MAG-COLORl base_cont, curtain
; plmeshc - magniturued color plot surface mesh with contour - 

; plsurf3d (x y z nx ny opt clevel nlevel)
; plot shaded 3d surface plot - similiar to mesh - difference in hidden line removeal
; options are faceted / base-contour / surface-contour / draw-sides

; plpat - (nline, inc, del)
; sets area fill pattern
; consists of 2 sets of parellel lines at given agles

; pl_setcontlabelformat (exponent, digits)
; sets the labelformat 
; if |exponenet| > 10, exponential is used
; digits is significant digits

; pl_setcontlabelparam(offset, size, spacing, active)
; sets the contour lablel parameters.
; offset is disntance from contour line
; size if font size
; active is 0 (off) by default

; plggriddata ...
; makes uniform grid data from irregularily spaced data
; uses one of several approximation aglorithms

; plpoly3 (n x y z draw ifcc)
; draws a polygon in 3space
; different from line 3d in that the polygon is drawn only 
; if it is viable
; x y z are arrays of points (n being number of points)
; draw is an array of booleans specifiying whether the particular segment is drawable
; ifcc is directionality of polygon (??) 1 is counter, 0 is clockwize

; plscmap0  (r g b ncol0)
; set entire cmap0 aray
; r g b are arrays of ints, ncol is lenght of those arrays


; plscmap0n (ncol0)
; set number of colors in map0
; ncol0 is the number of colors to support - everything above 16 is by default red


; plscmap1 (r g b ncol1)
; set entire cmap1 array
; same as plscmap0

; plscmap1n (ncol1)
; sets number of colors in color map 1

; plscmap1l (itype npts pos coord coord2 coord3 rev)
; "set colormap using ' piece-wise linear relationship'"
; more info in documentation

; plmtex (side disp pos just text)
; write text relative to viewport
; lots of options - more info in docs

; xtick, ytick (nticks)
; specifies distance between ticks

; nxsub, nysub (num-intervals)
; specifies number of sub-intervals between ticks

; pllighsource ( x y z)
; sets the 3d position of the light source for use with plsurf3d

;;;;;;;;;;;;;;
; colormap 0 and 1 stuff
; plcol0, plcol1
;
; basic line plots, scatter plots, errorbars
; titles, labels, gridlines, vectors, contours, shades
; 3d mesh, surface, .. 
; xtics, yticks, zticks
; parametetric.. 2