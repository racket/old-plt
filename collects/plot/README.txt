---------------
SCHEME PLOT LIB
---------------       

-------
Purpose
-------       

The purpose of this library is to provide users with the ability to 
generate plots using DrScheme. Users can create plots from both 
liniar (functional) data sources and discrete ones (originating from
observations, etc) using predifined renderers. If not satisfied with the 
built-in renderers users can define their own. This can be done
in two ways: either by combining the higher-level primitives provided,
or by importing more of the functionality provided by the plplot C library,
upon whitch this library is based. 

Installation
------------

Compile libgd with jpeg support.

Compile plplot to use the jpeg driver (using lib gd), make sure dynamic drivers 
are disabled.

mzc ++ldf "/path/to/plplot/lib.so" ++ccf "-I/path/to/plplot/lib/headers" plplot-low-level.ss

copy plplot-low-level.[so|dll] into  plt/plplot/compiled/native/your-archtecture/ 


The actuall compilation of plplot/libgd/libjpeg can be tricky, and will be further 
documented at a later point. 

Also, libgd/jpeg will be deprecated and replacted with a plplot driver that plots
directly to a scheme dc% (drawing-context)

For a quick demo load up and execute "run-tests.ss"


Structural Layout
-----------------

The ____ plot library consists of several layers, in an attempt to distance the end
user from the low level plplot C library. 


plplot-low-level.ss :
    At the lowest level, the file "plplot-low-level.ss" provides the scheme-to-c interface 
  to the plplot lib. The functions and declarations in this file provide the ability to call
  the underlying c-functions, sometimes doing some translation in order to pass scheme datatypes
  such as lists to functions that take arrays and pointers.
  A simple example of a translation function would be 

   ; set the line width
  (define pl-set-line-width 
    (c-lambda (int) void "plwid"))

  , relying on the foreign-function-interface for type translation, while a (significantly) 
  more complicated example 
  
  ; 2d-contour-plot  z-vals, x-vals, y-vals, levels
  ; z-vals : listof-listof number
  ; x,y,levels : listofnumber
  (define pl-2d-contour-plot 
    (c-lambda (scheme-object scheme-object scheme-object  scheme-object) void 
              "PLcGrid mapping;
               mapping.xg = list_to_array(___arg2);
               mapping.nx = scheme_list_length(___arg2);
               mapping.yg = list_to_array(___arg3);
               mapping.ny = scheme_list_length(___arg3);
               
               plcont(list_of_list_to_array(___arg1),mapping.nx,mapping.ny,
                      1, mapping.nx, 1,mapping.ny,
                      list_to_array(___arg4), scheme_list_length(___arg4),
                      pltr1, (void *) &mapping);
               "))
  
  does a lots of scheme->c conversion.

plot.scm
   This file contains the middle and top tier of the plotting library. The middle tier consists of
   the plot-view% , 2d-view% and 3d-view% classes, that provide (somewhat) safe access to the low-level
   library. The top tier consists of the data renderers, which interpret the data given to them and
   render it using the provided views.

   Renderers:

   A renderer takes in a source of data and creates a function that interprets that data and 
   produces a visual output when applied to a view.

   Renderers are currently higher order functions that use closures+macros to keep information about 
   their given parameters. A macro, called "r-lambda" was created to implement a keyworld-argument-like
   lambda that processes a list of arguments given in an assosiative array. This was done to allow renderers
   to have reasonable defaults (such as pen color, width, etc), allow the user to change only the 
   parameters they want, and at the same time remain syntactically simple. The downside is that the "instantiate"
   functionality of objects is being copied, as well as the fact that the 'renderers' really are objects, 
   and there are cases where subclassing would be usefull.
 
   
   The following renderers are currently defined:

   For 2d-view


   points - draws a set of points on the graph given (listof posn)
     options;
     char - the number of the character to plot on each point *** - explain more later    

   line - draws a line given a function in the form of (number->number)
     options: 
     samples - how often to sample the function 
     color - the color of the line produced
     width - the width of the pen

   field - draws a vector field given a vector valued function in the form of (posn -> posn) 
     options:
     samples - how many vectors to draw in the x and y direction
     color - the color of the vectors
     width - the width of the vector lines
     style - the style in whitch to draw the vectors ( real (real size), scaled (best for viewing), normalized (shows direction only) )

  contour - draws a contour plot on a 2d surfaces from functions in the form of (number number -> number)
     options:
     samples - number of times to sample the function in each direction
     color - the color of the contours
     width - the width of the contour lines
     levels - either the number of levels to draw, or a list of the specific levels to draw
  
  shade - draws a shade plot on a 2d surface from 3d functions like in contour
     options:
     samples - same as above
     levels - only number of levels allows

  For 3d-view:

  surface - draws a surface plot of a function in the form of (number number -> number)
     options: 
     samples - number of times to sample the function
     color - color of the lines
     width - width of the lines.
  

   The "view" classes:   


   plot-view% extends image-snip
      Base class for a plot-view
      Provides fascilities for starting and finishing plots. The underlying c-library
      follows and implicit sequence contract and this class will (at some point) enforce
      that contract.

      Also provides access to settings affecting all types of plots

      methods:
        start-plot: starts the plot - calls plplot to open a file stream for writing

        finish-plot: finishes the plot - calss plplot to close the file, clean up. also loads the created image

        set-line-color: sets the line colour

        set-line-width: sets the line width

        get: getter for a few of the fields
        set: setter for a few of the fields

        set-plot-environment: sets the 2d plot environment (x and y range)

        reset-to-default: resets the state of the c-code to default, called after every renderer

   2d-vew% extends plot-view
      Provides an interface to drawing 2d plots. Some methods call low level functions, others are 
      written in scheme.

      Methods:
  
        set-labels: Sets x, y and title labels
        
        plot-vector: plots a single vector

        plot-vectors: plots a list of vectors

        plot-points: plots points using a specified charater

        plot-line: plots a line given a set of coordinates

        plot-contours: plots a grid representing a 3d function using contours to distinguish levels

        plot-shades: plots a grid representing a 3d function using shades to represent height (z)


   3d-vew% extends plot-view
      Provides an interface to drawing 3d plots. Just started

      Methods:
  
        box-3d: Sets up the 3d drawing box settings, including ticks and lables
        
        world3d: Sets ups the 3d world - min/max coordinates / view angles 

        plot3d: draws a 3d surface plot given a grid
     

Interface/Interactions in the current system
--------------------------------------------

after loading the plot.scm library, the basic syntax is as follows:

(plot (line (lambda (x) x)))

This would create a basic plot with the line y = x going through it

multiple items cam be plotted as follows:

(plot (line (lambda (x) x))
      (line (lambda (x) (sin x)))

plot can also be given options

(plot '((x-min 0) (x-max 10))
      (line (lambda (x) ...
      ... )

The plot sequence is as follows:

plot creates a 2d-view
plot initializes the 2d-view, saves it's argument list and the list of renderes in the view
 plot calls the first renderer with the view as an argument 
 the renderer calls some methods on the view
 plot calls reset-to-defaults to clear the state
 plot repeats the indented block for each renderer
plot calls finalizes the view, and returns it

the view is an image-snip, and can be viewd by being evaluated in the drscheme interactions window

the views can be superimposed (primitively for now) 

(define a (plot ... )
(define b (plot ... )

(mix b a)







      




