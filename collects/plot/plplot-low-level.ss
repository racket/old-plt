(module plplot-low-level mzscheme
  (require 
   (lib "cffi.ss" "compiler"))
  
  (c-declare "#include \"plplot.h\"")

  
  ; makes an array of double from a scheme list
  (c-declare 
   "double * list_to_array(Scheme_Object * list)
       {
        
        double * ar = (double *)scheme_malloc(scheme_list_length(list) * sizeof(double));
        int i = 0;
        while(!SCHEME_NULLP(list))
        {
         Scheme_Object * car = SCHEME_CAR(list);
         double tmp;
                
         tmp = scheme_real_to_double(car);
         ar[i] = tmp;
         list = SCHEME_CDR(list);
         i++;
         }
        
        return ar;
        }")
  
  ; makes an array of ints from a list
  (c-declare
  
  "int * int_list_to_array(Scheme_Object * list)
       {
        
        int * ar = (int *)scheme_malloc(scheme_list_length(list) * sizeof(int));
        int i = 0;
        while(!SCHEME_NULLP(list))
        {
         Scheme_Object * car = SCHEME_CAR(list);
         double tmp;
                
         tmp = (int)scheme_real_to_double(car);
         ar[i] = tmp;
         list = SCHEME_CDR(list);
         i++;
         }
        
        return ar;
        }
  
  ")
  
  ; makes an array of arrays of doubles from a scheme list
  (c-declare 
   "double ** list_of_list_to_array(Scheme_Object * list)
   {
    double ** grid = (double *) scheme_malloc(scheme_list_length(list) * sizeof(double *));
           int i = 0 ;
           while(!SCHEME_NULLP(list))
           {
            Scheme_Object * car = SCHEME_CAR(list);
                          grid[i] = list_to_array(car);
                          i++;
                          list = SCHEME_CDR(list);
                          }
           return grid;
           }")
  
  ; setup the page
  
  (define pl-setup-page
    (c-lambda (int int) void
              "plspage(0.,0.,___arg1,___arg2,0,0);
  "))
              
  ; set output device
  (define pl-set-device
    (c-lambda (char-string) void "plsdev"))
  
  ; set the output file
  (define pl-set-output-file
    (c-lambda (char-string) void "plsfnam"))
  
  ; sets the memory output device
  ; number number scheme-object -> void 
  ; sets up the bitmap to be a homogenous 8 bit vector
  ;(define pl-setup-memory
  ;  (c-lambda (int int scheme-object) int "
  ;  homo_u8_vector * vec = (homo_u8_vector *) ___arg3;
  ;  plsmem(___arg1,___arg2,(char *) vec->els);    
  ;  "))
  
  ; convers a u8vector to a scheme string
  ;(define u8vec->scheme-string
  ;  (c-lambda (scheme-object) scheme-object 
              "
  ;homo_u8_vector * vec = (homo_u8_vector *) ___arg1;
 ; ___result = scheme_make_sized_string((char *) vec->els, vec->length, 0); ;"

  
  
  ; initialize the plot
  (define pl-init-plot
    (c-lambda () void "plinit();

  "))
  
  ; finish the plot
  (define pl-finish-plot
    (c-lambda () void "plend"))
  
  ; set the 2d plot environment
  ; args are : xmin xmax ymin ymax scaling axes
  (define pl-set-plot-environment
    (c-lambda (double double double double int int) void "plenv"))
  
  ; set plot labels
  (define pl-set-labels
    (c-lambda (nonnull-char-string  nonnull-char-string  nonnull-char-string) void "pllab"))
  
  ; plot a line
  ; args: num_points, x_array, y_array
  (define pl-plot-line
    (c-lambda ( int scheme-object scheme-object) void 
              "plline(___arg1,list_to_array(___arg2),list_to_array(___arg3));
              "))
  
  ; plot a line-segment
  ; args : x1, y1  x2,y2
  (define pl-plot-segment
    (c-lambda (double double double double) void "pljoin"))
  
  ; set the background colour
  (define pl-set-background-color
    (c-lambda (int int int) void "plscolbg"))
  
  ; select a color index 
  (define pl-select-colormap0-index
    (c-lambda (int) void "plcol0"))
  
  ; set a color at an index
  (define pl-set-colormap0-index
    (c-lambda (int int int int) void "plscol0"))
  
  ; set the line width
  (define pl-set-line-width 
    (c-lambda (int) void "plwid"))
  
  ; pl-write-text : number number number number number string
  ; write text on a line parellel to x,y dx,dy (args 1 -  4)
  ; justification is fraction where the string is to be placed
  ; 0 is left, .5 is center, etc.
  (define pl-write-text
    (c-lambda (double double double double double nonnull-char-string) void "plptex"))
  
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
  
  ; 2d-shade-plot  z-vals, x-vals, y-vals, levels
  ; z-vals : listof-listof number
  ; x,y,levels : listofnumber
  (define pl-2d-shade-plot 
    (c-lambda (scheme-object scheme-object scheme-object  scheme-object) void 
              "PLcGrid mapping;
               mapping.xg = list_to_array(___arg2);
               mapping.nx = scheme_list_length(___arg2);
               mapping.yg = list_to_array(___arg3);
               mapping.ny = scheme_list_length(___arg3);
               
               plshades(list_of_list_to_array(___arg1),mapping.nx,mapping.ny, NULL,
                                             0,0,0,0,                      
                                             list_to_array(___arg4), scheme_list_length(___arg4),
                                             1,1,0,
                                             plfill, 1 , pltr1, (void *) &mapping);
               "))
  
  ; pl-plot-points : x-vals y-vals code
  (define pl-plot-points 
    (c-lambda (int scheme-object scheme-object int) void
              "plpoin(___arg1,list_to_array(___arg2),list_to_array(___arg3),___arg4);
              "))
                
  ; pl-x-error-bars
  (define pl-x-error-bars
    (c-lambda (int scheme-object scheme-object scheme-object) void
              "plerrx(___arg1,list_to_array(___arg2),list_to_array(___arg3),list_to_array(___arg4));
              "))
  
  ; pl-y-error-bars
  (define pl-y-error-bars
    (c-lambda (int scheme-object scheme-object scheme-object) void
              "plerry(___arg1,list_to_array(___arg2),list_to_array(___arg3),list_to_array(___arg4));
              "))
  
  
  ;; 3D functions

  ; pl-world-3d
  ; (basex , basey , height , xmin , xmax , ymin , ymax , zmin , zmax , alt , az )
  (define pl-world-3d 
    (c-lambda (double double double double double double double double double double double) void "plw3d"))                      
              
  ; pl-plot3d
  ; x, y - (listof float)
  ; z - (listof (listof float))
  (define pl-plot3d
    (c-lambda (scheme-object scheme-object scheme-object) void
              "plot3d(list_to_array(___arg1),list_to_array(___arg2),list_of_list_to_array(___arg3),
                                   scheme_list_length(___arg1),scheme_list_length(___arg2),DRAW_LINEXY,0);
              "))
  

  ; no coloring
  (define pl-mesh3d
    (c-lambda (scheme-object scheme-object scheme-object) void
              "plmesh(list_to_array(___arg1),list_to_array(___arg2),list_of_list_to_array(___arg3),
                                   scheme_list_length(___arg1),scheme_list_length(___arg2),DRAW_LINEXY);
  "))
  
  ;mesh + colors
  ; x y z x Contours? x Lines? x Colored? Sides? levels
  (define pl-mesh3dc
    (c-lambda (scheme-object scheme-object scheme-object bool bool bool bool scheme-object) void
              "
  
    
    PLFLT i[2], h[2], l[2], s[2];

  i[0] = 0.0;		/* left boundary */
  i[1] = 1.0;		/* right boundary */

  h[0] = 240; /* blue -> green -> yellow -> */
  h[1] = 0;   /* -> red */

  l[0] = 0.6;
  l[1] = 0.6;

  s[0] = 0.8;
  s[1] = 0.8;

  plscmap1n(256);
  plscmap1l(0, 2, i, h, l, s, NULL);
  {
  int opts = (___arg4 ? DRAW_LINEXY : 0) |
                         (___arg5 ? MAG_COLOR : 0) |
                         (___arg6 ? BASE_CONT : 0) |
                         (___arg7 ? DRAW_SIDES : 0) ;
              plmeshc(list_to_array(___arg1),list_to_array(___arg2),list_of_list_to_array(___arg3),
                                    scheme_list_length(___arg1),scheme_list_length(___arg2),opts,
                                    list_to_array(___arg8),scheme_list_length(___arg8));
              }
  "))
                         
                         
  ; set up a 3d box with labels + ticks
  ; args are (repeated each time for x y and z)
  ; "options", title , major tick spacing, number of minor ticks
  ; 
  (define pl-box3
    (c-lambda (char-string char-string double int char-string char-string double int char-string char-string double int ) void "plbox3"))
  
  ; pl-poly3 
  ; draw a polygon in 3 space
  ; args are n x y z, draw (array of draw/not draw) , ifcc (directionality for hidden line removal) 1 or 0
  (define pl-poly3
    (c-lambda (int scheme-object scheme-object scheme-object scheme-object int) void
              "plpoly3(___arg1, 
                       list_to_array(___arg2),
                       list_to_array(___arg3),
                       list_to_array(___arg4),
                       int_list_to_array(___arg5),
                       ___arg6);
  "))
  
  ; pl-line3 
  ; draws a line in 3 space
  ; arsg are n, x y z (lists of number)
  (define pl-line3
    (c-lambda (int scheme-object scheme-object scheme-object) void
              "plline3(___arg1, 
                       list_to_array(___arg2),
                       list_to_array(___arg3),
                       list_to_array(___arg4));
  "))
  
  
  ; pl-fil
  ; fills a polygon
  ; args n , x list ylist
  (define pl-fill
    (c-lambda (int scheme-object scheme-object) void
              "plfill(___arg1,
               list_to_array(___arg2),
               list_to_array(___arg3));
  "))
             
  
  
  

                           
              
  
  ;; functions to add 
  ; area-fill -> plfill (number, list of number, list of number) 
  ; volume fill? - plfill3 (n x y z)
  ; line-style -> plstyl or pllsty
  ; area-fill-pattern -> plpat, plppat
  
  
  ; for dealing with colors
  ; plgcol0, (gets rgb value)
  ; plgcolbg (gets bg rgb value)
  
  ; more usefull
  ; plw3d
  ; plbox3
  ; plot3d(c)
  ; mesh, contours.. etc,
 
  ; contour lableformat - format numerical contour labels..
  ; setcontlabelparam - contour lable params
  ; plggriddata (make grid data from irregularilyu spaced..)
  
  ; plpat - (nline, incline, spacing)
  ; plpoin ( plot character at a point)
  ; plpoin3 (n x y z, code)
  ; plploy3 (... )
  ; plsurf3d
  
  
  ; plscmap0 -> set entire cmap0 aray
  ; plscmap0n -> set number of colors in map0
  ; plscmap1 (.... ) 
  ; plscmap1l (... set colormap using ' piece-wise linear relationship')
  
  ; pllstyl - set line style 1 - 8
  ; plmesh (x y z nx  ny , opt) -> make a mesh
  ; plmeshc - magniturued color plot surface mesh with contour - has options, linex , liny linexy, MAG-COLORl base_cont, curtain
  
  ; plmtex - text relative to viewport..
  ; xtick, ytick, nxsub, nysub
  
  ; light-source??
  ; pllighsource ( x y z)
  ; plline3 (n x y z) <- draw line in 3 space)
  
  ; colormap 0 and 1 stuff
  ; plcol0, plcol1

  
  (provide
   (all-defined))
  )









