  (unit/sig ()
    (import mred^
            mzlib:core^
            mzlib:print-convert^
            drscheme:export^
            zodiac:system^
            plt:parameters^)

    (define filename (build-path plt-home-directory 
				 "mrspidey"
				 "drspidey.ss"))
    
    (define name "Static Debugger")

    (include "../backward.ss"))