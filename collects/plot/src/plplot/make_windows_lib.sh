 cl /c /I../../gd /DPLD_mem /DPLD_png gd.c mem.c pdfutils.c pl*.c 
link -lib /out:plplot.lib *.obj libgd.lib