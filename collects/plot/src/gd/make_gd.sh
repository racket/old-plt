cl /c /I../libpng -I../jpeg-6b /I../zlib  /DHAVE_LIBPNG /DHAVE_LIBJPEG *.c

link -lib  /nologo /out:libgd.lib gd.obj gd_gd.obj gd_gd2.obj gd_io.obj gd_io_dp.obj                 gd_io_file.obj gd_ss.obj gd_io_ss.obj gd_png.obj gd_jpeg.obj gdxpm.obj                 gdfontt.obj gdfonts.obj gdfontmb.obj gdfontl.obj gdfontg.obj                 gdtables.obj  gdcache.obj gdkanji.obj wbmp.obj                 gdhelpers.obj gd_topal.obj ../libpng/libpng.lib
