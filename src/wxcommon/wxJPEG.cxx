/*
 * This file has both JPEG and PNG support (despite the file name).
 *
 * The JPEG part Derived from IJG's example.c.
 */

#if defined(_MSC_VER) && defined(MZ_PRECISE_GC)
# include "wx.h"
#endif
#ifdef MZ_PRECISE_GC
# include "common.h"
#endif

#include "wx_dcmem.h"
#include "wx_gdi.h"

#include <stdio.h>
#include <stdlib.h>
extern "C" {
# ifdef WX_USE_LIBJPEG
#  include <jpeglib.h>
# else
#  include "jpeg/jpeglib.h"
# endif
# ifdef WX_USE_LIBPNG
#  include <png.h>
# else
#  include "libpng/png.h"
# endif
}
#include <setjmp.h>

#ifdef MPW_CPLUS
extern "C" { typedef void (*JPEG_ERROR_F_PTR)(j_common_ptr info); }
# define CAST_JPEGP (JPEG_ERROR_F_PTR)
#else
# define CAST_JPEGP /* empty */
#endif

#ifdef wx_x
# define WX_QUANTIZE 1
# define Q_NOT !
#else
# define WX_QUANTIZE 0
# define Q_NOT /* empty */
#endif

static wxColor *the_color;
extern void wxmeError(const char *e);
extern int wxGetPreference(const char *name, char *res, long len);

static void draw_scanline(JSAMPROW row, int cols, int rownum, int step, JSAMPARRAY colormap, wxMemoryDC *dc,
			  int mono)
{
  int colnum;

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new wxColour(0, 0, 0);
  }

  for (colnum = 0; colnum < cols; colnum++) {
#if WX_QUANTIZE
    if (!mono) {
      int v;
      v = row[colnum];
      the_color->Set(colormap[0][v], colormap[1][v], colormap[2][v]);
    } else {
#endif
      if (step == 1) {
	the_color->Set(row[colnum], row[colnum], row[colnum]);
      } else {
	the_color->Set(row[colnum * step], 
		       row[colnum * step + 1], 
		       row[colnum * step + 2]);
      }
#if WX_QUANTIZE
    }
#endif
    dc->SetPixel(colnum, rownum, the_color);
  }
}

static void get_scanline(JSAMPROW row, int cols, int rownum, wxMemoryDC *dc)
{
  int colnum, d = 0, r, g, b;

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new wxColour(0, 0, 0);
  }

  for (colnum = 0; colnum < cols; colnum++, d += 3) {
    dc->GetPixel(colnum, rownum, the_color);
    r = the_color->Red();
    g = the_color->Green();
    b = the_color->Blue();
    row[d] = r;
    row[d+1] = g;
    row[d+2] = b;
  }
}

wxMemoryDC *create_dc(int width, int height, wxBitmap *bm, int mono)
{
  wxMemoryDC *dc;

  dc = new wxMemoryDC();
  if (width >= 0)
    bm->Create(width, height, mono ? 1 : -1);
  dc->SelectObject(bm);

  if (!dc->Ok()) {
    dc->SelectObject(NULL);
    return NULL;
  }

  return dc;
}

wxMemoryDC *create_reader_dc(wxBitmap *bm, volatile int *desel)
{
  wxMemoryDC *dc;

  dc = new wxMemoryDC(1); /* 1 => read-only */
  dc->SelectObject(bm);
  if (!dc->GetObject()) {
# ifdef wx_msw
    if (bm->selectedInto) {
      /* Even selecting into a read-only dc doesn't seem to work
	 if it already has a dc. Just use that one, then. */
      dc = (wxMemoryDC *)bm->selectedInto;
      *desel = 0;
    } else
# endif
      return NULL;
  }

  return dc;
}

/**********************************************************************/

char jpeg_err_buffer[JMSG_LENGTH_MAX + 256];

struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */
  jmp_buf setjmp_buffer;	/* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;

/*
 * Here's the routine that will replace the standard error_exit method:
 */

static void my_error_exit(j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  (*cinfo->err->format_message)(cinfo, jpeg_err_buffer);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}

/*
 * Sample routine for JPEG decompression.  We assume that the source file name
 * is passed in.  We want to return 1 on success, 0 on error.
 */

int read_JPEG_file(char * filename, wxBitmap *bm)
{
  FILE * volatile infile;       /* source file */
  JSAMPARRAY buffer;		/* Output row buffer */
  int row_stride;		/* physical row width in output buffer */
  wxMemoryDC *dc;
#ifdef MZ_PRECISE_GC
  START_XFORM_SKIP;
#endif
  /* This struct contains the JPEG decompression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   */
  struct jpeg_decompress_struct cinfo;
  /* We use our private extension JPEG error handler.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct my_error_mgr jerr;
#ifdef MZ_PRECISE_GC
  END_XFORM_SKIP;
#endif

  /* In this example we want to open the input file before doing anything else,
   * so that the setjmp() error recovery below can assume the file is open.
   * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
   * requires it in order to read binary files.
   */

  if ((infile = fopen(filename, "rb")) == NULL) {
    sprintf(jpeg_err_buffer, "can't open %.255s\n", filename);
    wxmeError(jpeg_err_buffer);
    return 0;
  }

  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = CAST_JPEGP my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_decompress(&cinfo);
    fclose(infile);
    wxmeError(jpeg_err_buffer);
    return 0;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  /* Step 2: specify data destnation (eg, a file) */

  jpeg_stdio_src(&cinfo, infile);

  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header(&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */

  /* Step 4: set parameters for decompression */
#if WX_QUANTIZE
  cinfo.quantize_colors = 1;
#endif

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 

  dc = create_dc(cinfo.output_width, cinfo.output_height, bm, 0);
  if (!dc) {
    /* couldn't allocate DC or select bitmap */
    return 0;
  }

  /* JSAMPLEs per row in output buffer */
  row_stride = cinfo.output_width * cinfo.output_components;
  /* Make a one-row-high sample array that will go away when done with image */
  buffer = (*cinfo.mem->alloc_sarray)
		((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

  /* Step 6: while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */

  /* Here we use the library's state variable cinfo.output_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   */
  while (cinfo.output_scanline < cinfo.output_height) {
    /* jpeg_read_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could ask for
     * more than one scanline at a time if that's more convenient.
     */
    (void) jpeg_read_scanlines(&cinfo, buffer, 1);
    /* Assume put_scanline_someplace wants a pointer and sample count. */
    draw_scanline(buffer[0],
		  cinfo.output_width, cinfo.output_scanline - 1, 
		  cinfo.output_components, cinfo.colormap,
		  dc, cinfo.num_components == 1);
  }

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);

  /* After finish_decompress, we can close the input file.
   * Here we postpone it until after no more JPEG errors are possible,
   * so as to simplify the setjmp error logic above.  (Actually, I don't
   * think that jpeg_destroy can do an error exit, but why assume anything...)
   */
  fclose(infile);

  /* At this point you may want to check to see whether any corrupt-data
   * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   */

  /* And we're done! */
  dc->SelectObject(NULL);
  return 1;
}

int write_JPEG_file(char *filename, wxBitmap *bm, int quality)
{
  /* More stuff */
  FILE * volatile outfile;		/* target file */
  JSAMPROW row_pointer;	/* pointer to JSAMPLE row[s] */
  wxMemoryDC * volatile dc;
  int wid;
  volatile int desel = 1;

#ifdef MZ_PRECISE_GC
  START_XFORM_SKIP;
#endif
  /* This struct contains the JPEG compression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   * It is possible to have several such structures, representing multiple
   * compression/decompression processes, in existence at once.  We refer
   * to any one struct (and its associated working data) as a "JPEG object".
   */
  struct jpeg_compress_struct cinfo;
  /* This struct represents a JPEG error handler.  It is declared separately
   * because applications often want to supply a specialized error handler
   * (see the second half of this file for an example).  But here we just
   * take the easy way out and use the standard error handler, which will
   * print a message on stderr and call exit() if compression fails.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct my_error_mgr jerr;
#ifdef MZ_PRECISE_GC
  END_XFORM_SKIP;
#endif

  dc = create_reader_dc(bm, (int *)&desel);

  wid = bm->GetWidth();
  row_pointer = new JSAMPLE[3 * wid];

  if ((outfile = fopen(filename, "wb")) == NULL) {
    free(row_pointer);
    if (desel)
      dc->SelectObject(NULL);
    sprintf(jpeg_err_buffer, "can't open %.255s\n", filename);
    wxmeError(jpeg_err_buffer);
    return 0;
  }
  /* Step 1: allocate and initialize JPEG compression object */

  /* We have to set up the error handler first, in case the initialization
   * step fails.  (Unlikely, but it could happen if you are out of memory.)
   * This routine fills in the contents of struct jerr, and returns jerr's
   * address which we place into the link field in cinfo.
   */
  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = CAST_JPEGP my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    free(row_pointer);
    if (desel)
      dc->SelectObject(NULL);
    jpeg_destroy_compress(&cinfo);
    fclose(outfile);
    wxmeError(jpeg_err_buffer);
    return 0;
  }

  /* Now we can initialize the JPEG compression object. */
  jpeg_create_compress(&cinfo);

  /* Step 2: specify data destination (eg, a file) */
  /* Note: steps 2 and 3 can be done in either order. */

  /* Here we use the library-supplied code to send compressed data to a
   * stdio stream.  You can also write your own code to do something else. */
  jpeg_stdio_dest(&cinfo, outfile);

  /* Step 3: set parameters for compression */

  /* First we supply a description of the input image.
   * Four fields of the cinfo struct must be filled in:
   */
  cinfo.image_width = wid; 	/* image width and height, in pixels */
  cinfo.image_height = bm->GetHeight();
  cinfo.input_components = 3;		/* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB; 	/* colorspace of input image */
  /* Now use the library's routine to set default compression parameters.
   * (You must set at least cinfo.in_color_space before calling this,
   * since the defaults depend on the source color space.)
   */
  jpeg_set_defaults(&cinfo);
  /* Now you can set any non-default parameters you wish to.
   * Here we just illustrate the use of quality (quantization table) scaling:
   */
  jpeg_set_quality(&cinfo, quality, TRUE /* limit to baseline-JPEG values */);

  /* Step 4: Start compressor */

  /* TRUE ensures that we will write a complete interchange-JPEG file.
   * Pass TRUE unless you are very sure of what you're doing.
   */
  jpeg_start_compress(&cinfo, TRUE);

  /* Step 5: while (scan lines remain to be written) */
  /*           jpeg_write_scanlines(...); */

  /* Here we use the library's state variable cinfo.next_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   * To keep things simple, we pass one scanline per call; you can pass
   * more if you wish, though.
   */

  while (cinfo.next_scanline < cinfo.image_height) {
    /* jpeg_write_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could pass
     * more than one scanline at a time if that's more convenient.
     */
    get_scanline(row_pointer, wid, cinfo.next_scanline, dc);
    (void)jpeg_write_scanlines(&cinfo, &row_pointer, 1);
  }

  /* Step 6: Finish compression */

  jpeg_finish_compress(&cinfo);
  /* After finish_compress, we can close the output file. */
  fclose(outfile);

  /* Step 7: release JPEG compression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_compress(&cinfo);

  free(row_pointer);

  if (desel)
    dc->SelectObject(NULL);
  
  /* And we're done! */
  return 1;
}

/*
 * SOME FINE POINTS:
 *
 * In the above code, we ignored the return value of jpeg_read_scanlines,
 * which is the number of scanlines actually read.  We could get away with
 * this because we asked for only one line at a time and we weren't using
 * a suspending data source.  See libjpeg.doc for more info.
 *
 * We cheated a bit by calling alloc_sarray() after jpeg_start_decompress();
 * we should have done it beforehand to ensure that the space would be
 * counted against the JPEG max_memory setting.  In some systems the above
 * code would risk an out-of-memory error.  However, in general we don't
 * know the output image dimensions before jpeg_start_decompress(), unless we
 * call jpeg_calc_output_dimensions().  See libjpeg.doc for more about this.
 *
 * Scanlines are returned in the same order as they appear in the JPEG file,
 * which is standardly top-to-bottom.  If you must emit data bottom-to-top,
 * you can use one of the virtual arrays provided by the JPEG memory manager
 * to invert the data.  See wrbmp.c for an example.
 *
 * As with compression, some operating modes may require temporary files.
 * On some systems you may need to set up a signal handler to ensure that
 * temporary files are deleted if the program is interrupted.  See libjpeg.doc.
 */

/**********************************************************************/

static char *png_err_msg;
static int pem_registered;

#ifdef MPW_CPLUS
extern "C" {
  typedef void (*UEP_PTR)(png_structp png_ptr, png_const_charp msg);
  typedef void (*UWP_PTR)(png_structp png_ptr, png_const_charp msg);
}
# define CAST_UEP (UEP_PTR)
# define CAST_UWP (UWP_PTR)
#else
# define CAST_UEP /* empty */
# define CAST_UWP /* empty */
#endif

static void user_error_proc(png_structp png_ptr, png_const_charp msg)
{
  int len;

  if (!pem_registered) {
    wxREGGLOB(png_err_msg);
  }
  len = strlen(msg);
  png_err_msg = new WXGC_ATOMIC char[len + 1];
  memcpy(png_err_msg, msg, len + 1);
  
  longjmp(png_ptr->jmpbuf, 1);
}

static void user_warn_proc(png_structp info, png_const_charp msg)
{
}

static void png_draw_line(png_bytep row, int cols, int rownum, wxMemoryDC *dc, wxMemoryDC *mdc, int step)
{
  int colnum, delta;

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new wxColour(0, 0, 0);
  }

  for (colnum = 0, delta = 0; colnum < cols; colnum++, delta += step) {
    the_color->Set(row[delta], 
		   row[delta + 1], 
		   row[delta + 2]);
    dc->SetPixel(colnum, rownum, the_color);
    if (mdc) {
      the_color->Set(row[delta + 3],
		     row[delta + 3],
		     row[delta + 3]);
      mdc->SetPixel(colnum, rownum, the_color);
    }
  }
}

static wxColour *the_white, *the_black;

static void png_draw_line1(png_bytep row, int cols, int rownum, wxMemoryDC *dc)
{
  int colnum, delta = 0, bit;

  if (!the_white) {
    wxREGGLOB(the_white);
    wxREGGLOB(the_black);
    the_white = new wxColour(255, 255, 255);
    the_black = new wxColour(0, 0, 0);
  }

  for (colnum = 0; colnum < cols; delta++) {
    for (bit = 128; (colnum < cols) && bit; colnum++, bit = bit >> 1) {
      if (row[delta] & bit)
	dc->SetPixel(colnum, rownum, the_white);
      else
	dc->SetPixel(colnum, rownum, the_black);
    }
  }
}

static void png_get_line(png_bytep row, int cols, int rownum, wxMemoryDC *dc, wxMemoryDC *mdc)
{
  int colnum, delta, r, g, b;
  int step = (mdc ? 4 : 3);

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new wxColour(0, 0, 0);
  }

  for (colnum = 0, delta = 0; colnum < cols; colnum++, delta += step) {
    dc->GetPixel(colnum, rownum, the_color);
    r = the_color->Red();
    g = the_color->Green();
    b = the_color->Blue();
    row[delta] = r;
    row[delta+1] = g;
    row[delta+2] = b;
    if (mdc) {
      mdc->GetPixel(colnum, rownum, the_color);
      r = the_color->Red();
      row[delta+3] = r;
    }
  }
}

static void png_get_line1(png_bytep row, int cols, int rownum, wxMemoryDC *dc)
{
  int colnum, delta, bit, r, g, b, bits;

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new wxColour(0, 0, 0);
  }

  for (colnum = 0, delta = 0; colnum < cols; delta++) {
    bits = 0;
    for (bit = 128; (colnum < cols) && bit; colnum++, bit = bit >> 1) {
      dc->GetPixel(colnum, rownum, the_color);
      r = the_color->Red();
      g = the_color->Green();
      b = the_color->Blue();
      if ((r == 255) && (g == 255) && (b == 255)) 
	bits |= bit;
    }
    row[delta] = bits;
  }
}

/**********************************************************************/

int wx_read_png(char *file_name, wxBitmap *bm, int w_mask, wxColour *bg)
{
   png_structp png_ptr;
   png_structp volatile png_ptr_orig;
   png_infop info_ptr;
   png_infop volatile info_ptr_orig;
   png_uint_32 width, height;
   int bit_depth, color_type, interlace_type, is_mono = 0, row_width;
   unsigned int number_passes, pass, y;
   FILE * volatile fp;
   png_bytep *rows, row;
   wxMemoryDC * volatile dc = NULL;
   wxMemoryDC *mdc = NULL;
   wxBitmap *mbm = NULL;

   if ((fp = fopen(file_name, "rb")) == NULL)
     return 0;

   /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also supply the
    * the compiler header file version, so that we know if the application
    * was compiled with a compatible version of the library.  REQUIRED
    */
   png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, 
				    CAST_UEP user_error_proc, 
				    CAST_UWP user_warn_proc);

   if (png_ptr == NULL)
   {
      fclose(fp);
      return 0;
   }

   /* Allocate/initialize the memory for image information.  REQUIRED. */
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL)
   {
      fclose(fp);
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      return 0;
   }

   /* Set error handling if you are using the setjmp/longjmp method (this is
    * the normal method of doing things with libpng).  REQUIRED unless you
    * set up your own error handlers in the png_create_read_struct() earlier.
    */

   png_ptr_orig = png_ptr;
   info_ptr_orig = info_ptr;

   if (setjmp(png_ptr->jmpbuf))
   {
     /* Free all of the memory associated with the png_ptr and info_ptr */
     png_ptr = png_ptr_orig;
     info_ptr = info_ptr_orig;
     png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
     fclose(fp);
     if (dc)
       dc->SelectObject(NULL);
     /* If we get here, we had a problem reading the file */
     return 0;
   }

   /* Set up the input control if you are using standard C streams */
   png_init_io(png_ptr, fp);

   /* The call to png_read_info() gives us all of the information from the
    * PNG file before the first IDAT (image data chunk).  REQUIRED
    */
   png_read_info(png_ptr, info_ptr);

   png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
		&interlace_type, NULL, NULL);

   if (w_mask) {
     /* Is the mask actually useful? */
     if (!png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)
	 && !(color_type & PNG_COLOR_MASK_ALPHA))
       w_mask = 0;
   }

   if ((bit_depth == 1)
       && (color_type == PNG_COLOR_TYPE_GRAY)
       && !png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) {
     /* Special handling for monochrome so that we don't use 32x
	the necessary memory. */
     is_mono = 1;
   } else {
     /* Normalize formal of returned rows: */
     if (color_type == PNG_COLOR_TYPE_PALETTE)
       png_set_palette_to_rgb(png_ptr);
     if (color_type == PNG_COLOR_TYPE_GRAY ||
	 color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
       png_set_gray_to_rgb(png_ptr);
     if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
       png_set_tRNS_to_alpha(png_ptr);
     if (bit_depth == 16)
       png_set_strip_16(png_ptr);

     /* Expand grayscale images to the full 8 bits from 1, 2, or 4 bits/pixel */
     png_set_gray_1_2_4_to_8(png_ptr);
   }

   /* Set the background color to draw transparent and alpha images over.
    * It is possible to set the red, green, and blue components directly
    * for paletted images instead of supplying a palette index.  Note that
    * even if the PNG file supplies a background, you are not required to
    * use it - you should use the (solid) application background if it has one.
    */
   if (!w_mask && !is_mono) {
     png_color_16 *image_background;

     if (!bg && png_get_bKGD(png_ptr, info_ptr, &image_background))
       png_set_background(png_ptr, image_background,
			  PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
     else {
       png_color_16 my_background;
       
       if (bg) {
	 int g;
	 my_background.red = bg->Red();
	 my_background.green = bg->Green();
	 my_background.blue = bg->Blue();
	 g = (((int)my_background.red) 
	      + ((int)my_background.green)
	      + ((int)my_background.blue)) / 3;
	 my_background.gray = g;
       } else {
	 my_background.red = 0xff;
	 my_background.green = 0xff;
	 my_background.blue = 0xff;
	 my_background.gray = 0xff;
       }

       if (bit_depth == 16) {
	 my_background.red = (my_background.red << 8) | my_background.red;
	 my_background.green = (my_background.green << 8) | my_background.green;
	 my_background.blue = (my_background.blue << 8) | my_background.blue;
	 my_background.gray = (my_background.gray << 8) | my_background.gray;
       }

       png_set_background(png_ptr, &my_background,
			  PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
     }
   }

   /* Gamma correction --- only if the file has a gamma.
      This gamma correction messes with the ability of
      PNGs to keep exact RGB information, for the many
      cases where that could make sense. So no gamma
      data in the file means we won't try to correct it. */
   {
     double gamma;

     if (png_get_gAMA(png_ptr, info_ptr, &gamma)) {
       double screen_gamma;
       char *gamma_str;
       char buf[30];
       
       if (wxGetPreference("gamma", buf, 30)) {
	 screen_gamma = (double)atof(buf);
       } else if ((gamma_str = getenv("SCREEN_GAMMA"))) {
	 screen_gamma = (double)atof(gamma_str);
       } else
	 screen_gamma = 0;
       
       if (!(screen_gamma > 0.0) || !(screen_gamma < 10.0)) {
	 /* Guess */
#ifdef wx_mac
	 screen_gamma = 1.7;  /* A good guess for Mac systems */
#else
	 screen_gamma = 2.0; /* A good guess for a PC monitor */
#endif
       }

       png_set_gamma(png_ptr, screen_gamma, gamma);
     }
   }

   if (w_mask && !is_mono) {
     /* Add filler (or alpha) byte (before/after each RGB triplet) */
     /* Actually, invert so that it's a mask. */
     png_set_filler(png_ptr, 0, PNG_FILLER_AFTER);
     png_set_invert_alpha(png_ptr);
   }

   /* Turn on interlace handling.  REQUIRED if you are not using
    * png_read_image().  To see how to handle interlacing passes,
    * see the png_read_row() method below:
    */
   number_passes = png_set_interlace_handling(png_ptr);

   /* Optional call to gamma correct and add the background to the palette
    * and update info structure.  REQUIRED if you are expecting libpng to
    * update the palette for you (ie you selected such a transform above).
    */
   png_read_update_info(png_ptr, info_ptr);

   /* Allocate the memory to hold the image using the fields of info_ptr. */

#ifdef MZ_PRECISE_GC
   rows = (png_bytep *)GC_malloc(sizeof(png_bytep) * height);
#else
   rows = new png_bytep[height];
#endif

   row_width = png_get_rowbytes(png_ptr, info_ptr);
   for (y = 0; y < height; y++) {
     row = new WXGC_ATOMIC png_byte[row_width];
     rows[y] = row;
   }

   dc = create_dc(width, height, bm, is_mono);
   if (!dc) {
     if (dc)
       dc->SelectObject(NULL);
     png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
     fclose(fp);
     return 0;
   }

   for (pass = 0; pass < number_passes; pass++) {
     png_read_rows(png_ptr, rows, NULL, height);
   }

   if (is_mono) {
     for (y = 0; y < height; y++) {
       png_draw_line1(rows[y], width, y, dc);
     }
   } else {
     if (w_mask) {
       int mono_mask;
       unsigned int x;

       /* Will a monochrome mask do? */
       for (y = 0; y < height; y++) {
	 row = rows[y];
	 for (x = 0; x < width; x++) {
	   int val;
	   val = row[(x * 4) + 3];
	   if ((val != 0) && (val != 255))
	     break;
	 }
	 if (x < width)
	   break;
       } 

       mono_mask = ((y < height) ? 0 : 1);

       mbm = new wxBitmap(width, height, mono_mask);
       if (mbm->Ok())
	 mdc = create_dc(-1, -1, mbm, mono_mask);
       else
	 mdc = NULL;
     }

     for (y = 0; y < height; y++) {
       png_draw_line(rows[y], width, y, dc, mdc, w_mask ? 4 : 3);
     }
   }

   /* read rest of file, and get additional chunks in info_ptr - REQUIRED */
   png_read_end(png_ptr, info_ptr);

   /* At this point you have read the entire image */

   /* clean up after the read, and free any memory allocated - REQUIRED */
   png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

   /* close the file */
   fclose(fp);

   dc->SelectObject(NULL);
   if (mdc) {
     mdc->SelectObject(NULL);
     bm->SetMask(mbm);
   }

   /* that's it */
   return 1;
}

int wx_write_png(char *file_name, wxBitmap *bm)
{
   png_structp png_ptr;
   png_structp volatile png_ptr_orig;
   png_infop info_ptr;
   png_infop volatile info_ptr_orig;
   int width, height;
   int bit_depth, color_type, row_width;
   int y;
   FILE *volatile fp;
   png_bytep *rows, row;
   wxMemoryDC * volatile dc = NULL;
   wxMemoryDC * volatile mdc = NULL;
   wxBitmap *mbm = NULL;
   volatile int desel = 1;
   volatile int mdesel = 1;

   if ((fp = fopen(file_name, "wb")) == NULL)
     return 0;

   /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also supply the
    * the compiler header file version, so that we know if the application
    * was compiled with a compatible version of the library.  REQUIRED
    */
   png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, 
				     CAST_UEP user_error_proc, 
				     CAST_UWP user_warn_proc);

   if (png_ptr == NULL)
   {
      fclose(fp);
      return 0;
   }

   /* Allocate/initialize the memory for image information.  REQUIRED. */
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL)
   {
      fclose(fp);
      png_destroy_write_struct(&png_ptr, NULL);
      return 0;
   }

   /* Set error handling if you are using the setjmp/longjmp method (this is
    * the normal method of doing things with libpng).  REQUIRED unless you
    * set up your own error handlers in the png_create_read_struct() earlier.
    */

   png_ptr_orig = png_ptr;
   info_ptr_orig = info_ptr;
   if (setjmp(png_ptr->jmpbuf)) {
     /* Free all of the memory associated with the png_ptr and info_ptr */
     png_ptr = png_ptr_orig;
     info_ptr = info_ptr_orig;
     png_destroy_write_struct(&png_ptr, &info_ptr);
     fclose(fp);
     if (dc && desel)
       dc->SelectObject(NULL);
     if (mdc && mdesel)
       mdc->SelectObject(NULL);
     /* If we get here, we had a problem reading the file */
     return 0;
   }

   /* Set up the input control if you are using standard C streams */
   png_init_io(png_ptr, fp);

   width = bm->GetWidth();
   height = bm->GetHeight();
   bit_depth = 8;
   
   mbm = bm->GetMask();
   if (mbm && mbm->Ok() && (mbm->GetWidth() == width) &&  (mbm->GetHeight() == height))
     color_type = PNG_COLOR_TYPE_RGB_ALPHA;
   else {
     color_type = PNG_COLOR_TYPE_RGB;
     mbm = NULL;
   }

   if ((bm->GetDepth() == 1) && !mbm) {
     bit_depth = 1;
     color_type = PNG_COLOR_TYPE_GRAY;
   }

   /* Set the image information here.  Width and height are up to 2^31,
    * bit_depth is one of 1, 2, 4, 8, or 16, but valid values also depend on
    * the color_type selected. color_type is one of PNG_COLOR_TYPE_GRAY,
    * PNG_COLOR_TYPE_GRAY_ALPHA, PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB,
    * or PNG_COLOR_TYPE_RGB_ALPHA.  interlace is either PNG_INTERLACE_NONE or
    * PNG_INTERLACE_ADAM7, and the compression_type and filter_type MUST
    * currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE. REQUIRED
    */
   png_set_IHDR(png_ptr, info_ptr, width, height, bit_depth, color_type,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, 
		PNG_FILTER_TYPE_DEFAULT);

   if (mbm)
     png_set_invert_alpha(png_ptr);

   /* Write the file header information.  REQUIRED */
   png_write_info(png_ptr, info_ptr);

   /* Allocate the memory to hold the image using the fields of info_ptr. */
#ifdef MZ_PRECISE_GC
   rows = (png_bytep *)GC_malloc(sizeof(png_bytep) * height);
#else
   rows = new png_bytep[height];
#endif
   row_width = png_get_rowbytes(png_ptr, info_ptr);
   for (y = 0; y < height; y++) {
     row = new WXGC_ATOMIC png_byte[row_width];
     rows[y] = row;
   }

   dc = create_reader_dc(bm, &desel);
   if (mbm)
     mdc = create_reader_dc(mbm, &mdesel);
   else
     mdc = NULL;

   if (bit_depth == 1) {
     for (y = 0; y < height; y++) {
       png_get_line1(rows[y], width, y, dc);
     }
   } else {
     for (y = 0; y < height; y++) {
       png_get_line(rows[y], width, y, dc, mdc);
     }
   }

   png_write_image(png_ptr, rows);

   png_write_end(png_ptr, info_ptr);

   /* clean up after the write, and free any memory allocated */
   png_destroy_write_struct(&png_ptr, &info_ptr);

   /* close the file */
   fclose(fp);

   if (desel)
     dc->SelectObject(NULL);
   if (mdc && mdesel) {
     mdc->SelectObject(NULL);
   }

   /* that's it */
   return 1;
}

/**********************************************************************/

static unsigned char alternate_icon[] = {
83,67,70,84,67,70,85,67,70,85,66,70,85,66,70,85,66,70,85,68,71,
86,69,73,87,68,72,87,67,71,87,66,71,87,66,71,87,66,71,86,66,71,
86,67,71,86,68,71,87,68,72,88,69,73,87,70,74,86,70,73,86,70,73,
86,70,73,85,69,71,86,69,72,90,71,76,90,70,76,89,67,73,89,68,71,
88,68,71,86,68,71,87,71,74,88,71,74,89,70,74,90,69,74,91,68,74,
90,68,73,89,70,74,88,69,73,90,71,75,91,72,76,90,71,75,89,70,74,
88,72,75,88,72,75,88,72,75,87,71,74,87,71,74,88,72,75,87,71,74,
87,71,74,88,72,75,86,70,73,87,71,74,89,73,76,88,72,75,87,71,74,
87,71,74,88,71,74,89,71,74,90,70,74,90,71,75,90,71,75,88,71,74,
89,73,76,83,67,70,84,67,70,85,67,70,85,66,70,85,66,70,86,67,71,
85,69,72,86,69,73,87,68,72,87,67,71,87,66,71,87,66,71,87,66,71,
86,66,70,86,66,70,85,66,71,86,67,71,87,67,72,86,68,72,87,68,72,
88,69,73,87,69,73,85,69,71,86,70,73,87,68,75,86,66,74,86,63,71,
84,63,69,81,62,67,79,61,66,79,62,68,80,63,68,82,63,68,84,64,70,
87,65,71,88,67,72,88,69,73,89,71,75,89,71,75,90,71,75,91,72,76,
89,71,74,88,71,74,88,73,76,88,72,75,87,71,74,86,70,73,88,72,75,
88,72,75,88,72,75,89,73,76,87,71,74,86,70,73,88,72,75,88,72,75,
87,71,74,87,71,74,88,71,74,89,71,74,90,70,74,90,71,75,90,71,75,
89,72,75,88,72,75,83,67,70,84,67,70,85,67,70,86,66,70,86,67,71,
87,68,72,86,69,72,86,69,72,87,68,72,87,67,71,87,66,71,87,66,71,
87,66,71,87,65,70,87,65,71,87,65,71,88,66,72,88,66,73,87,67,72,
87,68,72,88,68,72,85,66,71,83,65,70,81,63,69,76,57,65,73,52,63,
72,50,60,70,48,57,66,46,55,63,44,53,61,44,51,62,44,52,66,47,54,
71,50,58,77,54,61,82,60,65,86,66,71,89,71,75,88,69,73,89,69,73,
89,70,74,88,69,73,87,71,74,88,73,76,89,73,76,86,70,73,85,69,72,
88,72,75,89,73,76,89,73,77,89,73,76,87,71,74,87,71,74,88,72,75,
88,72,75,87,71,74,87,71,74,88,72,75,90,72,76,91,71,75,90,70,75,
89,71,75,89,72,75,87,71,74,85,69,72,84,68,71,85,67,70,85,66,70,
86,68,71,88,70,73,86,69,72,85,68,71,86,68,71,86,67,71,87,66,71,
88,67,72,88,66,71,88,66,71,88,67,72,89,67,72,90,68,73,89,67,72,
88,67,72,89,68,73,86,65,71,78,57,64,71,53,60,67,50,58,61,41,51,
58,38,50,59,36,49,56,33,46,53,32,45,51,31,44,50,32,43,51,33,44,
54,35,45,57,37,47,63,42,50,72,51,57,80,60,66,85,66,70,85,67,70,
86,68,71,88,69,72,89,70,73,89,71,75,89,72,76,89,73,76,88,72,74,
87,71,74,89,73,76,90,74,76,88,72,75,86,70,73,84,68,71,88,72,75,
88,72,75,88,72,75,88,72,75,87,71,74,88,71,75,90,71,75,90,72,76,
89,71,75,88,71,74,88,71,75,88,72,75,83,67,70,84,68,71,84,68,71,
83,67,70,84,68,71,86,70,73,85,69,72,85,68,71,84,68,71,85,69,72,
87,68,72,88,67,72,87,67,72,88,67,71,88,68,71,88,69,71,89,69,71,
87,67,69,85,65,70,84,63,70,78,57,66,69,47,58,62,41,52,57,37,48,
53,32,44,51,30,42,51,29,44,51,28,44,50,27,43,50,28,43,50,28,43,
50,29,44,50,29,44,51,29,44,56,34,47,64,42,52,71,50,58,77,56,63,
80,62,65,84,66,69,87,69,71,88,70,72,89,69,75,90,70,76,89,71,75,
88,72,73,89,73,74,90,74,75,89,73,74,87,71,72,84,68,71,83,67,70,
86,70,73,87,71,74,88,72,75,88,72,75,89,73,76,87,71,74,88,71,75,
89,73,76,89,73,76,89,72,75,89,72,75,89,73,76,82,66,69,83,67,70,
84,68,71,84,68,71,84,68,71,84,68,71,84,68,71,84,68,71,84,68,71,
85,69,72,87,69,73,87,68,72,88,68,72,88,69,73,87,68,71,87,68,71,
87,68,71,84,65,70,79,59,66,73,53,61,65,45,55,60,40,51,59,34,48,
56,30,45,55,28,44,53,27,43,50,26,42,50,28,44,52,29,45,51,28,44,
52,27,43,53,26,43,53,27,43,53,27,43,53,30,44,55,33,45,58,36,46,
63,42,50,70,50,56,76,56,62,79,59,65,79,60,65,80,60,68,82,62,69,
83,65,70,85,69,72,88,72,74,89,73,74,88,72,75,89,73,76,88,72,75,
89,73,76,89,73,76,89,73,76,90,74,77,88,72,75,91,75,78,90,74,77,
88,72,75,90,74,77,89,73,76,89,73,76,90,74,77,89,73,76,82,66,69,
82,66,69,83,67,70,83,67,70,85,69,72,84,68,71,83,67,70,85,69,72,
84,68,71,85,70,73,87,71,74,85,68,71,87,70,73,87,70,73,85,68,72,
84,66,72,81,64,69,74,58,63,67,50,57,61,43,53,54,35,47,51,32,45,
52,29,45,53,29,46,53,30,46,52,29,46,51,28,44,51,28,44,52,29,45,
51,28,44,51,28,44,51,27,43,52,28,44,52,28,44,52,29,45,52,30,43,
52,30,42,55,33,43,59,37,47,61,39,48,61,39,49,65,43,53,64,42,53,
67,47,56,73,55,62,78,62,67,82,66,70,85,67,73,87,69,75,88,71,76,
88,72,75,90,74,77,91,75,78,87,71,74,88,72,75,85,69,72,88,72,75,
90,74,77,88,72,75,91,75,78,89,73,76,89,73,76,92,76,79,91,75,78,
84,68,71,82,66,69,81,65,68,83,67,70,85,69,72,84,68,71,83,67,70,
86,70,73,83,67,70,86,70,73,87,72,75,84,69,72,87,71,75,86,69,74,
78,61,68,73,56,63,69,52,59,59,42,49,51,33,43,48,30,42,46,28,41,
45,27,42,45,26,43,48,28,46,50,29,46,50,29,45,52,30,45,51,29,43,
50,27,43,50,28,44,50,29,45,50,30,46,50,30,45,50,30,46,51,29,45,
52,29,44,51,29,42,52,29,43,54,31,44,54,30,44,52,29,43,55,31,45,
54,32,44,57,35,47,61,41,50,63,44,52,66,49,56,72,55,62,80,63,70,
84,67,74,85,69,74,86,69,74,87,71,75,86,70,73,88,72,75,87,71,74,
91,75,78,91,75,77,88,72,75,91,75,78,90,74,77,89,73,76,92,76,79,
91,75,78,84,68,71,82,66,69,81,65,68,84,68,71,86,70,73,84,68,71,
84,68,71,84,68,71,84,68,71,85,69,72,85,70,74,85,70,75,87,69,77,
77,58,68,60,42,53,52,35,45,51,34,44,48,30,41,43,25,37,45,27,40,
46,28,43,45,26,43,45,25,44,48,28,45,51,28,44,51,27,42,51,30,43,
51,30,43,50,29,45,51,29,46,51,28,46,51,28,46,51,28,45,51,28,46,
51,28,45,52,29,45,52,29,45,52,29,45,53,30,46,51,28,44,50,27,43,
51,28,44,52,30,43,52,30,42,53,31,44,55,33,45,56,36,47,59,40,51,
63,44,55,67,47,58,68,49,59,70,51,60,73,55,62,78,60,65,80,63,66,
82,66,69,88,69,73,90,69,73,88,71,74,88,72,75,89,73,76,89,73,76,
88,72,75,88,72,75,83,67,70,83,67,70,83,67,70,85,69,72,86,70,73,
85,69,72,84,68,72,80,63,69,85,68,73,83,66,71,84,68,74,82,66,72,
73,55,65,58,38,50,48,29,41,48,29,41,50,29,43,48,29,41,46,27,39,
46,28,42,46,28,44,43,25,42,45,24,42,50,27,43,54,30,45,55,31,45,
53,29,44,52,29,46,52,28,47,53,30,49,50,27,44,51,28,43,51,28,43,
51,28,43,50,27,42,48,25,41,50,27,43,53,30,46,52,29,45,50,27,43,
50,27,43,50,27,43,51,28,42,51,28,42,52,30,43,52,30,43,52,30,44,
51,31,44,51,31,43,52,31,44,53,33,46,56,35,48,58,37,48,61,40,50,
63,45,52,69,50,56,76,55,62,83,62,69,85,67,71,85,69,72,88,72,75,
89,73,76,89,73,76,88,72,75,84,68,71,84,68,71,83,67,70,84,68,71,
85,69,72,85,68,72,85,68,74,85,68,76,85,68,76,84,67,75,76,60,68,
61,45,54,51,33,44,48,28,41,48,27,42,49,27,43,49,28,44,49,28,43,
48,29,42,47,29,42,45,28,43,44,26,42,46,24,41,50,27,42,56,32,46,
58,31,45,58,34,44,58,34,45,54,30,43,54,30,44,51,28,42,53,31,44,
54,32,45,51,29,42,50,27,43,50,26,43,49,26,42,50,27,43,51,28,44,
52,29,45,51,28,44,50,27,43,50,27,44,52,29,45,52,29,45,51,28,44,
50,27,44,51,28,44,50,27,44,51,27,44,51,27,45,52,28,46,51,28,43,
51,28,42,51,30,42,56,35,46,60,37,49,68,44,56,72,54,59,75,60,62,
82,66,69,86,70,73,90,74,77,89,73,76,86,70,73,86,70,73,84,68,71,
83,67,70,84,68,71,84,68,71,82,66,71,83,66,73,81,64,72,72,55,64,
53,37,47,46,29,40,47,28,41,49,28,43,52,29,46,52,29,46,52,29,46,
52,29,47,50,32,45,49,31,45,48,30,44,49,30,46,49,28,44,55,31,45,
71,45,56,75,46,56,83,55,60,84,57,62,79,51,57,77,49,56,77,50,59,
77,51,61,67,42,53,55,31,43,53,30,43,57,33,48,55,31,48,53,30,46,
52,29,44,52,29,44,51,29,44,50,28,44,51,29,46,50,28,46,49,26,44,
50,27,44,51,27,45,51,26,45,51,26,44,50,26,44,50,25,45,52,27,47,
51,26,45,50,26,43,50,27,42,53,29,43,55,30,45,59,32,47,59,39,48,
63,47,52,72,56,61,79,62,66,84,67,71,87,71,74,86,70,73,84,68,71,
84,68,71,82,66,69,83,67,70,84,68,71,84,67,72,81,64,70,67,50,58,
50,33,42,44,27,40,50,31,45,53,32,47,53,30,46,55,32,48,59,36,52,
58,35,51,53,30,46,54,36,49,52,35,47,52,32,44,54,33,44,53,32,44,
62,38,49,85,55,61,102,66,67,106,68,71,105,69,73,105,69,73,103,67,71,
107,68,72,105,68,73,80,48,57,62,34,47,76,51,58,79,53,65,62,36,53,
59,37,51,61,37,48,57,33,45,52,28,42,49,27,42,50,30,47,51,30,47,
53,30,46,51,29,43,52,28,46,51,28,46,50,29,44,49,30,43,52,29,46,
52,27,46,52,27,45,51,26,46,52,26,44,53,26,43,54,28,44,55,29,45,
54,33,46,56,36,47,62,42,52,70,51,59,77,59,63,87,69,72,83,67,70,
82,66,69,81,65,68,81,65,68,82,66,69,83,67,70,82,65,71,70,54,61,
52,36,45,43,25,36,46,28,42,53,34,49,57,36,51,56,34,50,56,33,49,
57,34,50,56,33,49,54,31,47,59,39,53,58,39,51,55,34,44,57,35,44,
64,41,51,70,44,53,83,50,53,116,78,76,126,81,79,124,77,75,125,78,76,
127,80,78,130,83,84,125,82,86,91,56,63,67,39,48,98,68,72,95,66,74,
63,37,50,72,45,57,88,56,63,90,57,64,84,53,61,78,48,57,74,47,54,
68,43,50,64,38,47,58,33,42,52,30,43,50,30,45,51,32,47,49,31,47,
52,29,46,53,28,46,52,27,45,52,27,45,52,28,44,51,27,43,52,28,44,
53,29,45,52,30,45,51,30,43,54,33,45,60,40,50,68,48,55,78,59,64,
84,68,71,83,67,70,83,67,70,82,66,69,81,65,68,82,66,69,73,56,64,
52,35,45,47,29,40,46,28,41,52,34,48,58,39,53,57,36,51,53,30,46,
53,30,46,55,32,48,56,33,49,57,34,50,59,38,51,66,45,56,61,39,48,
63,40,48,74,48,56,76,47,53,83,47,47,124,83,79,138,90,86,136,85,82,
140,89,85,142,91,88,141,88,88,138,90,93,103,66,71,73,45,50,107,70,72,
95,64,68,69,43,51,87,52,60,107,64,67,112,70,71,116,76,78,116,76,80,
119,76,76,113,71,70,102,64,67,92,57,62,74,46,51,62,37,43,57,34,43,
55,33,46,54,30,45,52,28,44,51,27,43,52,28,43,51,28,44,50,28,44,
50,28,44,50,28,44,50,27,44,51,27,44,52,30,44,54,32,44,57,35,45,
66,44,53,83,67,70,84,68,70,84,68,70,83,67,70,85,69,72,79,63,67,
60,43,52,46,29,40,46,28,41,47,29,42,53,35,49,57,38,53,55,35,49,
53,30,46,52,29,45,54,31,46,54,31,47,54,31,48,58,35,49,65,42,53,
65,41,51,71,45,53,77,49,55,81,49,52,93,55,54,131,87,82,144,94,91,
143,92,90,147,96,94,149,98,96,149,94,92,141,91,91,120,80,82,85,54,59,
106,67,69,103,69,71,104,76,79,108,69,75,117,66,66,120,69,67,130,81,80,
132,85,85,135,82,80,134,80,78,131,81,82,127,80,84,109,73,72,95,64,63,
85,54,60,76,44,56,60,34,48,54,30,45,52,28,43,52,28,42,51,30,43,
50,29,45,50,28,45,51,28,44,51,26,44,50,26,43,51,27,43,52,28,42,
53,29,41,57,35,45,85,69,70,85,69,70,84,68,69,83,67,69,81,64,70,
64,47,56,51,33,46,47,29,43,46,28,42,48,30,44,51,33,48,51,33,48,
52,32,45,54,33,45,55,33,45,55,33,46,55,32,48,54,30,48,58,32,48,
63,35,49,69,40,51,74,44,52,78,48,52,83,49,50,102,60,59,140,93,89,
149,98,95,148,97,94,148,97,94,149,98,95,151,101,94,139,91,86,132,88,89,
109,68,74,106,67,71,112,77,78,134,99,100,120,78,81,120,67,67,126,71,69,
131,80,79,129,80,80,130,82,82,128,80,80,128,80,81,135,87,87,129,90,87,
119,83,82,114,77,81,114,76,84,99,67,75,74,47,57,56,31,45,49,28,41,
50,30,40,52,30,47,51,27,48,50,25,44,52,28,44,51,28,44,49,26,42,
48,25,41,50,26,41,53,30,43,85,69,70,86,70,71,85,69,71,84,68,72,
70,53,61,48,31,42,47,29,43,45,27,41,50,32,46,52,34,48,50,33,47,
50,31,44,55,35,47,59,38,49,59,37,49,59,38,50,59,36,50,57,34,49,
58,32,47,64,36,49,79,49,58,79,48,54,79,45,49,85,47,48,115,72,70,
147,98,94,150,99,95,151,100,97,151,100,97,153,102,99,154,103,98,148,99,95,
140,93,93,125,79,82,109,65,68,107,68,68,117,78,78,114,69,71,115,67,68,
120,72,72,119,72,74,116,69,72,122,74,76,114,68,67,106,62,61,114,73,71,
120,80,80,118,80,79,120,83,77,131,94,85,146,106,97,133,97,89,90,64,61,
58,37,45,54,31,42,55,30,48,53,28,50,51,27,45,52,29,44,50,27,43,
49,26,42,50,27,43,52,28,43,52,30,43,84,68,69,85,69,70,89,73,76,
83,66,72,57,39,49,44,26,39,44,26,41,45,27,41,50,32,46,54,36,50,
57,39,52,58,40,51,58,38,49,64,43,52,63,42,51,64,43,53,61,40,52,
59,37,50,62,35,49,68,38,49,80,50,58,80,49,54,87,51,54,95,54,55,
130,83,80,145,92,87,146,94,90,151,100,97,150,99,96,152,101,98,154,103,100,
158,108,104,148,97,95,138,87,86,127,77,77,110,66,65,105,61,60,114,63,64,
114,65,64,114,67,65,116,66,67,116,63,67,123,67,71,125,71,74,121,74,74,
112,70,69,100,60,61,99,60,59,103,65,60,112,74,66,137,94,86,152,112,97,
148,115,101,108,78,79,67,37,49,59,29,49,52,26,48,50,28,46,51,29,43,
51,27,44,50,27,43,51,27,44,52,28,45,52,28,44,85,69,70,85,69,71,
83,68,72,69,53,61,49,32,44,44,26,39,46,28,41,47,29,43,49,31,44,
56,38,51,61,44,55,64,46,55,59,39,48,64,43,51,66,43,52,67,45,55,
62,40,51,59,38,51,64,37,50,71,41,51,82,50,56,88,54,58,92,53,54,
109,64,64,141,91,88,140,85,81,146,94,90,151,101,96,151,100,96,156,104,102,
158,108,108,154,103,101,150,96,94,152,96,94,152,97,96,146,96,94,140,91,89,
140,87,86,138,87,82,135,84,79,143,87,85,142,83,84,144,80,84,147,85,89,
147,92,94,130,85,84,99,59,58,96,58,57,104,66,64,100,59,61,106,64,67,
118,79,72,146,105,95,154,111,112,119,83,93,76,45,60,58,32,48,54,33,48,
50,28,43,50,27,44,51,28,44,52,29,45,52,28,45,51,28,44,87,72,75,
84,69,72,75,62,66,53,39,50,48,31,46,47,29,41,49,31,42,48,30,42,
52,34,46,62,44,56,61,44,54,59,41,51,57,37,48,60,39,50,67,41,55,
67,41,56,60,38,51,62,43,55,62,37,51,72,41,52,92,57,62,103,63,64,
107,63,59,130,82,78,140,89,86,147,93,91,155,103,100,154,104,97,156,104,97,
162,108,108,156,108,111,134,86,87,134,81,80,149,92,90,158,101,99,163,106,103,
158,103,100,154,100,97,153,99,94,149,93,89,151,91,91,147,84,86,150,86,88,
162,98,101,164,107,109,134,89,88,109,69,68,110,72,66,119,79,74,103,59,62,
100,54,63,97,59,64,102,53,61,119,70,76,126,89,94,108,76,78,79,51,52,
59,35,44,54,30,46,52,28,46,50,27,44,51,28,46,51,28,43,51,30,42,
89,77,77,89,77,78,76,63,66,47,33,44,47,30,45,50,32,44,53,35,47,
54,36,49,58,40,52,64,46,58,60,42,53,55,37,48,56,36,48,59,38,50,
60,36,50,58,35,49,60,38,51,64,42,55,68,40,52,79,47,55,101,65,67,
118,78,75,126,83,78,134,87,82,142,91,88,154,99,98,159,101,102,161,104,101,
163,108,104,163,107,110,142,97,101,113,70,73,120,71,72,139,87,85,149,96,93,
153,99,96,152,96,94,151,94,92,150,92,89,149,90,87,147,88,87,142,82,84,
145,84,85,155,93,96,152,94,97,127,79,79,117,72,71,133,83,80,142,88,86,
117,64,68,124,76,84,126,87,91,106,58,64,105,59,62,98,59,65,105,68,71,
106,72,72,90,58,63,68,40,51,57,31,44,50,26,41,51,29,46,51,30,44,
51,31,44,103,96,93,102,95,92,72,61,63,47,33,44,48,31,46,53,35,47,
57,40,51,60,42,54,61,43,55,59,41,53,55,37,49,53,34,46,56,35,48,
60,38,52,56,35,47,52,31,43,57,33,47,58,32,47,66,37,46,80,47,52,
101,64,63,123,82,77,136,91,86,146,98,94,160,108,105,155,101,99,159,101,103,
164,108,107,168,117,114,169,121,123,135,97,102,103,65,69,115,72,74,130,83,83,
134,85,81,138,87,82,146,89,86,148,88,87,151,89,87,151,90,88,149,90,89,
143,86,87,142,86,85,154,93,95,145,85,89,129,78,80,125,79,77,143,88,85,
147,85,85,122,67,70,150,110,113,162,131,131,133,83,87,125,68,74,107,62,70,
103,61,65,114,73,72,120,77,79,101,65,71,75,46,55,55,31,44,51,30,46,
48,30,44,48,30,45,115,112,109,101,96,95,57,46,50,49,34,46,52,34,49,
55,37,50,58,40,52,60,42,54,59,41,53,53,35,48,52,34,49,50,32,47,
51,31,46,55,34,49,54,34,48,49,30,43,53,31,43,60,31,44,74,43,49,
93,58,60,119,81,79,142,99,93,153,108,102,159,111,107,163,112,109,160,107,105,
164,112,111,166,117,113,165,120,116,162,123,124,118,85,90,91,56,60,113,72,75,
116,72,75,114,68,66,132,81,78,143,85,83,149,87,87,154,91,90,152,90,90,
151,92,92,142,87,86,139,84,83,154,96,97,145,88,91,143,91,93,140,94,91,
147,93,90,143,79,79,121,70,71,157,133,131,171,147,144,144,88,93,136,66,75,
121,69,78,113,66,71,124,77,76,130,81,80,121,80,82,102,70,74,76,50,60,
50,31,45,45,27,42,47,28,44,112,112,116,96,90,96,56,40,52,53,35,49,
55,37,51,55,37,51,59,42,53,57,39,50,52,34,47,49,31,46,50,34,50,
48,31,47,46,28,43,52,32,45,51,32,46,47,30,42,55,33,41,71,38,45,
92,55,59,106,67,67,125,83,81,141,97,92,151,103,99,155,107,103,159,111,107,
164,116,112,167,120,116,169,121,116,166,121,118,150,114,118,96,67,73,83,48,51,
118,76,77,103,61,65,84,46,50,109,62,61,125,68,65,145,84,85,156,98,98,
152,92,92,159,98,98,145,91,89,142,88,86,152,96,96,144,90,92,150,98,100,
152,101,100,156,103,102,147,81,85,141,91,90,184,164,162,171,143,145,141,74,84,
137,61,69,127,72,77,124,73,77,134,84,85,140,91,88,132,89,88,119,83,86,
94,67,73,59,41,50,46,28,41,48,30,46,116,117,120,93,86,93,53,37,49,
52,34,48,56,38,52,54,36,50,55,37,50,51,33,46,47,29,44,47,29,46,
47,29,47,44,27,43,45,25,41,50,29,42,52,29,43,53,31,42,68,42,48,
95,58,64,111,69,72,118,75,75,134,88,85,138,88,85,144,94,90,156,106,102,
160,111,107,165,115,112,165,116,113,168,118,113,162,115,113,137,99,103,91,58,65,
97,61,64,126,85,86,99,61,64,81,48,54,94,52,54,111,60,56,131,77,74,
155,99,98,153,93,93,161,100,101,151,96,95,140,86,84,141,86,86,147,93,95,
155,103,105,163,109,111,165,114,115,147,87,91,148,97,98,186,158,158,155,120,122,
138,69,78,138,66,71,131,78,78,132,82,83,142,92,92,146,96,95,136,93,92,
125,90,90,102,76,79,73,55,62,64,49,61,47,33,47,115,115,119,80,73,80,
48,32,44,50,32,46,55,37,50,53,35,49,51,33,47,47,29,43,44,26,42,
45,26,45,46,27,45,46,26,42,49,27,43,54,31,45,61,35,47,67,40,49,
82,51,56,107,65,69,120,74,76,129,81,82,143,91,89,144,90,86,144,91,88,
150,99,96,162,111,108,168,117,114,166,116,114,165,114,110,156,107,106,125,85,90,
90,54,63,107,67,71,131,90,90,100,65,67,94,61,71,105,63,69,123,72,68,
127,74,68,145,90,87,151,91,91,157,96,96,151,97,95,139,85,83,145,89,90,
156,102,104,157,104,107,163,107,111,159,112,114,142,89,92,169,119,121,193,156,159,
148,105,108,149,79,86,146,78,80,138,87,83,140,90,87,154,104,103,153,103,104,
140,97,95,130,96,93,104,78,81,99,82,88,153,144,153,118,110,122,105,105,109,
67,60,68,46,30,42,47,29,43,52,34,49,53,34,49,52,34,49,48,30,46,
44,26,43,43,24,43,46,25,43,48,25,43,53,30,46,62,38,52,70,42,54,
80,50,58,95,62,65,114,72,73,126,79,80,135,86,86,145,92,91,150,95,92,
151,98,95,147,95,92,157,105,102,168,116,113,166,116,113,163,111,108,151,103,102,
113,72,77,89,54,60,117,79,82,136,96,98,122,88,90,131,97,107,121,77,85,
131,77,77,134,77,73,142,85,83,148,87,87,157,96,96,153,98,96,145,90,88,
159,104,104,168,114,116,158,105,108,161,104,111,154,106,110,139,89,92,179,132,132,
169,125,127,138,85,88,151,81,86,151,89,88,145,96,89,150,100,95,159,109,108,
160,110,112,151,108,106,134,100,98,110,84,85,99,78,82,175,162,171,206,200,212,
84,82,88,56,48,57,46,30,41,49,31,44,54,36,51,55,36,53,48,29,46,
44,26,42,44,26,42,44,26,42,47,25,42,50,27,44,56,34,48,64,42,54,
68,42,55,72,43,53,82,50,53,97,63,61,105,66,64,113,71,69,128,82,80,
137,88,85,144,94,90,152,102,98,155,105,102,161,110,107,159,112,107,156,108,104,
147,99,99,115,75,76,92,60,60,118,81,83,136,95,98,118,82,83,123,88,92,
138,93,98,143,88,89,139,82,78,148,90,88,152,93,92,159,102,101,160,104,103,
145,89,88,148,94,94,164,111,113,159,108,110,160,104,110,148,93,99,133,83,86,
162,117,116,139,88,89,139,77,80,152,87,90,149,96,92,149,101,95,156,107,103,
159,110,110,164,114,118,154,113,113,134,102,104,112,83,87,95,64,66,135,107,114,
196,182,192,72,69,72,52,43,50,50,33,44,52,34,48,54,36,51,54,36,53,
44,25,42,43,25,41,45,27,43,43,25,41,48,26,44,58,35,52,61,39,52,
60,38,50,61,36,49,62,34,45,72,42,47,89,57,56,101,62,60,105,63,62,
119,74,72,139,91,87,144,96,92,155,106,102,157,108,104,152,103,99,155,108,101,
154,104,100,148,99,98,127,87,86,102,68,66,117,77,77,132,89,90,106,66,67,
108,64,66,146,92,95,154,92,93,149,88,85,155,96,95,160,103,102,165,108,107,
166,111,109,151,98,96,139,87,86,152,101,102,152,102,103,155,103,101,140,87,87,
141,87,91,146,90,98,133,74,79,143,88,89,151,99,97,154,100,97,155,103,100,
156,108,104,159,112,112,163,118,120,150,112,112,128,98,101,106,78,84,92,62,65,
118,82,82,159,132,136,69,65,72,53,41,53,53,36,48,55,37,50,50,33,48,
51,33,50,42,24,40,42,24,40,45,27,43,44,26,42,50,28,46,61,38,55,
56,34,48,51,29,42,52,28,43,56,30,42,68,40,45,89,59,58,111,74,71,
121,79,77,132,87,84,147,98,95,150,102,98,154,107,103,153,106,102,148,101,96,
151,103,95,153,101,95,149,99,96,135,93,91,117,80,77,121,78,77,132,85,85,
114,70,69,115,68,65,147,90,88,152,90,89,157,99,95,161,105,103,168,112,111,
176,122,121,176,123,121,163,112,109,142,91,90,143,94,93,147,100,100,150,102,100,
134,82,81,133,74,78,140,74,82,147,86,91,152,102,101,149,104,100,155,104,99,
161,108,104,165,116,112,167,122,122,157,118,117,140,104,105,117,88,93,97,71,78,
92,63,68,122,83,81,137,100,102,73,66,79,55,41,58,52,34,48,56,39,52,
51,33,48,48,29,46,43,24,41,43,24,41,43,25,42,42,24,41,50,29,46,
64,41,57,59,36,51,52,30,43,54,30,45,56,31,43,68,41,48,84,54,57,
98,60,62,111,68,69,127,81,80,140,93,89,147,101,96,150,105,100,152,106,101,
151,104,99,149,100,93,148,97,91,144,93,90,137,91,88,129,89,84,131,86,83,
140,91,89,125,79,76,125,75,70,145,89,84,149,90,87,155,99,95,163,108,106,
170,114,113,175,121,119,175,124,121,167,117,114,150,100,99,138,89,89,138,91,91,
138,90,93,117,66,68,134,76,76,156,92,90,156,102,101,156,106,105,158,107,105,
158,111,106,164,114,110,166,118,114,161,119,118,143,107,106,125,93,93,105,79,82,
90,67,72,96,68,73,120,87,85,129,91,92,80,69,82,55,42,56,52,36,48,
55,39,52,49,30,46,47,26,44,42,22,41,43,24,42,42,23,42,43,23,42,
50,29,46,59,37,53,57,35,51,52,30,46,55,31,45,56,31,43,65,39,48,
79,51,60,92,56,67,98,59,67,111,72,72,126,85,81,137,97,94,145,102,99,
151,105,100,153,104,97,149,101,95,148,99,95,148,98,94,143,91,88,136,89,85,
136,91,86,144,97,93,138,89,85,137,82,79,148,90,88,154,94,93,157,96,95,
160,102,101,165,107,106,167,111,110,170,116,114,165,113,111,154,104,102,138,89,87,
130,83,81,130,83,84,125,77,76,147,98,92,158,109,98,151,107,100,158,105,105,
166,107,109,162,115,112,165,119,115,161,115,113,150,109,109,132,96,96,114,83,83,
101,76,75,88,66,66,99,71,74,126,100,93,133,96,93,74,64,75,53,39,51,
50,35,46,54,37,50,47,28,44,45,24,42,42,22,41,41,22,41,43,24,43,
45,26,45,50,30,48,53,32,49,49,28,45,49,28,45,51,29,43,52,29,42,
61,35,47,68,41,51,75,48,55,76,49,54,84,56,56,98,68,65,112,77,77,
122,84,84,133,91,88,142,98,94,149,102,98,146,98,94,145,96,92,143,92,89,
138,91,86,138,91,86,144,95,91,147,98,94,147,94,91,149,93,90,154,96,94,
157,98,97,157,98,97,158,101,100,159,104,102,160,106,104,158,108,104,153,105,101,
145,98,93,141,96,91,136,91,88,136,90,87,143,96,90,150,101,94,146,100,94,
155,104,101,161,107,105,160,112,108,160,114,109,151,108,104,139,99,98,123,88,88,
109,76,78,98,71,71,92,67,65,111,80,79,139,106,101,147,107,106,50,40,49,
46,34,44,49,33,44,53,37,49,49,30,46,45,24,42,43,23,42,41,23,42,
43,24,43,45,26,45,47,28,47,50,30,49,46,27,45,45,25,44,47,26,42,
51,29,42,59,35,48,64,39,50,66,42,49,66,43,47,68,45,48,77,52,55,
86,55,60,97,63,67,109,71,73,125,85,84,145,101,97,149,103,99,147,98,95,
143,94,90,140,92,88,137,89,85,139,90,86,145,94,90,146,93,90,145,91,87,
150,93,91,154,96,94,156,98,97,156,98,97,155,100,98,155,101,99,151,103,97,
151,105,98,150,106,99,149,107,99,148,106,98,148,105,98,150,104,99,149,101,98,
152,105,99,155,108,101,155,108,102,152,106,100,148,103,98,142,100,96,131,94,93,
120,88,87,107,72,76,96,65,67,103,75,72,128,95,90,149,110,107,154,112,113,
41,34,41,44,32,41,52,36,47,55,37,50,50,31,47,46,25,44,44,24,43,
43,24,43,44,25,45,43,24,44,44,25,44,46,27,46,42,24,43,42,23,42,
45,24,41,48,26,41,54,31,45,57,33,47,65,43,51,68,46,52,67,44,50,
72,47,54,73,46,53,81,51,58,93,59,65,110,72,77,137,96,93,149,106,100,
144,98,92,141,93,87,140,93,88,138,90,86,139,89,85,142,90,86,142,88,85,
142,87,84,145,89,86,148,90,89,151,93,91,152,95,93,154,99,96,154,101,97,
149,100,93,147,102,94,146,103,95,140,100,91,142,102,93,143,101,94,146,102,97,
145,100,97,143,97,91,143,99,91,146,103,95,145,99,92,140,96,89,135,95,90,
122,88,85,107,77,75,99,64,69,105,69,73,122,88,85,139,103,96,152,109,107,
157,113,115,48,45,52,46,37,46,50,33,44,56,35,50,52,30,49,46,25,46,
44,24,45,43,24,44,43,24,44,43,24,44,44,25,45,44,25,45,41,22,41,
42,24,40,45,24,40,46,24,40,51,29,44,55,33,48,62,43,55,66,48,58,
64,45,56,63,42,53,63,43,51,72,47,56,87,55,66,98,60,72,120,81,82,
139,100,95,141,98,91,140,95,84,139,92,84,139,90,85,143,92,87,143,90,86,
146,89,87,146,87,86,144,86,85,145,86,85,146,89,86,149,94,89,151,98,92,
150,98,92,146,96,89,143,96,90,141,100,93,135,98,90,136,98,92,137,98,92,
137,98,92,134,95,89,131,91,85,134,92,87,137,96,90,136,94,89,133,93,86,
128,92,85,110,80,74,96,69,66,102,69,72,122,80,84,141,96,96,146,105,99,
151,109,107,158,109,113,51,51,55,46,42,48,49,33,44,54,35,49,50,31,49,
43,23,44,42,22,43,42,23,43,42,23,43,42,23,43,43,24,44,44,25,45,
44,25,43,45,26,42,47,26,42,47,26,41,50,29,44,58,37,52,62,43,55,
62,44,55,61,41,53,63,42,53,65,44,52,70,47,55,81,54,63,86,54,65,
98,65,68,120,84,83,134,96,92,139,97,92,139,91,83,141,89,82,143,88,82,
142,87,81,142,86,84,141,86,85,141,86,84,141,85,83,139,87,84,140,89,85,
140,92,87,141,93,88,139,93,87,136,92,86,137,97,91,137,98,93,136,98,93,
135,98,92,131,94,88,127,89,83,124,85,82,125,86,84,130,91,89,131,92,90,
123,85,83,113,78,74,101,69,66,102,72,69,115,81,80,131,90,90,146,101,101,
144,104,99,148,106,106,148,99,103,60,63,64,52,51,54,53,40,49,55,38,52,
48,30,48,41,23,43,40,21,42,40,21,41,41,22,42,42,23,43,42,23,43,
42,23,43,43,24,43,48,29,46,51,33,48,49,30,45,48,29,43,53,34,48,
58,40,53,58,39,52,58,37,50,62,41,54,67,46,54,69,48,54,71,48,56,
75,50,59,82,54,60,96,64,68,111,77,78,121,84,83,131,86,82,138,89,84,
137,86,83,138,85,82,135,83,80,134,83,80,135,84,81,136,85,82,136,88,84,
133,88,84,133,89,84,134,92,86,133,94,87,131,92,86,133,93,89,135,94,92,
131,93,90,128,91,87,121,85,81,117,80,76,115,79,77,116,81,80,120,84,83,
119,83,83,109,73,74,101,65,66,102,66,65,115,78,76,132,91,89,142,94,92,
151,99,100,147,99,99,148,99,99,150,100,103,67,75,73,58,60,61,56,44,53,
58,41,56,49,32,49,41,24,43,40,21,42,41,22,42,42,23,43,43,24,44,
44,25,45,42,22,42,41,22,41,47,28,45,56,37,52,56,38,52,51,33,47,
50,33,46,55,37,51,58,39,53,59,38,53,61,38,54,66,44,56,69,48,57,
65,44,51,64,44,50,70,46,53,78,51,58,83,53,59,91,57,62,106,67,68,
126,83,83,136,89,89,137,89,87,132,84,80,130,81,77,130,81,76,131,82,78,
127,83,79,122,81,77,122,82,78,123,84,80,125,88,82,128,90,85,126,86,84,
126,84,84,124,85,84,123,86,83,115,79,76,111,76,72,110,75,75,111,76,76,
110,75,76,102,68,69,98,63,67,98,61,65,109,69,70,126,85,83,142,95,91,
148,93,92,152,92,95,148,89,94,148,91,92,146,95,98,63,75,71,52,56,55,
53,41,51,59,42,57,51,34,51,43,26,44,42,24,43,42,23,42,43,24,43,
44,25,44,45,26,43,43,25,42,44,26,42,46,28,44,54,36,51,60,42,56,
58,40,54,56,38,52,57,39,53,60,42,56,61,40,55,60,38,53,65,42,57,
68,46,59,64,43,53,63,42,49,62,43,49,67,46,54,74,47,57,80,49,60,
92,60,64,121,82,83,134,87,86,130,84,79,125,81,75,124,79,76,124,79,77,
120,75,74,111,73,69,111,73,69,111,72,70,112,72,72,117,79,78,120,82,80,
116,76,75,115,74,74,114,73,74,112,73,74,107,73,72,103,71,69,106,69,69,
107,69,70,102,66,68,95,60,64,99,61,67,106,65,69,119,75,75,138,92,88,
147,97,93,145,91,91,148,91,94,153,92,97,153,95,96,158,112,116,70,81,77,
54,57,56,50,37,46,58,41,54,55,37,52,47,29,46,45,26,44,43,25,42,
43,24,41,44,25,42,45,27,43,46,28,44,45,27,42,49,31,47,58,40,54,
57,39,53,51,33,47,50,32,46,53,36,50,59,40,54,60,39,54,59,36,52,
62,37,52,64,38,53,65,39,52,61,36,48,54,36,48,60,41,53,70,47,57,
74,46,56,85,54,60,113,75,79,119,77,80,114,75,75,111,73,72,111,70,71,
111,69,74,109,68,73,108,70,68,109,72,69,107,70,69,104,69,68,104,70,69,
103,68,68,103,66,66,105,66,68,103,69,70,101,69,70,101,70,70,98,67,67,
98,64,66,98,64,65,93,58,59,95,59,59,108,65,66,120,73,74,133,84,83,
147,95,92,153,98,96,149,94,94,153,96,99,153,95,98,151,105,105,189,159,169,
72,81,78,56,58,57,50,35,43,57,37,50,59,39,53,52,32,48,47,29,45,
45,28,43,46,28,43,46,28,44,47,29,43,50,32,46,51,33,47,50,32,46,
54,36,50,56,38,52,55,37,51,52,34,48,52,34,48,54,35,49,56,35,50,
59,36,52,59,38,50,59,37,50,62,39,54,58,36,53,55,35,48,61,39,48,
71,45,50,82,52,52,97,60,60,114,69,71,114,68,71,106,66,68,106,65,66,
112,69,71,110,67,71,108,64,70,107,66,67,104,66,65,102,68,67,97,67,66,
93,63,64,92,61,63,93,61,64,92,58,61,89,57,61,87,56,60,90,55,61,
91,54,61,90,52,60,94,54,60,102,58,61,115,67,68,126,76,74,135,82,80,
144,87,86,151,91,91,155,95,95,154,97,98,155,100,102,157,104,106,177,143,148,
214,202,214,64,71,70,53,55,56,51,38,46,52,34,45,58,38,52,57,35,51,
51,32,47,48,29,44,48,29,44,52,33,46,50,31,45,49,31,45,54,36,49,
54,36,49,57,39,53,56,38,52,55,38,52,55,37,51,54,36,50,55,36,50,
56,36,51,58,37,52,56,39,49,54,38,49,55,38,51,55,36,50,67,40,49,
92,60,64,117,81,79,126,87,80,127,83,77,129,79,77,134,82,82,129,83,83,
123,75,74,117,68,67,112,65,65,110,63,64,109,65,67,104,65,66,92,58,57,
88,55,56,85,54,57,83,51,54,85,51,56,84,49,54,86,48,54,90,49,56,
94,50,58,101,55,63,108,60,69,115,65,73,129,74,79,141,81,85,144,85,82,
146,86,83,150,88,87,155,92,92,156,95,97,156,96,98,152,96,98,171,124,127,
208,187,199,211,212,225,54,58,63,47,50,55,47,39,49,47,33,45,55,37,51,
59,38,53,55,33,49,50,29,44,51,30,45,54,33,48,55,37,52,49,31,47,
51,33,49,54,36,51,56,40,53,56,40,53,56,40,53,56,40,53,55,38,52,
53,35,49,54,36,50,55,37,51,52,37,47,56,38,50,59,35,48,68,41,47,
105,67,68,140,95,95,148,100,96,143,95,86,136,88,82,136,83,80,146,90,90,
153,103,102,150,98,96,140,89,86,139,90,86,137,88,84,144,96,96,135,92,93,
114,73,75,109,63,68,106,60,64,106,61,63,107,63,66,109,64,67,118,68,72,
126,72,77,129,73,75,133,76,75,138,80,80,139,79,80,145,82,84,153,87,90,
153,87,85,151,88,85,153,90,90,151,89,90,152,91,95,157,94,96,157,106,109,
194,168,175,212,207,222,203,208,225,51,54,59,47,48,53,48,41,51,45,35,46,
51,37,50,55,37,51,55,36,51,53,34,48,51,32,46,51,33,47,54,36,50,
54,36,49,50,32,45,50,32,45,53,36,49,54,38,51,52,36,49,55,39,52,
57,40,54,54,36,50,54,36,50,53,35,49,51,37,47,54,36,47,71,41,53,
106,69,72,144,96,98,156,107,108,162,114,112,154,104,100,148,96,96,145,89,91,
146,87,88,150,96,95,143,97,93,145,97,93,164,111,109,168,111,110,169,115,114,
143,95,93,131,86,85,135,85,88,133,78,81,129,74,77,127,73,75,128,73,76,
131,76,79,135,78,80,137,78,77,139,79,76,145,83,79,144,82,78,145,82,78,
150,86,83,148,88,85,146,88,85,149,88,88,149,87,88,151,87,84,148,89,90,
170,133,142,197,187,202,200,200,218,199,202,220,54,54,59,50,47,54,50,45,54,
49,42,52,52,41,52,51,37,49,56,39,52,55,38,52,51,35,48,51,35,48,
51,33,45,55,37,48,55,37,48,50,33,44,52,36,48,54,38,51,51,35,48,
53,37,50,55,37,51,52,34,48,56,38,52,55,38,52,51,39,49,57,37,49,
93,56,65,134,84,85,158,97,100,175,122,124,182,137,137,171,117,120,162,105,112,
172,111,119,176,113,117,168,111,109,153,102,99,167,115,113,178,123,121,172,116,115,
156,103,105,125,79,81,122,77,80,124,74,81,119,67,73,116,65,70,115,64,69,
117,66,71,118,65,71,121,66,71,126,70,72,129,72,72,134,74,72,137,76,74,
136,79,75,136,81,77,134,82,77,132,79,76,138,80,80,144,82,85,138,80,76,
140,93,97,184,160,175,196,194,210,197,198,218,191,192,212,51,50,57,47,43,50,
49,44,53,53,49,58,56,50,60,51,40,52,51,37,49,53,38,50,55,40,52,
55,39,51,54,37,48,55,37,47,55,38,49,50,33,44,51,34,47,49,32,47,
50,33,47,55,38,52,57,40,55,51,33,48,51,33,48,52,33,48,49,36,45,
57,37,46,96,58,65,141,86,85,158,91,94,159,101,102,167,118,117,166,111,114,
152,93,101,175,112,120,190,125,129,176,116,115,157,97,97,165,106,107,163,111,110,
143,98,95,105,64,68,91,54,61,90,54,61,93,53,63,93,55,63,92,57,64,
92,56,63,91,54,62,93,53,61,96,52,61,99,54,60,101,56,60,108,59,62,
113,62,65,112,63,64,114,67,66,116,71,68,119,70,67,126,70,70,132,71,73,
125,70,70,144,108,116,196,183,198,196,197,211,195,197,216,145,145,165,48,46,54,
45,43,51,51,46,56,53,50,59,51,49,58,53,45,56,53,42,52,49,38,48,
55,42,52,59,44,55,59,43,54,53,36,48,52,35,47,52,36,47,51,35,48,
46,30,45,49,33,48,52,35,50,52,36,51,52,35,50,53,34,49,54,33,49,
51,33,44,52,32,41,77,45,54,126,78,81,157,94,97,156,93,90,147,87,79,
153,94,90,154,95,95,156,92,93,163,95,98,164,100,101,161,100,101,150,96,97,
131,90,90,100,69,69,83,59,63,86,64,69,85,63,69,86,63,69,88,65,71,
89,67,72,88,66,71,88,65,71,89,64,70,88,61,68,85,56,64,83,53,61,
82,51,59,83,48,55,88,48,54,94,50,55,101,57,61,110,63,66,119,65,65,
122,63,61,118,65,68,155,126,135,193,190,204,190,194,212,172,173,195,84,77,99,
48,45,54,44,41,50,45,41,50,44,44,52,44,44,52,53,46,57,54,45,54,
47,39,47,53,42,52,56,44,54,56,43,54,51,37,48,50,36,47,51,37,48,
49,34,46,50,34,46,51,35,48,48,32,45,48,32,44,50,34,46,51,33,46,
54,33,47,53,34,47,52,32,48,60,34,49,84,46,57,127,79,82,155,103,99,
158,99,93,153,87,87,142,81,81,145,82,82,148,83,84,148,90,89,140,96,94,
121,85,84,97,68,68,87,64,66,88,67,72,91,70,75,89,68,73,86,66,71,
88,68,73,90,69,74,90,70,75,91,71,75,92,70,76,93,71,76,93,70,76,
93,67,74,88,65,72,86,62,70,85,58,66,86,57,66,100,63,71,99,58,65,
102,58,62,106,58,61,116,79,82,166,151,159,181,184,197,166,170,187,105,99,119,
51,35,56,47,44,54,43,40,49,42,39,49,46,48,56,42,43,51,48,42,52,
53,48,57,51,47,54,53,47,55,53,45,53,55,46,54,55,46,55,50,41,50,
46,36,45,48,33,44,50,34,44,46,30,41,47,31,42,46,30,41,48,31,42,
50,32,43,52,32,44,53,34,44,47,32,45,49,32,47,53,30,41,69,40,46,
93,61,61,116,76,75,129,81,86,125,79,83,122,74,76,118,72,74,108,71,70,
95,69,67,92,70,70,91,69,72,89,67,74,88,68,73,89,70,74,89,70,74,
86,67,71,87,68,72,88,69,73,89,70,74,90,71,75,88,72,75,87,71,74,
88,70,74,88,68,73,87,68,70,90,70,74,90,70,77,90,71,80,124,102,109,
126,101,108,101,75,83,93,66,74,129,108,114,169,162,170,152,154,169,95,92,108,
52,41,57,45,30,47,46,43,51,46,43,51,46,45,53,57,59,66,49,52,58,
47,45,53,53,51,58,53,52,58,52,49,55,52,48,55,56,52,59,59,54,61,
55,49,57,51,44,52,57,46,54,66,51,60,64,48,56,60,44,53,59,43,53,
59,43,52,60,43,52,61,42,52,63,44,52,63,49,57,63,50,59,69,53,60,
74,56,64,78,59,64,87,66,69,90,66,72,92,66,71,94,66,70,94,68,72,
88,71,72,86,69,70,90,70,73,90,68,74,89,66,75,86,67,72,87,70,73,
89,71,74,86,68,71,86,69,73,87,70,73,88,70,74,88,71,74,86,72,75,
85,71,73,86,71,74,87,72,75,88,69,69,88,69,70,92,74,76,86,69,74,
102,89,95,129,116,126,127,114,127,127,115,129,148,133,143,130,120,133,81,74,91,
47,33,52,43,30,45,43,32,47,49,46,52,47,44,50,44,44,51,55,56,64,
50,50,58,42,42,50,44,45,50,47,48,53,51,52,58,49,50,55,49,50,55,
54,54,60,63,61,67,54,51,58,56,54,58,75,66,71,85,69,73,83,66,70,
82,66,72,81,64,72,80,63,71,80,63,71,80,63,71,84,66,74,86,69,74,
85,69,72,85,67,71,88,69,73,86,67,71,86,68,71,87,69,72,85,67,71,
89,71,75,89,71,75,91,70,75,92,70,76,90,70,75,87,69,72,84,68,71,
85,69,72,87,69,73,87,68,73,86,69,73,88,72,75,87,71,74,88,71,74,
90,71,75,87,70,74,87,71,75,87,72,75,86,70,73,88,71,74,89,73,76,
88,72,75,88,73,76,95,80,91,92,78,97,86,72,90,80,65,80,61,44,61,
46,29,48,45,28,47,46,29,48,62,49,65,52,49,56,46,43,51,42,40,48,
47,46,54,45,43,52,43,42,50,46,49,54,47,51,55,53,58,62,49,54,58,
45,46,51,53,52,58,61,61,67,55,57,63,56,61,62,73,69,71,81,66,71,
82,66,70,85,68,73,86,69,75,85,68,74,84,67,74,86,69,76,86,69,77,
87,70,75,84,68,71,82,64,68,87,68,72,88,69,73,88,69,73,88,68,73,
87,67,72,89,69,74,90,69,74,90,68,73,91,69,74,90,70,75,88,70,74,
85,69,72,84,67,71,86,67,71,88,68,72,88,69,73,87,69,72,88,70,73,
90,72,75,90,70,75,89,70,74,87,70,74,85,70,73,85,70,73,87,71,74,
87,71,74,85,69,72,87,72,69,85,70,72,80,65,74,59,46,55,47,32,43,
42,25,40,46,29,46,45,28,47,50,30,49,58,38,57,52,49,59,45,42,52,
42,37,47,45,40,50,44,38,48,49,44,54,53,52,59,49,50,55,55,56,61,
54,55,60,48,46,53,48,47,53,49,53,57,56,63,66,62,74,74,75,77,78,
79,68,71,83,67,70,85,69,72,85,69,72,83,67,70,82,65,69,83,66,73,
84,67,74,85,68,73,84,68,70,84,65,69,87,67,71,88,69,73,89,69,74,
89,68,73,89,68,73,89,68,74,90,68,73,90,68,73,90,69,74,90,71,75,
88,71,74,85,69,72,84,68,71,87,68,72,88,68,73,87,68,72,86,67,71,
87,69,72,88,69,73,87,65,70,88,67,72,88,70,74,87,70,74,87,71,74,
87,71,74,87,71,74,84,68,71,89,72,67,89,73,72,88,73,80,81,67,72,
66,51,61,53,37,50,45,29,44,47,30,47,49,31,49,51,33,51,53,49,60,
47,43,54,44,36,47,47,38,50,44,36,47,47,39,50,52,47,56,46,42,50,
51,47,55,57,52,60,50,46,53,48,46,52,46,49,54,55,62,65,60,71,70,
74,77,77,83,70,74,86,69,72,84,68,70,83,67,71,83,67,71,83,66,70,
82,65,72,83,66,74,83,67,72,84,67,71,86,68,71,86,68,72,87,69,73,
88,70,73,90,68,73,90,67,73,90,67,73,90,67,73,91,68,74,91,69,75,
91,70,75,89,70,74,86,69,72,85,67,71,86,67,71,88,68,72,88,68,73,
87,68,72,88,69,73,88,68,72,85,65,70,88,68,73,89,69,73,88,69,73,
87,70,73,88,72,75,87,72,74,85,71,73,90,73,71,89,71,72,86,70,77,
86,71,77,86,70,79,78,62,74,60,44,57,49,32,48,41,25,43,45,29,48,
49,42,54,45,38,50,45,34,47,48,38,49,46,37,48,48,41,51,49,45,55,
43,40,49,47,40,50,50,42,53,49,44,51,47,42,49,47,43,49,54,49,55,
54,54,57,71,64,68,86,68,73,87,69,73,83,66,71,83,66,72,83,66,72,
82,65,71,83,66,72,84,67,73,83,66,72,84,67,72,85,69,72,85,69,72,
85,68,72,86,69,73,89,68,73,91,67,73,90,67,73,90,67,73,91,68,74,
92,69,75,91,68,74,90,67,73,87,67,72,86,67,71,87,68,72,86,67,71,
85,68,71,84,69,72,85,69,72,86,69,73,85,69,72,88,71,74,88,69,73,
87,67,72,89,68,73,88,71,75,88,71,75,87,72,74,87,72,72,85,69,70,
87,71,71,89,73,73,87,71,76,87,71,79,84,68,77,72,56,66,51,33,52,
48,29,49,49,45,53,46,40,49,45,38,48,48,40,51,49,43,53,52,48,57,
55,53,62,55,53,62,54,50,59,55,49,59,61,53,62,63,54,63,67,58,67,
69,60,69,66,65,70,75,69,74,82,66,73,84,67,72,85,68,73,84,67,73,
83,66,72,82,65,71,84,67,73,85,68,74,84,67,73,85,68,73,84,68,72,
84,68,70,84,68,71,86,69,73,89,68,73,90,66,72,90,67,73,89,66,72,
90,67,73,90,67,73,89,66,72,89,66,72,86,66,70,84,65,69,85,66,70,
86,67,71,85,68,71,85,70,72,86,70,73,86,70,73,86,70,73,88,71,75,
88,69,73,87,66,71,90,67,72,92,68,74,91,70,74,90,70,74,87,69,72,
86,70,71,88,72,73,88,72,73,86,71,72,89,74,75,88,73,75,87,72,76,
69,52,65,48,30,46,77,74,80,77,73,80,79,74,83,82,77,87,85,83,91,
89,88,96,92,92,100,95,94,102,94,93,101,98,94,103,103,97,107,104,98,109,
108,102,113,106,101,111,106,107,113,99,94,101,83,69,77,82,68,74,83,67,73,
85,67,74,84,67,73,83,66,72,82,65,71,83,66,72,82,65,71,83,66,72,
83,66,70,83,67,70,84,68,71,86,70,73,90,68,74,90,66,72,89,66,72,
88,65,71,91,68,74,90,67,73,87,64,70,87,64,70,88,67,72,88,69,73,
89,70,74,88,69,73,88,71,74,87,71,74,86,70,73,84,68,71,85,69,72,
87,70,74,87,68,72,86,66,71,89,67,72,90,68,74,89,70,74,88,71,74,
86,69,72,85,69,72,88,72,75,88,72,74,87,72,72,88,74,74,87,72,75,
88,73,77,84,67,75,60,43,55,102,103,108,105,105,111,110,110,116,110,111,118,
109,111,118,108,111,118,109,111,118,111,112,120,112,112,119,115,114,122,114,115,124,
114,116,125,117,118,127,114,115,124,115,120,125,103,101,107,81,68,76,83,69,76,
83,66,73,84,67,73,84,67,73,84,67,73,83,66,72,82,65,71,82,65,71,
82,65,71,82,66,69,82,66,69,84,68,71,86,70,73,89,68,73,90,67,73,
89,66,72,88,65,71,88,66,71,87,65,70,87,64,70,89,67,73,88,69,74,
86,68,72,86,68,72,87,69,72,88,71,74,87,71,74,85,69,72,83,67,70,
83,68,71,86,69,72,87,68,72,87,68,72,86,68,72,86,69,73,86,71,73,
86,71,73,86,70,73,86,70,73,87,72,75,87,72,75,86,72,75,89,74,77,
86,71,77,86,69,77,91,73,81,83,64,74,105,107,114,107,109,117,107,110,116,
107,110,116,108,111,116,108,111,117,109,110,118,109,110,118,110,111,116,108,109,114,
104,109,114,105,109,116,107,109,117,105,106,114,101,106,111,94,92,97,83,70,78,
85,68,75,85,68,74,84,66,72,83,66,72,84,67,73,83,66,72,82,65,71,
82,65,71,82,65,71,82,66,70,82,66,69,83,67,70,86,70,73,87,69,73,
88,68,72,87,67,71,87,66,71,87,66,71,87,66,71,87,66,71,87,67,72,
85,67,71,83,67,70,83,67,70,85,69,72,86,68,71,86,68,71,86,68,71,
86,68,71,85,68,71,85,69,72,86,69,72,85,68,72,84,68,72,85,69,72,
88,69,73,89,70,74,88,69,73,87,70,73,87,72,75,87,72,75,86,72,73,
86,72,73,88,71,75,90,71,77,89,69,76,109,88,89,90,88,99,89,86,99,
88,86,96,86,84,93,84,82,91,84,82,91,81,79,90,76,75,85,76,75,83,
72,71,77,69,70,77,71,72,79,70,69,77,68,65,74,62,66,71,71,67,73,
83,68,77,84,66,74,85,68,75,84,67,73,83,66,72,83,66,72,83,66,72,
82,65,71,83,66,72,84,67,72,82,66,70,83,67,70,83,67,70,84,68,71,
86,68,72,86,67,71,86,67,71,86,67,71,87,68,72,88,69,73,88,69,73,
87,68,72,84,67,70,83,67,70,85,69,72,86,70,73,87,68,73,87,67,71,
86,66,70,87,67,71,87,68,72,86,67,71,86,68,72,85,67,71,84,67,71,
85,68,71,88,69,73,89,68,73,87,66,71,87,67,72,87,71,74,87,71,74,
85,71,72,85,71,72,88,71,75,90,71,78,90,69,76,114,92,83,50,41,58,
51,42,60,51,42,57,50,42,55,52,43,56,53,45,58,48,42,56,42,36,49,
44,38,50,42,37,47,42,40,48,46,44,53,47,42,52,47,41,51,43,44,50,
56,50,57,79,62,72,85,65,74,85,67,74,83,66,72,82,65,71,82,65,71,
82,65,71,82,65,71,84,67,73,85,68,74,83,67,71,83,67,70,84,68,71,
82,66,69,84,68,71,86,69,72,86,69,72,84,68,71,84,67,71,85,69,72,
86,69,73,86,69,72,84,68,71,83,67,70,84,68,71,85,69,72,86,67,71,
87,65,70,86,65,70,86,65,70,85,65,70,85,66,70,85,66,70,86,67,71,
84,67,70,85,68,71,88,69,73,88,68,72,87,65,71,88,67,72,89,69,74,
88,71,74,87,72,73,86,72,73,87,70,74,87,68,75,89,68,74,114,92,77,
40,26,47,42,28,49,44,31,49,47,34,49,49,37,51,49,36,52,44,32,49,
41,31,47,44,35,49,43,34,46,42,36,46,47,41,51,47,40,51,48,40,51,
46,44,50,57,49,56,77,59,68,85,65,73,84,66,73,82,65,71,82,65,71,
83,66,72,83,66,72,82,65,71,84,67,73,86,69,75,84,68,71,84,68,71,
85,69,72,83,67,70,85,69,72,85,69,72,84,68,71,84,68,71,85,69,72,
83,67,70,83,67,70,84,68,71,85,69,72,84,68,71,82,66,69,82,66,69,
86,65,70,88,65,71,89,66,72,89,65,71,88,65,71,88,66,70,86,66,70,
85,65,70,84,67,70,86,70,73,89,70,74,89,69,74,89,67,72,89,67,73,
89,69,73,88,70,73,87,72,73,87,72,73,87,70,74,88,69,75,91,69,76,
112,89,76  };

wxBitmap *wx_get_alternate_icon(int little)
{
  wxBitmap *bm;
  wxMemoryDC *dc;
  int i, j, step, unstep, dx, dy;

  bm = new wxBitmap((little ? 20 : 64), (little ? 20 : 64), 0);

  dc = new wxMemoryDC();
  dc->SelectObject(bm);

  if (!dc->Ok()) {
    dc->SelectObject(NULL);
    return NULL;
  }

  if (!the_color) {
    wxREGGLOB(the_color);
    the_color = new wxColour(0, 0, 0);
  }

  if (little) {
    step = 4;
    unstep = 2;
    dx = 2;
    dy = 2;
  } else {
    step = 1;
    unstep = 0;
    dx = 0;
    dy = 0;
  }

  dc->Clear();

  for (i = 0; i < 64; i += step) {
    for (j = 0; j < 64; j += step) {
      the_color->Set(alternate_icon[i * 64 * 3 + j * 3], 
		     alternate_icon[i * 64 * 3 + j * 3 + 1], 
		     alternate_icon[i * 64 * 3 + j * 3 + 2]);
      dc->SetPixel(dx + (i >> unstep), dy + (j >> unstep), the_color);
    }
  }

  dc->SelectObject(NULL);

  return bm;
}
