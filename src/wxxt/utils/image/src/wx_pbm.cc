#if 0

/*
 * xvpbm.c - load routine for 'pm' format pictures
 *
 * LoadPBM(fname, numcols)  -  loads a PBM, PGM, or PPM file
 * WritePBM(fp,pic,w,h,r,g,b,numcols,style,raw,cmt)
 */

/*
 * Copyright 1989, 1990 by the University of Pennsylvania
 *
 * Permission to use, copy, and distribute for non-commercial purposes,
 * is hereby granted without fee, providing that the above copyright
 * notice appear in all copies and that both the copyright notice and this
 * permission notice appear in supporting documentation.
 *
 * The software may be modified for your own purposes, but modified versions
 * may not be distributed.
 *
 * This software is provided "as is" without any express or implied warranty.
 */

#include <stdlib.h>
#include "wx_image.h"



/* comments on error handling:
   a truncated file is not considered a Major Error.  The file is loaded, the
   rest of the pic is filled with 0's.

   a file with garbage characters in it is an unloadable file.  All allocated
   stuff is tossed, and LoadPBM returns non-zero

   not being able to malloc is a Fatal Error.  The program is aborted. */


#define TRUNCSTR "File appears to be truncated."

static int garbage;
static long numgot, filesize;

static int getint(FILE *);
static int getbit(FILE *);


/*******************************************/
int wxImage::LoadPBM(char *fname, int nc)
/*******************************************/
{
  FILE  *fp;
  int    c, c1;
  int    w, h, maxv, rv;

  garbage = maxv = 0;

  /* open the stream, if necesary */
  fp=fopen(fname,"r");
  if (!fp) return 1;
  
  /* figure out the file size (for Informational Purposes Only) */
  fseek(fp, 0L, 2);
  filesize = ftell(fp);
  fseek(fp, 0L, 0);

  /* read the first two bytes of the file to determine which format
     this file is.  "P1" = ascii bitmap, "P2" = ascii greymap,
     "P3" = ascii pixmap, "P4" = raw bitmap, "P5" = raw greymap,
     "P6" = raw pixmap */

  c = getc(fp);  c1 = getc(fp);
  if (c!='P' || c1<'1' || c1>'6') return 1;

  /* read in header information */
  w = getint(fp);  h = getint(fp);

  /* if we're not reading a bitmap, read the 'max value' */
  if ( !(c1=='1' || c1=='4')) {
    maxv = getint(fp);
    if (maxv < 1) garbage=1;    /* to avoid 'div by zero' probs */
  }


  if (garbage) {
    if (fp!=stdin) fclose(fp);
    return 1;
  }

  rv = 0;

  /* call the appropriate subroutine to handle format-specific stuff */
  if      (c1=='1' || c1=='4') rv = loadpbm(fp,w,h,       c1=='4' ? 1 : 0);
  else if (c1=='2' || c1=='5') rv = loadpgm(fp,w,h, maxv, c1=='5' ? 1 : 0);
  else if (c1=='3' || c1=='6') rv = loadppm(fp,w,h, maxv, c1=='6' ? 1 : 0, nc);

  if (fp!=stdin) fclose(fp);
  return(rv);
}  



/*******************************************/
int wxImage::loadpbm(FILE *fp, int w, int h, int raw)
{
  byte *pix;
  int   i,j,bit;


/*
  SetISTR(ISTR_FORMAT,"PBM, %s format.  (%ld bytes)", 
	  (raw) ? "raw" : "ascii", filesize);
*/

  /* load up the XV global variables */
  pic = (byte *) calloc(w*h,1);
  if (!pic) FatalError("couldn't malloc 'pic'");

  pWIDE = w;  pHIGH = h;

  /* B/W bitmaps have a two entry colormap */
  r[0] = g[0] = b[0] = 255;   /* 0 = white */
  r[1] = g[1] = b[1] = 0;     /* 1 = black */

  if (!raw) {
    numgot = 0;
    for (i=0, pix=pic; i<h; i++)
      for (j=0; j<w; j++, pix++)
	*pix = getbit(fp);

    if (numgot != w*h) return 1;
    if (garbage) {
      free(pic);
      return 1;
//      return(PBMerr("Garbage characters in image data."));
    }
  }


  else {   /* read raw bits */
    int trunc = 0, k = 0;

    for (i=0, pix=pic; i<h; i++)
      for (j=0,bit=0; j<w; j++, pix++, bit++) {

	bit &= 7;
	if (!bit) {
	  k = getc(fp);
	  if (k==EOF) { trunc=1; k=0; }
	}

	*pix = (k&0x80) ? 1 : 0;
	k = k << 1;
      }

//    if (trunc) PBMerr(TRUNCSTR);
  }

  return 0;
}


/*******************************************/
int wxImage::loadpgm(FILE *fp, int w, int h, int maxv, int raw)
{
  byte *pix;
  int   i,j,bitshift;

/*
  SetISTR(ISTR_FORMAT,"PGM, %s format.  (%ld bytes)", 
	  (raw) ? "raw" : "ascii", filesize);
*/

  /* load up the XV global variables */
  pic = (byte *) calloc(w*h,1);
  if (!pic) FatalError("couldn't malloc 'pic'");

  pWIDE = w;  pHIGH = h;

  /* if maxv>255, keep dropping bits until it's reasonable */
  bitshift = 0;
  while (maxv>255) { maxv = maxv>>1;  bitshift++; }

  /* fill in a greyscale colormap where maxv maps to 255 */
  for (i=0; i<=maxv; i++)
    r[i] = g[i] = b[i] = (i*255)/maxv;

  if (!raw) {
    numgot = 0;
    for (i=0, pix=pic; i<h; i++)
      for (j=0; j<w; j++, pix++)
	*pix = (getint(fp) >> bitshift);
  }

  else numgot = fread(pic, 1, w*h, fp);   /* read raw data */

//  if (numgot != w*h) PBMerr(TRUNCSTR);

  if (garbage) {
    free(pic);
    return 1;
//    return (PBMerr("Garbage characters in image data."));
  }

  return 0;
}


/*******************************************/
int wxImage::loadppm(FILE *fp, int w, int h, int maxv, int raw, int nc)
{
  byte *pix, *pic24, scale[256];
  int   i,j,bitshift;

/*
  SetISTR(ISTR_FORMAT,"PPM, %s format.  (%ld bytes)", 
	  (raw) ? "raw" : "ascii", filesize);
*/

  /* allocate 24-bit image */
  pic24 = (byte *) calloc(w*h*3,1);
  if (!pic24) FatalError("couldn't malloc 'pic24'");

  /* if maxv>255, keep dropping bits until it's reasonable */
  bitshift = 0;
  while (maxv>255) { maxv = maxv>>1;  bitshift++; }

  if (!raw) {
    numgot = 0;
    for (i=0, pix=pic24; i<h; i++)
      for (j=0; j<w*3; j++, pix++)
	*pix = (getint(fp) >> bitshift);
  }

  else numgot = fread(pic24, 1, w*h*3, fp);    /* read raw data */

//  if (numgot != w*h*3) PBMerr(TRUNCSTR);

  if (garbage) {
    free(pic24);
    return 1;
//    return(PBMerr("Garbage characters in image data."));
  }

  /* have to scale all RGB values up (Conv24to8 expects RGB values to
     range from 0-255 */

  if (maxv<255) { 
    for (i=0; i<=maxv; i++) scale[i] = (i * 255) / maxv;

    for (i=0, pix=pic24; i<h; i++) 
      for (j=0; j<w*3; j++, pix++) 
	*pix = scale[*pix];
  }

  i = Conv24to8(pic24,w,h,nc);
  free(pic24);
  return i;
}



/*******************************************/
static int getint(FILE *fp)
{
  int c, i;

  /* skip forward to start of next number */
  c = getc(fp);
  while (1) {
    /* eat comments */
    if (c=='#') {   /* if we're at a comment, read to end of line */
      while (c != '\n' && c != EOF) c=getc(fp);
    }

    if (c==EOF) return 0;
    if (c>='0' && c<='9') break;   /* we've found what we were looking for */

    /* see if we are getting garbage (non-whitespace) */
    if (c!=' ' && c!='\t' && c!='\r' && c!='\n' && c!=',') garbage=1;

    c = getc(fp);
  }


  /* we're at the start of a number, continue until we hit a non-number */
  i = 0;
  while (1) {
    i = (i*10) + (c - '0');
    c = getc(fp);
    if (c==EOF) return i;
    if (c<'0' || c>'9') break;
  }

  numgot++;
  return i;
}



/*******************************************/
static int getbit(FILE *fp)
{
  int c;

  /* skip forward to start of next number */
  c = getc(fp);
  while (1) {
    /* eat comments */
    if (c=='#') {   /* if we're at a comment, read to end of line */
      while (c != '\n' && c != EOF) c=getc(fp);
    }

    if (c==EOF) return 0;
    if (c=='0' || c=='1') break;   /* we've found what we were looking for */

    /* see if we are getting garbage (non-whitespace) */
    if (c!=' ' && c!='\t' && c!='\r' && c!='\n' && c!=',') garbage=1;

    c = getc(fp);
  }


  numgot++;
  return(c-'0');
}


/*******************************************/

/*
static int PBMerr(st)
char *st;
{
  SetISTR(ISTR_WARNING,st);
  return 1;
}
*/




/*******************************************/
int wxImage::WritePBM(FILE *fp,byte *pic,int w,int h,byte *rmap,byte *gmap,byte *bmap,int numcols,int colorstyle,int raw)
{
  /* writes a PBM/PGM/PPM file to the already open stream
     if (raw), writes as RAW bytes, otherwise writes as ASCII 
     'colorstyle' single-handedly determines the type of file written
     if colorstyle==0, (Full Color) a PPM file is written
     if colorstyle==1, (Greyscale)  a PGM file is written
     if colorstyle==2, (B/W stipple) a PBM file is written */

  int   magic;
  byte *pix;
  int   i,j,len;

  /* calc the appropriate magic number for this file type */
  magic = 0;
  if      (colorstyle==0) magic = 3;
  else if (colorstyle==1) magic = 2;
  else if (colorstyle==2) magic = 1;

  if (raw && magic) magic+=3;

//  if (!magic) return(PBMerr("WritePBM: unknown file format"));
  if (!magic) return 1;

  /* write the header info */
  fprintf(fp,"P%d\n",magic);
  fprintf(fp,"# created by wxImage\n");
  fprintf(fp,"%d %d\n",w,h);
  if (colorstyle!=2) fprintf(fp,"255\n");

  if (ferror(fp)) return -1;

  /* write the image data */

  if (colorstyle==0) {                  /* 24bit RGB, 3 bytes per pixel */
    for (i=0, pix=pic, len=0; i<h; i++)
      for (j=0; j<w; j++,pix++) {
	if (raw) {
	  putc(r[*pix],fp);  
	  putc(g[*pix],fp);  
	  putc(b[*pix],fp);
	}
	else {
	  fprintf(fp,"%3d %3d %3d ",r[*pix], g[*pix], b[*pix]);
	  len+=12;
	  if (len>58) { fprintf(fp,"\n");  len=0; }
	}
      }
  }

  else if (colorstyle==1) {             /* 8-bit greyscale */
    byte rgb[256];
    for (i=0; i<numcols; i++) rgb[i] = MONO(rmap[i],gmap[i],bmap[i]);
    for (i=0, pix=pic, len=0; i<w*h; i++,pix++) {
      if (raw) putc(rgb[*pix],fp);
      else {
	fprintf(fp,"%3d ",rgb[*pix]);
	len += 4;
	if (len>66) { fprintf(fp,"\n");  len=0; }
      }
    }
  }

  else if (colorstyle==2) {             /* 1-bit B/W stipple */
    int bit,k;
    for (i=0, pix=pic, len=0; i<h; i++) {
      for (j=0, bit=0, k=0; j<w; j++, pix++) {
	if (raw) {
	  k = (k << 1) | *pix;
	  bit++;
	  if (bit==8) {
	    fputc(~k,fp);
	    bit = k = 0;
	  }
	}
	else {
	  if (*pix) fprintf(fp,"0 ");
	       else fprintf(fp,"1 ");
	  len+=2;
	  if (len>68) { fprintf(fp,"\n"); len=0; }
	}
      } /* j */
      if (raw && bit) {
	k = k << (8-bit);
	fputc(~k,fp);
      }
    }
  }

  if (ferror(fp)) return -1;

  return 0;
}

#endif
