#if 0

/*
 * xvpcx.c - load routine for PCX format pictures
 *
 * LoadPCX(fname, pinfo)  -  loads a PCX file
 */

/* Copyright Notice
 * ================
 * Copyright 1989, 1990, 1991, 1992, 1993 by John Bradley
 * 
 * Permission to use, copy, and distribute XV in its entirety, for 
 * non-commercial purposes, is hereby granted without fee, provided that
 * this license information and copyright notice appear in all copies.
 * 
 * Note that distributing XV 'bundled' in with ANY product is considered
 * to be a 'commercial purpose'.
 *
 * Also note that any copies of XV that are distributed MUST be built
 * and/or configured to be in their 'unregistered copy' mode, so that it
 * is made obvious to the user that XV is shareware, and that they should
 * consider donating, or at least reading this License Info.
 * 
 * The software may be modified for your own purposes, but modified
 * versions may NOT be distributed without prior consent of the author.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the author be held liable for any damages
 * arising from the use of this software.
 * 
 * If you would like to do something with XV that this copyright
 * prohibits (such as distributing it with a commercial product, 
 * using portions of the source in some other program, etc.), please
 * contact the author (preferably via email).  Arrangements can
 * probably be worked out.
 *
 * XV is shareware for PERSONAL USE only.  You may use XV for your own
 * amusement, and if you find it nifty, useful, generally cool, or of
 * some value to you, your non-deductable donation would be greatly
 * appreciated.  $25 is the suggested donation, though, of course,
 * larger donations are quite welcome.  Folks who donate $25 or more
 * can receive a Real Nice bound copy of the XV manual for no extra
 * charge.
 * 
 * Commercial, government, and institutional users MUST register their
 * copies of XV, for the exceedingly REASONABLE price of just $25 per
 * workstation/X terminal.  Site licenses are available for those who
 * wish to run XV on a large number of machines.  Contact the author
 * for more details.
 *
 * The author may be contacted via:
 *    US Mail:  John Bradley
 *              1053 Floyd Terrace
 *              Bryn Mawr, PA  19010
 *
 *    Phone:    (215) 898-8813
 *    EMail:    bradley@cis.upenn.edu
 */

/*
 * the following code has been derived from code written by
 *  Eckhard Rueggeberg  (Eckhard.Rueggeberg@ts.go.dlr.de)
 */


#include <stdlib.h>
#include "wx_image.h"

/* offsets into PCX header */
#define PCX_ID      0
#define PCX_VER     1
#define PCX_ENC     2
#define PCX_BPP     3
#define PCX_XMINL   4
#define PCX_XMINH   5
#define PCX_YMINL   6
#define PCX_YMINH   7
#define PCX_XMAXL   8
#define PCX_XMAXH   9
#define PCX_YMAXL   10
#define PCX_YMAXH   11
                          /* hres (12,13) and vres (14,15) not used */
#define PCX_CMAP    16    /* start of 16*3 colormap data */
#define PCX_PLANES  65 
#define PCX_BPRL    66
#define PCX_BPRH    67

#define PCX_MAPSTART 0x0c	/* Start of appended colormap	*/


static int  pcxLoadImage(char *, FILE *, byte *, byte *, int, int);
static void pcxLoadRaster(FILE *, byte *, int, byte *, int, int);
static int  pcxError(char *, char *);

/*******************************************/
int wxImage::LoadPCX(char *fname, PICINFO *pinfo)
/*******************************************/
{
  FILE  *fp;
  long   filesize;
  byte   hdr[128], *image;
  int    i, colors, gray;

  pinfo->type = PIC8;
  pinfo->pic     = (byte *) NULL;
  pinfo->comment = (char *) NULL;

  /* open the stream */
  fp=fopen(fname,"r");
  if (!fp) return (pcxError(fname, "unable to open file"));
  

  /* figure out the file size */
  fseek(fp, 0L, 2);
  filesize = ftell(fp);
  fseek(fp, 0L, 0);


  /* read the PCX header */
  fread(hdr, 128, 1, fp);
  if (ferror(fp)) {
    fclose(fp);
    return pcxError(fname, "EOF reached in PCX header.\n");
  }

  if (hdr[PCX_ID] != 0x0a || hdr[PCX_VER] > 5) {
    fclose(fp);
    return pcxError(fname,"unrecognized magic number");
  }

  pinfo->w = (hdr[PCX_XMAXL] + ((int) hdr[PCX_XMAXH]<<8)) 
           - (hdr[PCX_XMINL] + ((int) hdr[PCX_XMINH]<<8));

  pinfo->h = (hdr[PCX_YMAXL] + ((int) hdr[PCX_YMAXH]<<8)) 
           - (hdr[PCX_YMINL] + ((int) hdr[PCX_YMINH]<<8));

  pinfo->w++;  pinfo->h++;

  colors = 1 << (hdr[PCX_BPP] * hdr[PCX_PLANES]);

  if (colors>256) {
    fclose(fp);
    return pcxError(fname,"No more than 256 colors allowed in PCX file.");
  }

  if (hdr[PCX_ENC] != 1) {
    fclose(fp);
    return pcxError(fname,"Unsupported PCX encoding format.");
  }

  /* note:  overallocation to make life easier... */
  image = (byte *) malloc((pinfo->h + 1) * pinfo->w + 16);
  if (!image) FatalError("Can't alloc 'image' in LoadPCX()");

  xvbzero((char *) image, (pinfo->h+1) * pinfo->w + 16);   /* must clear img */

  if (!pcxLoadImage(fname, fp, image, hdr, pinfo->w, pinfo->h)) {
    free(image);
    fclose(fp);
    return 0;
  }


  if (ferror(fp))    /* just a warning */
    pcxError(fname, "PCX file appears to be truncated.");

  if (colors>16) {       /* handle trailing colormap */
    while (1) {
      i=getc(fp);
      if (i==PCX_MAPSTART || i==EOF) break;
    }

    for (i=0; i<colors; i++) {
      pinfo->r[i] = getc(fp);
      pinfo->g[i] = getc(fp);
      pinfo->b[i] = getc(fp);
      // JACS code to fit in with old xv
      r[i] = rorg[i] = pinfo->r[i];
      b[i] = borg[i] = pinfo->b[i];
      g[i] = gorg[i] = pinfo->g[i];

    }

    if (ferror(fp)) {
      pcxError(fname,"Error reading PCX colormap.  Using grayscale.");
      for (i=0; i<256; i++) pinfo->r[i] = pinfo->g[i] = pinfo->b[i] = i;
    }
  }
  else if (colors<=16) {   /* internal colormap */
    for (i=0; i<colors; i++) {
      pinfo->r[i] = hdr[PCX_CMAP + i*3];
      pinfo->g[i] = hdr[PCX_CMAP + i*3 + 1];
      pinfo->b[i] = hdr[PCX_CMAP + i*3 + 2];
      // JACS code to fit in with old xv
      r[i] = rorg[i] = pinfo->r[i];
      b[i] = borg[i] = pinfo->b[i];
      g[i] = gorg[i] = pinfo->g[i];

    }
  }

  if (colors == 2) {    /* b&w */
    if (MONO(pinfo->r[0],pinfo->g[0],pinfo->b[0]) ==
	MONO(pinfo->r[1],pinfo->g[1],pinfo->b[1])) {    /* create cmap */
      pinfo->r[0] = pinfo->g[0] = pinfo->b[0] = 255;
      pinfo->r[1] = pinfo->g[1] = pinfo->b[1] = 0;

      // JACS code to fit in with old xv
      r[0] = g[0] = b[0] = rorg[0] = gorg[0] = borg[0] = pinfo->r[0];
      r[1] = g[1] = b[1] = rorg[1] = gorg[1] = borg[1] = pinfo->r[1];

    }
  }


  fclose(fp);


  /* finally, convert into XV internal format */


  pinfo->pic     = image;
  pinfo->type    = PIC8;
  pinfo->frmType = -1;    /* no default format to save in */

  /* check for grayscaleitude */
  for (i=0; i<colors; i++) {
    if ((pinfo->r[i] != pinfo->g[i]) || (pinfo->r[i] != pinfo->b[i])) break;
  }
  gray = (i==colors) ? 1 : 0;


  if (colors > 2 || (colors==2 && !gray)) {  /* grayscale or PseudoColor */
    pinfo->colType = (gray) ? F_GREYSCALE : F_FULLCOLOR;
    sprintf(pinfo->fullInfo, 
	    "%s PCX, %d plane%s, %d bit%s per pixel.  (%ld bytes)", 
	    (gray) ? "Greyscale" : "Color", 
	    hdr[PCX_PLANES], (hdr[PCX_PLANES]==1) ? "" : "s",
	    hdr[PCX_BPP],    (hdr[PCX_BPP]==1) ? "" : "s",
	    filesize);
  }
  else {
    pinfo->colType = F_BWDITHER;
    sprintf(pinfo->fullInfo, "B&W PCX.  (%ld bytes)", filesize);
  }

//  sprintf(pinfo->shrtInfo, "%dx%d PCX.", pinfo->w, pinfo->h);
//  cout << pinfo->fullInfo << "\n";

  return 1;
}



/*****************************/
static int pcxLoadImage(char *fname, FILE *fp, byte *image, byte *hdr, int w, int h)
{
  switch (hdr[PCX_BPP]) {
  case 1:   pcxLoadRaster(fp, image, 1, hdr, w, h);   break;
  case 8:   pcxLoadRaster(fp, image, 8, hdr, w, h);   break;
  default:
    pcxError(fname, "Unsupported # of bits per plane.");
    return (0);
  }

  return 1;
}


/*****************************/
static void pcxLoadRaster(FILE *fp, byte *image, int depth, byte *hdr, int w, int h)
{
  /* supported:  8 bits per pixel, 1 plane, or 1 bit per pixel, 1-8 planes */

  int row, bcnt, bperlin, pad;
  int i, j, b, cnt, mask, plane, pmask;
  byte *oldimage;

  bperlin = hdr[PCX_BPRL] + ((int) hdr[PCX_BPRH]<<8);
  if (depth == 1) pad = (bperlin * 8) - w;
             else pad = bperlin - w;

  row = bcnt = 0;

  plane = 0;  pmask = 1;  oldimage = image;

  while ( (b=getc(fp)) != EOF) {
    if ((b & 0xC0) == 0xC0) {   /* have a rep. count */
      cnt = b & 0x3F;
      b = getc(fp);
      if (b == EOF) { getc(fp); return; }
    }
    else cnt = 1;
    
    for (i=0; i<cnt; i++) {
      if (depth == 1) {
	for (j=0, mask=0x80; j<8; j++) {
	  *image++ |= ((b & mask) ? pmask : 0);
	  mask = mask >> 1;
	}
      }
      else *image++ = (byte) b;
      
      bcnt++;
	
      if (bcnt == bperlin) {     /* end of a line reached */
	bcnt = 0;
	plane++;  

	if (plane>=hdr[PCX_PLANES]) {   /* moved to next row */
	  plane = 0;
	  image -= pad;
	  oldimage = image;
	  row++;
	  if (row >= h) return;   /* done */
	}
	else {   /* next plane, same row */
	  image = oldimage;
	}	

	pmask = 1 << plane;
      }
    }
  }
}    



/*******************************************/
static int pcxError(char *fname, char *st)
{
  fprintf(stderr, "wxImage, %s: %s\n", fname, st);
  return 0;
}

#endif
