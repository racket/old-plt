
#include <stdlib.h>

typedef struct {
  unsigned short red_in, green_in, blue_in;
  unsigned short red_out, green_out, blue_out;
  unsigned long pixel;
  int weight;
} ColorCache;

#define COLOR_CACHE_SIZE 100

static ColorCache cache[COLOR_CACHE_SIZE];

long alloc_size;
long alloc_count;
unsigned long *alloced;

extern Screen *wxAPP_SCREEN;

#define OK 1

Status wxAllocColor(Display *d, Colormap cm, XColor *c)
{
  int i;
  int min_weight;
  int min_weight_pos;
  unsigned short ri, gi, bi;
  int p, w, o;
  unsigned long pixel;
  Status status;

  if (cm != DefaultColormapOfScreen(wxAPP_SCREEN)) {
    return XAllocColor(d, cm, c);
  }

  /* Check for black: */
  if (!c->red && !c->green && !c->blue) {
    c->pixel = BlackPixelOfScreen(wxAPP_SCREEN);
    return OK;
  }
  
  /* Check for white: */
  if ((c->red >= 0xFF00) && (c->green >= 0xFF00) && (c->blue >= 0xFF00)) {
    c->pixel = WhitePixelOfScreen(wxAPP_SCREEN);
    c->red = 0xFFFF;
    c->green = 0xFFFF;
    c->blue = 0xFFFF;
    return OK;
  }

  /* Check in cache: */ 
  min_weight_pos = 0;
  min_weight = cache[0].weight;
  for (i = 0; i < COLOR_CACHE_SIZE; i++) {
    if (cache[i].red_in == c->red
	&& cache[i].green_in == c->green
	&& cache[i].blue_in == c->blue) {
      c->red = cache[i].red_out;
      c->green = cache[i].green_out;
      c->blue = cache[i].blue_out;
      c->pixel = cache[i].pixel;

      if (cache[i].weight < 10000)
	cache[i].weight++;

      return OK;
    } else if (cache[i].weight < min_weight) {
      min_weight = cache[i].weight;
      min_weight_pos = i;
    }
  }

  /* Degrade weights: */
  if (cache[COLOR_CACHE_SIZE - 1].pixel) {
    for (i = 0; i < COLOR_CACHE_SIZE; i++) 
      if (cache[i].weight)
	--cache[i].weight;
  }

  ri = c->red;
  gi = c->green;
  bi = c->blue;

  status = XAllocColor(d, cm, c);

  if (status == OK) {
    /* Add to cache: */
    cache[min_weight_pos].red_in = ri;
    cache[min_weight_pos].green_in = gi;
    cache[min_weight_pos].blue_in = bi;
    cache[min_weight_pos].red_out = c->red;
    cache[min_weight_pos].green_out = c->green;
    cache[min_weight_pos].blue_out = c->blue;
    cache[min_weight_pos].pixel = c->pixel;
    cache[min_weight_pos].weight = 10;

    /* Record allocation */

    /* Binary search for pixel: */
    pixel = c->pixel;
    if (alloc_count) {
      o = 0;
      p = alloc_count >> 1;
      w = alloc_count;
      
      while (1) {
	unsigned long v;
	
	v = alloced[p];
	
	if (v == pixel) {
	  /* Balance redundant Alloc with Free: */
	  XFreeColors(d, cm, &pixel, 1, 0);
	  return OK;
	}
	if (w == 1) {
	  if (v < pixel)
	    p++;
	  break;
	}
	if (v < pixel) {
	  w = o + w - p;
	  o = p;
	} else {
	  w = p - o;
	}
	p = o + (w >> 1);
      }
    } else
      p = 0;

    /* Not alloced before. */
    /* First make sure array is large enough: */
    if (alloc_count == alloc_size) {
      unsigned long *old = alloced;

      if (!alloc_size)
	alloc_size = 256;
      else
	alloc_size = alloc_size * 2;
      
      alloced = (unsigned long *)malloc(sizeof(unsigned long) * alloc_size);
      for (i = 0; i < alloc_count; i++)
	alloced[i] = old[i];
      free(old);
    }
    
    for (i = alloc_count; i-- > p; )
      alloced[i + 1] = alloced[i];
    alloced[p] = pixel;
    alloc_count++;
    
    return OK;
  } else
    return status;
}
