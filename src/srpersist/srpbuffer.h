// srpbuffer.h

#define checkInterval(o,ty) \
  if (o->interval_type != ty) { \
    badInterval(ty,o->interval_type); \
  } 

void badInterval(int,int);


