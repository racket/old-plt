#include <stdio.h>
#include <sndfile.h>

#include "scheme.h"

#include "homo-all-vector-glue.c"

Scheme_Type sf_port_type;

typedef struct 
{
  Scheme_Type type;
  SNDFILE *port;
} SF_PORT;

Scheme_Object *make_sf_port(SNDFILE *ptr)
{
  SF_PORT* port;

  port = (SF_PORT *)scheme_malloc_atomic(sizeof(SF_PORT));
  port->type = sf_port_type;
  port->port = ptr;
  return (Scheme_Object *)port;
}

#define SF_PORT_PORT_VAL(obj) (((SF_PORT *)obj)->port)

#define SF_PORTP(obj)  SAME_TYPE(SCHEME_TYPE(obj), sf_port_type)

Scheme_Object *sf_portp(int argc, Scheme_Object **argv)
{
  if (SF_PORTP(argv[0]))
    return scheme_true;
  else
    return scheme_false;
}

int cast_scheme_number_to_int(Scheme_Object *num)
{
  long tmp;
  int result;

  if (! scheme_get_int_val(num,&tmp))
    scheme_signal_error ("unable to cast scheme number to int: %V", num);

  result = tmp;

  if (result != tmp) 
    scheme_signal_error("unable to cast scheme number to int: %V", num);

  return result;
}
 
Scheme_Object *open_abstraction(char *path, SF_INFO *info, int mode)
{
  SNDFILE *port;
  
  port = sf_open(path,mode,info);
  
  if (port == NULL) 
    scheme_signal_error("%s", sf_strerror(NULL));
  
  Scheme_Object *info_vec = scheme_make_vector(6,scheme_false);
  
  SCHEME_VEC_ELS(info_vec)[0] = scheme_make_integer_value(info->frames);
  SCHEME_VEC_ELS(info_vec)[1] = scheme_make_integer_value(info->samplerate);
  SCHEME_VEC_ELS(info_vec)[2] = scheme_make_integer_value(info->channels);
  SCHEME_VEC_ELS(info_vec)[3] = scheme_make_integer_value(info->format);
  SCHEME_VEC_ELS(info_vec)[4] = scheme_make_integer_value(info->sections);
  if (info->seekable) 
    SCHEME_VEC_ELS(info_vec)[5] = scheme_true;
  else
    SCHEME_VEC_ELS(info_vec)[5] = scheme_false;
 
 
  return scheme_make_pair(make_sf_port(port),
			  scheme_make_pair(info_vec, scheme_null));
}

Scheme_Object *open_for_read(int argc, Scheme_Object **argv)
{
  int mode;

  if (strcmp(SCHEME_SYM_VAL(argv[1]),"read") == 0)
    mode = SFM_READ;
  else if (strcmp(SCHEME_SYM_VAL(argv[1]),"readwrite") == 0)
    mode = SFM_RDWR;

  SF_INFO info;
  info.format = 0;

  return open_abstraction(SCHEME_STR_VAL(argv[0]),&info,mode);
}


Scheme_Object *open_for_write(int argc, Scheme_Object **argv)
{
   SF_INFO info;

  info.samplerate = cast_scheme_number_to_int(argv[1]);
  info.channels = cast_scheme_number_to_int(argv[2]);
  info.format =  cast_scheme_number_to_int(argv[3]);
  
  if (! sf_format_check(&info)) 
    scheme_signal_error("illegal format specified in open-for-write");
  
  return open_abstraction(SCHEME_STR_VAL(argv[0]),&info,SFM_WRITE);
}  

Scheme_Object *seek(int argc, Scheme_Object **argv)
{
  int frames = cast_scheme_number_to_int(argv[1]);
  int whence;
  char *sym = SCHEME_SYM_VAL(argv[2]);

  if (! strcmp("from-beginning",sym)) {
    whence = SEEK_SET;
  } else if (! strcmp("from-current",sym)) {
    whence = SEEK_CUR;
  } else if (! strcmp("from-end",sym)) {
    whence = SEEK_END;
  }

  if (sf_seek(SF_PORT_PORT_VAL(argv[0]),frames,whence) != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));

  return scheme_void;
}


/*

   [sf-command (-> sf-port? ; port
                   (symbols 'unknown) ; don't know what goes here...?
                   string? ; data
                   integer? ; datasize ... can use length of string?
                   void?)] ; result

*/

Scheme_Object *close_soundfile(int argc, Scheme_Object **argv)
{
  int result = sf_close(SF_PORT_PORT_VAL(argv[0]));

  if (result)
    scheme_signal_error("%s",sf_error_number(result));

  return scheme_void;
} 

Scheme_Object *read_short(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_readf_short(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_u16_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *read_int(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_readf_int(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_u32_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *read_float(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_readf_float(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_f32_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *read_double(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_readf_double(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_f64_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *write_short(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_writef_short(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_u16_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *write_int(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_writef_int(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_u32_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *write_float(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_writef_float(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_f32_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *write_double(int argc, Scheme_Object **argv)
{
  sf_count_t result;
  int offset = cast_scheme_number_to_int(argv[2]);
  int frames = cast_scheme_number_to_int(argv[3]);

  result = sf_writef_double(SF_PORT_PORT_VAL(argv[0]),
			  ((homo_f64_vector *)argv[1])->els + 
			  offset,
			  frames);

  if (result != frames)
    scheme_signal_error("%s",sf_strerror(SF_PORT_PORT_VAL(argv[0])));
  
  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object* funs[13];

  funs[0] = scheme_make_prim_w_arity(open_for_read,
				     "sf-open-for-read",
				     2, 2);
  funs[1] = scheme_make_prim_w_arity(open_for_write,
				     "sf-open-for-write",
				     4, 4);
  funs[2] = scheme_make_prim_w_arity(seek,
				     "sf-seek",
				     3, 3);
  funs[3] = scheme_make_prim_w_arity(close_soundfile,
				     "sf-close",
				     1, 1);
  funs[4] = scheme_make_prim_w_arity(read_short,
				     "sf-read",
				     4, 4);
  funs[5] = scheme_make_prim_w_arity(read_int,
				     "sf-read",
				     4, 4);
  funs[6] = scheme_make_prim_w_arity(read_float,
				     "sf-read",
				     4, 4);
  funs[7] = scheme_make_prim_w_arity(read_double,
				     "sf-read",
				     4, 4);
  funs[8] = scheme_make_prim_w_arity(write_short,
				     "sf-write",
				     4, 4);
  funs[9] = scheme_make_prim_w_arity(write_int,
				     "sf-write",
				     4, 4);
  funs[10] = scheme_make_prim_w_arity(write_float,
				     "sf-write",
				     4, 4);
  funs[11] = scheme_make_prim_w_arity(write_double,
				     "sf-write",
				     4, 4);
  funs[12] = scheme_make_prim_w_arity(sf_portp,
				      "sf-port?",
				      1, 1);

  return scheme_values(13, funs);
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  
  sf_port_type = scheme_make_type("sf-port");
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_void;
}
