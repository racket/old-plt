
wxGLConfig::wxGLConfig()
  : wxObject(FALSE)
{
  doubleBuffered = 1;
  depth = 1;
}

wxGLConfig::~wxGLConfig()
{
}

wxGLConfig *wxGLConfig::Clone()
{
  wxGLConfig *c;
  c = new wxGLConfig();
  c->doubleBuffered = doubleBuffered;
  c->stereo = stereo;
  c->stencil = stencil;
  c->accum = accum;
  c->depth = depth;
  c->multisample = multisample;
  return c;
}

#if defined(USE_GL) || defined(wx_msw) || defined(wx_mac)
# define wxGLSIZEOF(n, t) sizeof(t)
#else
# define wxGLSIZEOF(n, t) n
#endif

int wxGLConfig::sizeof_GLbyte() { return wxGLSIZEOF(1, GLbyte); }

int wxGLConfig::sizeof_GLubyte() { return wxGLSIZEOF(1, GLubyte); }

int wxGLConfig::sizeof_GLshort() { return wxGLSIZEOF(2, GLshort); }

int wxGLConfig::sizeof_GLushort() { return wxGLSIZEOF(2, GLushort); }

int wxGLConfig::sizeof_GLint() { return wxGLSIZEOF(4, GLint); }

int wxGLConfig::sizeof_GLuint() { return wxGLSIZEOF(4, GLuint); }

int wxGLConfig::sizeof_GLfloat() { return wxGLSIZEOF(4, GLfloat); }

int wxGLConfig::sizeof_GLdouble() { return wxGLSIZEOF(8, GLdouble); }

int wxGLConfig::sizeof_GLboolean() { return wxGLSIZEOF(1, GLboolean); }

int wxGLConfig::sizeof_GLsizei() { return wxGLSIZEOF(2, GLsizei); }

int wxGLConfig::sizeof_GLclampf() { return wxGLSIZEOF(4, GLclampf); }

int wxGLConfig::sizeof_GLclampd() { return wxGLSIZEOF(8, GLclampd); }

int wxGLConfig::sizeof_GLenum() { return wxGLSIZEOF(4, GLenum); }

int wxGLConfig::sizeof_GLbitfield() { return wxGLSIZEOF(4, GLbitfield); }
