
class wxGLConfig : public wxObject {
 public:
  Bool doubleBuffered, stereo;
  int stencil;
  int accum;
  int depth;
  int multisample;
  
  wxGLConfig();
  ~wxGLConfig();

  wxGLConfig *Clone();
  
  int sizeof_GLbyte();
  int sizeof_GLubyte();
  int sizeof_GLshort();
  int sizeof_GLushort();
  int sizeof_GLint();
  int sizeof_GLuint();
  int sizeof_GLfloat();
  int sizeof_GLdouble();
  int sizeof_GLboolean();
  int sizeof_GLsizei();
  int sizeof_GLclampf();
  int sizeof_GLclampd();
  int sizeof_GLenum();
  int sizeof_GLbitfield();
};
