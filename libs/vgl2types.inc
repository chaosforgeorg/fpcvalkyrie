type
  // OpenGL 1.1
  GLenum     = Cardinal;      PGLenum      = ^GLenum;       TGLenum     = GLenum;
  GLboolean  = Byte;          PGLboolean   = ^GLboolean;    TGLboolean  = GLboolean;
  GLbitfield = Cardinal;      PGLbitfield  = ^GLbitfield;   TGLbitfield = GLbitfield;
  GLbyte     = ShortInt;      PGLbyte      = ^GLbyte;       TGLbyte     = GLbyte;
  GLshort    = SmallInt;      PGLshort     = ^GLshort;      TGLshort    = GLshort;
  GLint      = Integer;       PGLint       = ^GLint;        TGLint      = GLint;
  GLsizei    = Integer;       PGLsizei     = ^GLsizei;      TGLsizei    = GLsizei;
  GLubyte    = Byte;          PGLubyte     = ^GLubyte;      TGLubyte    = GLubyte;
  GLushort   = Word;          PGLushort    = ^GLushort;     TGLushort   = GLushort;
  GLuint     = Cardinal;      PGLuint      = ^GLuint;       TGLuint     = GLuint;
  GLfloat    = Single;        PGLfloat     = ^GLfloat;      TGLfloat    = GLfloat;
  GLclampf   = Single;        PGLclampf    = ^GLclampf;     TGLclampf   = GLclampf;
  GLdouble   = Double;        PGLdouble    = ^GLdouble;     TGLdouble   = GLdouble;
  GLclampd   = Double;        PGLclampd    = ^GLclampd;     TGLclampd   = GLclampd;
                              PGLvoid      = Pointer;
                              PPGLvoid     = ^PGLvoid;

  // OpenGL 1.2.1
  GLhandle    = Cardinal;     PGLhandle    = ^GLhandle;    TGLhandleARB = GLhandle;
  GLintptr    = PtrInt;       PGLintptr    = ^GLintptr;    TGLintptr    = GLintptr;
  GLsizeiptr  = PtrInt;       PGLsizeiptr  = ^GLsizeiptr;  TGLsizeiptr  = GLsizeiptr;
  GLchar      = Char;         PGLchar      = Pchar;        TGLchar      = GLchar;
  GLint64     = Int64;        PGLint64     = ^GLint64;     TGLint64     = GLint64;
  GLuint64    = QWord;        PGLuint64    = ^GLuint64;    TGLuint64    = GLuint64;

  PPGLint     = ^PGLint;
  PPGLchar    = ^PGLchar;
