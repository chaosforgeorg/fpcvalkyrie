{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl}
{$ELSE}
  {$DEFINE extdecl := stdcall}
{$ENDIF}

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
//  GLcharARB   = Char;         PGLcharARB   = ^GLcharARB;   TGLcharARB   = GLcharARB;   PPGLchar    = ^PGLchar;
//  GLhandleARB = Cardinal;     PGLhandleARB = ^GLhandleARB; TGLhandleARB = GLhandleARB;
//  GLintptr    = PtrInt;       PGLintptr    = ^GLintptr;    TGLintptr    = GLintptr;
//  GLsizeiptr  = PtrInt;       PGLsizeiptr  = ^GLsizeiptr;  TGLsizeiptr  = GLsizeiptr;
//  GLchar      = Char;         PGLchar      = Pchar;        TGLchar      = GLchar;
//  GLint64     = Int64;        PGLint64     = ^GLint64;     TGLint64     = GLint64;
//  GLuint64    = QWord;        PGLuint64    = ^GLuint64;    TGLuint64    = GLuint64;

type
  // EXT_vertex_array
  PFNGLARRAYELEMENTEXTPROC             = procedure(i: GLint); extdecl;
  PFNGLDRAWARRAYSEXTPROC               = procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  PFNGLVERTEXPOINTEREXTPROC            = procedure(size: GLint; atype: GLenum; stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLNORMALPOINTEREXTPROC            = procedure(atype: GLenum; stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLCOLORPOINTEREXTPROC             = procedure(size: GLint; atype: GLenum; stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLINDEXPOINTEREXTPROC             = procedure(atype: GLenum; stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLTEXCOORDPOINTEREXTPROC          = procedure(size: GLint; atype: GLenum; stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLEDGEFLAGPOINTEREXTPROC          = procedure(stride, count: GLsizei; const pointer: PGLboolean); extdecl;
  PFNGLGETPOINTERVEXTPROC              = procedure(pname: GLenum; params: Pointer); extdecl;
  PFNGLARRAYELEMENTARRAYEXTPROC        = procedure(mode: GLenum; count: GLsizei; const pi: Pointer); extdecl;

  // WIN_swap_hint
  PFNGLADDSWAPHINTRECTWINPROC          = procedure(x, y: GLint; width, height: GLsizei); extdecl;

  // EXT_paletted_texture
  PFNGLCOLORTABLEEXTPROC               = procedure(target, internalFormat: GLenum; width: GLsizei; format, atype: GLenum; const data: Pointer); extdecl;
  PFNGLCOLORSUBTABLEEXTPROC            = procedure(target: GLenum; start, count: GLsizei; format, atype: GLenum; const data: Pointer); extdecl;
  PFNGLGETCOLORTABLEEXTPROC            = procedure(target, format, atype: GLenum; data: Pointer); extdecl;
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname: GLenum; params: PGLint); extdecl;
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname: GLenum; params: PGLfloat); extdecl;

