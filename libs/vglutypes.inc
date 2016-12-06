{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl}
{$ELSE}
  {$DEFINE extdecl := stdcall}
{$ENDIF}

type
  TViewPortArray = array [0..3] of GLint;
  T16dArray      = array [0..15] of GLdouble;
  TCallBack      = procedure;
  T3dArray       = array [0..2] of GLdouble;
  T4pArray       = array [0..3] of Pointer;
  T4fArray       = array [0..3] of GLfloat;
  PPointer       = ^Pointer;

type
  GLUnurbs      = record end; PGLUnurbs      = ^GLUnurbs;      TGLUnurbs      = GLUnurbs;
  GLUquadric    = record end; PGLUquadric    = ^GLUquadric;    TGLUquadric    = GLUquadric;
  GLUtesselator = record end; PGLUtesselator = ^GLUtesselator; TGLUtesselator = GLUtesselator;

type
  // gluQuadricCallback
  GLUquadricErrorProc     = procedure(p: GLenum); extdecl;

  // gluTessCallback
  GLUtessBeginProc        = procedure(p: GLenum); extdecl;
  GLUtessEdgeFlagProc     = procedure(p: GLboolean); extdecl;
  GLUtessVertexProc       = procedure(p: Pointer); extdecl;
  GLUtessEndProc          = procedure; extdecl;
  GLUtessErrorProc        = procedure(p: GLenum); extdecl;
  GLUtessCombineProc      = procedure(var p1: T3dArray; p2: T4pArray; p3: T4fArray; p4: PPointer); extdecl;
  GLUtessBeginDataProc    = procedure(p1: GLenum; p2: Pointer); extdecl;
  GLUtessEdgeFlagDataProc = procedure(p1: GLboolean; p2: Pointer); extdecl;
  GLUtessVertexDataProc   = procedure(p1, p2: Pointer); extdecl;
  GLUtessEndDataProc      = procedure(p: Pointer); extdecl;
  GLUtessErrorDataProc    = procedure(p1: GLenum; p2: Pointer); extdecl;
  GLUtessCombineDataProc  = procedure(var p1: T3dArray; var p2: T4pArray; var p3: T4fArray; p4: PPointer; p5: Pointer); extdecl;

  // gluNurbsCallback
  GLUnurbsErrorProc       = procedure(p: GLenum); extdecl;





