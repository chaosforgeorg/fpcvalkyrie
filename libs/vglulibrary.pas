unit vglulibrary;
{$include ../src/valkyrie.inc}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary, vgllibrary;

const
{$IFDEF WINDOWS}
  GLUDefaultPath = 'glu32.dll';
{$ELSE}
  {$IFDEF DARWIN}
  GLUDefaultPath = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib';
  {$ELSE}
  GLUDefaultPath = 'libGLU.so.1';
  {$ENDIF}
{$ENDIF}

{$include vglutypes.inc}
{$include vgluconst.inc}

{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl}
{$ELSE}
  {$DEFINE extdecl := stdcall}
{$ENDIF}

var
  gluErrorString: function(errCode: GLenum): PChar; extdecl;
  gluErrorUnicodeStringEXT: function(errCode: GLenum): PWideChar; extdecl;
  gluGetString: function(name: GLenum): PChar; extdecl;
  gluOrtho2D: procedure(left,right, bottom, top: GLdouble); extdecl;
  gluPerspective: procedure(fovy, aspect, zNear, zFar: GLdouble); extdecl;
  gluPickMatrix: procedure(x, y, width, height: GLdouble; var viewport: TViewPortArray); extdecl;
  gluLookAt: procedure(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: GLdouble); extdecl;
  gluProject: function(objx, objy, objz: GLdouble; var modelMatrix, projMatrix: T16dArray; var viewport: TViewPortArray; winx, winy, winz: PGLdouble): Integer; extdecl;
  gluUnProject: function(winx, winy, winz: GLdouble; var modelMatrix, projMatrix: T16dArray; var viewport: TViewPortArray; objx, objy, objz: PGLdouble): Integer; extdecl;
  gluScaleImage: function(format: GLenum; widthin, heightin: GLint; typein: GLenum; const datain: Pointer; widthout, heightout: GLint; typeout: GLenum; dataout: Pointer): Integer; extdecl;
  gluBuild1DMipmaps: function(target: GLenum; components, width: GLint; format, atype: GLenum; const data: Pointer): Integer; extdecl;
  gluBuild2DMipmaps: function(target: GLenum; components, width, height: GLint; format, atype: GLenum; const data: Pointer): Integer; extdecl;

  gluNewQuadric: function: PGLUquadric; extdecl;
  gluDeleteQuadric: procedure(state: PGLUquadric); extdecl;
  gluQuadricNormals: procedure(quadObject: PGLUquadric; normals: GLenum); extdecl;
  gluQuadricTexture: procedure(quadObject: PGLUquadric; textureCoords: GLboolean); extdecl;
  gluQuadricOrientation: procedure(quadObject: PGLUquadric; orientation: GLenum); extdecl;
  gluQuadricDrawStyle: procedure(quadObject: PGLUquadric; drawStyle: GLenum); extdecl;
  gluCylinder: procedure(qobj: PGLUquadric; baseRadius, topRadius, height: GLdouble; slices, stacks: GLint); extdecl;
  gluDisk: procedure(qobj: PGLUquadric; innerRadius, outerRadius: GLdouble; slices, loops: GLint); extdecl;
  gluPartialDisk: procedure(qobj: PGLUquadric; innerRadius, outerRadius: GLdouble; slices, loops: GLint; startAngle, sweepAngle: GLdouble); extdecl;
  gluSphere: procedure(qobj: PGLuquadric; radius: GLdouble; slices, stacks: GLint); extdecl;
  gluQuadricCallback: procedure(qobj: PGLUquadric; which: GLenum; fn: TCallBack); extdecl;
  gluNewTess: function: PGLUtesselator; extdecl;
  gluDeleteTess: procedure(tess: PGLUtesselator); extdecl;
  gluTessBeginPolygon: procedure(tess: PGLUtesselator; polygon_data: Pointer); extdecl;
  gluTessBeginContour: procedure(tess: PGLUtesselator); extdecl;
  gluTessVertex: procedure(tess: PGLUtesselator; var coords: T3dArray; data: Pointer); extdecl;
  gluTessEndContour: procedure(tess: PGLUtesselator); extdecl;
  gluTessEndPolygon: procedure(tess: PGLUtesselator); extdecl;
  gluTessProperty: procedure(tess: PGLUtesselator; which: GLenum; value: GLdouble); extdecl;
  gluTessNormal: procedure(tess: PGLUtesselator; x, y, z: GLdouble); extdecl;
  gluTessCallback: procedure(tess: PGLUtesselator; which: GLenum;fn: TCallBack); extdecl;
  gluGetTessProperty: procedure(tess: PGLUtesselator; which: GLenum; value: PGLdouble); extdecl;
  gluNewNurbsRenderer: function: PGLUnurbs; extdecl;
  gluDeleteNurbsRenderer: procedure(nobj: PGLUnurbs); extdecl;
  gluBeginSurface: procedure(nobj: PGLUnurbs); extdecl;
  gluBeginCurve: procedure(nobj: PGLUnurbs); extdecl;
  gluEndCurve: procedure(nobj: PGLUnurbs); extdecl;
  gluEndSurface: procedure(nobj: PGLUnurbs); extdecl;
  gluBeginTrim: procedure(nobj: PGLUnurbs); extdecl;
  gluEndTrim: procedure(nobj: PGLUnurbs); extdecl;
  gluPwlCurve: procedure(nobj: PGLUnurbs; count: GLint; aarray: PGLfloat; stride: GLint; atype: GLenum); extdecl;
  gluNurbsCurve: procedure(nobj: PGLUnurbs; nknots: GLint; knot: PGLfloat; stride: GLint; ctlarray: PGLfloat; order: GLint; atype: GLenum); extdecl;
  gluNurbsSurface: procedure(nobj: PGLUnurbs; sknot_count: GLint; sknot: PGLfloat; tknot_count: GLint; tknot: PGLfloat; s_stride, t_stride: GLint; ctlarray: PGLfloat; sorder, torder: GLint; atype: GLenum); extdecl;
  gluLoadSamplingMatrices: procedure(nobj: PGLUnurbs; var modelMatrix, projMatrix: T16dArray; var viewport: TViewPortArray); extdecl;
  gluNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: GLenum; value: GLfloat); extdecl;
  gluGetNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: GLenum; value: PGLfloat); extdecl;
  gluNurbsCallback: procedure(nobj: PGLUnurbs; which: GLenum; fn: TCallBack); extdecl;

var
  GLU : TLibrary = nil;

function LoadGLU( const aPath : AnsiString = GLUDefaultPath ) : Boolean;

implementation

uses math;

function LoadGLU ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := GLU.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'GLU : Symbol "'+aSymbol+'" not found!' );
  end;
  function TryGetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    TryGetSymbol := GLU.Get( aSymbol );
  end;
begin
  if GLU <> nil then Exit( True );

  GLU := TLibrary.Load( aPath );
  if GLU = nil then Exit( False );

  Pointer(gluErrorString) := GetSymbol('gluErrorString');
  Pointer(gluErrorUnicodeStringEXT) := TryGetSymbol('gluErrorUnicodeStringEXT');
  Pointer(gluGetString) := GetSymbol('gluGetString');
  Pointer(gluOrtho2D) := GetSymbol('gluOrtho2D');
  Pointer(gluPerspective) := GetSymbol('gluPerspective');
  Pointer(gluPickMatrix) := GetSymbol('gluPickMatrix');
  Pointer(gluLookAt) := GetSymbol('gluLookAt');
  Pointer(gluProject) := GetSymbol('gluProject');
  Pointer(gluUnProject) := GetSymbol('gluUnProject');
  Pointer(gluScaleImage) := GetSymbol('gluScaleImage');
  Pointer(gluBuild1DMipmaps) := GetSymbol('gluBuild1DMipmaps');
  Pointer(gluBuild2DMipmaps) := GetSymbol('gluBuild2DMipmaps');
  Pointer(gluNewQuadric) := GetSymbol('gluNewQuadric');
  Pointer(gluDeleteQuadric) := GetSymbol('gluDeleteQuadric');
  Pointer(gluQuadricNormals) := GetSymbol('gluQuadricNormals');
  Pointer(gluQuadricTexture) := GetSymbol('gluQuadricTexture');
  Pointer(gluQuadricOrientation) := GetSymbol('gluQuadricOrientation');
  Pointer(gluQuadricDrawStyle) := GetSymbol('gluQuadricDrawStyle');
  Pointer(gluCylinder) := GetSymbol('gluCylinder');
  Pointer(gluDisk) := GetSymbol('gluDisk');
  Pointer(gluPartialDisk) := GetSymbol('gluPartialDisk');
  Pointer(gluSphere) := GetSymbol('gluSphere');
  Pointer(gluQuadricCallback) := GetSymbol('gluQuadricCallback');
  Pointer(gluNewTess) := GetSymbol('gluNewTess');
  Pointer(gluDeleteTess) := GetSymbol('gluDeleteTess');
  Pointer(gluTessBeginPolygon) := GetSymbol('gluTessBeginPolygon');
  Pointer(gluTessBeginContour) := GetSymbol('gluTessBeginContour');
  Pointer(gluTessVertex) := GetSymbol('gluTessVertex');
  Pointer(gluTessEndContour) := GetSymbol('gluTessEndContour');
  Pointer(gluTessEndPolygon) := GetSymbol('gluTessEndPolygon');
  Pointer(gluTessProperty) := GetSymbol('gluTessProperty');
  Pointer(gluTessNormal) := GetSymbol('gluTessNormal');
  Pointer(gluTessCallback) := GetSymbol('gluTessCallback');
  Pointer(gluGetTessProperty) := GetSymbol('gluGetTessProperty');
  Pointer(gluNewNurbsRenderer) := GetSymbol('gluNewNurbsRenderer');
  Pointer(gluDeleteNurbsRenderer) := GetSymbol('gluDeleteNurbsRenderer');
  Pointer(gluBeginSurface) := GetSymbol('gluBeginSurface');
  Pointer(gluBeginCurve) := GetSymbol('gluBeginCurve');
  Pointer(gluEndCurve) := GetSymbol('gluEndCurve');
  Pointer(gluEndSurface) := GetSymbol('gluEndSurface');
  Pointer(gluBeginTrim) := GetSymbol('gluBeginTrim');
  Pointer(gluEndTrim) := GetSymbol('gluEndTrim');
  Pointer(gluPwlCurve) := GetSymbol('gluPwlCurve');
  Pointer(gluNurbsCurve) := GetSymbol('gluNurbsCurve');
  Pointer(gluNurbsSurface) := GetSymbol('gluNurbsSurface');
  Pointer(gluLoadSamplingMatrices) := GetSymbol('gluLoadSamplingMatrices');
  Pointer(gluNurbsProperty) := GetSymbol('gluNurbsProperty');
  Pointer(gluGetNurbsProperty) := GetSymbol('gluGetNurbsProperty');
  Pointer(gluNurbsCallback) := GetSymbol('gluNurbsCallback');

  Exit( True );
end;

finalization
  if GLU <> nil then FreeAndNil( GLU );

end.

