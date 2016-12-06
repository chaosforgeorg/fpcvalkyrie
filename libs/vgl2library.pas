unit vgl2library;
{$include ../src/valkyrie.inc}
{$MACRO ON}
interface
uses Classes, SysUtils, Types, vlibrary;

const
{$IFDEF WINDOWS}
  GL2DefaultPath = 'opengl32.dll';
{$ELSE}
  {$IFDEF DARWIN}
  GL2DefaultPath = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib';
  {$ELSE}
  GL2DefaultPath = 'libGL.so.1';
  {$ENDIF}
{$ENDIF}

{$include vgl2types.inc}
{$include vgl2const.inc}

{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl}
{$ELSE}
  {$DEFINE extdecl := stdcall}
{$ENDIF}

var
  glBindTexture: procedure(target: GLenum; texture: GLuint); extdecl;
  glBlendFunc: procedure(sfactor, dfactor: GLenum); extdecl;
  glClear: procedure(mask: GLbitfield); extdecl;
  glClearColor: procedure(red, green, blue, alpha: GLclampf); extdecl;
  glClearDepth: procedure(depth: GLclampd); extdecl;
  glClearStencil: procedure(s: GLint); extdecl;
  glColorMask: procedure(red, green, blue, alpha: GLboolean); extdecl;
  glCopyTexImage1D: procedure (target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); extdecl;
  glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y: GLint; width: GLsizei); extdecl;
  glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); extdecl;
  glCullFace: procedure(mode: GLenum); extdecl;
  glDeleteTextures: procedure(n: GLsizei; const textures: PGLuint); extdecl;
  glDepthFunc: procedure(func: GLenum); extdecl;
  glDepthMask: procedure(flag: GLboolean); extdecl;
  glDepthRange: procedure(zNear, zFar: GLclampd); extdecl;
  glDisable: procedure(cap: GLenum); extdecl;
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glDrawBuffer: procedure(mode: GLenum); extdecl;
  glDrawElements: procedure(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); extdecl;
  glEnable: procedure(cap: GLenum); extdecl;
  glFinish: procedure; extdecl;
  glFlush: procedure; extdecl;
  glFrontFace: procedure(mode: GLenum); extdecl;
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); extdecl;
  glGetBooleanv: procedure(pname: GLenum; params: PGLboolean); extdecl;
  glGetDoublev: procedure(pname: GLenum; params: PGLdouble); extdecl;
  glGetError: function: GLenum; extdecl;
  glGetFloatv: procedure(pname: GLenum; params: PGLfloat); extdecl;
  glGetIntegerv: procedure(pname: GLenum; params: PGLint); extdecl;
  glGetPointerv: procedure(pname: GLenum; params: Pointer); extdecl;
  glGetString: function(name: GLenum): PChar; extdecl;
  glGetTexImage: procedure(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer); extdecl;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; params: Pointer); extdecl;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); extdecl;
  glGetTexParameterfv: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glHint: procedure(target, mode: GLenum); extdecl;
  glIsEnabled: function(cap: GLenum): GLboolean; extdecl;
  glIsTexture: function(texture: GLuint): GLboolean; extdecl;
  glLineWidth: procedure(width: GLfloat); extdecl;
  glPixelStoref: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelStorei: procedure(pname: GLenum; param: GLint); extdecl;
  glPointSize: procedure(size: GLfloat); extdecl;
  glPolygonMode: procedure(face, mode: GLenum); extdecl;
  glPolygonOffset: procedure(factor, units: GLfloat); extdecl;
  glReadBuffer: procedure(mode: GLenum); extdecl;
  glReadPixels: procedure(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); extdecl;
  glScissor: procedure(x, y: GLint; width, height: GLsizei); extdecl;
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); extdecl;
  glStencilMask: procedure(mask: GLuint); extdecl;
  glStencilOp: procedure(fail, zfail, zpass: GLenum); extdecl;
  glTexImage1D: procedure(target: GLenum; level: GLInt; internalformat: GLEnum; width: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexImage2D: procedure(target: GLenum; level: GLInt; internalformat: GLEnum; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexSubImage1D: procedure(target: GLenum; level, xoffset: GLint; width: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glViewport: procedure(x, y: GLint; width, height: GLsizei); extdecl;

  //* OpenGL 1.2 non-deprecated functions */
  glBlendColor        : procedure( r : GLfloat; g : GLfloat; b : GLfloat; a : GLfloat );  extdecl;
  glBlendEquation     : procedure( mode : GLenum ); extdecl;
  glDrawRangeElements : procedure ( mode : GLenum; start : GLuint; end_ : GLuint; count : GLsizei; type_ : GLenum; const indices : Pointer ); extdecl;
  glTexSubImage3D : procedure( target: GLenum; level, xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei; format, atype: GLenum; const pixels: Pointer ); extdecl;
  glCopyTexSubImage3D: procedure(target: GLenum; level, xoffset, yoffset, zoffset, x, y : GLint; width, height : GLsizei);  extdecl;

  //* OpenGL 1.3 non-deprecated functions */
  glActiveTexture : procedure( texture : GLenum ); extdecl;
  glSampleCoverage : procedure( value : GLfloat; invert : GLboolean ); extdecl;
  glCompressedTexImage3D : procedure( target : GLenum; level : GLint; iformat : GLenum; width, height, depth : GLsizei; border : GLint; imageSize : GLsizei; const data : Pointer ); extdecl;
  glCompressedTexImage2D : procedure( target : GLenum; level : GLint; iformat : GLenum; width, height : GLsizei; border : GLint; imageSize : GLsizei; const data : Pointer ); extdecl;
  glCompressedTexImage1D : procedure( target : GLenum; level : GLint; iformat : GLenum; width : GLsizei; border : GLint; imageSize : GLsizei; const data : Pointer ); extdecl;
  glCompressedTexSubImage3D : procedure( target : GLenum; level : GLint; xoffset, yoffset, zoffset : GLint;  width, height, depth : GLsizei; format : GLenum; imageSize : GLsizei; data : Pointer ); extdecl;
  glCompressedTexSubImage2D : procedure( target : GLenum; level : GLint; xoffset, yoffset : GLint;  width, height : GLsizei; format : GLenum; imageSize : GLsizei; data : Pointer ); extdecl;
  glCompressedTexSubImage1D : procedure( target : GLenum; level : GLint; xoffset : GLint;  width : GLsizei; format : GLenum; imageSize : GLsizei; data : Pointer ); extdecl;
  glGetCompressedTexImage : procedure( target : GLenum; level : GLint; data : Pointer ); extdecl;

  //* OpenGL 1.4 non-deprecated functions */
  glBlendFuncSeparate : procedure( srcRGB, dstRGB, srcAlpha, dstAlpha: GLenum ); extdecl;
  glMultiDrawArrays : procedure( mode : GLenum; const first : PGLint; const count : PGLsizei; primcount : GLsizei ); extdecl;
  glMultiDrawElements : procedure( mode : GLenum; const count : PGLsizei; etype : GLenum; const indices : PPGLint;  primcount : GLsizei ); extdecl;
  glPointParameterf : procedure( pname : GLenum; param : GLfloat ); extdecl;
  glPointParameterfv : procedure( pname : GLenum; const param : PGLfloat ); extdecl;
  glPointParameteri : procedure( pname : GLenum; param : GLint ); extdecl;
  glPointParameteriv : procedure( pname : GLenum; const param : PGLint ); extdecl;


  //* OpenGL 1.5 non-deprecated functions */
  glGenQueries : procedure( n : GLsizei; ids : PGLuint ); extdecl;
  glDeleteQueries : procedure( n : GLsizei; const ids : PGLuint ); extdecl;
  glIsQuery : function( id : GLuint ) : GLboolean ; extdecl;
  glBeginQuery : procedure( target : GLenum;  id : GLuint ); extdecl;
  glEndQuery : procedure( target : GLenum ); extdecl;
  glGetQueryiv : procedure( target, pname : GLenum; params : PGLint ); extdecl;
  glGetQueryObjectiv : procedure( target : GLuint; pname : GLenum; params : PGLint ); extdecl;
  glGetQueryObjectuiv : procedure( target : GLuint; pname : GLenum; params : PGLuint ); extdecl;
  glBindBuffer : procedure( target : GLenum; buffer : GLuint ); extdecl;
  glDeleteBuffers : procedure( n : GLsizei; const ids : PGLuint ); extdecl;
  glGenBuffers : procedure( n : GLsizei; ids : PGLuint ); extdecl;
  glIsBuffer : function( id : GLuint ) : GLboolean ; extdecl;
  glBufferData : procedure( target : GLenum; size : GLsizeiptr; const data : Pointer; usage : GLenum ); extdecl;
  glBufferSubData : procedure( target : GLenum; offset : GLintptr; size : GLsizeiptr; const data : Pointer ); extdecl;
  glGetBufferSubData : procedure( target : GLenum; offset : GLintptr; size : GLsizeiptr; data : Pointer ); extdecl;
  glMapBuffer : function( target, access : GLenum ) : Pointer; extdecl;
  glUnmapBuffer : function( target : GLenum ) : GLboolean ; extdecl;
  glGetBufferParameteriv : procedure( target, value : GLenum; data : PGLint ); extdecl;
  glGetBufferPointerv : procedure( target, value : GLenum; data : PPointer ); extdecl;

  //* OpenGL 2.0 non-deprecated functions */
  glBlendEquationSeparate : procedure( modeRGB, modeAlpha : GLenum ); extdecl;
  glDrawBuffers : procedure( n : GLsizei; const bufs : PGLenum ); extdecl;
  glStencilOpSeparate : procedure( face, sfail, dpfail, dppass : GLenum ); extdecl;
  glStencilFuncSeparate : procedure( face, func : GLenum; ref : GLint; mask : GLuint ); extdecl;
  glStencilMaskSeparate : procedure( face : GLenum; mask : GLuint ); extdecl;
  glAttachShader : procedure( prog, shader : GLuint ); extdecl;
  glBindAttribLocation : procedure( prog, index : GLuint; const name : PGLchar ); extdecl;
  glCompileShader : procedure( shader : GLuint ); extdecl;
  glCreateProgram : function() : GLuint ; extdecl;
  glCreateShader : function( stype : GLenum ) : GLuint ; extdecl;
  glDeleteProgram : procedure( prog : GLuint ); extdecl;
  glDeleteShader : procedure( shader : GLuint ); extdecl;
  glDetachShader : procedure( prog, shader : GLuint ); extdecl;
  glDisableVertexAttribArray : procedure( index : GLuint ); extdecl;
  glEnableVertexAttribArray : procedure( index : GLuint ); extdecl;
  glGetActiveAttrib : procedure( prog : GLuint; index : GLuint; bufSize : GLsizei; length : PGLsizei; size : PGLint; typ : PGLenum; name : PGLchar ); extdecl;
  glGetActiveUniform : procedure( prog : GLuint; index : GLuint; bufSize : GLsizei; length : PGLsizei; size : PGLint; typ : PGLenum; name : PGLchar ); extdecl;
  glGetAttachedShaders : procedure( prog : GLuint; maxcount : GLsizei; count : PGLsizei; shaders : PGLuint ); extdecl;
  glGetAttribLocation : function( prog : GLuint; const name : PGLchar ) : GLint ; extdecl;
  glGetProgramiv : procedure( prog : GLuint; pname : GLenum; data : PGLint ); extdecl;
  glGetProgramInfoLog : procedure( prog : GLuint; bufsize : GLsizei; length : PGLsizei; source : PGLchar ); extdecl;
  glGetShaderiv : procedure( shader : GLuint; pname : GLenum; data : PGLint ); extdecl;
  glGetShaderInfoLog : procedure( shader : GLuint; bufsize : GLsizei; length : PGLsizei; source : PGLchar ); extdecl;
  glGetShaderSource : procedure( shader : GLuint; bufsize : GLsizei; length : PGLsizei; source : PGLchar ); extdecl;
  glGetUniformLocation : function( prog : GLuint; name : PGLchar ) : GLint ; extdecl;
  glGetUniformfv : procedure( prog : GLuint; location : GLint; params : PGLfloat ); extdecl;
  glGetUniformiv : procedure( prog : GLuint; location : GLint; params : PGLint ); extdecl;
  glGetVertexAttribdv : procedure( index : GLuint; pname : GLenum; data : PGLdouble ); extdecl;
  glGetVertexAttribfv : procedure( index : GLuint; pname : GLenum; data : PGLfloat ); extdecl;
  glGetVertexAttribiv : procedure( index : GLuint; pname : GLenum; data : PGLint ); extdecl;
  glGetVertexAttribPointerv : procedure( index : GLuint; pname : GLenum; data : PPointer ); extdecl;
  glIsProgram : function( progid : GLuint ) : GLboolean ; extdecl;
  glIsShader : function( shader : GLuint ) : GLboolean ; extdecl;
  glLinkProgram : procedure( progid : GLuint ); extdecl;
  glShaderSource : procedure( shader : GLuint; count : GLsizei; const str : PPGLchar; const len : PGLint); extdecl;
  glUseProgram : procedure( progid : GLuint ); extdecl;
  glUniform1f : procedure( location : GLint; v0 : GLfloat ); extdecl;
  glUniform2f : procedure( location : GLint; v0, v1 : GLfloat ); extdecl;
  glUniform3f : procedure( location : GLint; v0, v1, v2 : GLfloat ); extdecl;
  glUniform4f : procedure( location : GLint; v0, v1, v2, v3 : GLfloat ); extdecl;
  glUniform1i : procedure( location, v0 : GLint ); extdecl;
  glUniform2i : procedure( location, v0, v1 : GLint ); extdecl;
  glUniform3i : procedure( location, v0, v1, v2 : GLint ); extdecl;
  glUniform4i : procedure( location, v0, v1, v2, v3 : GLint ); extdecl;
  glUniform1fv : procedure( location : GLint; count : GLsizei; const data : PGLfloat ); extdecl;
  glUniform2fv : procedure( location : GLint; count : GLsizei; const data : PGLfloat ); extdecl;
  glUniform3fv : procedure( location : GLint; count : GLsizei; const data : PGLfloat ); extdecl;
  glUniform4fv : procedure( location : GLint; count : GLsizei; const data : PGLfloat ); extdecl;
  glUniform1iv : procedure( location : GLint; count : GLsizei; const data : PGLint ); extdecl;
  glUniform2iv : procedure( location : GLint; count : GLsizei; const data : PGLint ); extdecl;
  glUniform3iv : procedure( location : GLint; count : GLsizei; const data : PGLint ); extdecl;
  glUniform4iv : procedure( location : GLint; count : GLsizei; const data : PGLint ); extdecl;
  glUniformMatrix2fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glUniformMatrix3fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glUniformMatrix4fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glValidateProgram : procedure( progid : GLuint ); extdecl;
  glVertexAttribPointer : procedure( index : GLuint; size : GLint; typ : GLenum; normalized : GLboolean; stride : GLsizei; const data : Pointer ); extdecl;

  //* OpenGL 2.1 non-deprecated functions */
  glUniformMatrix2x3fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glUniformMatrix3x2fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glUniformMatrix2x4fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glUniformMatrix4x2fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glUniformMatrix3x4fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;
  glUniformMatrix4x3fv : procedure( location : GLint; count : GLsizei; transpose : GLboolean; const data : PGLfloat ); extdecl;

var
  GL2 : TLibrary = nil;

function LoadGL2( const aPath : AnsiString = GL2DefaultPath ) : Boolean;

implementation

uses math, vsdllibrary;

function LoadGL2 ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    //GetSymbol := GL2.Get( aSymbol );
    GetSymbol := SDL_GL_GetProcAddress( PChar(aSymbol) );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'GL2 : Symbol "'+aSymbol+'" not found!' );
  end;
  function TryGetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    TryGetSymbol := GL2.Get( aSymbol );
  end;
  function GetExtSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    //GetExtSymbol := GL2.Get( aSymbol );
    GetExtSymbol := SDL_GL_GetProcAddress( PChar(aSymbol) );
    if GetExtSymbol = nil then
      raise ELibraryError.Create( 'GL2 : ExtSymbol "'+aSymbol+'" not found!' );
  end;

begin
  if GL2 <> nil then Exit( True );
  if SDL = nil then LoadSDL();

  {$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
  {$endif}

  GL2 := TLibrary.Load( aPath );
  if GL2 = nil then Exit( False );

  Pointer(glBindTexture) := GetSymbol('glBindTexture');
  Pointer(glBlendFunc) := GetSymbol('glBlendFunc');
  Pointer(glClear) := GetSymbol('glClear');
  Pointer(glClearColor) := GetSymbol('glClearColor');
  Pointer(glClearDepth) := GetSymbol('glClearDepth');
  Pointer(glClearStencil) := GetSymbol('glClearStencil');
  Pointer(glColorMask) := GetSymbol('glColorMask');
  Pointer(glCopyTexImage1D) := GetSymbol('glCopyTexImage1D');
  Pointer(glCopyTexImage2D) := GetSymbol('glCopyTexImage2D');
  Pointer(glCopyTexSubImage1D) := GetSymbol('glCopyTexSubImage1D');
  Pointer(glCopyTexSubImage2D) := GetSymbol('glCopyTexSubImage2D');
  Pointer(glCullFace) := GetSymbol('glCullFace');
  Pointer(glDeleteTextures) := GetSymbol('glDeleteTextures');
  Pointer(glDepthFunc) := GetSymbol('glDepthFunc');
  Pointer(glDepthMask) := GetSymbol('glDepthMask');
  Pointer(glDepthRange) := GetSymbol('glDepthRange');
  Pointer(glDisable) := GetSymbol('glDisable');
  Pointer(glDrawArrays) := GetSymbol('glDrawArrays');
  Pointer(glDrawBuffer) := GetSymbol('glDrawBuffer');
  Pointer(glDrawElements) := GetSymbol('glDrawElements');
  Pointer(glEnable) := GetSymbol('glEnable');
  Pointer(glFinish) := GetSymbol('glFinish');
  Pointer(glFlush) := GetSymbol('glFlush');
  Pointer(glFrontFace) := GetSymbol('glFrontFace');
  Pointer(glGenTextures) := GetSymbol('glGenTextures');
  Pointer(glGetBooleanv) := GetSymbol('glGetBooleanv');
  Pointer(glGetDoublev) := GetSymbol('glGetDoublev');
  Pointer(glGetError) := GetSymbol('glGetError');
  Pointer(glGetFloatv) := GetSymbol('glGetFloatv');
  Pointer(glGetIntegerv) := GetSymbol('glGetIntegerv');
  Pointer(glGetPointerv) := GetSymbol('glGetPointerv');
  Pointer(glGetString) := GetSymbol('glGetString');
  Pointer(glGetTexImage) := GetSymbol('glGetTexImage');
  Pointer(glGetTexLevelParameterfv) := GetSymbol('glGetTexLevelParameterfv');
  Pointer(glGetTexLevelParameteriv) := GetSymbol('glGetTexLevelParameteriv');
  Pointer(glGetTexParameterfv) := GetSymbol('glGetTexParameterfv');
  Pointer(glGetTexParameteriv) := GetSymbol('glGetTexParameteriv');
  Pointer(glHint) := GetSymbol('glHint');
  Pointer(glIsEnabled) := GetSymbol('glIsEnabled');
  Pointer(glIsTexture) := GetSymbol('glIsTexture');
  Pointer(glLineWidth) := GetSymbol('glLineWidth');
  Pointer(glPixelStoref) := GetSymbol('glPixelStoref');
  Pointer(glPixelStorei) := GetSymbol('glPixelStorei');
  Pointer(glPointSize) := GetSymbol('glPointSize');
  Pointer(glPolygonMode) := GetSymbol('glPolygonMode');
  Pointer(glPolygonOffset) := GetSymbol('glPolygonOffset');
  Pointer(glReadBuffer) := GetSymbol('glReadBuffer');
  Pointer(glReadPixels) := GetSymbol('glReadPixels');
  Pointer(glScissor) := GetSymbol('glScissor');
  Pointer(glStencilFunc) := GetSymbol('glStencilFunc');
  Pointer(glStencilMask) := GetSymbol('glStencilMask');
  Pointer(glStencilOp) := GetSymbol('glStencilOp');
  Pointer(glTexImage1D) := GetSymbol('glTexImage1D');
  Pointer(glTexImage2D) := GetSymbol('glTexImage2D');
  Pointer(glTexParameterf) := GetSymbol('glTexParameterf');
  Pointer(glTexParameterfv) := GetSymbol('glTexParameterfv');
  Pointer(glTexParameteri) := GetSymbol('glTexParameteri');
  Pointer(glTexParameteriv) := GetSymbol('glTexParameteriv');
  Pointer(glTexSubImage1D) := GetSymbol('glTexSubImage1D');
  Pointer(glTexSubImage2D) := GetSymbol('glTexSubImage2D');
  Pointer(glViewport) := GetSymbol('glViewport');

  Pointer(glBlendColor) := GetExtSymbol('glBlendColor');
  Pointer(glBlendEquation) := GetExtSymbol('glBlendEquation');
  Pointer(glDrawRangeElements) := GetExtSymbol('glDrawRangeElements');
  Pointer(glTexSubImage3D) := GetExtSymbol('glTexSubImage3D');
  Pointer(glCopyTexSubImage3D) := GetExtSymbol('glCopyTexSubImage3D');

  //* OpenGL 1.3 non-deprecated functions */
  Pointer(glActiveTexture) := GetExtSymbol('glActiveTexture');
  Pointer(glSampleCoverage) := GetExtSymbol('glSampleCoverage');
  Pointer(glCompressedTexImage3D) := GetExtSymbol('glCompressedTexImage3D');
  Pointer(glCompressedTexImage2D) := GetExtSymbol('glCompressedTexImage2D');
  Pointer(glCompressedTexImage1D) := GetExtSymbol('glCompressedTexImage1D');
  Pointer(glCompressedTexSubImage3D) := GetExtSymbol('glCompressedTexSubImage3D');
  Pointer(glCompressedTexSubImage2D) := GetExtSymbol('glCompressedTexSubImage2D');
  Pointer(glCompressedTexSubImage1D) := GetExtSymbol('glCompressedTexSubImage1D');
  Pointer(glGetCompressedTexImage) := GetExtSymbol('glGetCompressedTexImage');

  //* OpenGL 1.4 non-deprecated functions */
  Pointer(glBlendFuncSeparate) := GetExtSymbol('glBlendFuncSeparate');
  Pointer(glMultiDrawArrays) := GetExtSymbol('glMultiDrawArrays');
  Pointer(glMultiDrawElements) := GetExtSymbol('glMultiDrawElements');
  Pointer(glPointParameterf) := GetExtSymbol('glPointParameterf');
  Pointer(glPointParameterfv) := GetExtSymbol('glPointParameterfv');
  Pointer(glPointParameteri) := GetExtSymbol('glPointParameteri');
  Pointer(glPointParameteriv) := GetExtSymbol('glPointParameteriv');


  //* OpenGL 1.5 non-deprecated functions */
  Pointer(glGenQueries) := GetExtSymbol('glGenQueries');
  Pointer(glDeleteQueries) := GetExtSymbol('glDeleteQueries');
  Pointer(glIsQuery) := GetExtSymbol('glIsQuery');
  Pointer(glBeginQuery) := GetExtSymbol('glBeginQuery');
  Pointer(glEndQuery) := GetExtSymbol('glEndQuery');
  Pointer(glGetQueryiv) := GetExtSymbol('glGetQueryiv');
  Pointer(glGetQueryObjectiv) := GetExtSymbol('glGetQueryObjectiv');
  Pointer(glGetQueryObjectuiv) := GetExtSymbol('glGetQueryObjectuiv');
  Pointer(glBindBuffer) := GetExtSymbol('glBindBuffer');
  Pointer(glDeleteBuffers) := GetExtSymbol('glDeleteBuffers');
  Pointer(glGenBuffers) := GetExtSymbol('glGenBuffers');
  Pointer(glIsBuffer) := GetExtSymbol('glIsBuffer');
  Pointer(glBufferData) := GetExtSymbol('glBufferData');
  Pointer(glBufferSubData) := GetExtSymbol('glBufferSubData');
  Pointer(glGetBufferSubData) := GetExtSymbol('glGetBufferSubData');
  Pointer(glMapBuffer) := GetExtSymbol('glMapBuffer');
  Pointer(glUnmapBuffer) := GetExtSymbol('glUnmapBuffer');
  Pointer(glGetBufferParameteriv) := GetExtSymbol('glGetBufferParameteriv');
  Pointer(glGetBufferPointerv) := GetExtSymbol('glGetBufferPointerv');

  //* OpenGL 2.0 non-deprecated functions */
  Pointer(glBlendEquationSeparate) := GetExtSymbol('glBlendEquationSeparate');
  Pointer(glDrawBuffers) := GetExtSymbol('glDrawBuffers');
  Pointer(glStencilOpSeparate) := GetExtSymbol('glStencilOpSeparate');
  Pointer(glStencilFuncSeparate) := GetExtSymbol('glStencilFuncSeparate');
  Pointer(glStencilMaskSeparate) := GetExtSymbol('glStencilMaskSeparate');
  Pointer(glAttachShader) := GetExtSymbol('glAttachShader');
  Pointer(glBindAttribLocation) := GetExtSymbol('glBindAttribLocation');
  Pointer(glCompileShader) := GetExtSymbol('glCompileShader');
  Pointer(glCreateProgram) := GetExtSymbol('glCreateProgram');
  Pointer(glCreateShader) := GetExtSymbol('glCreateShader');
  Pointer(glDeleteProgram) := GetExtSymbol('glDeleteProgram');
  Pointer(glDeleteShader) := GetExtSymbol('glDeleteShader');
  Pointer(glDetachShader) := GetExtSymbol('glDetachShader');
  Pointer(glDisableVertexAttribArray) := GetExtSymbol('glDisableVertexAttribArray');
  Pointer(glEnableVertexAttribArray) := GetExtSymbol('glEnableVertexAttribArray');
  Pointer(glGetActiveAttrib) := GetExtSymbol('glGetActiveAttrib');
  Pointer(glGetActiveUniform) := GetExtSymbol('glGetActiveUniform');
  Pointer(glGetAttachedShaders) := GetExtSymbol('glGetAttachedShaders');
  Pointer(glGetAttribLocation) := GetExtSymbol('glGetAttribLocation');
  Pointer(glGetProgramiv) := GetExtSymbol('glGetProgramiv');
  Pointer(glGetProgramInfoLog) := GetExtSymbol('glGetProgramInfoLog');
  Pointer(glGetShaderiv) := GetExtSymbol('glGetShaderiv');
  Pointer(glGetShaderInfoLog) := GetExtSymbol('glGetShaderInfoLog');
  Pointer(glGetShaderSource) := GetExtSymbol('glGetShaderSource');
  Pointer(glGetUniformLocation) := GetExtSymbol('glGetUniformLocation');
  Pointer(glGetUniformfv) := GetExtSymbol('glGetUniformfv');
  Pointer(glGetUniformiv) := GetExtSymbol('glGetUniformiv');
  Pointer(glGetVertexAttribdv) := GetExtSymbol('glGetVertexAttribdv');
  Pointer(glGetVertexAttribfv) := GetExtSymbol('glGetVertexAttribfv');
  Pointer(glGetVertexAttribiv) := GetExtSymbol('glGetVertexAttribiv');
  Pointer(glGetVertexAttribPointerv) := GetExtSymbol('glGetVertexAttribPointerv');
  Pointer(glIsProgram) := GetExtSymbol('glIsProgram');
  Pointer(glIsShader) := GetExtSymbol('glIsShader');
  Pointer(glLinkProgram) := GetExtSymbol('glLinkProgram');
  Pointer(glShaderSource) := GetExtSymbol('glShaderSource');
  Pointer(glUseProgram) := GetExtSymbol('glUseProgram');
  Pointer(glUniform1f) := GetExtSymbol('glUniform1f');
  Pointer(glUniform2f) := GetExtSymbol('glUniform2f');
  Pointer(glUniform3f) := GetExtSymbol('glUniform3f');
  Pointer(glUniform4f) := GetExtSymbol('glUniform4f');
  Pointer(glUniform1i) := GetExtSymbol('glUniform1i');
  Pointer(glUniform2i) := GetExtSymbol('glUniform2i');
  Pointer(glUniform3i) := GetExtSymbol('glUniform3i');
  Pointer(glUniform4i) := GetExtSymbol('glUniform4i');
  Pointer(glUniform1fv) := GetExtSymbol('glUniform1fv');
  Pointer(glUniform2fv) := GetExtSymbol('glUniform2fv');
  Pointer(glUniform3fv) := GetExtSymbol('glUniform3fv');
  Pointer(glUniform4fv) := GetExtSymbol('glUniform4fv');
  Pointer(glUniform1iv) := GetExtSymbol('glUniform1iv');
  Pointer(glUniform2iv) := GetExtSymbol('glUniform2iv');
  Pointer(glUniform3iv) := GetExtSymbol('glUniform3iv');
  Pointer(glUniform4iv) := GetExtSymbol('glUniform4iv');
  Pointer(glUniformMatrix2fv) := GetExtSymbol('glUniformMatrix2fv');
  Pointer(glUniformMatrix3fv) := GetExtSymbol('glUniformMatrix3fv');
  Pointer(glUniformMatrix4fv) := GetExtSymbol('glUniformMatrix4fv');
  Pointer(glValidateProgram) := GetExtSymbol('glValidateProgram');
  Pointer(glVertexAttribPointer) := GetExtSymbol('glVertexAttribPointer');

  //* OpenGL 2.1 non-deprecated functions */
  Pointer(glUniformMatrix2x3fv) := GetExtSymbol('glUniformMatrix2x3fv');
  Pointer(glUniformMatrix3x2fv) := GetExtSymbol('glUniformMatrix3x2fv');
  Pointer(glUniformMatrix2x4fv) := GetExtSymbol('glUniformMatrix2x4fv');
  Pointer(glUniformMatrix4x2fv) := GetExtSymbol('glUniformMatrix4x2fv');
  Pointer(glUniformMatrix3x4fv) := GetExtSymbol('glUniformMatrix3x4fv');
  Pointer(glUniformMatrix4x3fv) := GetExtSymbol('glUniformMatrix4x3fv');

  Exit( True );
end;

finalization
  if GL2 <> nil then FreeAndNil( GL2 );

end.

