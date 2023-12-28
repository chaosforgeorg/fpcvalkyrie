// OpenGL 1.0 non-deprecated functions
var
  glCullFace                  : procedure(mode: GLenum); extdecl;
  glFrontFace                 : procedure(mode: GLenum); extdecl;
  glHint                      : procedure(target, mode: GLenum); extdecl;
  glLineWidth                 : procedure(width: GLfloat); extdecl;
  glPointSize                 : procedure(size: GLfloat); extdecl;
  glPolygonMode               : procedure(face, mode: GLenum); extdecl;
  glScissor                   : procedure(x, y: GLint; width, height: GLsizei); extdecl;
  glTexParameterf             : procedure(target, pname: GLenum; param: GLfloat); extdecl;
  glTexParameterfv            : procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glTexParameteri             : procedure(target, pname: GLenum; param: GLint); extdecl;
  glTexParameteriv            : procedure(target, pname: GLenum; params: PGLint); extdecl;
  glTexImage1D                : procedure(target: GLenum; level, internalformat: GLint; width: GLsizei; border: GLint; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glTexImage2D                : procedure(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glDrawBuffer                : procedure(buf: GLenum); extdecl;
  glClear                     : procedure(mask: GLbitfield); extdecl;
  glClearColor                : procedure(red, green, blue, alpha: GLfloat); extdecl;
  glClearStencil              : procedure(s: GLint); extdecl;
  glClearDepth                : procedure(depth: GLdouble); extdecl;
  glStencilMask               : procedure(mask: GLuint); extdecl;
  glColorMask                 : procedure(red, green, blue, alpha: GLboolean); extdecl;
  glDepthMask                 : procedure(flag: GLboolean); extdecl;
  glDisable                   : procedure(cap: GLenum); extdecl;
  glEnable                    : procedure(cap: GLenum); extdecl;
  glFinish                    : procedure; extdecl;
  glFlush                     : procedure; extdecl;
  glBlendFunc                 : procedure(sfactor, dfactor: GLenum); extdecl;
  glLogicOp                   : procedure(opcode: GLenum); extdecl;
  glStencilFunc               : procedure(func: GLenum; ref: GLint; mask: GLuint); extdecl;
  glStencilOp                 : procedure(fail, zfail, zpass: GLenum); extdecl;
  glDepthFunc                 : procedure(func: GLenum); extdecl;
  glPixelStoref               : procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelStorei               : procedure(pname: GLenum; param: GLint); extdecl;
  glReadBuffer                : procedure(src: GLenum); extdecl;
  glReadPixels                : procedure(x, y: GLint; width, height: GLsizei; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glGetBooleanv               : procedure(pname: GLenum; data: PGLboolean); extdecl;
  glGetDoublev                : procedure(pname: GLenum; data: PGLdouble); extdecl;
  glGetError                  : function: GLenum; extdecl;
  glGetFloatv                 : procedure(pname: GLenum; data: PGLfloat); extdecl;
  glGetIntegerv               : procedure(pname: GLenum; data: PGLint); extdecl;
  glGetString                 : function(name: GLenum): PGLchar; extdecl;
  glGetTexImage               : procedure(target: GLenum; level: GLint; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glGetTexParameterfv         : procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexParameteriv         : procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetTexLevelParameterfv    : procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); extdecl;
  glGetTexLevelParameteriv    : procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); extdecl;
  glIsEnabled                 : function(cap: GLenum): GLboolean; extdecl;
  glDepthRange                : procedure(near, far: GLdouble); extdecl;
  glViewport                  : procedure(x, y: GLint; width, height: GLsizei); extdecl;

// OpenGL 1.1 non-deprecated functions
var
  glDrawArrays                : procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glDrawElements              : procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: PGLvoid); extdecl;
  glGetPointerv               : procedure(pname: GLenum; params: PPGLvoid); extdecl;
  glPolygonOffset             : procedure(factor, units: GLfloat); extdecl;
  glCopyTexImage1D            : procedure(target: GLenum; level: GLint; internalformat: GLenum; x, y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyTexImage2D            : procedure(target: GLenum; level: GLint; internalformat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); extdecl;
  glCopyTexSubImage1D         : procedure(target: GLenum; level: GLint; xoffset, x, y: GLint; width: GLsizei); extdecl;
  glCopyTexSubImage2D         : procedure(target: GLenum; level: GLint; xoffset, yoffset, x, y: GLint; width, height: GLsizei); extdecl;
  glTexSubImage1D             : procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glTexSubImage2D             : procedure(target: GLenum; level: GLint; xoffset, yoffset: GLint; width, height: GLsizei; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glBindTexture               : procedure(target: GLenum; texture: GLuint); extdecl;
  glDeleteTextures            : procedure(n: GLsizei; textures: PGLuint); extdecl;
  glGenTextures               : procedure(n: GLsizei; textures: PGLuint); extdecl;
  glIsTexture                 : function(texture: GLuint): GLboolean; extdecl;

// OpenGL 1.2 non-deprecated functions
var
  glDrawRangeElements    : procedure(mode: GLenum; start, end_: GLuint; count: GLsizei; type_: GLenum; indices: PGLvoid); extdecl;
  glTexImage3D           : procedure(target: GLenum; level: GLint; internalformat: GLint; width, height, depth: GLsizei; border: GLint; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glTexSubImage3D        : procedure(target: GLenum; level: GLint; xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei; format, type_: GLenum; pixels: PGLvoid); extdecl;
  glCopyTexSubImage3D    : procedure(target: GLenum; level: GLint; xoffset, yoffset, zoffset, x, y: GLint; width, height: GLsizei); extdecl;

// OpenGL 1.3 non-deprecated functions
var
  glActiveTexture               : procedure(texture: GLenum); extdecl;
  glSampleCoverage              : procedure(value: GLfloat; invert: GLboolean); extdecl;
  glCompressedTexImage3D        : procedure(target: GLenum; level: GLint; internalformat: GLenum; width, height, depth: GLsizei; border: GLint; imageSize: GLsizei; data: PGLvoid); extdecl;
  glCompressedTexImage2D        : procedure(target: GLenum; level: GLint; internalformat: GLenum; width, height: GLsizei; border: GLint; imageSize: GLsizei; data: PGLvoid); extdecl;
  glCompressedTexImage1D        : procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; data: PGLvoid); extdecl;
  glCompressedTexSubImage3D     : procedure(target: GLenum; level: GLint; xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei; format: GLenum; imageSize: GLsizei; data: PGLvoid); extdecl;
  glCompressedTexSubImage2D     : procedure(target: GLenum; level: GLint; xoffset, yoffset: GLint; width, height: GLsizei; format: GLenum; imageSize: GLsizei; data: PGLvoid); extdecl;
  glCompressedTexSubImage1D     : procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; data: PGLvoid); extdecl;
  glGetCompressedTexImage       : procedure(target: GLenum; level: GLint; img: PGLvoid); extdecl;

// OpenGL 1.4 non-deprecated functions
var
  glBlendFuncSeparate   : procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: GLenum); extdecl;
  glMultiDrawArrays     : procedure(mode: GLenum; const first: PGLint; const count: PGLsizei; drawcount: GLsizei); extdecl;
  glMultiDrawElements   : procedure(mode: GLenum; const count: PGLsizei; type_: GLenum; const indices: PPGLvoid; drawcount: GLsizei); extdecl;
  glPointParameterf     : procedure(pname: GLenum; param: GLfloat); extdecl;
  glPointParameterfv    : procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glPointParameteri     : procedure(pname: GLenum; param: GLint); extdecl;
  glPointParameteriv    : procedure(pname: GLenum; const params: PGLint); extdecl;
  glBlendColor          : procedure(red, green, blue, alpha: GLfloat); extdecl;
  glBlendEquation       : procedure(mode: GLenum); extdecl;

// OpenGL 1.5 non-deprecated functions
var
  glGenQueries          : procedure(n: GLsizei; ids: PGLuint); extdecl;
  glDeleteQueries       : procedure(n: GLsizei; const ids: PGLuint); extdecl;
  glIsQuery             : function(id: GLuint): GLboolean; extdecl;
  glBeginQuery          : procedure(target: GLenum; id: GLuint); extdecl;
  glEndQuery            : procedure(target: GLenum); extdecl;
  glGetQueryiv          : procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetQueryObjectiv    : procedure(id: GLuint; pname: GLenum; params: PGLint); extdecl;
  glGetQueryObjectuiv   : procedure(id: GLuint; pname: GLenum; params: PGLuint); extdecl;
  glBindBuffer          : procedure(target: GLenum; buffer: GLuint); extdecl;
  glDeleteBuffers       : procedure(n: GLsizei; const buffers: PGLuint); extdecl;
  glGenBuffers          : procedure(n: GLsizei; buffers: PGLuint); extdecl;
  glIsBuffer            : function(buffer: GLuint): GLboolean; extdecl;
  glBufferData          : procedure(target: GLenum; size: GLsizeiptr; const data: PGLvoid; usage: GLenum); extdecl;
  glBufferSubData       : procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; const data: PGLvoid); extdecl;
  glGetBufferSubData    : procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: PGLvoid); extdecl;
  glMapBuffer           : function(target, access: GLenum): PGLvoid; extdecl;
  glUnmapBuffer         : function(target: GLenum): GLboolean; extdecl;
  glGetBufferParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetBufferPointerv   : procedure(target, pname: GLenum; params: PPGLvoid); extdecl;


