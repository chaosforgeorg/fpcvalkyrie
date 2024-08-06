Pointer(glCullFace)               := GetSymbol('glCullFace');
Pointer(glFrontFace)              := GetSymbol('glFrontFace');
Pointer(glHint)                   := GetSymbol('glHint');
Pointer(glLineWidth)              := GetSymbol('glLineWidth');
Pointer(glPointSize)              := GetSymbol('glPointSize');
Pointer(glPolygonMode)            := GetSymbol('glPolygonMode');
Pointer(glScissor)                := GetSymbol('glScissor');
Pointer(glTexParameterf)          := GetSymbol('glTexParameterf');
Pointer(glTexParameterfv)         := GetSymbol('glTexParameterfv');
Pointer(glTexParameteri)          := GetSymbol('glTexParameteri');
Pointer(glTexParameteriv)         := GetSymbol('glTexParameteriv');
Pointer(glTexImage1D)             := GetSymbol('glTexImage1D');
Pointer(glTexImage2D)             := GetSymbol('glTexImage2D');
Pointer(glDrawBuffer)             := GetSymbol('glDrawBuffer');
Pointer(glClear)                  := GetSymbol('glClear');
Pointer(glClearColor)             := GetSymbol('glClearColor');
Pointer(glClearStencil)           := GetSymbol('glClearStencil');
Pointer(glClearDepth)             := GetSymbol('glClearDepth');
Pointer(glStencilMask)            := GetSymbol('glStencilMask');
Pointer(glColorMask)              := GetSymbol('glColorMask');
Pointer(glDepthMask)              := GetSymbol('glDepthMask');
Pointer(glDisable)                := GetSymbol('glDisable');
Pointer(glEnable)                 := GetSymbol('glEnable');
Pointer(glFinish)                 := GetSymbol('glFinish');
Pointer(glFlush)                  := GetSymbol('glFlush');
Pointer(glBlendFunc)              := GetSymbol('glBlendFunc');
Pointer(glLogicOp)                := GetSymbol('glLogicOp');
Pointer(glStencilFunc)            := GetSymbol('glStencilFunc');
Pointer(glStencilOp)              := GetSymbol('glStencilOp');
Pointer(glDepthFunc)              := GetSymbol('glDepthFunc');
Pointer(glPixelStoref)            := GetSymbol('glPixelStoref');
Pointer(glPixelStorei)            := GetSymbol('glPixelStorei');
Pointer(glReadBuffer)             := GetSymbol('glReadBuffer');
Pointer(glReadPixels)             := GetSymbol('glReadPixels');
Pointer(glGetBooleanv)            := GetSymbol('glGetBooleanv');
Pointer(glGetDoublev)             := GetSymbol('glGetDoublev');
Pointer(glGetError)               := GetSymbol('glGetError');
Pointer(glGetFloatv)              := GetSymbol('glGetFloatv');
Pointer(glGetIntegerv)            := GetSymbol('glGetIntegerv');
Pointer(glGetString)              := GetSymbol('glGetString');
Pointer(glGetTexImage)            := GetSymbol('glGetTexImage');
Pointer(glGetTexParameterfv)      := GetSymbol('glGetTexParameterfv');
Pointer(glGetTexParameteriv)      := GetSymbol('glGetTexParameteriv');
Pointer(glGetTexLevelParameterfv) := GetSymbol('glGetTexLevelParameterfv');
Pointer(glGetTexLevelParameteriv) := GetSymbol('glGetTexLevelParameteriv');
Pointer(glIsEnabled)              := GetSymbol('glIsEnabled');
Pointer(glDepthRange)             := GetSymbol('glDepthRange');
Pointer(glViewport)               := GetSymbol('glViewport');

// OpenGL 1.1 non-deprecated functions
Pointer(glDrawArrays)             := GetSymbol('glDrawArrays');
Pointer(glDrawElements)           := GetSymbol('glDrawElements');
Pointer(glGetPointerv)            := GetSymbol('glGetPointerv');
Pointer(glPolygonOffset)          := GetSymbol('glPolygonOffset');
Pointer(glCopyTexImage1D)         := GetSymbol('glCopyTexImage1D');
Pointer(glCopyTexImage2D)         := GetSymbol('glCopyTexImage2D');
Pointer(glCopyTexSubImage1D)      := GetSymbol('glCopyTexSubImage1D');
Pointer(glCopyTexSubImage2D)      := GetSymbol('glCopyTexSubImage2D');
Pointer(glTexSubImage1D)          := GetSymbol('glTexSubImage1D');
Pointer(glTexSubImage2D)          := GetSymbol('glTexSubImage2D');
Pointer(glBindTexture)            := GetSymbol('glBindTexture');
Pointer(glDeleteTextures)         := GetSymbol('glDeleteTextures');
Pointer(glGenTextures)            := GetSymbol('glGenTextures');
Pointer(glIsTexture)              := GetSymbol('glIsTexture');

// OpenGL 1.2 non-deprecated functions
Pointer(glDrawRangeElements) := GetSymbolExt('glDrawRangeElements');
Pointer(glTexImage3D)        := GetSymbolExt('glTexImage3D');
Pointer(glTexSubImage3D)     := GetSymbolExt('glTexSubImage3D');
Pointer(glCopyTexSubImage3D) := GetSymbolExt('glCopyTexSubImage3D');

// OpenGL 1.3 non-deprecated functions
Pointer(glActiveTexture)            := GetSymbolExt('glActiveTexture');
Pointer(glSampleCoverage)           := GetSymbolExt('glSampleCoverage');
Pointer(glCompressedTexImage3D)     := GetSymbolExt('glCompressedTexImage3D');
Pointer(glCompressedTexImage2D)     := GetSymbolExt('glCompressedTexImage2D');
Pointer(glCompressedTexImage1D)     := GetSymbolExt('glCompressedTexImage1D');
Pointer(glCompressedTexSubImage3D)  := GetSymbolExt('glCompressedTexSubImage3D');
Pointer(glCompressedTexSubImage2D)  := GetSymbolExt('glCompressedTexSubImage2D');
Pointer(glCompressedTexSubImage1D)  := GetSymbolExt('glCompressedTexSubImage1D');
Pointer(glGetCompressedTexImage)    := GetSymbolExt('glGetCompressedTexImage');

// OpenGL 1.4 non-deprecated functions
Pointer(glBlendFuncSeparate):= GetSymbolExt('glBlendFuncSeparate');
Pointer(glMultiDrawArrays)  := GetSymbolExt('glMultiDrawArrays');
Pointer(glMultiDrawElements):= GetSymbolExt('glMultiDrawElements');
Pointer(glPointParameterf)  := GetSymbolExt('glPointParameterf');
Pointer(glPointParameterfv) := GetSymbolExt('glPointParameterfv');
Pointer(glPointParameteri)  := GetSymbolExt('glPointParameteri');
Pointer(glPointParameteriv) := GetSymbolExt('glPointParameteriv');
Pointer(glBlendColor)       := GetSymbolExt('glBlendColor');
Pointer(glBlendEquation)    := GetSymbolExt('glBlendEquation');

// OpenGL 1.5 non-deprecated functions
Pointer(glGenQueries)       := GetSymbolExt('glGenQueries');
Pointer(glDeleteQueries)    := GetSymbolExt('glDeleteQueries');
Pointer(glIsQuery)          := GetSymbolExt('glIsQuery');
Pointer(glBeginQuery)       := GetSymbolExt('glBeginQuery');
Pointer(glEndQuery)         := GetSymbolExt('glEndQuery');
Pointer(glGetQueryiv)       := GetSymbolExt('glGetQueryiv');
Pointer(glGetQueryObjectiv) := GetSymbolExt('glGetQueryObjectiv');
Pointer(glGetQueryObjectuiv):= GetSymbolExt('glGetQueryObjectuiv');
Pointer(glBindBuffer)       := GetSymbolExt('glBindBuffer');
Pointer(glDeleteBuffers)    := GetSymbolExt('glDeleteBuffers');
Pointer(glGenBuffers)       := GetSymbolExt('glGenBuffers');
Pointer(glIsBuffer)         := GetSymbolExt('glIsBuffer');
Pointer(glBufferData)       := GetSymbolExt('glBufferData');
Pointer(glBufferSubData)    := GetSymbolExt('glBufferSubData');
Pointer(glGetBufferSubData) := GetSymbolExt('glGetBufferSubData');
Pointer(glMapBuffer)        := GetSymbolExt('glMapBuffer');
Pointer(glUnmapBuffer)      := GetSymbolExt('glUnmapBuffer');
Pointer(glGetBufferParameteriv):= GetSymbolExt('glGetBufferParameteriv');
Pointer(glGetBufferPointerv):= GetSymbolExt('glGetBufferPointerv');

