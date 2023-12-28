type
  GLbyte       = ShortInt;
  GLshort      = SmallInt;
  GLushort     = Word;

  PGLbyte      = ^GLbyte;
  PGLshort     = ^GLshort;
  PGLushort    = ^GLushort;

// OpenGL 2.0 non-deprecated constants
const
  GL_BLEND_EQUATION_RGB                  = $8009;
  GL_BLEND_EQUATION                      = $8009;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED         = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE            = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE          = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE            = $8625;
  GL_CURRENT_VERTEX_ATTRIB               = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE           = $8642;
  GL_VERTEX_ATTRIB_ARRAY_POINTER         = $8645;
  GL_STENCIL_BACK_FUNC                   = $8800;
  GL_STENCIL_BACK_FAIL                   = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL        = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS        = $8803;
  GL_MAX_DRAW_BUFFERS                    = $8824;
  GL_DRAW_BUFFER0                        = $8825;
  GL_DRAW_BUFFER1                        = $8826;
  GL_DRAW_BUFFER2                        = $8827;
  GL_DRAW_BUFFER3                        = $8828;
  GL_DRAW_BUFFER4                        = $8829;
  GL_DRAW_BUFFER5                        = $882A;
  GL_DRAW_BUFFER6                        = $882B;
  GL_DRAW_BUFFER7                        = $882C;
  GL_DRAW_BUFFER8                        = $882D;
  GL_DRAW_BUFFER9                        = $882E;
  GL_DRAW_BUFFER10                       = $882F;
  GL_DRAW_BUFFER11                       = $8830;
  GL_DRAW_BUFFER12                       = $8831;
  GL_DRAW_BUFFER13                       = $8832;
  GL_DRAW_BUFFER14                       = $8833;
  GL_DRAW_BUFFER15                       = $8834;
  GL_BLEND_EQUATION_ALPHA                = $883D;
  GL_MAX_VERTEX_ATTRIBS                  = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED      = $886A;
  GL_MAX_TEXTURE_IMAGE_UNITS             = $8872;
  GL_FRAGMENT_SHADER                     = $8B30;
  GL_VERTEX_SHADER                       = $8B31;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS     = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS       = $8B4A;
  GL_MAX_VARYING_FLOATS                  = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS      = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS    = $8B4D;
  GL_SHADER_TYPE                         = $8B4F;
  GL_FLOAT_VEC2                          = $8B50;
  GL_FLOAT_VEC3                          = $8B51;
  GL_FLOAT_VEC4                          = $8B52;
  GL_INT_VEC2                            = $8B53;
  GL_INT_VEC3                            = $8B54;
  GL_INT_VEC4                            = $8B55;
  GL_BOOL                                = $8B56;
  GL_BOOL_VEC2                           = $8B57;
  GL_BOOL_VEC3                           = $8B58;
  GL_BOOL_VEC4                           = $8B59;
  GL_FLOAT_MAT2                          = $8B5A;
  GL_FLOAT_MAT3                          = $8B5B;
  GL_FLOAT_MAT4                          = $8B5C;
  GL_SAMPLER_1D                          = $8B5D;
  GL_SAMPLER_2D                          = $8B5E;
  GL_SAMPLER_3D                          = $8B5F;
  GL_SAMPLER_CUBE                        = $8B60;
  GL_SAMPLER_1D_SHADOW                   = $8B61;
  GL_SAMPLER_2D_SHADOW                   = $8B62;
  GL_DELETE_STATUS                       = $8B80;
  GL_COMPILE_STATUS                      = $8B81;
  GL_LINK_STATUS                         = $8B82;
  GL_VALIDATE_STATUS                     = $8B83;
  GL_INFO_LOG_LENGTH                     = $8B84;
  GL_ATTACHED_SHADERS                    = $8B85;
  GL_ACTIVE_UNIFORMS                     = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH           = $8B87;
  GL_SHADER_SOURCE_LENGTH                = $8B88;
  GL_ACTIVE_ATTRIBUTES                   = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH         = $8B8A;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT     = $8B8B;
  GL_SHADING_LANGUAGE_VERSION            = $8B8C;
  GL_CURRENT_PROGRAM                     = $8B8D;
  GL_POINT_SPRITE_COORD_ORIGIN           = $8CA0;
  GL_LOWER_LEFT                          = $8CA1;
  GL_UPPER_LEFT                          = $8CA2;
  GL_STENCIL_BACK_REF                    = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK             = $8CA4;
  GL_STENCIL_BACK_WRITEMASK              = $8CA5;

// OpenGL 2.1 non-deprecated constants
const
  GL_PIXEL_PACK_BUFFER             = $88EB;
  GL_PIXEL_UNPACK_BUFFER           = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING     = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING   = $88EF;
  GL_FLOAT_MAT2x3                  = $8B65;
  GL_FLOAT_MAT2x4                  = $8B66;
  GL_FLOAT_MAT3x2                  = $8B67;
  GL_FLOAT_MAT3x4                  = $8B68;
  GL_FLOAT_MAT4x2                  = $8B69;
  GL_FLOAT_MAT4x3                  = $8B6A;
  GL_SRGB                          = $8C40;
  GL_SRGB8                         = $8C41;
  GL_SRGB_ALPHA                    = $8C42;
  GL_SRGB8_ALPHA8                  = $8C43;
  GL_COMPRESSED_SRGB               = $8C48;
  GL_COMPRESSED_SRGB_ALPHA         = $8C49;
