// FreePascal Types
type
  GLenum      = Cardinal;
  GLfloat     = Single;
  GLint       = Integer;
  GLsizei     = Integer;
  GLbitfield  = Cardinal;
  GLdouble    = Double;
  GLuint      = Cardinal;
  GLboolean   = Byte;
  GLubyte     = Byte;
  GLchar      = Char;
  GLclampf    = Single;
  GLclampd    = Double;

  PGLvoid     = Pointer;
  PGLenum     = ^GLenum;
  PGLfloat    = ^GLfloat;
  PGLdouble   = ^GLdouble;
  PGLuint     = ^GLuint;
  PGLint      = ^GLint;
  PGLsizei    = ^GLsizei;
  PGLboolean  = ^GLboolean;
  PGLubyte    = ^GLubyte;
  PGLchar     = PChar;

  PPGLvoid    = ^PGLvoid;
  PPGLchar    = ^PGLchar;

  GLsizeiptr  = PtrInt;
  GLintptr    = PtrInt;

  PGLsizeiptr = ^PtrInt;
  PGLintptr   = ^PtrInt;

// OpenGL 1.1 non-deprecated constants
const
  GL_DEPTH_BUFFER_BIT            = $00000100;
  GL_STENCIL_BUFFER_BIT          = $00000400;
  GL_COLOR_BUFFER_BIT            = $00004000;
  GL_FALSE                       = 0;
  GL_TRUE                        = 1;
  GL_POINTS                      = $0000;
  GL_LINES                       = $0001;
  GL_LINE_LOOP                   = $0002;
  GL_LINE_STRIP                  = $0003;
  GL_TRIANGLES                   = $0004;
  GL_TRIANGLE_STRIP              = $0005;
  GL_TRIANGLE_FAN                = $0006;
  GL_QUADS                       = $0007;
  GL_NEVER                       = $0200;
  GL_LESS                        = $0201;
  GL_EQUAL                       = $0202;
  GL_LEQUAL                      = $0203;
  GL_GREATER                     = $0204;
  GL_NOTEQUAL                    = $0205;
  GL_GEQUAL                      = $0206;
  GL_ALWAYS                      = $0207;
  GL_ZERO                        = 0;
  GL_ONE                         = 1;
  GL_SRC_COLOR                   = $0300;
  GL_ONE_MINUS_SRC_COLOR         = $0301;
  GL_SRC_ALPHA                   = $0302;
  GL_ONE_MINUS_SRC_ALPHA         = $0303;
  GL_DST_ALPHA                   = $0304;
  GL_ONE_MINUS_DST_ALPHA         = $0305;
  GL_DST_COLOR                   = $0306;
  GL_ONE_MINUS_DST_COLOR         = $0307;
  GL_SRC_ALPHA_SATURATE          = $0308;
  GL_NONE                        = 0;
  GL_FRONT_LEFT                  = $0400;
  GL_FRONT_RIGHT                 = $0401;
  GL_BACK_LEFT                   = $0402;
  GL_BACK_RIGHT                  = $0403;
  GL_FRONT                       = $0404;
  GL_BACK                        = $0405;
  GL_LEFT                        = $0406;
  GL_RIGHT                       = $0407;
  GL_FRONT_AND_BACK              = $0408;
  GL_NO_ERROR                    = 0;
  GL_INVALID_ENUM                = $0500;
  GL_INVALID_VALUE               = $0501;
  GL_INVALID_OPERATION           = $0502;
  GL_OUT_OF_MEMORY               = $0505;
  GL_CW                          = $0900;
  GL_CCW                         = $0901;
  GL_POINT_SIZE                  = $0B11;
  GL_POINT_SIZE_RANGE            = $0B12;
  GL_POINT_SIZE_GRANULARITY      = $0B13;
  GL_LINE_SMOOTH                 = $0B20;
  GL_LINE_WIDTH                  = $0B21;
  GL_LINE_WIDTH_RANGE            = $0B22;
  GL_LINE_WIDTH_GRANULARITY      = $0B23;
  GL_POLYGON_MODE                = $0B40;
  GL_POLYGON_SMOOTH              = $0B41;
  GL_CULL_FACE                   = $0B44;
  GL_CULL_FACE_MODE              = $0B45;
  GL_FRONT_FACE                  = $0B46;
  GL_DEPTH_RANGE                 = $0B70;
  GL_DEPTH_TEST                  = $0B71;
  GL_DEPTH_WRITEMASK             = $0B72;
  GL_DEPTH_CLEAR_VALUE           = $0B73;
  GL_DEPTH_FUNC                  = $0B74;
  GL_STENCIL_TEST                = $0B90;
  GL_STENCIL_CLEAR_VALUE         = $0B91;
  GL_STENCIL_FUNC                = $0B92;
  GL_STENCIL_VALUE_MASK          = $0B93;
  GL_STENCIL_FAIL                = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL     = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS     = $0B96;
  GL_STENCIL_REF                 = $0B97;
  GL_STENCIL_WRITEMASK           = $0B98;
  GL_VIEWPORT                    = $0BA2;
  GL_DITHER                      = $0BD0;
  GL_BLEND_DST                   = $0BE0;
  GL_BLEND_SRC                   = $0BE1;
  GL_BLEND                       = $0BE2;
  GL_LOGIC_OP_MODE               = $0BF0;
  GL_COLOR_LOGIC_OP              = $0BF2;
  GL_DRAW_BUFFER                 = $0C01;
  GL_READ_BUFFER                 = $0C02;
  GL_SCISSOR_BOX                 = $0C10;
  GL_SCISSOR_TEST                = $0C11;
  GL_COLOR_CLEAR_VALUE           = $0C22;
  GL_COLOR_WRITEMASK             = $0C23;
  GL_DOUBLEBUFFER                = $0C32;
  GL_STEREO                      = $0C33;
  GL_LINE_SMOOTH_HINT            = $0C52;
  GL_POLYGON_SMOOTH_HINT         = $0C53;
  GL_UNPACK_SWAP_BYTES           = $0CF0;
  GL_UNPACK_LSB_FIRST            = $0CF1;
  GL_UNPACK_ROW_LENGTH           = $0CF2;
  GL_UNPACK_SKIP_ROWS            = $0CF3;
  GL_UNPACK_SKIP_PIXELS          = $0CF4;
  GL_UNPACK_ALIGNMENT            = $0CF5;
  GL_PACK_SWAP_BYTES             = $0D00;
  GL_PACK_LSB_FIRST              = $0D01;
  GL_PACK_ROW_LENGTH             = $0D02;
  GL_PACK_SKIP_ROWS              = $0D03;
  GL_PACK_SKIP_PIXELS            = $0D04;
  GL_PACK_ALIGNMENT              = $0D05;
  GL_MAX_TEXTURE_SIZE            = $0D33;
  GL_MAX_VIEWPORT_DIMS           = $0D3A;
  GL_SUBPIXEL_BITS               = $0D50;
  GL_TEXTURE_1D                  = $0DE0;
  GL_TEXTURE_2D                  = $0DE1;
  GL_POLYGON_OFFSET_UNITS        = $2A00;
  GL_POLYGON_OFFSET_POINT        = $2A01;
  GL_POLYGON_OFFSET_LINE         = $2A02;
  GL_POLYGON_OFFSET_FILL         = $8037;
  GL_POLYGON_OFFSET_FACTOR       = $8038;
  GL_TEXTURE_BINDING_1D          = $8068;
  GL_TEXTURE_BINDING_2D          = $8069;
  GL_TEXTURE_WIDTH               = $1000;
  GL_TEXTURE_HEIGHT              = $1001;
  GL_TEXTURE_INTERNAL_FORMAT     = $1003;
  GL_TEXTURE_BORDER_COLOR        = $1004;
  GL_TEXTURE_RED_SIZE            = $805C;
  GL_TEXTURE_GREEN_SIZE          = $805D;
  GL_TEXTURE_BLUE_SIZE           = $805E;
  GL_TEXTURE_ALPHA_SIZE          = $805F;
  GL_DONT_CARE                   = $1100;
  GL_FASTEST                     = $1101;
  GL_NICEST                      = $1102;
  GL_BYTE                        = $1400;
  GL_UNSIGNED_BYTE               = $1401;
  GL_SHORT                       = $1402;
  GL_UNSIGNED_SHORT              = $1403;
  GL_INT                         = $1404;
  GL_UNSIGNED_INT                = $1405;
  GL_FLOAT                       = $1406;
  GL_DOUBLE                      = $140A;
  GL_STACK_OVERFLOW              = $0503;
  GL_STACK_UNDERFLOW             = $0504;
  GL_CLEAR                       = $1500;
  GL_AND                         = $1501;
  GL_AND_REVERSE                 = $1502;
  GL_COPY                        = $1503;
  GL_AND_INVERTED                = $1504;
  GL_NOOP                        = $1505;
  GL_XOR                         = $1506;
  GL_OR                          = $1507;
  GL_NOR                         = $1508;
  GL_EQUIV                       = $1509;
  GL_INVERT                      = $150A;
  GL_OR_REVERSE                  = $150B;
  GL_COPY_INVERTED               = $150C;
  GL_OR_INVERTED                 = $150D;
  GL_NAND                        = $150E;
  GL_SET                         = $150F;
  GL_TEXTURE                     = $1702;
  GL_COLOR                       = $1800;
  GL_DEPTH                       = $1801;
  GL_STENCIL                     = $1802;
  GL_STENCIL_INDEX               = $1901;
  GL_DEPTH_COMPONENT             = $1902;
  GL_RED                         = $1903;
  GL_GREEN                       = $1904;
  GL_BLUE                        = $1905;
  GL_ALPHA                       = $1906;
  GL_RGB                         = $1907;
  GL_RGBA                        = $1908;
  GL_POINT                       = $1B00;
  GL_LINE                        = $1B01;
  GL_FILL                        = $1B02;
  GL_KEEP                        = $1E00;
  GL_REPLACE                     = $1E01;
  GL_INCR                        = $1E02;
  GL_DECR                        = $1E03;
  GL_VENDOR                      = $1F00;
  GL_RENDERER                    = $1F01;
  GL_VERSION                     = $1F02;
  GL_EXTENSIONS                  = $1F03;
  GL_NEAREST                     = $2600;
  GL_LINEAR                      = $2601;
  GL_NEAREST_MIPMAP_NEAREST      = $2700;
  GL_LINEAR_MIPMAP_NEAREST       = $2701;
  GL_NEAREST_MIPMAP_LINEAR       = $2702;
  GL_LINEAR_MIPMAP_LINEAR        = $2703;
  GL_TEXTURE_MAG_FILTER          = $2800;
  GL_TEXTURE_MIN_FILTER          = $2801;
  GL_TEXTURE_WRAP_S              = $2802;
  GL_TEXTURE_WRAP_T              = $2803;
  GL_PROXY_TEXTURE_1D            = $8063;
  GL_PROXY_TEXTURE_2D            = $8064;
  GL_REPEAT                      = $2901;
  GL_R3_G3_B2                    = $2A10;
  GL_RGB4                        = $804F;
  GL_RGB5                        = $8050;
  GL_RGB8                        = $8051;
  GL_RGB10                       = $8052;
  GL_RGB12                       = $8053;
  GL_RGB16                       = $8054;
  GL_RGBA2                       = $8055;
  GL_RGBA4                       = $8056;
  GL_RGB5_A1                     = $8057;
  GL_RGBA8                       = $8058;
  GL_RGB10_A2                    = $8059;
  GL_RGBA12                      = $805A;
  GL_RGBA16                      = $805B;
  GL_VERTEX_ARRAY                = $8074;

// OpenGL 1.2 non-deprecated constants
const
  GL_UNSIGNED_BYTE_3_3_2            = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4         = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1         = $8034;
  GL_UNSIGNED_INT_8_8_8_8           = $8035;
  GL_UNSIGNED_INT_10_10_10_2        = $8036;
  GL_TEXTURE_BINDING_3D             = $806A;
  GL_PACK_SKIP_IMAGES               = $806B;
  GL_PACK_IMAGE_HEIGHT              = $806C;
  GL_UNPACK_SKIP_IMAGES             = $806D;
  GL_UNPACK_IMAGE_HEIGHT            = $806E;
  GL_TEXTURE_3D                     = $806F;
  GL_PROXY_TEXTURE_3D               = $8070;
  GL_TEXTURE_DEPTH                  = $8071;
  GL_TEXTURE_WRAP_R                 = $8072;
  GL_MAX_3D_TEXTURE_SIZE            = $8073;
  GL_UNSIGNED_BYTE_2_3_3_REV        = $8362;
  GL_UNSIGNED_SHORT_5_6_5           = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV       = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV     = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV     = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV       = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV    = $8368;
  GL_BGR                            = $80E0;
  GL_BGRA                           = $80E1;
  GL_MAX_ELEMENTS_VERTICES          = $80E8;
  GL_MAX_ELEMENTS_INDICES           = $80E9;
  GL_CLAMP_TO_EDGE                  = $812F;
  GL_TEXTURE_MIN_LOD                = $813A;
  GL_TEXTURE_MAX_LOD                = $813B;
  GL_TEXTURE_BASE_LEVEL             = $813C;
  GL_TEXTURE_MAX_LEVEL              = $813D;
  GL_SMOOTH_POINT_SIZE_RANGE        = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY  = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE        = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY  = $0B23;
  GL_ALIASED_LINE_WIDTH_RANGE       = $846E;

// OpenGL 1.3 non-deprecated constants
const
  GL_TEXTURE0                      = $84C0;
  GL_TEXTURE1                      = $84C1;
  GL_TEXTURE2                      = $84C2;
  GL_TEXTURE3                      = $84C3;
  GL_TEXTURE4                      = $84C4;
  GL_TEXTURE5                      = $84C5;
  GL_TEXTURE6                      = $84C6;
  GL_TEXTURE7                      = $84C7;
  GL_TEXTURE8                      = $84C8;
  GL_TEXTURE9                      = $84C9;
  GL_TEXTURE10                     = $84CA;
  GL_TEXTURE11                     = $84CB;
  GL_TEXTURE12                     = $84CC;
  GL_TEXTURE13                     = $84CD;
  GL_TEXTURE14                     = $84CE;
  GL_TEXTURE15                     = $84CF;
  GL_TEXTURE16                     = $84D0;
  GL_TEXTURE17                     = $84D1;
  GL_TEXTURE18                     = $84D2;
  GL_TEXTURE19                     = $84D3;
  GL_TEXTURE20                     = $84D4;
  GL_TEXTURE21                     = $84D5;
  GL_TEXTURE22                     = $84D6;
  GL_TEXTURE23                     = $84D7;
  GL_TEXTURE24                     = $84D8;
  GL_TEXTURE25                     = $84D9;
  GL_TEXTURE26                     = $84DA;
  GL_TEXTURE27                     = $84DB;
  GL_TEXTURE28                     = $84DC;
  GL_TEXTURE29                     = $84DD;
  GL_TEXTURE30                     = $84DE;
  GL_TEXTURE31                     = $84DF;
  GL_ACTIVE_TEXTURE                = $84E0;
  GL_MULTISAMPLE                   = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE      = $809E;
  GL_SAMPLE_ALPHA_TO_ONE           = $809F;
  GL_SAMPLE_COVERAGE               = $80A0;
  GL_SAMPLE_BUFFERS                = $80A8;
  GL_SAMPLES                       = $80A9;
  GL_SAMPLE_COVERAGE_VALUE         = $80AA;
  GL_SAMPLE_COVERAGE_INVERT        = $80AB;
  GL_TEXTURE_CUBE_MAP              = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP      = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X   = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X   = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y   = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y   = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z   = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z   = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP        = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE     = $851C;
  GL_COMPRESSED_RGB                = $84ED;
  GL_COMPRESSED_RGBA               = $84EE;
  GL_TEXTURE_COMPRESSION_HINT      = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED            = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS    = $86A3;
  GL_CLAMP_TO_BORDER               = $812D;

// OpenGL 1.4 non-deprecated constants
const
  GL_BLEND_DST_RGB               = $80C8;
  GL_BLEND_SRC_RGB               = $80C9;
  GL_BLEND_DST_ALPHA             = $80CA;
  GL_BLEND_SRC_ALPHA             = $80CB;
  GL_POINT_FADE_THRESHOLD_SIZE   = $8128;
  GL_DEPTH_COMPONENT16           = $81A5;
  GL_DEPTH_COMPONENT24           = $81A6;
  GL_DEPTH_COMPONENT32           = $81A7;
  GL_MIRRORED_REPEAT             = $8370;
  GL_MAX_TEXTURE_LOD_BIAS        = $84FD;
  GL_TEXTURE_LOD_BIAS            = $8501;
  GL_INCR_WRAP                   = $8507;
  GL_DECR_WRAP                   = $8508;
  GL_TEXTURE_DEPTH_SIZE          = $884A;
  GL_TEXTURE_COMPARE_MODE        = $884C;
  GL_TEXTURE_COMPARE_FUNC        = $884D;
  GL_FUNC_ADD                    = $8006;
  GL_FUNC_SUBTRACT               = $800A;
  GL_FUNC_REVERSE_SUBTRACT       = $800B;
  GL_MIN                         = $8007;
  GL_MAX                         = $8008;
  GL_CONSTANT_COLOR              = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR    = $8002;
  GL_CONSTANT_ALPHA              = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA    = $8004;

// OpenGL 1.5 non-deprecated constants
const
  GL_BUFFER_SIZE                        = $8764;
  GL_BUFFER_USAGE                       = $8765;
  GL_QUERY_COUNTER_BITS                 = $8864;
  GL_CURRENT_QUERY                      = $8865;
  GL_QUERY_RESULT                       = $8866;
  GL_QUERY_RESULT_AVAILABLE             = $8867;
  GL_ARRAY_BUFFER                       = $8892;
  GL_ELEMENT_ARRAY_BUFFER               = $8893;
  GL_ARRAY_BUFFER_BINDING               = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING       = $8895;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY                          = $88B8;
  GL_WRITE_ONLY                         = $88B9;
  GL_READ_WRITE                         = $88BA;
  GL_BUFFER_ACCESS                      = $88BB;
  GL_BUFFER_MAPPED                      = $88BC;
  GL_BUFFER_MAP_POINTER                 = $88BD;
  GL_STREAM_DRAW                        = $88E0;
  GL_STREAM_READ                        = $88E1;
  GL_STREAM_COPY                        = $88E2;
  GL_STATIC_DRAW                        = $88E4;
  GL_STATIC_READ                        = $88E5;
  GL_STATIC_COPY                        = $88E6;
  GL_DYNAMIC_DRAW                       = $88E8;
  GL_DYNAMIC_READ                       = $88E9;
  GL_DYNAMIC_COPY                       = $88EA;
  GL_SAMPLES_PASSED                     = $8914;
  GL_SRC1_ALPHA                         = $8589;

