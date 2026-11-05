{$INCLUDE valkyrie.inc}
unit vgltypes;
interface
uses Classes, SysUtils, vvector;

type
  TGLVec2f        = TVec2f;
  TGLVec3f        = TVec3f;
  TGLVec4f        = TVec4f;

  TGLVec2i        = TVec2i;
  TGLVec3i        = TVec3i;
  TGLVec4i        = TVec4i;

  TGLVec2b        = TVec2b;
  TGLVec3b        = TVec3b;
  TGLVec4b        = TVec4b;

  TGLRawQCoord    = specialize TGVectorQuad<TGLVec2i,Integer>;
  TGLRawQTexCoord = specialize TGVectorQuad<TGLVec2f,Single>;
  TGLRawQColor    = specialize TGVectorQuad<TGLVec3b,Byte>;
  TGLRawQColor4f  = specialize TGVectorQuad<TGLVec4f,Single>;

  PGLRawQCoord    = ^TGLRawQCoord;
  PGLRawQTexCoord = ^TGLRawQTexCoord;
  PGLRawQColor    = ^TGLRawQColor;

  TGLByteColor    = TGLVec3b;
  TGLFloatColor   = TGLVec3f;
  TGLFloatColor4  = TGLVec4f;

  TMatrix44       = array[0..15] of Single;

  TVertex2f2f     = record
    position : TVec2f;
    texcoord : TVec2f;
  end;

const VGL_POSITION_LOCATION = 0;
      VGL_TEXCOORD_LOCATION = 1;
      VGL_COLOR_LOCATION    = 2;
      VGL_COLOR2_LOCATION   = 3;
      VGL_COLOR3_LOCATION   = 4;
      VGL_COLOR4_LOCATION   = 5;

// Inline
function GLVec2f( aX : Single = 0; aY : Single = 0 ) : TGLVec2f; overload;
function GLVec3f( aX : Single = 0; aY : Single = 0; aZ : Single = 0 ) : TGLVec3f; overload;
function GLVec4f( aX : Single = 0; aY : Single = 0; aZ : Single = 0; aW : Single = 0 ) : TGLVec4f; overload;
// Convert
function GLVec2f( const aGLVec2i : TGLVec2i ) : TGLVec2f; overload;
function GLVec3f( const aGLVec3i : TGLVec3i ) : TGLVec3f; overload;
function GLVec4f( const aGLVec4i : TGLVec4i ) : TGLVec4f; overload;
// Expand
function GLVec3f( const aGLVec2f : TGLVec2f; aZ : Single = 0 ) : TGLVec3f; overload;
function GLVec4f( const aGLVec3f : TGLVec3f; aW : Single = 0 ) : TGLVec4f; overload;

// Lerp support
function Lerp( const a, b : TGLVec2f; aValue : Single ) : TGLVec2f; overload;
function Lerp( const a, b : TGLVec3f; aValue : Single ) : TGLVec3f; overload;

// Inline
function GLVec2i( aX : Integer = 0; aY : Integer = 0 ) : TGLVec2i; overload;
function GLVec3i( aX : Integer = 0; aY : Integer = 0; aZ : Integer = 0 ) : TGLVec3i; overload;
function GLVec4i( aX : Integer = 0; aY : Integer = 0; aZ : Integer = 0; aW : Integer = 0 ) : TGLVec4i; overload;
// Expand
function GLVec3i( const aGLVec2i : TGLVec2i; aZ : Integer = 0 ) : TGLVec3i; overload;
function GLVec4i( const aGLVec3i : TGLVec3i; aW : Integer = 0 ) : TGLVec4i; overload;

// Lerp support
function Lerp( const a, b : TGLVec2i; aValue : Single ) : TGLVec2i; overload;
function Lerp( const a, b : TGLVec3i; aValue : Single ) : TGLVec3i; overload;

function GLCreateOrtho( left, right, bottom, top, nearVal, farVal: Single ) : TMatrix44;

type TGLPixelFormat = ( RGB8, RGBA8, R8, DEPTH16, DEPTH24, DEPTH32, R8I, R8UI, R16F, R32F, RGBA16F, RGBA32F );

function GLPixelFormatIsDepth( aFormat : TGLPixelFormat ) : Boolean;
function GLPixelFormatToEnum( aFormat : TGLPixelFormat ) : Integer;
function GLPixelFormatToInternalEnum( aFormat : TGLPixelFormat ) : Integer;
function GLPixelFormatToSize( aFormat : TGLPixelFormat ) : Integer;
function GLPixelFormatToType( aFormat : TGLPixelFormat ) : Integer;

implementation

uses vgl3library;

function GLVec2f(aX: Single; aY: Single): TGLVec2f;
begin
  GLVec2f.Data[0] := aX;
  GLVec2f.Data[1] := aY;
end;

function GLVec3f(aX: Single; aY: Single; aZ: Single): TGLVec3f;
begin
  GLVec3f.Data[0] := aX;
  GLVec3f.Data[1] := aY;
  GLVec3f.Data[2] := aZ;
end;

function GLVec4f(aX: Single; aY: Single; aZ: Single; aW: Single): TGLVec4f;
begin
  GLVec4f.Data[0] := aX;
  GLVec4f.Data[1] := aY;
  GLVec4f.Data[2] := aZ;
  GLVec4f.Data[3] := aW;
end;

function GLVec2f( const aGLVec2i: TGLVec2i): TGLVec2f;
begin
  GLVec2f.Data[0] := aGLVec2i.Data[0];
  GLVec2f.Data[1] := aGLVec2i.Data[1];
end;

function GLVec3f( const aGLVec3i: TGLVec3i): TGLVec3f;
begin
  GLVec3f.Data[0] := aGLVec3i.Data[0];
  GLVec3f.Data[1] := aGLVec3i.Data[1];
  GLVec3f.Data[2] := aGLVec3i.Data[2];
end;

function GLVec4f( const aGLVec4i: TGLVec4i): TGLVec4f;
begin
  GLVec4f.Data[0] := aGLVec4i.Data[0];
  GLVec4f.Data[1] := aGLVec4i.Data[1];
  GLVec4f.Data[2] := aGLVec4i.Data[2];
  GLVec4f.Data[3] := aGLVec4i.Data[3];
end;

function GLVec3f( const aGLVec2f: TGLVec2f; aZ: Single): TGLVec3f;
begin
  GLVec3f.Data[0] := aGLVec2f.Data[0];
  GLVec3f.Data[1] := aGLVec2f.Data[1];
  GLVec3f.Data[2] := aZ;
end;

function GLVec4f( const aGLVec3f: TGLVec3f; aW: Single): TGLVec4f;
begin
  GLVec4f.Data[0] := aGLVec3f.Data[0];
  GLVec4f.Data[1] := aGLVec3f.Data[1];
  GLVec4f.Data[2] := aGLVec3f.Data[2];
  GLVec4f.Data[3] := aW;
end;

function Lerp( const a, b: TGLVec2f; aValue: Single): TGLVec2f;
begin
  Lerp.Data[0] := a.Data[0] + (b.Data[0] - a.Data[0]) * aValue;
  Lerp.Data[1] := a.Data[1] + (b.Data[1] - a.Data[1]) * aValue;
end;

function Lerp( const a, b: TGLVec3f; aValue: Single): TGLVec3f;
begin
  Lerp.Data[0] := a.Data[0] + (b.Data[0] - a.Data[0]) * aValue;
  Lerp.Data[1] := a.Data[1] + (b.Data[1] - a.Data[1]) * aValue;
  Lerp.Data[2] := a.Data[2] + (b.Data[2] - a.Data[2]) * aValue;
end;

// Integer

function GLVec2i(aX: Integer; aY: Integer): TGLVec2i;
begin
  GLVec2i.Data[0] := aX;
  GLVec2i.Data[1] := aY;
end;

function GLVec3i(aX: Integer; aY: Integer; aZ: Integer): TGLVec3i;
begin
  GLVec3i.Data[0] := aX;
  GLVec3i.Data[1] := aY;
  GLVec3i.Data[2] := aZ;
end;

function GLVec4i(aX: Integer; aY: Integer; aZ: Integer; aW: Integer): TGLVec4i;
begin
  GLVec4i.Data[0] := aX;
  GLVec4i.Data[1] := aY;
  GLVec4i.Data[2] := aZ;
  GLVec4i.Data[3] := aW;
end;

function GLVec3i( const aGLVec2i: TGLVec2i; aZ: Integer): TGLVec3i;
begin
  GLVec3i.Data[0] := aGLVec2i.Data[0];
  GLVec3i.Data[1] := aGLVec2i.Data[1];
  GLVec3i.Data[2] := aZ;
end;

function GLVec4i( const aGLVec3i: TGLVec3i; aW: Integer): TGLVec4i;
begin
  GLVec4i.Data[0] := aGLVec3i.Data[0];
  GLVec4i.Data[1] := aGLVec3i.Data[1];
  GLVec4i.Data[2] := aGLVec3i.Data[2];
  GLVec4i.Data[3] := aW;
end;

function Lerp(const a, b: TGLVec2i; aValue: Single): TGLVec2i;
begin
  Lerp.Data[0] := a.Data[0] + Round( (b.Data[0] - a.Data[0]) * aValue );
  Lerp.Data[1] := a.Data[1] + Round( (b.Data[1] - a.Data[1]) * aValue );
end;

function Lerp(const a, b: TGLVec3i; aValue: Single): TGLVec3i;
begin
  Lerp.Data[0] := a.Data[0] + Round( (b.Data[0] - a.Data[0]) * aValue );
  Lerp.Data[1] := a.Data[1] + Round( (b.Data[1] - a.Data[1]) * aValue );
  Lerp.Data[2] := a.Data[2] + Round( (b.Data[2] - a.Data[2]) * aValue );
end;

function GLCreateOrtho( left, right, bottom, top, nearVal, farVal: Single ) : TMatrix44;
var
  tx, ty, tz: Single;
begin
  tx := - (right + left) / (right - left);
  ty := - (top + bottom) / (top - bottom);
  tz := - (farVal + nearVal) / (farVal - nearVal);

  Result[0] := 2 / (right - left);  Result[4] := 0;                    Result[8] := 0;                   Result[12] := tx;
  Result[1] := 0;                   Result[5] := 2 / (top - bottom);   Result[9] := 0;                   Result[13] := ty;
  Result[2] := 0;                   Result[6] := 0;                    Result[10] := -2 / (farVal - nearVal); Result[14] := tz;
  Result[3] := 0;                   Result[7] := 0;                    Result[11] := 0;                  Result[15] := 1;
end;

function GLPixelFormatIsDepth( aFormat : TGLPixelFormat ) : Boolean;
begin
  Exit( aFormat in [ DEPTH16, DEPTH24, DEPTH32 ] );
end;

function GLPixelFormatToEnum( aFormat : TGLPixelFormat ) : Integer;
const Enums : array[TGLPixelFormat] of Integer = (
  { RGB8    } GL_RGB,
  { RGBA8   } GL_RGBA,
  { R8      } GL_RED,
  { DEPTH16 } GL_DEPTH_COMPONENT,
  { DEPTH24 } GL_DEPTH_COMPONENT,
  { DEPTH32 } GL_DEPTH_COMPONENT,
  { R8I     } GL_RED_INTEGER,
  { R8UI    } GL_RED_INTEGER,
  { R16F    } GL_RED,
  { R32F    } GL_RED,
  { RGBA16F } GL_RGBA,
  { RGBA32F } GL_RGBA );
begin
  Exit( Enums[aFormat] );
end;

function GLPixelFormatToInternalEnum( aFormat : TGLPixelFormat ) : Integer;
const InternalEnums : array[TGLPixelFormat] of Integer = (
  { RGB8    } GL_RGB8,
  { RGBA8   } GL_RGBA8,
  { R8      } GL_R8,
  { DEPTH16 } GL_DEPTH_COMPONENT16,
  { DEPTH24 } GL_DEPTH_COMPONENT24,
  { DEPTH32 } GL_DEPTH_COMPONENT32,
  { R8I     } GL_R8I,
  { R8UI    } GL_R8UI,
  { R16F    } GL_R16F,
  { R32F    } GL_R32F,
  { RGBA16F } GL_RGBA16F,
  { RGBA32F } GL_RGBA32F );
begin
  Exit( InternalEnums[aFormat] );
end;

function GLPixelFormatToSize( aFormat : TGLPixelFormat ) : Integer;
const Sizes : array[TGLPixelFormat] of Integer = (
  { RGB8    } 3,
  { RGBA8   } 4,
  { R8      } 1,
  { DEPTH16 } 1,
  { DEPTH24 } 1,
  { DEPTH32 } 1,
  { R8I     } 1,
  { R8UI    } 1,
  { R16F    } 1,
  { R32F    } 1,
  { RGBA16F } 4,
  { RGBA32F } 4 );
begin
  Exit( Sizes[aFormat] );
end;

function GLPixelFormatToType( aFormat : TGLPixelFormat ) : Integer;
const DataTypes : array[TGLPixelFormat] of Integer = (
  { RGB8    } GL_UNSIGNED_BYTE,
  { RGBA8   } GL_UNSIGNED_BYTE,
  { R8      } GL_UNSIGNED_BYTE,
  { DEPTH16 } GL_UNSIGNED_SHORT,
  { DEPTH24 } GL_UNSIGNED_INT,
  { DEPTH32 } GL_UNSIGNED_INT,
  { R8I     } GL_BYTE,
  { R8UI    } GL_UNSIGNED_BYTE,
  { R16F    } GL_HALF_FLOAT,
  { R32F    } GL_FLOAT,
  { RGBA16F } GL_HALF_FLOAT,
  { RGBA32F } GL_FLOAT );
begin
  Exit( DataTypes[aFormat] );
end;

end.

