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

implementation

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


end.

