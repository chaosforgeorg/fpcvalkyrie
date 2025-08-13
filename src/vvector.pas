{$INCLUDE valkyrie.inc}
{$HINTS OFF}
unit vvector;
interface
uses Classes, SysUtils;

type
  generic TGVec2<T> = packed record
  type
    TData    = packed array[0..1] of T;
    TPointer = ^T;
  var
    Data : TData;
  public
    class function Create( aX : T = 0; aY : T = 0 ) : TGVec2; static;
    class function CreateAll( A : T ) : TGVec2; static;
    class function CreateModDiv( A,B : LongInt ) : TGVec2; static;
    procedure Init( aX : T = 0; aY : T = 0 );
    function ScaledF( A : Single ) : TGVec2;
    function Scaled( A : T ) : TGVec2;
    function Shifted( A : T ) : TGVec2;
    function Length() : Single;
    function LengthSq() : T;
    function Distance( A : TGVec2 ) : Single;
    class operator +( const A, B : TGVec2 ) : TGVec2;
    class operator -( const A, B : TGVec2 ) : TGVec2;
    class operator *( const A, B : TGVec2 ) : TGVec2;
    class operator **( const A, B : TGVec2 ) : T;
    class operator -( const A : TGVec2 ) : TGVec2;
    class operator :=( const A : TGVec2 ) : TData;
    class operator :=( const A : TData ) : TGVec2;
    class operator Explicit( const A : array of T ) : TGVec2;
  public
    property X : T read Data[0] write Data[0];
    property Y : T read Data[1] write Data[1];
  end;

  { TGVec3 }

  generic TGVec3<T> = packed record
  type
    TData = packed array[0..2] of T;
    TVec2 = specialize TGVec2<T>;
  var
    Data : TData;
  public
    class function Create( aX : T = 0; aY : T = 0; aZ : T = 0 ) : TGVec3; static;
    class function CreateFrom( const aVec2 : TVec2; aZ : T = 0 ) : TGVec3; static;
    class function CreateAll( A : T ) : TGVec3; static;
    procedure Init( aX : T = 0; aY : T = 0; aZ : T = 0 );
    function ScaledF( A : Single ) : TGVec3;
    function Scaled( A : T ) : TGVec3;
    function Shifted( A : T ) : TGVec3;
    function Length() : Single;
    function LengthSq() : T;
    function Distance( A : TGVec3 ) : Single;
    class operator +( const A, B : TGVec3 ) : TGVec3;
    class operator -( const A, B : TGVec3 ) : TGVec3;
    class operator *( const A, B : TGVec3 ) : TGVec3;
    class operator **( const A, B : TGVec3 ) : T;
    class operator -( const A : TGVec3 ) : TGVec3;
    class operator :=( const A : TGVec3 ) : TData;
    class operator :=( const A : TData ) : TGVec3;
    class operator Explicit( const A : array of T ) : TGVec3;
  private
    function GetXY : TVec2;
    procedure SetXY( const aValue : TVec2 );
  public
    property XY  : TVec2 read GetXY write SetXY;
    property X   : T     read Data[0] write Data[0];
    property Y   : T     read Data[1] write Data[1];
    property Z   : T     read Data[2] write Data[2];
  end;

  generic TGVec4<T> = packed record
  type
    TData = packed array[0..3] of T;
    TVec2 = specialize TGVec2<T>;
    TVec3 = specialize TGVec3<T>;
  var
    Data : TData;
  public
    class function Create( aX : T = 0; aY : T = 0; aZ : T = 0; aW : T = 0 ) : TGVec4; static;
    class function CreateAll( A : T ) : TGVec4; static;
    function ScaledF( A : Single ) : TGVec4;
    function Scaled( A : T ) : TGVec4;
    function Shifted( A : T ) : TGVec4;
    procedure Init( aX : T = 0; aY : T = 0; aZ : T = 0; aW : T = 0 );
    class operator +( const A, B : TGVec4 ) : TGVec4;
    class operator -( const A, B : TGVec4 ) : TGVec4;
    class operator *( const A, B : TGVec4 ) : TGVec4;
    class operator **( const A, B : TGVec4 ) : T;
    class operator -( const A : TGVec4 ) : TGVec4;
    class operator :=( const A : TGVec4 ) : TData;
    class operator :=( const A : TData ) : TGVec4;
    class operator Explicit( const A : array of T ) : TGVec4;
  private
    function GetXY : TVec2;
    procedure SetXY( const aValue : TVec2 );
    function GetXYZ : TVec3;
    procedure SetXYZ( const aValue : TVec3 );
  public
    property XY  : TVec2 read GetXY   write SetXY;
    property XYZ : TVec3 read GetXYZ  write SetXYZ;
    property X   : T     read Data[0] write Data[0];
    property Y   : T     read Data[1] write Data[1];
    property Z   : T     read Data[2] write Data[2];
    property W   : T     read Data[3] write Data[3];
  end;

  TVec2f = specialize TGVec2<Single>;
  PVec2f = ^TVec2f;
  TVec3f = specialize TGVec3<Single>;
  PVec3f = ^TVec3f;
  TVec4f = specialize TGVec4<Single>;
  PVec4f = ^TVec4f;

  TVec2d = specialize TGVec2<Double>;
  PVec2d = ^TVec2d;
  TVec3d = specialize TGVec3<Double>;
  PVec3d = ^TVec3d;
  TVec4d = specialize TGVec4<Double>;
  PVec4d = ^TVec4d;

  TVec2b = specialize TGVec2<Byte>;
  PVec2b = ^TVec2b;
  TVec3b = specialize TGVec3<Byte>;
  PVec3b = ^TVec3b;
  TVec4b = specialize TGVec4<Byte>;
  PVec4b = ^TVec4b;

  TVec2i = specialize TGVec2<LongInt>;
  PVec2i = ^TVec2i;
  TVec3i = specialize TGVec3<LongInt>;
  PVec3i = ^TVec3i;
  TVec4i = specialize TGVec4<LongInt>;
  PVec4i = ^TVec4i;

  generic TGVectorQuad<T,TE> = packed object
  type
    TData = packed array[0..3] of T;
  var
    Data : TData;
    class function Create( const q0, q1, q2, q3 : T ) : TGVectorQuad; static;
    procedure Init( const q0, q2 : T );
    procedure SetAll( const q : T );
    procedure FillAll( const q : TE );
  end;

  generic TGVectorTriQuad<T,TE> = packed object
  type
    TData = packed array[0..5] of T;
  var
    Data : TData;
    class function Create( const q0, q1, q2, q3 : T ) : TGVectorTriQuad; static;
    class function CreateInit( const q0, q2 : T ) : TGVectorTriQuad; static;
    class function CreateAll( const q0 : T ) : TGVectorTriQuad; static;
    procedure Init( const q0, q2 : T );
    procedure SetAll( const q : T );
    procedure FillAll( const q : TE );
  end;

// Inline
function Vec2f( aX : Single = 0; aY : Single = 0 ) : TVec2f; overload;
function Vec3f( aX : Single = 0; aY : Single = 0; aZ : Single = 0 ) : TVec3f; overload;
function Vec4f( aX : Single = 0; aY : Single = 0; aZ : Single = 0; aW : Single = 0 ) : TVec4f; overload;
// Convert
function Vec2f( const aVec2i : TVec2i ) : TVec2f; overload;
function Vec3f( const aVec3i : TVec3i ) : TVec3f; overload;
function Vec4f( const aVec4i : TVec4i ) : TVec4f; overload;
// Expand
function Vec3f( const aVec2f : TVec2f; aZ : Single = 0 ) : TVec3f; overload;
function Vec4f( const aVec3f : TVec3f; aW : Single = 0 ) : TVec4f; overload;

// Lerp support
function Lerp( const a, b : TVec2f; aValue : Single ) : TVec2f; overload;
function Lerp( const a, b : TVec3f; aValue : Single ) : TVec3f; overload;

// Clamp support
function Clamp( const x, min, max : TVec2f ) : TVec2f; overload;
function Clamp( const x, min, max : TVec3f ) : TVec3f; overload;
function Clamp( const x, min, max : TVec4f ) : TVec4f; overload;

// Ceil and floor
function Ceilf( const aVec2f : TVec2f ) : TVec2f; overload;
function Ceilf( const aVec3f : TVec3f ) : TVec3f; overload;
function Ceilf( const aVec4f : TVec4f ) : TVec4f; overload;
function Floorf( const aVec2f : TVec2f ) : TVec2f; overload;
function Floorf( const aVec3f : TVec3f ) : TVec3f; overload;
function Floorf( const aVec4f : TVec4f ) : TVec4f; overload;
function Roundf( const aVec2f : TVec2f ) : TVec2f; overload;
function Roundf( const aVec3f : TVec3f ) : TVec3f; overload;
function Roundf( const aVec4f : TVec4f ) : TVec4f; overload;

// Inline
function Vec2i( aX : Integer = 0; aY : Integer = 0 ) : TVec2i; overload;
function Vec3i( aX : Integer = 0; aY : Integer = 0; aZ : Integer = 0 ) : TVec3i; overload;
function Vec4i( aX : Integer = 0; aY : Integer = 0; aZ : Integer = 0; aW : Integer = 0 ) : TVec4i; overload;
// Expand
function Vec3i( const aVec2i : TVec2i; aZ : Integer = 0 ) : TVec3i; overload;
function Vec4i( const aVec3i : TVec3i; aW : Integer = 0 ) : TVec4i; overload;
  
// Lerp support
function Lerp( const a, b : TVec2i; aValue : Single ) : TVec2i; overload;
function Lerp( const a, b : TVec3i; aValue : Single ) : TVec3i; overload;

// Ceil and floor I
function Ceil( const aVec2f : TVec2f ) : TVec2i; overload;
function Ceil( const aVec3f : TVec3f ) : TVec3i; overload;
function Ceil( const aVec4f : TVec4f ) : TVec4i; overload;
function Floor( const aVec2f : TVec2f ) : TVec2i; overload;
function Floor( const aVec3f : TVec3f ) : TVec3i; overload;
function Floor( const aVec4f : TVec4f ) : TVec4i; overload;
function Round( const aVec2f : TVec2f ) : TVec2i; overload;
function Round( const aVec3f : TVec3f ) : TVec3i; overload;
function Round( const aVec4f : TVec4f ) : TVec4i; overload;

// Clamp support
function Clamp( const x, min, max : TVec2i ) : TVec2i; overload;
function Clamp( const x, min, max : TVec3i ) : TVec3i; overload;
function Clamp( const x, min, max : TVec4i ) : TVec4i; overload;

implementation

uses math, vmath;

// --------------------- TGVEC 2 --------------------- //

class function TGVec2.Create( aX : T = 0; aY : T = 0 ) : TGVec2; static;
begin
  Result.Data[0] := aX;
  Result.Data[1] := aY;
end;

class function TGVec2.CreateAll( A : T ) : TGVec2; static;
begin
  Result.Data[0] := A;
  Result.Data[1] := A;
end;

class function TGVec2.CreateModDiv( A,B : LongInt ) : TGVec2; static;
begin
  Result.Data[0] := A mod B;
  Result.Data[1] := A div B;
end;

procedure TGVec2.Init( aX : T = 0; aY : T = 0 );
begin
  Data[0] := aX;
  Data[1] := aY;
end;

function TGVec2.ScaledF( A : Single ) : TGVec2;
begin
  Result.Data[0] := T( Floor( Data[0] * A ) );
  Result.Data[1] := T( Floor( Data[1] * A ) );
end;

function TGVec2.Scaled( A : T ) : TGVec2;
begin
  Result.Data[0] := Data[0] * A;
  Result.Data[1] := Data[1] * A;
end;

function TGVec2.Shifted( A : T ) : TGVec2;
begin
  Result.Data[0] := Data[0] + A;
  Result.Data[1] := Data[1] + A;
end;

function TGVec2.Length() : Single;
begin
  Exit( Sqrt( Data[0] * Data[0] + Data[1] * Data[1] ) );
end;

function TGVec2.LengthSq() : T;
begin
  Exit( Data[0] * Data[0] + Data[1] * Data[1] );
end;

function TGVec2.Distance( A : TGVec2 ) : Single;
begin
  Exit( Sqrt(Sqr(Abs(Single(A.Data[0]-Data[0]))) + Sqr(Abs(Single(A.Data[1]-Data[1])))));
end;

class operator TGVec2.+( const A, B : TGVec2 ) : TGVec2;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
end;

class operator TGVec2.-( const A, B : TGVec2 ) : TGVec2;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
end;

class operator TGVec2.*( const A, B : TGVec2 ) : TGVec2;
begin
  Result.Data[0] := A.Data[0] * B.Data[0];
  Result.Data[1] := A.Data[1] * B.Data[1];
end;

class operator TGVec2.**( const A, B : TGVec2 ) : T;
begin
  Result := A.Data[0] * B.Data[0]
          + A.Data[1] * B.Data[1];
end;

class operator TGVec2.-( const A : TGVec2 ) : TGVec2;
begin
  Result.Data[0] := -A.Data[0];
  Result.Data[1] := -A.Data[1];
end;

class operator TGVec2.:=( const A : TGVec2 ) : TData;
begin
  Result[0] := A.Data[0];
  Result[1] := A.Data[1];
end;

class operator TGVec2.:=( const A : TData ) : TGVec2;
begin
  Result.Data := A;
end;

class operator TGVec2.Explicit( const A : array of T ) : TGVec2;
begin
  Result.Data[0] := A[0];
  Result.Data[1] := A[1];
end;


// --------------------- TGVEC 3 --------------------- //

class function TGVec3.Create( aX : T = 0; aY : T = 0; aZ : T = 0 ) : TGVec3; static;
begin
  Result.Data[0] := aX;
  Result.Data[1] := aY;
  Result.Data[2] := aZ;
end;

class function TGVec3.CreateFrom( const aVec2: TVec2; aZ : T = 0 ): TGVec3;
begin
  Result.Data[0] := aVec2.Data[0];
  Result.Data[1] := aVec2.Data[1];
  Result.Data[2] := aZ;
end;

procedure TGVec3.Init( aX : T = 0; aY : T = 0; aZ : T = 0 );
begin
  Data[0] := aX;
  Data[1] := aY;
  Data[2] := aZ;
end;

class function TGVec3.CreateAll( A : T ) : TGVec3; static;
begin
  Result.Data[0] := A;
  Result.Data[1] := A;
  Result.Data[2] := A;
end;

function TGVec3.Scaled( A : T ) : TGVec3;
begin
  Result.Data[0] := Data[0] * A;
  Result.Data[1] := Data[1] * A;
  Result.Data[2] := Data[2] * A;
end;

function TGVec3.ScaledF( A : Single ) : TGVec3;
begin
  Result.Data[0] := T( Floor( Data[0] * A ) );
  Result.Data[1] := T( Floor( Data[1] * A ) );
  Result.Data[2] := T( Floor( Data[2] * A ) );
end;

function TGVec3.Shifted( A : T ) : TGVec3;
begin
  Result.Data[0] := Data[0] + A;
  Result.Data[1] := Data[1] + A;
  Result.Data[2] := Data[2] + A;
end;

function TGVec3.Length() : Single;
begin
  Exit( Sqrt( Data[0] * Data[0] + Data[1] * Data[1] + Data[2] * Data[2] ) );
end;

function TGVec3.LengthSq() : T;
begin
  Exit( Data[0] * Data[0] + Data[1] * Data[1] + Data[2] * Data[2] );
end;

function TGVec3.Distance( A : TGVec3 ) : Single;
begin
  Exit( Sqrt(Sqr(Abs(Single(A.Data[0]-Data[0]))) + Sqr(Abs(Single(A.Data[1]-Data[1])))) + Sqr(Abs(Single(A.Data[2]-Data[2]))));
end;

class operator TGVec3.+( const A, B : TGVec3 ) : TGVec3;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
end;

class operator TGVec3.-( const A, B : TGVec3 ) : TGVec3;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
end;

class operator TGVec3.*( const A, B : TGVec3 ) : TGVec3;
begin
  Result.Data[0] := A.Data[0] * B.Data[0];
  Result.Data[1] := A.Data[1] * B.Data[1];
  Result.Data[2] := A.Data[2] * B.Data[2];
end;

class operator TGVec3.**( const A, B : TGVec3 ) : T;
begin
  Result := A.Data[0] * B.Data[0]
          + A.Data[1] * B.Data[1]
          + A.Data[2] * B.Data[2];
end;

class operator TGVec3.-( const A : TGVec3 ) : TGVec3;
begin
  Result.Data[0] := -A.Data[0];
  Result.Data[1] := -A.Data[1];
  Result.Data[2] := -A.Data[2];
end;

class operator TGVec3.:=( const A : TGVec3 ) : TData;
begin
  Result[0] := A.Data[0];
  Result[1] := A.Data[1];
  Result[2] := A.Data[2];
end;

class operator TGVec3.:=( const A : TData ) : TGVec3;
begin
  Result.Data[0] := A[0];
  Result.Data[1] := A[1];
  Result.Data[2] := A[2];
end;

class operator TGVec3.Explicit( const A : array of T ) : TGVec3;
begin
  Result.Data[0] := A[0];
  Result.Data[1] := A[1];
  Result.Data[2] := A[2];
end;

function TGVec3.GetXY : TVec2;
begin
  Result.Data[0] := Data[0];
  Result.Data[1] := Data[1];
end;

procedure TGVec3.SetXY( const aValue : TVec2 );
begin
  Data[0] := aValue.Data[0];
  Data[1] := aValue.Data[1];
end;

// --------------------- TGVEC 4 --------------------- //

class function TGVec4.Create( aX : T = 0; aY : T = 0; aZ : T = 0; aW : T = 0 ) : TGVec4; static;
begin
  Result.Data[0] := aX;
  Result.Data[1] := aY;
  Result.Data[2] := aZ;
  Result.Data[3] := aW;
end;

class function TGVec4.CreateAll( A : T ) : TGVec4; static;
begin
  Result.Data[0] := A;
  Result.Data[1] := A;
  Result.Data[2] := A;
  Result.Data[3] := A;
end;

procedure TGVec4.Init( aX : T = 0; aY : T = 0; aZ : T = 0; aW : T = 0 );
begin
  Data[0] := aX;
  Data[1] := aY;
  Data[2] := aZ;
  Data[3] := aW;
end;

function TGVec4.ScaledF( A : Single ) : TGVec4;
begin
  Result.Data[0] := T( Floor( Data[0] * A ) );
  Result.Data[1] := T( Floor( Data[1] * A ) );
  Result.Data[2] := T( Floor( Data[2] * A ) );
  Result.Data[3] := T( Floor( Data[3] * A ) );
end;

function TGVec4.Scaled( A : T ) : TGVec4;
begin
  Result.Data[0] := Data[0] * A;
  Result.Data[1] := Data[1] * A;
  Result.Data[2] := Data[2] * A;
  Result.Data[3] := Data[3] * A;
end;

function TGVec4.Shifted( A : T ) : TGVec4;
begin
  Result.Data[0] := Data[0] + A;
  Result.Data[1] := Data[1] + A;
  Result.Data[2] := Data[2] + A;
  Result.Data[3] := Data[3] + A;
end;

class operator TGVec4.+( const A, B : TGVec4 ) : TGVec4;
begin
  Result.Data[0] := A.Data[0] + B.Data[0];
  Result.Data[1] := A.Data[1] + B.Data[1];
  Result.Data[2] := A.Data[2] + B.Data[2];
  Result.Data[3] := A.Data[3] + B.Data[3];
end;

class operator TGVec4.-( const A, B : TGVec4 ) : TGVec4;
begin
  Result.Data[0] := A.Data[0] - B.Data[0];
  Result.Data[1] := A.Data[1] - B.Data[1];
  Result.Data[2] := A.Data[2] - B.Data[2];
  Result.Data[3] := A.Data[3] - B.Data[3];
end;

class operator TGVec4.*( const A, B : TGVec4 ) : TGVec4;
begin
  Result.Data[0] := A.Data[0] * B.Data[0];
  Result.Data[1] := A.Data[1] * B.Data[1];
  Result.Data[2] := A.Data[2] * B.Data[2];
  Result.Data[3] := A.Data[3] * B.Data[3];
end;

class operator TGVec4.**( const A, B : TGVec4 ) : T;
begin
  Result := A.Data[0] * B.Data[0]
          + A.Data[1] * B.Data[1]
          + A.Data[2] * B.Data[2]
          + A.Data[3] * B.Data[3];
end;

class operator TGVec4.-( const A : TGVec4 ) : TGVec4;
begin
  Result.Data[0] := -A.Data[0];
  Result.Data[1] := -A.Data[1];
  Result.Data[2] := -A.Data[2];
  Result.Data[3] := -A.Data[3];
end;

class operator TGVec4.:=( const A : TGVec4 ) : TData;
begin
  Result[0] := A.Data[0];
  Result[1] := A.Data[1];
  Result[2] := A.Data[2];
  Result[3] := A.Data[3];
end;

class operator TGVec4.:=( const A : TData ) : TGVec4;
begin
  Result.Data[0] := A[0];
  Result.Data[1] := A[1];
  Result.Data[2] := A[2];
  Result.Data[3] := A[3];
end;

class operator TGVec4.Explicit( const A : array of T ) : TGVec4;
begin
  Result.Data[0] := A[0];
  Result.Data[1] := A[1];
  Result.Data[2] := A[2];
  Result.Data[3] := A[3];
end;

function TGVec4.GetXY : TVec2;
begin
  Result.Data[0] := Data[0];
  Result.Data[1] := Data[1];
end;

procedure TGVec4.SetXY( const aValue : TVec2 );
begin
  Data[0] := aValue.Data[0];
  Data[1] := aValue.Data[1];
end;

function TGVec4.GetXYZ : TVec3;
begin
  Result.Data[0] := Data[0];
  Result.Data[1] := Data[1];
  Result.Data[2] := Data[2];
end;

procedure TGVec4.SetXYZ( const aValue : TVec3 );
begin
  Data[0] := aValue.Data[0];
  Data[1] := aValue.Data[1];
  Data[2] := aValue.Data[2];
end;

// --------------------- TGVEC 4 --------------------- //

class function TGVectorQuad.Create( const q0, q1, q2, q3 : T ) : TGVectorQuad; static;
begin
  Result.Data[0] := q0;
  Result.Data[1] := q1;
  Result.Data[2] := q2;
  Result.Data[3] := q3;
end;

procedure TGVectorQuad.Init( const q0, q2 : T );
begin
  Data[0] := q0;
  Data[1].Init( q0.x, q2.y );
  Data[2] := q2;
  Data[3].Init( q2.x, q0.y );
end;

procedure TGVectorQuad.SetAll( const q : T );
begin
  Data[0] := q;
  Data[1] := q;
  Data[2] := q;
  Data[3] := q;
end;

procedure TGVectorQuad.FillAll( const q : TE );
begin
  Data[0] := T.CreateAll( q );
  Data[1] := T.CreateAll( q );
  Data[2] := T.CreateAll( q );
  Data[3] := T.CreateAll( q );
end;

// --------------------- TGVEC 4 --------------------- //

class function TGVectorTriQuad.Create( const q0, q1, q2, q3 : T ) : TGVectorTriQuad; static;
begin
  Result.Data[0] := q0;
  Result.Data[1] := q1;
  Result.Data[2] := q2;
  Result.Data[3] := q2;
  Result.Data[4] := q3;
  Result.Data[5] := q0;
end;

class function TGVectorTriQuad.CreateInit( const q0, q2 : T ) : TGVectorTriQuad; static;
begin
  Result.Data[0] := q0;
  Result.Data[1] := q0;
  Result.Data[1].y := q2.y;
  Result.Data[2] := q2;
  Result.Data[3] := q2;
  Result.Data[4] := q2;
  Result.Data[4].y := q0.y;
  Result.Data[5] := q0;
end;

class function TGVectorTriQuad.CreateAll( const q0 : T ) : TGVectorTriQuad; static;
begin
  Result.Data[0] := q0;
  Result.Data[1] := q0;
  Result.Data[2] := q0;
  Result.Data[3] := q0;
  Result.Data[4] := q0;
  Result.Data[5] := q0;
end;

procedure TGVectorTriQuad.Init( const q0, q2 : T );
begin
  Data[0] := q0;
  Data[1] := q0;
  Data[1].y := q2.y;
  Data[2] := q2;
  Data[3] := q2;
  Data[4] := q2;
  Data[4].y := q0.y;
  Data[5] := q0;
end;

procedure TGVectorTriQuad.SetAll( const q : T );
begin
  Data[0] := q;
  Data[1] := q;
  Data[2] := q;
  Data[3] := q;
  Data[4] := q;
  Data[5] := q;
end;

procedure TGVectorTriQuad.FillAll( const q : TE );
begin
  Data[0] := T.CreateAll( q );
  Data[1] := T.CreateAll( q );
  Data[2] := T.CreateAll( q );
  Data[3] := T.CreateAll( q );
  Data[4] := T.CreateAll( q );
  Data[5] := T.CreateAll( q );
end;

function Vec2f(aX: Single; aY: Single): TVec2f;
begin
  Vec2f.Data[0] := aX;
  Vec2f.Data[1] := aY;
end;

function Vec3f(aX: Single; aY: Single; aZ: Single): TVec3f;
begin
  Vec3f.Data[0] := aX;
  Vec3f.Data[1] := aY;
  Vec3f.Data[2] := aZ;
end;

function Vec4f(aX: Single; aY: Single; aZ: Single; aW: Single): TVec4f;
begin
  Vec4f.Data[0] := aX;
  Vec4f.Data[1] := aY;
  Vec4f.Data[2] := aZ;
  Vec4f.Data[3] := aW;
end;

function Vec2f( const aVec2i: TVec2i): TVec2f;
begin
  Vec2f.Data[0] := aVec2i.Data[0];
  Vec2f.Data[1] := aVec2i.Data[1];
end;

function Vec3f( const aVec3i: TVec3i): TVec3f;
begin
  Vec3f.Data[0] := aVec3i.Data[0];
  Vec3f.Data[1] := aVec3i.Data[1];
  Vec3f.Data[2] := aVec3i.Data[2];
end;

function Vec4f( const aVec4i: TVec4i): TVec4f;
begin
  Vec4f.Data[0] := aVec4i.Data[0];
  Vec4f.Data[1] := aVec4i.Data[1];
  Vec4f.Data[2] := aVec4i.Data[2];
  Vec4f.Data[3] := aVec4i.Data[3];
end;

function Vec3f( const aVec2f: TVec2f; aZ: Single): TVec3f;
begin
  Vec3f.Data[0] := aVec2f.Data[0];
  Vec3f.Data[1] := aVec2f.Data[1];
  Vec3f.Data[2] := aZ;
end;

function Vec4f( const aVec3f: TVec3f; aW: Single): TVec4f;
begin
  Vec4f.Data[0] := aVec3f.Data[0];
  Vec4f.Data[1] := aVec3f.Data[1];
  Vec4f.Data[2] := aVec3f.Data[2];
  Vec4f.Data[3] := aW;
end;

function Lerp( const a, b: TVec2f; aValue: Single): TVec2f;
begin
  Lerp.Data[0] := a.Data[0] + (b.Data[0] - a.Data[0]) * aValue;
  Lerp.Data[1] := a.Data[1] + (b.Data[1] - a.Data[1]) * aValue;
end;

function Lerp( const a, b: TVec3f; aValue: Single): TVec3f;
begin
  Lerp.Data[0] := a.Data[0] + (b.Data[0] - a.Data[0]) * aValue;
  Lerp.Data[1] := a.Data[1] + (b.Data[1] - a.Data[1]) * aValue;
  Lerp.Data[2] := a.Data[2] + (b.Data[2] - a.Data[2]) * aValue;
end;

// Clamp support

function Clamp( const x, min, max : TVec2f ) : TVec2f; overload;
begin
  Clamp.Data[0] := Clampf( x.Data[0], min.Data[0], max.Data[0] );
  Clamp.Data[1] := Clampf( x.Data[1], min.Data[1], max.Data[1] );
end;

function Clamp( const x, min, max : TVec3f ) : TVec3f; overload;
begin
  Clamp.Data[0] := Clampf( x.Data[0], min.Data[0], max.Data[0] );
  Clamp.Data[1] := Clampf( x.Data[1], min.Data[1], max.Data[1] );
  Clamp.Data[2] := Clampf( x.Data[2], min.Data[2], max.Data[2] );
end;

function Clamp( const x, min, max : TVec4f ) : TVec4f; overload;
begin
  Clamp.Data[0] := Clampf( x.Data[0], min.Data[0], max.Data[0] );
  Clamp.Data[1] := Clampf( x.Data[1], min.Data[1], max.Data[1] );
  Clamp.Data[2] := Clampf( x.Data[2], min.Data[2], max.Data[2] );
  Clamp.Data[3] := Clampf( x.Data[3], min.Data[3], max.Data[3] );
end;

// Ceil and floor

function Ceilf( const aVec2f : TVec2f ) : TVec2f; overload;
begin
  Ceilf.Data[0] := Ceil( aVec2f.Data[0] );
  Ceilf.Data[1] := Ceil( aVec2f.Data[1] );
end;

function Ceilf( const aVec3f : TVec3f ) : TVec3f; overload;
begin
  Ceilf.Data[0] := Ceil( aVec3f.Data[0] );
  Ceilf.Data[1] := Ceil( aVec3f.Data[1] );
  Ceilf.Data[2] := Ceil( aVec3f.Data[2] );
end;

function Ceilf( const aVec4f : TVec4f ) : TVec4f; overload;
begin
  Ceilf.Data[0] := Ceil( aVec4f.Data[0] );
  Ceilf.Data[1] := Ceil( aVec4f.Data[1] );
  Ceilf.Data[2] := Ceil( aVec4f.Data[2] );
  Ceilf.Data[3] := Ceil( aVec4f.Data[3] );
end;

function Floorf( const aVec2f : TVec2f ) : TVec2f; overload;
begin
  Floorf.Data[0] := Floor( aVec2f.Data[0] );
  Floorf.Data[1] := Floor( aVec2f.Data[1] );
end;

function Floorf( const aVec3f : TVec3f ) : TVec3f; overload;
begin
  Floorf.Data[0] := Floor( aVec3f.Data[0] );
  Floorf.Data[1] := Floor( aVec3f.Data[1] );
  Floorf.Data[2] := Floor( aVec3f.Data[2] );
end;

function Floorf( const aVec4f : TVec4f ) : TVec4f; overload;
begin
  Floorf.Data[0] := Floor( aVec4f.Data[0] );
  Floorf.Data[1] := Floor( aVec4f.Data[1] );
  Floorf.Data[2] := Floor( aVec4f.Data[2] );
  Floorf.Data[3] := Floor( aVec4f.Data[3] );
end;

function Roundf( const aVec2f : TVec2f ) : TVec2f; overload;
begin
  Roundf.Data[0] := Round( aVec2f.Data[0] );
  Roundf.Data[1] := Round( aVec2f.Data[1] );
end;

function Roundf( const aVec3f : TVec3f ) : TVec3f; overload;
begin
  Roundf.Data[0] := Round( aVec3f.Data[0] );
  Roundf.Data[1] := Round( aVec3f.Data[1] );
  Roundf.Data[2] := Round( aVec3f.Data[2] );
end;

function Roundf( const aVec4f : TVec4f ) : TVec4f; overload;
begin
  Roundf.Data[0] := Round( aVec4f.Data[0] );
  Roundf.Data[1] := Round( aVec4f.Data[1] );
  Roundf.Data[2] := Round( aVec4f.Data[2] );
  Roundf.Data[3] := Round( aVec4f.Data[3] );
end;

// Integer

function Vec2i(aX: Integer; aY: Integer): TVec2i;
begin
  Vec2i.Data[0] := aX;
  Vec2i.Data[1] := aY;
end;

function Vec3i(aX: Integer; aY: Integer; aZ: Integer): TVec3i;
begin
  Vec3i.Data[0] := aX;
  Vec3i.Data[1] := aY;
  Vec3i.Data[2] := aZ;
end;

function Vec4i(aX: Integer; aY: Integer; aZ: Integer; aW: Integer): TVec4i;
begin
  Vec4i.Data[0] := aX;
  Vec4i.Data[1] := aY;
  Vec4i.Data[2] := aZ;
  Vec4i.Data[3] := aW;
end;

function Vec3i( const aVec2i: TVec2i; aZ: Integer ): TVec3i;
begin
  Vec3i.Data[0] := aVec2i.Data[0];
  Vec3i.Data[1] := aVec2i.Data[1];
  Vec3i.Data[2] := aZ;
end;

function Vec4i( const aVec3i: TVec3i; aW: Integer ): TVec4i;
begin
  Vec4i.Data[0] := aVec3i.Data[0];
  Vec4i.Data[1] := aVec3i.Data[1];
  Vec4i.Data[2] := aVec3i.Data[2];
  Vec4i.Data[3] := aW;
end;

function Lerp( const a, b: TVec2i; aValue: Single ): TVec2i;
begin
  Lerp.Data[0] := a.Data[0] + Round( (b.Data[0] - a.Data[0]) * aValue );
  Lerp.Data[1] := a.Data[1] + Round( (b.Data[1] - a.Data[1]) * aValue );
end;

function Lerp( const a, b: TVec3i; aValue: Single ): TVec3i;
begin
  Lerp.Data[0] := a.Data[0] + Round( (b.Data[0] - a.Data[0]) * aValue );
  Lerp.Data[1] := a.Data[1] + Round( (b.Data[1] - a.Data[1]) * aValue );
  Lerp.Data[2] := a.Data[2] + Round( (b.Data[2] - a.Data[2]) * aValue );
end;

function Ceil( const aVec2f : TVec2f ) : TVec2i; overload;
begin
  Ceil.Data[0] := Ceil( aVec2f.Data[0] );
  Ceil.Data[1] := Ceil( aVec2f.Data[1] );
end;

function Ceil( const aVec3f : TVec3f ) : TVec3i; overload;
begin
  Ceil.Data[0] := Ceil( aVec3f.Data[0] );
  Ceil.Data[1] := Ceil( aVec3f.Data[1] );
  Ceil.Data[2] := Ceil( aVec3f.Data[2] );
end;

function Ceil( const aVec4f : TVec4f ) : TVec4i; overload;
begin
  Ceil.Data[0] := Ceil( aVec4f.Data[0] );
  Ceil.Data[1] := Ceil( aVec4f.Data[1] );
  Ceil.Data[2] := Ceil( aVec4f.Data[2] );
  Ceil.Data[3] := Ceil( aVec4f.Data[3] );
end;

function Floor( const aVec2f : TVec2f ) : TVec2i; overload;
begin
  Floor.Data[0] := Floor( aVec2f.Data[0] );
  Floor.Data[1] := Floor( aVec2f.Data[1] );
end;

function Floor( const aVec3f : TVec3f ) : TVec3i; overload;
begin
  Floor.Data[0] := Floor( aVec3f.Data[0] );
  Floor.Data[1] := Floor( aVec3f.Data[1] );
  Floor.Data[2] := Floor( aVec3f.Data[2] );
end;

function Floor( const aVec4f : TVec4f ) : TVec4i; overload;
begin
  Floor.Data[0] := Floor( aVec4f.Data[0] );
  Floor.Data[1] := Floor( aVec4f.Data[1] );
  Floor.Data[2] := Floor( aVec4f.Data[2] );
  Floor.Data[3] := Floor( aVec4f.Data[3] );
end;

function Round( const aVec2f : TVec2f ) : TVec2i; overload;
begin
  Round.Data[0] := Round( aVec2f.Data[0] );
  Round.Data[1] := Round( aVec2f.Data[1] );
end;

function Round( const aVec3f : TVec3f ) : TVec3i; overload;
begin
  Round.Data[0] := Round( aVec3f.Data[0] );
  Round.Data[1] := Round( aVec3f.Data[1] );
  Round.Data[2] := Round( aVec3f.Data[2] );
end;

function Round( const aVec4f : TVec4f ) : TVec4i; overload;
begin
  Round.Data[0] := Round( aVec4f.Data[0] );
  Round.Data[1] := Round( aVec4f.Data[1] );
  Round.Data[2] := Round( aVec4f.Data[2] );
  Round.Data[3] := Round( aVec4f.Data[3] );
end;

function Clamp( const x, min, max : TVec2i ) : TVec2i; overload;
begin
  Clamp.Data[0] := Clamp( x.Data[0], min.Data[0], max.Data[0] );
  Clamp.Data[1] := Clamp( x.Data[1], min.Data[1], max.Data[1] );
end;

function Clamp( const x, min, max : TVec3i ) : TVec3i; overload;
begin
  Clamp.Data[0] := Clamp( x.Data[0], min.Data[0], max.Data[0] );
  Clamp.Data[1] := Clamp( x.Data[1], min.Data[1], max.Data[1] );
  Clamp.Data[2] := Clamp( x.Data[2], min.Data[2], max.Data[2] );
end;

function Clamp( const x, min, max : TVec4i ) : TVec4i; overload;
begin
  Clamp.Data[0] := Clamp( x.Data[0], min.Data[0], max.Data[0] );
  Clamp.Data[1] := Clamp( x.Data[1], min.Data[1], max.Data[1] );
  Clamp.Data[2] := Clamp( x.Data[2], min.Data[2], max.Data[2] );
  Clamp.Data[3] := Clamp( x.Data[3], min.Data[3], max.Data[3] );
end;

end.

