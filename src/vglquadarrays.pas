{$INCLUDE valkyrie.inc}
unit vglquadarrays;
interface
uses vgldrawarrays, vgltypes, vvector, vgenerics;

type TGLQVec3i = specialize TGVectorTriQuad<TGLVec3i,Integer>;
     TGLQVec2f = specialize TGVectorTriQuad<TGLVec2f,Single>;
     TGLQVec4f = specialize TGVectorTriQuad<TGLVec4f,Single>;

     TGLQVec3iArray = specialize TGArray< TGLQVec3i >;
     TGLQVec2fArray = specialize TGArray< TGLQVec2f >;
     TGLQVec4fArray = specialize TGArray< TGLQVec4f >;

type TGLColoredQuads = class( TGLDrawArrays )
  constructor Create;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColor : TGLVec4f ) ;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColorQuad : TGLQVec4f ) ;
end;

type TGLTexturedQuads = class( TGLDrawArrays )
  constructor Create;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aTUR, aTLL : TGLVec2f ) ;
  procedure PushRotatedQuad ( aCenter, aSize : TGLVec3i; aDegrees : Single; aTUR, aTLL : TGLVec2f ) ;
end;

type TGLTexturedColoredQuads = class( TGLTexturedQuads )
  constructor Create;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColorQuad : TGLQVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure PushRotatedQuad ( aCenter, aSize : TGLVec3i; aDegrees : Single; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
end;


implementation

uses vgl3library;

{ TGLColoredQuads }

constructor TGLColoredQuads.Create;
begin
  inherited Create;
  PushArray( TGLQVec3iArray.Create, 3, GL_INT,   VGL_POSITION_LOCATION );
  PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, VGL_COLOR_LOCATION );
end;

procedure TGLColoredQuads.PushQuad ( aUR, aLL : TGLVec3i; aColor : TGLVec4f ) ;
begin
  TGLQVec3iArray(FArrays[0]).Push( TGLQVec3i.CreateInit( aUR, aLL ) );
  TGLQVec4fArray(FArrays[1]).Push( TGLQVec4f.CreateAll( aColor ) );
end;

procedure TGLColoredQuads.PushQuad ( aUR, aLL : TGLVec3i; aColorQuad: TGLQVec4f ) ;
begin
  TGLQVec3iArray(FArrays[0]).Push( TGLQVec3i.CreateInit( aUR, aLL ) );
  TGLQVec4fArray(FArrays[1]).Push( aColorQuad );
end;

{ TGLTexturedQuads }

constructor TGLTexturedQuads.Create;
begin
  inherited Create;
  PushArray( TGLQVec3iArray.Create, 3, GL_INT,   VGL_POSITION_LOCATION );
  PushArray( TGLQVec2fArray.Create, 2, GL_FLOAT, VGL_TEXCOORD_LOCATION );
end;

procedure TGLTexturedQuads.PushQuad ( aUR, aLL : TGLVec3i; aTUR, aTLL : TGLVec2f ) ;
begin
  TGLQVec3iArray(FArrays[0]).Push( TGLQVec3i.CreateInit( aUR, aLL ) );
  TGLQVec2fArray(FArrays[1]).Push( TGLQVec2f.CreateInit( aTUR, aTLL ) );
end;

procedure TGLTexturedQuads.PushRotatedQuad( aCenter, aSize: TGLVec3i;
  aDegrees: Single; aTUR, aTLL: TGLVec2f);
const Pi180 = Pi / 180;
var iRotated   : TGLVec3i;
    iFlipped   : TGLVec3i;
    iCos, iSin : Single;
begin
  aSize.x := aSize.x div 2;
  aSize.y := aSize.y div 2;
  iCos  := Cos( Pi180 * aDegrees );
  iSin  := Sin( Pi180 * aDegrees );
  iRotated.x := Round( aSize.x * iCos - aSize.y * iSin );
  iRotated.y := Round( aSize.x * iSin + aSize.y * iCos );
  iRotated.z := aCenter.z;
  iFlipped.x := Round( aSize.x * iCos + aSize.y * iSin );
  iFlipped.y := Round( aSize.x * iSin - aSize.y * iCos );
  iFlipped.z := aCenter.z;
  TGLQVec3iArray(FArrays[0]).Push( TGLQVec3i.Create( aCenter - iRotated, aCenter - iFlipped, aCenter + iRotated, aCenter + iFlipped ) );
  TGLQVec2fArray(FArrays[1]).Push( TGLQVec2f.CreateInit( aTUR, aTLL ) );
end;

{ TGLTexturedColoredQuads }

constructor TGLTexturedColoredQuads.Create;
begin
  inherited Create;
  PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, VGL_COLOR_LOCATION );
end;

procedure TGLTexturedColoredQuads.PushQuad ( aUR, aLL : TGLVec3i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
begin
  inherited PushQuad(aUR, aLL, aTUR, aTLL );
  TGLQVec4fArray(FArrays[2]).Push( TGLQVec4f.CreateAll( aColor ) );
end;

procedure TGLTexturedColoredQuads.PushQuad(aUR, aLL: TGLVec3i; aColorQuad: TGLQVec4f; aTUR, aTLL: TGLVec2f);
begin
  inherited PushQuad(aUR, aLL, aTUR, aTLL );
  TGLQVec4fArray(FArrays[2]).Push( aColorQuad );
end;

procedure TGLTexturedColoredQuads.PushRotatedQuad(aCenter, aSize: TGLVec3i;
  aDegrees: Single; aColor: TGLVec4f; aTUR, aTLL: TGLVec2f);
begin
  inherited PushRotatedQuad( aCenter, aSize, aDegrees, aTUR, aTLL );
  TGLQVec4fArray(FArrays[2]).Push( TGLQVec4f.CreateAll( aColor ) );
end;


end.

