unit vspriteengine;
{$include valkyrie.inc}
interface
uses
  Classes, SysUtils, vcolor, vgltypes;

type TSpriteEngine = class;


type

{ TSpriteDataVTC }

TSpriteDataVTC = class
  constructor Create( aEngine : TSpriteEngine );
  procedure Push( Source : TSpriteDataVTC; Idx, Amount : DWord );
  procedure Push( PosID : DWord; Pos : TGLVec2i; Color : TColor );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; color : PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0 );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; Color : TColor );
  procedure Push( coord : PGLRawQCoord; tex : PGLRawQTexCoord; color : PGLRawQColor );
//  procedure PushXY( PosID, Color, PosX, PosY : DWord );
  procedure Resize( newSize : DWord );
  procedure Reserve( newCapacity : DWord );
  procedure Clear;
private
  FCoords    : packed array of TGLRawQCoord;
  FTexCoords : packed array of TGLRawQTexCoord;
  FColors    : packed array of TGLRawQColor;
  FSize      : DWord;
  FCapacity  : DWord;
  FEngine    : TSpriteEngine;
  procedure GrowTo( NewSize : DWord );
public
  property Size : DWord     read FSize;
  property Capacity : DWord read FCapacity;
end;

type

{ TSpriteDataSet }

TSpriteDataSet = class
  Normal  : TSpriteDataVTC;
  Cosplay : TSpriteDataVTC;
  Glow    : TSpriteDataVTC;

  constructor Create( aEngine : TSpriteEngine; aCosplay, aGlow : Boolean );
  procedure Resize( Size : DWord );
  procedure Clear;
  destructor Destroy; override;
end;

type TTextureDataSet = record
  Normal  : DWord;
  Cosplay : DWord;
  Glow    : DWord;
end;

type TTextureSet = record
  Layer      : array[1..5] of TTextureDataSet;
end;

const VSE_BG_LAYER = 1;
      VSE_FG_LAYER = 2;

type

{ TSpriteEngine }

TSpriteEngine = class
//  FTextures          : array of TTextureSet;
//  FTextureSets       : Byte;
//  FCurrentTextureSet : Byte;
  FTextureSet        : TTextureSet;

  FGrid              : TGLVec2i;
  FTexUnit           : TGLVec2f;
  FPos               : TGLVec2i;
  FLayers            : array[1..5] of TSpriteDataSet;
  FLayerCount        : Byte;
  FStaticLayerCount  : Byte;
  FCurrentTexture    : DWord;

  FSpriteRowCount    : Word;

  constructor Create;
  procedure Clear;
  procedure Draw;
  procedure DrawVTC( Data : TSpriteDataVTC );
  procedure DrawSet( const Data : TSpriteDataSet; const Tex : TTextureDataSet );
  // Foreground layer
  // Animation layer
  procedure SetTexture( TexID : DWord );
  destructor Destroy; override;
end;


implementation

uses
  vgllibrary, math;

{ TSpriteDataSet }

constructor TSpriteDataSet.Create( aEngine : TSpriteEngine; aCosplay, aGlow : Boolean );
begin
  Normal  := nil;
  Cosplay := nil;
  Glow    := nil;

  Normal  := TSpriteDataVTC.Create( aEngine );
  if aCosplay then Cosplay := TSpriteDataVTC.Create( aEngine );
  if aGlow    then Glow    := TSpriteDataVTC.Create( aEngine );
end;

procedure TSpriteDataSet.Resize( Size: DWord );
begin
  Normal.Resize( Size );
  if Cosplay <> nil then Cosplay.Resize( Size );
  if Glow    <> nil then Glow.Resize( Size );
end;

procedure TSpriteDataSet.Clear;
begin
  Normal.Clear;
  if Cosplay <> nil then Cosplay.Clear;
  if Glow    <> nil then Glow.Clear;
end;

destructor TSpriteDataSet.Destroy;
begin
  FreeAndNil( Normal );
  FreeAndNil( Cosplay );
  FreeAndNil( Glow );
end;

{ TSpriteDataVTC }

constructor TSpriteDataVTC.Create( aEngine : TSpriteEngine );
begin
  FSize     := 0;
  FCapacity := 0;
  FEngine   := aEngine;
end;

procedure TSpriteDataVTC.Push( Source: TSpriteDataVTC; Idx, Amount : DWord);
begin
  if Amount + FSize > FCapacity then GrowTo( Amount + FSize );
  Move( Source.FCoords[ Idx ], FCoords[ FSize ], Amount * SizeOf(TGLRawQCoord) );
  Move( Source.FTexCoords[ Idx ], FTexCoords[ FSize ], Amount * SizeOf(TGLRawQTexCoord) );
  Move( Source.FColors[ Idx ], FColors[ FSize ], Amount * SizeOf(TGLRawQColor) );
  FSize += Amount;
end;

procedure TSpriteDataVTC.Push(PosID : DWord; Pos : TGLVec2i; Color : TColor);
var p1, p2     : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );

  p1 := Pos.Shifted(-1) * FEngine.FGrid;
  p2 := Pos * FEngine.FGrid;

  FCoords[ FSize ].Init( p1, p2 );

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(1) * FEngine.FTexUnit;

  FTexCoords[ FSize ].Init( t1, t2 );
  FColors[ FSize ].SetAll( TGLVec3b.Create( Color.R, Color.G, Color.B ) );

  Inc( FSize );
end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; color: PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0 );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );

  p2 := pos + FEngine.FGrid.Scaled( Size );

  FCoords[ FSize ].Init( pos, p2 );

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );
  tp += TGLVec2f.Create( TShiftX, TShiftY );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(Size) * FEngine.FTexUnit;

  FTexCoords[ FSize ].Init( t1, t2 );

  FColors[ FSize ] := color^;
  Inc( FSize );
end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; Color : TColor );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );

  p2 := pos + FEngine.FGrid.Scaled( Size );

  FCoords[ FSize ].Init( pos, p2 );

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(Size) * FEngine.FTexUnit;

  FTexCoords[ FSize ].Init( t1, t2 );
  FColors[ FSize ].SetAll( TGLVec3b.Create( Color.R, Color.G, Color.B ) );
  Inc( FSize );
end;

procedure TSpriteDataVTC.Push(coord: PGLRawQCoord; tex: PGLRawQTexCoord; color: PGLRawQColor);
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );
  FCoords[ FSize ] := coord^;
  FTexCoords[ FSize ] := tex^;
  FColors[ FSize ] := color^;
  Inc( FSize );
end;

procedure TSpriteDataVTC.Resize( newSize: DWord );
begin
  Reserve( newSize );
  FSize := newSize;
end;

procedure TSpriteDataVTC.Reserve( newCapacity: DWord );
begin
  SetLength( FCoords, newCapacity );
  SetLength( FTexCoords, newCapacity );
  SetLength( FColors, newCapacity );
  FCapacity := newCapacity;
end;

procedure TSpriteDataVTC.Clear;
begin
  FSize := 0;
end;

procedure TSpriteDataVTC.GrowTo( NewSize: DWord );
begin
  Reserve( newSize );
end;

{ TSpriteEngine }

procedure TSpriteEngine.DrawVTC( Data : TSpriteDataVTC );
begin
  glEnableClientState( GL_VERTEX_ARRAY );
  glEnableClientState( GL_TEXTURE_COORD_ARRAY );
  glEnableClientState( GL_COLOR_ARRAY );

  glVertexPointer( 2, GL_INT, 0, @(Data.FCoords[0]) );
  glTexCoordPointer( 2, GL_FLOAT, 0, @(Data.FTexCoords[0]) );
  glColorPointer( 3, GL_UNSIGNED_BYTE, 0, @(Data.FColors[0]) );
  glDrawArrays( GL_QUADS, 0, Data.FSize*4 );

  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  glDisableClientState( GL_COLOR_ARRAY );
end;

procedure TSpriteEngine.DrawSet(const Data: TSpriteDataSet; const Tex : TTextureDataSet);
begin
  if Data.Normal.Size > 0 then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    SetTexture( Tex.Normal );
    DrawVTC( Data.Normal );
  end;

  if (Data.Cosplay <> nil) and (Data.Cosplay.Size > 0) then
  begin
    glBlendFunc( GL_ONE, GL_ONE );
    SetTexture( Tex.Cosplay );
    DrawVTC( Data.Cosplay );
  end;

  if (Data.Glow <> nil) and (Data.Glow.Size > 0) then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    SetTexture( Tex.Glow );
    DrawVTC( Data.Glow );
  end;
end;

procedure TSpriteEngine.SetTexture(TexID: DWord);
begin
  if FCurrentTexture <> TexID then
  begin
    glBindTexture( GL_TEXTURE_2D, TexID );
    FCurrentTexture := TexID;
  end;
end;

destructor TSpriteEngine.Destroy;
var i : Byte;
begin
  for i := 1 to High(FLayers) do
    FreeAndNil( FLayers[i] );
end;

constructor TSpriteEngine.Create;
var i : Byte;
begin
  for i := 1 to High(FLayers) do
    FLayers[i] := nil;
  FSpriteRowCount    := 16;
  FGrid.Init( 32, 32 );
  FTexUnit.Init( 1.0 / FSpriteRowCount, 1.0 / 32 );
  FPos.Init(0,0);
  FCurrentTexture    := 0;
  FLayerCount        := 0;
  FStaticLayerCount  := 0;
end;

procedure TSpriteEngine.Clear;
var i : Byte;
begin
  if FLayerCount > 0 then
  for i := 1 to FLayerCount do
    FLayers[ i ].Clear;
end;

procedure TSpriteEngine.Draw;
var i : Byte;
begin
  FCurrentTexture := 0;
  glTranslatef( -FPos.X, -FPos.Y, 0.0 );
  glEnable( GL_TEXTURE_2D );
  glDisable( GL_DEPTH_TEST );
  glEnable( GL_BLEND );
  glColor4f( 1.0, 1.0, 1.0, 1.0 );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  if FLayerCount > 0 then
  for i := 1 to FLayerCount do
    DrawSet( FLayers[ i ], FTextureSet.Layer[ i ] );

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glTranslatef( FPos.X, FPos.Y, 0.0 );
end;

initialization

  Assert( SizeOf( Integer ) = SizeOf( GLInt ) );
  Assert( SizeOf( Single )  = SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQCoord )    = 8 * SizeOf( GLInt ) );
  Assert( SizeOf( TGLRawQTexCoord ) = 8 * SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQColor )    = 12 * SizeOf( GLByte ) );

end.

