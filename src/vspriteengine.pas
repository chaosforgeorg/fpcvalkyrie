unit vspriteengine;
{$include valkyrie.inc}
interface
uses
  Classes, SysUtils, vcolor, vgltypes, vglprogram, vglquadarrays;

type TSpriteEngine = class;


type

{ TSpriteDataVTC }

TSpriteDataVTC = class
  constructor Create( aEngine : TSpriteEngine; aTextureID : DWord; aTilesX, aTilesY : Word );
  procedure Push( PosID : DWord; Pos : TGLVec2i; Color : TColor; aZ : Integer = 0 );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; color : PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0; aZ : Integer = 0 );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; Color : TColor; aZ : Integer = 0 );
  procedure Push( coord : PGLRawQCoord; tex : PGLRawQTexCoord; color : PGLRawQColor; aZ : Integer = 0 );
  procedure PushPart( PosID : DWord; p1,p2 : TGLVec2i; color : PGLRawQColor; aZ : Integer; t1,t2 : TGLVec2f );
  destructor Destroy; override;
private
  FData      : TGLTexturedColoredQuads;
  FEngine    : TSpriteEngine;
  FTexUnit   : TGLVec2f;
  FRowSize   : Word;
  FTextureID : DWord;
public
  property TexUnit   : TGLVec2f read FTexUnit;
  property RowSize   : Word     read FRowSize;
  property TextureID : DWord    read FTextureID;
end;

type

{ TSpriteDataSet }

TSpriteDataSet = class
  Normal  : TSpriteDataVTC;
  Cosplay : TSpriteDataVTC;
  Glow    : TSpriteDataVTC;

  constructor Create( aEngine : TSpriteEngine; aNormal, aCosplay, aGlow : DWord; aTilesX, aTilesY : Word );
  destructor Destroy; override;
end;

type TTextureDataSet = record
  Normal  : DWord;
  Cosplay : DWord;
  Glow    : DWord;
end;

type

{ TSpriteEngine }

TSpriteEngine = class
  FPos        : TGLVec2i;
  FLayers     : array[1..11] of TSpriteDataSet;
  FLayerCount : Byte;

  constructor Create( aTileSize : TGLVec2i; aScale : Byte = 1 );
  procedure SetScale( aScale : Byte );
  procedure Draw;
  procedure Update( aProjection : TMatrix44 );
  procedure DrawVTC( Data : TSpriteDataVTC );
  procedure DrawSet( const Data : TSpriteDataSet );
  // Foreground layer
  // Animation layer
  procedure SetTexture( TexID : DWord );
  destructor Destroy; override;
private
  FVAO            : Cardinal;
  FProgram        : TGLProgram;
  FProjection     : TMatrix44;
  FCurrentTexture : DWord;
  FGrid           : TGLVec2i;
  FTileSize       : TGLVec2i;
public
  property Grid     : TGLVec2i read FGrid;
  property TileSize : TGLVec2i read FTileSize;
end;


implementation

uses
  math, vgl3library;

const
VSpriteVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec3 position;'+#10+
'layout (location = 1) in vec2 texcoord;'+#10+
'layout (location = 2) in vec4 color;'+#10+
'uniform mat4 utransform;'+#10+
'uniform vec3 uposition;'+#10+
#10+
'out vec4 ocolor;'+#10+
'out vec2 otexcoord;'+#10+
#10+
'void main() {'+#10+
'ocolor    = color;'+#10+
'otexcoord = texcoord;'+#10+
'gl_Position = utransform * vec4(uposition + position, 1.0);'+#10+
'}'+#10;
VSpriteFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec4 ocolor;'+#10+
'in vec2 otexcoord;'+#10+
'uniform sampler2D utexture;'+#10+
'out vec4 frag_color;'+#10+
#10+
'void main() {'+#10+
'frag_color = texture(utexture, otexcoord) * ocolor;'+#10+
'if ( frag_color.a < 0.01 ) discard;'+#10+
'}'+#10;

{ TSpriteDataSet }

constructor TSpriteDataSet.Create( aEngine : TSpriteEngine; aNormal, aCosplay, aGlow : DWord; aTilesX, aTilesY : Word );
begin
  Normal  := nil;
  Cosplay := nil;
  Glow    := nil;

  Normal  := TSpriteDataVTC.Create( aEngine, aNormal, aTilesX, aTilesY );
  if aCosplay > 0 then Cosplay := TSpriteDataVTC.Create( aEngine, aCosplay, aTilesX, aTilesY );
  if aGlow    > 0 then Glow    := TSpriteDataVTC.Create( aEngine, aGlow, aTilesX, aTilesY );
end;

destructor TSpriteDataSet.Destroy;
begin
  FreeAndNil( Normal );
  FreeAndNil( Cosplay );
  FreeAndNil( Glow );
end;

{ TSpriteDataVTC }

constructor TSpriteDataVTC.Create( aEngine : TSpriteEngine; aTextureID : DWord; aTilesX, aTilesY : Word );
begin
  FEngine    := aEngine;
  FData      := TGLTexturedColoredQuads.Create;
  FRowSize   := aTilesX;
  FTextureID := aTextureID;
  FTexUnit.Init( 1.0 / aTilesX, 1.0 / aTilesY );
end;


procedure TSpriteDataVTC.Push(PosID : DWord; Pos : TGLVec2i; Color : TColor; aZ : Integer = 0);
var p1, p2     : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  p1 := Pos.Shifted(-1) * FEngine.FGrid;
  p2 := Pos * FEngine.FGrid;

  tp := TGLVec2f.CreateModDiv( PosID-1, FRowSize );
  t1 := tp * FTexUnit;
  t2 := tp.Shifted(1) * FTexUnit;

  FData.PushQuad(
    TGLVec3i.CreateFrom( p1, aZ ),
    TGLVec3i.CreateFrom( p2, aZ ),
    Color.toVec43f,
    t1, t2
  );

end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; color: PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0; aZ : Integer = 0 );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  p2 := pos + FEngine.FGrid.Scaled( Size );

  tp := TGLVec2f.CreateModDiv( PosID-1, FRowSize );
  tp += TGLVec2f.Create( TShiftX, TShiftY );

  t1 := tp * FTexUnit;
  t2 := tp.Shifted(Size) * FTexUnit;

  FData.PushQuad(
    TGLVec3i.CreateFrom( pos, aZ ),
    TGLVec3i.CreateFrom( p2, aZ ),
    TGLQVec4f.Create(
      NewColor( color^.Data[0] ).toVec43f,
      NewColor( color^.Data[1] ).toVec43f,
      NewColor( color^.Data[2] ).toVec43f,
      NewColor( color^.Data[3] ).toVec43f
    ),
    t1, t2
  );
end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; Color : TColor; aZ : Integer = 0 );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  p2 := pos + FEngine.FGrid.Scaled( Size );
  tp := TGLVec2f.CreateModDiv( PosID-1, FRowSize );

  t1 := tp * FTexUnit;
  t2 := tp.Shifted(Size) * FTexUnit;

  FData.PushQuad(
    TGLVec3i.CreateFrom( pos, aZ ),
    TGLVec3i.CreateFrom( p2, aZ ),
    Color.toVec43f,
    t1, t2
  );
end;

procedure TSpriteDataVTC.Push(coord: PGLRawQCoord; tex: PGLRawQTexCoord; color: PGLRawQColor; aZ : Integer = 0);
begin
  FData.PushQuad(
    TGLQVec3i.Create(
      TGLVec3i.CreateFrom( coord^.Data[0], aZ ),
      TGLVec3i.CreateFrom( coord^.Data[1], aZ ),
      TGLVec3i.CreateFrom( coord^.Data[2], aZ ),
      TGLVec3i.CreateFrom( coord^.Data[3], aZ )
    ),
    TGLQVec4f.Create(
      NewColor( color^.Data[0] ).toVec43f,
      NewColor( color^.Data[1] ).toVec43f,
      NewColor( color^.Data[2] ).toVec43f,
      NewColor( color^.Data[3] ).toVec43f
    ),
    tex^.Data[0], tex^.Data[2]
  );
end;

procedure TSpriteDataVTC.PushPart( PosID : DWord; p1,p2 : TGLVec2i; color : PGLRawQColor; aZ : Integer; t1,t2 : TGLVec2f );
var tp : TGLVec2f;
begin
  tp := TGLVec2f.CreateModDiv( PosID-1, FRowSize );
  t1 := ( tp + t1 ) * FTexUnit;
  t2 := ( tp + t2 ) * FTexUnit;

  FData.PushQuad(
    TGLVec3i.CreateFrom( p1, aZ ),
    TGLVec3i.CreateFrom( p2, aZ ),
    TGLQVec4f.Create(
      NewColor( color^.Data[0] ).toVec43f,
      NewColor( color^.Data[1] ).toVec43f,
      NewColor( color^.Data[2] ).toVec43f,
      NewColor( color^.Data[3] ).toVec43f
    ),
    t1, t2
  );
end;

destructor TSpriteDataVTC.Destroy;
begin
  FreeAndNil( FData );
end;

{ TSpriteEngine }

procedure TSpriteEngine.Update ( aProjection : TMatrix44 );
var i : Integer;
begin
  for i := 0 to 15 do
    if FProjection[i] <> aProjection[i] then
    begin
      FProjection := aProjection;
      FProgram.Bind;
      glUniformMatrix4fv( FProgram.GetUniformLocation( 'utransform' ), 1, GL_FALSE, @FProjection[0] );
      glUniform1i( FProgram.GetUniformLocation('utexture'), 0 );
      FProgram.UnBind;
      Exit;
    end;
end;

procedure TSpriteEngine.DrawVTC( Data : TSpriteDataVTC );
begin
  SetTexture( Data.TextureID );
  FProgram.Bind;
  Data.FData.Update;
  Data.FData.Draw;
  Data.FData.Clear;
  FProgram.UnBind;
end;

procedure TSpriteEngine.DrawSet(const Data: TSpriteDataSet );
begin
  glActiveTexture(0);

  if not Data.Normal.FData.Empty then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    DrawVTC( Data.Normal );
  end;

  if (Data.Cosplay <> nil) and (not Data.Cosplay.FData.Empty) then
  begin
    glBlendFunc( GL_ONE, GL_ONE );
    DrawVTC( Data.Cosplay );
  end;

  if (Data.Glow <> nil) and (not Data.Glow.FData.Empty) then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    DrawVTC( Data.Glow );
  end;

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
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
  glDeleteVertexArrays(1, @FVAO);
  FreeAndNil( FProgram );
end;

constructor TSpriteEngine.Create( aTileSize : TGLVec2i; aScale : Byte = 1 );
var i : Byte;
begin
  for i := 1 to High(FLayers) do
    FLayers[i] := nil;
  FTileSize := aTileSize;
  SetScale( aScale );
  FPos.Init(0,0);
  FCurrentTexture    := 0;
  FLayerCount        := 0;

  FProgram := TGLProgram.Create( VSpriteVertexShader, VSpriteFragmentShader );
  glGenVertexArrays(1, @FVAO);
end;

procedure TSpriteEngine.SetScale( aScale : Byte );
begin
  FGrid.Init( FTileSize.X * aScale, FTileSize.Y * aScale );
end;

procedure TSpriteEngine.Draw;
var i : Byte;
    c : Byte;
begin
  FCurrentTexture := 0;
  FProgram.Bind;
  glUniform3f( FProgram.GetUniformLocation('uposition'), -FPos.X, -FPos.Y, 0 );
  if FLayerCount > 0 then
  for i := 1 to FLayerCount do
    DrawSet( FLayers[ i ] );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
end;

initialization

  Assert( SizeOf( Integer ) = SizeOf( GLInt ) );
  Assert( SizeOf( Single )  = SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQCoord )    = 8 * SizeOf( GLInt ) );
  Assert( SizeOf( TGLRawQTexCoord ) = 8 * SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQColor )    = 12 * SizeOf( GLByte ) );

end.

