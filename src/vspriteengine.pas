unit vspriteengine;
{$include valkyrie.inc}
interface
uses SysUtils, vgenerics, vcolor, vgltypes, vglprogram, vglquadarrays, vtextures;

type TSpriteEngine = class;


type

{ TSpriteDataVTC }

TSpriteDataVTC = class
  constructor Create( aEngine : TSpriteEngine; aTexture : TTexture );
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
  constructor Create( aEngine : TSpriteEngine; aNormal, aCosplay, aGlow : TTexture; aOrder : Integer );
  destructor Destroy; override;
private
  FNormal  : TSpriteDataVTC;
  FCosplay : TSpriteDataVTC;
  FGlow    : TSpriteDataVTC;
  FOrder   : Integer;
public
  property Normal  : TSpriteDataVTC read FNormal;
  property Cosplay : TSpriteDataVTC read FCosplay;
  property Glow    : TSpriteDataVTC read FGlow;
  property Order   : Integer read FOrder;
end;

type TSpriteDataSetArray = specialize TGArray< TSpriteDataSet >;

{ TSpriteEngine }

TSpriteEngine = class
  constructor Create( aTileSize : TGLVec2i; aScale : Byte = 1 );
  procedure SetScale( aScale : Byte );
  procedure Draw;
  procedure Update( aProjection : TMatrix44 );
  procedure DrawVTC( Data : TSpriteDataVTC );
  procedure DrawSet( const Data : TSpriteDataSet );
  procedure SetTexture( TexID : DWord );
  function Add( aNormal, aCosplay, aGlow : TTexture; aOrder : Integer ) : Integer;
  destructor Destroy; override;
private
  FVAO            : Cardinal;
  FProgram        : TGLProgram;
  FProjection     : TMatrix44;
  FCurrentTexture : DWord;
  FGrid           : TGLVec2i;
  FTileSize       : TGLVec2i;
  FPosition       : TGLVec2i;
  FLayersDirty    : Boolean;
  FLayers         : TSpriteDataSetArray;
  FLayersSorted   : TSpriteDataSetArray;
public
  property Grid     : TGLVec2i read FGrid;
  property TileSize : TGLVec2i read FTileSize;
  property Position : TGLVec2i read FPosition write FPosition;
  property Layers   : TSpriteDataSetArray read FLayers;
end;


implementation

uses vgl3library, vdebug;

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

constructor TSpriteDataSet.Create( aEngine : TSpriteEngine; aNormal, aCosplay, aGlow : TTexture; aOrder : Integer );
begin
  FNormal  := nil;
  FCosplay := nil;
  FGlow    := nil;

  if aNormal  <> nil then FNormal  := TSpriteDataVTC.Create( aEngine, aNormal );
  if aCosplay <> nil then FCosplay := TSpriteDataVTC.Create( aEngine, aCosplay );
  if aGlow    <> nil then FGlow    := TSpriteDataVTC.Create( aEngine, aGlow );

  FOrder := aOrder;
end;

destructor TSpriteDataSet.Destroy;
begin
  FreeAndNil( FNormal );
  FreeAndNil( FCosplay );
  FreeAndNil( FGlow );
end;

{ TSpriteDataVTC }

constructor TSpriteDataVTC.Create( aEngine : TSpriteEngine; aTexture : TTexture );
var iTilesY : DWord;
begin
  Assert( aTexture <> nil, 'Nil texture passed!');
  FEngine    := aEngine;
  FData      := TGLTexturedColoredQuads.Create;
  FTextureID := aTexture.GLTexture;
  FRowSize   := aTexture.Size.X div FEngine.TileSize.X;
  iTilesY    := aTexture.Size.Y div FEngine.TileSize.Y;
  FTexUnit.Init( 1.0 / FRowSize, 1.0 / iTilesY );
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

  if not Data.FNormal.FData.Empty then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    DrawVTC( Data.FNormal );
  end;

  if (Data.FCosplay <> nil) and (not Data.Cosplay.FData.Empty) then
  begin
    glBlendFunc( GL_ONE, GL_ONE );
    DrawVTC( Data.FCosplay );
  end;

  if (Data.FGlow <> nil) and (not Data.Glow.FData.Empty) then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    DrawVTC( Data.FGlow );
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
var iSet : TSpriteDataSet;
begin
  for iSet in FLayers do
    iSet.Free;
  glDeleteVertexArrays(1, @FVAO);
  FreeAndNil( FProgram );
  FreeAndNil( FLayers );
  FreeAndNil( FLayersSorted );
end;

constructor TSpriteEngine.Create( aTileSize : TGLVec2i; aScale : Byte = 1 );
begin
  FTileSize := aTileSize;
  SetScale( aScale );
  FPosition.Init(0,0);
  FCurrentTexture    := 0;
  FLayersDirty       := True;

  FProgram := TGLProgram.Create( VSpriteVertexShader, VSpriteFragmentShader );
  glGenVertexArrays(1, @FVAO);

  FLayers       := TSpriteDataSetArray.Create;
  FLayersSorted := TSpriteDataSetArray.Create;
end;

procedure TSpriteEngine.SetScale( aScale : Byte );
begin
  FGrid.Init( FTileSize.X * aScale, FTileSize.Y * aScale );
end;

function TSpriteEngine.Add( aNormal, aCosplay, aGlow : TTexture; aOrder : Integer ) : Integer;
var i : DWord;
begin
  Assert( aNormal <> nil, 'Normal texture needs to be present in spritesheet!');
  if FLayers.Size > 0 then
  for i := 0 to FLayers.Size - 1 do
    if FLayers[i].Normal.TextureID = aNormal.GLTexture then
      Exit(i);
  FLayersDirty := True;
  FLayers.Push( TSpriteDataSet.Create( Self, aNormal, aCosplay, aGlow, aOrder ) );
  Exit( Flayers.Size - 1 );
end;

function SpriteEngineLayerSort( const aLayerA, aLayerB : TSpriteDataSet ) : Integer;
begin
  Exit( aLayerA.Order - aLayerB.Order );
end;

procedure TSpriteEngine.Draw;
var iSet : TSpriteDataSet;
begin
  if FLayersDirty then
  begin
    FLayersSorted.Clear;
    for iSet in FLayers do
      FLayersSorted.Push( iSet );
    FLayersSorted.Sort( @SpriteEngineLayerSort );
    FLayersDirty := False;
  end;

  FCurrentTexture := 0;
  FProgram.Bind;
  glUniform3f( FProgram.GetUniformLocation('uposition'), -FPosition.X, -FPosition.Y, 0 );
  for iSet in FLayersSorted do
    DrawSet( iSet );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
end;

initialization

  Assert( SizeOf( Integer ) = SizeOf( GLInt ) );
  Assert( SizeOf( Single )  = SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQCoord )    = 8 * SizeOf( GLInt ) );
  Assert( SizeOf( TGLRawQTexCoord ) = 8 * SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQColor )    = 12 * SizeOf( GLByte ) );

end.

