unit vspriteengine;
{$include valkyrie.inc}
interface
uses
  Classes, SysUtils, vcolor, vgltypes, vglprogram, vglquadarrays;

type TSpriteEngine = class;


type

{ TSpriteDataVTC }

TSpriteDataVTC = class
  constructor Create( aEngine : TSpriteEngine );
  procedure Push( PosID : DWord; Pos : TGLVec2i; Color : TColor );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; color : PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0 );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; Color : TColor );
  procedure Push( coord : PGLRawQCoord; tex : PGLRawQTexCoord; color : PGLRawQColor );
  destructor Destroy; override;
private
  FData      : TGLTexturedColoredQuads;
  FEngine    : TSpriteEngine;
end;

type

{ TSpriteDataSet }

TSpriteDataSet = class
  Normal  : TSpriteDataVTC;
  Cosplay : TSpriteDataVTC;
  Glow    : TSpriteDataVTC;

  constructor Create( aEngine : TSpriteEngine; aCosplay, aGlow : Boolean );
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
  procedure Draw;
  procedure Update( aProjection : TMatrix44 );
  procedure DrawVTC( Data : TSpriteDataVTC );
  procedure DrawSet( const Data : TSpriteDataSet; const Tex : TTextureDataSet );
  // Foreground layer
  // Animation layer
  procedure SetTexture( TexID : DWord );
  destructor Destroy; override;
private
  FVAO         : Cardinal;
  FProgram     : TGLProgram;
  FProjection  : TMatrix44;
end;


implementation

uses
  vgl3library, math;

const VSPRITE_Z = 0;

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
'}'+#10;

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

destructor TSpriteDataSet.Destroy;
begin
  FreeAndNil( Normal );
  FreeAndNil( Cosplay );
  FreeAndNil( Glow );
end;

{ TSpriteDataVTC }

constructor TSpriteDataVTC.Create( aEngine : TSpriteEngine );
begin
  FEngine   := aEngine;
  FData     := TGLTexturedColoredQuads.Create;
end;


procedure TSpriteDataVTC.Push(PosID : DWord; Pos : TGLVec2i; Color : TColor);
var p1, p2     : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  p1 := Pos.Shifted(-1) * FEngine.FGrid;
  p2 := Pos * FEngine.FGrid;

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );
  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(1) * FEngine.FTexUnit;

  FData.PushQuad(
    TGLVec3i.CreateFrom( p1, VSPRITE_Z ),
    TGLVec3i.CreateFrom( p2, VSPRITE_Z ),
    Color.toVec43f,
    t1, t2
  );

end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; color: PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0 );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  p2 := pos + FEngine.FGrid.Scaled( Size );

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );
  tp += TGLVec2f.Create( TShiftX, TShiftY );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(Size) * FEngine.FTexUnit;

  FData.PushQuad(
    TGLVec3i.CreateFrom( pos, VSPRITE_Z ),
    TGLVec3i.CreateFrom( p2, VSPRITE_Z ),
    TGLQVec4f.Create(
      NewColor( color^.Data[0] ).toVec43f,
      NewColor( color^.Data[1] ).toVec43f,
      NewColor( color^.Data[2] ).toVec43f,
      NewColor( color^.Data[3] ).toVec43f
    ),
    t1, t2
  );
end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; Color : TColor );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  p2 := pos + FEngine.FGrid.Scaled( Size );
  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(Size) * FEngine.FTexUnit;

  FData.PushQuad(
    TGLVec3i.CreateFrom( pos, VSPRITE_Z ),
    TGLVec3i.CreateFrom( p2, VSPRITE_Z ),
    Color.toVec43f,
    t1, t2
  );
end;

procedure TSpriteDataVTC.Push(coord: PGLRawQCoord; tex: PGLRawQTexCoord; color: PGLRawQColor);
begin
  FData.PushQuad(
    TGLQVec3i.Create(
      TGLVec3i.CreateFrom( coord^.Data[0], VSPRITE_Z ),
      TGLVec3i.CreateFrom( coord^.Data[1], VSPRITE_Z ),
      TGLVec3i.CreateFrom( coord^.Data[2], VSPRITE_Z ),
      TGLVec3i.CreateFrom( coord^.Data[3], VSPRITE_Z )
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
  FProgram.Bind;

  Data.FData.Update;
  Data.FData.Draw;

  Data.FData.Clear;
{
  glBindBuffer( GL_ARRAY_BUFFER, Data.FCoordVBO );
  glBufferData( GL_ARRAY_BUFFER, Data.FSize*sizeof(TGLRawQCoord), @(Data.FCoords[0]), GL_STREAM_DRAW );
  glVertexAttribPointer(0, 2, GL_INT, GL_FALSE, 2 * sizeof(Integer), nil );
  glEnableVertexAttribArray(0);

  glBindBuffer( GL_ARRAY_BUFFER, Data.FTexCoVBO );
  glBufferData( GL_ARRAY_BUFFER, Data.FSize*sizeof(TGLRawQTexCoord), @(Data.FTexCoords[0]), GL_STREAM_DRAW );
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nil );
  glEnableVertexAttribArray(1);

  glBindBuffer( GL_ARRAY_BUFFER, Data.FColorVBO);
  glBufferData( GL_ARRAY_BUFFER, Data.FSize*sizeof(TGLRawQColor), @(Data.FColors[0]), GL_STREAM_DRAW );
  glVertexAttribPointer(2, 3, GL_UNSIGNED_BYTE, GL_TRUE, 3 * sizeof(GLubyte), nil );
  glEnableVertexAttribArray(2);

  glDrawArrays( GL_QUADS, 0, Data.FSize*4 );
}
  FProgram.UnBind;
//  glDisableVertexAttribArray(0);
//  glDisableVertexAttribArray(1);
//  glDisableVertexAttribArray(2);
//  glBindBuffer( GL_ARRAY_BUFFER, 0);
end;

procedure TSpriteEngine.DrawSet(const Data: TSpriteDataSet; const Tex : TTextureDataSet);
begin
  glActiveTexture(0);
  if not Data.Normal.FData.Empty then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    SetTexture( Tex.Normal );
    DrawVTC( Data.Normal );
  end;

  if (Data.Cosplay <> nil) and (not Data.Cosplay.FData.Empty) then
  begin
    glBlendFunc( GL_ONE, GL_ONE );
    SetTexture( Tex.Cosplay );
    DrawVTC( Data.Cosplay );
  end;

  if (Data.Glow <> nil) and (not Data.Glow.FData.Empty) then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    SetTexture( Tex.Glow );
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

  FProgram := TGLProgram.Create( VSpriteVertexShader, VSpriteFragmentShader );
  glGenVertexArrays(1, @FVAO);
end;

procedure TSpriteEngine.Draw;
var i : Byte;
begin
  FCurrentTexture := 0;
  FProgram.Bind;
  glUniform3f( FProgram.GetUniformLocation('uposition'), -FPos.X, -FPos.Y, 0 );
  if FLayerCount > 0 then
  for i := 1 to FLayerCount do
    DrawSet( FLayers[ i ], FTextureSet.Layer[ i ] );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
end;

initialization

  Assert( SizeOf( Integer ) = SizeOf( GLInt ) );
  Assert( SizeOf( Single )  = SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQCoord )    = 8 * SizeOf( GLInt ) );
  Assert( SizeOf( TGLRawQTexCoord ) = 8 * SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQColor )    = 12 * SizeOf( GLByte ) );

end.

