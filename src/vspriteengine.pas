unit vspriteengine;
{$INCLUDE valkyrie.inc}
interface
uses sysutils, vgenerics, vvector, vrltools, vcolor, vgltypes, vglprogram, vglquadarrays, vglframebuffer, vtextures;

type TSpriteEngine = class;
type TGLTexturedColored4Quads = class;
type TGLSpriteTransitionQuads = class;

// Sprite IDs are local to a single dataset. Shift uses sprite-sized UV units.
type TSpriteTransitionMaterial = object
  SpriteID : DWord;
  Color    : TColor;
  Emissive : TColor;
  Shift    : TVec2f;
  function Compare( const aOther : TSpriteTransitionMaterial ) : Integer;
end;

type TSpriteTransitionMaterials = array[0..3] of TSpriteTransitionMaterial;

// Constant attributes shared by the six vertices of one transition patch.
type TSpriteTransitionPayload = record
  PivotWidth  : TVec4f;
  Origins01   : TVec4f;
  Origins23   : TVec4f;
  CornerMasks : TVec4f;
  Tints       : array[0..3] of TVec4f;
  Emissions   : array[0..3] of TVec4f;
end;

type TSpriteProgramUniforms = record
  Transform : Integer;
  Position  : Integer;
end;

type

{ TSpriteDataSet }

TSpriteDataSet = class
  constructor Create( aEngine : TSpriteEngine; aNormal, aCosplay, aEmissive, aOutline : TTexture; aOrder : Integer; aTransitions : Boolean = False );
  procedure Push( aSpriteID : DWord; aCoord : TCoord2D; aColor, aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0 );
  procedure PushXY( aSpriteID, aSize : DWord; aPos : TVec2i; aQColor : PGLRawQColor; aCosColor, aGlowColor, aEmissive : TColor; TShiftX : Single = 0; TShiftY : Single = 0; aZ : Integer = 0 );
  procedure PushXY( aSpriteID, aSize : DWord; aPos : TVec2i; aColor, aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0; aScale : Single = 1.0 );
  procedure Push( aQCoord : PGLRawQCoord; aQTex : PGLRawQTexCoord; aQColor : PGLRawQColor; aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0 );
  procedure Push( aQCoord : PGLRawQCoord; aQTex : PGLRawQTexCoord; aColor, aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0 );
  procedure PushPart( aSpriteID : DWord; aPa, aPb : TVec2i; aQColor : PGLRawQColor; aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer; aTa, aTb : TVec2f );
  // Quadrants and candidate slots are NW, NE, SW, SE; aMask selects candidates.
  procedure PushTransition( aCoord : TCoord2D; aQuadrant, aMask : Byte; const aMaterials : TSpriteTransitionMaterials;
    const aLight : TGLRawQColor; aZ : Integer; aWidth : Single );
  destructor Destroy; override;
private
  FTransitions : TGLSpriteTransitionQuads;
  FData        : TGLTexturedColored4Quads;
  FEngine      : TSpriteEngine;
  FTexUnit     : TVec2f;
  FRowSize     : Word;
  FTNormalID   : DWord;
  FTCosplayID  : DWord;
  FTEmissiveID : DWord;
  FTOutlineID  : DWord;
  FOrder       : Integer;
  procedure EnableTransitions;
  function GetSupportsTransitions : Boolean;
public
  property SupportsTransitions : Boolean read GetSupportsTransitions;
  property TexUnit     : TVec2f  read FTexUnit;
  property RowSize     : Word    read FRowSize;
  property TNormalID   : DWord   read FTNormalID;
  property TCosplayID  : DWord   read FTCosplayID;
  property TEmissiveID : DWord   read FTEmissiveID;
  property TOutlineID  : DWord   read FTOutlineID;
  property Order       : Integer read FOrder;
end;

type TSpriteDataSetArray = specialize TGArray< TSpriteDataSet >;

{ TSpriteEngine }

TSpriteEngine = class
  constructor Create( aTileSize : TVec2i; aScale : Byte = 1 );
  procedure Reset;
  procedure Clear;
  // Loading only: prepare transition-capable datasets on each target (nil = window).
  // Reject queued geometry, restore GL state, and touch only one pixel per target.
  // The caller must clear the targets before presenting the next frame.
  procedure WarmUp( const aTargets : array of TGLFramebuffer );
  procedure SetScale( aScale : Byte );
  procedure SetScale( aScale : Single );
  procedure Draw;
  procedure Update( aProjection : TMatrix44 );
  procedure DrawSet( const aData : TSpriteDataSet );
  function Add( aNormal, aCosplay, aEmissive, aOutline : TTexture; aOrder : Integer; aTransitions : Boolean = False ) : Integer;
  destructor Destroy; override;
private
  procedure SetTexture( aTextureID : DWord );
  procedure UpdatePosition( aLocation : Integer );
  procedure WarmUpDataSet( aData : TSpriteDataSet );
private

  FVAO                : Cardinal;
  FProgram            : TGLProgram;
  FTransitionProgram  : TGLProgram;
  FSpriteUniforms     : TSpriteProgramUniforms;
  FTransitionUniforms : TSpriteProgramUniforms;
  FTransitionTexUnit  : Integer;
  FProjection         : TMatrix44;
  FCurrentTexture     : DWord;
  FGrid               : TVec2i;
  FTileSize           : TVec2i;
  FPosition           : TVec2i;
  FScale              : Single;
  FLayersDirty        : Boolean;
  FFuzzyMode          : Boolean;
  FLayers             : TSpriteDataSetArray;
  FLayersSorted       : TSpriteDataSetArray;
  FTZeroID            : DWord;
public
  property Scale     : Single read FScale;
  property Grid      : TVec2i read FGrid;
  property TileSize  : TVec2i read FTileSize;
  property Position  : TVec2i read FPosition write FPosition;
  property Layers    : TSpriteDataSetArray read FLayers;
end;

type TGLTexturedColored4Quads = class( TGLTexturedQuads )
  constructor Create;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColor, aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColorQuad : TGLQVec4f; aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure PushQuad ( aCoord : TGLQVec3i; aColorQuad : TGLQVec4f; aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure PushRotatedQuad ( aCenter, aSize : TGLVec3i; aDegrees : Single; aColor, aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure Append( aList : TGLTexturedColored4Quads );
end;

type TGLSpriteTransitionQuads = class( TGLTexturedQuads )
  constructor Create;
  procedure PushQuad( aPa, aPb : TVec2i; aSourceA, aSourceB : TVec2f; aZ : Integer;
    const aLight : TGLQVec4f; const aPayload : TSpriteTransitionPayload );
end;

implementation

uses math, vgl3library, vdebug;

{$PUSH}
{$H+}
const
// Match VSpriteTransitionVertexShader's attribute locations.
VGL_TRANSITION_LIGHT       = 2;
VGL_TRANSITION_PIVOT_WIDTH = 3;
VGL_TRANSITION_ORIGINS01   = 4;
VGL_TRANSITION_ORIGINS23   = 5;
VGL_TRANSITION_MASKS       = 6;
VGL_TRANSITION_TINTS       = 7;
VGL_TRANSITION_EMISSIONS   = 11;

// Both material-selection paths use the same tint, light and emission rules.
VSpriteShadingFunctions =
'vec4 shade_sprite(vec2 uv, vec4 light, vec4 tint_color, vec4 emission_color, out float emissive) {'+#10+
'  emissive = texture(uemissive, uv).x;'+#10+
'  vec4 tint = vec4(texture(ucosplay, uv).xyz, 0.0);'+#10+
'  if (emissive > 0.0) tint *= emission_color; else tint *= vec4(tint_color.xyz, 1.0);'+#10+
'  vec4 color = (texture(unormal, uv) + tint) * vec4(max(light.xyz, vec3(emissive)), light.w);'+#10+
'  if (emissive > 0.0 && (light.x != light.y || light.y != light.z)) color *= light;'+#10+
'  if (color.w < 0.1) discard;'+#10+
'  return color;'+#10+
'}'+#10+
'vec4 sprite_emission(vec4 color, float emissive, float strength) {'+#10+
'  return vec4(emissive * color.xyz * strength, 1.0);'+#10+
'}'+#10;

VSpriteVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec3 position;'+#10+
'layout (location = 1) in vec2 texcoord;'+#10+
'layout (location = 2) in vec4 color;'+#10+
'layout (location = 3) in vec4 cos_color;'+#10+
'layout (location = 4) in vec4 glow_color;'+#10+
'layout (location = 5) in vec4 emissive_color;'+#10+
'uniform mat4 utransform;'+#10+
'uniform vec3 uposition;'+#10+
#10+
'out vec4 ocolor;'+#10+
'out vec4 ocos_color;'+#10+
'out vec4 oglow_color;'+#10+
'out vec4 oemissive_color;'+#10+
'out vec2 otexcoord;'+#10+
#10+
'void main() {'+#10+
'ocolor          = color;'+#10+
'ocos_color      = cos_color;'+#10+
'oglow_color     = glow_color;'+#10+
'oemissive_color = emissive_color;'+#10+
'otexcoord = texcoord;'+#10+
'gl_Position = utransform * vec4(uposition + position, 1.0);'+#10+
'}'+#10;
VSpriteFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec4 ocolor;'+#10+
'in vec4 ocos_color;'+#10+
'in vec4 oglow_color;'+#10+
'in vec4 oemissive_color;'+#10+
'in vec2 otexcoord;'+#10+
'uniform sampler2D unormal;'+#10+
'uniform sampler2D ucosplay;'+#10+
'uniform sampler2D uemissive;'+#10+
'uniform sampler2D uoutline;'+#10+
'layout (location = 0) out vec4 frag_color;'+#10+
'layout (location = 1) out vec4 emissive_color;'+#10+
VSpriteShadingFunctions+
'void main() {'+#10+
'float emissive;'+#10+
'vec4 out_color = shade_sprite(otexcoord, ocolor, ocos_color, oemissive_color, emissive);'+#10+
'float outline    = texture(uoutline, otexcoord).x;'+#10+
'if ( oglow_color.w > 0 && outline > 0 && emissive == 0.0 ) {'+#10+
'  if ( oglow_color.w < 0.2 ) out_color.xyz = oglow_color.xyz * outline;'+#10+
'  emissive_color = vec4( oglow_color.xyz, emissive > 0.0 ? 1.0 : 0.0 );'+#10+
'} else'+#10+
'  emissive_color = sprite_emission(out_color, emissive, oemissive_color.w);'+#10+
'frag_color     = out_color;'+#10+
'}'+#10;

VSpriteTransitionVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec3 position;'+#10+
'layout (location = 1) in vec2 source_position;'+#10+
'layout (location = 2) in vec4 color;'+#10+
'layout (location = 3) in vec4 pivot_width;'+#10+
'layout (location = 4) in vec4 origins01;'+#10+
'layout (location = 5) in vec4 origins23;'+#10+
'layout (location = 6) in vec4 masks;'+#10+
'layout (location = 7) in vec4 colors[4];'+#10+
'layout (location = 11) in vec4 emissions[4];'+#10+
'uniform mat4 utransform;'+#10+
'uniform vec3 uposition;'+#10+
'out vec2 osource;'+#10+
'out vec4 ocolor;'+#10+
'flat out vec4 opivot_width;'+#10+
'flat out vec4 oorigins01;'+#10+
'flat out vec4 oorigins23;'+#10+
'flat out vec4 omasks;'+#10+
'flat out vec4 ocolors[4];'+#10+
'flat out vec4 oemissions[4];'+#10+
'void main() {'+#10+
'  osource = source_position;'+#10+
'  ocolor = color;'+#10+
'  opivot_width = pivot_width;'+#10+
'  oorigins01 = origins01;'+#10+
'  oorigins23 = origins23;'+#10+
'  omasks = masks;'+#10+
'  for (int i = 0; i < 4; ++i) {'+#10+
'    ocolors[i] = colors[i];'+#10+
'    oemissions[i] = emissions[i];'+#10+
'  }'+#10+
'  gl_Position = utransform * vec4(uposition + position, 1.0);'+#10+
'}'+#10;
VSpriteTransitionFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec2 osource;'+#10+
'in vec4 ocolor;'+#10+
'flat in vec4 opivot_width;'+#10+
'flat in vec4 oorigins01;'+#10+
'flat in vec4 oorigins23;'+#10+
'flat in vec4 omasks;'+#10+
'flat in vec4 ocolors[4];'+#10+
'flat in vec4 oemissions[4];'+#10+
'uniform sampler2D unormal;'+#10+
'uniform sampler2D ucosplay;'+#10+
'uniform sampler2D uemissive;'+#10+
'uniform vec2 utile_size;'+#10+
'uniform vec2 utex_unit;'+#10+
'layout (location = 0) out vec4 frag_color;'+#10+
'layout (location = 1) out vec4 emissive_color;'+#10+
VSpriteShadingFunctions+
'const int bayer[16] = int[16](0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5);'+#10+
'void main() {'+#10+
'  ivec2 pixel = ivec2(floor(osource));'+#10+
'  vec2 fraction = clamp(0.5 + (vec2(pixel) + 0.5 - opivot_width.xy) / (2.0 * opivot_width.z), 0.0, 1.0);'+#10+
'  vec4 corners = vec4((1.0-fraction.x)*(1.0-fraction.y), fraction.x*(1.0-fraction.y),'+#10+
'                      (1.0-fraction.x)*fraction.y, fraction.x*fraction.y);'+#10+
'  float weights[4];'+#10+
'  float total = 0.0;'+#10+
'  for (int i = 0; i < 4; ++i) {'+#10+
'    int mask = int(omasks[i]);'+#10+
'    weights[i] = 0.0;'+#10+
'    for (int j = 0; j < 4; ++j)'+#10+
'      if ((mask & (1 << j)) != 0) weights[i] += corners[j];'+#10+
'    total += weights[i];'+#10+
'  }'+#10+
'  float threshold = (float(bayer[(pixel.y & 3)*4 + (pixel.x & 3)]) + 0.5) / 16.0 * total;'+#10+
'  float cumulative = 0.0;'+#10+
'  int selected = 0;'+#10+
'  for (int i = 0; i < 4; ++i) {'+#10+
'    cumulative += weights[i];'+#10+
'    if (threshold < cumulative) { selected = i; break; }'+#10+
'  }'+#10+
'  vec2 origins[4] = vec2[4](oorigins01.xy, oorigins01.zw, oorigins23.xy, oorigins23.zw);'+#10+
'  vec2 uv = origins[selected] + fract(osource / utile_size) * utex_unit;'+#10+
'  float emissive;'+#10+
'  vec4 color = shade_sprite(uv, ocolor, ocolors[selected], oemissions[selected], emissive);'+#10+
'  frag_color = color;'+#10+
'  emissive_color = sprite_emission(color, emissive, oemissions[selected].w);'+#10+
'}'+#10;
{$POP}

{ TSpriteTransitionMaterial }

function TSpriteTransitionMaterial.Compare( const aOther : TSpriteTransitionMaterial ) : Integer;
begin
  if SpriteID < aOther.SpriteID then Exit( -1 );
  if SpriteID > aOther.SpriteID then Exit( 1 );
  if Color.toDWord < aOther.Color.toDWord then Exit( -1 );
  if Color.toDWord > aOther.Color.toDWord then Exit( 1 );
  if Emissive.toDWord < aOther.Emissive.toDWord then Exit( -1 );
  if Emissive.toDWord > aOther.Emissive.toDWord then Exit( 1 );
  if Shift.X < aOther.Shift.X then Exit( -1 );
  if Shift.X > aOther.Shift.X then Exit( 1 );
  if Shift.Y < aOther.Shift.Y then Exit( -1 );
  if Shift.Y > aOther.Shift.Y then Exit( 1 );
  Exit( 0 );
end;

{ TSpriteDataSet }

constructor TSpriteDataSet.Create( aEngine : TSpriteEngine; aNormal, aCosplay, aEmissive, aOutline : TTexture; aOrder : Integer; aTransitions : Boolean );
var iTilesY : Integer;
begin
  Assert( aNormal <> nil, 'Nil texture passed!');
  FEngine      := aEngine;
  FData        := TGLTexturedColored4Quads.Create;
  FTNormalID   := aNormal.GLTexture;
  FTCosplayID  := 0;
  FTEmissiveID := 0;
  FTOutlineID  := 0;
  if aCosplay  <> nil then FTCosplayID  := aCosplay.GLTexture;
  if aEmissive <> nil then FTEmissiveID := aEmissive.GLTexture;
  if aOutline  <> nil then FTOutlineID  := aOutline.GLTexture;
  FRowSize     := aNormal.Size.X div FEngine.TileSize.X;
  iTilesY      := aNormal.Size.Y div FEngine.TileSize.Y;
  FTexUnit.Init( 1.0 / FRowSize, 1.0 / iTilesY );
  FOrder       := aOrder;
  if aTransitions then EnableTransitions;
end;

procedure TSpriteDataSet.EnableTransitions;
begin
  if FTransitions = nil then FTransitions := TGLSpriteTransitionQuads.Create;
end;

function TSpriteDataSet.GetSupportsTransitions : Boolean;
begin
  Result := FTransitions <> nil;
end;

destructor TSpriteDataSet.Destroy;
begin
  FreeAndNil( FTransitions );
  FreeAndNil( FData );
end;

{ TSpriteDataVTC }

procedure TSpriteDataSet.Push( aSpriteID : DWord; aCoord : TCoord2D; aColor, aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0);
var iv2a, iv2b    : TVec2i;
    ita, itb, its : TVec2f;
begin
  iv2a := Vec2i( aCoord.X-1, aCoord.Y-1 ) * FEngine.FGrid;
  iv2b := Vec2i( aCoord.X, aCoord.Y )     * FEngine.FGrid;

  its := TVec2f.CreateModDiv( aSpriteID-1, FRowSize );
  ita := its * FTexUnit;
  itb := its.Shifted(1) * FTexUnit;

  FData.PushQuad(
    TVec3i.CreateFrom( iv2a, aZ ),
    TVec3i.CreateFrom( iv2b, aZ ),
    aColor.toVec43f,
    aCosColor.toVec43f,
    aGlowColor.toVec4f,
    aEmissive.toVec4f,
    ita, itb
  );

end;

procedure TSpriteDataSet.PushXY( aSpriteID, aSize : DWord; aPos : TVec2i; aQColor : PGLRawQColor; aCosColor, aGlowColor, aEmissive : TColor; TShiftX : Single = 0; TShiftY : Single = 0; aZ : Integer = 0 );
var iv2b          : TVec2i;
    ita, itb, its : TVec2f;
begin
  iv2b := aPos + FEngine.FGrid.Scaled( aSize );

  its := TVec2f.CreateModDiv( aSpriteID-1, FRowSize );
  its += TVec2f.Create( TShiftX, TShiftY );

  ita := its * FTexUnit;
  itb := its.Shifted( aSize ) * FTexUnit;

  FData.PushQuad(
    TVec3i.CreateFrom( aPos, aZ ),
    TVec3i.CreateFrom( iv2b, aZ ),
    TGLQVec4f.Create(
      NewColor( aQColor^.Data[0] ).toVec43f,
      NewColor( aQColor^.Data[1] ).toVec43f,
      NewColor( aQColor^.Data[2] ).toVec43f,
      NewColor( aQColor^.Data[3] ).toVec43f
    ),
    aCosColor.toVec43f,
    aGlowColor.toVec4f,
    aEmissive.toVec4f,
    ita, itb
  );
end;

procedure TSpriteDataSet.PushXY( aSpriteID, aSize : DWord; aPos : TVec2i; aColor, aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0; aScale : Single = 1.0 );
var iv2a, iv2b, iv2o : TVec2i;
    ita, itb, its    : TVec2f;
begin
  iv2a := aPos;
  iv2b := aPos + FEngine.FGrid.Scaled( aSize );
  if aScale <> 1.0 then
  begin
    iv2o := iv2b - iv2a;
    iv2o := iv2o.ScaledF( ( 1.0 - aScale ) * 0.5 );
    iv2a += iv2o;
    iv2b -= iv2o;
  end;

  its := TVec2f.CreateModDiv( aSpriteID-1, FRowSize );
  ita := its * FTexUnit;
  itb := its.Shifted( aSize ) * FTexUnit;

  FData.PushQuad(
    TVec3i.CreateFrom( iv2a, aZ ),
    TVec3i.CreateFrom( iv2b, aZ ),
    aColor.toVec43f,
    aCosColor.toVec4f,
    aGlowColor.toVec4f,
    aEmissive.toVec4f,
    ita, itb
  );
end;

procedure TSpriteDataSet.Push( aQCoord : PGLRawQCoord; aQTex : PGLRawQTexCoord; aQColor : PGLRawQColor; aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0);
begin
  FData.PushQuad(
    TGLQVec3i.Create(
      TVec3i.CreateFrom( aQCoord^.Data[0], aZ ),
      TVec3i.CreateFrom( aQCoord^.Data[1], aZ ),
      TVec3i.CreateFrom( aQCoord^.Data[2], aZ ),
      TVec3i.CreateFrom( aQCoord^.Data[3], aZ )
    ),
    TGLQVec4f.Create(
      NewColor( aQColor^.Data[0] ).toVec43f,
      NewColor( aQColor^.Data[1] ).toVec43f,
      NewColor( aQColor^.Data[2] ).toVec43f,
      NewColor( aQColor^.Data[3] ).toVec43f
    ),
    aCosColor.toVec43f,
    aGlowColor.toVec4f,
    aEmissive.toVec4f,
    aQTex^.Data[0], aQTex^.Data[2]
  );
end;

procedure TSpriteDataSet.Push( aQCoord : PGLRawQCoord; aQTex : PGLRawQTexCoord; aColor, aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer = 0);
var iColorVec : TGLVec4f;
begin
  iColorVec := aColor.toVec4f;
  FData.PushQuad(
    TGLQVec3i.Create(
      TVec3i.CreateFrom( aQCoord^.Data[0], aZ ),
      TVec3i.CreateFrom( aQCoord^.Data[1], aZ ),
      TVec3i.CreateFrom( aQCoord^.Data[2], aZ ),
      TVec3i.CreateFrom( aQCoord^.Data[3], aZ )
    ),
    TGLQVec4f.Create( iColorVec, iColorVec, iColorVec, iColorVec ),
    aCosColor.toVec4f,
    aGlowColor.toVec4f,
    aEmissive.toVec4f,
    aQTex^.Data[0], aQTex^.Data[2]
  );
end;

procedure TSpriteDataSet.PushPart( aSpriteID : DWord; aPa, aPb : TVec2i; aQColor : PGLRawQColor; aCosColor, aGlowColor, aEmissive : TColor; aZ : Integer; aTa, aTb : TVec2f );
var its : TVec2f;
begin
  its := TVec2f.CreateModDiv( aSpriteID-1, FRowSize );
  ata := ( its + aTa ) * FTexUnit;
  atb := ( its + aTb ) * FTexUnit;

  FData.PushQuad(
    TVec3i.CreateFrom( aPa, aZ ),
    TVec3i.CreateFrom( aPb, aZ ),
    TGLQVec4f.Create(
      NewColor( aQColor^.Data[0] ).toVec43f,
      NewColor( aQColor^.Data[1] ).toVec43f,
      NewColor( aQColor^.Data[2] ).toVec43f,
      NewColor( aQColor^.Data[3] ).toVec43f
    ),
    aCosColor.toVec43f,
    aGlowColor.toVec4f,
    aEmissive.toVec4f,
    ata, atb
  );
end;

procedure TSpriteDataSet.PushTransition( aCoord : TCoord2D; aQuadrant, aMask : Byte;
  const aMaterials : TSpriteTransitionMaterials; const aLight : TGLRawQColor; aZ : Integer; aWidth : Single );
var iSorted       : TSpriteTransitionMaterials;
    iMasks        : array[0..3] of Byte;
    iPayload      : TSpriteTransitionPayload;
    iUV           : array[0..3] of TVec2f;
    iColors       : array[0..3] of TVec4f;
    iCount, i, j  : Integer;
    iIndex        : Integer;
    iQuadrant     : TVec2i;
    iTilePos      : TVec2i;
    iPa, iPb      : TVec2i;
    iStart, iEnd  : TVec2f;
    iWorld        : TVec2f;
    iSourceA      : TVec2f;
    iSourceB      : TVec2f;
    iPivot        : TVec2f;
    iLight        : TGLQVec4f;

    function LightAt( aX, aY : Single ) : TVec4f;
    begin
      // Preserve the ordinary quad's NW-to-SE diagonal and triangle gradients.
      if aX <= aY then
        Exit( iColors[0].Scaled( 1-aY ) + iColors[1].Scaled( aY-aX ) + iColors[2].Scaled( aX ) );
      Exit( iColors[0].Scaled( 1-aX ) + iColors[2].Scaled( aY ) + iColors[3].Scaled( aX-aY ) );
    end;

begin
  if FTransitions = nil then
    raise Exception.Create( 'Sprite transitions must be enabled at registration' );
  Assert( aQuadrant < 4 );
  Assert( (aMask and (1 shl (3 xor aQuadrant))) <> 0, 'Transition must include its destination' );
  Assert( (aWidth > 0) and (aWidth <= FEngine.FTileSize.X/2) and (aWidth <= FEngine.FTileSize.Y/2) );

  // Sort by appearance, merging corner weights for identical surfaces.
  iCount := 0;
  FillChar( iMasks, SizeOf( iMasks ), 0 );
  for i := 0 to 3 do
    if (aMask and (1 shl i)) <> 0 then
    begin
      Assert( aMaterials[i].SpriteID > 0 );
      iIndex := 0;
      while (iIndex < iCount) and (iSorted[iIndex].Compare( aMaterials[i] ) < 0) do Inc( iIndex );
      if (iIndex < iCount) and (iSorted[iIndex].Compare( aMaterials[i] ) = 0) then
        iMasks[iIndex] := iMasks[iIndex] or (1 shl i)
      else
      begin
        for j := iCount downto iIndex+1 do
        begin
          iSorted[j] := iSorted[j-1];
          iMasks[j] := iMasks[j-1];
        end;
        iSorted[iIndex] := aMaterials[i];
        iMasks[iIndex] := 1 shl i;
        Inc( iCount );
      end;
    end;
  for i := iCount to 3 do
  begin
    iSorted[i] := iSorted[0];
    iMasks[i] := 0;
  end;
  for i := 0 to 3 do
    iColors[i] := NewColor( aLight.Data[i] ).toVec43f;
  iQuadrant := Vec2i( aQuadrant and 1, aQuadrant shr 1 );
  iTilePos := Vec2i( aCoord.X-1, aCoord.Y-1 ) * FEngine.FGrid;
  iPa := Vec2i( iQuadrant.X * FEngine.FGrid.X div 2, iQuadrant.Y * FEngine.FGrid.Y div 2 );
  iPb := Vec2i( (iQuadrant.X+1) * FEngine.FGrid.X div 2, (iQuadrant.Y+1) * FEngine.FGrid.Y div 2 );
  iStart := TVec2f.Create( iPa.X / FEngine.FGrid.X, iPa.Y / FEngine.FGrid.Y );
  iEnd := TVec2f.Create( iPb.X / FEngine.FGrid.X, iPb.Y / FEngine.FGrid.Y );
  iLight := TGLQVec4f.Create( LightAt( iStart.X, iStart.Y ), LightAt( iStart.X, iEnd.Y ),
    LightAt( iEnd.X, iEnd.Y ), LightAt( iEnd.X, iStart.Y ) );
  if iCount = 1 then
  begin
    // Uniform quadrants retain the same geometry/light but use the ordinary format.
    iUV[0] := (TVec2f.CreateModDiv( iSorted[0].SpriteID-1, FRowSize ) + iSorted[0].Shift) * FTexUnit;
    FData.PushQuad( TVec3i.CreateFrom( iTilePos+iPa, aZ ), TVec3i.CreateFrom( iTilePos+iPb, aZ ),
      iLight, iSorted[0].Color.toVec43f, ColorZero.toVec4f, iSorted[0].Emissive.toVec4f,
      iUV[0] + iStart * FTexUnit, iUV[0] + iEnd * FTexUnit );
    Exit;
  end;
  for i := 0 to 3 do
  begin
    iUV[i] := (TVec2f.CreateModDiv( iSorted[i].SpriteID-1, FRowSize ) + iSorted[i].Shift) * FTexUnit;
    iPayload.CornerMasks.Data[i] := iMasks[i];
    iPayload.Tints[i] := iSorted[i].Color.toVec43f;
    iPayload.Emissions[i] := iSorted[i].Emissive.toVec4f;
  end;
  iWorld := TVec2f.Create( (aCoord.X-1) * FEngine.FTileSize.X, (aCoord.Y-1) * FEngine.FTileSize.Y );
  iSourceA := iWorld + TVec2f.Create( iStart.X * FEngine.FTileSize.X, iStart.Y * FEngine.FTileSize.Y );
  iSourceB := iWorld + TVec2f.Create( iEnd.X * FEngine.FTileSize.X, iEnd.Y * FEngine.FTileSize.Y );
  iPivot := iWorld + TVec2f.Create( iQuadrant.X * FEngine.FTileSize.X, iQuadrant.Y * FEngine.FTileSize.Y );
  iPayload.PivotWidth := TVec4f.Create( iPivot.X, iPivot.Y, aWidth, 0 );
  iPayload.Origins01 := TVec4f.Create( iUV[0].X, iUV[0].Y, iUV[1].X, iUV[1].Y );
  iPayload.Origins23 := TVec4f.Create( iUV[2].X, iUV[2].Y, iUV[3].X, iUV[3].Y );
  FTransitions.PushQuad( iTilePos+iPa, iTilePos+iPb, iSourceA, iSourceB, aZ, iLight, iPayload );
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
      glUniformMatrix4fv( FSpriteUniforms.Transform, 1, GL_FALSE, @FProjection[0] );
      FTransitionProgram.Bind;
      glUniformMatrix4fv( FTransitionUniforms.Transform, 1, GL_FALSE, @FProjection[0] );
      FTransitionProgram.UnBind;
      Exit;
    end;
end;

procedure TSpriteEngine.UpdatePosition( aLocation : Integer );
var iX, iY : Single;
begin
  iX := -FPosition.X;
  iY := -FPosition.Y;
  if FFuzzyMode then
  begin
    iX += 0.01;
    iY += 0.01;
  end;
  glUniform3f( aLocation, iX, iY, 0 );
end;

procedure TSpriteEngine.DrawSet( const aData : TSpriteDataSet );
begin
  glActiveTexture( GL_TEXTURE0 );

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glEnablei( GL_BLEND, 0 );
  glDisablei( GL_BLEND, 1 );

  if (not aData.FData.Empty) or ((aData.FTransitions <> nil) and (not aData.FTransitions.Empty)) then
  begin
    glActiveTexture( GL_TEXTURE0 );
    SetTexture( aData.TNormalID );
    glActiveTexture( GL_TEXTURE1 );
    SetTexture( aData.TCosplayID );
    glActiveTexture( GL_TEXTURE2 );
    SetTexture( aData.TEmissiveID );
    glActiveTexture( GL_TEXTURE3 );
    SetTexture( aData.TOutlineID );
    if not aData.FData.Empty then
    begin
      FProgram.Bind;
      aData.FData.Update;
      aData.FData.Draw;
      aData.FData.Clear;
      FProgram.UnBind;
    end;
    if (aData.FTransitions <> nil) and (not aData.FTransitions.Empty) then
    begin
      FTransitionProgram.Bind;
      UpdatePosition( FTransitionUniforms.Position );
      glUniform2f( FTransitionTexUnit, aData.FTexUnit.X, aData.FTexUnit.Y );
      aData.FTransitions.Update;
      aData.FTransitions.Draw;
      aData.FTransitions.Clear;
      FTransitionProgram.UnBind;
    end;
    glActiveTexture( GL_TEXTURE0 );
    glBindTexture( GL_TEXTURE_2D, 0 );
    glActiveTexture( GL_TEXTURE1 );
    glBindTexture( GL_TEXTURE_2D, 0 );
    glActiveTexture( GL_TEXTURE2 );
    glBindTexture( GL_TEXTURE_2D, 0 );
    glActiveTexture( GL_TEXTURE3 );
    glBindTexture( GL_TEXTURE_2D, 0 );
  end;

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
end;

procedure TSpriteEngine.SetTexture( aTextureID : DWord );
begin
  if aTextureID <> 0
    then glBindTexture( GL_TEXTURE_2D, aTextureID )
    else glBindTexture( GL_TEXTURE_2D, FTZeroID );
end;

destructor TSpriteEngine.Destroy;
var iSet : TSpriteDataSet;
begin
  for iSet in FLayers do
    iSet.Free;
  glDeleteVertexArrays(1, @FVAO);
  FreeAndNil( FTransitionProgram );
  FreeAndNil( FProgram );
  FreeAndNil( FLayers );
  FreeAndNil( FLayersSorted );
end;

constructor TSpriteEngine.Create( aTileSize : TVec2i; aScale : Byte = 1 );
var iZeroPixel : array[0..3] of GLubyte = (0, 0, 0, 0);
begin
  FTileSize := aTileSize;
  FFuzzyMode := False;
  SetScale( aScale );
  FPosition.Init(0,0);
  FCurrentTexture    := 0;
  FLayersDirty       := True;

  FProgram := TGLProgram.Create( VSpriteVertexShader, VSpriteFragmentShader );
  FTransitionProgram := TGLProgram.Create( VSpriteTransitionVertexShader, VSpriteTransitionFragmentShader );
  FSpriteUniforms.Transform := FProgram.GetUniformLocation( 'utransform' );
  FSpriteUniforms.Position := FProgram.GetUniformLocation( 'uposition' );
  FTransitionUniforms.Transform := FTransitionProgram.GetUniformLocation( 'utransform' );
  FTransitionUniforms.Position := FTransitionProgram.GetUniformLocation( 'uposition' );
  FTransitionTexUnit := FTransitionProgram.GetUniformLocation( 'utex_unit' );
  FProgram.Bind;
  FProgram.SetUniformi( 'unormal', 0 );
  FProgram.SetUniformi( 'ucosplay', 1 );
  FProgram.SetUniformi( 'uemissive', 2 );
  FProgram.SetUniformi( 'uoutline', 3 );
  FTransitionProgram.Bind;
  FTransitionProgram.SetUniformi( 'unormal', 0 );
  FTransitionProgram.SetUniformi( 'ucosplay', 1 );
  FTransitionProgram.SetUniformi( 'uemissive', 2 );
  FTransitionProgram.SetUniformf( 'utile_size', FTileSize.X, FTileSize.Y );
  FTransitionProgram.UnBind;
  glGenVertexArrays(1, @FVAO);

  FLayers       := TSpriteDataSetArray.Create;
  FLayersSorted := TSpriteDataSetArray.Create;

  // Create dummy texture
  glGenTextures(1, @FTZeroID );
  glBindTexture( GL_TEXTURE_2D, FTZeroID );
    glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, @iZeroPixel );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
  glBindTexture( GL_TEXTURE_2D, 0 );

end;

procedure TSpriteEngine.Reset;
var iSet : TSpriteDataSet;
begin
  FCurrentTexture    := 0;
  FLayersDirty       := True;
  for iSet in FLayers do
    iSet.Free;
  FLayers.Clear;
  FLayersSorted.Clear;
end;

procedure TSpriteEngine.Clear;
var iSet : TSpriteDataSet;
begin
  for iSet in FLayers do
  begin
    iSet.FData.Clear;
    if iSet.FTransitions <> nil then iSet.FTransitions.Clear;
  end;
end;

procedure TSpriteEngine.WarmUpDataSet( aData : TSpriteDataSet );
var iLight       : TGLRawQColor;
    iMaterials   : TSpriteTransitionMaterials;
    i            : Integer;
begin
  for i := 0 to 3 do
  begin
    iLight.Data[i] := TVec3b.CreateAll( 255 );
    iMaterials[i].SpriteID := 1;
    iMaterials[i].Color := ColorBlack;
    // Distinct tints keep this sample on the transition shader.
    iMaterials[i].Color.R := i;
    iMaterials[i].Emissive := ColorWhite;
    iMaterials[i].Shift := TVec2f.Create( 0, 0 );
  end;
  try
    aData.Push( 1, NewCoord2D( 1, 1 ), ColorWhite, ColorBlack, ColorZero, ColorWhite );
    for i := 0 to 3 do
      aData.PushTransition( NewCoord2D( 1, 1 ), i, 15, iMaterials, iLight, 1,
        Min( FTileSize.X, FTileSize.Y ) / 4 );
    DrawSet( aData );
  finally
    aData.FData.Clear;
    aData.FTransitions.Clear;
  end;
end;

procedure TSpriteEngine.WarmUp( const aTargets : array of TGLFramebuffer );
var iProjection      : TMatrix44;
    iPosition        : TVec2i;
    iData            : TSpriteDataSet;
    iTarget          : TGLFramebuffer;
    iHasTransitions  : Boolean;
    iViewport        : array[0..3] of GLint;
    iScissor         : array[0..3] of GLint;
    iDrawTarget      : GLint;
    iReadTarget      : GLint;
    iDepth           : GLboolean;
    iDepthMask       : GLboolean;
    iScissorTest     : GLboolean;
    iProgram         : GLint;
    iVAO             : GLint;
    iBuffer          : GLint;
    iActive          : GLint;
    iTextures        : array[0..3] of GLint;
    iBlend           : array[0..1] of GLboolean;
    iBlendSrcRGB     : GLint;
    iBlendDstRGB     : GLint;
    iBlendSrcA       : GLint;
    iBlendDstA       : GLint;
    i                : Integer;
begin
  if Length( aTargets ) = 0 then Exit;
  iHasTransitions := False;
  for iData in FLayers do
    if iData.SupportsTransitions then
    begin
      if not iData.FData.Empty or not iData.FTransitions.Empty then
        raise Exception.Create( 'Sprite warm-up belongs before frame submission' );
      iHasTransitions := True;
    end;
  if not iHasTransitions then Exit;

  glGetIntegerv( GL_VIEWPORT, @iViewport[0] );
  glGetIntegerv( GL_SCISSOR_BOX, @iScissor[0] );
  glGetIntegerv( GL_DRAW_FRAMEBUFFER_BINDING, @iDrawTarget );
  glGetIntegerv( GL_READ_FRAMEBUFFER_BINDING, @iReadTarget );
  glGetBooleanv( GL_DEPTH_WRITEMASK, @iDepthMask );
  iDepth := glIsEnabled( GL_DEPTH_TEST );
  iScissorTest := glIsEnabled( GL_SCISSOR_TEST );
  glGetIntegerv( GL_CURRENT_PROGRAM, @iProgram );
  glGetIntegerv( GL_VERTEX_ARRAY_BINDING, @iVAO );
  glGetIntegerv( GL_ARRAY_BUFFER_BINDING, @iBuffer );
  glGetIntegerv( GL_ACTIVE_TEXTURE, @iActive );
  glGetIntegerv( GL_BLEND_SRC_RGB, @iBlendSrcRGB );
  glGetIntegerv( GL_BLEND_DST_RGB, @iBlendDstRGB );
  glGetIntegerv( GL_BLEND_SRC_ALPHA, @iBlendSrcA );
  glGetIntegerv( GL_BLEND_DST_ALPHA, @iBlendDstA );
  for i := 0 to 1 do iBlend[i] := glIsEnabledi( GL_BLEND, i );
  for i := 0 to 3 do
  begin
    glActiveTexture( GL_TEXTURE0+i );
    glGetIntegerv( GL_TEXTURE_BINDING_2D, @iTextures[i] );
  end;
  iProjection := FProjection;
  iPosition := FPosition;
  try
    glEnable( GL_DEPTH_TEST );
    glDepthMask( GL_TRUE );
    glEnable( GL_SCISSOR_TEST );
    glScissor( 0, 0, 1, 1 );
    FPosition := Vec2i( 0, 0 );
    Update( GLCreateOrtho( 0, FGrid.X, FGrid.Y, 0, -16384, 16384 ) );
    FProgram.Bind;
    UpdatePosition( FSpriteUniforms.Position );
    for iTarget in aTargets do
    begin
      if iTarget <> nil then iTarget.BindAndClear
      else
      begin
        glBindFramebuffer( GL_FRAMEBUFFER, 0 );
        glClear( GL_DEPTH_BUFFER_BIT );
      end;
      glViewport( 0, 0, 1, 1 );
      for iData in FLayers do
        if iData.SupportsTransitions then WarmUpDataSet( iData );
    end;
    // Drivers can defer pipeline work until the first draw, even after linking.
    glFinish;
  finally
    FPosition := iPosition;
    Update( iProjection );
    FProgram.Bind;
    UpdatePosition( FSpriteUniforms.Position );
    FTransitionProgram.Bind;
    UpdatePosition( FTransitionUniforms.Position );
    for i := 0 to 1 do
      if iBlend[i] <> GL_FALSE then glEnablei( GL_BLEND, i ) else glDisablei( GL_BLEND, i );
    glBlendFuncSeparate( iBlendSrcRGB, iBlendDstRGB, iBlendSrcA, iBlendDstA );
    for i := 0 to 3 do
    begin
      glActiveTexture( GL_TEXTURE0+i );
      glBindTexture( GL_TEXTURE_2D, iTextures[i] );
    end;
    glActiveTexture( iActive );
    glBindVertexArray( iVAO );
    glBindBuffer( GL_ARRAY_BUFFER, iBuffer );
    glUseProgram( iProgram );
    glBindFramebuffer( GL_DRAW_FRAMEBUFFER, iDrawTarget );
    glBindFramebuffer( GL_READ_FRAMEBUFFER, iReadTarget );
    glViewport( iViewport[0], iViewport[1], iViewport[2], iViewport[3] );
    glScissor( iScissor[0], iScissor[1], iScissor[2], iScissor[3] );
    if iScissorTest <> GL_FALSE then glEnable( GL_SCISSOR_TEST ) else glDisable( GL_SCISSOR_TEST );
    if iDepth <> GL_FALSE then glEnable( GL_DEPTH_TEST ) else glDisable( GL_DEPTH_TEST );
    glDepthMask( iDepthMask );
  end;
end;

procedure TSpriteEngine.SetScale( aScale : Byte );
begin
  FScale := aScale;
  FGrid.Init( FTileSize.X * aScale, FTileSize.Y * aScale );
end;

procedure TSpriteEngine.SetScale( aScale : Single );
begin
  FFuzzyMode := Abs( aScale - Integer( aScale ) ) > 0.1;
  FScale := aScale;
  FGrid.Init( Round( FTileSize.X * aScale ), Round( FTileSize.Y * aScale ) );
end;

function TSpriteEngine.Add( aNormal, aCosplay, aEmissive, aOutline : TTexture; aOrder : Integer; aTransitions : Boolean ) : Integer;
var i : DWord;
begin
  Assert( aNormal <> nil, 'Normal texture needs to be present in spritesheet!');
  if FLayers.Size > 0 then
  for i := 0 to FLayers.Size - 1 do
    if FLayers[i].TNormalID = aNormal.GLTexture then
    begin
      if aTransitions then FLayers[i].EnableTransitions;
      Exit( i );
    end;
  FLayersDirty := True;
  FLayers.Push( TSpriteDataSet.Create( Self, aNormal, aCosplay, aEmissive, aOutline, aOrder, aTransitions ) );
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
  UpdatePosition( FSpriteUniforms.Position );
  for iSet in FLayersSorted do
    DrawSet( iSet );

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
end;

{ TGLTexturedColored4Quads }

constructor TGLTexturedColored4Quads.Create;
begin
  inherited Create;
  PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, VGL_COLOR_LOCATION );
  PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, VGL_COLOR2_LOCATION );
  PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, VGL_COLOR3_LOCATION );
  PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, VGL_COLOR4_LOCATION );
end;

procedure TGLTexturedColored4Quads.PushQuad ( aUR, aLL : TGLVec3i; aColor, aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
begin
  inherited PushQuad(aUR, aLL, aTUR, aTLL );
  TGLQVec4fArray(FArrays[2]).Push( TGLQVec4f.CreateAll( aColor ) );
  TGLQVec4fArray(FArrays[3]).Push( TGLQVec4f.CreateAll( aCosColor ) );
  TGLQVec4fArray(FArrays[4]).Push( TGLQVec4f.CreateAll( aGlowColor ) );
  TGLQVec4fArray(FArrays[5]).Push( TGLQVec4f.CreateAll( aEmissive ) );
end;

procedure TGLTexturedColored4Quads.PushQuad(aUR, aLL: TGLVec3i; aColorQuad: TGLQVec4f; aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL: TGLVec2f);
begin
  inherited PushQuad(aUR, aLL, aTUR, aTLL );
  TGLQVec4fArray(FArrays[2]).Push( aColorQuad );
  TGLQVec4fArray(FArrays[3]).Push( TGLQVec4f.CreateAll( aCosColor ) );
  TGLQVec4fArray(FArrays[4]).Push( TGLQVec4f.CreateAll( aGlowColor ) );
  TGLQVec4fArray(FArrays[5]).Push( TGLQVec4f.CreateAll( aEmissive ) );
end;

procedure TGLTexturedColored4Quads.PushQuad(aCoord: TGLQVec3i; aColorQuad: TGLQVec4f; aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL: TGLVec2f);
begin
  inherited PushQuad(aCoord, aTUR, aTLL );
  TGLQVec4fArray(FArrays[2]).Push( aColorQuad );
  TGLQVec4fArray(FArrays[3]).Push( TGLQVec4f.CreateAll( aCosColor ) );
  TGLQVec4fArray(FArrays[4]).Push( TGLQVec4f.CreateAll( aGlowColor ) );
  TGLQVec4fArray(FArrays[5]).Push( TGLQVec4f.CreateAll( aEmissive ) );
end;

procedure TGLTexturedColored4Quads.PushRotatedQuad(aCenter, aSize: TGLVec3i;
  aDegrees: Single; aColor, aCosColor, aGlowColor, aEmissive : TGLVec4f; aTUR, aTLL: TGLVec2f);
begin
  inherited PushRotatedQuad( aCenter, aSize, aDegrees, aTUR, aTLL );
  TGLQVec4fArray(FArrays[2]).Push( TGLQVec4f.CreateAll( aColor ) );
  TGLQVec4fArray(FArrays[3]).Push( TGLQVec4f.CreateAll( aCosColor ) );
  TGLQVec4fArray(FArrays[4]).Push( TGLQVec4f.CreateAll( aGlowColor ) );
  TGLQVec4fArray(FArrays[5]).Push( TGLQVec4f.CreateAll( aEmissive ) );
end;

procedure TGLTexturedColored4Quads.Append( aList : TGLTexturedColored4Quads );
begin
  inherited Append( aList );
  TGLQVec4fArray(FArrays[2]).Append( TGLQVec4fArray(aList.FArrays[2]) );
  TGLQVec4fArray(FArrays[3]).Append( TGLQVec4fArray(aList.FArrays[3]) );
  TGLQVec4fArray(FArrays[4]).Append( TGLQVec4fArray(aList.FArrays[4]) );
  TGLQVec4fArray(FArrays[5]).Append( TGLQVec4fArray(aList.FArrays[5]) );
end;

{ TGLSpriteTransitionQuads }

constructor TGLSpriteTransitionQuads.Create;
var i : Integer;
begin
  inherited Create;
  // The base class supplies position/source coordinates at locations 0/1.
  for i := VGL_TRANSITION_LIGHT to VGL_TRANSITION_EMISSIONS+3 do
    PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, i );
end;

procedure TGLSpriteTransitionQuads.PushQuad( aPa, aPb : TVec2i; aSourceA, aSourceB : TVec2f; aZ : Integer;
  const aLight : TGLQVec4f; const aPayload : TSpriteTransitionPayload );
var i : Integer;
begin
  inherited PushQuad( TVec3i.CreateFrom( aPa, aZ ), TVec3i.CreateFrom( aPb, aZ ), aSourceA, aSourceB );
  TGLQVec4fArray( FArrays[VGL_TRANSITION_LIGHT] ).Push( aLight );
  TGLQVec4fArray( FArrays[VGL_TRANSITION_PIVOT_WIDTH] ).Push( TGLQVec4f.CreateAll( aPayload.PivotWidth ) );
  TGLQVec4fArray( FArrays[VGL_TRANSITION_ORIGINS01] ).Push( TGLQVec4f.CreateAll( aPayload.Origins01 ) );
  TGLQVec4fArray( FArrays[VGL_TRANSITION_ORIGINS23] ).Push( TGLQVec4f.CreateAll( aPayload.Origins23 ) );
  TGLQVec4fArray( FArrays[VGL_TRANSITION_MASKS] ).Push( TGLQVec4f.CreateAll( aPayload.CornerMasks ) );
  for i := 0 to 3 do
  begin
    TGLQVec4fArray( FArrays[VGL_TRANSITION_TINTS+i] ).Push( TGLQVec4f.CreateAll( aPayload.Tints[i] ) );
    TGLQVec4fArray( FArrays[VGL_TRANSITION_EMISSIONS+i] ).Push( TGLQVec4f.CreateAll( aPayload.Emissions[i] ) );
  end;
end;

initialization

  Assert( SizeOf( Integer ) = SizeOf( GLInt ) );
  Assert( SizeOf( Single )  = SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQCoord )    = 8 * SizeOf( GLInt ) );
  Assert( SizeOf( TGLRawQTexCoord ) = 8 * SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQColor )    = 12 * SizeOf( GLByte ) );

end.

