{$INCLUDE valkyrie.inc}
unit vglquadrenderer;
interface

uses Classes, SysUtils, vnode, vgltypes, vglprogram, vgldrawarrays,
     vglquadarrays;

type TGLTexturedColoredQuadLayer = class( TGLTexturedArrays )
  function GetArray( aTextureID : Cardinal ) : TGLTexturedColoredQuads;
public
  property Quads[ aTextureID : Cardinal ]  : TGLTexturedColoredQuads read GetArray; default;
end;


type TGLQuadList = class
  procedure PushColoredQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aZ : Integer = 0 );
  procedure PushTexturedQuad( aUR, aLL : TGLVec2i; aTUR, aTLL : TGLVec2f; aTexture : DWord; aZ : Integer = 0 );
  procedure PushTexturedQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f; aTexture : DWord; aZ : Integer = 0 );
  procedure Append( aList : TGLTexturedColoredQuads; aTexture : DWord );
  destructor Destroy; override;
private
  FColored   : TGLColoredQuads;
  FTextured  : TGLTexturedColoredQuadLayer;
end;

type TGLQuadRenderer = class( TVObject )
  constructor Create;
  procedure Update( aProjection : TMatrix44 );
  procedure Render( aList : TGLQuadList );
  destructor Destroy; override;
private
  FProjection  : TMatrix44;
  FCProgram    : TGLProgram;
  FTProgram    : TGLProgram;
end;

implementation

uses vgl3library;

const
GLQuadColorVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec3 position;'+#10+
'layout (location = 2) in vec4 color;'+#10+
'uniform mat4 utransform;'+#10+
#10+
'out vec4 ocolor;'+#10+
#10+
'void main() {'+#10+
'ocolor = color;'+#10+
'gl_Position = utransform * vec4(position, 1.0);'+#10+
'}'+#10;
GLQuadColorFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec4 ocolor;'+#10+
'out vec4 frag_color;'+#10+
#10+
'void main() {'+#10+
'frag_color = ocolor;'+#10+
'}'+#10;
GLQuadTextureVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec3 position;'+#10+
'layout (location = 1) in vec2 texcoord;'+#10+
'layout (location = 2) in vec4 color;'+#10+
'uniform mat4 utransform;'+#10+
#10+
'out vec4 ocolor;'+#10+
'out vec2 otexcoord;'+#10+
#10+
'void main() {'+#10+
'otexcoord = texcoord;'+#10+
'ocolor = color;'+#10+
'gl_Position = utransform * vec4(position, 1.0);'+#10+
'}'+#10;
GLQuadTextureFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec4 ocolor;'+#10+
'in vec2 otexcoord;'+#10+
'out vec4 frag_color;'+#10+
#10+
'uniform sampler2D utexture;'+#10+
#10+
'void main() {'+#10+
'frag_color = ocolor * texture( utexture, otexcoord );'+#10+
'}'+#10;

{ TGLTexturedColoredQuadLayer }

function TGLTexturedColoredQuadLayer.GetArray(aTextureID: Cardinal ): TGLTexturedColoredQuads;
begin
  Result := TGLTexturedColoredQuads( GetDrawArray( aTextureID ) );
  if Result = nil then
  begin
    Result := TGLTexturedColoredQuads.Create;
    FDrawArrays.Push( Result );
    FTextureIDs.Push( aTextureID );
  end;
end;

procedure TGLQuadList.PushColoredQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aZ : Integer = 0 );
begin
  if not Assigned( FColored ) then FColored := TGLColoredQuads.Create;
  FColored.PushQuad( TGLVec3i.CreateFrom( aUR, aZ ), TGLVec3i.CreateFrom( aLL, aZ ), aColor );
end;

procedure TGLQuadList.PushTexturedQuad( aUR, aLL : TGLVec2i; aTUR, aTLL : TGLVec2f; aTexture : DWord; aZ : Integer = 0 );
begin
  if not Assigned( FTextured ) then FTextured := TGLTexturedColoredQuadLayer.Create;
  FTextured[ aTexture ].PushQuad( TGLVec3i.CreateFrom( aUR, aZ ), TGLVec3i.CreateFrom( aLL, aZ ), TGLVec4f.Create(1,1,1,1), aTUR, aTLL );
end;

procedure TGLQuadList.PushTexturedQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f; aTexture : DWord; aZ : Integer = 0 );
begin
  if not Assigned( FTextured ) then FTextured := TGLTexturedColoredQuadLayer.Create;
  FTextured[ aTexture ].PushQuad( TGLVec3i.CreateFrom( aUR, aZ ), TGLVec3i.CreateFrom( aLL, aZ ), aColor, aTUR, aTLL );
end;

procedure TGLQuadList.Append ( aList : TGLTexturedColoredQuads; aTexture : DWord ) ;
begin
  if not Assigned( FTextured ) then FTextured := TGLTexturedColoredQuadLayer.Create;
  FTextured[aTexture].Append( aList );
end;

destructor TGLQuadList.Destroy;
begin
  FreeAndNil( FColored );
  FreeAndNil( FTextured );
  inherited Destroy;
end;

constructor TGLQuadRenderer.Create;
begin
  inherited Create;
  FillChar( FProjection, SizeOf( FProjection ), 0 );
  FCProgram   := TGLProgram.Create( GLQuadColorVertexShader,   GLQuadColorFragmentShader );
  FTProgram   := TGLProgram.Create( GLQuadTextureVertexShader, GLQuadTextureFragmentShader );
end;

procedure TGLQuadRenderer.Update ( aProjection : TMatrix44 );
var i : Integer;
begin
  for i := 0 to 15 do
    if FProjection[i] <> aProjection[i] then
    begin
      FProjection := aProjection;
      FCProgram.Bind;
      glUniformMatrix4fv( FCProgram.GetUniformLocation( 'utransform' ), 1, GL_FALSE, @FProjection[0] );
      FCProgram.UnBind;
      FTProgram.Bind;
      glUniformMatrix4fv( FTProgram.GetUniformLocation( 'utransform' ), 1, GL_FALSE, @FProjection[0] );
      glUniform1i( FTProgram.GetUniformLocation('utexture'), 0 );
      FTProgram.UnBind;
      Exit;
    end;
end;

procedure TGLQuadRenderer.Render ( aList : TGLQuadList );
begin
  if (aList.FTextured <> nil) and (not aList.FTextured.Empty) then
  begin
    aList.FTextured.Update;
    FTProgram.Bind;
    glActiveTexture(GL_TEXTURE0);
    aList.FTextured.Draw;
    FTProgram.UnBind;
    aList.FTextured.Clear;
  end;

  if (aList.FColored <> nil) and (not aList.FColored.Empty) then
  begin
    aList.FColored.Update;
    FCProgram.Bind;
    aList.FColored.Draw;
    FTProgram.UnBind;
    aList.FColored.Clear;
  end;
end;

destructor TGLQuadRenderer.Destroy;
begin
  inherited Destroy;
  FreeAndNil( FCProgram );
  FreeAndNil( FTProgram );
end;


end.

