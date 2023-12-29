{$INCLUDE valkyrie.inc}
unit vglquadsheet;
interface
uses Classes, SysUtils, vnode, vgltypes, vgenerics, vglprogram;

// TODO : write Append for all using System.Move
type TGLQuadList = class( TVObject )
  constructor Create;
  procedure Clear;
  procedure PostQuad( aUR, aLL : TGLVec2i );
  procedure PostLine( aFrom, aTo : TGLVec2i );
  procedure Draw; virtual; abstract;
  procedure IncSize; virtual;
protected
  FCount    : DWord;
  FCoords   : packed array of TGLRawQCoord;
public
  property Count : DWord read FCount;
end;

type TGLColoredQuadList = class( TGLQuadList )
  procedure PostLine( aFrom, aTo : TGLVec2i; aColor : TGLVec4f ); reintroduce;
  procedure PostQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f ); reintroduce;
  procedure PostQuad( aUR, aLL : TGLVec2i; aColor : TGLRawQColor4f ); reintroduce;
  procedure Draw; virtual;
  procedure IncSize; override;
private
  FColors    : packed array of TGLRawQColor4f;
end;

type TGLTexturedQuadList = class( TGLColoredQuadList )
  procedure PostQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ); reintroduce;
  procedure PostQuad( aUR, aLL : TGLVec2i; aColor : TGLRawQColor4f; aTUR, aTLL : TGLVec2f ); reintroduce;
  procedure PostQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTexCoord : TGLRawQTexCoord ) ;
  procedure Append( aList : TGLTexturedQuadList );
  procedure Draw; virtual;
  procedure IncSize; override;
private
  FTexCoords : packed array of TGLRawQTexCoord;
end;

type
  TGLTextureIDs        = specialize TGArray< DWord >;
  TGLTexturedQuadLists = specialize TGObjectArray< TGLTexturedQuadList >;

type TGLQuadSheet = class( TVObject )
  constructor Create;
  procedure PostColoredLine( aFrom, aTo : TGLVec2i; aColor : TGLVec4f );
  procedure PostColoredQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f );
  procedure PostTexturedQuad( aUR, aLL : TGLVec2i; aTUR, aTLL : TGLVec2f; aTexture : DWord );
  procedure PostTexturedQuad( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f; aTexture : DWord );
  procedure PostTexturedQuad( aUR, aLL : TGLVec2i; aColor : TGLRawQColor4f; aTUR, aTLL : TGLVec2f; aTexture : DWord );
  procedure Append( aList : TGLTexturedQuadList; aTexture : DWord );
  procedure Draw;
  destructor Destroy; override;
private
  function GetIndex( aTexture : DWord ) : Integer;
private
  FColored   : TGLColoredQuadList;
  FTextured  : TGLTexturedQuadLists;
  FTextures  : TGLTextureIDs;
end;

type TGLQuadSheetRenderer = class( TVObject )
  constructor Create;
  procedure Update( aProjection : TMatrix44 );
  procedure Render( aSheet : TGLQuadSheet );
  destructor Destroy; override;
private
  FProjection  : TMatrix44;
  FCProgram    : TGLProgram;
  FTProgram    : TGLProgram;
  FVAO         : Cardinal;
  FVBOCoord    : Cardinal;
  FVBOColor    : Cardinal;
  FVBOTexCoord : Cardinal;
end;

implementation

uses math, vgl3library;

{ TGLQuadList }

constructor TGLQuadList.Create;
begin
  FCount := 0;
end;

procedure TGLQuadList.Clear;
begin
  FCount := 0;
end;

procedure TGLQuadList.PostQuad ( aUR, aLL : TGLVec2i ) ;
begin
  if FCount > High( FCoords ) then IncSize;
  FCoords[ FCount ].Init( aUR, aLL );
  Inc( FCount );
end;

procedure TGLQuadList.PostLine ( aFrom, aTo : TGLVec2i ) ;
var aShift : TGLVec2i;
begin
  if FCount > High( FCoords ) then IncSize;
  FCoords[ FCount ].Data[0] := aFrom;
  FCoords[ FCount ].Data[1] := aTo;
  aShift.Init( -Sign( aFrom.Y - aTo.Y), Sign( aFrom.X - aTo.X ) );
  FCoords[ FCount ].Data[2] := aTo + aShift;
  FCoords[ FCount ].Data[3] := aFrom + aShift;
  Inc( FCount );
end;

procedure TGLQuadList.IncSize;
begin
  SetLength( FCoords,  Max( 16, 2*FCount ) );
end;

{ TGLColoredQuadList }

procedure TGLColoredQuadList.PostLine ( aFrom, aTo : TGLVec2i; aColor : TGLVec4f ) ;
begin
  inherited PostLine( aFrom, aTo );
  FColors[ FCount-1 ].SetAll( aColor );
end;

procedure TGLColoredQuadList.PostQuad ( aUR, aLL : TGLVec2i; aColor : TGLVec4f ) ;
begin
  inherited PostQuad( aUR, aLL );
  FColors[ FCount-1 ].SetAll( aColor );
end;

procedure TGLColoredQuadList.PostQuad ( aUR, aLL : TGLVec2i; aColor : TGLRawQColor4f ) ;
begin
  inherited PostQuad( aUR, aLL );
  FColors[ FCount-1 ] := aColor;
end;

procedure TGLColoredQuadList.Draw;
begin
  glVertexPointer( 2, GL_INT, 0, @(FCoords[0]) );
  glColorPointer( 4, GL_FLOAT, 0, @(FColors[0]) );
  glDrawArrays( GL_QUADS, 0, FCount*4 );
  FCount := 0;
end;

procedure TGLColoredQuadList.IncSize;
begin
  SetLength( FColors,  Max( 16, 2*FCount ) );
  inherited IncSize;
end;

{ TGLTexturedQuadList }

procedure TGLTexturedQuadList.PostQuad ( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
begin
  inherited PostQuad( aUR, aLL, aColor );
  FTexCoords[ FCount-1 ].Init( aTUR, aTLL );
end;

procedure TGLTexturedQuadList.PostQuad ( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTexCoord : TGLRawQTexCoord ) ;
begin
  inherited PostQuad( aUR, aLL, aColor );
  FTexCoords[ FCount-1 ] := aTexCoord;
end;

procedure TGLTexturedQuadList.PostQuad ( aUR, aLL : TGLVec2i;
  aColor : TGLRawQColor4f; aTUR, aTLL : TGLVec2f ) ;
begin
  inherited PostQuad( aUR, aLL, aColor );
  FTexCoords[ FCount-1 ].Init( aTUR, aTLL );
end;

procedure TGLTexturedQuadList.Append ( aList : TGLTexturedQuadList ) ;
var iCount : DWord;
begin
  if aList.FCount = 0 then Exit;
  for iCount := 0 to aList.FCount-1 do
  begin
    if FCount > High( FCoords ) then IncSize;
    FCoords[ FCount ]    := aList.FCoords[iCount];
    FColors[ FCount ]    := aList.FColors[iCount];
    FTexCoords[ FCount ] := aList.FTexCoords[iCount];
    Inc( FCount );
  end;
end;

procedure TGLTexturedQuadList.Draw;
begin
  glVertexPointer( 2, GL_INT, 0, @(FCoords[0]) );
  glColorPointer( 4, GL_FLOAT, 0, @(FColors[0]) );
  glTexCoordPointer( 2, GL_FLOAT, 0, @(FTexCoords[0]) );
  glDrawArrays( GL_QUADS, 0, FCount*4 );
  FCount := 0;
end;

procedure TGLTexturedQuadList.IncSize;
begin
  SetLength( FTexCoords,  Max( 16, 2*FCount ) );
  inherited IncSize;
end;


{ TGLQuadSheet }

constructor TGLQuadSheet.Create;
begin
  FColored   := nil;
  FTextured  := nil;
  FTextures  := nil;
end;

procedure TGLQuadSheet.PostColoredLine ( aFrom, aTo : TGLVec2i;
  aColor : TGLVec4f ) ;
begin
  if FColored = nil then FColored := TGLColoredQuadList.Create;
  FColored.PostLine( aFrom, aTo, aColor );
end;

procedure TGLQuadSheet.PostColoredQuad ( aUR, aLL : TGLVec2i; aColor : TGLVec4f ) ;
begin
  if FColored = nil then FColored := TGLColoredQuadList.Create;
  FColored.PostQuad( aUR, aLL, aColor );
end;

procedure TGLQuadSheet.PostTexturedQuad ( aUR, aLL : TGLVec2i; aTUR,
  aTLL : TGLVec2f; aTexture : DWord ) ;
var iIndex : Integer;
begin
  iIndex := GetIndex( aTexture );
  FTextured[iIndex].PostQuad( aUR, aLL, TGLVec4f.Create(1,1,1,1), aTUR, aTLL );
end;

procedure TGLQuadSheet.PostTexturedQuad ( aUR, aLL : TGLVec2i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f; aTexture : DWord ) ;
var iIndex : Integer;
begin
  iIndex := GetIndex( aTexture );
  FTextured[iIndex].PostQuad( aUR, aLL, aColor, aTUR, aTLL );
end;

procedure TGLQuadSheet.PostTexturedQuad ( aUR, aLL : TGLVec2i; aColor : TGLRawQColor4f; aTUR, aTLL : TGLVec2f; aTexture : DWord ) ;
var iIndex : Integer;
begin
  iIndex := GetIndex( aTexture );
  FTextured[iIndex].PostQuad( aUR, aLL, aColor, aTUR, aTLL );
end;

procedure TGLQuadSheet.Append ( aList : TGLTexturedQuadList; aTexture : DWord ) ;
var iIndex : Integer;
begin
  iIndex := GetIndex( aTexture );
  FTextured[iIndex].Append( aList );
end;

procedure TGLQuadSheet.Draw;
var iCount : DWord;
begin
  if (FTextured <> nil) and (FTextured.Size > 0) then
  begin
    Log( '%d', [FTextured.Size] );
    glColor4f( 1.0, 1.0, 1.0, 1.0 );
    glEnable( GL_TEXTURE_2D );

    glEnableClientState( GL_VERTEX_ARRAY );
    glEnableClientState( GL_COLOR_ARRAY );
    glEnableClientState( GL_TEXTURE_COORD_ARRAY );

    for iCount := 0 to FTextured.Size-1 do
    if FTextured[iCount].Count > 0 then
    begin
      glBindTexture( GL_TEXTURE_2D, FTextures[ iCount ] );
      FTextured[ iCount ].Draw;
    end;

    glDisableClientState( GL_VERTEX_ARRAY );
    glDisableClientState( GL_COLOR_ARRAY );
    glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  end;

  if (FColored <> nil) and (FColored.Count > 0) then
  begin
    glColor4f( 1.0, 1.0, 1.0, 1.0 );
    glEnable( GL_BLEND );
    glDisable( GL_TEXTURE_2D );
    glEnableClientState( GL_VERTEX_ARRAY );
    glEnableClientState( GL_COLOR_ARRAY );
    FColored.Draw;
    glDisableClientState( GL_VERTEX_ARRAY );
    glDisableClientState( GL_COLOR_ARRAY );
  end;
end;

destructor TGLQuadSheet.Destroy;
begin
  FreeAndNil( FColored );
  FreeAndNil( FTextured );
  FreeAndNil( FTextures );
  inherited Destroy;
end;

function TGLQuadSheet.GetIndex ( aTexture : DWord ) : Integer;
begin
  if FTextured = nil then
  begin
    FTextured := TGLTexturedQuadLists.Create;
    FTextures := TGLTextureIDs.Create;
    GetIndex  := -1;
  end
  else
    GetIndex := FTextures.IndexOf( aTexture );

  if GetIndex = -1 then
  begin
    FTextures.Push( aTexture );
    FTextured.Push( TGLTexturedQuadList.Create );
    GetIndex := FTextured.Size - 1;
  end;
end;

{ TGLQuadSheetRenderer }

const
GLQuadSheetColorVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec2 position;'+#10+
'layout (location = 1) in vec4 color;'+#10+
'uniform mat4 projection;'+#10+
#10+
'out vec4 ocolor;'+#10+
#10+
'void main() {'+#10+
'ocolor = color;'+#10+
'gl_Position = projection * vec4(position, 0.0, 1.0);'+#10+
'}'+#10;
GLQuadSheetColorFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec4 ocolor;'+#10+
'out vec4 frag_color;'+#10+
#10+
'void main() {'+#10+
'frag_color = ocolor;'+#10+
'}'+#10;
GLQuadSheetTextureVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec2 position;'+#10+
'layout (location = 1) in vec4 color;'+#10+
'layout (location = 2) in vec2 texcoord;'+#10+
'uniform mat4 projection;'+#10+
#10+
'out vec4 ocolor;'+#10+
'out vec2 otexcoord;'+#10+
#10+
'void main() {'+#10+
'otexcoord = texcoord;'+#10+
'ocolor = color;'+#10+
'gl_Position = projection * vec4(position, 0.0, 1.0);'+#10+
'}'+#10;
GLQuadSheetTextureFragmentShader : Ansistring =
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


constructor TGLQuadSheetRenderer.Create;
begin
  inherited Create;
  FillChar( FProjection, SizeOf( FProjection ), 0 );
  FCProgram   := TGLProgram.Create( GLQuadSheetColorVertexShader, GLQuadSheetColorFragmentShader );
  FTProgram   := TGLProgram.Create( GLQuadSheetTextureVertexShader, GLQuadSheetTextureFragmentShader );
  glGenVertexArrays(1, @FVAO);
  glGenBuffers(1, @FVBOCoord);
  glGenBuffers(1, @FVBOColor);
  glGenBuffers(1, @FVBOTexCoord);
end;

procedure TGLQuadSheetRenderer.Update ( aProjection : TMatrix44 );
var iLocation, i : Integer;
begin
  for i := 0 to 15 do
    if FProjection[i] <> aProjection[i] then
    begin
      FProjection := aProjection;
      FCProgram.Bind;
      glUniformMatrix4fv( FCProgram.GetUniformLocation( 'projection' ), 1, GL_FALSE, @FProjection[0] );
      FCProgram.UnBind;
      FTProgram.Bind;
      glUniformMatrix4fv( FTProgram.GetUniformLocation( 'projection' ), 1, GL_FALSE, @FProjection[0] );
      glUniform1i( FTProgram.GetUniformLocation('utexture'), 0 );
      FTProgram.UnBind;
      Exit;
    end;
end;

procedure TGLQuadSheetRenderer.Render ( aSheet : TGLQuadSheet );
var iCount : DWord;
begin
  glBindVertexArray(FVAO);

  if (aSheet.FTextured <> nil) and (aSheet.FTextured.Size > 0) then
  begin
    FTProgram.Bind;
    glActiveTexture(0);
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);    
    glEnableVertexAttribArray(2);
    for iCount := 0 to aSheet.FTextured.Size-1 do
    if aSheet.FTextured[iCount].Count > 0 then
    begin
      glBindTexture( GL_TEXTURE_2D, aSheet.FTextures[ iCount ] );
      glBindBuffer( GL_ARRAY_BUFFER, FVBOCoord );
      glBufferData( GL_ARRAY_BUFFER, aSheet.FTextured[ iCount ].Count*sizeof(TGLRawQCoord), @(aSheet.FTextured[iCount].FCoords[0]), GL_STREAM_DRAW );
      glVertexAttribPointer(0, 2, GL_INT, GL_FALSE, 2 * sizeof(Integer), nil );

      glBindBuffer( GL_ARRAY_BUFFER, FVBOColor );
      glBufferData( GL_ARRAY_BUFFER, aSheet.FTextured[ iCount ].Count*sizeof(TGLRawQColor4f), @(aSheet.FTextured[iCount].FColors[0]), GL_STREAM_DRAW );
      glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 4 * sizeof(Single), nil );
      glEnableVertexAttribArray(1);

      glBindBuffer( GL_ARRAY_BUFFER, FVBOTexCoord );
      glBufferData( GL_ARRAY_BUFFER, aSheet.FTextured[iCount].Count*sizeof(TGLRawQTexCoord), @(aSheet.FTextured[iCount].FTexCoords[0]), GL_STREAM_DRAW );
      glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nil );

      glDrawArrays( GL_QUADS, 0, aSheet.FTextured[iCount].Count*4 );

      aSheet.FTextured[iCount].FCount := 0;
    end;
    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(2);
    FTProgram.UnBind;
  end;

  if (aSheet.FColored <> nil) and (aSheet.FColored.Count > 0) then
  begin
    FCProgram.Bind;

    glBindBuffer( GL_ARRAY_BUFFER, FVBOCoord );
    glBufferData( GL_ARRAY_BUFFER, aSheet.FColored.Count*sizeof(TGLRawQCoord), @(aSheet.FColored.FCoords[0]), GL_STREAM_DRAW );
    glVertexAttribPointer(0, 2, GL_INT, GL_FALSE, 2 * sizeof(Integer), nil );
    glEnableVertexAttribArray(0);

    glBindBuffer( GL_ARRAY_BUFFER, FVBOColor );
    glBufferData( GL_ARRAY_BUFFER, aSheet.FColored.Count*sizeof(TGLRawQColor4f), @(aSheet.FColored.FColors[0]), GL_STREAM_DRAW );
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 4 * sizeof(Single), nil );
    glEnableVertexAttribArray(1);

    glDrawArrays( GL_QUADS, 0, aSheet.FColored.Count*4 );

    FCProgram.UnBind;
    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);

    aSheet.FColored.FCount := 0;
  end;
  glBindBuffer( GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
end;

destructor TGLQuadSheetRenderer.Destroy;
begin
  inherited Destroy;
  glDeleteBuffers(1, @FVBOCoord);
  glDeleteBuffers(1, @FVBOColor);
  glDeleteBuffers(1, @FVBOTexCoord);
  glDeleteVertexArrays(1, @FVAO);
  FreeAndNil( FCProgram );
  FreeAndNil( FTProgram );
end;

end.
