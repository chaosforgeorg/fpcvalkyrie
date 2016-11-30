{$INCLUDE valkyrie.inc}
unit vglquadsheet;
interface
uses Classes, SysUtils, vnode, vgltypes, vgenerics;

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

implementation

uses math, vgllibrary;


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

end.

