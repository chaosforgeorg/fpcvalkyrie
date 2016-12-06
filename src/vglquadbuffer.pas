{$INCLUDE valkyrie.inc}
unit vglquadbuffer;
interface

uses Classes, SysUtils, vgltypes, vgenerics, vvector, vglprogram;

type

TGLQVec3i = specialize TGVectorTriQuad<TGLVec3i,Integer>;
TGLQVec2f = specialize TGVectorTriQuad<TGLVec2f,Single>;
TGLQVec4f = specialize TGVectorTriQuad<TGLVec4f,Single>;

TGLQVec3iArray = specialize TGArray< TGLQVec3i >;
TGLQVec2fArray = specialize TGArray< TGLQVec2f >;
TGLQVec4fArray = specialize TGArray< TGLQVec4f >;

type

TRawArrayArray = specialize TGObjectArray< TRawPointerArray >;

type TGLArrayData = record
    VBO      : Integer;
    Elements : Cardinal;
    GLType   : Cardinal;
    AttrLoc  : Integer;
  end;

TGLArrayDataArray      = specialize TGArray< TGLArrayData >;

type

{ TGLDrawArrays }

TGLDrawArrays = class
  constructor Create;
  procedure Clear;
  procedure Update;
  procedure Draw;
  destructor Destroy; override;
protected
  procedure PushArray( aArray : TRawPointerArray; aElements : Cardinal; aGLType   : Cardinal; aLocation : Integer );
protected
  FArrays : TRawArrayArray;
  FData   : TGLArrayDataArray;
end;

TGLDrawArraysArray = specialize TGObjectArray< TGLDrawArrays >;

type

{ TGLTexturedColoredQuads }

TGLTexturedColoredQuads = class( TGLDrawArrays )
  constructor Create( aAttribCoords, aAttribTexCoord, aAttribColor : Integer );
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure PushQuad ( aUR, aLL : TGLVec3i; aColorQuad : TGLQVec4f; aTUR, aTLL : TGLVec2f ) ;
  procedure PushRotatedQuad ( aCenter, aSize : TGLVec3i; aDegrees : Single; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
  destructor Destroy; override;
end;

TCardinalArray     = specialize TGArray< Cardinal >;

type

{ TGLQuadBufferLayer }

TGLQuadBufferLayer = class
  constructor Create;
  function AddDrawArray( aDrawArray : TGLDrawArrays; aKey : Cardinal ) : TGLDrawArrays;
  function GetDrawArray( aKey : Cardinal ) : TGLDrawArrays;
  destructor Destroy; override;
protected
  FDrawArrays : TGLDrawArraysArray;
  FKeys       : TCardinalArray;
end;

{ TGLTexturedColoredQuadLayer }

TGLTexturedColoredQuadLayer = class( TGLQuadBufferLayer )
  constructor Create( aProgram : TGLProgram; const aTextureName, aCoordName, aTexCoordName, aColorName : AnsiString );
  function GetArray( aTextureID : Cardinal ) : TGLTexturedColoredQuads;
  procedure Draw;
  destructor Destroy; override;
private
  FProgram    : TGLProgram;
  FLTexture   : Integer;
  FLCoords    : Integer;
  FLTexCoords : Integer;
  FLColors    : Integer;
public
  property Quads[ aTextureID : Cardinal ]  : TGLTexturedColoredQuads read GetArray; default;
end;

implementation

uses vgl2library;

{ TGLTexturedColoredQuadLayer }

constructor TGLTexturedColoredQuadLayer.Create( aProgram: TGLProgram; const aTextureName, aCoordName, aTexCoordName, aColorName: AnsiString );
begin
  inherited Create;
  FProgram    := aProgram;
  FLTexture   := FProgram.GetUniformLocation( aTextureName );
  FLCoords    := FProgram.GetAttribLocation( aCoordName );
  FLTexCoords := FProgram.GetAttribLocation( aTexCoordName );
  FLColors    := FProgram.GetAttribLocation( aColorName );
end;

function TGLTexturedColoredQuadLayer.GetArray(aTextureID: Cardinal ): TGLTexturedColoredQuads;
begin
  Result := TGLTexturedColoredQuads( GetDrawArray( aTextureID ) );
  if Result = nil then
  begin
    Result := TGLTexturedColoredQuads.Create( FLCoords, FLTexCoords, FLColors );
    FDrawArrays.Push( Result );
    FKeys.Push( aTextureID );
  end;
end;

procedure TGLTexturedColoredQuadLayer.Draw;
var i : Integer;
begin
  if FDrawArrays.Size = 0 then Exit;

  for i := 0 to FDrawArrays.Size - 1 do
    FDrawArrays[i].Update;

  FProgram.Bind;
  for i := 0 to FDrawArrays.Size - 1 do
  begin
    glActiveTexture( GL_TEXTURE0 );
    glBindTexture( GL_TEXTURE_2D, FKeys[i]  );
    glUniform1i( FLTexture, 0 );
    FDrawArrays[i].Draw;
  end;
  FProgram.UnBind;

  for i := 0 to FDrawArrays.Size - 1 do
    FDrawArrays[i].Clear;
end;

destructor TGLTexturedColoredQuadLayer.Destroy;
begin
  inherited Destroy;
end;

{ TGLQuadBufferLayer }

constructor TGLQuadBufferLayer.Create;
begin
  FDrawArrays := TGLDrawArraysArray.Create;
  FKeys       := TCardinalArray.Create;
end;

function TGLQuadBufferLayer.AddDrawArray(aDrawArray: TGLDrawArrays; aKey : Cardinal): TGLDrawArrays;
begin
  FDrawArrays.Push( aDrawArray );
  FKeys.Push( aKey );
  Exit( aDrawArray );
end;

function TGLQuadBufferLayer.GetDrawArray(aKey : Cardinal): TGLDrawArrays;
var i : Integer;
begin
  if FKeys.Size > 0 then
    for i := 0 to FKeys.Size-1 do
      if FKeys[i] = aKey then
        Exit( FDrawArrays[i] );
  Exit( nil );
end;

destructor TGLQuadBufferLayer.Destroy;
begin
  FreeAndNil( FDrawArrays );
  FreeAndNil( FKeys );
  inherited Destroy;
end;

{ TGLDrawArrays }

constructor TGLDrawArrays.Create;
begin
  FArrays := TRawArrayArray.Create;
  FData   := TGLArrayDataArray.Create;
end;

procedure TGLDrawArrays.Clear;
var iCount : DWord;
begin
  if FArrays.Size > 0 then
  for iCount := 0 to FArrays.Size-1 do
    FArrays[ iCount ].Clear;
end;

procedure TGLDrawArrays.Update;
var iCount : DWord;
begin
  if FArrays.Size > 0 then
  for iCount := 0 to FArrays.Size-1 do
  begin
    glBindBuffer( GL_ARRAY_BUFFER, FData[iCount].VBO );
    glBufferData( GL_ARRAY_BUFFER, FArrays[iCount].Size * FArrays[iCount].ItemSize, FArrays[iCount].Data, GL_STREAM_DRAW );
  end;
  glBindBuffer( GL_ARRAY_BUFFER, 0);
end;

procedure TGLDrawArrays.Draw;
var iCount : DWord;
begin
  if FArrays.Size = 0 then Exit;

  for iCount := 0 to FArrays.Size-1 do
  begin
    glEnableVertexAttribArray( FData[iCount].AttrLoc );
    glBindBuffer(GL_ARRAY_BUFFER, FData[iCount].VBO );
    glVertexAttribPointer( FData[iCount].AttrLoc, FData[iCount].Elements, FData[iCount].GLType, GL_FALSE, 0, nil );
  end;

  glDrawArrays( GL_TRIANGLES, 0, FArrays[0].Size * 6 );

  for iCount := 0 to FArrays.Size-1 do
    glDisableVertexAttribArray( FData[iCount].AttrLoc );

  glBindBuffer( GL_ARRAY_BUFFER, 0);
end;

destructor TGLDrawArrays.Destroy;
var iCount : DWord;
    iValue : GLuint;
begin
  FreeAndNil( FArrays );
  if FData.Size > 0 then
  for iCount := 0 to FData.Size-1 do
  begin
    iValue := FData[iCount].VBO;
    glDeleteBuffers(1, @iValue );
  end;
  FreeAndNil( FData );
  inherited Destroy;
end;

procedure TGLDrawArrays.PushArray(aArray: TRawPointerArray; aElements: Cardinal; aGLType: Cardinal; aLocation : Integer );
var iData : TGLArrayData;
    iVBO  : Integer;
begin
  FArrays.Push( aArray );
  glGenBuffers( 1, @iVBO );
  iData.VBO      := iVBO;
  iData.Elements := aElements;
  iData.GLType   := aGLType;
  iData.AttrLoc  := aLocation;
  FData.Push( iData );
end;

{ TGLTexturedColoredQuads }

constructor TGLTexturedColoredQuads.Create( aAttribCoords, aAttribTexCoord, aAttribColor : Integer );
var i : LongInt;
begin
  inherited Create;
  PushArray( TGLQVec3iArray.Create, 3, GL_INT, aAttribCoords );
  PushArray( TGLQVec2fArray.Create, 2, GL_FLOAT, aAttribTexCoord );
  PushArray( TGLQVec4fArray.Create, 4, GL_FLOAT, aAttribColor );
end;

procedure TGLTexturedColoredQuads.PushQuad ( aUR, aLL : TGLVec3i; aColor : TGLVec4f; aTUR, aTLL : TGLVec2f ) ;
begin
  TGLQVec3iArray(FArrays[0]).Push( TGLQVec3i.CreateInit( aUR, aLL ) );
  TGLQVec2fArray(FArrays[1]).Push( TGLQVec2f.CreateInit( aTUR, aTLL ) );
  TGLQVec4fArray(FArrays[2]).Push( TGLQVec4f.CreateAll( aColor ) );
end;

procedure TGLTexturedColoredQuads.PushQuad(aUR, aLL: TGLVec3i;
  aColorQuad: TGLQVec4f; aTUR, aTLL: TGLVec2f);
begin
  TGLQVec3iArray(FArrays[0]).Push( TGLQVec3i.CreateInit( aUR, aLL ) );
  TGLQVec2fArray(FArrays[1]).Push( TGLQVec2f.CreateInit( aTUR, aTLL ) );
  TGLQVec4fArray(FArrays[2]).Push( aColorQuad );
end;

procedure TGLTexturedColoredQuads.PushRotatedQuad(aCenter, aSize: TGLVec3i;
  aDegrees: Single; aColor: TGLVec4f; aTUR, aTLL: TGLVec2f);
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
  TGLQVec4fArray(FArrays[2]).Push( TGLQVec4f.CreateAll( aColor ) );
end;

destructor TGLTexturedColoredQuads.Destroy;
begin
  inherited Destroy;
end;

end.

