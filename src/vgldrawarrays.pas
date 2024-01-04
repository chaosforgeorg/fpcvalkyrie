{$INCLUDE valkyrie.inc}
unit vgldrawarrays;
interface

uses vgenerics;

type TGLArrayData = record
    VBO      : Integer;
    Elements : Cardinal;
    GLType   : Cardinal;
    AttrLoc  : Integer;
  end;

type TRawArrayArray    = specialize TGObjectArray< TRawPointerArray >;
type TGLArrayDataArray = specialize TGArray< TGLArrayData >;

type TGLDrawArrays = class
  constructor Create;
  procedure Clear;
  procedure Update;
  procedure Draw;
  function Empty : Boolean;
  destructor Destroy; override;
protected
  procedure PushArray( aArray : TRawPointerArray; aElements : Cardinal; aGLType   : Cardinal; aLocation : Integer );
protected
  FVAO    : Cardinal;
  FArrays : TRawArrayArray;
  FData   : TGLArrayDataArray;
end;

type TGLDrawArraysArray = specialize TGObjectArray< TGLDrawArrays >;
type TTextureIDArray    = specialize TGArray< Cardinal >;

{ TGLQuadBufferLayer }
type TGLTexturedArrays = class
  constructor Create;
  procedure Draw;
  procedure Update;
  procedure Clear;
  function AddDrawArray( aDrawArray : TGLDrawArrays; aTexture : Cardinal ) : TGLDrawArrays;
  function GetDrawArray( aTexture : Cardinal ) : TGLDrawArrays;
  function Empty : Boolean;
  destructor Destroy; override;
protected
  FDrawArrays : TGLDrawArraysArray;
  FTextureIDs : TTextureIDArray;
end;

implementation

uses SysUtils, vgl3library;

{ TGLDrawArrays }

constructor TGLDrawArrays.Create;
begin
  FArrays := TRawArrayArray.Create;
  FData   := TGLArrayDataArray.Create;
  glGenVertexArrays( 1, @FVAO );
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
  glBindVertexArray( FVAO );

  for iCount := 0 to FArrays.Size-1 do
  begin
    glEnableVertexAttribArray( FData[iCount].AttrLoc );
    glBindBuffer( GL_ARRAY_BUFFER, FData[iCount].VBO );
    glVertexAttribPointer( FData[iCount].AttrLoc, FData[iCount].Elements, FData[iCount].GLType, GL_FALSE, 0, nil );
  end;

  glDrawArrays( GL_TRIANGLES, 0, FArrays[0].Size * 6 );

  for iCount := 0 to FArrays.Size-1 do
    glDisableVertexAttribArray( FData[iCount].AttrLoc );

  glBindBuffer( GL_ARRAY_BUFFER, 0);
  glBindVertexArray( 0 );
end;

function TGLDrawArrays.Empty : Boolean;
begin
  Exit( FArrays.Size = 0 );
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
  glDeleteVertexArrays( 1, @FVAO );
  FreeAndNil( FData );
  inherited Destroy;
end;

procedure TGLDrawArrays.PushArray( aArray: TRawPointerArray; aElements: Cardinal; aGLType: Cardinal; aLocation : Integer );
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

{ TGLTexturedArrays }

constructor TGLTexturedArrays.Create;
begin
  FDrawArrays := TGLDrawArraysArray.Create;
  FTextureIDs := TTextureIDArray.Create;
end;

function TGLTexturedArrays.AddDrawArray( aDrawArray: TGLDrawArrays; aTexture : Cardinal ): TGLDrawArrays;
begin
  FDrawArrays.Push( aDrawArray );
  FTextureIDs.Push( aTexture );
  Exit( aDrawArray );
end;

function TGLTexturedArrays.GetDrawArray(aTexture : Cardinal): TGLDrawArrays;
var i : Integer;
begin
  if Assigned( FTextureIDs ) then
    if FTextureIDs.Size > 0 then
      for i := 0 to FTextureIDs.Size-1 do
        if FTextureIDs[i] = aTexture then
          Exit( FDrawArrays[i] );
  Exit( nil );
end;

procedure TGLTexturedArrays.Update;
var i : Integer;
begin
   if FDrawArrays.Size > 0 then
     for i := 0 to FDrawArrays.Size - 1 do
       FDrawArrays[i].Update;
end;

procedure TGLTexturedArrays.Draw;
var i : Integer;
begin
  if FDrawArrays.Size > 0 then
    for i := 0 to FDrawArrays.Size - 1 do
    begin
      glActiveTexture( GL_TEXTURE0 );
      glBindTexture( GL_TEXTURE_2D, FTextureIDs[i]  );
      FDrawArrays[i].Draw;
    end;
end;

procedure TGLTexturedArrays.Clear;
var i : Integer;
begin
  if FDrawArrays.Size > 0 then
    for i := 0 to FDrawArrays.Size - 1 do
      FDrawArrays[i].Clear;
end;

function TGLTexturedArrays.Empty : Boolean;
var i : Integer;
begin
  if FDrawArrays.Size = 0 then Exit( True );
  for i := 0 to FDrawArrays.Size - 1 do
    if not FDrawArrays[i].Empty then
      Exit( False );
  Exit( True );
end;

destructor TGLTexturedArrays.Destroy;
begin
  FreeAndNil( FDrawArrays );
  FreeAndNil( FTextureIDs );
  inherited Destroy;
end;

end.

