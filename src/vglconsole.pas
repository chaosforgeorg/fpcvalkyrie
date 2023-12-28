{$INCLUDE valkyrie.inc}
unit vglconsole;
interface
uses Classes, SysUtils, vgltypes, viotypes, vioconsole, vglprogram, vbitmapfont;

// TODO: rewrite to use indices

type TGLConsoleRenderer = class( TIOConsoleRenderer )
  constructor Create( aFont : TBitmapFont; aSizeX, aSizeY : DWord; aLineSpace : DWord = 0; aReqCapabilities : TIOConsoleCapSet = [VIO_CON_CURSOR] );
  constructor Create( aFont : AnsiString; aFontGridX, aFontCount, aFontShift, aSizeX, aSizeY : DWord; aLineSpace : DWord = 0; aReqCapabilities : TIOConsoleCapSet = [VIO_CON_CURSOR] );
  procedure OutputChar( x,y : Integer; aColor : TIOColor; aChar : char ); override;
  procedure OutputChar( x,y : Integer; aFrontColor, aBackColor : TIOColor; aChar : char ); override;
  function GetChar( x,y : Integer ) : Char; override;
  function GetColor( x,y : Integer ) : TIOColor; override;
  function GetBackColor( x,y : Integer ) : TIOColor; override;
  procedure MoveCursor( x, y : Integer ); override;
  procedure ShowCursor; override;
  procedure HideCursor; override;
  procedure SetCursorType( aType : TIOCursorType ); override;
  procedure Clear; override;
  procedure ClearRect(x1,y1,x2,y2 : Integer; aBackColor : TIOColor = 0 ); override;
  procedure Resize( aNewSizeX, aNewSizeY, aLineSpace : DWord );
  procedure Update; override;
  destructor Destroy; override;
  function GetDeviceArea : TIORect; override;
  function GetSupportedCapabilities : TIOConsoleCapSet; override;
  procedure SetPositionScale( x, y : Integer; aLineSpace : Word; aScale : Byte );
private
  procedure Initialize( aFont : TBitmapFont; aLineSpace : DWord );
  procedure SetData( aIndex : DWord; aChar : Char; aFrontColor, aBackColor : TIOColor ); inline;
  procedure SetCoord( out aQCoord : TGLRawQCoord; x, y : Integer; aBackGround : Boolean = False ); inline;
  procedure SetColor( out aQColor : TGLRawQColor; aColor : TIOColor ); inline;
  procedure CreateColor( out aQColor : TGLRawQColor; aR, aG, aB : Byte );
private
  FOwnTextures  : Boolean;
  FFont         : TBitmapFont;
  FCurVisible   : Boolean;
  FLineSpace    : DWord;
  FStartTime    : TDateTime;
  FBSupport     : Boolean;
  FColorMask    : DWord;
  FPositionX    : Integer;
  FPositionY    : Integer;
  FScale        : Integer;
  FASCII        : packed array of Char;
  FColor        : packed array of TIOColor;
  FBColor       : packed array of TIOColor;
  FColors       : packed array of TGLRawQColor;
  FCoords       : packed array of TGLRawQCoord;
  FBColors      : packed array of TGLRawQColor;
  FBCoords      : packed array of TGLRawQCoord;
  FTexCoords    : packed array of TGLRawQTexCoord;
  FColorLookup  : packed array of TGLRawQColor;
  FVAO          : Cardinal;
  FBColorVBO    : Cardinal;
  FFColorVBO    : Cardinal;
  FTCoordVBO    : Cardinal;
  FBCoordVBO    : Cardinal;
  FFCoordVBO    : Cardinal;
  FBProgram     : TGLProgram;
  FFProgram     : TGLProgram;
public
  property Font : TBitmapFont read FFont;
end;

implementation

uses dateutils, vgl3library, vmath, vutil, vcolor, vtextures;

var EmptyTexCoord  : TGLRawQTexCoord;

const BColorMask = $000000F0;

const GLByteColors : array[0..15] of TGLByteColor = (
      ( Data : (   0,   0,   0 ) ),
      ( Data : (   0,   0, 160 ) ),
      ( Data : (   0, 160,   0 ) ),
      ( Data : (   0, 160, 160 ) ),
      ( Data : ( 160,   0,   0 ) ),
      ( Data : ( 160,   0, 160 ) ),
      ( Data : ( 160, 160,   0 ) ),
      ( Data : ( 216, 216, 216 ) ),
      ( Data : ( 127, 127, 127 ) ),
      ( Data : (   0,   0, 255 ) ),
      ( Data : (   0, 255,   0 ) ),
      ( Data : (   0, 255, 255 ) ),
      ( Data : ( 255,   0,   0 ) ),
      ( Data : ( 255,   0, 255 ) ),
      ( Data : ( 255, 255,   0 ) ),
      ( Data : ( 255, 255, 255 ) )
      );

const
GLConsoleBGVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec2 position;'+#10+
'layout (location = 1) in vec3 bcolor;'+#10+
'uniform mat4 projection;'+#10+
#10+
'out vec4 obcolor;'+#10+
#10+
'void main() {'+#10+
'if ( bcolor != vec3(0) ) obcolor = vec4( bcolor, 1.0 ); else obcolor = vec4(0);'+#10+
'gl_Position = projection * vec4(position, 0.0, 1.0);'+#10+
'}'+#10;
GLConsoleBGFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec4 obcolor;'+#10+
'out vec4 frag_color;'+#10+
#10+
'void main() {'+#10+
'frag_color = obcolor;'+#10+
'}'+#10;

const
GLConsoleFGVertexShader : Ansistring =
'#version 330 core'+#10+
'layout (location = 0) in vec2 position;'+#10+
'layout (location = 1) in vec3 fcolor;'+#10+
'layout (location = 2) in vec2 tcoord;'+#10+
'uniform mat4 projection;'+#10+
#10+
'out vec4 ofcolor;'+#10+
'out vec2 otcoord;'+#10+
#10+
'void main() {'+#10+
'ofcolor = vec4( fcolor, 1.0 );'+#10+
'otcoord = tcoord;'+#10+
'gl_Position = projection * vec4(position, 0.0, 1.0);'+#10+
'}'+#10;
GLConsoleFGFragmentShader : Ansistring =
'#version 330 core'+#10+
'in vec4 ofcolor;'+#10+
'in vec2 otcoord;'+#10+
'uniform sampler2D utexture;'+#10+
'out vec4 frag_color;'+#10+
#10+
'void main() {'+#10+
'frag_color = texture(utexture, otcoord) * ofcolor;'+#10+
'}'+#10;

{ TGLConsoleRenderer }

procedure TGLConsoleRenderer.Initialize( aFont : TBitmapFont; aLineSpace : DWord );
var i : Byte;
begin
  Log('Initializing GL Console Renderer...');

  FBProgram := TGLProgram.Create( GLConsoleBGVertexShader, GLConsoleBGFragmentShader );
  FFProgram := TGLProgram.Create( GLConsoleFGVertexShader, GLConsoleFGFragmentShader );
  glGenVertexArrays(1, @FVAO);
  glGenBuffers(1, @FFCoordVBO);
  glGenBuffers(1, @FBCoordVBO);
  glGenBuffers(1, @FBColorVBO);
  glGenBuffers(1, @FFColorVBO);
  glGenBuffers(1, @FTCoordVBO);

  FFont := aFont;
  FFont.SetTexCoord( EmptyTexCoord, ' ' );

  FBSupport  := VIO_CON_BGCOLOR in FCapabilities;
  FBSupport  := True;

  if VIO_CON_EXTCOLOR in FCapabilities
    then FColorMask := $FFFFFF0F
    else FColorMask := $0000000F;

  FLineSpace := aLineSpace;

  SetLength( FColorLookup, 16 );
  for i := 0 to 15 do
    CreateColor( FColorLookup[i], GLByteColors[i].Data[0], GLByteColors[i].Data[1], GLByteColors[i].Data[2] );

  FPositionX := 0;
  FPositionY := 0;
  FScale     := 1;
  Resize( FSizeX, FSizeY, aLineSpace );
  FStartTime := Now;
  FCurVisible := VIO_CON_CURSOR in FCapabilities;
  SetCursorType( VIO_CURSOR_SMALL );
end;

constructor TGLConsoleRenderer.Create ( aFont : TBitmapFont; aSizeX, aSizeY : DWord; aLineSpace : DWord; aReqCapabilities : TIOConsoleCapSet );
begin
  inherited Create( aSizeX, aSizeY, aReqCapabilities );
  Initialize( aFont, aLineSpace );
  FOwnTextures  := False;
end;

constructor TGLConsoleRenderer.Create ( aFont : AnsiString; aFontGridX, aFontCount, aFontShift, aSizeX, aSizeY : DWord; aLineSpace : DWord; aReqCapabilities : TIOConsoleCapSet ) ;
var iTextureID : TTextureID;
begin
  inherited Create( aSizeX, aSizeY, aReqCapabilities );
  FOwnTextures  := False;
  if not TTextureManager.Initialized then
  begin
    FOwnTextures  := True;
    TTextureManager.Create();
  end;
  with TTextureManager.Get() do
  begin
    iTextureID := AddFile( aFont, BlendDefault );
    Texture[ iTextureID ].Image.SubstituteColor( ColorBlack, ColorZero );
    Texture[ iTextureID ].Upload;
  end;
  Initialize( TBitmapFont.CreateFromGrid( iTextureID, aFontGridX, aFontCount, aFontShift ), aLineSpace );
end;

procedure TGLConsoleRenderer.OutputChar ( x, y : Integer; aColor : TIOColor; aChar : char ) ;
var iCoord      : LongInt;
    iBackColor  : DWord;
begin
  if aColor = ColorNone then Exit;
  if FBSupport then
  begin
    iBackColor := (aColor and BColorMask) shr 4;
    if iBackColor <> 0 then
    begin
      OutputChar( x, y, aColor, iBackColor, aChar );
      Exit;
    end;
  end;
  iCoord := FSizeX*Clamp( y-1, 0, FSizeY-1 )+Clamp( x-1, 0, FSizeX-1 );
  aColor := aColor and FColorMask;
  if (FASCII[ iCoord ] = aChar) and (FColor[ iCoord ] = aColor) then Exit;
  if FBSupport then
    SetData( iCoord, aChar, aColor, FBColor[ iCoord ] )
  else
    SetData( iCoord, aChar, aColor, Black );
end;

procedure TGLConsoleRenderer.OutputChar ( x, y : Integer; aFrontColor, aBackColor : TIOColor; aChar : char ) ;
var iCoord : LongInt;
begin
  if (aBackColor = ColorNone) or (not FBSupport) then begin OutputChar( x, y, aFrontColor, aChar ); Exit; end;
  iCoord := FSizeX*Clamp( y-1, 0, FSizeY-1 )+Clamp( x-1, 0, FSizeX-1 );
  aFrontColor := aFrontColor and FColorMask;
  aBackColor  := aBackColor  and FColorMask;
  if ( FASCII[ iCoord ] = aChar ) and ( FColor[ iCoord ] = aFrontColor ) and ( FBColor[ iCoord ] = aBackColor ) then Exit;
  SetData( iCoord, aChar, aFrontColor, aBackColor );
end;

function TGLConsoleRenderer.GetChar ( x, y : Integer ) : Char;
begin
  x := Clamp( x-1, 0, FSizeX-1 );
  y := Clamp( y-1, 0, FSizeY-1 );
  Exit( FASCII[ FSizeX*y+x ] );
end;

function TGLConsoleRenderer.GetColor ( x, y : Integer ) : TIOColor;
begin
  x := Clamp( x-1, 0, FSizeX-1 );
  y := Clamp( y-1, 0, FSizeY-1 );
  Exit( FColor[ FSizeX*y+x ] );
end;

function TGLConsoleRenderer.GetBackColor ( x, y : Integer ) : TIOColor;
begin
  if not FBSupport then Exit( 0 );
  x := Clamp( x-1, 0, FSizeX-1 );
  y := Clamp( y-1, 0, FSizeY-1 );
  Exit( FBColor[ FSizeX*y+x ] );
end;

procedure TGLConsoleRenderer.MoveCursor ( x, y : Integer ) ;
begin
  if not (VIO_CON_CURSOR in FCapabilities) then Exit;
  x := Clamp( x-1, 0, FSizeX-1 );
  y := Clamp( y-1, 0, FSizeY-1 );
  FCoords[ FSizeX * FSizeY ] := FCoords[ FSizeX*y+x ];
end;

procedure TGLConsoleRenderer.ShowCursor;
begin
  FCurVisible := VIO_CON_CURSOR in FCapabilities;
end;

procedure TGLConsoleRenderer.HideCursor;
begin
  FCurVisible := False;
end;

procedure TGLConsoleRenderer.SetCursorType ( aType : TIOCursorType ) ;
var iChar : Char;
begin
  if not (VIO_CON_CURSOR in FCapabilities) then Exit;
  FCurVisible := True;
  case aType of
     VIO_CURSOR_SMALL : iChar := '_';
     VIO_CURSOR_HALF  : iChar := #220;
     VIO_CURSOR_BLOCK : iChar := #219;
  end;
  FFont.SetTexCoord( FTexCoords[ FSizeX * FSizeY ], iChar );
  inherited SetCursorType( aType );
end;

procedure TGLConsoleRenderer.Clear;
var iCount, iSize : DWord;
begin
  iSize  := FSizeX * FSizeY;
  for iCount := 0 to iSize-1 do
  begin
    FASCII[ iCount ]     := ' ';
    FColor[ iCount ]     := LightGray;
    FTexCoords[ iCount ] := EmptyTexCoord;
    FColors[ iCount ]    := FColorLookup[ LightGray ];
    if FBSupport then
    begin
      FBColor[ iCount ]  := Black;
      FBColors[ iCount ] := FColorLookup[ Black ];
    end;
  end;
end;

procedure TGLConsoleRenderer.ClearRect ( x1, y1, x2, y2 : Integer; aBackColor : TIOColor ) ;
var iX, iY  : LongInt;
    iBColor : TGLRawQColor;
    iCoord  : LongInt;
begin
  x1 := Clamp( x1-1, 0, FSizeX-1 );
  y1 := Clamp( y1-1, 0, FSizeY-1 );
  x2 := Clamp( x2-1, 0, FSizeX-1 );
  y2 := Clamp( y2-1, 0, FSizeY-1 );

  if FBSupport then
    SetColor( iBColor, aBackColor );

  if (x2 < x1) or (y2 < y1) then Exit;
  for iY := y1 to y2 do
    for iX := x1 to x2 do
    begin
      iCoord := iY*FSizeX+iX;
      FASCII[ iCoord ]     := ' ';
      FTexCoords[ iCoord ] := EmptyTexCoord;
      if aBackColor = ColorNone then Continue;
      FColor[ iCoord ]     := LightGray;
      FColors[ iCoord ]    := FColorLookup[ LightGray ];
      if FBSupport then
      begin
        FBColor[ iCoord ]  := aBackColor;
        FBColors[ iCoord ] := iBColor;
      end;
    end;
end;

procedure TGLConsoleRenderer.Resize ( aNewSizeX, aNewSizeY, aLineSpace : DWord );
var iSize, iX, iY : LongInt;
    iMatrix       : TMatrix44;
    iPLoc         : Integer;
    iRect         : TIORect;
begin
  FLineSpace := aLineSpace;
  FSizeX     := aNewSizeX;
  FSizeY     := aNewSizeY;
  iSize      := aNewSizeX * aNewSizeY;

  SetLength( FASCII, iSize );
  SetLength( FColor, iSize );
  SetLength( FCoords, iSize + 1 );
  SetLength( FTexCoords, iSize + 1 );
  SetLength( FColors, iSize + 1 );

  for iY := 0 to FSizeY - 1 do
    for iX := 0 to FSizeX - 1 do
      SetCoord( FCoords[ iY*FSizeX+iX ], iX, iY );

  if FBSupport then
  begin
    SetLength( FBColors, iSize );
    SetLength( FBColor, iSize );
    if aLineSpace = 0 then
      FBCoords := FCoords
    else
    begin
      SetLength( FBCoords, iSize );
      for iY := 0 to FSizeY - 1 do
        for iX := 0 to FSizeX - 1 do
          SetCoord( FBCoords[ iY*FSizeX+iX ], iX, iY, True );
    end;
  end;

  SetColor( FColors[ iSize ], LightGray );
  SetCursorType( VIO_CURSOR_SMALL );
  MoveCursor( 1, 1 );
  Clear;

  iRect   := GetDeviceArea();
  iMatrix := GLCreateOrtho( 0, iRect.Dim.X, iRect.Dim.Y, 0, -1, 1 );

  FBProgram.Bind;
    iPLoc := FBProgram.GetUniformLocation('projection');
    glUniformMatrix4fv(iPLoc, 1, GL_FALSE, @iMatrix[0]);
  FBProgram.Unbind;

  FFProgram.Bind;
    iPLoc := FFProgram.GetUniformLocation('projection');
    glUniformMatrix4fv(iPLoc, 1, GL_FALSE, @iMatrix[0]);

    iPLoc := FFProgram.GetUniformLocation('utexture');
    glUniform1i(iPLoc, 0);
  FFProgram.Unbind;

end;

procedure TGLConsoleRenderer.Update;
var iCount : DWord;
    iTick  : DWord;
begin
  iCount := FSizeX * FSizeY;

  glBindVertexArray(FVAO);

  if FBSupport then
  begin // background rendering
    FBProgram.Bind;

    glBindBuffer( GL_ARRAY_BUFFER, FBCoordVBO);
    glBufferData( GL_ARRAY_BUFFER, iCount*sizeof(TGLRawQCoord), @(FBCoords[0]), GL_STREAM_DRAW );
    glVertexAttribPointer(0, 2, GL_INT, GL_FALSE, 2 * sizeof(Integer), nil );
    glEnableVertexAttribArray(0);

    glBindBuffer( GL_ARRAY_BUFFER, FBColorVBO);
    glBufferData( GL_ARRAY_BUFFER, iCount*sizeof(TGLRawQColor), @(FBColors[0]), GL_STREAM_DRAW );
    glVertexAttribPointer(1, 3, GL_UNSIGNED_BYTE, GL_TRUE, 3 * sizeof(GLubyte), nil );
    glEnableVertexAttribArray(1);

    glDrawArrays( GL_QUADS, 0, iCount*4 );

    FBProgram.UnBind;
    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    glBindBuffer( GL_ARRAY_BUFFER, 0);
  end;

  begin
    if (VIO_CON_CURSOR in FCapabilities) and FCurVisible then
        Inc( iCount );

    FFProgram.Bind;

    glActiveTexture(0);
    glBindTexture( GL_TEXTURE_2D, FFont.GLTexture );

    glBindBuffer( GL_ARRAY_BUFFER, FFCoordVBO);
    glBufferData( GL_ARRAY_BUFFER, iCount*sizeof(TGLRawQCoord), @(FCoords[0]), GL_STREAM_DRAW );
    glVertexAttribPointer(0, 2, GL_INT, GL_FALSE, 2 * sizeof(Integer), nil );
    glEnableVertexAttribArray(0);

    glBindBuffer( GL_ARRAY_BUFFER, FFColorVBO);
    glBufferData( GL_ARRAY_BUFFER, iCount*sizeof(TGLRawQColor), @(FColors[0]), GL_STREAM_DRAW );
    glVertexAttribPointer(1, 3, GL_UNSIGNED_BYTE, GL_TRUE, 3 * sizeof(GLubyte), nil );
    glEnableVertexAttribArray(1);

    glBindBuffer( GL_ARRAY_BUFFER, FTCoordVBO);
    glBufferData( GL_ARRAY_BUFFER, iCount*sizeof(TGLRawQTexCoord), @(FTexCoords[0]), GL_STREAM_DRAW );
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nil );
    glEnableVertexAttribArray(2);


    if (VIO_CON_CURSOR in FCapabilities) and FCurVisible then
    begin
      iTick := MilliSecondsBetween(Now,FStartTime) div 500;
      if iTick mod 2 = 0 then Dec( iCount );
    end;

    glDrawArrays( GL_QUADS, 0, iCount*4 );

    FFProgram.UnBind;
    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(2);
    glBindBuffer( GL_ARRAY_BUFFER, 0);
  end;

  glBindVertexArray(0);
end;

destructor TGLConsoleRenderer.Destroy;
begin
  glDeleteBuffers(1, @FBColorVBO);
  glDeleteBuffers(1, @FFColorVBO);
  glDeleteBuffers(1, @FFCoordVBO);
  glDeleteBuffers(1, @FBCoordVBO);
  glDeleteBuffers(1, @FTCoordVBO);
  glDeleteVertexArrays(1, @FVAO);
  FreeAndNil( FBProgram );
  FreeAndNil( FFProgram );
  FreeAndNil( FFont );
  if FOwnTextures then TTextureManager.Get().Free;
  inherited Destroy;
end;

function TGLConsoleRenderer.GetDeviceArea : TIORect;
begin
  GetDeviceArea.Pos := Point( FPositionX, FPositionY );
  GetDeviceArea.Dim := Point( FFont.GylphSize.X * FSizeX * FScale, (FFont.GylphSize.Y + FLineSpace) * FSizeY * FScale );
end;

procedure TGLConsoleRenderer.SetData( aIndex : DWord; aChar : Char; aFrontColor, aBackColor : TIOColor );
begin
  if (aFrontColor <> ColorNone) then
  begin
    FASCII[ aIndex ] := aChar;
    FFont.SetTexCoord( FTexCoords[ aIndex ], aChar );
  end
  else
    aFrontColor := 0;
  FColor[ aIndex ] := aFrontColor;
  SetColor( FColors[ aIndex ], aFrontColor );
  if FBSupport then
  begin
    FBColor[ aIndex ] := aBackColor;
    SetColor( FBColors[ aIndex ], aBackColor );
  end;
end;

procedure TGLConsoleRenderer.SetCoord ( out aQCoord : TGLRawQCoord; x, y : Integer; aBackGround : Boolean = False ) ;
var c1, c2, p : TGLVec2i;
begin
  c1.x := x * FFont.GylphSize.X;
  c2.x := c1.x + FFont.GylphSize.X;

  if aBackGround then
  begin
    c1.y := y * ( FFont.GylphSize.Y + FLineSpace );
    c2.y := c1.y + FFont.GylphSize.Y + FLineSpace;
  end
  else
  begin
    c1.y := y * ( FFont.GylphSize.Y + FLineSpace ) + FLineSpace div 2;
    c2.y := c1.y + FFont.GylphSize.Y;
  end;
  p := TGLVec2i.Create( FPositionX, FPositionY );
  aQCoord.Init( p + c1.Scaled( FScale ), p + c2.Scaled( FScale ) );
end;

procedure TGLConsoleRenderer.SetColor ( out aQColor : TGLRawQColor; aColor : TIOColor ) ;
begin
  if aColor = ColorNone then aColor := 0;
  if aColor < 256
    then aQColor := FColorLookup[ aColor mod 16 ]
    else CreateColor( aQColor,
      (aColor and $FF000000) shr 24,
      (aColor and $00FF0000) shr 16,
      (aColor and $0000FF00) shr 8
    );
end;

procedure TGLConsoleRenderer.CreateColor( out aQColor : TGLRawQColor; aR, aG, aB : Byte );
begin
  aQColor.SetAll( TGLByteColor.Create( aR, aG, aB) );
end;

function TGLConsoleRenderer.GetSupportedCapabilities : TIOConsoleCapSet;
begin
  Result := [ VIO_CON_BGCOLOR, VIO_CON_CURSOR, VIO_CON_EXTCOLOR ];
end;

procedure TGLConsoleRenderer.SetPositionScale ( x, y : Integer; aLineSpace : Word; aScale : Byte ) ;
begin
  FPositionX    := x;
  FPositionY    := y;
  FLineSpace    := aLineSpace;
  FScale        := aScale;
  Resize( FSizeX, FSizeY, FLineSpace );
end;

end.

