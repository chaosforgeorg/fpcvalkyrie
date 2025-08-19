{$INCLUDE valkyrie.inc}
unit vglconsole;
interface
uses Classes, SysUtils, vgltypes, viotypes, vioconsole, vglprogram, vbitmapfont;

type TGLCell = packed record
  fgcolor : Cardinal;
  bgcolor : Cardinal;
  glyph   : Cardinal;
  padding : Cardinal;
end;


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
  function MakeColor( aColor : TIOColor ) : TIOColor;
private
  FOwnTextures  : Boolean;
  FFont         : TBitmapFont;
  FCurVisible   : Boolean;
  FLineSpace    : DWord;
  FGlyphStretch : Boolean;
  FStartTime    : TDateTime;
  FBSupport     : Boolean;
  FColorMask    : DWord;
  FPositionX    : Integer;
  FPositionY    : Integer;
  FCursorX      : Integer;
  FCursorY      : Integer;
  FScale        : Integer;
  FColor        : packed array of TIOColor;
  FBColor       : packed array of TIOColor;
  FCells        : packed array of TGLCell;
  FDataVBO      : Cardinal;
  FDataVAO      : Cardinal;
  FDataTexture  : Cardinal;
  FTriangleVBO  : Cardinal;
  FTriangleVAO  : Cardinal;
  FProgram      : TGLProgram;
  FPCursor      : Integer;
  FCGlyph       : Integer;
public
  property Font : TBitmapFont read FFont;
  property GlyphStretch : Boolean read FGlyphStretch write FGlyphStretch;
end;

implementation

uses dateutils, vgl3library, vsdlio, vmath, vutil, vcolor, vtextures;

var EmptyTexCoord  : TGLRawQTexCoord;

const BColorMask = $000000F0;

const GLFSTriangle : array[0..2] of TVertex2f2f = (
      ( position : ( Data : ( -1, -1 ) ); texcoord : ( Data : ( 0, 0 ) ) ),
      ( position : ( Data : (  3, -1 ) ); texcoord : ( Data : ( 2, 0 ) ) ),
      ( position : ( Data : ( -1,  3 ) ); texcoord : ( Data : ( 0, 2 ) ) )
);

const
GLConsoleVertexShader   : AnsiString =
'#version 330' + #10 +
'layout( location = 0 ) in vec2 position;' + #10 +
'layout( location = 1 ) in vec2 texcoord;' + #10 +
'' + #10 +
'out vec2 otexcoord;' + #10 +
'' + #10 +
'uniform mat4 projection;' + #10 +
'' + #10 +
'void main( void )' + #10 +
'{' + #10 +
'    gl_Position = projection * vec4( position, 0.0, 1.0 );' + #10 +
'    otexcoord   = texcoord;' + #10 +
'}' + #10
;

GLConsoleFragmentShader : AnsiString =
'#version 330' + #10 +
'' + #10 +
'in  vec2 otexcoord;' + #10 +
'out vec4 frag_color;' + #10 +
'' + #10 +
'uniform vec2 uterm_size;' + #10 +
'uniform ivec2 usheet_size;' + #10 +
'uniform int usheet_offset;' + #10 +
'uniform int uline_space;' + #10 +
'uniform sampler2D udiffuse;' + #10 +
'uniform usamplerBuffer udata;' + #10 +
'uniform ivec3 ucursor;' + #10 +
'uniform int ustretch;' + #10 +
'' + #10 +
'void main(void)' + #10 +
'{' + #10 +
'    if ( otexcoord.x > 1.0 || otexcoord.y > 1.0 )' + #10 +
'        discard;' + #10 +
'    vec2 coord = otexcoord * uterm_size;' + #10 +
'    coord.y = uterm_size.y - coord.y;' + #10 +
'    ivec2 ts = textureSize( udiffuse, 0 );' + #10 +
'    int lines = ts.y / usheet_size.y;' + #10 +
'    float lfac = float( uline_space ) / float( lines );' + #10 +
'    vec2 tc = fract( coord ) * vec2( 1.0, 1.0 + lfac ) - vec2( 0.0, lfac * 0.5 );' + #10 +
'    if ( ustretch > 0 ) tc.y = clamp(tc.y, 0.0, 0.99); ' + #10 +
'' + #10 +
'    int index = int( coord.x ) + int( uterm_size.x ) * int( coord.y );' + #10 +
'    uvec4 data_sample = texelFetch( udata, index ).rgba;' + #10 +
'' + #10 +
'    uint fg = data_sample.r;' + #10 +
'    uint bg = data_sample.g;' + #10 +
'' + #10 +
'    int glyph = int( data_sample.b ) - usheet_offset;' + #10 +
'    ivec2 gxy = ivec2( glyph % usheet_size.x, glyph / usheet_size.x );' + #10 +
'    vec2 gpos = vec2( ( float( gxy.x ) + tc.x ) / float( usheet_size.x ), ( float( gxy.y ) + tc.y ) / float( usheet_size.y ) );' + #10 +
'    vec4 tt = texelFetch( udiffuse, ivec2( gpos * vec2( ts ) ), 0 );' + #10 +
'' + #10 +
'    if ( int( coord.x ) == ucursor.x && int( coord.y ) == ucursor.y ) {' + #10 +
'      int cglyph = ucursor.z - usheet_offset;' + #10 +
'      ivec2 cgxy = ivec2( cglyph % usheet_size.x, cglyph / usheet_size.x );' + #10 +
'      vec2 cgpos = vec2( ( float( cgxy.x ) + tc.x ) / float( usheet_size.x ), ( float( cgxy.y ) + tc.y ) / float( usheet_size.y ) );' + #10 +
'      vec4 ctt = texelFetch( udiffuse, ivec2( cgpos * vec2( ts ) ), 0 );' + #10 +
'      tt.x = max( ctt.x, tt.x );' + #10 +
'    }' + #10 +
'    if ( tc.y < 0 || tc.y > 1.0 )' + #10 +
'        tt.x = 0;' + #10 +
'' + #10 +
'    vec4 fg_color = vec4(' + #10 +
'        float( ( fg & uint(0xFF000000) ) >> 24 ) / 255.0,' + #10 +
'        float( ( fg & uint(0x00FF0000) ) >> 16 ) / 255.0,' + #10 +
'        float( ( fg & uint(0x0000FF00) ) >> 8 ) / 255.0,' + #10 +
'        float( ( fg & uint(0x0000000F) ) ) / 16.0' + #10 +
'    );' + #10 +
'    vec4 bg_color = vec4(' + #10 +
'        float( ( bg & uint(0xFF000000) ) >> 24 ) / 255.0,' + #10 +
'        float( ( bg & uint(0x00FF0000) ) >> 16 ) / 255.0,' + #10 +
'        float( ( bg & uint(0x0000FF00) ) >> 8 ) / 255.0,' + #10 +
'        float( ( bg & uint(0x000000FF) ) ) / 255.0' + #10 +
'    );' + #10 +
'    vec3 out_fg = fg_color.xyz * tt.x;' + #10 +
'    vec3 out_bg = bg_color.xyz * ( 1.0f - tt.x );' + #10 +
'    float fg_a = fg_color.w * tt.x;' + #10 +
'    float bg_a = bg_color.w * ( 1.0f - tt.x );' + #10 +
'    frag_color = vec4( out_fg + out_bg, fg_a + bg_a );' + #10 +
'}'+ #10
;


function TGLConsoleRenderer.MakeColor( aColor : TIOColor ) : TIOColor;
begin
  if aColor = ColorNone then aColor := 0;
  if aColor < 256
    then Exit( IoColors[ aColor mod 16 ] )
    else Exit( aColor );
end;

{ TGLConsoleRenderer }

procedure TGLConsoleRenderer.Initialize( aFont : TBitmapFont; aLineSpace : DWord );
begin
  Log('Initializing GL Console Renderer...');

  FProgram  := TGLProgram.Create( GLConsoleVertexShader,   GLConsoleFragmentShader );
  glGenVertexArrays(1, @FDataVAO);
  glGenVertexArrays(1, @FTriangleVAO);
  glGenBuffers( 1, @FDataVBO );
  glGenBuffers( 1, @FTriangleVBO );

  glGenTextures( 1, @FDataTexture );

  glBindVertexArray(FTriangleVAO);
  glBindBuffer( GL_ARRAY_BUFFER, FTriangleVBO );
  glBufferData( GL_ARRAY_BUFFER, sizeof(GLFSTriangle), @(GLFSTriangle), GL_STATIC_DRAW );
  glVertexAttribPointer( 0, 2, GL_FLOAT, GL_FALSE, sizeof(TVertex2f2f), nil );
  glEnableVertexAttribArray(0);
  glVertexAttribPointer( 1, 2, GL_FLOAT, GL_FALSE, sizeof(TVertex2f2f), Pointer(2 * sizeof(Single)));
  glEnableVertexAttribArray(1);
  glBindBuffer( GL_ARRAY_BUFFER, 0 );
  glBindVertexArray(0);

  FFont := aFont;
  FFont.SetTexCoord( EmptyTexCoord, ' ' );

  FBSupport  := VIO_CON_BGCOLOR in FCapabilities;

  if VIO_CON_EXTCOLOR in FCapabilities
    then FColorMask := $FFFFFFFF
    else FColorMask := $0000000F;

  FLineSpace := aLineSpace;

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
  FGlyphStretch := False;
end;

constructor TGLConsoleRenderer.Create ( aFont : AnsiString; aFontGridX, aFontCount, aFontShift, aSizeX, aSizeY : DWord; aLineSpace : DWord; aReqCapabilities : TIOConsoleCapSet ) ;
var iTextureID : TTextureID;
begin
  inherited Create( aSizeX, aSizeY, aReqCapabilities );
  FOwnTextures  := False;
  FGlyphStretch := False;
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
  if (FCells[ iCoord ].glyph = Ord( aChar ) ) and (FColor[ iCoord ] = aColor) then Exit;
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
  if ( FCells[ iCoord ].glyph = Ord( aChar ) ) and ( FColor[ iCoord ] = aFrontColor ) and ( FBColor[ iCoord ] = aBackColor ) then Exit;
  SetData( iCoord, aChar, aFrontColor, aBackColor );
end;

function TGLConsoleRenderer.GetChar ( x, y : Integer ) : Char;
begin
  x := Clamp( x-1, 0, FSizeX-1 );
  y := Clamp( y-1, 0, FSizeY-1 );
  Exit( Chr( FCells[ FSizeX*y+x ].glyph ) );
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
  FCursorX := x;
  FCursorY := y;
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
  FCGlyph := Ord( iChar );
  inherited SetCursorType( aType );
end;

procedure TGLConsoleRenderer.Clear;
var iCount, iSize : DWord;
begin
  iSize  := FSizeX * FSizeY;
  for iCount := 0 to iSize-1 do
  begin
    FCells[ iCount ].fgcolor := IoColors[LightGray];
    FCells[ iCount ].bgcolor := 0;
    FCells[ iCount ].glyph   := Ord(' ');

    FColor[ iCount ]     := LightGray;
    if FBSupport then
      FBColor[ iCount ]  := Black;
  end;
end;

procedure TGLConsoleRenderer.ClearRect ( x1, y1, x2, y2 : Integer; aBackColor : TIOColor ) ;
var iX, iY  : LongInt;
    iCoord  : LongInt;
begin
  x1 := Clamp( x1-1, 0, FSizeX-1 );
  y1 := Clamp( y1-1, 0, FSizeY-1 );
  x2 := Clamp( x2-1, 0, FSizeX-1 );
  y2 := Clamp( y2-1, 0, FSizeY-1 );

  if (x2 < x1) or (y2 < y1) then Exit;
  for iY := y1 to y2 do
    for iX := x1 to x2 do
    begin
      iCoord := iY*FSizeX+iX;
      FCells[ iCoord ].glyph   := Ord(' ');

      if aBackColor = ColorNone then Continue;
      FCells[ iCoord ].fgcolor := IoColors[LightGray];
      FColor[ iCoord ]         := LightGray;
      if FBSupport then
      begin
        FCells[ iCoord ].bgcolor := MakeColor( aBackColor );
        FBColor[ iCoord ]  := aBackColor;
      end;
    end;
end;

procedure TGLConsoleRenderer.Resize ( aNewSizeX, aNewSizeY, aLineSpace : DWord );
var iSize   : LongInt;
    iMatrix : TMatrix44;
    iPLoc   : Integer;
    iRect   : TIORect;
    iPart   : TGLVec2f;
begin
  Log( 'Resize %dx%d - %d', [ aNewSizeX, aNewSizeY, aLineSpace] );
  FLineSpace := aLineSpace;
  FSizeX     := aNewSizeX;
  FSizeY     := aNewSizeY;
  iSize      := aNewSizeX * aNewSizeY;

  SetLength( FColor, iSize );
  SetLength( FCells, iSize );

  if FBSupport then
    SetLength( FBColor, iSize );

  SetCursorType( VIO_CURSOR_SMALL );
  MoveCursor( 1, 1 );
  Clear;

  iRect   := GetDeviceArea();

  glBindVertexArray(FDataVAO);
  glBindBuffer( GL_TEXTURE_BUFFER, FDataVBO );
  glBufferData( GL_TEXTURE_BUFFER, FSizeX * FSizeY * sizeof(TGLCell), @(FCells[0]), GL_STREAM_DRAW );
  glBindTexture( GL_TEXTURE_BUFFER, FDataTexture );
  glTexBuffer( GL_TEXTURE_BUFFER, GL_RGBA32UI, FDataVBO );
  glBindBuffer( GL_TEXTURE_BUFFER, 0 );
  glBindTexture( GL_TEXTURE_BUFFER, 0 );
  glBindVertexArray(0);

  FProgram.Bind;
    iPart.Init( SDLIO.GetSizeX / iRect.Dim.X, SDLIO.GetSizeY / iRect.Dim.Y );
    iMatrix := GLCreateOrtho(-iPart.X, iPart.X, 1 - 2*iPart.Y, 1, -1, 1 );

    iPLoc := FProgram.GetUniformLocation('projection');
    glUniformMatrix4fv(iPLoc, 1, GL_FALSE, @iMatrix[0]);

    iPLoc := FProgram.GetUniformLocation('udiffuse');
    glUniform1i(iPLoc, 0);

    iPLoc := FProgram.GetUniformLocation('udata');
    glUniform1i(iPLoc, 1);

    iPLoc := FProgram.GetUniformLocation('uterm_size');
    glUniform2f(iPLoc, Single(FSizeX), Single(FSizeY));

    iPLoc := FProgram.GetUniformLocation('usheet_size');
    glUniform2i( iPLoc, 32, 7 );

    iPLoc := FProgram.GetUniformLocation('usheet_offset');
    glUniform1i( iPLoc, 32 );

    iPLoc := FProgram.GetUniformLocation('uline_space');
    glUniform1i( iPLoc, FLineSpace );

    iPLoc := FProgram.GetUniformLocation('ustretch');
    glUniform1i( iPLoc, Iif( FGlyphStretch, 1, 0 ) );

    FPCursor := FProgram.GetUniformLocation('ucursor');
  FProgram.Unbind;

end;

procedure TGLConsoleRenderer.Update;
begin
  glBindVertexArray(FTriangleVAO);
  FProgram.Bind;
  if (VIO_CON_CURSOR in FCapabilities) and FCurVisible then
  begin
    if (MilliSecondsBetween(Now,FStartTime) div 500) mod 2 = 0 then
      glUniform3i( FPCursor, FCursorX, FCursorY, FCGlyph )
    else
      glUniform3i( FPCursor, -1, -1, 0 );
  end
  else
    glUniform3i( FPCursor, -1, -1, 0 );


  glActiveTexture( GL_TEXTURE0 );
  glBindTexture( GL_TEXTURE_2D, FFont.GLTexture );

  glActiveTexture( GL_TEXTURE1 );
  glBindTexture( GL_TEXTURE_BUFFER, FDataTexture );

  glBindBuffer( GL_TEXTURE_BUFFER, FDataVBO );
  glBufferSubData( GL_TEXTURE_BUFFER, 0, FSizeX * FSizeY * sizeof(TGLCell), @(FCells[0]) );

  glBindBuffer( GL_ARRAY_BUFFER, FTriangleVBO );
  glDrawArrays( GL_TRIANGLES, 0, 3 );
  FProgram.UnBind;
  glBindBuffer( GL_ARRAY_BUFFER, 0);
  glBindTexture( GL_TEXTURE_BUFFER, 0 );
  glBindTexture( GL_TEXTURE_2D, 0 );
  glBindVertexArray(0);
  glActiveTexture( GL_TEXTURE0 );
end;

destructor TGLConsoleRenderer.Destroy;
begin
  glDeleteTextures(1, @FDataTexture );
  glDeleteBuffers(1, @FDataVBO);
  glDeleteVertexArrays(1, @FDataVAO);
  glDeleteVertexArrays(1, @FTriangleVAO);
  FreeAndNil( FFont );
  FreeAndNil( FProgram );
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
    FCells[ aIndex ].glyph := Ord( aChar );
  end
  else
    aFrontColor := 0;
  FColor[ aIndex ] := aFrontColor;
  FCells[ aIndex ].fgcolor := MakeColor( aFrontColor );

  if FBSupport then
  begin
    FBColor[ aIndex ] := aBackColor;
    FCells[ aIndex ].bgcolor := MakeColor( aBackColor );
  end;
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

