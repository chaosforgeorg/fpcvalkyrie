{$INCLUDE valkyrie.inc}
unit vtextconsole;
interface
uses Classes, SysUtils, viotypes, vioevent, vioconsole;

type TTextConsoleRenderer = class( TIOConsoleRenderer )
  constructor Create( aCols : Word = 80; aRows : Word = 25; aReqCapabilities : TIOConsoleCapSet = [VIO_CON_BGCOLOR, VIO_CON_CURSOR] );
  procedure OutputChar( x,y : Integer; aColor : TIOColor; aChar : char ); override;
  procedure OutputChar( x,y : Integer; aFrontColor, aBackColor : TIOColor; aChar : char ); override;
  function GetChar( x,y : Integer ) : Char; override;
  function GetColor( x,y : Integer ) : TIOColor; override;
  function GetBackColor( x,y : Integer ) : TIOColor; override;
  procedure MoveCursor( x,y : Integer ); override;
  procedure ShowCursor; override;
  procedure HideCursor; override;
  procedure SetCursorType( aType : TIOCursorType ); override;
  procedure Update; override;
  procedure Clear; override;
  procedure ClearRect(x1,y1,x2,y2 : Integer; aBackColor : TIOColor = 0 ); override;
  function GetDeviceArea : TIORect; override;
  function GetSupportedCapabilities : TIOConsoleCapSet; override;
private
  FClearCell   : Word;
  FOutputCMask : DWord;
end;

implementation

uses vutil, keyboard, video;

const ColorMask     = $000000FF;
      ForeColorMask = $0000000F;

constructor TTextConsoleRenderer.Create ( aCols : Word; aRows : Word; aReqCapabilities : TIOConsoleCapSet ) ;
begin
  Log('Initializing Text Console Renderer...');
  inherited Create( aCols, aRows, aReqCapabilities );

  if VIO_CON_CURSOR in FCapabilities then
    SetCursorType( VIO_CURSOR_SMALL )
  else
    video.SetCursorType( crHidden );

  if (VIO_CON_BGCOLOR in FCapabilities) and (not (VIO_CON_BGSTABLE in FCapabilities))
    then FOutputCMask := ColorMask
    else FOutputCMask := ForeColorMask;

  FClearCell := Ord(' ')+(LightGray shl 8);
end;

procedure TTextConsoleRenderer.OutputChar ( x, y : Integer; aColor : TIOColor; aChar : char ) ;
var iIndex : LongInt;
    iValue : Word;
begin
  if aColor = ColorNone then Exit;
  iIndex := (x-1)+(y-1)*ScreenWidth;
  if (iIndex < 0) or (iIndex > FSizeX * FSizeY) then Exit;
  iValue := Ord(aChar) + ((aColor and FOutputCMask) shl 8);
  if VIO_CON_BGSTABLE in FCapabilities then
    iValue += (VideoBuf^[iIndex] shr 12) shl 12;
  VideoBuf^[iIndex] := iValue;
end;

procedure TTextConsoleRenderer.OutputChar ( x, y : Integer; aFrontColor, aBackColor : TIOColor; aChar : char ) ;
var iIndex : LongInt;
    iValue : Word;
begin
  if aBackColor = ColorNone then begin OutputChar( x, y, aFrontColor, aChar ); Exit; end;
  iIndex := (x-1)+(y-1)*ScreenWidth;
  if aFrontColor = ColorNone then begin VideoBuf^[iIndex] := (aBackColor and ForeColorMask) shl 12; Exit; end;

  iValue := Ord(aChar) + (aFrontColor and ForeColorMask) shl 8;
  if VIO_CON_BGCOLOR in FCapabilities then
    iValue += (aBackColor and ForeColorMask) shl 12;
  VideoBuf^[iIndex] := iValue;
end;

function TTextConsoleRenderer.GetChar ( x, y : Integer ) : Char;
begin
  Exit( Chr( VideoBuf^[(x-1)+(y-1)*ScreenWidth] mod 256 ) );
end;

function TTextConsoleRenderer.GetColor ( x, y : Integer ) : TIOColor;
begin
  Exit( (VideoBuf^[(x-1)+(y-1)*ScreenWidth] div 256) mod 16 );
end;

function TTextConsoleRenderer.GetBackColor ( x, y : Integer ) : TIOColor;
begin
  Exit( (VideoBuf^[(x-1)+(y-1)*ScreenWidth] div 256) div 16 );
end;

procedure TTextConsoleRenderer.MoveCursor ( x, y : Integer ) ;
begin
  if VIO_CON_CURSOR in FCapabilities then
    SetCursorPos(x-1,y-1);
end;

procedure TTextConsoleRenderer.ShowCursor;
begin
  if VIO_CON_CURSOR in FCapabilities then
    SetCursorType( FCursorType );
end;

procedure TTextConsoleRenderer.HideCursor;
begin
  video.SetCursorType( crHidden );
end;

procedure TTextConsoleRenderer.SetCursorType ( aType : TIOCursorType ) ;
begin
  if VIO_CON_CURSOR in FCapabilities then
  begin
    FCursorType  := aType;
    case aType of
      VIO_CURSOR_SMALL : video.SetCursorType( crUnderLine );
      VIO_CURSOR_HALF  : video.SetCursorType( crHalfBlock );
      VIO_CURSOR_BLOCK : video.SetCursorType( crBlock );
    end;
  end;
end;

procedure TTextConsoleRenderer.Update;
begin

end;

procedure TTextConsoleRenderer.Clear;
begin
  FillWord(VideoBuf^,ScreenWidth*ScreenHeight,FClearCell);
end;

procedure TTextConsoleRenderer.ClearRect ( x1, y1, x2, y2 : Integer; aBackColor : TIOColor ) ;
var x,y    : Word;
    iColor : Word;
begin
  if aBackColor = ColorNone then
  begin
    for y := y1 to y2 do
      for x := x1 to x2 do
        VideoBuf^[(x-1)+(y-1)*ScreenWidth] := ((VideoBuf^[(x-1)+(y-1)*ScreenWidth]) shr 8) shl 8;
    Exit;
  end;
  iColor := Ord(' ')+LightGray shl 8;
  if VIO_CON_BGCOLOR in FCapabilities then
    iColor += (aBackColor and ForeColorMask) shl 12;
  for y := y1 to y2 do
    for x := x1 to x2 do
      VideoBuf^[(x-1)+(y-1)*ScreenWidth] := iColor;
end;

function TTextConsoleRenderer.GetDeviceArea : TIORect;
begin
  GetDeviceArea.Pos := PointZero;
  GetDeviceArea.Dim := Point( FSizeX, FSizeY );
end;

function TTextConsoleRenderer.GetSupportedCapabilities : TIOConsoleCapSet;
begin
  Result := [ VIO_CON_BGCOLOR, VIO_CON_CURSOR, VIO_CON_BGSTABLE ];
end;


end.

