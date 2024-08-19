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
  FClearCell     : Word;
  FOutputCMask   : DWord;
  FCursorVisible : Boolean;
  FCX, FCY       : Integer;
end;

implementation

uses vutil, keyboard, video;

const ForeColorMask = $0000000F;
      BackColorMask = $000000F0;

constructor TTextConsoleRenderer.Create ( aCols : Word; aRows : Word; aReqCapabilities : TIOConsoleCapSet ) ;
begin
  Log('Initializing Text Console Renderer...');
  inherited Create( aCols, aRows, aReqCapabilities );

  FCursorType    := VIO_CURSOR_SMALL;
  FCX := -1;
  FCY := -1;
  if VIO_CON_CURSOR in FCapabilities then
  begin
    SetCursorType( VIO_CURSOR_SMALL )
    FCursorVisible := True;
  end
  else
  begin
    video.SetCursorType( crHidden );
    FCursorVisible := False;
  end;

  if VIO_CON_BGCOLOR in FCapabilities then
    FOutputCMask := ForeColorMask;

  FClearCell := Ord(' ')+(LightGray shl 8);
end;

procedure TTextConsoleRenderer.OutputChar ( x, y : Integer; aColor : TIOColor; aChar : char ) ;
var iIndex     : LongInt;
    iValue     : Word;
    iBackColor : TIOColor;
begin
  if aColor = ColorNone then Exit;
  if VIO_CON_BGCOLOR in FCapabilities then
  begin
    iBackColor := (aColor and BackColorMask) shr 4;
    if iBackColor <> 0 then
    begin
      OutputChar( x, y, aColor mod 16, iBackColor, aChar );
      Exit;
    end;
  end;
  iIndex := (x-1)+(y-1)*ScreenWidth;
  if (iIndex < 0) or (iIndex > FSizeX * FSizeY) then Exit;
  iValue := Ord(aChar) + ((aColor and FOutputCMask) shl 8);
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
  if ( FCX <> x ) or ( FCY <> Y ) then
  begin
    if VIO_CON_CURSOR in FCapabilities then
      SetCursorPos(x-1,y-1);
    FCX := x;
    FCY := y;
  end;
end;

procedure TTextConsoleRenderer.ShowCursor;
begin
  if not FCursorVisible then
  begin
    if VIO_CON_CURSOR in FCapabilities then
      SetCursorType( FCursorType );
    FCursorVisible := True;
  end;
end;

procedure TTextConsoleRenderer.HideCursor;
begin
  if FCursorVisible then
  begin
    video.SetCursorType( crHidden );
    FCursorVisible := False;
  end;
end;

procedure TTextConsoleRenderer.SetCursorType ( aType : TIOCursorType ) ;
begin
  if VIO_CON_CURSOR in FCapabilities then
  begin
    if ( FCursorType <> aType ) then
    begin
      case aType of
        VIO_CURSOR_SMALL : video.SetCursorType( crUnderLine );
        VIO_CURSOR_HALF  : video.SetCursorType( crHalfBlock );
        VIO_CURSOR_BLOCK : video.SetCursorType( crBlock );
      end;
      FCursorType := aType;
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
    iValue : Word;
begin
  if aBackColor = ColorNone then
  begin
    for y := y1 to y2 do
      for x := x1 to x2 do
      begin
        iValue := DWord((VideoBuf^[(x-1)+(y-1)*ScreenWidth]) shr 8) shl 8;
        VideoBuf^[(x-1)+(y-1)*ScreenWidth] := TVideoCell(iValue);
      end;
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
  Result := [ VIO_CON_BGCOLOR, VIO_CON_CURSOR ];
end;


end.

