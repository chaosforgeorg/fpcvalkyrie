{$INCLUDE valkyrie.inc}
unit vcursesconsole;
interface
uses Classes, SysUtils, viotypes, vioevent, vioconsole;

type TCursesArray = array of array of DWord;

type TCursesConsoleRenderer = class( TIOConsoleRenderer )
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
  procedure ClearArray( var A : TCursesArray; aClear : DWord );
private
  FClearCell    : Word;
  FOutputCMask  : DWord;
  FCursesPos    : TPoint;
  FCursesArray  : TCursesArray;
  FNewArray     : TCursesArray;
  FUpdateNeeded : Boolean;
end;

implementation

uses vutil, nCurses, vcursesio;

const ColorMask     = $000000FF;
      ForeColorMask = $0000000F;

procedure TCursesConsoleRenderer.ClearArray( var A : TCursesArray; aClear : DWord );
var y : Integer;
begin
  for y := Low(A) to High(A) do
    FillDWord( A[y][0], FSizeX, aClear );
end;

constructor TCursesConsoleRenderer.Create ( aCols : Word; aRows : Word; aReqCapabilities : TIOConsoleCapSet ) ;
begin
  Log('Initializing Curses Console Renderer...');
  inherited Create( aCols, aRows, aReqCapabilities );
//  if VIO_CON_CURSOR in FCapabilities then
//    SetCursorType( VIO_CURSOR_SMALL )
//  else
//    video.SetCursorType( crHidden );
  FOutputCMask := ColorMask;
  FClearCell := Ord(' ')+(LightGray shl 8);
  FCursesPos.x := 1;
  FCursesPos.y := 1;
  FUpdateNeeded := True;
  SetLength( FCursesArray, FSizeY, FSizeX );
  ClearArray( FCursesArray, 0 );
  SetLength( FNewArray, FSizeY, FSizeX );
  ClearArray( FNewArray, 0 );
  Log('Initialized.');
end;

procedure TCursesConsoleRenderer.OutputChar ( x, y : Integer; aColor : TIOColor; aChar : char ) ;
var iValue : Word;
begin
  if aColor = ColorNone then Exit;
  if aColor = Black then
  begin
    aChar  := ' ';
    aColor := LightGray;
  end;
  iValue := Ord(aChar) + ((aColor and FOutputCMask) shl 8);
  if ( FNewArray[y-1][x-1] <> iValue ) then
  begin
    FNewArray[y-1][x-1] := iValue;
    FUpdateNeeded := True;
  end;
end;

procedure TCursesConsoleRenderer.OutputChar ( x, y : Integer; aFrontColor, aBackColor : TIOColor; aChar : char ) ;
var iValue : Word;
begin
  if aBackColor = ColorNone then
  begin
    OutputChar( x, y, aFrontColor, aChar );
    Exit;
  end;
  if aFrontColor = ColorNone then
  begin
    FNewArray[y-1][x-1] :=  Ord(' ') + (aBackColor and ForeColorMask) shl 12;
    Exit;
  end;
  if ( aFrontColor = Black ) and ( aBackColor = Black ) then
  begin
    aChar       := ' ';
    aFrontColor := LightGray;
  end;

  iValue := Ord(aChar) + (aFrontColor and ForeColorMask) shl 8;
  if VIO_CON_BGCOLOR in FCapabilities then
    iValue += (aBackColor and ForeColorMask) shl 12;
  if ( FNewArray[y-1][x-1] <> iValue ) then
  begin
    FNewArray[y-1][x-1] := iValue;
    FUpdateNeeded := True;
  end;
end;

function TCursesConsoleRenderer.GetChar ( x, y : Integer ) : Char;
begin
  Exit( Chr( FCursesArray[y-1][x-1] mod 256 ) );
end;

function TCursesConsoleRenderer.GetColor ( x, y : Integer ) : TIOColor;
begin
  Exit( (FCursesArray[y-1][x-1] div 256) mod 16 );
end;

function TCursesConsoleRenderer.GetBackColor ( x, y : Integer ) : TIOColor;
begin
  Exit( (FCursesArray[y-1][x-1] div 256) div 16 );
end;

procedure TCursesConsoleRenderer.MoveCursor ( x, y : Integer ) ;
begin
  if VIO_CON_CURSOR in FCapabilities then
  begin
    FCursesPos.X := x;
    FCursesPos.Y := x;
    nCurses.move( FCursesPos.y-1, FCursesPos.x-1 );
  end;
end;

procedure TCursesConsoleRenderer.ShowCursor;
begin
  //if VIO_CON_CURSOR in FCapabilities then
  //  SetCursorType( FCursorType );
end;

procedure TCursesConsoleRenderer.HideCursor;
begin
  //video.SetCursorType( crHidden );
end;

procedure TCursesConsoleRenderer.SetCursorType ( aType : TIOCursorType ) ;
begin
//  if VIO_CON_CURSOR in FCapabilities then
//  begin
//    FCursorType  := aType;
//    case aType of
//      VIO_CURSOR_SMALL : video.SetCursorType( crUnderLine );
//      VIO_CURSOR_HALF  : video.SetCursorType( crHalfBlock );
//      VIO_CURSOR_BLOCK : video.SetCursorType( crBlock );
//    end;
//  end;
end;

procedure TCursesConsoleRenderer.Update;
var iRefresh : Boolean;
    x,y      : Integer;
    iValue   : DWord;
    iChar    : Char;
    iFr, iBk : TIOColor; 
    iOut     : QWord;
begin
  iRefresh := False;
  if FUpdateNeeded then
  begin
    for y := 0 to FSizeY-1 do
      for x := 0 to FSizeX-1 do
        if FNewArray[y][x] <> FCursesArray[y][x] then
        begin
          iValue := FNewArray[y][x];
          iChar  := Char( iValue mod 256 );
          iFr    := ( iValue div 256 ) mod 16;
          if iFr > 0 then
          begin
            iBk    := ( iValue div 256 ) div 16;
            if iBk > 7 then 
              iBk := iBk - 8;

            iOut := QWord(iChar);
            if Byte(iChar) > 127 then
            begin
              iOut := iOut - 128;
              iOut := iOut or A_ALTCHARSET;
            end;
            if iFr > 7 then
              iOut := iOut or COLOR_PAIR( iFr - 8 + iBk*8 ) or A_BOLD
            else
              iOut := iOut or COLOR_PAIR( iFr + iBk*8 );

            nCurses.mvaddch( y, x, iOut );
            iRefresh := True;
          end;
          FCursesArray[y][x] := iValue;
        end;
    if iRefresh then
      nCurses.refresh();
    nCurses.move( FCursesPos.y-1, FCursesPos.x-1 );
    FUpdateNeeded := false;
  end;
end;

procedure TCursesConsoleRenderer.Clear;
begin
  ClearArray( FNewArray, FClearCell );
  FUpdateNeeded := True;
end;

procedure TCursesConsoleRenderer.ClearRect ( x1, y1, x2, y2 : Integer; aBackColor : TIOColor ) ;
var x,y    : Word;
    iColor : Word;
begin
  iColor := Ord(' ')+LightGray shl 8;
  if VIO_CON_BGCOLOR in FCapabilities then
    iColor += (aBackColor and ForeColorMask) shl 12;
  for y := y1 to y2 do
    for x := x1 to x2 do
      FNewArray[y-1][x-1] := iColor;
  FUpdateNeeded := True;
end;

function TCursesConsoleRenderer.GetDeviceArea : TIORect;
begin
  GetDeviceArea.Pos := PointZero;
  GetDeviceArea.Dim := Point( FSizeX, FSizeY );
end;

function TCursesConsoleRenderer.GetSupportedCapabilities : TIOConsoleCapSet;
begin
  Result := [ VIO_CON_BGCOLOR, VIO_CON_CURSOR, VIO_CON_BGSTABLE ];
end;


end.

