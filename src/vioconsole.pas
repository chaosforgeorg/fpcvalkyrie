{$INCLUDE valkyrie.inc}
unit vioconsole;
interface
uses Classes, SysUtils, vnode, vioevent, viotypes;

type TIOConsoleCap    = ( VIO_CON_BGCOLOR, VIO_CON_CURSOR, VIO_CON_EXTCOLOR, VIO_CON_BGSTABLE, VIO_CON_EXTOUT );
type TIOConsoleCapSet = set of TIOConsoleCap;
const IOConsoleCapNames  : array[TIOConsoleCap] of AnsiString = ( 'CON_BGCOLOR', 'CON_CURSOR', 'CON_EXTCOLOR', 'CON_BGSTABLE', 'CON_EXTOUT' );

function IOConsoleCapToString( aConsoleCap : TIOConsoleCap ) : AnsiString;
function IOConsoleCapSetToString( aConsoleCaps : TIOConsoleCapSet ) : AnsiString;

type

{ TIOConsoleRenderer }

 TIOConsoleRenderer = class( TVObject )
  constructor Create( aSizeX, aSizeY : DWord; aReqCapabilities : TIOConsoleCapSet );
  procedure OutputChar( x,y : Integer; aColor : TIOColor; chr : char ); virtual; abstract;
  procedure OutputChar( x,y : Integer; aFrontColor, aBackColor : TIOColor; chr : char ); virtual; abstract;
  procedure OutputCharEx( x, y, ox, oy : Integer; aFont: byte; aColor : TIOColor; chr : char ); virtual;
  procedure OutputCharEx( x, y, ox, oy : Integer; aFont: byte; aFrontColor, aBackColor : TIOColor; chr : char ); virtual;
  function GetChar( x,y : Integer ) : Char; virtual; abstract;
  function GetColor( x,y : Integer ) : TIOColor; virtual; abstract;
  function GetBackColor( x,y : Integer ) : TIOColor; virtual; abstract;
  procedure MoveCursor( x, y : Integer ); virtual; abstract;
  procedure ShowCursor; virtual; abstract;
  procedure HideCursor; virtual; abstract;
  procedure SetCursorType( aType : TIOCursorType ); virtual;
  function GetCursorType() : TIOCursorType;
  procedure Update; virtual; abstract;
  procedure Clear; virtual; abstract;
  procedure ClearRect(x1,y1,x2,y2 : Integer; aBackColor : TIOColor = 0 ); virtual; abstract;
  function GetDeviceArea : TIORect; virtual; abstract;
  function GetSupportedCapabilities : TIOConsoleCapSet; virtual; abstract;
protected
  FCapabilities : TIOConsoleCapSet;
  FSizeX        : LongInt;
  FSizeY        : LongInt;
  FCursorType   : TIOCursorType;
public
  property Capabilities : TIOConsoleCapSet read FCapabilities;
  property SizeX        : LongInt          read FSizeX;
  property SizeY        : LongInt          read FSizeY;
end;

implementation

uses math;

function IOConsoleCapToString ( aConsoleCap : TIOConsoleCap ) : AnsiString;
begin
  Exit( IOConsoleCapNames[ aConsoleCap ] );
end;

function IOConsoleCapSetToString ( aConsoleCaps : TIOConsoleCapSet ) : AnsiString;
var iConsoleCap : TIOConsoleCap;
begin
  Result := ' ';
  for iConsoleCap in aConsoleCaps do
    Result += IOConsoleCapToString( iConsoleCap )+' ';
end;

{ TIOConsoleRenderer }

constructor TIOConsoleRenderer.Create ( aSizeX, aSizeY : DWord; aReqCapabilities : TIOConsoleCapSet ) ;
begin
  inherited Create;
  Log('Requesting capabilities ['+IOConsoleCapSetToString( aReqCapabilities )+']...');
  FCapabilities := aReqCapabilities;
  if aReqCapabilities - GetSupportedCapabilities <> [] then
    raise EIOSupportException.Create( ClassName+' : Capabilities ['+IOConsoleCapSetToString( aReqCapabilities - GetSupportedCapabilities )+'] not supported!' );

  if (VIO_CON_BGSTABLE in FCapabilities) and (not (VIO_CON_BGCOLOR in FCapabilities)) then
    raise EIOSupportException.Create( ClassName+' : Capability '+IOConsoleCapToString( VIO_CON_BGSTABLE )+' requested without '+IOConsoleCapToString( VIO_CON_BGCOLOR )+'!');

  FSizeX := aSizeX;
  FSizeY := aSizeY;
end;

procedure TIOConsoleRenderer.OutputCharEx(x, y, ox, oy: Integer; aFont: byte;
  aColor: TIOColor; chr: char);
begin
  OutputChar(x, y, aColor, chr);
end;

procedure TIOConsoleRenderer.OutputCharEx(x, y, ox, oy: Integer; aFont: byte;
  aFrontColor, aBackColor: TIOColor; chr: char);
begin
    OutputChar(x, y, aFrontColor, aBackColor, chr);
end;

procedure TIOConsoleRenderer.SetCursorType ( aType : TIOCursorType ) ;
begin
  FCursorType := aType;
end;

function TIOConsoleRenderer.GetCursorType : TIOCursorType;
begin
  Exit( FCursorType );
end;

end.

