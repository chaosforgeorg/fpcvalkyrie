{$INCLUDE valkyrie.inc}
unit viotypes;
interface
uses Classes, SysUtils, vutil, vnode, vioevent, vgltypes, vgenerics;

type TIOColor         = DWord;
type TIOCursorType    = ( VIO_CURSOR_SMALL, VIO_CURSOR_HALF, VIO_CURSOR_BLOCK );
type TIORect          = TRectangle;
type TIOPoint         = TPoint;

type TIOLayer = class
  procedure Update( aDTime : Integer ); virtual; abstract;
  function IsFinished : Boolean; virtual; abstract;
  function IsModal : Boolean; virtual;
  function HandleEvent( const aEvent : TIOEvent ) : Boolean; virtual;
  function HandleInput( aInput : Integer ) : Boolean; virtual;
end;

type TIOLayerStack = specialize TGArray<TIOLayer>;

{ TIOGylph }

type TIOGylph = packed object
  ASCII : Char;
  Color : TIOColor;
  procedure Init( aASCII : Char; aColor : TIOColor );
end;

type TIODisplayMode = record
   Index   : Integer;
   Width   : Integer;
   Height  : Integer;
   Refresh : Integer;
   Name    : AnsiString;
 end;

type TIODisplayModeArray = specialize TGArray< TIODisplayMode >;

type TIOTerminateEventHandler = function : Boolean of object;
type TIOInterrupt = function( aEvent : TIOEvent ) : Boolean of object;
type TIOInterrupts = array[0..IOKeyCodeMax] of TIOInterrupt;

type TIODriver = class( TVObject )
  constructor Create;
  function PollEvent( out aEvent : TIOEvent ) : Boolean; virtual; abstract;
  function PeekEvent( out aEvent : TIOEvent ) : Boolean; virtual; abstract;
  function EventPending : Boolean; virtual; abstract;
  procedure SetEventMask( aMask : TIOEventType ); virtual; abstract;
  procedure Sleep( Milliseconds : DWord ); virtual; abstract;
  procedure PreUpdate; virtual; abstract;
  procedure PostUpdate; virtual; abstract;
  function GetMs : DWord; virtual; abstract;
  function GetSizeX : DWord; virtual; abstract;
  function GetSizeY : DWord; virtual; abstract;
  function GetMousePos( out aResult : TIOPoint) : Boolean; virtual; abstract;
  function GetMouseButtonState( out aResult : TIOMouseButtonSet) : Boolean; virtual; abstract;
  function GetModKeyState : TIOModKeySet; virtual; abstract;
  procedure SetTitle( const aLongTitle : AnsiString; const aShortTitle : AnsiString = '' ); virtual; abstract;
  procedure ClearInterrupts;
  procedure RegisterInterrupt( aCode : TIOKeyCode; aInterrupt : TIOInterrupt );
  procedure StartTextInput; virtual;
  procedure StopTextInput; virtual;
  procedure SetClipboard( const aValue : Ansistring ); virtual;
  function GetClipboard : Ansistring; virtual;
  function Rumble( aLow, aHigh : Word; aDuration : DWord ) : Boolean; virtual;
protected
  FOnQuit        : TIOInterrupt;
  FInterrupts    : TIOInterrupts;
  FDisplayModes  : TIODisplayModeArray;
  FFakeClipboard : Ansistring;
public
  property OnQuitEvent  : TIOInterrupt        write FOnQuit;
  property DisplayModes : TIODisplayModeArray read  FDisplayModes default nil;
end;

type IIOElement = interface['viotypes.iioelement']
  procedure OnUpdate( aTime : DWord );
  function OnEvent( const event : TIOEvent ) : Boolean;
end;

//type TIODeviceCap     = ( VIO_DEV_OPENGL, VIO_DEV_MOUSE, VIO_DEV_KEYUP );
//type TIODeviceCapSet  = set of TIODeviceCap;

type EIOException = class(Exception);
type EIOSupportException = class(EIOException);

const
  Black        = 0;    DarkGray     = 8;
  Blue         = 1;    LightBlue    = 9;
  Green        = 2;    LightGreen   = 10;
  Cyan         = 3;    LightCyan    = 11;
  Red          = 4;    LightRed     = 12;
  Magenta      = 5;    LightMagenta = 13;
  Brown        = 6;    Yellow       = 14;
  LightGray    = 7;    White        = 15;
  ColorNone    = $EFFFFFFF;

const IOColors : array[0..15] of TIOColor = (
  $000000FF,
  $00009FFF,
  $009F00FF,
  $009F9FFF,
  $9F0000FF,
  $9F009FFF,
  $9F9F00FF,
  $CCCCCCFF,

  $808080FF,
  $0000FFFF,
  $00FF00FF,
  $00FFFFFF,
  $FF0000FF,
  $FF00FFFF,
  $FFFF00FF,
  $FFFFFFFF
);


const GLFloatColors : array[0..15] of TGLFloatColor = (
      ( Data : ( 0.0, 0.0, 0.0 ) ),
      ( Data : ( 0.0, 0.0, 0.6 ) ),
      ( Data : ( 0.0, 0.6, 0.0 ) ),
      ( Data : ( 0.0, 0.6, 0.6 ) ),
      ( Data : ( 0.6, 0.0, 0.0 ) ),
      ( Data : ( 0.6, 0.0, 0.6 ) ),
      ( Data : ( 0.6, 0.6, 0.0 ) ),
      ( Data : ( 0.8, 0.8, 0.8 ) ),
      ( Data : ( 0.5, 0.5, 0.5 ) ),
      ( Data : ( 0.0, 0.0, 1.0 ) ),
      ( Data : ( 0.0, 1.0, 0.0 ) ),
      ( Data : ( 0.0, 1.0, 1.0 ) ),
      ( Data : ( 1.0, 0.0, 0.0 ) ),
      ( Data : ( 1.0, 0.0, 1.0 ) ),
      ( Data : ( 1.0, 1.0, 0.0 ) ),
      ( Data : ( 1.0, 1.0, 1.0 ) )
      );

const GLFloatColors4 : array[0..15] of TGLFloatColor4 = (
      ( Data : ( 0.0, 0.0, 0.0, 1.0 ) ),
      ( Data : ( 0.0, 0.0, 0.6, 1.0 ) ),
      ( Data : ( 0.0, 0.6, 0.0, 1.0 ) ),
      ( Data : ( 0.0, 0.6, 0.6, 1.0 ) ),
      ( Data : ( 0.6, 0.0, 0.0, 1.0 ) ),
      ( Data : ( 0.6, 0.0, 0.6, 1.0 ) ),
      ( Data : ( 0.6, 0.6, 0.0, 1.0 ) ),
      ( Data : ( 0.8, 0.8, 0.8, 1.0 ) ),
      ( Data : ( 0.5, 0.5, 0.5, 1.0 ) ),
      ( Data : ( 0.0, 0.0, 1.0, 1.0 ) ),
      ( Data : ( 0.0, 1.0, 0.0, 1.0 ) ),
      ( Data : ( 0.0, 1.0, 1.0, 1.0 ) ),
      ( Data : ( 1.0, 0.0, 0.0, 1.0 ) ),
      ( Data : ( 1.0, 0.0, 1.0, 1.0 ) ),
      ( Data : ( 1.0, 1.0, 0.0, 1.0 ) ),
      ( Data : ( 1.0, 1.0, 1.0, 1.0 ) )
      );


const ColorNames : array[0..15] of AnsiString =
        ('BLACK',     'BLUE',     'GREEN',    'CYAN',        'RED',
         'MAGENTA',   'BROWN',    'LIGHTGRAY','DARKGRAY',    'LIGHTBLUE',
         'LIGHTGREEN','LIGHTCYAN','LIGHTRED', 'LIGHTMAGENTA','YELLOW',
         'WHITE');

const BBColorNames : array[0..15] of AnsiString =
        ('#333',   'navy',  'green',  'teal',   'maroon',
         'purple', 'olive', 'silver', 'gray',   'blue',
         'lime',   'aqua',  'red',    'fuchsia','yellow',
         'white');

const ColorCodes : array[0..15] of Char =
        ('D','b','g','c','r','v','n','l','d','B','G','C','R','V','y','L');

function IOGylph( aChar : Char; aColor : TIOColor ) : TIOGylph;
function IOColor( aR, aG, aB : Byte; aA : Byte = 255 ) : TIOColor;

implementation

function TIOLayer.IsModal : Boolean;
begin
  Exit( False );
end;

function TIOLayer.HandleEvent( const aEvent : TIOEvent ) : Boolean;
begin
  Exit( IsModal );
end;

function TIOLayer.HandleInput( aInput : Integer ) : Boolean;
begin
  Exit( False );
end;

function IOGylph( aChar : Char; aColor : TIOColor ) : TIOGylph;
begin
  IOGylph.ASCII := aChar;
  IOGylph.Color := aColor;
end;

function IOColor(aR, aG, aB: Byte; aA: Byte): TIOColor;
begin
  IOColor := aA + aB shl 8 + aG shl 16 + aR shl 24;
end;

{ TIOGylph }

procedure TIOGylph.Init(aASCII: Char; aColor: TIOColor);
begin
  ASCII := aASCII;
  Color := aColor;
end;

{ TIODriver }

constructor TIODriver.Create;
begin
  FFakeClipboard := '';
  ClearInterrupts;
end;

procedure TIODriver.ClearInterrupts;
var iCount : Integer;
begin
  FOnQuit := nil;
  for iCount := Low(FInterrupts) to High(FInterrupts) do FInterrupts[iCount] := nil;
end;

procedure TIODriver.RegisterInterrupt ( aCode : TIOKeyCode; aInterrupt : TIOInterrupt );
begin
  FInterrupts[aCode] := aInterrupt;
end;

procedure TIODriver.StartTextInput;
begin
end;

procedure TIODriver.StopTextInput;
begin
end;

procedure TIODriver.SetClipboard( const aValue : Ansistring );
begin
  FFakeClipboard := aValue;
end;

function TIODriver.GetClipboard : Ansistring;
begin
  Exit( FFakeClipboard );
end;

function TIODriver.Rumble( aLow, aHigh : Word; aDuration : DWord ) : Boolean;
begin
  Exit( False );
end;



end.

