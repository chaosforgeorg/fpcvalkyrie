{$INCLUDE valkyrie.inc}
unit viotypes;
interface
uses Classes, SysUtils, vutil, vnode, vioevent, vgltypes, vgenerics;

type TIOColor         = DWord;
type TIOCursorType    = ( VIO_CURSOR_SMALL, VIO_CURSOR_HALF, VIO_CURSOR_BLOCK );
type TIORect          = TRectangle;
type TIOPoint         = TPoint;
type TIOStringArray   = specialize TGArray<AnsiString>;
type TIOStringBuffer  = specialize TGRingBuffer<AnsiString>;

// TEMPORARY, move to vtextmap later
type TTextExplosionArray = array of record Color : TIOColor; Time : DWord; end;

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

// Architectural boundary: owns device mechanics and interrupt registration.
// Renderer, path, and game policy belong to callers.
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
  function CaptureScreen( const aFileName : AnsiString ) : Boolean; virtual;
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

type TIOWindowMode = ( VIO_WINDOWED, VIO_FULLSCREEN );

type TIODisplayMetrics = record
  Mode         : TIOWindowMode;
  Maximized    : Boolean;
  Minimized    : Boolean;
  PixelSize    : TIOPoint; // drawable; layout and mouse coordinates use this space
  PixelDensity : Single;
end;

type TIODisplayInfo = record
  Name    : AnsiString; // as reported by the platform
  Ordinal : Integer;    // 0-based among connected displays with the same name
  Bounds  : TIORect;
  Usable  : TIORect;
  Primary : Boolean;
end;

type TIODisplayInfoArray = specialize TGArray< TIODisplayInfo >;

// Window and display mechanics for the display controller. Holds no policy:
// presets, persistence and deciding when to apply belong to the controller.
// Window sizes and display rectangles are logical; metrics are in pixels.
type TIOWindowDriver = class( TIODriver )
  constructor Create;
  destructor Destroy; override;
  // Changes only the fullscreen state; the floating geometry is kept.
  function SetWindowMode( aMode : TIOWindowMode ) : Boolean; virtual; abstract;
  function GetWindowSize( out aSize : TIOPoint ) : Boolean; virtual; abstract;
  // Fails while fullscreen, where the platform would discard the size.
  function SetWindowSize( const aSize : TIOPoint; aCenter : Boolean ) : Boolean; virtual; abstract;
  // Usable area of the display the window is on.
  function GetUsableArea( out aArea : TIORect ) : Boolean; virtual; abstract;
  // Index into Displays.
  function GetWindowDisplay( out aIndex : Integer ) : Boolean; virtual; abstract;
  // Centers the window on the display, clamping a windowed size to its usable
  // area; a fullscreen window moves and stays fullscreen.
  function MoveToDisplay( aIndex : Integer ) : Boolean; virtual; abstract;
  // Reads the actual state and clears DisplayDirty.
  function ReadMetrics( out aMetrics : TIODisplayMetrics ) : Boolean; virtual; abstract;
  // The minimum is kept in pixels and reapplied when the pixel density changes.
  function SetWindowLimits( const aMinimumPixels : TIOPoint; aResizable : Boolean ) : Boolean; virtual; abstract;
  // Waits for pending window changes; only for launch placement.
  function SynchronizeWindow : Boolean; virtual; abstract;
  // True once per fullscreen toggle key press received while DisplayManaged.
  function TakeToggleRequest : Boolean;
protected
  procedure RefreshDisplays; virtual; abstract;
  function GetDisplays : TIODisplayInfoArray;
  procedure SetDisplayManaged( aValue : Boolean );
protected
  FDisplays       : TIODisplayInfoArray;
  FDisplaysDirty  : Boolean;
  FDisplayDirty   : Boolean;
  FToggleRequest  : Boolean;
  FDisplayManaged : Boolean;
public
  // Connected displays in platform order. Borrowed; rebuilt after displays
  // are added, removed or changed, so do not keep it across frames.
  property Displays       : TIODisplayInfoArray read GetDisplays;
  // Set when window or display events arrive; cleared by ReadMetrics.
  property DisplayDirty   : Boolean read FDisplayDirty;
  // Set by the display controller once it owns the window. Until then the
  // legacy video-mode path keeps logical mouse coordinates, no per-frame
  // viewport and no fullscreen toggle key.
  property DisplayManaged : Boolean read FDisplayManaged write SetDisplayManaged;
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
function TextFileToIOStringArray( const aPath : AnsiString ) : TIOStringArray;
// Persistent display identity, "name#ordinal". Platform display IDs and list
// indices do not survive a reboot or a re-plug.
function IODisplayIdentity( const aInfo : TIODisplayInfo ) : AnsiString;

implementation

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

function TIODriver.CaptureScreen( const aFileName : AnsiString ) : Boolean;
begin
  Result := False;
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

{ TIOWindowDriver }

constructor TIOWindowDriver.Create;
begin
  inherited Create;
  FDisplays       := TIODisplayInfoArray.Create;
  FDisplaysDirty  := True;
  FDisplayDirty   := True;
  FToggleRequest  := False;
  FDisplayManaged := False;
end;

destructor TIOWindowDriver.Destroy;
begin
  FreeAndNil( FDisplays );
  inherited Destroy;
end;

function TIOWindowDriver.TakeToggleRequest : Boolean;
begin
  Result := FToggleRequest;
  FToggleRequest := False;
end;

function TIOWindowDriver.GetDisplays : TIODisplayInfoArray;
begin
  if FDisplaysDirty then
  begin
    FDisplaysDirty := False;
    RefreshDisplays;
  end;
  Exit( FDisplays );
end;

procedure TIOWindowDriver.SetDisplayManaged( aValue : Boolean );
begin
  FDisplayManaged := aValue;
  FToggleRequest  := False;
  FDisplayDirty   := True;
end;

function IODisplayIdentity( const aInfo : TIODisplayInfo ) : AnsiString;
begin
  Exit( aInfo.Name + '#' + IntToStr( aInfo.Ordinal ) );
end;

function TextFileToIOStringArray ( const aPath : AnsiString ) : TIOStringArray;
var iLines : TStringList;
    i      : Integer;
begin
  Result := TIOStringArray.Create;
  iLines := TStringList.Create;
  try
    iLines.LoadFromFile( aPath );
    for i := 0 to iLines.Count - 1 do
      Result.Push( iLines[i] );
  finally
    iLines.Free;
  end;
end;

end.

