{$INCLUDE valkyrie.inc}
unit vio;
interface
uses Classes, SysUtils, vsystem, vgenerics,
     vioevent, viopadstate, viotypes, vtigconsole, vioconsole, vbindings;

// Architectural boundary: owns driver, console, layers, and binding contexts.
// Device mechanics, renderer implementation, path, and game policy belong to callers.
type TIO = class( TSystem )
  constructor Create( aIODriver : TIODriver; aConsole : TIOConsoleRenderer  ); reintroduce;
  procedure Initialize( aConsole : TIOConsoleRenderer );
  procedure PreUpdate; virtual;
  procedure FullUpdate; virtual;
  procedure PostUpdate; virtual;
  procedure Clear; virtual;
  procedure Update( aMSec : DWord ); virtual;
  procedure Delay( aTime : Integer );
  procedure ClearEventBuffer;
  function OnEvent( const aEvent : TIOEvent ) : Boolean; virtual;
  function HandleEvents : Boolean; virtual;
  destructor Destroy; override;
  procedure RegisterDebugConsole( aKey : TIOKeyCode );
  function PushLayer( aLayer : TIOLayer ) : TIOLayer; virtual;
  function IsTopLayer( aLayer : TIOLayer ) : Boolean;
  function IsModal : Boolean;
  procedure WaitForLayer;
  function EventToUIInput( const aEvent : TIOEvent ) : Integer; virtual;
  function DeviceCoordToConsoleCoord( aCoord : TIOPoint ) : TIOPoint; virtual;
  function ConsoleCoordToDeviceCoord( aCoord : TIOPoint ) : TIOPoint; virtual;
  function CaptureScreen( const aFileName : AnsiString ) : Boolean;
  function SaveConsoleTextDump( const aFileName : AnsiString ) : Boolean;
protected
  function HandleInput( aInput : Integer ) : Boolean;
  function ConsoleCallback( aEvent : TIOEvent ) : Boolean;
  procedure ClearFinishedLayers;
protected
  FIODriver       : TIODriver;
  FConsole        : TIOConsoleRenderer;
  FTIGConsoleView : TTIGConsoleView;
  FLayers         : TIOLayerStack;
  FPadState       : TIOPadState;
  FBindings       : TBindings;
  FUIBindings     : TBindingContext;
  FLastUpdate     : DWord;

  FMouseLast      : TIOPoint;
  FMouse          : TIOPoint;
public
  property Driver     : TIODriver          read FIODriver;
  property Console    : TIOConsoleRenderer read FConsole;
  property PadState   : TIOPadState        read FPadState;
  property Bindings   : TBindings          read FBindings;
  property UIBindings : TBindingContext    read FUIBindings;
end;

var IO : TIO;

implementation

uses vutil, vtig, vtigio, vioeventstate, dateutils, math;

{ TIO }

constructor TIO.Create( aIODriver : TIODriver; aConsole : TIOConsoleRenderer );
begin
  inherited Create;
  FIODriver        := aIODriver;
  FConsole         := nil;
  FLastUpdate      := FIODriver.GetMs;
  FTIGConsoleView  := nil;
  FLayers          := TIOLayerStack.Create;
  FPadState        := TIOPadState.Create;
  FBindings        := TBindings.Create;
  FUIBindings       := FBindings.CreateContext;
  FUIBindings.BindKey( VKEY_UP,     VTIG_IE_UP );
  FUIBindings.BindKey( VKEY_DOWN,   VTIG_IE_DOWN );
  FUIBindings.BindKey( VKEY_LEFT,   VTIG_IE_LEFT );
  FUIBindings.BindKey( VKEY_RIGHT,  VTIG_IE_RIGHT );
  FUIBindings.BindKey( VKEY_HOME,   VTIG_IE_HOME );
  FUIBindings.BindKey( VKEY_END,    VTIG_IE_END );
  FUIBindings.BindKey( VKEY_PGUP,   VTIG_IE_PGUP );
  FUIBindings.BindKey( VKEY_PGDOWN, VTIG_IE_PGDOWN );
  FUIBindings.BindKey( VKEY_ESCAPE, VTIG_IE_CANCEL );
  FUIBindings.BindKey( VKEY_ENTER,  VTIG_IE_CONFIRM );
  FUIBindings.BindKey( VKEY_SPACE,  VTIG_IE_SELECT );
  FUIBindings.BindPad( VPAD_BUTTON_DPAD_UP,       VTIG_IE_UP );
  FUIBindings.BindPad( VPAD_BUTTON_DPAD_DOWN,     VTIG_IE_DOWN );
  FUIBindings.BindPad( VPAD_BUTTON_DPAD_LEFT,     VTIG_IE_LEFT );
  FUIBindings.BindPad( VPAD_BUTTON_DPAD_RIGHT,    VTIG_IE_RIGHT );
  FUIBindings.BindPad( VPAD_BUTTON_B,             VTIG_IE_CANCEL );
  FUIBindings.BindPad( VPAD_BUTTON_A,             VTIG_IE_CONFIRM );
  FUIBindings.BindPad( VPAD_BUTTON_LEFTSHOULDER,  VTIG_IE_LEFT );
  FUIBindings.BindPad( VPAD_BUTTON_RIGHTSHOULDER, VTIG_IE_RIGHT );
  FMouseLast       := Point(-1,-1);
  FMouse           := Point(-1,-1);

  if aConsole <> nil then
    Initialize( aConsole );
  IO := Self;
end;

procedure TIO.Initialize( aConsole : TIOConsoleRenderer );
begin
  VTIG_Shutdown;
  if aConsole <> nil then
    VTIG_Initialize( aConsole, FIODriver, False );
  if FConsole <> aConsole then FreeAndNil( FConsole );
  FConsole    := aConsole;
end;

destructor TIO.Destroy;
var iLayer : TIOLayer;
begin
  if IO = Self then IO := nil;
  if FLayers <> nil then
    for iLayer in FLayers do
      iLayer.Free;
  FreeAndNil( FLayers );
  FreeAndNil( FPadState );
  VTIG_Shutdown;

  FreeAndNil( FBindings );
  FreeAndNil( FConsole );
  FreeAndNil( FIODriver );
  inherited Destroy;
end;

function TIO.CaptureScreen( const aFileName : AnsiString ) : Boolean;
begin
  if FIODriver = nil then Exit( False );
  Result := FIODriver.CaptureScreen( aFileName );
end;

function TIO.SaveConsoleTextDump( const aFileName : AnsiString ) : Boolean;
var iLines : TStringList;
    iLine  : AnsiString;
    iX, iY : Integer;
begin
  Result := False;
  if FConsole = nil then Exit;

  iLines := TStringList.Create;
  try
    for iY := 1 to FConsole.SizeY do
    begin
      SetLength( iLine, FConsole.SizeX );
      for iX := 1 to FConsole.SizeX do
        iLine[ iX ] := FConsole.GetChar( iX, iY );
      iLines.Add( iLine );
    end;
    try
      iLines.SaveToFile( aFileName );
      Result := True;
    except
      on E : Exception do Result := False;
    end;
  finally
    iLines.Free;
  end;
end;

procedure TIO.RegisterDebugConsole ( aKey : TIOKeyCode ) ;
begin
  FIODriver.RegisterInterrupt( aKey, @ConsoleCallback );
end;

procedure TIO.PreUpdate;
begin
  FIODriver.PreUpdate;
end;

procedure TIO.FullUpdate;
var iTickTime : DWord;
    iNow      : DWord;
begin
  iNow        := FIODriver.GetMs;
  iTickTime   := iNow - FLastUpdate;
  FLastUpdate := iNow;

  VTIG_NewFrame;
  PreUpdate;
  Update( iTickTime );

  PostUpdate;
end;

procedure TIO.PostUpdate;
begin
  FIODriver.PostUpdate;
end;

procedure TIO.Clear;
var iLayer : TIOLayer;
begin
  for iLayer in FLayers do
    iLayer.Free;
  FLayers.Clear;
  FPadState.Clear;
  FMouseLast := Point(-1,-1);
  FMouse     := Point(-1,-1);
end;

procedure TIO.Update ( aMSec : DWord ) ;
var iMEvent : TIOEvent;
    i,iM    : Integer;
begin
  if FMouse <> FMouseLast then
  begin
    FMouseLast := FMouse;
    iMEvent.EType:= VEVENT_MOUSEMOVE;
    iMEvent.MouseMove.Pos := FMouse;
    VTIG_GetIOState.MouseState.HandleEvent( iMEvent );
  end;

  if FLayers.Size > 0 then
  begin
    ClearFinishedLayers;
    iM := -1;
    for i := FLayers.Size-1 downto 0 do
      if FLayers[i].IsModal then
        begin
          iM := i;
          Break;
        end;
    for i := 0 to FLayers.Size-1 do
      FLayers[i].Update( Integer( aMSec ), i >= iM );
    ClearFinishedLayers;
  end;

  VTIG_EndFrame;
  VTIG_Render;
end;

function TIO.OnEvent( const aEvent : TIOEvent ) : Boolean;
var i, iInput : Integer;
    iEvent    : TIOEvent;
    iWide     : WideString;
    iAction   : TBindingAction;
begin
  FPadState.HandleEvent( aEvent );
  case aEvent.EType of
    VEVENT_KEYDOWN,
    VEVENT_KEYUP:
      if not aEvent.Key.Repeated then
      begin
        iAction := FUIBindings.ResolveKey( TIOKeyCode( aEvent.Key.Code ) );
        if ( iAction >= 0 ) and ( iAction < VIO_MAXEVENTS ) then
          VTIG_GetIOState.EventState.SetState( iAction, aEvent.Key.Pressed );
      end;
    VEVENT_PADDOWN,
    VEVENT_PADUP:
      begin
        iAction := FUIBindings.ResolvePad( aEvent.Pad.Button );
        if ( iAction >= 0 ) and ( iAction < VIO_MAXEVENTS ) then
          VTIG_GetIOState.EventState.SetState( iAction, aEvent.Pad.Pressed );
      end;
  end;

  if ( aEvent.EType in [ VEVENT_MOUSEMOVE ] ) then
    FMouse := DeviceCoordToConsoleCoord( aEvent.MouseMove.Pos );

  if ( aEvent.EType in [ VEVENT_MOUSEDOWN, VEVENT_MOUSEUP ] ) then
  begin
    iEvent := aEvent;
    iEvent.Mouse.Pos := DeviceCoordToConsoleCoord( aEvent.Mouse.Pos );
    VTIG_GetIOState.MouseState.HandleEvent( iEvent );
    if ( aEvent.EType = VEVENT_MOUSEDOWN ) and ( aEvent.Mouse.Button = VMB_BUTTON_LEFT ) then
      VTIG_GetIOState.EventState.SetState( VTIG_IE_MCONFIRM, True );
  end;

  if ( aEvent.EType = VEVENT_TEXT ) then
  begin
    iWide := UTF8Decode( UTF8String( aEvent.Text.Text ) );
    VTIG_GetIOState.EventState.AppendText( PWideChar( iWide ) );
  end;

  if ( aEvent.EType in [ VEVENT_KEYDOWN, VEVENT_KEYUP ] ) and
     ( not aEvent.Key.Repeated ) then
  begin
    VTIG_GetIOState.KeyState.SetState( aEvent.Key.Code, aEvent.Key.Pressed );
    VTIG_GetIOState.EventState.SetState( VTIG_IE_SHIFT, VKMOD_SHIFT in aEvent.Key.ModState );
    case aEvent.Key.Code of
      VKEY_0 : VTIG_GetIOState.EventState.SetState( VTIG_IE_0, aEvent.Key.Pressed );
      VKEY_1 : VTIG_GetIOState.EventState.SetState( VTIG_IE_1, aEvent.Key.Pressed );
      VKEY_2 : VTIG_GetIOState.EventState.SetState( VTIG_IE_2, aEvent.Key.Pressed );
      VKEY_3 : VTIG_GetIOState.EventState.SetState( VTIG_IE_3, aEvent.Key.Pressed );
      VKEY_4 : VTIG_GetIOState.EventState.SetState( VTIG_IE_4, aEvent.Key.Pressed );
      VKEY_5 : VTIG_GetIOState.EventState.SetState( VTIG_IE_5, aEvent.Key.Pressed );
      VKEY_6 : VTIG_GetIOState.EventState.SetState( VTIG_IE_6, aEvent.Key.Pressed );
      VKEY_7 : VTIG_GetIOState.EventState.SetState( VTIG_IE_7, aEvent.Key.Pressed );
      VKEY_8 : VTIG_GetIOState.EventState.SetState( VTIG_IE_8, aEvent.Key.Pressed );
      VKEY_9 : VTIG_GetIOState.EventState.SetState( VTIG_IE_9, aEvent.Key.Pressed );
      VKEY_C : VTIG_GetIOState.EventState.SetState( VTIG_IE_COPY,
        aEvent.Key.Pressed and ( VKMOD_CTRL in aEvent.Key.ModState ) );
      VKEY_V : VTIG_GetIOState.EventState.SetState( VTIG_IE_PASTE,
        aEvent.Key.Pressed and ( VKMOD_CTRL in aEvent.Key.ModState ) );
    end;
  end;

  if not FLayers.IsEmpty then
  begin
    iInput := EventToUIInput( aEvent );
    for i := FLayers.Size - 1 downto 0 do
      if not FLayers[i].isFinished then
      begin
        if ( iInput > 0 ) and FLayers[i].HandleInput( iInput ) then Exit( True );
        if FLayers[i].HandleEvent( aEvent ) then Exit( True );
      end;
  end;
  Exit( False );
end;

function TIO.HandleInput( aInput : Integer ) : Boolean;
var i : Integer;
begin
  if not FLayers.IsEmpty then
    for i := FLayers.Size - 1 downto 0 do
      if not FLayers[i].isFinished then
        if FLayers[i].HandleInput( aInput ) then
          Exit( True );
  Exit( False );
end;

function TIO.HandleEvents : Boolean;
var iEvent : TIOEvent;
begin
  HandleEvents := False;
  while FIODriver.PollEvent( iEvent ) do
    HandleEvents := OnEvent( iEvent ) or HandleEvents;
end;

procedure TIO.ClearEventBuffer;
var iEvent : TIOEvent;
begin
  while FIODriver.PollEvent( iEvent ) do
    FPadState.HandleEvent( iEvent );
end;

procedure TIO.Delay( aTime : Integer );
var Started : TDateTime;
begin
  while aTime > 0 do
  begin
    FullUpdate;
    Started := Now;
    FIODriver.Sleep(Min(10,aTime));
    aTime -= Min(MilliSecondsBetween(Now,Started), aTime);
  end;
end;

function TIO.ConsoleCallback ( aEvent : TIOEvent ) : Boolean;
begin
  if FTIGConsoleView <> nil then
  begin
    FConsole.HideCursor;
    FTIGConsoleView.SaveHistory('console.history');
    FTIGConsoleView.Finish;
    FTIGConsoleView := nil;
    Exit( True );
  end;
  FConsole.ShowCursor;
  FTIGConsoleView := PushLayer( TTIGConsoleView.Create ) as TTIGConsoleView;
  FTIGConsoleView.LoadHistory('console.history');
  Exit( True );
end;

procedure TIO.ClearFinishedLayers;
var i,j : Integer;
begin
  i := 0;
  while i < FLayers.Size do
    if FLayers[i].IsFinished then
    begin
      FLayers[i].Free;
      if i < FLayers.Size - 1 then
        for j := i to FLayers.Size - 2 do
          FLayers[j] := FLayers[j + 1];
      FLayers.Pop;
    end
    else
      Inc( i );
end;

function TIO.PushLayer( aLayer : TIOLayer ) : TIOLayer;
begin
  FLayers.Push( aLayer );
  Result := aLayer;
end;

function TIO.IsTopLayer( aLayer : TIOLayer ) : Boolean;
begin
  Exit( ( FLayers.Size > 0 ) and ( FLayers.Top = aLayer ) );
end;

function TIO.IsModal : Boolean;
var iLayer : TIOLayer;
begin
  for iLayer in FLayers do
    if iLayer.IsModal then Exit( True );
  Exit( False );
end;

procedure TIO.WaitForLayer;
begin
  repeat
    Sleep(10);
    FullUpdate;
    HandleEvents;
  until FLayers.IsEmpty or (not IsModal);
end;

function TIO.EventToUIInput( const aEvent : TIOEvent ) : Integer;
begin
  Exit( 0 );
end;

function TIO.DeviceCoordToConsoleCoord( aCoord : TIOPoint ) : TIOPoint;
begin
  Exit( aCoord );
end;

function TIO.ConsoleCoordToDeviceCoord( aCoord : TIOPoint ) : TIOPoint;
begin
  Exit( aCoord );
end;
end.
