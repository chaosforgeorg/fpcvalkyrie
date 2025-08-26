{$INCLUDE valkyrie.inc}
unit vio;
interface
uses Classes, SysUtils, vsystem, vgenerics,
     vuielement, vconui, vioevent, viotypes, vtigconsole, vioconsole;

type TIO = class( TSystem )
  constructor Create( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean = False  ); reintroduce;
  procedure Initialize( aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean = False );
  procedure PreUpdate; virtual;
  procedure FullUpdate; virtual;
  procedure PostUpdate; virtual;
  procedure Clear; virtual;
  procedure Update( aMSec : DWord ); virtual;
  procedure Delay( aTime : Integer );
  procedure ClearEventBuffer;
  function OnEvent( const aEvent : TIOEvent ) : Boolean; virtual;
  function RunUILoop( aElement : TUIElement = nil ) : DWord; virtual;
  procedure SetUILoopResult( aResult : DWord );
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
protected
  function HandleInput( aInput : Integer ) : Boolean;
  function ConsoleCallback( aEvent : TIOEvent ) : Boolean;
  procedure ClearFinishedLayers;
protected
  FIODriver       : TIODriver;
  FUIRoot         : TConUIRoot;
  FConsole        : TIOConsoleRenderer;
  FTIGConsoleView : TTIGConsoleView;
  FLayers         : TIOLayerStack;

  FLastUpdate     : DWord;
  FUILoop         : Boolean;
  FUILoopResult   : DWord;
  FTIGActive      : Boolean;

  FUIMouseLast : TIOPoint;
  FUIMouse     : TIOPoint;
public
  property Root      : TConUIRoot read FUIRoot;
  property Driver    : TIODriver  read FIODriver;
  property Console   : TIOConsoleRenderer read FConsole;
end;

var IO : TIO;

implementation

uses vutil, vtig, vtigio, dateutils, math;

{ TIO }

constructor TIO.Create ( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean ) ;
begin
  inherited Create;
  IO := Self;
  FIODriver        := aIODriver;
  FConsole         := nil;
  FUIRoot          := nil;
  FLastUpdate      := FIODriver.GetMs;
  FTIGConsoleView  := nil;
  FLayers          := TIOLayerStack.Create;
  FTIGActive       := False;
  FUIMouseLast     := Point(-1,-1);
  FUIMouse         := Point(-1,-1);

  if aConsole <> nil then
    Initialize( aConsole, aStyle, aInitTIG );
end;

procedure TIO.Initialize( aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean );
begin
  FreeAndNil( FUIRoot );
  if aInitTIG then
  begin
    if FTIGActive then
      VTIG_Shutdown;
    if aConsole <> nil then
    begin
      FTIGActive := True;
      VTIG_Initialize( aConsole, FIODriver, False );
    end;
  end;
  if FConsole <> aConsole then FreeAndNil( FConsole );
  FConsole    := aConsole;
  if FConsole = nil then Exit;
  FUIRoot     := TConUIRoot.Create( FConsole, aStyle );
  FUIRoot.Fullscreen := True;
end;

destructor TIO.Destroy;
var iLayer : TIOLayer;
begin
  for iLayer in FLayers do
    iLayer.Free;
  FreeAndNil( FLayers );
  if FTIGActive then
    VTIG_Shutdown;

  FreeAndNil( FUIRoot );
  FreeAndNil( FConsole );
  FreeAndNil( FIODriver );
  inherited Destroy;
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

  if FTIGActive then
    VTIG_NewFrame;
  PreUpdate;
  Update( iTickTime );
  if FTIGActive then
  begin
    VTIG_EndFrame;
    VTIG_Render;
  end;
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
  FUIMouseLast := Point(-1,-1);
  FUIMouse     := Point(-1,-1);
end;

procedure TIO.Update ( aMSec : DWord ) ;
var iMEvent : TIOEvent;
    i,iM    : Integer;
begin
  if FUIMouse <> FUIMouseLast then
  begin
    FUIMouseLast := FUIMouse;
    if FTIGActive then
    begin
      iMEvent.EType:= VEVENT_MOUSEMOVE;
      iMEvent.MouseMove.Pos := FUIMouse;
      VTIG_GetIOState.MouseState.HandleEvent( iMEvent );
    end;
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

  FUIRoot.OnUpdate( aMSec );
  FUIRoot.Render;
  if not FTIGActive then
    FConsole.Update;
end;

function TIO.OnEvent( const aEvent : TIOEvent ) : Boolean;
var i, iInput : Integer;
    iEvent    : TIOEvent;
    iWide     : WideString;
begin
  if ( aEvent.EType in [ VEVENT_MOUSEMOVE ] ) then
    FUIMouse := DeviceCoordToConsoleCoord( aEvent.MouseMove.Pos );

  if ( aEvent.EType in [ VEVENT_MOUSEDOWN, VEVENT_MOUSEUP ] ) then
  begin
    iEvent := aEvent;
    iEvent.Mouse.Pos := DeviceCoordToConsoleCoord( aEvent.Mouse.Pos );
    if FTIGActive then
    begin
      VTIG_GetIOState.MouseState.HandleEvent( iEvent );
      if ( aEvent.EType = VEVENT_MOUSEDOWN ) and ( aEvent.Mouse.Button = VMB_BUTTON_LEFT ) then
        VTIG_GetIOState.EventState.SetState( VTIG_IE_MCONFIRM, True );
    end;
  end;

  if ( aEvent.EType = VEVENT_TEXT ) and FTIGActive then
  begin
    iWide := UTF8Decode( UTF8String( aEvent.Text.Text ) );
    VTIG_GetIOState.EventState.AppendText( PWideChar( iWide ) );
  end;

  if ( aEvent.EType = VEVENT_KEYDOWN ) or ( aEvent.EType = VEVENT_KEYUP ) and ( not aEvent.Key.Repeated ) then
  begin
    VTIG_GetIOState.EventState.SetState( VTIG_IE_SHIFT, VKMOD_SHIFT in aEvent.Key.ModState );
    case aEvent.Key.Code of
      VKEY_UP     : VTIG_GetIOState.EventState.SetState( VTIG_IE_UP,        aEvent.Key.Pressed );
      VKEY_DOWN   : VTIG_GetIOState.EventState.SetState( VTIG_IE_DOWN,      aEvent.Key.Pressed );
      VKEY_LEFT   : VTIG_GetIOState.EventState.SetState( VTIG_IE_LEFT,      aEvent.Key.Pressed );
      VKEY_RIGHT  : VTIG_GetIOState.EventState.SetState( VTIG_IE_RIGHT,     aEvent.Key.Pressed );
      VKEY_HOME   : VTIG_GetIOState.EventState.SetState( VTIG_IE_HOME,      aEvent.Key.Pressed );
      VKEY_END    : VTIG_GetIOState.EventState.SetState( VTIG_IE_END,       aEvent.Key.Pressed );
      VKEY_PGUP   : VTIG_GetIOState.EventState.SetState( VTIG_IE_PGUP,      aEvent.Key.Pressed );
      VKEY_PGDOWN : VTIG_GetIOState.EventState.SetState( VTIG_IE_PGDOWN,    aEvent.Key.Pressed );
      VKEY_ESCAPE : VTIG_GetIOState.EventState.SetState( VTIG_IE_CANCEL,    aEvent.Key.Pressed );
      VKEY_ENTER  : VTIG_GetIOState.EventState.SetState( VTIG_IE_CONFIRM,   aEvent.Key.Pressed );
      VKEY_SPACE  : VTIG_GetIOState.EventState.SetState( VTIG_IE_SELECT,    aEvent.Key.Pressed );
      VKEY_BACK   : VTIG_GetIOState.EventState.SetState( VTIG_IE_BACKSPACE, aEvent.Key.Pressed );
      VKEY_TAB    : VTIG_GetIOState.EventState.SetState( VTIG_IE_TAB,       aEvent.Key.Pressed );
      VKEY_DELETE : VTIG_GetIOState.EventState.SetState( VTIG_IE_DELETE,    aEvent.Key.Pressed );
      VKEY_0      : VTIG_GetIOState.EventState.SetState( VTIG_IE_0, aEvent.Key.Pressed );
      VKEY_1      : VTIG_GetIOState.EventState.SetState( VTIG_IE_1, aEvent.Key.Pressed );
      VKEY_2      : VTIG_GetIOState.EventState.SetState( VTIG_IE_2, aEvent.Key.Pressed );
      VKEY_3      : VTIG_GetIOState.EventState.SetState( VTIG_IE_3, aEvent.Key.Pressed );
      VKEY_4      : VTIG_GetIOState.EventState.SetState( VTIG_IE_4, aEvent.Key.Pressed );
      VKEY_5      : VTIG_GetIOState.EventState.SetState( VTIG_IE_5, aEvent.Key.Pressed );
      VKEY_6      : VTIG_GetIOState.EventState.SetState( VTIG_IE_6, aEvent.Key.Pressed );
      VKEY_7      : VTIG_GetIOState.EventState.SetState( VTIG_IE_7, aEvent.Key.Pressed );
      VKEY_8      : VTIG_GetIOState.EventState.SetState( VTIG_IE_8, aEvent.Key.Pressed );
      VKEY_9      : VTIG_GetIOState.EventState.SetState( VTIG_IE_9, aEvent.Key.Pressed );
      VKEY_C      : VTIG_GetIOState.EventState.SetState( VTIG_IE_COPY,  aEvent.Key.Pressed and ( VKMOD_CTRL in aEvent.Key.ModState ) );
      VKEY_V      : VTIG_GetIOState.EventState.SetState( VTIG_IE_PASTE, aEvent.Key.Pressed and ( VKMOD_CTRL in aEvent.Key.ModState ) );
    end;
  end;

  // TODO: auto-repeat
  if ( aEvent.EType = VEVENT_PADDOWN ) or ( aEvent.EType = VEVENT_PADUP ) then
  begin
    case aEvent.Pad.Button of
      VPAD_BUTTON_DPAD_UP       : VTIG_GetIOState.EventState.SetState( VTIG_IE_UP,        aEvent.Pad.Pressed );
      VPAD_BUTTON_DPAD_DOWN     : VTIG_GetIOState.EventState.SetState( VTIG_IE_DOWN,      aEvent.Pad.Pressed );
      VPAD_BUTTON_DPAD_LEFT     : VTIG_GetIOState.EventState.SetState( VTIG_IE_LEFT,      aEvent.Pad.Pressed );
      VPAD_BUTTON_DPAD_RIGHT    : VTIG_GetIOState.EventState.SetState( VTIG_IE_RIGHT,     aEvent.Pad.Pressed );
      VPAD_BUTTON_B             : VTIG_GetIOState.EventState.SetState( VTIG_IE_CANCEL,    aEvent.Pad.Pressed );
      VPAD_BUTTON_A             : VTIG_GetIOState.EventState.SetState( VTIG_IE_CONFIRM,   aEvent.Pad.Pressed );
      VPAD_BUTTON_LEFTSHOULDER  : VTIG_GetIOState.EventState.SetState( VTIG_IE_LEFT,      aEvent.Pad.Pressed );
      VPAD_BUTTON_RIGHTSHOULDER : VTIG_GetIOState.EventState.SetState( VTIG_IE_RIGHT,     aEvent.Pad.Pressed );
      VPAD_BUTTON_Y             : VTIG_GetIOState.EventState.SetState( VTIG_IE_BACKSPACE, aEvent.Pad.Pressed );
      VPAD_BUTTON_X             : VTIG_GetIOState.EventState.SetState( VTIG_IE_TAB,       aEvent.Pad.Pressed );
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

function TIO.RunUILoop ( aElement : TUIElement ) : DWord;
var iCount  : DWord;
    iParent : TUIElement;
begin
  FConsole.HideCursor;
  iCount := 0;
  iParent := FUIRoot;
  if aElement <> nil then
  begin
    iParent := aElement.Parent as TUIElement;
    iCount := iParent.ChildCount-1;
  end;
  FUILoop := True;
  FLastUpdate := FIODriver.GetMs;
  repeat
    Sleep(10);
    FullUpdate;
    HandleEvents;
  until ( not FUILoop ) or ( iParent.ChildCount = iCount );
  FUILoop := True;
  Exit( FUILoopResult );
end;

procedure TIO.SetUILoopResult ( aResult : DWord ) ;
begin
  FUILoopResult := aResult;
end;

function TIO.HandleEvents : Boolean;
var iEvent : TIOEvent;
begin
  HandleEvents := False;
  while FIODriver.PollEvent( iEvent ) do
    HandleEvents := OnEvent( iEvent ) or FUIRoot.OnEvent( iEvent ) or HandleEvents;
end;

procedure TIO.ClearEventBuffer;
var iEvent : TIOEvent;
begin
  while FIODriver.EventPending do
    FIODriver.PollEvent( iEvent );
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
  if FTIGActive then
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
  Exit( False );
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

