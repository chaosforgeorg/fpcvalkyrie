{$INCLUDE valkyrie.inc}
unit vio;
interface
uses Classes, SysUtils, vsystem, vuitypes, vgenerics,
     vuielement, vconui, vioevent, viotypes, vioconsole;

type TIOLayer = class
  procedure Update( aDTime : Integer ); virtual; abstract;
  function IsFinished : Boolean; virtual; abstract;
  function IsModal : Boolean; virtual;
  function HandleEvent( const aEvent : TIOEvent ) : Boolean; virtual;
  function HandleInput( aInput : Integer ) : Boolean; virtual;
end;

type TIOLayerStack = specialize TGArray<TIOLayer>;

type TIO = class( TSystem )
  constructor Create( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean = False  );
  procedure Initialize( aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean = False );
  procedure PreUpdate; virtual;
  procedure FullUpdate; virtual;
  procedure PostUpdate; virtual;
  procedure Clear; virtual;
  procedure Update( aMSec : DWord ); virtual;
  procedure Delay( aTime : Integer );
  procedure ClearEventBuffer;
  function OnEvent( const event : TIOEvent ) : Boolean; virtual;
  function RunUILoop( aElement : TUIElement = nil ) : DWord; virtual;
  procedure SetUILoopResult( aResult : DWord );
  function HandleEvents : Boolean; virtual;
  destructor Destroy; override;
  procedure RegisterDebugConsole( aKey : TIOKeyCode );
  procedure ConsolePrint( const aText : AnsiString );
  function PushLayer( aLayer : TIOLayer ) : TIOLayer; virtual;
  function IsTopLayer( aLayer : TIOLayer ) : Boolean;
  function IsModal : Boolean;
  procedure WaitForLayer;
protected
  function HandleInput( aInput : Integer ) : Boolean;
  function ConsoleInputCallback( aConsole : TConUIConsole; const aInput : TUIString ) : Boolean;
  function ConsoleCallback( aEvent : TIOEvent ) : Boolean;
  procedure ClearFinishedLayers;
protected
  FIODriver       : TIODriver;
  FUIRoot         : TConUIRoot;
  FConsole        : TIOConsoleRenderer;
  FConsoleWindow  : TConUIConsole;
  FLayers         : TIOLayerStack;

  FLastUpdate     : DWord;
  FUILoop         : Boolean;
  FUILoopResult   : DWord;
  FTIGActive      : Boolean;
public
  property Root      : TConUIRoot read FUIRoot;
  property Driver    : TIODriver  read FIODriver;
  property Console   : TIOConsoleRenderer read FConsole;
end;

var IO : TIO;

implementation

uses vtig, vluasystem, dateutils, math;

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

{ TIO }

constructor TIO.Create ( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean ) ;
begin
  inherited Create;
  IO := Self;
  FIODriver        := aIODriver;
  FConsole         := nil;
  FUIRoot          := nil;
  FLastUpdate      := FIODriver.GetMs;
  FConsoleWindow   := nil;
  FLayers          := TIOLayerStack.Create;
  FTIGActive       := False;

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
  if LuaSystem <> nil then
    LuaSystem.SetPrintFunction( @ConsolePrint );

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
end;

procedure TIO.Update ( aMSec : DWord ) ;
var iLayer : TIOLayer;
begin
  if FLayers.Size > 0 then
  begin
    ClearFinishedLayers;
    for iLayer in FLayers do
      iLayer.Update( Integer( aMSec ) );
    ClearFinishedLayers;
  end;

  FUIRoot.OnUpdate( aMSec );
  FUIRoot.Render;
  if not FTIGActive then
    FConsole.Update;
end;

function TIO.OnEvent( const event : TIOEvent ) : Boolean;
var i : Integer;
begin
  if not FLayers.IsEmpty then
    for i := FLayers.Size - 1 downto 0 do
      if not FLayers[i].isFinished then
        if FLayers[i].HandleEvent( event ) then
          Exit( True );
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

function TIO.ConsoleInputCallback ( aConsole : TConUIConsole; const aInput : TUIString ) : Boolean;
begin
  LuaSystem.ConsoleExecute( aInput );
  Exit( True );
end;

function TIO.ConsoleCallback ( aEvent : TIOEvent ) : Boolean;
begin
  if FConsoleWindow <> nil then
  begin
    FConsole.HideCursor;
    FConsoleWindow.SaveHistory('console.history');
    FreeAndNil( FConsoleWindow );
    Exit( True );
  end;
  FConsole.ShowCursor;
  FConsoleWindow := TConUIConsole.Create( FUIRoot, @ConsoleInputCallback );
  FConsoleWindow.LoadHistory('console.history');
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


procedure TIO.ConsolePrint ( const aText : AnsiString ) ;
begin
  if FConsoleWindow <> nil then
    FConsoleWindow.Writeln( aText );
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

end.

