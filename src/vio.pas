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

type TInterfaceLayerStack = specialize TGArray<TIOLayer>;

type TIO = class( TSystem )
  constructor Create( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle );
  procedure Initialize( aConsole : TIOConsoleRenderer; aStyle : TUIStyle );
  procedure FullUpdate; virtual;
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
protected
  function ConsoleInputCallback( aConsole : TConUIConsole; const aInput : TUIString ) : Boolean;
  function ConsoleCallback( aEvent : TIOEvent ) : Boolean;
protected
  FIODriver       : TIODriver;
  FUIRoot         : TConUIRoot;
  FConsole        : TIOConsoleRenderer;
  FConsoleWindow  : TConUIConsole;

  FLastUpdate     : DWord;
  FUILoop         : Boolean;
  FUILoopResult   : DWord;
public
  property Root      : TConUIRoot read FUIRoot;
  property Driver    : TIODriver  read FIODriver;
  property Console   : TIOConsoleRenderer read FConsole;
end;

var IO : TIO;

implementation

uses vluasystem, dateutils, math;

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

constructor TIO.Create ( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle ) ;
begin
  inherited Create;
  IO := Self;
  FIODriver   := aIODriver;
  FConsole    := nil;
  FUIRoot     := nil;
  FLastUpdate := FIODriver.GetMs;
  FConsoleWindow := nil;
  if aConsole <> nil then
    Initialize( aConsole, aStyle );
end;

procedure TIO.Initialize( aConsole : TIOConsoleRenderer; aStyle : TUIStyle );
begin
  FreeAndNil( FUIRoot );
  if FConsole <> aConsole then FreeAndNil( FConsole );
  FConsole    := aConsole;
  if FConsole = nil then Exit;
  FUIRoot     := TConUIRoot.Create( FConsole, aStyle );
  FUIRoot.Fullscreen := True;
end;

destructor TIO.Destroy;
begin
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

procedure TIO.FullUpdate;
var iTickTime : DWord;
    iNow      : DWord;
begin
  iNow        := FIODriver.GetMs;
  iTickTime   := iNow - FLastUpdate;
  FLastUpdate := iNow;

  FIODriver.PreUpdate;
  Update( iTickTime );
  FIODriver.PostUpdate;
end;

procedure TIO.Update ( aMSec : DWord ) ;
begin
  FUIRoot.OnUpdate( aMSec );
  FUIRoot.Render;
  FConsole.Update;
end;

function TIO.OnEvent( const event : TIOEvent ) : Boolean;
begin
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

procedure TIO.ConsolePrint ( const aText : AnsiString ) ;
begin
  if FConsoleWindow <> nil then
    FConsoleWindow.Writeln( aText );
end;

end.

