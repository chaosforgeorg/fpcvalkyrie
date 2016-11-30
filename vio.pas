{$INCLUDE valkyrie.inc}
unit vio;
interface
uses Classes, SysUtils, vsystem, vuitypes,
     vuielement, vconui, vioevent, viotypes, vioconsole;

type TIO = class( TSystem )
  constructor Create( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle );
  procedure FullUpdate; virtual;
  procedure Update( aMSec : DWord ); virtual;
  function BreakUILoop( aSender : TUIElement ) : Boolean; virtual;
  function EventFreeElement( aSender : TUIElement ) : Boolean;
  function EventPending : Boolean;
  procedure Delay( aTime : Integer );
  procedure ClearEventBuffer;
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
  FStoredCursor   : Boolean;

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

{ TIO }

constructor TIO.Create ( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle ) ;
begin
  inherited Create;
  IO := Self;
  FIODriver   := aIODriver;
  FConsole    := aConsole;
  FUIRoot     := TConUIRoot.Create( FConsole, aStyle );
  FUIRoot.Fullscreen := True;
  FLastUpdate := FIODriver.GetMs;
  FConsoleWindow := nil;
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

function TIO.BreakUILoop ( aSender : TUIElement ) : Boolean;
begin
  FUILoop := False;
  Exit( True );
end;

function TIO.EventFreeElement ( aSender : TUIElement ) : Boolean;
begin
  FreeAndNil( aSender );
  Exit( True );
end;

function TIO.EventPending : Boolean;
begin
  Exit( FIODriver.EventPending );
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
    HandleEvents := FUIRoot.OnEvent( iEvent ) or HandleEvents;
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

