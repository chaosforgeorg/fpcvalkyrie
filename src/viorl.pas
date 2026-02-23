{$INCLUDE valkyrie.inc}
unit viorl;
interface
uses Classes, SysUtils, vio, vrltools, vconuirl, vuitypes, vluaentitynode, vluamapnode,
     vuielement, vconui, vluastate, vluaconfig, vioevent, viotypes, vioconsole, vtextmap, vmessages;

const COMMAND_INVALID = 255;
      COMMAND_SYSQUIT = 253;

type TCommandSet = Set of Byte;
     TKeySet     = Set of Byte;

type

{ TIORL }

 TIORL = class( TIO )
  constructor Create( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean = False );

  // TIG-version functions
  procedure RunLayer( aLayer : TIOLayer ); virtual;

  // Settings

  procedure Configure( aLuaConfig : TLuaConfig ); virtual;
  procedure SetPlayer( aPlayer : TLuaEntityNode ); virtual;
  procedure SetLevel( aLevel : TLuaMapNode ); virtual;

  // Messages
  // Adds a message for the message buffer
  procedure Msg( const aMessage : Ansistring); virtual;
  // Adds a message for the message buffer (params passed to Format)
  procedure Msg( const aMessage : Ansistring; const aParams : array of Const );
  // Kills last message from the message buffer.
  procedure MsgKill;
  // Update messages
  procedure MsgUpdate; virtual;
  // Dump last messages to file.
  procedure MsgDump( var iTextFile : Text; iLastCount : DWord );

  // Events
  function GetCommand : Byte;
  function WaitForCommand( const aSet : TCommandSet ) : Byte;
  function WaitForKey ( const aSet : TKeySet ) : Byte;
  function WaitForKeyEvent ( out aEvent : TIOEvent ) : Boolean;
  procedure BreakKeyLoop;

  // Animation
  // Marks the given tile with the specified gylph. Use MarkDisplay afterwards.
  procedure MarkTile( aCoord : TCoord2D; aSign : char; aColor : byte );
  // Clears all marks.
  procedure MarkClear;
  procedure AddMarkAnimation( aCoord : TCoord2D; aSign : char; aColor : TIOColor; aDuration : DWord; aDelay : DWord = 0  );
  procedure AddBulletAnimation( aFrom, aTo : TCoord2D; aSign : char; aColor : TIOColor; aDuration : DWord; aDelay : DWord = 0  );
  procedure AddExplodeAnimation( aCoord : TCoord2D; const aArray : TTextExplosionArray; aDelay : DWord );
  procedure ClearAnimations;
  procedure WaitForAnimations;
  // Renders an explosion on the screen using Explode marks
  procedure Explosion( aWhere : TCoord2D; aColor : byte; aRange : byte; aDrawDelay : Word; aDelay : Word );

  // Cursor
  procedure FocusCursor( aCoord : TCoord2D );
  procedure ShowCursor;
  procedure HideCursor;
  function IOKeyCodeToCommand( aKey : TIOKeyCode ) : Byte;

  destructor Destroy; override;

  // Register Lua API
  class procedure RegisterLuaAPI( const aTableName : AnsiString );
  // Register Lua API
  class procedure RegisterLuaAPI( State : TLuaState; const aTableName : AnsiString );
private
  function GetMapShift : TUIPoint;
protected
  FUIMap       : TConUIMapArea;
  FUIMessages  : TConUIMessages;
  FTMap        : TTextMap;
  FTMessages   : TMessages;
  FConfig    : TLuaConfig;
  FKeyCode   : Word;

  FLevel     : TLuaMapNode;
  FPlayer    : TLuaEntityNode;
  FBreakLoop : Boolean;
public
  property Config      : TLuaConfig read FConfig;
  property MapShift    : TUIPoint read GetMapShift;
  property LastKeyCode : Word read FKeyCode;
  property Messages    : TMessages read FTMessages;
end;

implementation

uses variants, vluasystem, vutil, math;

var IORL : TIORL = nil;

{ TIORL }

constructor TIORL.Create ( aIODriver : TIODriver; aConsole : TIOConsoleRenderer; aStyle : TUIStyle; aInitTIG : Boolean = False ) ;
begin
  IORL := Self;
  inherited Create( aIODriver, aConsole, aStyle, aInitTIG );
  FUIMap       := nil;
  FUIMessages  := nil;
  FTMap        := nil;
  FTMessages   := nil;
  FLevel     := nil;
  FPlayer    := nil;
  FConfig    := nil;
  FBreakLoop := False;
end;

procedure TIORL.RunLayer( aLayer : TIOLayer );
begin
  PushLayer( aLayer );
  WaitForLayer;
end;

procedure TIORL.Configure ( aLuaConfig : TLuaConfig ) ;
begin
  FConfig := aLuaConfig;
end;

procedure TIORL.SetPlayer ( aPlayer : TLuaEntityNode ) ;
begin
  FPlayer := aPlayer;
end;

procedure TIORL.SetLevel ( aLevel : TLuaMapNode ) ;
begin
  FLevel := aLevel;
end;

procedure TIORL.Msg ( const aMessage : Ansistring ) ;
begin
  if FUIMessages <> nil then FUIMessages.Add(aMessage);
  if FTMessages <> nil then FTMessages.Add(aMessage);
end;

procedure TIORL.Msg( const aMessage : Ansistring; const aParams : array of const ) ;
begin
  Msg( Format( aMessage, aParams ) );
end;

procedure TIORL.MsgKill;
begin
  if FUIMessages <> nil then FUIMessages.Pop;
  if FTMessages <> nil then FTMessages.Pop;
end;

procedure TIORL.MsgUpdate;
begin
  if FUIMessages <> nil then FUIMessages.Update;
  if FTMessages <> nil then FTMessages.Update;
end;

procedure TIORL.MsgDump ( var iTextFile : Text; iLastCount : DWord ) ;
var iCount   : Word;
    iMessage : AnsiString;
begin
  if FUIMessages <> nil then
  for iCount := Min( iLastCount, FUIMessages.Content.Size ) downto 1 do
  begin
    iMessage := ChunkListToString( FUIMessages.Content[-iCount] );
    if iMessage <> '' then
      Writeln(iTextFile,' '+iMessage);
  end;
  if FTMessages <> nil then
  for iCount := Min( iLastCount, FTMessages.Size ) downto 1 do
  begin
    iMessage := FTMessages.Content[-iCount];
    if iMessage <> '' then
      Writeln(iTextFile,' '+iMessage);
  end;
end;

function TIORL.GetCommand : Byte;
var iSpecial    : Variant;
begin
  Assert( FConfig <> nil );
  GetCommand := WaitForCommand([]);
  MsgUpdate;

  if GetCommand = COMMAND_INVALID then
  begin
    iSpecial := FConfig.RunKey( IOKeyCodeToString( FKeyCode ) );
    if VarIsOrdinal(iSpecial) and (not VarIsType( iSpecial, varBoolean ) ) then GetCommand := iSpecial;
  end;

  Exit( GetCommand );
end;

function TIORL.WaitForCommand ( const aSet : TCommandSet ) : Byte;
var iCommand : Byte;
    iEvent   : TIOEvent;
begin
  Assert( FConfig <> nil );
  repeat
    iCommand := 0;
    if not WaitForKeyEvent( iEvent ) then Exit( 0 );
    if (iEvent.EType = VEVENT_SYSTEM) and (iEvent.System.Code = VIO_SYSEVENT_QUIT) then Exit( COMMAND_SYSQUIT );
    FKeyCode := IOKeyEventToIOKeyCode( iEvent.Key );
    iCommand := FConfig.Commands[ FKeyCode ];
    if (aSet = []) and ((FKeyCode mod 256) <> 0) then Exit( iCommand );
  until (iCommand in aSet);
  Exit( iCommand )
end;

function TIORL.WaitForKey ( const aSet : TKeySet ) : Byte;
var iEvent : TIOEvent;
begin
  repeat
    if not WaitForKeyEvent( iEvent ) then Exit( 0 );
    if (iEvent.EType = VEVENT_SYSTEM) and (iEvent.System.Code = VIO_SYSEVENT_QUIT) then Exit( COMMAND_SYSQUIT );
    if aSet = [] then Exit( iEvent.Key.Code );
  until iEvent.Key.Code in aSet;
  Exit( iEvent.Key.Code );
end;


function TIORL.WaitForKeyEvent ( out aEvent : TIOEvent ) : Boolean;
var iEndLoop : TIOEventTypeSet;
begin
  FBreakLoop := False;
  iEndLoop := [VEVENT_KEYDOWN];
  repeat
    repeat
      FullUpdate;
      FIODriver.Sleep(10);
    until FIODriver.EventPending and FIODriver.PollEvent( aEvent );
    if FUIRoot.OnEvent( aEvent ) then aEvent.EType := VEVENT_KEYUP;
    if (aEvent.EType = VEVENT_SYSTEM) and (aEvent.System.Code = VIO_SYSEVENT_QUIT) then Exit( True );
    if FBreakLoop then Exit( False );
  until aEvent.EType in iEndLoop;
  Exit( True );
end;

procedure TIORL.BreakKeyLoop;
begin
  FBreakLoop := True;
end;

procedure TIORL.MarkTile ( aCoord : TCoord2D; aSign : char; aColor : byte ) ;
begin
  if FUIMap <> nil then FUIMap.Mark( aCoord, aSign, aColor );
  if FTMap <> nil then FTMap.Mark( aCoord, aSign, aColor );
end;

procedure TIORL.MarkClear;
begin
  if FUIMap <> nil then FUIMap.ClearMarks;
  if FTMap <> nil then FTMap.ClearMarks;
end;

procedure TIORL.AddMarkAnimation ( aCoord : TCoord2D; aSign : char; aColor : TIOColor; aDuration : DWord; aDelay : DWord ) ;
begin
  if FUIMap <> nil then FUIMap.AddAnimation( TConUIMarkAnimation.Create( aCoord, IOGylph( aSign, aColor ), aDuration, aDelay ) );
  if FTMap <> nil then FTMap.AddAnimation( TTextMarkAnimation.Create( aCoord, IOGylph( aSign, aColor ), aDuration, aDelay ) );
end;

procedure TIORL.AddBulletAnimation ( aFrom, aTo : TCoord2D; aSign : char; aColor : TIOColor; aDuration : DWord; aDelay : DWord ) ;
begin
  if FUIMap <> nil then FUIMap.AddAnimation( TConUIBulletAnimation.Create( FLevel, aFrom, aTo, IOGylph( aSign, aColor ), aDuration, aDelay ) );
  if FTMap <> nil then FTMap.AddAnimation( TTextBulletAnimation.Create( FLevel, aFrom, aTo, IOGylph( aSign, aColor ), aDuration, aDelay ) );
end;

procedure TIORL.AddExplodeAnimation ( aCoord : TCoord2D; const aArray : TTextExplosionArray; aDelay : DWord ) ;
begin
  if FUIMap <> nil then FUIMap.AddAnimation( TConUIExplosionAnimation.Create( aCoord, '*', aArray, aDelay ) );
  if FTMap <> nil then FTMap.AddAnimation( TTextExplosionAnimation.Create( aCoord, '*', aArray, aDelay ) );
end;

procedure TIORL.ClearAnimations;
begin
  if FUIMap <> nil then FUIMap.ClearAnimations;
  if FTMap <> nil then FTMap.ClearAnimations;
end;

procedure TIORL.WaitForAnimations;
begin
  if FUIMap <> nil then
  while not FUIMap.AnimationsFinished do
  begin
    FIODriver.Sleep(10);
    FullUpdate;
  end;
  if FTMap <> nil then
  while not FTMap.AnimationsFinished do
  begin
    FIODriver.Sleep(10);
    FullUpdate;
  end;
end;

procedure TIORL.Explosion ( aWhere : TCoord2D; aColor : byte; aRange : byte;
  aDrawDelay : Word; aDelay : Word ) ;
var iExpl     : TTextExplosionArray;
    iCoord    : TCoord2D;
    iDistance : DWord;
begin
  if FUIMap <> nil then FUIMap.FreezeMarks;
  if FTMap <> nil then FTMap.FreezeMarks;
  SetLength( iExpl, 4 );
  iExpl[0].Time := aDrawDelay;
  iExpl[1].Time := aDrawDelay;
  iExpl[2].Time := aDrawDelay;
  iExpl[3].Time := aDrawDelay;
  case aColor of
    Blue    : begin iExpl[3].Color := Blue;    iExpl[0].Color := LightBlue;  iExpl[1].Color := White; end;
    Magenta : begin iExpl[3].Color := Magenta; iExpl[0].Color := Red;        iExpl[1].Color := Blue; end;
    Green   : begin iExpl[3].Color := Green;   iExpl[0].Color := LightGreen; iExpl[1].Color := White; end;
    LightRed: begin iExpl[3].Color := LightRed;iExpl[0].Color := Yellow;     iExpl[1].Color := White; end;
     else     begin iExpl[3].Color := Red;     iExpl[0].Color := LightRed;   iExpl[1].Color := Yellow; end;
  end;
  iExpl[2].Color := iExpl[0].Color;

  for iCoord in NewArea( aWhere, aRange ).Clamped( FLevel.Area ) do
  begin
    if not FLevel.isVisible( iCoord ) then Continue;
    iDistance := Distance( iCoord, aWhere );
    if iDistance > aRange then Continue;
    if not FLevel.isEyeContact( iCoord, aWhere ) then Continue;
    AddExplodeAnimation( iCoord, iExpl, iDistance*aDrawDelay+aDelay );
  end;
  if FUIMap <> nil then FUIMap.AddAnimation( TConUIClearMarkAnimation.Create( aRange*aDrawDelay+aDelay ) );
  if FTMap <> nil then FTMap.AddAnimation( TTextClearMarkAnimation.Create( aRange*aDrawDelay+aDelay ) );
end;

procedure TIORL.FocusCursor ( aCoord : TCoord2D ) ;
var iPoint : TUIPoint;
begin
  if FUIMap <> nil then
  begin
    iPoint := FUIMap.Screen( aCoord );
    FConsole.MoveCursor( iPoint.X, iPoint.Y );
  end;
  if FTMap <> nil then
  begin
    iPoint := FTMap.Screen( aCoord );
    FConsole.MoveCursor( iPoint.X, iPoint.Y );
  end;
end;

procedure TIORL.ShowCursor;
begin
  FConsole.ShowCursor;
end;

procedure TIORL.HideCursor;
begin
  FConsole.HideCursor;
end;

function TIORL.IOKeyCodeToCommand ( aKey : TIOKeyCode ) : Byte;
begin
  Exit( FConfig.Commands[ aKey ] );
end;


destructor TIORL.Destroy;
begin
  IORL := nil;
  inherited Destroy;
end;

function lua_iorl_msg(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
begin
  if IORL = nil then Exit(0);
  iState.Init(L);
  if iState.StackSize < 1 then Exit( 0 );
  IORL.Msg( Capitalized( iState.ToString(1) ) );
  Result := 0;
end;

function lua_iorl_msg_enter(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
begin
  if IORL = nil then Exit(0);
  iState.Init(L);
  IORL.Msg( iState.ToString(1) + ' Press <@<Enter@>>...' );
  IORL.WaitForKey( [ VKEY_ENTER ] );
  IORL.MsgUpdate;
  Result := 0;
end;

function lua_iorl_delay(L: Plua_State): Integer; cdecl;
var State : TLuaState;
begin
  if IORL = nil then Exit(0);
  State.Init(L);
  if State.StackSize = 0 then Exit(0);
  IORL.Delay(State.ToInteger(1));
  Result := 0;
end;

const lua_iorl_lib : array[0..3] of luaL_Reg = (
  ( name : 'msg';                func : @lua_iorl_msg),
  ( name : 'msg_enter';          func : @lua_iorl_msg_enter),
  ( name : 'delay';              func : @lua_iorl_delay),
  ( name : nil;                  func : nil; )
);

class procedure TIORL.RegisterLuaAPI ( const aTableName : AnsiString ) ;
begin
  LuaSystem.Register( aTableName, lua_iorl_lib );
end;

class procedure TIORL.RegisterLuaAPI ( State : TLuaState;
  const aTableName : AnsiString ) ;
begin
  State.Register( aTableName, lua_iorl_lib );
end;

function TIORL.GetMapShift : TUIPoint;
begin
  if FUIMap <> nil then Exit( FUIMap.Shift );
  if FTMap <> nil then Exit( FTMap.Shift );
  Exit( Point(0,0) );
end;


end.

