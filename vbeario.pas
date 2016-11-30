unit vbeario;

{$mode objfpc}

interface

uses
  Classes, SysUtils, viotypes, vioevent;

type TBearFlag  = ( Bear_FullScreen, Bear_Resizable );
     TBearFlags = set of TBearFlag;

type

{ TBearIODriver }

 TBearIODriver = class( TIODriver )
  constructor Create( aWidth, aHeight : Word; aFlags : TBearFlags );
  destructor Destroy; override;
  function EventPending: Boolean; override;
  function GetModKeyState: TIOModKeySet; override;
  function GetMouseButtonState(out aResult: TIOMouseButtonSet): Boolean;
    override;
  function GetMousePos(out aResult: TIOPoint): Boolean; override;
  function GetMs: DWord; override;
  function GetSizeX: DWord; override;
  function GetSizeY: DWord; override;
  function PeekEvent(out aEvent: TIOEvent): Boolean; override;
  function PollEvent(out aEvent: TIOEvent): Boolean; override;
  procedure PostUpdate; override;
  procedure PreUpdate; override;
  procedure SetEventMask(aMask: TIOEventType); override;
  procedure SetTitle(const aLongTitle: AnsiString; const aShortTitle: AnsiString
    =''); override;
  procedure Sleep(Milliseconds: DWord); override;
private
  FStartTime    : TDateTime;
  FFScreen  : Boolean;
  FSizeX    : DWord;
  FSizeY    : DWord;
public
  property Width : DWord        read FSizeX;
  property Height : DWord       read FSizeY;
  property FullScreen : Boolean read FFScreen;
end;

var BearIO : TBearIODriver = nil;

implementation

uses BearLibTerminal, dateutils, vutil;

function bearKeyToVKey(key : integer): byte;
begin
  case (key) of
    TK_A : exit( VKEY_A );
    TK_B : exit( VKEY_B );
    TK_C : exit( VKEY_C );
    TK_D : exit( VKEY_D );
    TK_E : exit( VKEY_E );
    TK_F : exit( VKEY_F );
    TK_G : exit( VKEY_G );
    TK_H : exit( VKEY_H );
    TK_I : exit( VKEY_I );
    TK_J : exit( VKEY_J );
    TK_K : exit( VKEY_K );
    TK_L : exit( VKEY_L );
    TK_M : exit( VKEY_M );
    TK_N : exit( VKEY_N );
    TK_O : exit( VKEY_O );
    TK_P : exit( VKEY_P );
    TK_Q : exit( VKEY_Q );
    TK_R : exit( VKEY_R );
    TK_S : exit( VKEY_S );
    TK_T : exit( VKEY_T );
    TK_U : exit( VKEY_U );
    TK_V : exit( VKEY_V );
    TK_W : exit( VKEY_W );
    TK_X : exit( VKEY_X );
    TK_Y : exit( VKEY_Y );
    TK_Z : exit( VKEY_Z );
    TK_1 : exit( VKEY_1 );
    TK_2 : exit( VKEY_2 );
    TK_3 : exit( VKEY_3 );
    TK_4 : exit( VKEY_4 );
    TK_5 : exit( VKEY_5 );
    TK_6 : exit( VKEY_6 );
    TK_7 : exit( VKEY_7 );
    TK_8 : exit( VKEY_8 );
    TK_9 : exit( VKEY_9 );
    TK_0 : exit( VKEY_0 );
    TK_ENTER : exit( VKEY_ENTER );
    TK_ESCAPE : exit( VKEY_ESCAPE );
    TK_BACKSPACE : exit( VKEY_BACK );
    TK_TAB : exit( VKEY_TAB );
    TK_SPACE : exit( VKEY_SPACE );
    TK_MINUS : exit( VKEY_MINUS );
    TK_EQUALS : exit( VKEY_EQUALS );
    TK_LBRACKET : exit( VKEY_LBRACKET );
    TK_RBRACKET : exit( VKEY_RBRACKET );
    TK_BACKSLASH : exit( VKEY_BSLASH );
    TK_SEMICOLON : exit( VKEY_SCOLON );
    TK_APOSTROPHE : exit( VKEY_QUOTE );
    TK_GRAVE : exit( VKEY_BQUOTE );
    TK_COMMA : exit( VKEY_COMMA );
    TK_PERIOD : exit( VKEY_PERIOD );
    TK_SLASH : exit( VKEY_SLASH );
    TK_F1 : exit( VKEY_F1 );
    TK_F2 : exit( VKEY_F2 );
    TK_F3 : exit( VKEY_F3 );
    TK_F4 : exit( VKEY_F4 );
    TK_F5 : exit( VKEY_F5 );
    TK_F6 : exit( VKEY_F6 );
    TK_F7 : exit( VKEY_F7 );
    TK_F8 : exit( VKEY_F8 );
    TK_F9 : exit( VKEY_F9 );
    TK_F10 : exit( VKEY_F10 );
    TK_F11 : exit( VKEY_F11 );
    TK_F12 : exit( VKEY_F12 );
    TK_INSERT : exit( VKEY_INSERT );
    TK_HOME : exit( VKEY_HOME );
    TK_PAGEUP : exit( VKEY_PGUP );
    TK_DELETE : exit( VKEY_DELETE );
    TK_END : exit( VKEY_END );
    TK_PAGEDOWN : exit( VKEY_PGDOWN );
    TK_RIGHT : exit( VKEY_RIGHT );
    TK_LEFT : exit( VKEY_LEFT );
    TK_DOWN : exit( VKEY_DOWN );
    TK_UP : exit( VKEY_UP );
    TK_KP_DIVIDE : exit( VKEY_SLASH );
    TK_KP_MINUS : exit( VKEY_MINUS );
    TK_KP_PLUS : exit( VKEY_EQUALS );
    TK_KP_ENTER : exit( VKEY_ENTER );
    TK_KP_1 : exit( VKEY_END );
    TK_KP_2 : exit( VKEY_DOWN );
    TK_KP_3 : exit( VKEY_PGDOWN );
    TK_KP_4 : exit( VKEY_LEFT );
    TK_KP_5 : exit( VKEY_CENTER );
    TK_KP_6 : exit( VKEY_RIGHT );
    TK_KP_7 : exit( VKEY_HOME );
    TK_KP_8 : exit( VKEY_UP );
    TK_KP_9 : exit( VKEY_PGUP );
    TK_KP_0 : exit( VKEY_0 );
    TK_KP_PERIOD : exit( VKEY_PERIOD );
  end;
  exit( VKEY_NONE );
end;

function bearEventToIOEvent(key : integer; released : boolean): TIOEvent;
var ASCII : char;
begin
  ASCII := char(terminal_state(TK_CHAR));
  if Ord(ASCII) in VKEY_PRINTABLESET then
  begin
    Result := PrintableToIOEvent( ASCII );
    if ( released )
      then Result.EType := VEVENT_KEYUP;
    Exit;
  end;

  Result.Key.ASCII    := #0;
  Result.Key.Code     := bearKeyToVKey( key );
  if ( released ) then
    Result.EType := VEVENT_KEYUP
  else
    Result.EType := VEVENT_KEYDOWN;
end;

function bearEventToMouseEvent( key: integer; released : boolean ): TIOEvent;
begin
  Result.EType := VEVENT_MOUSEMOVE;
  Result.MouseMove.Pos.X := terminal_state(TK_MOUSE_PIXEL_X);
  Result.MouseMove.Pos.Y := terminal_state(TK_MOUSE_PIXEL_Y);
  Result.MouseMove.RelPos.X := terminal_state(TK_MOUSE_PIXEL_X);
  Result.MouseMove.RelPos.Y := terminal_state(TK_MOUSE_PIXEL_Y);
end;

{ TBearIODriver }

constructor TBearIODriver.Create(aWidth, aHeight : Word; aFlags : TBearFlags );
var Size : string;
begin
  BearIO := Self;
  inherited Create;
  FFScreen := Bear_FullScreen in aFlags;
  Size := inttostr(aWidth)+'x'+inttostr(aHeight);
  Log('Initializing BearIO driver (' + size + ')...' );
  terminal_open();
  terminal_set('window: client-size='+Size+', cellsize=10x18');
  if FFScreen then
     terminal_set('window.fullscreen=true');
  terminal_set('input.filter="keyboard"');
  terminal_set('0x0020: font10x18.png, size=10x18, transparent=auto');
  terminal_set('0x0120: exocet.png, size=10x18');
  terminal_composition(TK_OFF);
  terminal_refresh();
  FStartTime := Now;
  FSizeX := aWidth;
  FSizeY := aHeight;
end;

destructor TBearIODriver.Destroy;
begin
  terminal_close();
  inherited Destroy;
end;

function TBearIODriver.EventPending: Boolean;
begin
  exit( terminal_has_input() );
end;

function TBearIODriver.GetModKeyState: TIOModKeySet;
begin
  Result := [];
  if ( terminal_check(TK_SHIFT) ) then Include( Result, VKMOD_SHIFT );
  if ( terminal_check(TK_CONTROL) ) then Include( Result, VKMOD_CTRL );
end;

function TBearIODriver.GetMouseButtonState(out aResult: TIOMouseButtonSet
  ): Boolean;
begin
  Exit( True );
end;

function TBearIODriver.GetMousePos(out aResult: TIOPoint): Boolean;
begin
  aResult := Point(terminal_state(TK_MOUSE_X), terminal_state(TK_MOUSE_Y));
  Exit( True );
end;

function TBearIODriver.GetMs: DWord;
begin
  Exit( MilliSecondsBetween(Now,FStartTime) );
end;

function TBearIODriver.GetSizeX: DWord;
begin
  Result := FSizeX;
end;

function TBearIODriver.GetSizeY: DWord;
begin
  Result := FSizeY;
end;

function TBearIODriver.PeekEvent(out aEvent: TIOEvent): Boolean;
var key: integer;
    released : boolean;
begin
  key := terminal_peek();
  if (key = 0) then Exit ( False );
  released := longbool(key and TK_KEY_RELEASED);
  if ( released ) then
    key := key and not TK_KEY_RELEASED;
  aEvent := bearEventToIOEvent(key, released);
  aEvent.Key.ModState := GetModKeyState;
  Result := true;
end;

function TBearIODriver.PollEvent(out aEvent: TIOEvent): Boolean;
var key: integer;
    released : boolean;
begin
  if ( not terminal_has_input() ) then Exit(false);
  key := terminal_read();
  if ( key = TK_MOUSE_MOVE ) then Exit(false);
  released := longbool(key and TK_KEY_RELEASED);
  if ( released ) then
    key := key and not TK_KEY_RELEASED;
  if ( key = TK_MOUSE_MOVE ) then
  begin
     aEvent := bearEventToMouseEvent(key, released);
     exit(false);
  end;
  aEvent := bearEventToIOEvent(key, released);
  aEvent.Key.ModState := GetModKeyState;
  Result := true;
end;

procedure TBearIODriver.PostUpdate;
begin
  terminal_refresh();
end;

procedure TBearIODriver.PreUpdate;
begin

end;

procedure TBearIODriver.SetEventMask(aMask: TIOEventType);
begin

end;

procedure TBearIODriver.SetTitle(const aLongTitle: AnsiString;
  const aShortTitle: AnsiString);
begin
  terminal_set('window.title='+aLongTitle);
end;

procedure TBearIODriver.Sleep(Milliseconds: DWord);
begin
  terminal_delay( Milliseconds );
end;



end.


