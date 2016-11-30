{$INCLUDE valkyrie.inc}
unit vtextio;
interface
uses Classes, SysUtils, vutil, viotypes, vioevent;

type TTextIODriver = class( TIODriver )
  constructor Create( aCols : Word = 80; aRows : Word = 25; aMouse : Boolean = False );
  function PollEvent( out aEvent : TIOEvent ) : Boolean; override;
  function PeekEvent( out aEvent : TIOEvent ) : Boolean; override;
  function EventPending : Boolean; override;
  procedure SetEventMask( aMask : TIOEventType ); override;
  procedure Sleep( Milliseconds : DWord ); override;
  function GetMs : DWord; override;
  procedure Update;
  procedure PreUpdate; override;
  procedure PostUpdate; override;
  destructor Destroy; override;
  function GetSizeX : DWord; override;
  function GetSizeY : DWord; override;
  function GetMousePos( out aResult : TIOPoint) : Boolean; override;
  function GetMouseButtonState( out aResult : TIOMouseButtonSet) : Boolean; override;
  function GetModKeyState : TIOModKeySet; override;
  procedure SetTitle( const aLongTitle : AnsiString; const aShortTitle : AnsiString = '' ); override;
protected
  function PollKey : Cardinal;
private
  FMouse        : Boolean;
  FStartTime    : TDateTime;
  FFirstUpdate  : Boolean;
  FSizeX        : DWord;
  FSizeY        : DWord;
  FLastMousePos : TPoint;
end;

var TextIO : TTextIODriver = nil;

implementation

uses
  {$IFDEF UNIX}unix,{$ENDIF}
  {$IFDEF Windows}Windows,{$ENDIF}
  math, video, keyboard, mouse, dateutils;

const InputKeyBackSpace = $0E08;
      InputKeyEnter     = $1C0D;
      InputKeyTab       = $0F09;
      InputKeyEscape    = $011B;

function ShiftStateToModKeySet( ShiftState : Byte ) : TIOModKeySet;
begin
  case ShiftState of
    1..3  : Exit( [ VKMOD_SHIFT ] );
    4     : Exit( [ VKMOD_CTRL ] );
    5..7  : Exit( [ VKMOD_CTRL, VKMOD_SHIFT ] );
    8     : Exit( [ VKMOD_ALT ] );
    9..11 : Exit( [ VKMOD_ALT, VKMOD_SHIFT ] );
    12    : Exit( [ VKMOD_CTRL, VKMOD_ALT ] );
    13..15: Exit( [ VKMOD_SHIFT, VKMOD_ALT, VKMOD_CTRL ] );
  else
    Exit( [] );
  end;
end;

function KeyEventToCode( KeyEvent: TKeyEvent ) : Byte;
begin
  Result     := VKEY_NONE;
  KeyEvent   := KeyEvent and $0000FFFF;
  case KeyEvent of
    InputKeyEscape    : Result := VKEY_ESCAPE;
    InputKeyTab       : Result := VKEY_TAB;
    InputKeyBackspace : Result := VKEY_BACK;
    InputKeyEnter     : Result := VKEY_ENTER;
    kbdInsert         : Result := VKEY_INSERT;
    kbdDelete         : Result := VKEY_DELETE;
    kbdHome           : Result := VKEY_HOME;
    kbdEnd            : Result := VKEY_END;
    kbdPgUp           : Result := VKEY_PGUP;
    kbdPgDn           : Result := VKEY_PGDOWN;
    kbdUp             : Result := VKEY_UP;
    kbdDown           : Result := VKEY_DOWN;
    kbdLeft           : Result := VKEY_LEFT;
    kbdRight          : Result := VKEY_RIGHT;
    19456             : Result := VKEY_CENTER;
    kbdF1             : Result := VKEY_F1;
    kbdF2             : Result := VKEY_F2;
    kbdF3             : Result := VKEY_F3;
    kbdF4             : Result := VKEY_F4;
    kbdF5             : Result := VKEY_F5;
    kbdF6             : Result := VKEY_F6;
    kbdF7             : Result := VKEY_F7;
    kbdF8             : Result := VKEY_F8;
    kbdF9             : Result := VKEY_F9;
    kbdF10            : Result := VKEY_F10;
    kbdF11            : Result := VKEY_F11;
    kbdF12            : Result := VKEY_F12;
  else
    if (KeyEvent < 256) and (Ord(KeyEvent) in VKEY_SCANSET) then
      Exit( KeyEvent );
  end;
end;

function KeyEventToKeyCode( KeyEvent: TKeyEvent ) : TIOKeyCode;
begin
  Result := KeyEventToCode( KeyEvent );
  if kbCtrl  and KeyEvent <> 0 then Result += IOKeyCodeCtrlMask;
  if kbShift and KeyEvent <> 0 then Result += IOKeyCodeShiftMask;
  if kbAlt   and KeyEvent <> 0 then Result += IOKeyCodeAltMask;
end;

function KeyEventToIOEvent( KeyEvent: TKeyEvent ) : TIOEvent;
var ASCII : Char;
begin
  KeyEvent := TranslateKeyEvent(KeyEvent);
  ASCII    := GetKeyEventChar(KeyEvent);
  if Ord(ASCII) in VKEY_PRINTABLESET then Exit( PrintableToIOEvent( ASCII ) );
  Result.EType    := VEVENT_KEYDOWN;
  Result.Key.ASCII    := #0;
  Result.Key.ModState := ShiftStateToModKeySet( GetKeyEventShiftState(KeyEvent) );
  Result.Key.Code     := KeyEventToCode( KeyEvent );
end;

function MouseButtonToVMB( buttons : Word ) : TIOMouseButton;
begin
  if (buttons and MouseLeftButton   ) <> 0 then Exit( VMB_BUTTON_LEFT );
  if (buttons and MouseRightButton  ) <> 0 then Exit( VMB_BUTTON_RIGHT );
  if (buttons and MouseMiddleButton ) <> 0 then Exit( VMB_BUTTON_MIDDLE );
  Exit( VMB_UNKNOWN );
end;

function MouseButtonToVMBSet( buttons : Word ) : TIOMouseButtonSet;
begin
  Result := [];
  if (buttons and MouseLeftButton   ) <> 0 then Include( Result, VMB_BUTTON_LEFT );
  if (buttons and MouseRightButton  ) <> 0 then Include( Result, VMB_BUTTON_RIGHT );
  if (buttons and MouseMiddleButton ) <> 0 then Include( Result, VMB_BUTTON_MIDDLE );
end;


function MouseEventToIOEvent( aMouseEvent: TMouseEvent ) : TIOEvent;
begin
  case aMouseEvent.Action of
    MouseActionDown : Result.EType := VEVENT_MOUSEDOWN;
    MouseActionUp   : Result.EType := VEVENT_MOUSEUP;
    MouseActionMove : Result.EType := VEVENT_MOUSEMOVE;
  end;
  if Result.EType = VEVENT_MOUSEMOVE then
  begin
    Result.MouseMove.ButtonState := MouseButtonToVMBSet( aMouseEvent.buttons );
    Result.MouseMove.Pos.Init( aMouseEvent.x, aMouseEvent.y );
    Result.MouseMove.RelPos := Result.MouseMove.Pos - TextIO.FLastMousePos;
    TextIO.FLastMousePos := Result.MouseMove.Pos;
  end
  else
  begin
    Result.Mouse.Button := MouseButtonToVMB( aMouseEvent.buttons );
    Result.Mouse.Pos.Init( aMouseEvent.x, aMouseEvent.y );
    Result.Mouse.Pressed := aMouseEvent.Action = MouseActionDown;
  end;
end;


{ TTextIODriver }

constructor TTextIODriver.Create( aCols : Word = 80; aRows : Word = 25; aMouse : Boolean = False );
var iVideoMode : TVideoMode;
begin
  ClearInterrupts;
  TextIO := Self;
  FMouse := aMouse;

  inherited Create;
  Log('Initializing TextIO driver...');

  InitKeyboard;
  Log('Terminal Keyboard system ready.');

  InitVideo;
  ClearScreen;

  if FMouse then
  begin
    InitMouse;
    if DetectMouse > 0 then
    begin
      Log('Mouse detected!');
      Log('Terminal Mouse system ready.');
      FLastMousePos.Init( GetMouseX, GetMouseY );
    end
    else
    begin
      Log('Mouse not detected!');
      FMouse := False;
    end;
  end;

  if aCols <> 0 then
  begin
    iVideoMode.Col   := aCols;
    iVideoMode.Row   := aRows;
    iVideoMode.Color := True;
    Log('Setting terminal video to %dx%d color...',[aCols,aRows]);
    if not SetVideoMode(iVideoMode)
       then Log('Failed to set terminal %dx%d color mode.',[aCols,aRows]);
  end;

  GetVideoMode(iVideoMode);
  FSizeX := math.Min( iVideoMode.Col, aCols );
  FSizeY := math.Min( iVideoMode.Row, aRows );
  if iVideoMode.Color then
    Log('Terminal video mode - %dx%d color.',[FSizeX,FSizeY])
  else
    Log('Terminal video mode - %dx%d NO COLOR.',[FSizeX,FSizeY]);

  FFirstUpdate := True;
  FStartTime   := Now;
end;

procedure TTextIODriver.Sleep ( Milliseconds : DWord ) ;
begin
  SysUtils.Sleep( Milliseconds );
end;

function TTextIODriver.GetMs : DWord;
begin
  Exit( MilliSecondsBetween(Now,FStartTime) );
end;

procedure TTextIODriver.Update;
begin
end;

procedure TTextIODriver.PreUpdate;
begin
end;

procedure TTextIODriver.PostUpdate;
begin
  UpdateScreen( FFirstUpdate );
  FFirstUpdate := False;
end;

function TTextIODriver.PollEvent ( out aEvent : TIOEvent ) : Boolean;
var iMouseEvent : TMouseEvent;
begin
  if PollKey = 0 then
  begin
    if (not FMouse) or (not PollMouseEvent( iMouseEvent )) then Exit( False );
    GetMouseEvent( iMouseEvent );
    aEvent := MouseEventToIOEvent( iMouseEvent );
  end
  else
    aEvent := KeyEventToIOEvent( GetKeyEvent );
  Exit( True );
end;

function TTextIODriver.PeekEvent ( out aEvent : TIOEvent ) : Boolean;
var iMouseEvent : TMouseEvent;
begin
  if PollKey = 0 then
  begin
    if (not FMouse) or (not PollMouseEvent( iMouseEvent )) then Exit( False );
    aEvent := MouseEventToIOEvent( iMouseEvent );
  end
  else
    aEvent := KeyEventToIOEvent( PollKeyEvent );
  Exit( True );
end;

function TTextIODriver.EventPending : Boolean;
var iMouseEvent : TMouseEvent;
begin
  Result := (PollKey <> 0) or ( FMouse and PollMouseEvent( iMouseEvent ) );
end;

procedure TTextIODriver.SetEventMask ( aMask : TIOEventType ) ;
begin
  // ignored for now
end;

destructor TTextIODriver.Destroy;
begin
  Log('Shutting down terminal system...');
  try DoneKeyboard except on Exception do end;
  try if FMouse then DoneMouse; except on Exception do end;
  try DoneVideo except on Exception do end;
  //Unknown to myself, I have no clue why I get an access violation here...
  {$IFDEF UNIX}Shell('reset');{$ENDIF}
  inherited Destroy;
end;

function TTextIODriver.GetSizeX : DWord;
begin
  Result := FSizeX;
end;

function TTextIODriver.GetSizeY : DWord;
begin
  Result := FSizeY;
end;

function TTextIODriver.GetMousePos ( out aResult : TIOPoint ) : Boolean;
begin
  if not FMouse then Exit( False );
  aResult := vutil.Point( GetMouseX, GetMouseY );
  Exit( True );
end;

function TTextIODriver.GetMouseButtonState ( out aResult : TIOMouseButtonSet
  ) : Boolean;
begin
  if not FMouse then Exit( False );
  aResult := MouseButtonToVMBSet( GetMouseButtons );
  Exit( True );
end;

function TTextIODriver.GetModKeyState : TIOModKeySet;
begin
  Result := ShiftStateToModKeySet( GetKeyEventShiftState( PollShiftStateEvent ) );
end;

procedure TTextIODriver.SetTitle ( const aLongTitle : AnsiString;
  const aShortTitle : AnsiString ) ;
begin
  {$IFDEF WIN32}
  SetConsoleTitle(PChar(aLongTitle));
  {$ENDIF}
end;

function TTextIODriver.PollKey : Cardinal;
begin
  Result := PollKeyEvent;
  if (Result <> 0) then
  begin
    Result := TranslateKeyEvent(Result);
    if (FInterrupts[ KeyEventToCode( Result ) ] <> nil) then
    begin
      if FInterrupts[ KeyEventToCode( Result ) ](KeyEventToIOEvent( Result )) then
      begin
        GetKeyEvent;
        Exit(0);
      end;
    end;
  end;
  Exit( Result );
end;

end.

