{$INCLUDE valkyrie.inc}
unit vcursesio;
interface
uses Classes, SysUtils, vutil, viotypes, vioevent;

type TCursesIODriver = class( TIODriver )
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
private
  FStartTime    : TDateTime;
  FSizeX        : DWord;
  FSizeY        : DWord;
  FLastGet      : LongInt;
end;

implementation

uses
  nCurses, math, dateutils;

constructor TCursesIODriver.Create( aCols : Word = 80; aRows : Word = 25; aMouse : Boolean = False );
var i,c : Word;
begin
  inherited Create;
  ClearInterrupts;
  Log('Initializing CursesIO driver...');

  initscr();
  resize_term( aRows, aCols );
  raw();
  cbreak();
  noecho();

  nodelay  ( stdscr, true );
  intrflush( stdscr, false );
  keypad   ( stdscr, true );
  ESCDELAY := 0;


  start_color();
  FSizeX := aCols;
  FSizeY := aRows;
//  PDC_save_key_modifiers( true );
  c := COLOR_BLACK;
  for i := 0 to 8 do 
  begin
    case i of 
    1 : c := COLOR_BLUE;
    2 : c := COLOR_GREEN;
    3 : c := COLOR_CYAN;
    4 : c := COLOR_RED;
    5 : c := COLOR_MAGENTA;
    6 : c := COLOR_YELLOW;
    7 : c := COLOR_WHITE;
    8 : c := COLOR_BLACK;
    end;
    init_pair( 1 + 8 * i, COLOR_BLUE, c );
    init_pair( 2 + 8 * i, COLOR_GREEN, c );
    init_pair( 3 + 8 * i, COLOR_CYAN, c );
    init_pair( 4 + 8 * i, COLOR_RED, c );
    init_pair( 5 + 8 * i, COLOR_MAGENTA, c );
    init_pair( 6 + 8 * i, COLOR_YELLOW, c );
    init_pair( 7 + 8 * i, COLOR_WHITE, c );
  end;

  init_pair( 8 * 8, COLOR_BLACK, COLOR_BLACK );


  clear();
  refresh();
  curs_set(1);

  FStartTime    := Now;
end;

procedure TCursesIODriver.Sleep ( Milliseconds : DWord ) ;
begin
  SysUtils.Sleep( Milliseconds );
end;

function TCursesIODriver.GetMs : DWord;
begin
  Exit( MilliSecondsBetween(Now,FStartTime) );
end;

procedure TCursesIODriver.Update;
begin
end;

procedure TCursesIODriver.PreUpdate;
begin
end;

procedure TCursesIODriver.PostUpdate;
begin

end;

function CursesToKeyCode( CursesResult : LongInt ) : Byte;
begin
  Result     := VKEY_NONE;
  case CursesResult of
    nCurses.KEY_BACKSPACE : Result := VKEY_BACK;

    9                     : Result := VKEY_TAB;
    nCurses.KEY_STAB      : Result := VKEY_TAB;

    10                    : Result := VKEY_ENTER;
    nCurses.KEY_ENTER     : Result := VKEY_ENTER;

    27                    : Result := VKEY_ESCAPE;
    nCurses.KEY_CANCEL    : Result := VKEY_ESCAPE;

    nCurses.KEY_UP    : Result := VKEY_UP;
    nCurses.KEY_DOWN  : Result := VKEY_DOWN;
    nCurses.KEY_LEFT  : Result := VKEY_LEFT;
    nCurses.KEY_RIGHT : Result := VKEY_RIGHT;

    nCurses.KEY_PPAGE : Result := VKEY_PGUP;
    nCurses.KEY_A3    : Result := VKEY_PGUP;
    nCurses.KEY_NPAGE : Result := VKEY_PGDOWN;
    nCurses.KEY_C3    : Result := VKEY_PGDOWN;
    nCurses.KEY_HOME  : Result := VKEY_HOME;
    nCurses.KEY_A1    : Result := VKEY_HOME;
    nCurses.KEY_END   : Result := VKEY_END;
    nCurses.KEY_C1    : Result := VKEY_END;
    nCurses.KEY_B2    : Result := VKEY_CENTER;
    nCurses.KEY_DC    : Result := VKEY_DELETE;
    nCurses.KEY_IC    : Result := VKEY_INSERT;

    nCurses.KEY_F1    : Result := VKEY_F1;
    nCurses.KEY_F2    : Result := VKEY_F2;
    nCurses.KEY_F3    : Result := VKEY_F3;
    nCurses.KEY_F4    : Result := VKEY_F4;
    nCurses.KEY_F5    : Result := VKEY_F5;
    nCurses.KEY_F6    : Result := VKEY_F6;
    nCurses.KEY_F7    : Result := VKEY_F7;
    nCurses.KEY_F8    : Result := VKEY_F8;
    nCurses.KEY_F9    : Result := VKEY_F9;
    nCurses.KEY_F10   : Result := VKEY_F10;
    nCurses.KEY_F11   : Result := VKEY_F11;
    nCurses.KEY_F12   : Result := VKEY_F12;
  else
    if (CursesResult < 256) and (Ord(CursesResult) in VKEY_SCANSET) then
      Exit( CursesResult );
  end;
end;

function TCursesIODriver.PollEvent ( out aEvent : TIOEvent ) : Boolean;
begin
  // Sleep so we don't get 100% processor usage
  SysUtils.Sleep(1);

  // Get value from curses
  FLastGet := wgetch(stdscr);

  // If value is err, return none event
  if FLastGet = -1 then Exit( False );

  // Zero the fields
  if ( FLastGet >= 32 ) and ( FLastGet < 128 ) then
  begin
    aEvent.Key.ASCII := Char(FLastGet);
    if Ord(aEvent.Key.ASCII) in VKEY_PRINTABLESET then
    begin
      aEvent := PrintableToIOEvent( aEvent.Key.ASCII );
      Exit( True );
    end;
  end;
  aEvent.EType        := VEVENT_KEYDOWN;
  aEvent.Key.ASCII    := #0;
  aEvent.Key.Code     := CursesToKeyCode( FLastGet );
  aEvent.Key.Pressed  := True;
  aEvent.Key.ModState := [];

  // Check the control and shift states
  // TODO: obviously there is an ERROR here :P
//  kevent.key.control = (PDC_get_key_modifiers() & 2) != 0;
//  kevent.key.shift   = (PDC_get_key_modifiers() & 2) != 0;

  Exit( True );
end;

function TCursesIODriver.PeekEvent ( out aEvent : TIOEvent ) : Boolean;
begin
  if PollEvent( aEvent ) then
  begin
    ungetch( FLastGet );
    Exit( True );
  end;
  Exit( False );
end;

function TCursesIODriver.EventPending : Boolean;
var iEvent : TIOEvent;
begin
  Result := PeekEvent( iEvent );
end;

procedure TCursesIODriver.SetEventMask ( aMask : TIOEventType ) ;
begin
  // ignored for now
end;

destructor TCursesIODriver.Destroy;
begin
  Log('De-initializing CursesIO driver...');
  nCurses.endwin();
  inherited Destroy;
end;

function TCursesIODriver.GetSizeX : DWord;
begin
  Result := FSizeX;
end;

function TCursesIODriver.GetSizeY : DWord;
begin
  Result := FSizeY;
end;

function TCursesIODriver.GetMousePos ( out aResult : TIOPoint ) : Boolean;
begin
  Exit( False );
end;

function TCursesIODriver.GetMouseButtonState ( out aResult : TIOMouseButtonSet
  ) : Boolean;
begin
  Exit( False );
end;

function TCursesIODriver.GetModKeyState : TIOModKeySet;
begin
  Result := [];
end;

procedure TCursesIODriver.SetTitle ( const aLongTitle : AnsiString;
  const aShortTitle : AnsiString ) ;
begin
end;

end.

