{$INCLUDE valkyrie.inc}
unit vioevent;
interface
uses Classes, SysUtils, vutil;

const
{$INCLUDE vioevent.inc}

const VKEY_ARROWSET       = [VKEY_HOME,VKEY_END,VKEY_PGDOWN,VKEY_PGUP,
                             VKEY_LEFT,VKEY_RIGHT,VKEY_UP,VKEY_DOWN];
      VKEY_CONTROLSET     = [VKEY_BACK,VKEY_TAB,VKEY_ENTER,VKEY_DELETE,VKEY_INSERT,VKEY_CENTER,VKEY_ESCAPE,VKEY_F1..VKEY_F12];
type  VKEY_PRINTABLERANGE = 32..126;
const VKEY_PRINTABLESET   = [32..126];
      VKEY_SCANSET        = [VKEY_SPACE,VKEY_QUOTE,VKEY_COMMA,VKEY_MINUS,VKEY_PERIOD,VKEY_SLASH,
                             VKEY_0..VKEY_9, VKEY_SCOLON, VKEY_EQUALS, VKEY_LBRACKET, VKEY_BSLASH, VKEY_RBRACKET, VKEY_BQUOTE,
                             VKEY_A..VKEY_Z ];
      VKEY_FULLSET        = VKEY_ARROWSET + VKEY_SCANSET + VKEY_CONTROLSET;

// Aliases
const VKEY_UPRIGHT   = VKEY_PGUP;
      VKEY_DOWNRIGHT = VKEY_PGDOWN;
      VKEY_UPLEFT    = VKEY_HOME;
      VKEY_DOWNLEFT  = VKEY_END;

type TIOEventType      = ( VEVENT_SYSTEM, VEVENT_KEYDOWN, VEVENT_KEYUP, VEVENT_MOUSEMOVE, VEVENT_MOUSEDOWN, VEVENT_MOUSEUP );
     TIOEventTypeSet   = set of TIOEventType;
     TIOModKey         = ( VKMOD_UNKNOWN, VKMOD_CTRL, VKMOD_SHIFT, VKMOD_ALT );
     TIOModKeySet      = set of TIOModKey;
     TIOMouseButton    = ( VMB_UNKNOWN, VMB_BUTTON_LEFT, VMB_BUTTON_MIDDLE, VMB_BUTTON_RIGHT, VMB_WHEEL_UP, VMB_WHEEL_DOWN );
     TIOMouseButtonSet = set of TIOMouseButton;
     TIOKeyEvent       = record Code : Byte; ASCII : Char; ModState : TIOModKeySet; Pressed : Boolean; end;
     TIOMouseEvent     = record Button : TIOMouseButton; Pos : TPoint; Pressed : Boolean; end;
     TIOMouseMoveEvent = record ButtonState : TIOMouseButtonSet; Pos : TPoint; RelPos : TPoint; end;
     TIOSystemEvent    = record Code : DWord; Param1 : DWord; Param2 : DWord; end;

const VIO_SYSEVENT_NONE    = 0;
      VIO_SYSEVENT_UNKNOWN = 1;
      VIO_SYSEVENT_QUIT    = 2;
      VIO_SYSEVENT_RESIZE  = 3;
      VIO_SYSEVENT_EXPOSE  = 4;
      VIO_SYSEVENT_ACTIVE  = 5;
      VIO_SYSEVENT_WM      = 6;
      VIO_SYSEVENT_COUNT   = 6;

const IOSystemEventNames : array[0..VIO_SYSEVENT_COUNT] of AnsiString =
  ( 'NONE', 'UNKNOWN', 'QUIT', 'RESIZE', 'EXPOSE', 'ACTIVE', 'WM' );

type TIOEvent = record
  case EType : TIOEventType of
    VEVENT_SYSTEM     : ( System : TIOSystemEvent; );
    VEVENT_KEYDOWN,
    VEVENT_KEYUP      : ( Key : TIOKeyEvent; );
    VEVENT_MOUSEMOVE  : ( MouseMove : TIOMouseMoveEvent; );
    VEVENT_MOUSEDOWN,
    VEVENT_MOUSEUP    : ( Mouse : TIOMouseEvent; );
  end;

type  TIOKeyCode = Word;
const IOKeyCodeKeyMask   = %0000000011111111;
      IOKeyCodeShiftMask = %0000000100000000;
      IOKeyCodeAltMask   = %0000001000000000;
      IOKeyCodeCtrlMask  = %0000010000000000;
      IOKeyCodeModMask   = %0000011100000000;
      IOKeyCodeMax       = %0000011111111111;

function IOModState( aShift, aCtrl, aAlt : Boolean ) : TIOModKeySet;
function IOModState( aWord : TIOKeyCode ) : TIOModKeySet;
function VKeyToString( aKey : Byte ) : AnsiString;
function VModKeyToString( aModKey : TIOModKey ) : AnsiString;
function VModKeySetToString( const aModKeySet : TIOModKeySet ) : AnsiString;
function IOKeyCodeToString( const aKey : TIOKeyCode ) : AnsiString;
function VKeyAndModToString( aKey : Byte; const aModKeySet : TIOModKeySet ) : AnsiString;
function VMBToString( aMB : TIOMouseButton ) : AnsiString;
function VMBSetToString( aMB : TIOMouseButtonSet ) : AnsiString;

function StringToVKey( const aCode : AnsiString ) : Byte;
function StringToVModKey( const aCode : AnsiString ) : TIOModKey;
function StringToVModKeySet( const aCode : AnsiString ) : TIOModKeySet;
function StringToIOKeyCode( const aCode : AnsiString ) : TIOKeyCode;
function StringToVMB( const aCode : AnsiString ) : TIOMouseButton;
function StringToVMBSet( const aCode : AnsiString ) : TIOMouseButtonSet;

function IOKeyEventToString( const aKeyEvent : TIOKeyEvent ) : AnsiString;
function IOMouseEventToString( const aMouseEvent : TIOMouseEvent ) : AnsiString;
function IOMouseMoveEventToString ( const aMouseMoveEvent : TIOMouseMoveEvent ) : AnsiString;
function IOSystemEventToString( const aSystemEvent : TIOSystemEvent ) : AnsiString;
function IOEventToString( const aEvent : TIOEvent ) : AnsiString;

function IOEventTypeToString( aType : TIOEventType ) : AnsiString;

function IOKeyCode( aKey : Byte; aModState : TIOModKeySet ) : TIOKeyCode;
function IOKeyEventToIOKeyCode( const aKeyEvent : TIOKeyEvent ) : TIOKeyCode;
function Unshift( aKey : Char ) : Char;
function PrintableToIOEvent( aKey : Char ) : TIOEvent;

implementation

uses StrUtils;

function IOModState ( aShift, aCtrl, aAlt : Boolean ) : TIOModKeySet;
begin
  IOModState := [];
  if aShift then Include( IOModState, VKMOD_SHIFT );
  if aCtrl  then Include( IOModState, VKMOD_CTRL );
  if aAlt   then Include( IOModState, VKMOD_ALT );
end;

function IOModState ( aWord : TIOKeyCode ) : TIOModKeySet;
begin
  IOModState := [];
  if aWord and IOKeyCodeModMask   = 0 then Exit;
  if aWord and IOKeyCodeShiftMask <> 0 then Include( IOModState, VKMOD_SHIFT );
  if aWord and IOKeyCodeCtrlMask  <> 0 then Include( IOModState, VKMOD_CTRL );
  if aWord and IOKeyCodeAltMask   <> 0 then Include( IOModState, VKMOD_ALT );
end;

function VKeyToString ( aKey : Byte ) : AnsiString;
begin
  case aKey of
    VKEY_NONE    : Exit( 'NONE' );
    VKEY_BACK    : Exit( 'BACKSPACE' );
    VKEY_TAB     : Exit( 'TAB' );
    VKEY_DELETE  : Exit( 'DELETE' );
    VKEY_INSERT  : Exit( 'INSERT' );
    VKEY_CENTER  : Exit( 'CENTER' );

    VKEY_QUOTE   : Exit( 'QUOTE' );
    VKEY_COMMA   : Exit( 'COMMA' );
    VKEY_MINUS   : Exit( 'MINUS' );
    VKEY_PERIOD  : Exit( 'PERIOD' );
    VKEY_SLASH   : Exit( 'SLASH' );
    VKEY_SCOLON  : Exit( 'SCOLON' );
    VKEY_EQUALS  : Exit( 'EQUALS' );
    VKEY_LBRACKET: Exit( 'LBRACKET' );
    VKEY_BSLASH  : Exit( 'BSLASH' );
    VKEY_RBRACKET: Exit( 'RBRACKET' );
    VKEY_BQUOTE  : Exit( 'BQUOTE' );

    VKEY_ENTER   : Exit( 'ENTER' );
    VKEY_PGUP    : Exit( 'PGUP' );
    VKEY_PGDOWN  : Exit( 'PGDOWN' );
    VKEY_END     : Exit( 'END' );
    VKEY_HOME    : Exit( 'HOME' );
    VKEY_LEFT    : Exit( 'LEFT' );
    VKEY_UP      : Exit( 'UP' );
    VKEY_RIGHT   : Exit( 'RIGHT' );
    VKEY_DOWN    : Exit( 'DOWN' );

    VKEY_ESCAPE  : Exit( 'ESCAPE' );

    VKEY_SPACE   : Exit( 'SPACE' );

    VKEY_0..VKEY_9   : Exit( Chr( aKey - VKEY_0  + Ord('0') ) );
    VKEY_A..VKEY_Z   : Exit( Chr( aKey - VKEY_A  + Ord('A') ) );
    VKEY_F1..VKEY_F12: Exit( 'F'+IntToStr( aKey - VKEY_F1 + 1 ) );

  else Exit('UNKNOWN('+IntToStr(aKey)+')');
  end;
end;

function VModKeyToString ( aModKey : TIOModKey ) : AnsiString;
begin
  case aModKey of
    VKMOD_CTRL    : Exit( 'CTRL' );
    VKMOD_SHIFT   : Exit( 'SHIFT' );
    VKMOD_ALT     : Exit( 'ALT' );
    VKMOD_UNKNOWN : Exit( 'UNKNOWN' );
  end;
end;

function VModKeySetToString ( const aModKeySet : TIOModKeySet ) : AnsiString;
var MKey : TIOModKey;
begin
  Result := '';
  for MKey in aModKeySet do
    Result += VModKeyToString( MKey ) + '+';
  if Length( Result ) > 0 then
    Delete( Result, Length( Result ), 1 );
end;

function IOKeyCodeToString ( const aKey : TIOKeyCode ) : AnsiString;
begin
  Result := VKeyAndModToString( aKey mod 256, IOModState( aKey ) );
end;

function VKeyAndModToString ( aKey : Byte; const aModKeySet : TIOModKeySet ) : AnsiString;
begin
  Result := VModKeySetToString( aModKeySet );
  if Length( Result ) > 0 then Result += '+';
  Result += VKeyToString( aKey );
end;

function VMBToString ( aMB : TIOMouseButton ) : AnsiString;
begin
  case aMB of
    VMB_UNKNOWN       : Exit( 'UNKNOWN' );
    VMB_BUTTON_LEFT   : Exit( 'BUTTON_LEFT' );
    VMB_BUTTON_MIDDLE : Exit( 'BUTTON_MIDDLE' );
    VMB_BUTTON_RIGHT  : Exit( 'BUTTON_RIGHT' );
    VMB_WHEEL_UP      : Exit( 'WHEEL_UP' );
    VMB_WHEEL_DOWN    : Exit( 'WHEEL_DOWN' );
  end;
end;

function VMBSetToString ( aMB : TIOMouseButtonSet ) : AnsiString;
var MB : TIOMouseButton;
begin
  Result := '';
  for MB in aMB do
    Result += VMBToString( MB ) + '+';
  if Length( Result ) > 0 then
    Delete( Result, Length( Result ), 1 );
end;

function StringToVKey ( const aCode : AnsiString ) : Byte;
var n : Byte;
begin
  if Length( aCode ) = 1 then
  begin
    case aCode[1] of
      'A'..'Z' : Exit( VKEY_A + Ord( aCode[1] ) - Ord( 'A' ) );
      '0'..'9' : Exit( VKEY_0 + Ord( aCode[1] ) - Ord( '0' ) );
    end;
    Exit( VKEY_NONE );
  end;

  if aCode[1] = 'F' then
  begin
    n := StrToInt( Copy( aCode, 2, 2 ) );
    if n in [1..12] then Exit( VKEY_F1 + n - 1 );
  end;

  if aCode = 'QUOTE'     then Exit( VKEY_QUOTE );
  if aCode = 'COMMA'     then Exit( VKEY_COMMA );

  if aCode = 'MINUS'     then Exit( VKEY_MINUS );
  if aCode = 'PERIOD'    then Exit( VKEY_PERIOD );
  if aCode = 'SLASH'     then Exit( VKEY_SLASH );
  if aCode = 'SCOLON'    then Exit( VKEY_SCOLON );
  if aCode = 'EQUALS'    then Exit( VKEY_EQUALS );
  if aCode = 'LBRACKET'  then Exit( VKEY_LBRACKET );
  if aCode = 'BSLASH'    then Exit( VKEY_BSLASH );
  if aCode = 'RBRACKET'  then Exit( VKEY_RBRACKET );
  if aCode = 'BQUOTE'    then Exit( VKEY_BQUOTE );


  if aCode = 'PERIOD'    then Exit( VKEY_PERIOD );
  if aCode = 'NONE'      then Exit( VKEY_NONE );
  if aCode = 'BACKSPACE' then Exit( VKEY_BACK );
  if aCode = 'TAB'       then Exit( VKEY_TAB );
  if aCode = 'DELETE'    then Exit( VKEY_DELETE );
  if aCode = 'INSERT'    then Exit( VKEY_INSERT );

  if aCode = 'CENTER'    then Exit( VKEY_CENTER );
  if aCode = 'ENTER'     then Exit( VKEY_ENTER );
  if aCode = 'PGUP'      then Exit( VKEY_PGUP );
  if aCode = 'PGDOWN'    then Exit( VKEY_PGDOWN );
  if aCode = 'END'       then Exit( VKEY_END );
  if aCode = 'HOME'      then Exit( VKEY_HOME );
  if aCode = 'LEFT'      then Exit( VKEY_LEFT );
  if aCode = 'UP'        then Exit( VKEY_UP );
  if aCode = 'RIGHT'     then Exit( VKEY_RIGHT );
  if aCode = 'DOWN'      then Exit( VKEY_DOWN );

  if aCode = 'ESCAPE'    then Exit( VKEY_ESCAPE );

  if aCode = 'SPACE'     then Exit( VKEY_SPACE );

  Exit(VKEY_NONE);
end;

function StringToVModKey ( const aCode : AnsiString ) : TIOModKey;
begin
  if aCode = 'ALT'   then Exit( VKMOD_ALT );
  if aCode = 'CTRL'  then Exit( VKMOD_CTRL );
  if aCode = 'SHIFT' then Exit( VKMOD_SHIFT );
  Exit( VKMOD_UNKNOWN );
end;

function StringToVModKeySet ( const aCode : AnsiString ) : TIOModKeySet;
var Value : AnsiString;
    Count : Byte;
begin
  if Pos('+', aCode ) = 0 then Exit( [ StringToVModKey( aCode ) ] );
  Count := 0;
  Result := [];
  repeat
    Inc( Count );
    Value := ExtractDelimited( Count, aCode, ['+'] );
    if Value = '' then Exit;
    Include( Result, StringToVModKey( Value ) );
  until Count = 255;
end;

function StringToIOKeyCode ( const aCode : AnsiString ) : TIOKeyCode;
var p,l : Word;
begin
  p := RPos( '+', aCode );
  if p = 0 then Exit( StringToVKey( aCode ) );
  l := Length( aCode );
  Result := IOKeyCode(
    StringToVKey( Copy( aCode, p+1, l - p ) ),
    StringToVModKeySet( Copy( aCode, 1, p-1 ) )
  );
end;

function StringToVMB( const aCode : AnsiString ) : TIOMouseButton;
begin
  if aCode = 'VMB_BUTTON_LEFT'   then Exit( VMB_BUTTON_LEFT );
  if aCode = 'VMB_BUTTON_MIDDLE' then Exit( VMB_BUTTON_MIDDLE );
  if aCode = 'VMB_BUTTON_RIGHT'  then Exit( VMB_BUTTON_RIGHT );
  if aCode = 'WHEEL_UP'          then Exit( VMB_WHEEL_UP );
  if aCode = 'WHEEL_DOWN'        then Exit( VMB_WHEEL_DOWN );
  Exit( VMB_UNKNOWN );
end;

function StringToVMBSet ( const aCode : AnsiString ) : TIOMouseButtonSet;
var Value : AnsiString;
    Count : Byte;
begin
  if Pos('+', aCode ) = 0 then Exit( [ StringToVMB( aCode ) ] );
  Count := 0;
  Result := [];
  repeat
    Inc( Count );
    Value := ExtractDelimited( Count, aCode, ['+'] );
    if Value = '' then Exit;
    Include( Result, StringToVMB( Value ) );
  until Count = 255;
end;

function IOKeyEventToString ( const aKeyEvent : TIOKeyEvent ) : AnsiString;
begin
  Exit( VKeyAndModToString( aKeyEvent.Code, aKeyEvent.ModState )+'/'+aKeyEvent.ASCII );
end;

function IOMouseEventToString ( const aMouseEvent : TIOMouseEvent ) : AnsiString;
begin
  Exit( aMouseEvent.Pos.ToString+'/'+VMBToString( aMouseEvent.Button ) )
end;

function IOMouseMoveEventToString ( const aMouseMoveEvent : TIOMouseMoveEvent ) : AnsiString;
begin
  Exit( aMouseMoveEvent.Pos.ToString+'/'+aMouseMoveEvent.RelPos.ToString+'/'+VMBSetToString( aMouseMoveEvent.ButtonState ) );
end;

function IOSystemEventToString ( const aSystemEvent : TIOSystemEvent ) : AnsiString;
begin
  if aSystemEvent.Code <= VIO_SYSEVENT_COUNT
    then Result := IOSystemEventNames[ aSystemEvent.Code ]+' : '
    else Result := 'UNKNOWN('+IntToStr( aSystemEvent.Code )+') : ';
  Result += IntToStr( aSystemEvent.Param1 )+'/'+IntToStr( aSystemEvent.Param2 );
end;

function IOEventToString ( const aEvent : TIOEvent ) : AnsiString;
begin
  Result := IOEventTypeToString( aEvent.EType );
  case aEvent.EType of
    VEVENT_SYSTEM     : Result += '['+IOSystemEventToString( aEvent.System )+']';
    VEVENT_KEYDOWN,
    VEVENT_KEYUP      : Result += '['+IOKeyEventToString( aEvent.Key )+']';
    VEVENT_MOUSEMOVE  : Result += '['+IOMouseMoveEventToString( aEvent.MouseMove )+']';
    VEVENT_MOUSEDOWN,
    VEVENT_MOUSEUP    : Result += '['+IOMouseEventToString( aEvent.Mouse )+']';
  end;
end;

function IOEventTypeToString ( aType : TIOEventType ) : AnsiString;
begin
  case aType of
    VEVENT_SYSTEM     : Exit('System');
    VEVENT_KEYDOWN    : Exit('KeyDown');
    VEVENT_KEYUP      : Exit('KeyUp');
    VEVENT_MOUSEMOVE  : Exit('MouseMove');
    VEVENT_MOUSEDOWN  : Exit('MouseDown');
    VEVENT_MOUSEUP    : Exit('MouseUp');
  end;
end;

function IOKeyCode ( aKey : Byte; aModState : TIOModKeySet ) : TIOKeyCode;
begin
  Result := aKey;
  if VKMOD_SHIFT in aModState then Result += IOKeyCodeShiftMask;
  if VKMOD_CTRL  in aModState then Result += IOKeyCodeCtrlMask;
  if VKMOD_ALT   in aModState then Result += IOKeyCodeAltMask;
end;

function IOKeyEventToIOKeyCode( const aKeyEvent : TIOKeyEvent ) : TIOKeyCode;
begin
  Exit( IOKeyCode( aKeyEvent.Code, aKeyEvent.ModState ) );
end;

const Keys32_126 =
  ' 1'+#39+'3457'+#39+'908=,-./0123456789'+
  ';;,=./2'+
  'abcdefghijklmnopqrstuvwxyz'+'[\]6-`'+
  'abcdefghijklmnopqrstuvwxyz'+'[\]`';

function Unshift ( aKey : Char ) : Char;
begin
  Unshift := Keys32_126[Ord(aKey)-31];
end;

function PrintableToIOEvent ( aKey : Char ) : TIOEvent;
begin
  Assert( Ord(aKey) in VKEY_PRINTABLESET );
  Result.EType        := VEVENT_KEYDOWN;
  Result.Key.ASCII    := aKey;
  Result.Key.Code     := Ord(Unshift( aKey ));
  Result.Key.Pressed  := True;
  if Ord(aKey) in VKEY_SCANSET then
    Result.Key.ModState := []
  else
    Result.Key.ModState := [ VKMOD_SHIFT ];
end;




end.

