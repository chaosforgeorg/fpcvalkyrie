{$INCLUDE valkyrie.inc}
unit vsdlio;
interface
uses Classes, SysUtils, vutil, viotypes, vsdl3library, vioevent;

type TSDLIOFlag  = ( SDLIO_OpenGL, SDLIO_FullScreen, SDLIO_Resizable, SDLIO_Gamepad );
     TSDLIOFlags = set of TSDLIOFlag;

type TSDLIODriver = class( TIODriver )
  class function GetCurrentResolution( out aResult : TIOPoint ) : Boolean;

  constructor Create( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags );
  function ResetVideoMode( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags ) : Boolean;
  function SetupOpenGL : Boolean;
  function PollEvent( out aEvent : TIOEvent ) : Boolean; override;
  function PeekEvent( out aEvent : TIOEvent ) : Boolean; override;
  function EventPending : Boolean; override;
  procedure SetEventMask( aMask : TIOEventType ); override;
  procedure Sleep( Milliseconds : DWord ); override;
  function GetMs : DWord; override;
  procedure PreUpdate; override;
  procedure PostUpdate; override;
  destructor Destroy; override;
  function GetSizeX : DWord; override;
  function GetSizeY : DWord; override;
  function GetMousePos( out aResult : TIOPoint) : Boolean; override;
  function GetMouseButtonState( out aResult : TIOMouseButtonSet) : Boolean; override;
  function GetModKeyState : TIOModKeySet; override;
  procedure SetTitle( const aLongTitle : AnsiString; const aShortTitle : AnsiString ); override;
  procedure ShowMouse( aShow : Boolean );
  procedure ScreenShot( const aFileName : AnsiString );
  function SetDisplayMode( aIndex : Integer ) : Boolean;
  procedure StartTextInput; override;
  procedure StopTextInput; override;
  function Rumble( aLow, aHigh : Word; aDuration : DWord ) : Boolean; override;
private
  FFlags     : TSDLIOFlags;
  FSizeX     : DWord;
  FSizeY     : DWord;
  FBPP       : DWord;
  FOpenGL    : Boolean;
  FFScreen   : Boolean;
  FOnResize  : TIOInterrupt;
  FWindow    : PSDL_Window;
  FGLContext : SDL_GLContext;


  FGamePadID      : DWord;
  FGamePadRumble  : Boolean;
  FGamePadSupport : Boolean;
  FGamePadIndex   : Integer;
  FGamePadHandle  : Pointer;
private
  procedure ScanDisplayModes;
  function ScanGamepads( aAllowLoop : Boolean = True ) : Boolean;
  procedure SetGamePadSupport( aValue : Boolean  );
public
  property Width : DWord            read FSizeX;
  property Height : DWord           read FSizeY;
  property BPP : DWord              read FBPP;
  property GamePadSupport : Boolean read FGamePadSupport write SetGamePadSupport;
  property OpenGLMode : Boolean read FOpenGL;
  property FullScreen : Boolean read FFScreen;
  property Flags : TSDLIOFlags  read FFlags;
  property NativeWindow : PSDL_Window read FWindow;
  property OnResizeEvent : TIOInterrupt write FOnResize;
end;

var SDLIO : TSDLIODriver = nil;

implementation

uses vdebug, vgl3library,
     vsdl3imagelibrary{$IFDEF WINDOWS}, Windows{$ENDIF};

var HackLastMouseX : Integer;
    HackLastMouseY : Integer;

function SDLSymToCode( Key : SDL_Keycode ) : Byte;
begin
  Result := VKEY_NONE;
  case Key of
    SDLK_ESCAPE         : Result := VKEY_ESCAPE;
    SDLK_TAB            : Result := VKEY_TAB;
    SDLK_BACKSPACE      : Result := VKEY_BACK;
    SDLK_RETURN         : Result := VKEY_ENTER;
    SDLK_INSERT         : Result := VKEY_INSERT;
    SDLK_DELETE         : Result := VKEY_DELETE;
    SDLK_HOME           : Result := VKEY_HOME;
    SDLK_END            : Result := VKEY_END;
    SDLK_PAGEUP         : Result := VKEY_PGUP;
    SDLK_PAGEDOWN       : Result := VKEY_PGDOWN;
    SDLK_UP             : Result := VKEY_UP;
    SDLK_DOWN           : Result := VKEY_DOWN;
    SDLK_LEFT           : Result := VKEY_LEFT;
    SDLK_RIGHT          : Result := VKEY_RIGHT;
    SDLK_F1             : Result := VKEY_F1;
    SDLK_F2             : Result := VKEY_F2;
    SDLK_F3             : Result := VKEY_F3;
    SDLK_F4             : Result := VKEY_F4;
    SDLK_F5             : Result := VKEY_F5;
    SDLK_F6             : Result := VKEY_F6;
    SDLK_F7             : Result := VKEY_F7;
    SDLK_F8             : Result := VKEY_F8;
    SDLK_F9             : Result := VKEY_F9;
    SDLK_F10            : Result := VKEY_F10;
    SDLK_F11            : Result := VKEY_F11;
    SDLK_F12            : Result := VKEY_F12;

    // TEMPORARY
    SDLK_KP_1           : Result := VKEY_END;
    SDLK_KP_2           : Result := VKEY_DOWN;
    SDLK_KP_3           : Result := VKEY_PGDOWN;
    SDLK_KP_4           : Result := VKEY_LEFT;
    SDLK_KP_5           : Result := VKEY_CENTER;
    SDLK_KP_6           : Result := VKEY_RIGHT;
    SDLK_KP_7           : Result := VKEY_HOME;
    SDLK_KP_8           : Result := VKEY_UP;
    SDLK_KP_9           : Result := VKEY_PGUP;
    SDLK_KP_ENTER       : Result := VKEY_ENTER;
  else
    if Key in VKEY_SCANSET then
      Result := Key;
  end;
end;

function SDLKeyEventToKeyCode( event : PSDL_Event ) : TIOKeyCode;
var smod : SDL_Keymod;
begin
  Result := SDLSymToCode( event^.key.key );
  smod := SDL_GetModState();
  if (smod and SDL_KMOD_CTRL)  <> 0 then Result += IOKeyCodeCtrlMask;
  if (smod and SDL_KMOD_SHIFT) <> 0 then Result += IOKeyCodeShiftMask;
  if (smod and SDL_KMOD_ALT)   <> 0 then Result += IOKeyCodeAltMask;
end;

function SDLModToModKeySet( smod : SDL_Keymod ) : TIOModKeySet;
begin
  Result := [];
  if smod = SDL_KMOD_NONE then Exit;
  if (smod and SDL_KMOD_CTRL)  <> 0 then Include( Result, VKMOD_CTRL );
  if (smod and SDL_KMOD_SHIFT) <> 0 then Include( Result, VKMOD_SHIFT );
  if (smod and SDL_KMOD_ALT)   <> 0 then Include( Result, VKMOD_ALT );
end;

function SDLKeyEventToIOEvent( event : PSDL_Event ) : TIOEvent;
const KNumerics : array[0..9] of Char = (')','!','@','#','$','%','^','&','*','(');
var iCode  : Integer;
    iShift : Boolean;
    iMod   : SDL_Keymod;
begin
  iCode  := event^.key.key;
  iMod   := SDL_GetModState();
  iShift := ( (iMod and SDL_KMOD_SHIFT) <> 0 ) or
            ( (iMod and SDL_KMOD_CAPS) <> 0 );
  Result.Key.Repeated := ( event^._type = SDL_EVENT_KEY_DOWN ) and ( event^.key.repeat_ );
  Result.Key.Pressed  := event^._type = SDL_EVENT_KEY_DOWN;
  if ( iCode >= 32 )
    and ( iCode < 127 )
    and ( iCode <> SDLK_PAGEDOWN )
    and ( iCode <> SDLK_PAGEUP )
    and ( iCode <> SDLK_HOME )
    and ( iCode <> SDLK_END )
  then
  begin
    if iShift then
    begin
        case iCode of
          Ord('a')..Ord('z') : iCode := Ord(iCode) - 32;
          Ord('0')..Ord('9') : iCode := Ord(KNumerics[ iCode - Ord('0') ] );
          Ord('`')           : iCode := Ord('~');
          Ord('-')           : iCode := Ord('_');
          Ord('=')           : iCode := Ord('+');
          Ord('\')           : iCode := Ord('|');
          Ord('[')           : iCode := Ord('{');
          Ord(']')           : iCode := Ord('}');
          Ord(';')           : iCode := Ord(':');
          Ord('''')          : iCode := Ord('"');
          Ord(',')           : iCode := Ord('<');
          Ord('.')           : iCode := Ord('>');
          Ord('/')           : iCode := Ord('?');
        end
    end;
    Result := PrintableToIOEvent( Char( iCode ) );
    Result.Key.Pressed := event^._type = SDL_EVENT_KEY_DOWN;
    if event^._type = SDL_EVENT_KEY_UP then Result.EType := VEVENT_KEYUP;
    Exit;
  end;
  if event^._type = SDL_EVENT_KEY_DOWN
    then Result.EType := VEVENT_KEYDOWN
    else Result.EType := VEVENT_KEYUP;
  Result.Key.ASCII    := #0;
  Result.Key.ModState := SDLModToModKeySet( iMod );
  Result.Key.Code     := SDLSymToCode( event^.key.key );
end;

function SDLSystemEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_SYSTEM;
  Result.System.Param1 := 0;
  Result.System.Param2 := 0;

  case event^._type of
    SDL_EVENT_QUIT           : Result.System.Code := VIO_SYSEVENT_QUIT;
    SDL_EVENT_WINDOW_EXPOSED : Result.System.Code := VIO_SYSEVENT_EXPOSE;
    SDL_EVENT_WINDOW_RESIZED :
      begin
        Result.System.Code := VIO_SYSEVENT_RESIZE;
        Result.System.Param1 := event^.window.data1;
        Result.System.Param2 := event^.window.data2;
      end;
    else
      begin
        Result.System.Code := VIO_SYSEVENT_UNKNOWN;
        Result.System.Param1 := event^._type;
      end;
  end;
end;

function SDLMouseButtonToVMB( Button : Byte ) : TIOMouseButton;
begin
  case button of
    SDL_BUTTON_LEFT     : Exit( VMB_BUTTON_LEFT );
    SDL_BUTTON_MIDDLE   : Exit( VMB_BUTTON_MIDDLE );
    SDL_BUTTON_RIGHT    : Exit( VMB_BUTTON_RIGHT );
//    SDL_BUTTON_WHEELUP  : Exit( VMB_WHEEL_UP );
//    SDL_BUTTON_WHEELDOWN: Exit( VMB_WHEEL_DOWN );
  end;
  Exit( VMB_UNKNOWN );
end;

function SDLMouseButtonSetToVMB( ButtonMask : Byte ) : TIOMouseButtonSet;
begin
  Result := [];
  if (ButtonMask and SDL_BUTTON_MASK( SDL_BUTTON_LEFT ))   <> 0 then Include( Result, VMB_BUTTON_LEFT );
  if (ButtonMask and SDL_BUTTON_MASK( SDL_BUTTON_MIDDLE )) <> 0 then Include( Result, VMB_BUTTON_MIDDLE );
  if (ButtonMask and SDL_BUTTON_MASK( SDL_BUTTON_RIGHT ))  <> 0 then Include( Result, VMB_BUTTON_RIGHT );
  if (ButtonMask and SDL_BUTTON_MASK( SDL_BUTTON_X1 ))     <> 0 then Include( Result, VMB_UNKNOWN );
  if (ButtonMask and SDL_BUTTON_MASK( SDL_BUTTON_X2 ))     <> 0 then Include( Result, VMB_UNKNOWN );
end;

function SDLMouseEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  if event^._type = SDL_EVENT_MOUSE_WHEEL then
  begin
      Result.EType         := VEVENT_MOUSEDOWN;
      if event^.wheel.y > 0 then
            Result.Mouse.Button  := VMB_WHEEL_UP
      else
            Result.Mouse.Button  := VMB_WHEEL_DOWN;
      Result.Mouse.Pos.X   := HackLastMouseX;
      Result.Mouse.Pos.Y   := HackLastMouseY;
      Result.Mouse.Pressed := True;
      Exit;
  end;
  case event^._type of
    SDL_EVENT_MOUSE_BUTTON_DOWN : Result.EType := VEVENT_MOUSEDOWN;
    SDL_EVENT_MOUSE_BUTTON_UP   : Result.EType := VEVENT_MOUSEUP;
  end;
  Result.Mouse.Button  := SDLMouseButtonToVMB( event^.button.button );
  Result.Mouse.Pos.X   := Trunc( event^.button.x );
  Result.Mouse.Pos.Y   := Trunc( event^.button.y );
  Result.Mouse.Pressed := event^.button.down;
end;

function SDLPadDeviceEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_PADDEVICE;
  Result.PadDevice.Which := event^.cdevice.which;
  case event^._type of
    SDL_EVENT_GAMEPAD_ADDED    : Result.PadDevice.Event := VPAD_ADDED;
    SDL_EVENT_GAMEPAD_REMOVED  : Result.PadDevice.Event := VPAD_REMOVED;
    SDL_EVENT_GAMEPAD_REMAPPED : Result.PadDevice.Event := VPAD_REMAPPED;
  end;
end;

function SDLPadAxisEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType         := VEVENT_PADAXIS;
  Result.PadAxis.Axis  := TIOPadAxis( event^.gaxis.axis );
  Result.PadAxis.Value := event^.gaxis.value;
  Result.PadAxis.Which := event^.gaxis.which;
end;

function SDLPadEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  case event^._type of
    SDL_EVENT_GAMEPAD_BUTTON_DOWN    : Result.EType := VEVENT_PADDOWN;
    SDL_EVENT_GAMEPAD_BUTTON_UP      : Result.EType := VEVENT_PADUP;
  end;
  Result.Pad.Button  := TIOPadButton( event^.gbutton.button );
  Result.Pad.Which   := event^.gbutton.which;
  Result.Pad.Pressed := event^.gbutton.down;
end;

function SDLTextEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_TEXT;
  StrLCopy( @Result.Text.Text[0], event^.text.text, Length(Result.Text.Text));
end;

function SDLMouseMoveEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_MOUSEMOVE;
  Result.MouseMove.ButtonState := SDLMouseButtonSetToVMB( event^.motion.state );
  Result.MouseMove.Pos.X       := Trunc( event^.motion.x );
  Result.MouseMove.Pos.Y       := Trunc( event^.motion.y );
  Result.MouseMove.RelPos.X    := Trunc( event^.motion.xrel );
  Result.MouseMove.RelPos.Y    := Trunc( event^.motion.yrel );
  HackLastMouseX := Trunc( event^.motion.x );
  HackLastMouseY := Trunc( event^.motion.y );
end;

function SDLEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  case event^._type of
    SDL_EVENT_KEY_DOWN : Exit( SDLKeyEventToIOEvent( event ) );
    SDL_EVENT_KEY_UP   : Exit( SDLKeyEventToIOEvent( event ) );
    SDL_EVENT_TEXT_INPUT : Exit( SDLTextEventToIOEvent( event ) );

    SDL_EVENT_MOUSE_MOTION     : Exit( SDLMouseMoveEventToIOEvent( event ) );
    SDL_EVENT_MOUSE_BUTTON_DOWN : Exit( SDLMouseEventToIOEvent( event ) );
    SDL_EVENT_MOUSE_BUTTON_UP   : Exit( SDLMouseEventToIOEvent( event ) );
    SDL_EVENT_MOUSE_WHEEL      : Exit( SDLMouseEventToIOEvent( event ) );

    SDL_EVENT_GAMEPAD_AXIS_MOTION : Exit( SDLPadAxisEventToIOEvent( event ) );
    SDL_EVENT_GAMEPAD_BUTTON_DOWN,
    SDL_EVENT_GAMEPAD_BUTTON_UP : Exit( SDLPadEventToIOEvent( event ) );

    SDL_EVENT_GAMEPAD_ADDED,
    SDL_EVENT_GAMEPAD_REMOVED,
    SDL_EVENT_GAMEPAD_REMAPPED :
    begin
      SDLIO.ScanGamepads;
      Exit( SDLPadDeviceEventToIOEvent( event ) );
    end;
  end;
  Result.EType := VEVENT_SYSTEM;
  Result.System.Code := VIO_SYSEVENT_NONE;
end;

function SDLIOEventFilter( userdata : Pointer; event: PSDL_Event) : Boolean; cdecl;
var iCode : TIOKeyCode;
begin
  if event^._type = SDL_EVENT_QUIT then
    if Assigned( SDLIO.FOnQuit ) then
      if SDLIO.FOnQuit( SDLEventToIOEvent( event ) ) then
        Exit(False);
  if event^._type = SDL_EVENT_WINDOW_RESIZED then
    if Assigned( SDLIO.FOnResize ) then
      if SDLIO.FOnResize( SDLEventToIOEvent( event ) ) then
        Exit(False);
  if event^._type = SDL_EVENT_KEY_DOWN then
  begin
    iCode := SDLKeyEventToKeyCode( event );
    if SDLIO.FInterrupts[iCode] <> nil then
      if SDLIO.FInterrupts[iCode]( SDLKeyEventToIOEvent( event ) ) then
        Exit(False);
  end;
  case event^._type of
  SDL_EVENT_QUIT,
  SDL_EVENT_WINDOW_SHOWN,
  SDL_EVENT_WINDOW_HIDDEN,
  SDL_EVENT_WINDOW_EXPOSED,
  SDL_EVENT_WINDOW_MOVED,
  SDL_EVENT_WINDOW_RESIZED,
  SDL_EVENT_WINDOW_RESTORED,
  SDL_EVENT_KEY_DOWN,
  SDL_EVENT_KEY_UP,
  SDL_EVENT_TEXT_INPUT,
  SDL_EVENT_MOUSE_MOTION,
  SDL_EVENT_MOUSE_BUTTON_DOWN,
  SDL_EVENT_MOUSE_WHEEL : Exit(True);
  SDL_EVENT_JOYSTICK_AXIS_MOTION,
  SDL_EVENT_JOYSTICK_BALL_MOTION,
  SDL_EVENT_JOYSTICK_HAT_MOTION,
  SDL_EVENT_JOYSTICK_BUTTON_DOWN,
  SDL_EVENT_JOYSTICK_BUTTON_UP,
  SDL_EVENT_JOYSTICK_ADDED,
  SDL_EVENT_JOYSTICK_REMOVED,
  SDL_EVENT_JOYSTICK_BATTERY_UPDATED,
  SDL_EVENT_JOYSTICK_UPDATE_COMPLETE,
  SDL_EVENT_GAMEPAD_AXIS_MOTION,
  SDL_EVENT_GAMEPAD_BUTTON_DOWN,
  SDL_EVENT_GAMEPAD_BUTTON_UP,
  SDL_EVENT_GAMEPAD_ADDED,
  SDL_EVENT_GAMEPAD_REMOVED,
  SDL_EVENT_GAMEPAD_REMAPPED,
  SDL_EVENT_GAMEPAD_SENSOR_UPDATE,
  SDL_EVENT_GAMEPAD_UPDATE_COMPLETE,
  SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED : if SDLIO.GamePadSupport then Exit(True);
  end;
  Exit(True);
end;

{$IFDEF WINDOWS}
function SetProcessDPIAware: BOOL; stdcall; external 'user32.dll';

var GSetDPIAwarenessSet : Boolean = False;

procedure SetDPIAwareness;
type TProcessDpiAwareness = (
    PROCESS_DPI_UNAWARE = 0,
    PROCESS_SYSTEM_DPI_AWARE = 1,
    PROCESS_PER_MONITOR_DPI_AWARE = 2
  );
  TSetProcessDpiAwareness = function( aValue : TProcessDpiAwareness ): HRESULT; stdcall;
var iShcoreHandle    : HMODULE;
    iSetDpiAwareness : TSetProcessDpiAwareness;
begin
  if GSetDPIAwarenessSet then Exit;
  GSetDPIAwarenessSet := True;
  Log('Setting process DPI awareness...');
  iShcoreHandle := LoadLibrary( 'Shcore.dll' );
  if iShcoreHandle <> 0 then
  begin
    Pointer(iSetDpiAwareness) := GetProcAddress( iShcoreHandle, 'SetProcessDpiAwareness' );
    if Assigned( iSetDpiAwareness )
      then iSetDpiAwareness(PROCESS_PER_MONITOR_DPI_AWARE)
      else SetProcessDPIAware;
    FreeLibrary( iShcoreHandle );
  end
  else
    SetProcessDPIAware;
end;
{$ENDIF}

{ TSDLIODriver }

class function TSDLIODriver.GetCurrentResolution ( out aResult : TIOPoint ) : Boolean;
var iCurrent      : PSDL_DisplayMode;
    iDisplayIndex : SDL_DisplayID;
begin
{$IFDEF WINDOWS}
  SetDPIAwareness;
{$ENDIF}
  LoadSDL3;
  if not SDL_Init(SDL_INIT_VIDEO) then
  begin
    SDL_Quit();
    SDLIO := nil;
    raise EIOException.Create('Couldn''t initialize SDL : '+SDL_GetError());
  end;
  iDisplayIndex := SDL_GetPrimaryDisplay();
  if iDisplayIndex = 0 then
    Exit( False );
  iCurrent := SDL_GetCurrentDisplayMode( iDisplayIndex );
  if iCurrent = nil then
     Exit( False );
  aResult.Init( iCurrent^.w, iCurrent^.h );
  Exit( True );
end;

constructor TSDLIODriver.Create( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags );
begin
  ClearInterrupts;
  SDLIO := Self;
  inherited Create;
  FWindow    := nil;
  FGLContext := nil;
  FGamePadSupport := SDLIO_Gamepad in aFlags;
  FGamePadID      := 0;
  FGamePadIndex   := -1;
  FGamePadHandle  := nil;
  FGamePadRumble  := False;
  {$IFDEF WINDOWS}
  SetDPIAwareness;
  {$ENDIF}
  LoadSDL3;

  Log('Initializing SDL...');

  if not SDL_Init( SDL_INIT_VIDEO or SDL_INIT_GAMEPAD ) then
  begin
    SDL_Quit();
    SDLIO := nil;
    raise EIOException.Create('Couldn''t initialize SDL : '+SDL_GetError());
  end;

  ScanDisplayModes;

  if not ResetVideoMode( aWidth, aHeight, aBPP, aFlags ) then
  begin
    SDL_Quit();
    SDLIO := nil;
    raise EIOException.Create('Could not set '+IntToStr(aWidth)+'x'+IntToStr(aHeight)+'@'+IntToStr(aBPP)+'bpp!' );
  end;

  Log('Mode %dx%d/%d set.', [aWidth,aHeight,aBPP]);

  if SDLIO_OpenGL in aFlags then
  begin
    Log( LOGINFO, 'OpenGL Vendor       : %s', [ glGetString(GL_VENDOR) ] );
    Log( LOGINFO, 'OpenGL Renderer     : %s', [ glGetString(GL_RENDERER) ] );
    Log( LOGINFO, 'OpenGL Version      : %s', [ glGetString(GL_VERSION) ] );
    Log( LOGINFO, 'OpenGL GLSL Version : %s', [ glGetString(35724) ] );
  end;

  SDL_SetEventFilter( @SDLIOEventFilter, nil );

  if FGamePadSupport then ScanGamepads;

  Log('SDL IO system ready.');
end;

function TSDLIODriver.ResetVideoMode ( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags ) : Boolean;
var iSDLFlags : DWord;
    iDM       : PSDL_DisplayMode;

// Resize/Toggle
var iFScreen : Boolean;
    iClosest : SDL_DisplayMode;
    iCurrent : SDL_DisplayID;
begin
  iCurrent := 0;
//  if FWindow <> nil then
//    iCurrent := SDL_GetWindowDisplay(FWindow);
  if iCurrent = 0 then
    iCurrent := SDL_GetPrimaryDisplay();
  if iCurrent = 0 then
    raise EIOException.Create('No primary display: ' + SDL_GetError());

  if ( aWidth * aHeight = 0 ) then
  begin
    iDM := SDL_GetCurrentDisplayMode( iCurrent );
    if iDM = nil then
      raise EIOException.Create(' SDL_GetCurrentDisplayMode returned 0 : '+SDL_GetError());
    aWidth  := iDM^.w;
    aHeight := iDM^.h;
  end;


  if FWindow <> nil then
  begin
    iFScreen  := ( SDLIO_FullScreen in aFlags );
    if ( FFScreen = iFScreen )
      and ( FSizeX = aWidth )
      and ( FSizeY = aHeight )
          then Exit( True );
    if iFScreen then
    begin
      if not SDL_GetClosestFullscreenDisplayMode( iCurrent, aWidth, aHeight, 60.0, True, @iClosest) then
      begin
        Log('Failed to find fullscreen mode %dx%d/%dbit', [aWidth, aHeight, aBPP]);
        Exit(False);
      end;

      // If we were fullscreen already, drop out first to avoid mode churn
      if FFScreen then SDL_SetWindowFullscreen(FWindow, False);

      if not SDL_SetWindowFullscreenMode( FWindow, @iClosest ) then
        Exit(False);
      if not SDL_SetWindowFullscreen( FWindow, True ) then
        Exit(False);
    end
    else
    begin
      if FFScreen then SDL_SetWindowFullscreen( FWindow, False );
      SDL_SetWindowSize( FWindow, aWidth, aHeight );
    end;
  end;

  FFScreen  := SDLIO_FullScreen in aFlags;
  FOpenGL   := SDLIO_OpenGL in aFlags;
  FSizeX    := aWidth;
  FSizeY    := aHeight;
  FBPP      := aBPP;
  FFlags    := aFlags;

  if FWindow <> nil then
  begin
    SDL_SetWindowPosition( FWindow, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED );
    Exit( True );
  end;

  iSDLFlags := SDL_WINDOW_HIGH_PIXEL_DENSITY;
  if FOpenGL  then iSDLFlags := iSDLFlags or SDL_WINDOW_OPENGL;
  if FFScreen then iSDLFlags := iSDLFlags or SDL_WINDOW_FULLSCREEN;

  if SDLIO_Resizable in aFlags then iSDLFlags := iSDLFlags or SDL_WINDOW_RESIZABLE;

  Log('Checking mode %dx%d/%dbit...', [aWidth,aHeight,aBPP]);

  if FOpenGL then
  begin

    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

    SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3 );
    SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 3 );
    // TODO: Change to CORE profile
    //       This at least needs to remove GL_QUADS from all rendering
    SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE );
  end;

  FWindow := SDL_CreateWindow( 'Valkyrie SDL Application', aWidth, aHeight, iSDLFlags or SDL_WINDOW_HIDDEN );
  if FWindow = nil then Exit( False );
  SDL_SetWindowPosition( FWindow, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED );
  SDL_ShowWindow( FWindow );

  if FOpenGL then
    if not SetupOpenGL then
       Exit( False );

  Exit( True );
end;

function TSDLIODriver.SetupOpenGL : Boolean;
begin
  FGLContext := SDL_GL_CreateContext( FWindow );
  if FGLContext = nil then raise EIOException.Create('OpenGL context could not be created! SDL_Error: ' + SDL_GetError() );
  SDL_GL_SetSwapInterval( 1 );

  LoadGL3;

  glClearColor( 0.0, 0.0, 0.0, 0.0 );
  glClearDepth( 1.0 );
  glHint( GL_LINE_SMOOTH_HINT,            GL_NICEST );
  glHint( GL_POLYGON_SMOOTH_HINT,         GL_NICEST );
  glEnable( GL_CULL_FACE );
  glEnable( GL_DEPTH_TEST );
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glDepthFunc( GL_LEQUAL );
  glCullFace( GL_BACK );
  glFrontFace( GL_CCW );
  glClearColor( 0, 0, 0, 0 );
  glViewport( 0, 0, FSizeX, FSizeY );
  Exit( True );
end;

procedure TSDLIODriver.Sleep ( Milliseconds : DWord ) ;
begin
  SDL_Delay( Milliseconds );
  SDL_PumpEvents;
end;

function TSDLIODriver.PollEvent ( out aEvent : TIOEvent ) : Boolean;
var event : SDL_Event;
begin
  Result := SDL_PollEvent( @event );
  if Result then
    aEvent := SDLEventToIOEvent( @event );
end;

function TSDLIODriver.PeekEvent ( out aEvent : TIOEvent ) : Boolean;
var event : SDL_Event;
begin
  SDL_PumpEvents();
  Result := (SDL_PeepEvents( @event, 1, SDL_PEEKEVENT, SDL_EVENT_FIRST, SDL_EVENT_LAST ) > 0 );
  if Result then
    aEvent := SDLEventToIOEvent( @event );
end;

function TSDLIODriver.EventPending : Boolean;
var event : SDL_Event;
begin
  SDL_PumpEvents();
  Result := (SDL_PeepEvents( @event, 1, SDL_PEEKEVENT, SDL_EVENT_FIRST, SDL_EVENT_LAST  ) > 0 );
end;

procedure TSDLIODriver.SetEventMask ( aMask : TIOEventType ) ;
begin

end;

function TSDLIODriver.GetMs : DWord;
begin
  Exit( SDL_GetTicks() );
end;

procedure TSDLIODriver.PreUpdate;
begin
  if FOpenGL then
  begin
    glClearColor(0.0,0.0,0.0,1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
end;

procedure TSDLIODriver.PostUpdate;
begin
  if FOpenGL then
     SDL_GL_SwapWindow( FWindow )
  else
    Assert( False, 'Non-OpenGL IODriver not implemented!' );
//    SDL_Flip( SDL_GetVideoSurface() );
end;

destructor TSDLIODriver.Destroy;
begin
  if FOpenGL then SDL_GL_DestroyContext( FGLContext );
  SDL_DestroyWindow( FWindow );
  FreeAndNil( FDisplayModes );
  inherited Destroy;
end;

function TSDLIODriver.GetSizeX : DWord;
begin
  Result := FSizeX;
end;

function TSDLIODriver.GetSizeY : DWord;
begin
  Result := FSizeY;
end;

function TSDLIODriver.GetMousePos ( out aResult : TIOPoint ) : Boolean;
var x,y : Single;
begin
  //if SDL_GetAppState() and SDL_APPMOUSEFOCUS = 0 then Exit( False );
  x := 0; y := 0;
  SDL_GetMouseState(@x,@y);
  aResult := vutil.Point( Trunc(x), Trunc(y) );
  Exit( True );
end;

function TSDLIODriver.GetMouseButtonState ( out aResult : TIOMouseButtonSet
  ) : Boolean;
var x,y : Integer;
begin
//  if SDL_GetAppState() and SDL_APPMOUSEFOCUS = 0 then Exit( False );
  x := 0; y := 0;
  aResult := SDLMouseButtonSetToVMB( SDL_GetMouseState(@x,@y) );
  Exit( True );
end;

function TSDLIODriver.GetModKeyState : TIOModKeySet;
begin
  Exit( SDLModToModKeySet( SDL_GetModState() ) );
end;

procedure TSDLIODriver.SetTitle ( const aLongTitle : AnsiString;
  const aShortTitle : AnsiString ) ;
begin
  SDL_SetWindowTitle(FWindow, PChar(aLongTitle));
end;

procedure TSDLIODriver.ShowMouse ( aShow : Boolean ) ;
begin
  if aShow
    then SDL_ShowCursor
    else SDL_HideCursor;
end;

procedure TSDLIODriver.ScreenShot( const aFileName: AnsiString );
var iSx, iSy : Integer;
    iPitch   : Int64;
    iBuf     : PByte;
    iFlipBuf : PByte;
    iY       : Integer;
    iSrcRow  : PByte;
    iDstRow  : PByte;
    iSurface : PSDL_Surface;
    iRGB     : PSDL_Surface;
begin
  iSx := GetSizeX;
  iSy := GetSizeY;
  if (iSx <= 0) or (iSy <= 0) then Exit;

  iPitch := iSx * 4;

  GetMem(iBuf,     QWord(iPitch) * QWord(iSy));
  GetMem(iFlipBuf, QWord(iPitch) * QWord(iSy));
  try
    glPixelStorei( GL_PACK_ALIGNMENT, 1 );
    glReadPixels( 0, 0, iSx, iSy, GL_RGBA, GL_UNSIGNED_BYTE, iBuf );

    for iY := 0 to iSy - 1 do
    begin
      iSrcRow := iBuf     + (QWord(iY)           * QWord(iPitch));
      iDstRow := iFlipBuf + (QWord(iSy - 1 - iY) * QWord(iPitch));
      Move( iSrcRow^, iDstRow^, iPitch );
    end;

    iSurface := SDL_CreateSurfaceFrom(iSx, iSy, SDL_PIXELFORMAT_RGBA32, iFlipBuf, iPitch);
    if iSurface = nil then
      raise Exception.Create(SDL_GetError());
    try
      iRGB := SDL_ConvertSurface(iSurface, SDL_PIXELFORMAT_RGB24);
      if iRGB = nil then raise Exception.Create(SDL_GetError());
      try
        if not IMG_SavePNG( iRGB, PAnsiChar(aFileName) ) then
          raise Exception.Create(SDL_GetError());
      finally
        SDL_DestroySurface(iRGB);
      end;
    finally
      SDL_DestroySurface(iSurface);
    end;

  finally
    FreeMem(iFlipBuf);
    FreeMem(iBuf);
  end;
end;

procedure TSDLIODriver.ScanDisplayModes;
var i, iCount   : Integer;
    iMode       : PSDL_DisplayMode;
    iModes      : PPSDL_DisplayMode;
    iEntry      : TIODisplayMode;
    iIdn, iLast : Integer;
    iCD         : SDL_DisplayID;
begin
  if Assigned( FDisplayModes )
    then FDisplayModes.Clear
    else FDisplayModes := TIODisplayModeArray.Create;

  iCD := SDL_GetPrimaryDisplay();
  if iCD = 0 then
    raise EIOException.Create('No primary display: ' + SDL_GetError());

  iModes := SDL_GetFullscreenDisplayModes( iCD, @iCount );
  if iModes = nil then
    raise EIOException.Create('SDL_GetFullscreenDisplayModes failed: ' + SDL_GetError());

  iLast  := -1;
  i      := 0;
  while iModes[i] <> nil do
  begin
    iMode := iModes[i];
    iIdn  := Int64(iMode^.h) * 100000 + iMode^.w;

    if (iMode^.w >= 1280) and (iIdn <> iLast) then
    begin
      iLast := iIdn;
      iEntry.Name    := Format('%dx%d (%dr)', [iMode^.w, iMode^.h, Round(iMode^.refresh_rate)]);
      iEntry.Width   := iMode^.w;
      iEntry.Height  := iMode^.h;
      iEntry.Refresh := Round(iMode^.refresh_rate);
      iEntry.Index   := FDisplayModes.Size;
      FDisplayModes.Push(iEntry);
    end;
    Inc( i );
  end;

  SDL_free( iModes );
end;

function TSDLIODriver.SetDisplayMode( aIndex : Integer ) : Boolean;
begin
  if ( aIndex < 0 ) or ( aIndex >= FDisplayModes.Size ) then Exit( False );
  with FDisplayModes[ aIndex ] do
    Exit( ResetVideoMode( Width, Height, 32, FFlags ) );
end;

function TSDLIODriver.ScanGamepads( aAllowLoop : Boolean ) : Boolean;
var iIDs       : PSDL_JoystickID;
    iCount     : Integer;
    iIndex     : Integer;
    iID        : Uint32;
    iNewID     : Uint32;
    iGamepad   : PSDL_Gamepad;
begin
  Result    := False;
  iGamepad  := nil;
  iNewID    := 0;

  iIDs := SDL_GetGamepads(@iCount);
  if iIDs = nil then
  begin
    Log(LOGERROR, 'SDL_GetGamepads failed: %s', [SDL_GetError()]);
    Exit;
  end;

  for iIndex := 0 to iCount - 1 do
  begin
    iID := iIDs[iIndex];

    if iID <> FGamePadID then
    begin
      iGamepad := SDL_OpenGamepad(iID);
      if iGamepad <> nil then
      begin
        iNewID := iID;
        Break;
      end
      else
        Log(LOGERROR, 'Could not open gamepad %u: %s', [iID, SDL_GetError()]);
    end
    else
    begin
      // Keep current one
      iNewID   := FGamePadID;
      iGamepad := FGamePadHandle;
      Break;
    end;
  end;

  SDL_free(iIDs);

  if iNewID <> FGamePadID then
  begin
    if FGamePadHandle <> nil then
      SDL_CloseGamepad(FGamePadHandle);

    FGamePadID     := iNewID;
    FGamePadHandle := iGamepad;
    FGamePadRumble := False;
    if iGamepad <> nil then
    begin
      FGamePadRumble := SDL_GetBooleanProperty(SDL_GetGamepadProperties(iGamepad),
                               SDL_PROP_GAMEPAD_CAP_RUMBLE_BOOLEAN,
                               False);
    end;

    if iGamepad <> nil then
    begin
      Log('Controller connected.');
      Log('ID: %u  Name: %s', [FGamePadID, SDL_GetGamepadName(iGamepad)]);
      Log('Rumble: %s', [Iif(FGamePadRumble, 'supported', 'unsupported')]);
    end
    else
      Log('Controller has been removed, no fallback present.');

    Result := True;
  end;

  // Check if still attached
  if (FGamePadHandle <> nil) and (not SDL_GamepadConnected(FGamePadHandle)) then
  begin
    Log(LOGERROR, 'Controller %u is not attached anymore, disabling!', [FGamePadID]);
    SDL_CloseGamepad(FGamePadHandle);
    FGamePadID     := 0;        // 0 = "none"
    FGamePadHandle := nil;
    FGamePadRumble := False;

    Log(LOGINFO, 'Attempting to reacquire another controller...');
    if aAllowLoop then
      Exit(ScanGamepads(False));
  end;
end;

procedure TSDLIODriver.SetGamePadSupport( aValue : Boolean );
begin
  if aValue <> FGamePadSupport then
  begin
    if aValue then ScanGamepads;
    FGamePadSupport := aValue;
  end;
end;

procedure TSDLIODriver.StartTextInput;
begin
  SDL_StartTextInput( FWindow );
end;

procedure TSDLIODriver.StopTextInput;
begin
  SDL_StopTextInput( FWindow );
end;

function TSDLIODriver.Rumble( aLow, aHigh : Word; aDuration : DWord ) : Boolean;
begin
  if (FGamePadHandle = nil) or (not FGamePadRumble) then Exit( False );
  Exit( SDL_RumbleGamepad( FGamePadHandle, aLow, aHigh, aDuration ) );
end;

end.

