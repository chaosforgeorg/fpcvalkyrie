{$INCLUDE valkyrie.inc}
unit vsdlio;
interface
uses Classes, SysUtils, vutil, viotypes, vsdl3library, vioevent, viopadstate;

// Architectural boundary: owns SDL window, event, and capture mechanics only.
// Renderer, path, and game policy belong to callers.
type TSDLIODriver = class( TIOWindowDriver )
  // An OpenGL window placed from aSetup on the primary display, then shown
  // directly in its mode.
  constructor CreateWindow( const aSetup : TIOWindowSetup; aGamepad : Boolean );
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
  function CaptureScreen( const aFileName : AnsiString ) : Boolean; override;
  procedure StartTextInput; override;
  procedure StopTextInput; override;
  procedure SetClipboard( const aValue : Ansistring ); override;
  function GetClipboard : Ansistring; override;
  function Rumble( aLow, aHigh : Word; aDuration : DWord ) : Boolean; override;
  procedure SetFullscreen( aValue : Boolean ); override;
  procedure SetMinimumPixels( const aPixels : TIOPoint ); override;
  // Converted like the minimum. Kept while fullscreen and applied, centered,
  // when the window is next windowed.
  procedure SetWindowPixels( const aPixels : TIOPoint );
  function ReadMetrics( out aMetrics : TIODisplayMetrics ) : Boolean; override;
protected
  function GetDisplaySize : TIOPoint; override;
private
  FSizeX      : DWord;
  FSizeY      : DWord;
  FWindow     : PSDL_Window;
  FGLContext  : SDL_GLContext;
  FLastMouseX : Integer;
  FLastMouseY : Integer;

  FGamePadID      : DWord;
  FGamePadRumble  : Boolean;
  FGamePadSupport : Boolean;
  FMouseSupport   : Boolean;
  FGamePadIndex   : Integer;
  FGamePadHandle  : Pointer;
  FPadTriggerState    : TIOPadTriggerState;
  FPendingPadEvent    : TIOEvent;
  FHasPendingPadEvent : Boolean;

  FPixelDensity   : Single;
  FPendingSize    : TIOPoint; // SetWindowPixels while fullscreen; zero = none
private
  procedure OpenWindow( aResizable : Boolean );
  procedure SetupOpenGL;
  procedure PlaceWindow( aSize : TIOPoint );
  function RefreshWindowSize : Boolean;
  function EventToIOEvent( event : PSDL_Event ) : TIOEvent;
  function MouseEventToIOEvent( event : PSDL_Event ) : TIOEvent;
  function MouseMoveEventToIOEvent( event : PSDL_Event ) : TIOEvent;
  procedure ClearPadEventState;
  procedure ResetPadEventState( aWhich : Int32 );
  function ScanGamepads( aAllowLoop : Boolean = True ) : Boolean;
  procedure SetGamePadSupport( aValue : Boolean  );
  procedure SetMouseSupport( aValue : Boolean );
  function ConsumeWindowKey( aEvent : PSDL_Event ) : Boolean;
  function PeekQueuedEvent( out aEvent : SDL_Event ) : Boolean;
  function IsWindowFullscreen : Boolean;
public
  property Width : DWord            read FSizeX;
  property Height : DWord           read FSizeY;
  property GamePadSupport : Boolean read FGamePadSupport write SetGamePadSupport;
  // When False, mouse events are not delivered; polling the position still works.
  property MouseSupport : Boolean read FMouseSupport write SetMouseSupport;
end;

implementation

uses Math, vdebug, vgl3library;

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
    if (iMod and SDL_KMOD_CTRL)  <> 0 then Include( Result.Key.ModState, VKMOD_CTRL );
    if (iMod and SDL_KMOD_ALT)   <> 0 then Include( Result.Key.ModState, VKMOD_ALT );
  end
  else
  begin
    Result.Key.ASCII    := #0;
    Result.Key.ModState := SDLModToModKeySet( iMod );
    Result.Key.Code     := SDLSymToCode( event^.key.key );
  end;
  // PrintableToIOEvent clears the record, so apply SDL state after conversion.
  Result.Key.Pressed  := event^._type = SDL_EVENT_KEY_DOWN;
  Result.Key.Repeated := Result.Key.Pressed and event^.key.repeat_;
  if event^._type = SDL_EVENT_KEY_DOWN
    then Result.EType := VEVENT_KEYDOWN
    else Result.EType := VEVENT_KEYUP;
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

function TSDLIODriver.MouseEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  if event^._type = SDL_EVENT_MOUSE_WHEEL then
  begin
      Result.EType         := VEVENT_MOUSEDOWN;
      if event^.wheel.y > 0 then
            Result.Mouse.Button  := VMB_WHEEL_UP
      else
            Result.Mouse.Button  := VMB_WHEEL_DOWN;
      Result.Mouse.Pos.X   := FLastMouseX;
      Result.Mouse.Pos.Y   := FLastMouseY;
      Result.Mouse.Pressed := True;
      Exit;
  end;
  case event^._type of
    SDL_EVENT_MOUSE_BUTTON_DOWN : Result.EType := VEVENT_MOUSEDOWN;
    SDL_EVENT_MOUSE_BUTTON_UP   : Result.EType := VEVENT_MOUSEUP;
  end;
  Result.Mouse.Button  := SDLMouseButtonToVMB( event^.button.button );
  Result.Mouse.Pos.X   := Trunc( event^.button.x * FPixelDensity );
  Result.Mouse.Pos.Y   := Trunc( event^.button.y * FPixelDensity );
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

function TSDLIODriver.MouseMoveEventToIOEvent( event : PSDL_Event ) : TIOEvent;
var iScale : Single;
begin
  iScale := FPixelDensity;
  Result.EType := VEVENT_MOUSEMOVE;
  Result.MouseMove.ButtonState := SDLMouseButtonSetToVMB( event^.motion.state );
  Result.MouseMove.Pos.X       := Trunc( event^.motion.x * iScale );
  Result.MouseMove.Pos.Y       := Trunc( event^.motion.y * iScale );
  Result.MouseMove.RelPos.X    := Trunc( event^.motion.xrel * iScale );
  Result.MouseMove.RelPos.Y    := Trunc( event^.motion.yrel * iScale );
  FLastMouseX := Result.MouseMove.Pos.X;
  FLastMouseY := Result.MouseMove.Pos.Y;
end;

function TSDLIODriver.EventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  case event^._type of
    SDL_EVENT_KEY_DOWN : Exit( SDLKeyEventToIOEvent( event ) );
    SDL_EVENT_KEY_UP   : Exit( SDLKeyEventToIOEvent( event ) );
    SDL_EVENT_TEXT_INPUT : Exit( SDLTextEventToIOEvent( event ) );

    SDL_EVENT_MOUSE_MOTION     : Exit( MouseMoveEventToIOEvent( event ) );
    SDL_EVENT_MOUSE_BUTTON_DOWN : Exit( MouseEventToIOEvent( event ) );
    SDL_EVENT_MOUSE_BUTTON_UP   : Exit( MouseEventToIOEvent( event ) );
    SDL_EVENT_MOUSE_WHEEL      : Exit( MouseEventToIOEvent( event ) );

    SDL_EVENT_GAMEPAD_AXIS_MOTION : Exit( SDLPadAxisEventToIOEvent( event ) );
    SDL_EVENT_GAMEPAD_BUTTON_DOWN,
    SDL_EVENT_GAMEPAD_BUTTON_UP : Exit( SDLPadEventToIOEvent( event ) );

    SDL_EVENT_GAMEPAD_ADDED,
    SDL_EVENT_GAMEPAD_REMOVED,
    SDL_EVENT_GAMEPAD_REMAPPED :
    begin
      ScanGamepads;
      Exit( SDLPadDeviceEventToIOEvent( event ) );
    end;
  end;
  Result.EType := VEVENT_SYSTEM;
  Result.System.Code := VIO_SYSEVENT_NONE;
end;

function SDLIOEventFilter( userdata : Pointer; event : PSDL_Event ) : Boolean; cdecl;
var iCode : TIOKeyCode;
    iDriver : TSDLIODriver;
begin
  iDriver := TSDLIODriver( userdata );
  // State over events: mark display state for a fresh read whatever happens to
  // the queued event later (ClearEventBuffer, layers, game loops).
  case event^._type of
    SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED,
    SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED :
      begin
        iDriver.FPixelDensity := SDL_GetWindowPixelDensity( iDriver.FWindow );
        iDriver.FDisplayDirty := True;
      end;
    // Only a plain windowed window owns the remembered size.
    SDL_EVENT_WINDOW_RESIZED :
      begin
        iDriver.FDisplayDirty := True;
        if SDL_GetWindowFlags( iDriver.FWindow ) and ( SDL_WINDOW_FULLSCREEN or SDL_WINDOW_MAXIMIZED ) = 0 then
          iDriver.FWindowSize := vutil.Point( event^.window.data1, event^.window.data2 );
      end;
    // The platform may change the mode itself (macOS, window managers).
    SDL_EVENT_WINDOW_ENTER_FULLSCREEN,
    SDL_EVENT_WINDOW_LEAVE_FULLSCREEN :
      begin
        iDriver.FDisplayDirty := True;
        iDriver.FFullscreen   := event^._type = SDL_EVENT_WINDOW_ENTER_FULLSCREEN;
      end;
    SDL_EVENT_WINDOW_MOVED,
    SDL_EVENT_WINDOW_MINIMIZED,
    SDL_EVENT_WINDOW_MAXIMIZED,
    SDL_EVENT_WINDOW_RESTORED,
    SDL_EVENT_WINDOW_DISPLAY_CHANGED,
    SDL_EVENT_DISPLAY_ORIENTATION,
    SDL_EVENT_DISPLAY_ADDED,
    SDL_EVENT_DISPLAY_REMOVED,
    SDL_EVENT_DISPLAY_MOVED,
    SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED,
    SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED,
    SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED :
      iDriver.FDisplayDirty := True;
  end;
  if event^._type = SDL_EVENT_QUIT then
    if Assigned( iDriver.FOnQuit ) then
      if iDriver.FOnQuit( iDriver.EventToIOEvent( event ) ) then
        Exit(False);
  if event^._type = SDL_EVENT_KEY_DOWN then
  begin
    iCode := SDLKeyEventToKeyCode( event );
    if iDriver.FInterrupts[iCode] <> nil then
      if iDriver.FInterrupts[iCode]( SDLKeyEventToIOEvent( event ) ) then
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
  SDL_EVENT_TEXT_INPUT : Exit(True);
  SDL_EVENT_MOUSE_MOTION,
  SDL_EVENT_MOUSE_BUTTON_DOWN,
  SDL_EVENT_MOUSE_BUTTON_UP,
  SDL_EVENT_MOUSE_WHEEL : Exit( iDriver.MouseSupport );
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
  SDL_EVENT_GAMEPAD_UPDATE_COMPLETE,
  SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED : Exit( iDriver.GamePadSupport );
  SDL_EVENT_GAMEPAD_SENSOR_UPDATE : Exit( False );
  end;
  Exit(True);
end;

{ TSDLIODriver }

// Default windowed size, as a share of the usable display area.
const DefaultWindowShare = 0.75;

constructor TSDLIODriver.CreateWindow( const aSetup : TIOWindowSetup; aGamepad : Boolean );
var iVersion : Sint32;
begin
  inherited Create;
  ClearPadEventState;
  FWindow    := nil;
  FGLContext := nil;
  FGamePadSupport := aGamepad;
  FMouseSupport   := True;
  FGamePadID      := 0;
  FGamePadIndex   := -1;
  FGamePadHandle  := nil;
  FGamePadRumble  := False;
  FPixelDensity   := 1.0;
  {$IF DEFINED(CPUI386) OR DEFINED(CPUX86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$ENDIF}
  if not LoadSDL3 then
    raise EIOException.Create( 'Could not load SDL3.' );

  Log('Initializing SDL...');
  iVersion := SDL_GetVersion();
  Log( LOGINFO, 'SDL Version         : %d.%d.%d (compiled for %d.%d.%d)',
    [ iVersion div 1000000, ( iVersion div 1000 ) mod 1000, iVersion mod 1000,
      SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL ] );
  if iVersion < SDL_VERSION then
    Log( LOGWARN, 'SDL library is older than the version Valkyrie was compiled for.' );

  if not SDL_Init( SDL_INIT_VIDEO or SDL_INIT_GAMEPAD ) then
  begin
    SDL_Quit();
    raise EIOException.Create('Couldn''t initialize SDL : '+SDL_GetError());
  end;

  // Failed construction destroys acquired resources while SDL is still valid.
  OpenWindow( aSetup.Resizable );

  // One SDL driver owns the process event filter. Registration may call it
  // synchronously for queued events, so all receiver state is ready here.
  SDL_SetEventFilter( @SDLIOEventFilter, Self );
  SDL_SetEventEnabled( SDL_EVENT_JOYSTICK_UPDATE_COMPLETE, False );
  SDL_SetEventEnabled( SDL_EVENT_GAMEPAD_UPDATE_COMPLETE, False );

  if FGamePadSupport then ScanGamepads;

  FAllowWindowed := aSetup.AllowWindowed;
  SetMinimumPixels( aSetup.MinimumPixels );
  PlaceWindow( aSetup.WindowSize );
  // SDL applies a fullscreen request on a hidden window when it is shown.
  if aSetup.Fullscreen or not aSetup.AllowWindowed then
    SetFullscreen( True );
  SDL_ShowWindow( FWindow );
  SDL_SyncWindow( FWindow );
  RefreshWindowSize;
  FDisplayDirty := True;
  Log('SDL IO system ready.');
end;

// A hidden OpenGL 3.3 core window and its context. CreateWindow sizes and
// places it before showing it.
procedure TSDLIODriver.OpenWindow( aResizable : Boolean );
var iFlags : DWord;
begin
  iFlags := SDL_WINDOW_HIGH_PIXEL_DENSITY or SDL_WINDOW_OPENGL or SDL_WINDOW_HIDDEN;
  if aResizable then iFlags := iFlags or SDL_WINDOW_RESIZABLE;

  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3 );
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 3 );
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE );

  FWindow := SDL_CreateWindow( 'Valkyrie SDL Application', 1280, 720, iFlags );
  if FWindow = nil then
    raise EIOException.Create( 'Could not create the window: ' + SDL_GetError() );
  SetupOpenGL;
  FPixelDensity := SDL_GetWindowPixelDensity( FWindow );
  if FPixelDensity <= 0 then FPixelDensity := 1.0;

  Log( LOGINFO, 'OpenGL Vendor       : %s', [ glGetString(GL_VENDOR) ] );
  Log( LOGINFO, 'OpenGL Renderer     : %s', [ glGetString(GL_RENDERER) ] );
  Log( LOGINFO, 'OpenGL Version      : %s', [ glGetString(GL_VERSION) ] );
  Log( LOGINFO, 'OpenGL GLSL Version : %s', [ glGetString(35724) ] );
end;

procedure TSDLIODriver.SetupOpenGL;
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
end;

procedure TSDLIODriver.Sleep ( Milliseconds : DWord ) ;
begin
  SDL_Delay( Milliseconds );
  SDL_PumpEvents;
end;

function TSDLIODriver.PollEvent ( out aEvent : TIOEvent ) : Boolean;
var event : SDL_Event;
begin
  if FHasPendingPadEvent then
  begin
    aEvent := FPendingPadEvent;
    FHasPendingPadEvent := False;
    Exit( True );
  end;

  repeat
    Result := SDL_PollEvent( @event );
  until not ( Result and ConsumeWindowKey( @event ) );
  if Result then
  begin
    aEvent := EventToIOEvent( @event );
    if aEvent.EType = VEVENT_PADAXIS then
      FHasPendingPadEvent := FPadTriggerState.Update( aEvent.PadAxis, FPendingPadEvent )
    else
      if aEvent.EType = VEVENT_PADDEVICE then
        ClearPadEventState;
  end;
end;

function TSDLIODriver.PeekEvent ( out aEvent : TIOEvent ) : Boolean;
var event : SDL_Event;
begin
  if FHasPendingPadEvent then
  begin
    aEvent := FPendingPadEvent;
    Exit( True );
  end;

  Result := PeekQueuedEvent( event );
  if Result then
    aEvent := EventToIOEvent( @event );
end;

function TSDLIODriver.EventPending : Boolean;
var iEvent : SDL_Event;
begin
  Result := FHasPendingPadEvent or PeekQueuedEvent( iEvent );
end;

// Pumps, then peeks the head of the queue, removing window-control keys so
// no caller sees an event that PollEvent would not deliver.
function TSDLIODriver.PeekQueuedEvent( out aEvent : SDL_Event ) : Boolean;
begin
  repeat
    if not SDL_PollEvent( nil ) then Exit( False );
    if SDL_PeepEvents( @aEvent, 1, SDL_PEEKEVENT, SDL_EVENT_FIRST, SDL_EVENT_LAST ) <= 0 then Exit( False );
    if not ConsumeWindowKey( @aEvent ) then Exit( True );
    SDL_PeepEvents( @aEvent, 1, SDL_GETEVENT, SDL_EVENT_FIRST, SDL_EVENT_LAST );
  until False;
end;

// Alt+Enter is recognized here, on the main thread and outside the event
// filter, so loops that poll or discard events cannot lose the toggle.
function TSDLIODriver.ConsumeWindowKey( aEvent : PSDL_Event ) : Boolean;
begin
  if aEvent^._type <> SDL_EVENT_KEY_DOWN then Exit( False );
  if aEvent^.key.key <> SDLK_RETURN then Exit( False );
  if ( aEvent^.key.mod_ and SDL_KMOD_ALT ) = 0 then Exit( False );
  if ( aEvent^.key.mod_ and ( SDL_KMOD_CTRL or SDL_KMOD_SHIFT or SDL_KMOD_GUI ) ) <> 0 then Exit( False );
  if FAllowWindowed and not aEvent^.key.repeat_ then
    FToggleRequest := True;
  Exit( True );
end;

procedure TSDLIODriver.ClearPadEventState;
begin
  FPadTriggerState.Clear;
  FHasPendingPadEvent := False;
end;

procedure TSDLIODriver.ResetPadEventState( aWhich : Int32 );
begin
  ClearPadEventState;
  FPendingPadEvent.EType           := VEVENT_PADDEVICE;
  FPendingPadEvent.PadDevice.Event := VPAD_REMAPPED;
  FPendingPadEvent.PadDevice.Which := aWhich;
  FHasPendingPadEvent := True;
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
  // The full drawable each frame; games set no viewport for resolution.
  glViewport( 0, 0, FSizeX, FSizeY );
  glClearColor(0.0,0.0,0.0,1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TSDLIODriver.PostUpdate;
begin
  SDL_GL_SwapWindow( FWindow );
end;

destructor TSDLIODriver.Destroy;
begin
  if Assigned( SDL_SetEventFilter ) then
    SDL_SetEventFilter( nil, nil );
  ClearPadEventState;
  if FGamePadHandle <> nil then SDL_CloseGamepad( FGamePadHandle );
  if FGLContext <> nil then SDL_GL_DestroyContext( FGLContext );
  if FWindow <> nil then SDL_DestroyWindow( FWindow );
  inherited Destroy;
end;

function TSDLIODriver.RefreshWindowSize : Boolean;
var iWidth, iHeight : LongInt;
begin
  // The drawable in pixels; without one (minimized) the last size stays.
  iWidth  := 0;
  iHeight := 0;
  Result := SDL_GetWindowSizeInPixels( FWindow, @iWidth, @iHeight ) and
    ( iWidth > 0 ) and ( iHeight > 0 );
  if not Result then Exit;
  FSizeX := iWidth;
  FSizeY := iHeight;
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
  aResult := vutil.Point( Trunc( x * FPixelDensity ), Trunc( y * FPixelDensity ) );
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

function TSDLIODriver.CaptureScreen( const aFileName : AnsiString ) : Boolean;
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
  Result := False;
  if not SDL_GetWindowSizeInPixels( FWindow, @iSx, @iSy ) then Exit;
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
        if not SDL_SavePNG( iRGB, PAnsiChar(aFileName) ) then
          raise Exception.Create(SDL_GetError());
        Result := True;
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
    ClearPadEventState;
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
    ClearPadEventState;
    FGamePadSupport := aValue;
    if aValue then
      ScanGamepads
    else
    begin
      SDL_PumpEvents;
      SDL_FlushEvents(
        SDL_EVENT_GAMEPAD_AXIS_MOTION,
        SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED
      );
    end;
    ResetPadEventState( Int32( FGamePadID ) );
  end;
end;

procedure TSDLIODriver.SetMouseSupport( aValue : Boolean );
begin
  if aValue = FMouseSupport then Exit;
  FMouseSupport := aValue;
  if not aValue then
  begin
    SDL_PumpEvents;
    SDL_FlushEvents( SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_WHEEL );
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

procedure TSDLIODriver.SetClipboard( const aValue : Ansistring );
begin
  SDL_SetClipboardText( PAnsiChar( aValue ) );
end;

function TSDLIODriver.GetClipboard : Ansistring;
var iResult : PAnsiChar;
begin
  iResult := SDL_GetClipboardText();
  GetClipboard := Ansistring( iResult );
  SDL_free( iResult );
end;

function TSDLIODriver.Rumble( aLow, aHigh : Word; aDuration : DWord ) : Boolean;
begin
  if (FGamePadHandle = nil) or (not FGamePadRumble) then Exit( False );
  Exit( SDL_RumbleGamepad( FGamePadHandle, aLow, aHigh, aDuration ) );
end;

// The size (zero = default) within the usable area and the minimum, centered.
procedure TSDLIODriver.PlaceWindow( aSize : TIOPoint );
var iDisplay : SDL_DisplayID;
    iUsable  : SDL_Rect;
    iMinimum : TIOPoint;
begin
  iDisplay := SDL_GetDisplayForWindow( FWindow );
  iUsable  := Default( SDL_Rect );
  SDL_GetDisplayUsableBounds( iDisplay, @iUsable );
  iMinimum := vutil.Point( 0, 0 );
  SDL_GetWindowMinimumSize( FWindow, @iMinimum.X, @iMinimum.Y );
  if ( aSize.X <= 0 ) or ( aSize.Y <= 0 ) then
    aSize := vutil.Point( Round( iUsable.w * DefaultWindowShare ), Round( iUsable.h * DefaultWindowShare ) );
  aSize.X := Max( Min( aSize.X, iUsable.w ), Min( iMinimum.X, iUsable.w ) );
  aSize.Y := Max( Min( aSize.Y, iUsable.h ), Min( iMinimum.Y, iUsable.h ) );
  if not SDL_SetWindowSize( FWindow, aSize.X, aSize.Y ) then
    Log( LOGERROR, 'SDL_SetWindowSize %dx%d failed: %s', [ aSize.X, aSize.Y, SDL_GetError() ] );
  SDL_SetWindowPosition( FWindow,
    SDL_WINDOWPOS_CENTERED_DISPLAY( LongInt( iDisplay ) ),
    SDL_WINDOWPOS_CENTERED_DISPLAY( LongInt( iDisplay ) ) );
  FWindowSize := aSize;
end;

procedure TSDLIODriver.SetWindowPixels( const aPixels : TIOPoint );
var iSize : TIOPoint;
begin
  iSize := vutil.Point( Ceil( aPixels.X / FPixelDensity ), Ceil( aPixels.Y / FPixelDensity ) );
  FPendingSize := vutil.Point( 0, 0 );
  if iSize = FWindowSize then Exit;
  // SDL discards a size change on a fullscreen window.
  if FFullscreen
    then FPendingSize := iSize
    else PlaceWindow( iSize );
end;

function TSDLIODriver.IsWindowFullscreen : Boolean;
begin
  Exit( SDL_GetWindowFlags( FWindow ) and SDL_WINDOW_FULLSCREEN <> 0 );
end;

procedure TSDLIODriver.SetFullscreen( aValue : Boolean );
begin
  if ( aValue = FFullscreen ) or not ( aValue or FAllowWindowed ) then Exit;
  ResetPadEventState( Int32( FGamePadID ) );
  if aValue and not SDL_SetWindowFullscreenMode( FWindow, nil ) then
  begin
    Log( LOGERROR, 'SDL_SetWindowFullscreenMode failed: %s', [ SDL_GetError() ] );
    Exit;
  end;
  if not SDL_SetWindowFullscreen( FWindow, aValue ) then
  begin
    Log( LOGERROR, 'SDL_SetWindowFullscreen failed: %s', [ SDL_GetError() ] );
    Exit;
  end;
  FFullscreen := aValue;
  if aValue or ( FPendingSize.X <= 0 ) then Exit;
  // The restored floating geometry must be in place before it is replaced.
  SDL_SyncWindow( FWindow );
  PlaceWindow( FPendingSize );
  FPendingSize := vutil.Point( 0, 0 );
end;

// Converted with the current density; not reapplied when it changes.
procedure TSDLIODriver.SetMinimumPixels( const aPixels : TIOPoint );
begin
  if not SDL_SetWindowMinimumSize( FWindow,
    Ceil( aPixels.X / FPixelDensity ), Ceil( aPixels.Y / FPixelDensity ) ) then
    Log( LOGERROR, 'SDL_SetWindowMinimumSize failed: %s', [ SDL_GetError() ] );
end;

function TSDLIODriver.GetDisplaySize : TIOPoint;
var iMode : PSDL_DisplayMode;
begin
  iMode := SDL_GetDesktopDisplayMode( SDL_GetDisplayForWindow( FWindow ) );
  if iMode = nil then Exit( vutil.Point( FSizeX, FSizeY ) );
  Exit( vutil.Point( Round( iMode^.w * iMode^.pixel_density ), Round( iMode^.h * iMode^.pixel_density ) ) );
end;

function TSDLIODriver.ReadMetrics( out aMetrics : TIODisplayMetrics ) : Boolean;
begin
  // Cleared first, so events arriving during the read mark it again.
  FDisplayDirty := False;
  FPixelDensity := SDL_GetWindowPixelDensity( FWindow );
  if FPixelDensity <= 0 then FPixelDensity := 1.0;
  aMetrics.Fullscreen   := IsWindowFullscreen;
  aMetrics.PixelDensity := FPixelDensity;
  aMetrics.DisplaySize  := GetDisplaySize;
  Result := RefreshWindowSize;
  aMetrics.PixelSize := vutil.Point( FSizeX, FSizeY );
end;

end.
