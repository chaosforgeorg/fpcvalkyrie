{$INCLUDE valkyrie.inc}
unit vsdlio;
interface
uses Classes, SysUtils, vutil, viotypes, vsdl2library, vioevent;

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

function SDLIOEventFilter( userdata : Pointer; event: PSDL_Event) : Integer; cdecl;

implementation

uses vgl3library,
     {Screenshot support}
     vdebug,
     FPImage, FPCanvas,
     FPWritePNG{$IFDEF WINDOWS}, Windows{$ENDIF};

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
  Result := SDLSymToCode( event^.key.keysym.sym );
  smod := event^.key.keysym.mod_;
  if (smod and KMOD_CTRL)  <> 0 then Result += IOKeyCodeCtrlMask;
  if (smod and KMOD_SHIFT) <> 0 then Result += IOKeyCodeShiftMask;
  if (smod and KMOD_ALT)   <> 0 then Result += IOKeyCodeAltMask;
end;

function SDLModToModKeySet( smod : SDL_Keymod ) : TIOModKeySet;
begin
  Result := [];
  if smod = KMOD_NONE then Exit;
  if (smod and KMOD_CTRL)  <> 0 then Include( Result, VKMOD_CTRL );
  if (smod and KMOD_SHIFT) <> 0 then Include( Result, VKMOD_SHIFT );
  if (smod and KMOD_ALT)   <> 0 then Include( Result, VKMOD_ALT );
end;

function SDLKeyEventToIOEvent( event : PSDL_Event ) : TIOEvent;
const KNumerics : array[0..9] of Char = (')','!','@','#','$','%','^','&','*','(');
var iCode    : Integer;
    iShift   : Boolean;
begin
  iCode  := event^.key.keysym.sym;
  iShift := ( (event^.key.keysym.mod_ and KMOD_SHIFT) <> 0 ) or
            ( (event^.key.keysym.mod_ and KMOD_CAPS) <> 0 );
  Result.Key.Repeated := ( event^.type_ = SDL_KEYDOWN ) and (event^.key.repeat_ > 0);

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
          Ord('a')..Ord('z') : iCode := SDL_toupper( iCode );
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
    if event^.type_ = SDL_KEYUP then Result.EType := VEVENT_KEYUP;
    Exit;
  end;
  if event^.type_ = SDL_KEYDOWN
    then Result.EType := VEVENT_KEYDOWN
    else Result.EType := VEVENT_KEYUP;
  Result.Key.ASCII    := #0;
  Result.Key.ModState := SDLModToModKeySet( event^.key.keysym.mod_ );
  Result.Key.Code     := SDLSymToCode( event^.key.keysym.sym );
end;

function SDLSystemEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_SYSTEM;
  Result.System.Param1 := 0;
  Result.System.Param2 := 0;

  case event^.type_ of
    SDL_QUIT_       : Result.System.Code := VIO_SYSEVENT_QUIT;
    SDL_WINDOWEVENT_:
    case event^.window.event of
      SDL_WINDOWEVENT_EXPOSED : Result.System.Code := VIO_SYSEVENT_EXPOSE;
      SDL_WINDOWEVENT_RESIZED :
      begin
        Result.System.Code := VIO_SYSEVENT_RESIZE;
        Result.System.Param1 := event^.window.data1;
        Result.System.Param2 := event^.window.data2;
      end;
    end;
    SDL_SYSWMEVENT_ :
    begin
      Result.System.Code := VIO_SYSEVENT_WM;
      // TODO : Windows messages?
    end
    else
      begin
        Result.System.Code := VIO_SYSEVENT_UNKNOWN;
        Result.System.Param1 := event^.type_;
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
  if (ButtonMask and SDL_BUTTON( 1 )) <> 0 then Include( Result, VMB_BUTTON_LEFT );
  if (ButtonMask and SDL_BUTTON( 2 )) <> 0 then Include( Result, VMB_BUTTON_MIDDLE );
  if (ButtonMask and SDL_BUTTON( 3 )) <> 0 then Include( Result, VMB_BUTTON_RIGHT );
  if (ButtonMask and SDL_BUTTON( 4 )) <> 0 then Include( Result, VMB_WHEEL_UP );
  if (ButtonMask and SDL_BUTTON( 5 )) <> 0 then Include( Result, VMB_WHEEL_DOWN );
  if (ButtonMask and SDL_BUTTON( 6 )) <> 0 then Include( Result, VMB_UNKNOWN );
  if (ButtonMask and SDL_BUTTON( 7 )) <> 0 then Include( Result, VMB_UNKNOWN );
end;

function SDLMouseEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  if event^.type_ = SDL_MOUSEWHEEL then
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
  case event^.type_ of
    SDL_MOUSEBUTTONDOWN : Result.EType := VEVENT_MOUSEDOWN;
    SDL_MOUSEBUTTONUP   : Result.EType := VEVENT_MOUSEUP;
  end;
  Result.Mouse.Button  := SDLMouseButtonToVMB( event^.button.button );
  Result.Mouse.Pos.X   := event^.button.x;
  Result.Mouse.Pos.Y   := event^.button.y;
  Result.Mouse.Pressed := event^.button.state = SDL_PRESSED;
end;

function SDLPadDeviceEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_PADDEVICE;
  Result.PadDevice.Which := event^.cdevice.which;
  case event^.type_ of
    SDL_CONTROLLERDEVICEADDED    : Result.PadDevice.Event := VPAD_ADDED;
    SDL_CONTROLLERDEVICEREMOVED  : Result.PadDevice.Event := VPAD_REMOVED;
    SDL_CONTROLLERDEVICEREMAPPED : Result.PadDevice.Event := VPAD_REMAPPED;
  end;
end;

function SDLPadAxisEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType         := VEVENT_PADAXIS;
  Result.PadAxis.Axis  := TIOPadAxis( event^.caxis.axis );
  Result.PadAxis.Value := event^.caxis.value;
  Result.PadAxis.Which := event^.caxis.which;
end;

function SDLPadEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  case event^.type_ of
    SDL_CONTROLLERBUTTONDOWN    : Result.EType := VEVENT_PADDOWN;
    SDL_CONTROLLERBUTTONUP      : Result.EType := VEVENT_PADUP;
  end;
  Result.Pad.Button  := TIOPadButton( event^.cbutton.button );
  Result.Pad.Which   := event^.cbutton.which;
  Result.Pad.Pressed := event^.cbutton.state = SDL_PRESSED;
end;

function SDLTextEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_TEXT;
  Move( event^.text.text[0], Result.Text.Text[0], SDL_TEXTINPUTEVENT_TEXT_SIZE );
end;

function SDLMouseMoveEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_MOUSEMOVE;
  Result.MouseMove.ButtonState := SDLMouseButtonSetToVMB( event^.motion.state );
  Result.MouseMove.Pos.X       := event^.motion.x;
  Result.MouseMove.Pos.Y       := event^.motion.y;
  Result.MouseMove.RelPos.X    := event^.motion.xrel;
  Result.MouseMove.RelPos.Y    := event^.motion.yrel;
  HackLastMouseX := event^.motion.x;
  HackLastMouseY := event^.motion.y;
end;

function SDLEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  case event^.type_ of
    SDL_KEYDOWN : Exit( SDLKeyEventToIOEvent( event ) );
    SDL_KEYUP   : Exit( SDLKeyEventToIOEvent( event ) );
    SDL_TEXTINPUT : Exit( SDLTextEventToIOEvent( event ) );

    SDL_MOUSEMOTION     : Exit( SDLMouseMoveEventToIOEvent( event ) );
    SDL_MOUSEBUTTONDOWN : Exit( SDLMouseEventToIOEvent( event ) );
    SDL_MOUSEBUTTONUP   : Exit( SDLMouseEventToIOEvent( event ) );
    SDL_MOUSEWHEEL      : Exit( SDLMouseEventToIOEvent( event ) );

    SDL_CONTROLLERAXISMOTION : Exit( SDLPadAxisEventToIOEvent( event ) );
    SDL_CONTROLLERBUTTONDOWN,
    SDL_CONTROLLERBUTTONUP : Exit( SDLPadEventToIOEvent( event ) );

    SDL_CONTROLLERDEVICEADDED,
    SDL_CONTROLLERDEVICEREMOVED,
    SDL_CONTROLLERDEVICEREMAPPED :
    begin
      SDLIO.ScanGamepads;
      Exit( SDLPadDeviceEventToIOEvent( event ) );
    end;

    SDL_JOYAXISMOTION : ;
    SDL_JOYBALLMOTION : ;
    SDL_JOYHATMOTION  : ;
    SDL_JOYBUTTONDOWN,
    SDL_JOYBUTTONUP   : ;
    else
      Exit( SDLSystemEventToIOEvent( event ) );
  end;
  Result.EType := VEVENT_SYSTEM;
  Result.System.Code := VIO_SYSEVENT_NONE;
end;

function SDLIOEventFilter( userdata : Pointer; event: PSDL_Event) : Integer; cdecl;
var iCode : TIOKeyCode;
begin
  if event^.type_ = SDL_QUIT_ then
    if Assigned( SDLIO.FOnQuit ) then
      if SDLIO.FOnQuit( SDLEventToIOEvent( event ) ) then
        Exit(0);
  if event^.type_ = SDL_WINDOWEVENT_ then
    if event^.window.event = SDL_WINDOWEVENT_RESIZED then
      if Assigned( SDLIO.FOnResize ) then
        if SDLIO.FOnResize( SDLEventToIOEvent( event ) ) then
          Exit(0);
  if event^.type_ = SDL_KEYDOWN then
  begin
    iCode := SDLKeyEventToKeyCode( event );
    if SDLIO.FInterrupts[iCode] <> nil then
      if SDLIO.FInterrupts[iCode]( SDLKeyEventToIOEvent( event ) ) then
        Exit(0);
  end;
  case event^.type_ of
  SDL_QUIT_,
//  SDL_APP_TERMINATING,
//  SDL_APP_LOWMEMORY,
//  SDL_APP_WILLENTERBACKGROUND,
//  SDL_APP_DIDENTERBACKGROUND,
//  SDL_APP_WILLENTERFOREGROUND,
//  SDL_APP_DIDENTERFOREGROUND,
  SDL_WINDOWEVENT_,
  SDL_SYSWMEVENT_,
  SDL_KEYDOWN,
  SDL_KEYUP,
  SDL_TEXTINPUT,
//  SDL_TEXTEDITING,
//  SDL_TEXTINPUT,
  SDL_MOUSEMOTION,
  SDL_MOUSEBUTTONDOWN,
  SDL_MOUSEBUTTONUP,
  SDL_MOUSEWHEEL : Exit(1);
  SDL_JOYAXISMOTION,
  SDL_JOYBALLMOTION,
  SDL_JOYHATMOTION,
  SDL_JOYBUTTONDOWN,
  SDL_JOYBUTTONUP,
  SDL_JOYDEVICEADDED,
  SDL_JOYDEVICEREMOVED,
  SDL_CONTROLLERAXISMOTION,
  SDL_CONTROLLERBUTTONDOWN,
  SDL_CONTROLLERBUTTONUP,
  SDL_CONTROLLERDEVICEADDED,
  SDL_CONTROLLERDEVICEREMOVED,
  SDL_CONTROLLERDEVICEREMAPPED : if SDLIO.GamePadSupport then Exit(1);
  end;
  Exit(0);
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
var iCurrent      : TSDL_DisplayMode;
    iDisplayIndex : Integer;
begin
{$IFDEF WINDOWS}
  SetDPIAwareness;
{$ENDIF}
  LoadSDL2;
  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) then
  begin
    SDL_Quit();
    SDLIO := nil;
    raise EIOException.Create('Couldn''t initialize SDL : '+SDL_GetError());
  end;
  iDisplayIndex := 0;
  if SDL_GetCurrentDisplayMode( iDisplayIndex, @iCurrent ) <> 0 then
     Exit( False );
  aResult.Init( iCurrent.w, iCurrent.h );
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
  FGamePadIndex   := -1;
  FGamePadHandle  := nil;
  {$IFDEF WINDOWS}
  SetDPIAwareness;
  {$ENDIF}
  LoadSDL2;

  Log('Initializing SDL...');

  if ( SDL_Init( SDL_INIT_VIDEO or SDL_INIT_TIMER or SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER  ) < 0 ) then
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
    iDM       : TSDL_DisplayMode;

// Resize/Toggle
var iFScreen : Boolean;
    iTarget  : TSDL_DisplayMode;
    iClosest : TSDL_DisplayMode;
begin
  if ( aWidth * aHeight = 0 ) then
  begin
    if SDL_GetCurrentDisplayMode( 0, @iDM ) <> 0 then
      raise EIOException.Create(' SDL_GetCurrentDisplayMode returned 0 : '+SDL_GetError());
    aWidth  := iDM.w;
    aHeight := iDM.h;
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
      iTarget.w := aWidth;
      iTarget.h := aHeight;
      iTarget.format       := 0;
      iTarget.refresh_rate := 60;
      iTarget.driverdata   := nil;
      if ( SDL_GetClosestDisplayMode( 0, @iTarget, @iClosest ) = nil ) then
      begin
        Log('Failed to set fullscreen video mode %dx%d/%dbit!', [aWidth,aHeight,aBPP]);
        Exit( False );
      end;

      if FFScreen then SDL_SetWindowFullscreen( FWindow, 0 );
      SDL_SetWindowDisplayMode( FWindow, @iClosest );
      SDL_SetWindowFullscreen( FWindow, SDL_WINDOW_FULLSCREEN );
    end
    else
    begin
      if FFScreen then SDL_SetWindowFullscreen( FWindow, 0 );
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

  iSDLFlags := SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI;
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

  FWindow := SDL_CreateWindow( 'Valkyrie SDL Application',
          SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
          aWidth, aHeight, iSDLFlags );

  if FWindow = nil then Exit( False );

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
  Result := SDL_PollEvent( @event ) > 0;
  if Result then
    aEvent := SDLEventToIOEvent( @event );
end;

function TSDLIODriver.PeekEvent ( out aEvent : TIOEvent ) : Boolean;
var event : SDL_Event;
begin
  SDL_PumpEvents();
  Result := (SDL_PeepEvents( @event, 1, SDL_PEEKEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT ) > 0 );
  if Result then
    aEvent := SDLEventToIOEvent( @event );
end;

function TSDLIODriver.EventPending : Boolean;
var event : SDL_Event;
begin
  SDL_PumpEvents();
  Result := (SDL_PeepEvents( @event, 1, SDL_PEEKEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT ) > 0 );
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
  if FOpenGL then SDL_GL_DeleteContext( FGLContext );
  SDL_DestroyWindow( FWindow );
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
var x,y : Integer;
begin
  //if SDL_GetAppState() and SDL_APPMOUSEFOCUS = 0 then Exit( False );
  x := 0; y := 0;
  SDL_GetMouseState(@x,@y);
  aResult := vutil.Point( x, y );
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
    then SDL_ShowCursor(1)
    else SDL_ShowCursor(0);
end;

procedure TSDLIODriver.ScreenShot ( const aFileName : AnsiString ) ;
var image  : TFPCustomImage;
    writer : TFPWriterPNG;
    data   : PByte;
    sx,sy  : Word;
    x,y    : Word;
begin
try
  try
    sx := GetSizeX;
    sy := GetSizeY;
    Image  := TFPMemoryImage.Create(sx,sy);
    Data   := GetMem( sx*sy*4 );
    glReadPixels(0, 0, sx, sy, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    for x := 0 to sx-1 do
      for y := 0 to sy-1 do
        Image.Colors[x,sy-y-1] := FPColor( Data[ 4*(sx*y+x) ] shl 8, Data[ 4*(sx*y+x)+1 ] shl 8, Data[ 4*(sx*y+x)+2 ] shl 8);
    Writer := TFPWriterPNG.Create;
    Writer.Indexed := False;
    Image.SaveToFile( aFileName, writer );
  finally
    FreeMem( Data );
    FreeAndNil( image );
    FreeAndNil( writer );
  end;
except on e : Exception do
end;
end;

procedure TSDLIODriver.ScanDisplayModes;
var i, iCount   : Integer;
    iMode       : TSDL_DisplayMode;
    iEntry      : TIODisplayMode;
    iIdn, iLast : Integer;
begin
  if Assigned( FDisplayModes )
    then FDisplayModes.Clear
    else FDisplayModes := TIODisplayModeArray.Create;
  iCount := SDL_GetNumDisplayModes( 0 );
  for i := 0 to iCount - 1 do
  begin
    FillChar( iMode, SizeOf( iMode ), 0 );
    if SDL_GetDisplayMode( 0, i, @iMode ) = 0 then
    begin
      iIdn := iMode.h * 100000 + iMode.w;
      if (iMode.w < 1280) or (iIdn = iLast) then
        Continue;
      iLast := iIdn;
      iEntry.Name    := Format( '%dx%d (%dr)', [iMode.w, iMode.h, iMode.refresh_rate] );
      iEntry.Width   := iMode.w;
      iEntry.Height  := iMode.h;
      iEntry.Refresh := iMode.refresh_rate;
      iEntry.Index   := i;
      FDisplayModes.Push( iEntry );
    end
    else
      Log( LOGERROR, 'Failed to get display mode %d : %s', [i, SDL_GetError()]);
  end;
end;

function TSDLIODriver.SetDisplayMode( aIndex : Integer ) : Boolean;
var iMode : TSDL_DisplayMode;
begin
  FillChar( iMode, SizeOf( iMode ), 0 );
  if SDL_GetDisplayMode( 0, aIndex, @iMode ) = 0 then
  begin
    SDL_SetWindowDisplayMode( FWindow, @iMode );
    SDL_SetWindowSize( FWindow, iMode.w, iMode.h );
    Exit( True );
  end;
  Exit( False );
end;

function TSDLIODriver.ScanGamepads( aAllowLoop : Boolean ) : Boolean;
var iController : PSDL_GameController = nil;
    iJoystick   : PSDL_Joystick;
    i, iNew     : Integer;
begin
  Result      := False;
  iNew        := -1;
  iController := nil;
  iJoystick   := nil;
  for i := 0 to SDL_NumJoysticks() - 1 do
    if SDL_IsGameController(i) then
    begin
      if i <> FGamePadIndex then
      begin
        iController := SDL_GameControllerOpen(i);
        iNew := i;
        if iController <> nil then
          Break
        else
          Log(LOGERROR, 'Could not open gamecontroller %d: %s', [i, SDL_GetError()]);
      end
      else
      begin
        iNew := FGamePadIndex;
        Break;
      end;
    end;

  if iNew <> FGamePadIndex then
  begin
    if FGamePadHandle <> nil then
      SDL_GameControllerClose( FGamePadHandle );
    FGamePadIndex  := iNew;
    FGamePadHandle := iController;

    if iController <> nil then
    begin
      iJoystick      := SDL_GameControllerGetJoystick(iController);
      Log('Controller connected.');
      Log('Index: %d  Name: %s', [FGamePadIndex,SDL_GameControllerNameForIndex(FGamePadIndex)]);
      Log('Axes: %d  Buttons: %d  Balls: %d', [SDL_JoystickNumAxes(iJoystick), SDL_JoystickNumButtons(iJoystick),SDL_JoystickNumBalls(iJoystick)]);
    end
    else
      Log('Controller has been removed, no fallback present.');

    Result := True;
  end;

  if (FGamePadHandle <> nil) and (not SDL_GameControllerGetAttached(FGamePadHandle) ) then
  begin
    Log( LOGERROR, 'Controller %d is not attached anymore, disabling!', [FGamePadIndex]);
    SDL_GameControllerClose( FGamePadHandle );
    FGamePadIndex  := -1;
    FGamePadHandle := nil;

    Log( LOGINFO, 'Attempting to reaquire another controller...');
    if aAllowLoop then ScanGamepads( False );
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
  SDL_StartTextInput;
end;

procedure TSDLIODriver.StopTextInput;
begin
  SDL_StopTextInput;
end;

end.

