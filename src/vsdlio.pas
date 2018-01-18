{$INCLUDE valkyrie.inc}
unit vsdlio;
interface
uses Classes, SysUtils, vutil, viotypes, vsdllibrary, vioevent;

type TSDLIOFlag  = ( SDLIO_OpenGL, SDLIO_FullScreen, SDLIO_Resizable );
     TSDLIOFlags = set of TSDLIOFlag;

type TSDLIODriver = class( TIODriver )
  class function GetCurrentResolution( out aResult : TIOPoint ) : Boolean;

  constructor Create( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags );
  function ResetVideoMode( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags ) : Boolean;
  procedure SetupOpenGL;
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
private
  FFlags    : TSDLIOFlags;
  FSizeX    : DWord;
  FSizeY    : DWord;
  FBPP      : DWord;
  FOpenGL   : Boolean;
  FFScreen  : Boolean;
  FOnResize : TIOInterrupt;
public
  property Width : DWord        read FSizeX;
  property Height : DWord       read FSizeY;
  property BPP : DWord          read FBPP;
  property OpenGLMode : Boolean read FOpenGL;
  property FullScreen : Boolean read FFScreen;
  property Flags : TSDLIOFlags  read FFlags;

  property OnResizeEvent : TIOInterrupt write FOnResize;
end;

var SDLIO : TSDLIODriver = nil;

function SDLIOEventFilter(event: PSDL_Event) : Integer; cdecl;

implementation

uses vgllibrary, vglulibrary,
     {Screenshot support}
     FPImage, FPCanvas,
     FPWritePNG;

function SDLSymToCode( Key : TSDLKey ) : Byte;
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
    SDLK_KP1            : Result := VKEY_END;
    SDLK_KP2            : Result := VKEY_DOWN;
    SDLK_KP3            : Result := VKEY_PGDOWN;
    SDLK_KP4            : Result := VKEY_LEFT;
    SDLK_KP5            : Result := VKEY_CENTER;
    SDLK_KP6            : Result := VKEY_RIGHT;
    SDLK_KP7            : Result := VKEY_HOME;
    SDLK_KP8            : Result := VKEY_UP;
    SDLK_KP9            : Result := VKEY_PGUP;
    SDLK_KP_ENTER       : Result := VKEY_ENTER;
  else
    if Key in VKEY_SCANSET then
      Result := Key;
  end;
end;

function SDLKeyEventToKeyCode( event : PSDL_Event ) : TIOKeyCode;
var smod : TSDLMod;
begin
  Result := SDLSymToCode( event^.key.keysym.sym );
  smod := event^.key.keysym.modifier;
  if smod and KMOD_CTRL  <> 0 then Result += IOKeyCodeCtrlMask;
  if smod and KMOD_SHIFT <> 0 then Result += IOKeyCodeShiftMask;
  if smod and KMOD_ALT   <> 0 then Result += IOKeyCodeAltMask;
end;

function SDLModToModKeySet( smod : TSDLMod ) : TIOModKeySet;
begin
  Result := [];
  if smod = KMOD_NONE then Exit;
  if smod and KMOD_CTRL  <> 0 then Include( Result, VKMOD_CTRL );
  if smod and KMOD_SHIFT <> 0 then Include( Result, VKMOD_SHIFT );
  if smod and KMOD_ALT   <> 0 then Include( Result, VKMOD_ALT );
end;

function SDLKeyEventToIOEvent( event : PSDL_Event ) : TIOEvent;
var ASCII : Char;
begin
  ASCII := Char(event^.key.keysym.unicode);
  // Dirty patch to make diagonal keys work on Mac. Probably other PC keys are also broken.
  // Looks like on Mac we only have reliable information in event^.key.keysym.sym.
  // The function should be rewritten to utilize it.
  if (Ord(ASCII) in VKEY_PRINTABLESET)
    and (event^.key.keysym.sym <> SDLK_PAGEDOWN)
    and (event^.key.keysym.sym <> SDLK_PAGEUP)
    and (event^.key.keysym.sym <> SDLK_HOME)
    and (event^.key.keysym.sym <> SDLK_END)
  then
  begin
    Result := PrintableToIOEvent( ASCII );
    if event^.type_ = SDL_KEYUP then Result.EType := VEVENT_KEYUP;
    Exit;
  end;
  if event^.type_ = SDL_KEYDOWN
    then Result.EType := VEVENT_KEYDOWN
    else Result.EType := VEVENT_KEYUP;
  Result.Key.ASCII    := #0;
  Result.Key.ModState := SDLModToModKeySet( event^.key.keysym.modifier );
  Result.Key.Code     := SDLSymToCode( event^.key.keysym.sym );
end;

function SDLSystemEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_SYSTEM;
  Result.System.Param1 := 0;
  Result.System.Param2 := 0;

  case event^.type_ of
    SDL_QUITEV      : Result.System.Code := VIO_SYSEVENT_QUIT;
    SDL_VIDEOEXPOSE : Result.System.Code := VIO_SYSEVENT_EXPOSE;
    SDL_ACTIVEEVENT :
      begin
        Result.System.Code := VIO_SYSEVENT_ACTIVE;
        Result.System.Param1 := event^.active.gain;
        Result.System.Param2 := event^.active.state;
      end;
    SDL_VIDEORESIZE :
      begin
        Result.System.Code := VIO_SYSEVENT_RESIZE;
        Result.System.Param1 := event^.resize.w;
        Result.System.Param2 := event^.resize.h;
      end;
    SDL_SYSWMEVENT :
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
    SDL_BUTTON_WHEELUP  : Exit( VMB_WHEEL_UP );
    SDL_BUTTON_WHEELDOWN: Exit( VMB_WHEEL_DOWN );
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
  case event^.type_ of
    SDL_MOUSEBUTTONDOWN : Result.EType := VEVENT_MOUSEDOWN;
    SDL_MOUSEBUTTONUP   : Result.EType := VEVENT_MOUSEUP;
  end;
  Result.Mouse.Button  := SDLMouseButtonToVMB( event^.button.button );
  Result.Mouse.Pos.X   := event^.button.x;
  Result.Mouse.Pos.Y   := event^.button.y;
  Result.Mouse.Pressed := event^.button.state = SDL_PRESSED;
end;

function SDLMouseMoveEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  Result.EType := VEVENT_MOUSEMOVE;
  Result.MouseMove.ButtonState := SDLMouseButtonSetToVMB( event^.motion.state );
  Result.MouseMove.Pos.X       := event^.motion.x;
  Result.MouseMove.Pos.Y       := event^.motion.y;
  Result.MouseMove.RelPos.X    := event^.motion.xrel;
  Result.MouseMove.RelPos.Y    := event^.motion.yrel;
end;

function SDLEventToIOEvent( event : PSDL_Event ) : TIOEvent;
begin
  case event^.type_ of
    SDL_KEYDOWN : Exit( SDLKeyEventToIOEvent( event ) );
    SDL_KEYUP   : Exit( SDLKeyEventToIOEvent( event ) );

    SDL_MOUSEMOTION     : Exit( SDLMouseMoveEventToIOEvent( event ) );
    SDL_MOUSEBUTTONDOWN : Exit( SDLMouseEventToIOEvent( event ) );
    SDL_MOUSEBUTTONUP   : Exit( SDLMouseEventToIOEvent( event ) );

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

function SDLIOEventFilter(event: PSDL_Event) : Integer; cdecl;
var iCode : TIOKeyCode;
begin
  if event^.type_ = SDL_QUITEV then
    if Assigned( SDLIO.FOnQuit ) then
      if SDLIO.FOnQuit( SDLEventToIOEvent( event ) ) then
        Exit(0);
  if event^.type_ = SDL_VIDEORESIZE then
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
  Exit(1);
end;

{ TSDLIODriver }

class function TSDLIODriver.GetCurrentResolution ( out aResult : TIOPoint ) : Boolean;
var info : PSDL_VideoInfo;
begin
  LoadSDL;
  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) then
  begin
    SDL_Quit();
    SDLIO := nil;
    raise EIOException.Create('Couldn''t initialize SDL : '+SDL_GetError());
  end;

  info := SDL_GetVideoInfo();
  if info = nil then Exit( False );
  aResult.Init( info^.current_w, info^.current_h );
  Exit( True );
end;

constructor TSDLIODriver.Create( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags );
begin
  ClearInterrupts;
  SDLIO := Self;
  inherited Create;
  LoadSDL;
  if SDLIO_OpenGL in aFlags then
  begin
    LoadGL;
    LoadGLu;
  end;

  Log('Initializing SDL...');

  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) then
  begin
    SDL_Quit();
    SDLIO := nil;
    raise EIOException.Create('Couldn''t initialize SDL : '+SDL_GetError());
  end;

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


  SDL_WM_SetCaption('Valkyrie SDL Application','VSDL Application');

  SDL_EventState(SDL_ACTIVEEVENT, SDL_IGNORE);
  SDL_EventState(SDL_KEYUP, SDL_IGNORE);
//  SDL_EventState(SDL_MOUSEMOTION, SDL_IGNORE);
//  SDL_EventState(SDL_MOUSEBUTTONDOWN, SDL_IGNORE);
//  SDL_EventState(SDL_MOUSEBUTTONUP, SDL_IGNORE);
//  SDL_EventState(SDL_VIDEORESIZE, SDL_IGNORE);
//  SDL_EventState(SDL_VIDEOEXPOSE, SDL_IGNORE);
//  SDL_EventState(SDL_USEREVENT, SDL_IGNORE);

  SDL_EnableUNICODE(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
  SDL_SetEventFilter(@SDLIOEventFilter);

  Log('SDL IO system ready.');
end;

function TSDLIODriver.ResetVideoMode ( aWidth, aHeight, aBPP : Word; aFlags : TSDLIOFlags ) : Boolean;
var iSDLFlags : DWord;
begin
  iSDLFlags := 0;
  FFScreen  := SDLIO_FullScreen in aFlags;
  FOpenGL   := SDLIO_OpenGL in aFlags;
  FSizeX    := aWidth;
  FSizeY    := aHeight;
  FBPP      := aBPP;
  FFlags    := aFlags;

  if not FOpenGL then
  begin
    iSDLFlags := iSDLFlags or SDL_HWSURFACE;
    iSDLFlags := iSDLFlags or SDL_DOUBLEBUF;
  end;

  if FOpenGL  then iSDLFlags := iSDLFlags or SDL_OPENGL;
  if FFScreen then iSDLFlags := iSDLFlags or SDL_FULLSCREEN;

  if SDLIO_Resizable in aFlags then iSDLFlags := iSDLFlags or SDL_RESIZABLE;

  Log('Checking mode %dx%d/%dbit...', [aWidth,aHeight,aBPP]);

  if aBPP <> SDL_VideoModeOK( aWidth, aHeight, aBPP, iSDLFlags ) then Exit( False );

  if FOpenGL then
  begin
    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
    SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  end;

  if SDL_SetVideoMode( aWidth, aHeight, aBPP, iSDLFlags ) = nil then Exit( False );

  if FOpenGL then SetupOpenGL;
  Exit( True );
end;

procedure TSDLIODriver.SetupOpenGL;
begin
  glShadeModel( GL_SMOOTH );
  glClearColor( 0.0, 0.0, 0.0, 0.0 );
  glClearDepth( 1.0 );
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );
  glHint( GL_LINE_SMOOTH_HINT,            GL_NICEST );
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );
  glHint( GL_POINT_SMOOTH_HINT,           GL_NICEST );
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
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity( );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity( );
end;

procedure TSDLIODriver.Sleep ( Milliseconds : DWord ) ;
begin
  SDL_Delay( Milliseconds );
end;

function TSDLIODriver.PollEvent ( out aEvent : TIOEvent ) : Boolean;
var event : TSDL_Event;
begin
  Result := SDL_PollEvent( @event ) > 0;
  if Result then
    aEvent := SDLEventToIOEvent( @event );
end;

function TSDLIODriver.PeekEvent ( out aEvent : TIOEvent ) : Boolean;
var event : TSDL_Event;
begin
  SDL_PumpEvents();
  Result := (SDL_PeepEvents( @event, 1, SDL_PEEKEVENT, SDL_ALLEVENTS ) > 0 );
  if Result then
    aEvent := SDLEventToIOEvent( @event );
end;

function TSDLIODriver.EventPending : Boolean;
var event : TSDL_Event;
begin
  SDL_PumpEvents();
  Result := (SDL_PeepEvents( @event, 1, SDL_PEEKEVENT, SDL_ALLEVENTS ) > 0 );
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
    SDL_GL_SwapBuffers()
  else
    SDL_Flip( SDL_GetVideoSurface() );
end;

destructor TSDLIODriver.Destroy;
begin
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
  if SDL_GetAppState() and SDL_APPMOUSEFOCUS = 0 then Exit( False );
  x := 0; y := 0;
  SDL_GetMouseState(x,y);
  aResult := Point( x, y );
  Exit( True );
end;

function TSDLIODriver.GetMouseButtonState ( out aResult : TIOMouseButtonSet
  ) : Boolean;
var x,y : Integer;
begin
  if SDL_GetAppState() and SDL_APPMOUSEFOCUS = 0 then Exit( False );
  x := 0; y := 0;
  aResult := SDLMouseButtonSetToVMB( SDL_GetMouseState(x,y) );
  Exit( True );
end;

function TSDLIODriver.GetModKeyState : TIOModKeySet;
begin
  Exit( SDLModToModKeySet( SDL_GetModState() ) );
end;

procedure TSDLIODriver.SetTitle ( const aLongTitle : AnsiString;
  const aShortTitle : AnsiString ) ;
begin
  if aShortTitle = ''
    then SDL_WM_SetCaption(PChar(aLongTitle),PChar(aLongTitle))
    else SDL_WM_SetCaption(PChar(aLongTitle),PChar(aShortTitle));
end;

procedure TSDLIODriver.ShowMouse ( aShow : Boolean ) ;
begin
  if aShow
    then SDL_ShowCursor(1)
    else SDL_ShowCursor(0);

  SDL_GetVideoInfo()
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


end.

