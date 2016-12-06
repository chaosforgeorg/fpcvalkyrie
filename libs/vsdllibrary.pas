unit vsdllibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, Types, vlibrary,
{$IFDEF WINDOWS}
  Windows;
{$ENDIF}
{$IFDEF UNIX}
  pthreads,
  baseunix,
  {$IFDEF DARWIN}
     unix;
  {$ELSE}
     unix, x, xlib;
  {$ENDIF}
{$ENDIF}

const
{$IFDEF WINDOWS}
  SDLDefaultPath = 'SDL.dll';
{$ELSE}
  {$IFDEF DARWIN}
    SDLDefaultPath = 'SDL.framework/SDL';
    {$linklib SDLmain}
    {$linkframework Cocoa}
    {$linkframework SDL}
    {$PASCALMAINNAME SDL_main}
  {$ELSE}
  SDLDefaultPath = 'libSDL-1.2.so.0';
  {$ENDIF}
{$ENDIF}

const

  SDL_MAJOR_VERSION = 1;
  SDL_MINOR_VERSION = 2;
  SDL_PATCHLEVEL    = 13;

{$include vsdlconst.inc}
{$include vsdltypes.inc}

{$DEFINE calldecl := cdecl}

// initialization

var
  SDL_Init          : function ( flags : UInt32 ) : Integer; calldecl;
  SDL_InitSubSystem : function ( flags : UInt32 ) : Integer; calldecl;
  SDL_QuitSubSystem : procedure( flags : UInt32 ); calldecl;
  SDL_WasInit       : function ( flags : UInt32 ) : UInt32; calldecl;
  SDL_Quit          : procedure; calldecl;
{$IFDEF WINDOWS}
  SDL_RegisterApp   : function (name: PChar; style: UInt32; h_Inst: Pointer): Integer; calldecl;
{$ENDIF}

// error handling
  SDL_GetError      : function : PChar; calldecl;
  SDL_SetError      : procedure ( fmt : PChar ); calldecl;
  SDL_ClearError    : procedure; calldecl;

// io handling
var
  SDL_RWFromFile     : function (filename, mode: PChar): PSDL_RWops; calldecl;
  SDL_FreeRW         : procedure (area: PSDL_RWops); calldecl;
  SDL_RWFromFP       : function (fp: Pointer; autoclose: Integer): PSDL_RWops; calldecl;
  SDL_RWFromMem      : function (mem: Pointer; size: Integer): PSDL_RWops; calldecl;
  SDL_RWFromConstMem : function (const mem: Pointer; size: Integer) : PSDL_RWops; calldecl;
  SDL_AllocRW        : function : PSDL_RWops; calldecl;

function SDL_RWSeek( context : PSDL_RWops; offset: Integer; whence: Integer ) : Integer; calldecl;
function SDL_RWTell( context : PSDL_RWops ) : Integer; calldecl;
function SDL_RWRead( context : PSDL_RWops; ptr: Pointer; size: Integer; n : Integer ) : Integer; calldecl;
function SDL_RWWrite( context : PSDL_RWops; ptr: Pointer; size: Integer; n : Integer ) : Integer; calldecl;
function SDL_RWClose( context : PSDL_RWops ) : Integer; calldecl;

var
// time-handling
  SDL_GetTicks       : function : UInt32; calldecl;
  SDL_Delay          : procedure (msec: UInt32); calldecl;
  SDL_AddTimer       : function (interval: UInt32; callback: TSDL_NewTimerCallback; param : Pointer): PSDL_TimerID; calldecl;
  SDL_RemoveTimer    : function (t: PSDL_TimerID): TSDL_Bool; calldecl;
  SDL_SetTimer       : function (interval: UInt32; callback: TSDL_TimerCallback): Integer; calldecl;

// audio-routines

  SDL_AudioInit      : function (driver_name: PChar): Integer; calldecl;
  SDL_AudioQuit      : procedure ; calldecl;
  SDL_AudioDriverName: function (namebuf: PChar; maxlen: Integer): PChar; calldecl;
  SDL_OpenAudio      : function (desired, obtained: PSDL_AudioSpec): Integer; calldecl;
  SDL_GetAudioStatus : function : TSDL_Audiostatus; calldecl;
  SDL_PauseAudio     : procedure (pause_on: Integer); calldecl;
  SDL_LoadWAV_RW     : function (src: PSDL_RWops; freesrc: Integer; spec:
    PSDL_AudioSpec; audio_buf: PUInt8; audiolen: PUInt32): PSDL_AudioSpec; calldecl;
  SDL_FreeWAV        : procedure (audio_buf: PUInt8); calldecl;
  SDL_BuildAudioCVT  : function (cvt: PSDL_AudioCVT; src_format: UInt16;
    src_channels: UInt8; src_rate: Integer; dst_format: UInt16; dst_channels: UInt8;
    dst_rate: Integer): Integer; calldecl;
  SDL_ConvertAudio   : function (cvt: PSDL_AudioCVT): Integer; calldecl;
  SDL_MixAudio       : procedure (dst, src: PUInt8; len: UInt32; volume: Integer); calldecl;
  SDL_LockAudio      : procedure ; calldecl;
  SDL_UnlockAudio    : procedure ; calldecl;
  SDL_CloseAudio     : procedure ; calldecl;

// CD-routines }
  SDL_CDNumDrives    : function : Integer; calldecl;
  SDL_CDName         : function (drive: Integer): PChar; calldecl;
  SDL_CDOpen         : function (drive: Integer): PSDL_CD; calldecl;
  SDL_CDStatus       : function (cdrom: PSDL_CD): TSDL_CDStatus; calldecl;
  SDL_CDPlayTracks   : function (cdrom: PSDL_CD; start_track: Integer; start_frame:
    Integer; ntracks: Integer; nframes: Integer): Integer; calldecl;
  SDL_CDPlay         : function (cdrom: PSDL_CD; start: Integer; length: Integer): Integer; calldecl;
  SDL_CDPause        : function (cdrom: PSDL_CD): Integer; calldecl;
  SDL_CDResume       : function (cdrom: PSDL_CD): Integer; calldecl;
  SDL_CDStop         : function (cdrom: PSDL_CD): Integer; calldecl;
  SDL_CDEject        : function (cdrom: PSDL_CD): Integer; calldecl;
  SDL_CDClose        : procedure (cdrom: PSDL_CD); calldecl;

function SDL_CDInDrive( status : TSDL_CDStatus ) : LongBool; calldecl;
procedure FRAMES_TO_MSF(frames: Integer; var M: Integer; var S: Integer; var F: Integer); calldecl;
function MSF_TO_FRAMES(M: Integer; S: Integer; F: Integer): Integer; calldecl;

// JoyStick-routines
var
  SDL_NumJoysticks      : function : Integer; calldecl;
  SDL_JoystickName      : function (index: Integer): PChar; calldecl;
  SDL_JoystickOpen      : function (index: Integer): PSDL_Joystick; calldecl;
  SDL_JoystickOpened    : function (index: Integer): Integer; calldecl;
  SDL_JoystickIndex     : function (joystick: PSDL_Joystick): Integer; calldecl;
  SDL_JoystickNumAxes   : function (joystick: PSDL_Joystick): Integer; calldecl;
  SDL_JoystickNumBalls  : function (joystick: PSDL_Joystick): Integer; calldecl;
  SDL_JoystickNumHats   : function (joystick: PSDL_Joystick): Integer; calldecl;
  SDL_JoystickNumButtons: function (joystick: PSDL_Joystick): Integer; calldecl;
  SDL_JoystickUpdate    : procedure ; calldecl;
  SDL_JoystickEventState: function (state: Integer): Integer; calldecl;
  SDL_JoystickGetAxis   : function (joystick: PSDL_Joystick; axis: Integer) : SInt16; calldecl;
  SDL_JoystickGetHat    : function (joystick: PSDL_Joystick; hat: Integer): UInt8; calldecl;
  SDL_JoystickGetBall   : function (joystick: PSDL_Joystick; ball: Integer; var dx: Integer; var dy: Integer): Integer; calldecl;
  SDL_JoystickGetButton : function ( joystick: PSDL_Joystick; Button: Integer): UInt8; calldecl;
  SDL_JoystickClose     : procedure (joystick: PSDL_Joystick); calldecl;

// event-handling
  SDL_PumpEvents        : procedure ; calldecl;
  SDL_PeepEvents        : function (events: PSDL_Event; numevents: Integer; action: TSDL_eventaction; mask: UInt32): Integer; calldecl;
  SDL_PollEvent         : function (event: PSDL_Event): Integer; calldecl;
  SDL_WaitEvent         : function (event: PSDL_Event): Integer; calldecl;
  SDL_PushEvent         : function ( event : PSDL_Event ) : Integer; calldecl;
  SDL_SetEventFilter    : procedure ( filter : TSDL_EventFilter ); calldecl;
  SDL_GetEventFilter    : function : TSDL_EventFilter; calldecl;
  SDL_EventState        : function (type_: UInt8; state: Integer): UInt8; calldecl;

// Version Routines

procedure SDL_VERSION(var X: TSDL_Version);
function SDL_VERSIONNUM(X, Y, Z: Integer): Integer;
function SDL_COMPILEDVERSION: Integer;
function SDL_VERSION_ATLEAST(X: Integer; Y: Integer; Z: Integer) : LongBool;
function SDL_RWopsFromStream ( Stream : TStream; Size : DWord ) : PSDL_RWops;

var
  SDL_Linked_Version : function : PSDL_version; calldecl;

// video                                                                        }
var
  SDL_VideoInit           : function (driver_name: PChar; flags: UInt32): Integer; calldecl;
  SDL_VideoQuit           : procedure ; calldecl;
  SDL_VideoDriverName     : function (namebuf: PChar; maxlen: Integer): PChar; calldecl;
  SDL_GetVideoSurface     : function : PSDL_Surface; calldecl;
  SDL_GetVideoInfo        : function : PSDL_VideoInfo; calldecl;
  SDL_VideoModeOK         : function (width, height, bpp: Integer; flags: UInt32): Integer; calldecl;
  SDL_ListModes           : function (format: PSDL_PixelFormat; flags: UInt32): PPSDL_Rect; calldecl;
  SDL_SetVideoMode        : function (width, height, bpp: Integer; flags: UInt32): PSDL_Surface; calldecl;
  SDL_UpdateRects         : procedure (screen: PSDL_Surface; numrects: Integer; rects: PSDL_Rect); calldecl;
  SDL_UpdateRect          : procedure (screen: PSDL_Surface; x, y: SInt32; w, h: UInt32); calldecl;
  SDL_Flip                : function (screen: PSDL_Surface): Integer; calldecl;
  SDL_SetGamma            : function (redgamma: single; greengamma: single; bluegamma: single ): Integer; calldecl;
  SDL_SetGammaRamp        : function ( redtable: PUInt16; greentable: PUInt16; bluetable: PUInt16): Integer; calldecl;
  SDL_GetGammaRamp        : function ( redtable: PUInt16; greentable: PUInt16; bluetable: PUInt16): Integer; calldecl;
  SDL_SetColors           : function (surface: PSDL_Surface; colors: PSDL_Color; firstcolor : Integer; ncolors: Integer) : Integer; calldecl;
  SDL_SetPalette          : function (surface: PSDL_Surface; flags: Integer; colors: PSDL_Color; firstcolor: Integer; ncolors: Integer): Integer; calldecl;
  SDL_MapRGB              : function (format: PSDL_PixelFormat; r: UInt8; g: UInt8; b: UInt8) : UInt32; calldecl;
  SDL_MapRGBA             : function (format: PSDL_PixelFormat; r: UInt8; g: UInt8; b: UInt8; a: UInt8): UInt32; calldecl;
  SDL_GetRGB              : procedure (pixel: UInt32; fmt: PSDL_PixelFormat; r: PUInt8; g: PUInt8; b: PUInt8); calldecl;
  SDL_GetRGBA             : procedure (pixel: UInt32; fmt: PSDL_PixelFormat; r: PUInt8; g: PUInt8; b: PUInt8; a: PUInt8); calldecl;
  SDL_CreateRGBSurface    : function (flags: UInt32; width, height, depth: Integer; RMask, GMask, BMask, AMask: UInt32): PSDL_Surface; calldecl;
  SDL_CreateRGBSurfaceFrom: function (pixels: Pointer; width, height, depth, pitch
    : Integer; RMask, GMask, BMask, AMask: UInt32): PSDL_Surface; calldecl;
  SDL_FreeSurface         : procedure (surface: PSDL_Surface); calldecl;
  SDL_LockSurface         : function (surface: PSDL_Surface): Integer; calldecl;
  SDL_UnlockSurface       : procedure (surface: PSDL_Surface); calldecl;
  SDL_LoadBMP_RW          : function (src: PSDL_RWops; freesrc: Integer): PSDL_Surface; calldecl;
  SDL_SaveBMP_RW          : function (surface: PSDL_Surface; dst: PSDL_RWops; freedst: Integer): Integer; calldecl;
  SDL_SetColorKey         : function (surface: PSDL_Surface; flag, key: UInt32) : Integer; calldecl;
  SDL_SetAlpha            : function (surface: PSDL_Surface; flag: UInt32; alpha: UInt8): Integer; calldecl;
  SDL_SetClipRect         : procedure (surface: PSDL_Surface; rect: PSDL_Rect); calldecl;
  SDL_GetClipRect         : procedure (surface: PSDL_Surface; rect: PSDL_Rect); calldecl;
  SDL_ConvertSurface      : function (src: PSDL_Surface; fmt: PSDL_PixelFormat; flags: UInt32): PSDL_Surface; calldecl;
  SDL_UpperBlit           : function (src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; calldecl;
  SDL_LowerBlit           : function (src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; calldecl;
  SDL_FillRect            : function (dst: PSDL_Surface; dstrect: PSDL_Rect; color: UInt32) : Integer; calldecl;
  SDL_DisplayFormat       : function (surface: PSDL_Surface): PSDL_Surface; calldecl;
  SDL_DisplayFormatAlpha  : function (surface: PSDL_Surface): PSDL_Surface; calldecl;
  SDL_CreateYUVOverlay    : function (width: Integer; height: Integer; format: UInt32; display: PSDL_Surface): PSDL_Overlay; calldecl;
  SDL_LockYUVOverlay      : function (Overlay: PSDL_Overlay): Integer; calldecl;
  SDL_UnlockYUVOverlay    : procedure (Overlay: PSDL_Overlay); calldecl;
  SDL_DisplayYUVOverlay   : function (Overlay: PSDL_Overlay; dstrect: PSDL_Rect) : Integer; calldecl;
  SDL_FreeYUVOverlay      : procedure (Overlay: PSDL_Overlay); calldecl;

function SDL_MustLock(Surface: PSDL_Surface): Boolean; calldecl;
function SDL_AllocSurface(flags: UInt32; width, height, depth: Integer;
    RMask, GMask, BMask, AMask: UInt32): PSDL_Surface; calldecl;
function SDL_LoadBMP(filename: PChar): PSDL_Surface; calldecl;
function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer; calldecl;
function SDL_LoadWAV(filename: PChar; spec: PSDL_AudioSpec; audio_buf: PUInt8; audiolen: PUInt32): PSDL_AudioSpec; calldecl;
function SDL_BlitSurface(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; calldecl;

// GL-functions
var
  SDL_GL_LoadLibrary    : function (filename: PChar): Integer; calldecl;
  SDL_GL_GetProcAddress : function (procname: PChar) : Pointer; calldecl;
  SDL_GL_SetAttribute   : function (attr: TSDL_GLAttr; value: Integer) : Integer; calldecl;
  SDL_GL_GetAttribute   : function (attr: TSDL_GLAttr; var value: Integer): Integer; calldecl;
  SDL_GL_SwapBuffers    : procedure ; calldecl;
  SDL_GL_UpdateRects    : procedure (numrects: Integer; rects: PSDL_Rect); calldecl;
  SDL_GL_Lock           : procedure ; calldecl;
  SDL_GL_Unlock         : procedure ; calldecl;


// Window manager
  SDL_WM_GetCaption       : procedure (var title : PChar; var icon : PChar); calldecl;
  SDL_WM_SetCaption       : procedure ( const title : PChar; const icon : PChar); calldecl;
  SDL_WM_SetIcon          : procedure (icon: PSDL_Surface; mask: UInt8); calldecl;
  SDL_WM_IconifyWindow    : function : Integer; calldecl;
  SDL_WM_ToggleFullScreen : function (surface: PSDL_Surface): Integer; calldecl;
  SDL_WM_GrabInput        : function (mode: TSDL_GrabMode): TSDL_GrabMode; calldecl;
  SDL_GetWMInfo           : function (info : PSDL_SysWMinfo) : integer; calldecl;

// Mouse-routines
  SDL_GetMouseState         : function (var x: Integer; var y: Integer): UInt8; calldecl;
  SDL_GetRelativeMouseState : function (var x: Integer; var y: Integer): UInt8; calldecl;
  SDL_WarpMouse             : procedure (x, y: UInt16); calldecl;
  SDL_CreateCursor          : function (data, mask: PUInt8; w, h, hot_x, hot_y: Integer): PSDL_Cursor; calldecl;
  SDL_SetCursor             : procedure (cursor: PSDL_Cursor); calldecl;
  SDL_GetCursor             : function : PSDL_Cursor; calldecl;
  SDL_FreeCursor            : procedure (cursor: PSDL_Cursor); calldecl;
  SDL_ShowCursor            : function (toggle: Integer): Integer; calldecl;

function SDL_BUTTON( Button : Integer ) : Integer;

// Keyboard-routines
var
  SDL_EnableUNICODE   : function (enable: Integer): Integer; calldecl;
  SDL_EnableKeyRepeat : function (delay: Integer; interval: Integer): Integer; calldecl;
  SDL_GetKeyRepeat    : procedure (delay : PInteger; interval: PInteger); calldecl;
  SDL_GetKeyState     : function (numkeys: PInt): PUInt8; calldecl;
  SDL_GetModState     : function : TSDLMod; calldecl;
  SDL_SetModState     : procedure (modstate: TSDLMod); calldecl;
  SDL_GetKeyName      : function (key: TSDLKey): PChar; calldecl;

// Active Routines
  SDL_GetAppState : function : UInt8; calldecl;

// Mutex functions
  SDL_CreateMutex  : function : PSDL_Mutex; calldecl;
  SDL_mutexP       : function (mutex: PSDL_mutex): Integer; calldecl;
  SDL_mutexV       : function (mutex: PSDL_mutex): Integer; calldecl;
  SDL_DestroyMutex : procedure (mutex: PSDL_mutex); calldecl;

function SDL_LockMutex(mutex: PSDL_mutex): Integer; calldecl;
function SDL_UnlockMutex(mutex: PSDL_mutex): Integer; calldecl;

// Semaphore functions
var
  SDL_CreateSemaphore  : function (initial_value: UInt32): PSDL_Sem; calldecl;
  SDL_DestroySemaphore : procedure (sem: PSDL_sem); calldecl;
  SDL_SemWait          : function (sem: PSDL_sem): Integer; calldecl;
  SDL_SemTryWait       : function (sem: PSDL_sem): Integer; calldecl;
  SDL_SemWaitTimeout   : function (sem: PSDL_sem; ms: UInt32): Integer; calldecl;
  SDL_SemPost          : function (sem: PSDL_sem): Integer; calldecl;
  SDL_SemValue         : function (sem: PSDL_sem): UInt32; calldecl;

// Condition variable functions
  SDL_CreateCond       : function : PSDL_Cond; calldecl;
  SDL_DestroyCond      : procedure (cond: PSDL_Cond); calldecl;
  SDL_CondSignal       : function (cond: PSDL_cond): Integer; calldecl;
  SDL_CondBroadcast    : function (cond: PSDL_cond): Integer; calldecl;
  SDL_CondWait         : function (cond: PSDL_cond; mut: PSDL_mutex): Integer; calldecl;
  SDL_CondWaitTimeout  : function (cond: PSDL_cond; mut: PSDL_mutex; ms: UInt32) : Integer; calldecl;

// Thread functions
  SDL_CreateThread : function (fn: PInt; data: Pointer): PSDL_Thread; calldecl;
  SDL_ThreadID     : function : UInt32; calldecl;
  SDL_GetThreadID  : function (thread: PSDL_Thread): UInt32; calldecl;
  SDL_WaitThread   : procedure (thread: PSDL_Thread; var status: Integer); calldecl;
  SDL_KillThread   : procedure (thread: PSDL_Thread); calldecl;


function SDL_Swap32(D: Uint32): Uint32;

var
  SDL : TLibrary = nil;

function LoadSDL( const aPath : AnsiString = SDLDefaultPath ) : Boolean;

implementation

function SDL_RWSeek(context: PSDL_RWops; offset: Integer; whence: Integer) : Integer; calldecl;
begin
  Result := context^.seek(context, offset, whence);
end;

function SDL_RWTell(context: PSDL_RWops): Integer; calldecl;
begin
  Result := context^.seek(context, 0, 1);
end;

function SDL_RWRead(context: PSDL_RWops; ptr: Pointer; size: Integer; n: Integer): Integer; calldecl;
begin
  Result := context^.read(context, ptr, size, n);
end;

function SDL_RWWrite(context: PSDL_RWops; ptr: Pointer; size: Integer; n: Integer): Integer; calldecl;
begin
  Result := context^.write(context, ptr, size, n);
end;

function SDL_RWClose(context: PSDL_RWops): Integer; calldecl;
begin
  Result := context^.close(context);
end;

function SDL_CDInDrive( status : TSDL_CDStatus ): LongBool; calldecl;
begin
  Result := ord( status ) > ord( CD_ERROR );
end;

procedure FRAMES_TO_MSF(frames: Integer; var M: Integer; var S: Integer; var
  F: Integer); calldecl;
var
  value: Integer;
begin
  value := frames;
  F := value mod CD_FPS;
  value := value div CD_FPS;
  S := value mod 60;
  value := value div 60;
  M := value;
end;

function MSF_TO_FRAMES(M: Integer; S: Integer; F: Integer): Integer; calldecl;
begin
  Result := M * 60 * CD_FPS + S * CD_FPS + F;
end;

procedure SDL_VERSION(var X: TSDL_Version);
begin
  X.major := SDL_MAJOR_VERSION;
  X.minor := SDL_MINOR_VERSION;
  X.patch := SDL_PATCHLEVEL;
end;

function SDL_VERSIONNUM(X, Y, Z: Integer): Integer;
begin
  Result := X * 1000 + Y * 100 + Z;
end;

function SDL_COMPILEDVERSION: Integer;
begin
  Result := SDL_VERSIONNUM(SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL
    );
end;

function SDL_VERSION_ATLEAST(X, Y, Z: Integer): LongBool;
begin
  Result := (SDL_COMPILEDVERSION >= SDL_VERSIONNUM(X, Y, Z));
end;

function SDL_LoadWAV(filename: PChar; spec: PSDL_AudioSpec; audio_buf: PUInt8; audiolen: PUInt32): PSDL_AudioSpec; calldecl;
begin
  Result := SDL_LoadWAV_RW(SDL_RWFromFile(filename, 'rb'), 1, spec, audio_buf, audiolen);
end;

function SDL_LoadBMP(filename: PChar): PSDL_Surface; calldecl;
begin
  Result := SDL_LoadBMP_RW(SDL_RWFromFile(filename, 'rb'), 1);
end;

function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer; calldecl;
begin
  Result := SDL_SaveBMP_RW(surface, SDL_RWFromFile(filename, 'wb'), 1);
end;

function SDL_BlitSurface(src: PSDL_Surface; srcrect: PSDL_Rect; dst:
  PSDL_Surface;
  dstrect: PSDL_Rect): Integer; calldecl;
begin
  Result := SDL_UpperBlit(src, srcrect, dst, dstrect);
end;

function SDL_AllocSurface(flags: UInt32; width, height, depth: Integer;
  RMask, GMask, BMask, AMask: UInt32): PSDL_Surface; calldecl;
begin
  Result := SDL_CreateRGBSurface(flags, width, height, depth, RMask, GMask,
    BMask, AMask);
end;

function SDL_MustLock(Surface: PSDL_Surface): Boolean; calldecl;
begin
  Result := ( ( surface^.offset <> 0 ) or
           ( ( surface^.flags and ( SDL_HWSURFACE or SDL_ASYNCBLIT or SDL_RLEACCEL ) ) <> 0 ) );
end;

function SDL_LockMutex(mutex: PSDL_mutex): Integer; calldecl;
begin
  Result := SDL_mutexP(mutex);
end;

function SDL_UnlockMutex(mutex: PSDL_mutex): Integer; calldecl;
begin
  Result := SDL_mutexV(mutex);
end;

function SDL_BUTTON( Button : Integer ) : Integer;
begin
  Result := SDL_PRESSED shl ( Button - 1 );
end;

function SDL_Swap32(D: Uint32): Uint32;
begin
  Result := ((D shl 24) or ((D shl 8) and $00FF0000) or ((D shr 8) and $0000FF00) or (D shr 24));
end;

function LoadSDL( const aPath : AnsiString = SDLDefaultPath ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL <> nil then Exit( True );
  SDL := TLibrary.Load( aPath );
  if SDL = nil then Exit( False );

// main
  Pointer(SDL_Init)           := GetSymbol('SDL_Init');
  Pointer(SDL_InitSubSystem)  := GetSymbol('SDL_InitSubSystem');
  Pointer(SDL_QuitSubSystem)  := GetSymbol('SDL_QuitSubSystem');
  Pointer(SDL_WasInit)        := GetSymbol('SDL_WasInit');
  Pointer(SDL_Quit)           := GetSymbol('SDL_Quit');
  {$IFDEF WINDOWS}
  Pointer(SDL_RegisterApp)    := GetSymbol('SDL_RegisterApp');
  {$ENDIF}

// error handling
  Pointer(SDL_GetError)       := GetSymbol('SDL_GetError');
  Pointer(SDL_SetError)       := GetSymbol('SDL_SetError');
  Pointer(SDL_ClearError)     := GetSymbol('SDL_ClearError');

// io handling
  Pointer(SDL_RWFromFile)      := GetSymbol('SDL_RWFromFile');
  Pointer(SDL_FreeRW)          := GetSymbol('SDL_FreeRW');
  Pointer(SDL_RWFromFP)        := GetSymbol('SDL_RWFromFP');
  Pointer(SDL_RWFromMem)       := GetSymbol('SDL_RWFromMem');
  Pointer(SDL_RWFromConstMem)  := GetSymbol('SDL_RWFromConstMem');
  Pointer(SDL_AllocRW)         := GetSymbol('SDL_AllocRW');

// time-handling
  Pointer(SDL_GetTicks)        := GetSymbol('SDL_GetTicks');
  Pointer(SDL_Delay)           := GetSymbol('SDL_Delay');
  Pointer(SDL_AddTimer)        := GetSymbol('SDL_AddTimer');
  Pointer(SDL_RemoveTimer)     := GetSymbol('SDL_RemoveTimer');
  Pointer(SDL_SetTimer)        := GetSymbol('SDL_SetTimer');

// audio-routines
  Pointer(SDL_AudioInit)       := GetSymbol('SDL_AudioInit');
  Pointer(SDL_AudioQuit)       := GetSymbol('SDL_AudioQuit');
  Pointer(SDL_AudioDriverName) := GetSymbol('SDL_AudioDriverName');
  Pointer(SDL_OpenAudio)       := GetSymbol('SDL_OpenAudio');
  Pointer(SDL_GetAudioStatus)  := GetSymbol('SDL_GetAudioStatus');
  Pointer(SDL_PauseAudio)      := GetSymbol('SDL_PauseAudio');
  Pointer(SDL_LoadWAV_RW)      := GetSymbol('SDL_LoadWAV_RW');
  Pointer(SDL_FreeWAV)         := GetSymbol('SDL_FreeWAV');
  Pointer(SDL_BuildAudioCVT)   := GetSymbol('SDL_BuildAudioCVT');
  Pointer(SDL_ConvertAudio)    := GetSymbol('SDL_ConvertAudio');
  Pointer(SDL_MixAudio)        := GetSymbol('SDL_MixAudio');
  Pointer(SDL_LockAudio)       := GetSymbol('SDL_LockAudio');
  Pointer(SDL_UnlockAudio)     := GetSymbol('SDL_UnlockAudio');
  Pointer(SDL_CloseAudio)      := GetSymbol('SDL_CloseAudio');

// CD-routines
  Pointer(SDL_CDNumDrives)     := GetSymbol('SDL_CDNumDrives');
  Pointer(SDL_CDName)          := GetSymbol('SDL_CDName');
  Pointer(SDL_CDOpen)          := GetSymbol('SDL_CDOpen');
  Pointer(SDL_CDStatus)        := GetSymbol('SDL_CDStatus');
  Pointer(SDL_CDPlayTracks)    := GetSymbol('SDL_CDPlayTracks');
  Pointer(SDL_CDPlay)          := GetSymbol('SDL_CDPlay');
  Pointer(SDL_CDPause)         := GetSymbol('SDL_CDPause');
  Pointer(SDL_CDResume)        := GetSymbol('SDL_CDResume');
  Pointer(SDL_CDStop)          := GetSymbol('SDL_CDStop');
  Pointer(SDL_CDEject)         := GetSymbol('SDL_CDEject');
  Pointer(SDL_CDClose)         := GetSymbol('SDL_CDClose');

// JoyStick-routines
  Pointer(SDL_NumJoysticks)       := GetSymbol('SDL_NumJoysticks');
  Pointer(SDL_JoystickName)       := GetSymbol('SDL_JoystickName');
  Pointer(SDL_JoystickOpen)       := GetSymbol('SDL_JoystickOpen');
  Pointer(SDL_JoystickOpened)     := GetSymbol('SDL_JoystickOpened');
  Pointer(SDL_JoystickIndex)      := GetSymbol('SDL_JoystickIndex');
  Pointer(SDL_JoystickNumAxes)    := GetSymbol('SDL_JoystickNumAxes');
  Pointer(SDL_JoystickNumBalls)   := GetSymbol('SDL_JoystickNumBalls');
  Pointer(SDL_JoystickNumHats)    := GetSymbol('SDL_JoystickNumHats');
  Pointer(SDL_JoystickNumButtons) := GetSymbol('SDL_JoystickNumButtons');
  Pointer(SDL_JoystickEventState) := GetSymbol('SDL_JoystickEventState');
  Pointer(SDL_JoystickGetAxis)    := GetSymbol('SDL_JoystickGetAxis');
  Pointer(SDL_JoystickGetHat)     := GetSymbol('SDL_JoystickGetHat');
  Pointer(SDL_JoystickGetBall)    := GetSymbol('SDL_JoystickGetBall');
  Pointer(SDL_JoystickGetButton)  := GetSymbol('SDL_JoystickGetButton');
  Pointer(SDL_JoystickClose)      := GetSymbol('SDL_JoystickClose');

// event-handling
  Pointer(SDL_PumpEvents)         := GetSymbol('SDL_PumpEvents');
  Pointer(SDL_PeepEvents)         := GetSymbol('SDL_PeepEvents');
  Pointer(SDL_PollEvent)          := GetSymbol('SDL_PollEvent');
  Pointer(SDL_WaitEvent)          := GetSymbol('SDL_WaitEvent');
  Pointer(SDL_PushEvent)          := GetSymbol('SDL_PushEvent');
  Pointer(SDL_SetEventFilter)     := GetSymbol('SDL_SetEventFilter');
  Pointer(SDL_GetEventFilter)     := GetSymbol('SDL_GetEventFilter');
  Pointer(SDL_EventState)         := GetSymbol('SDL_EventState');

// Version Routines
  Pointer(SDL_Linked_Version)     := GetSymbol('SDL_Linked_Version');

// video
  Pointer(SDL_VideoInit)            := GetSymbol('SDL_VideoInit');
  Pointer(SDL_VideoQuit)            := GetSymbol('SDL_VideoQuit');
  Pointer(SDL_VideoDriverName)      := GetSymbol('SDL_VideoDriverName');
  Pointer(SDL_GetVideoSurface)      := GetSymbol('SDL_GetVideoSurface');
  Pointer(SDL_GetVideoInfo)         := GetSymbol('SDL_GetVideoInfo');
  Pointer(SDL_VideoModeOK)          := GetSymbol('SDL_VideoModeOK');
  Pointer(SDL_ListModes)            := GetSymbol('SDL_ListModes');
  Pointer(SDL_SetVideoMode)         := GetSymbol('SDL_SetVideoMode');
  Pointer(SDL_UpdateRects)          := GetSymbol('SDL_UpdateRects');
  Pointer(SDL_UpdateRect)           := GetSymbol('SDL_UpdateRect');
  Pointer(SDL_Flip)                 := GetSymbol('SDL_Flip');
  Pointer(SDL_SetGamma)             := GetSymbol('SDL_SetGamma');
  Pointer(SDL_SetGammaRamp)         := GetSymbol('SDL_SetGammaRamp');
  Pointer(SDL_GetGammaRamp)         := GetSymbol('SDL_GetGammaRamp');
  Pointer(SDL_SetColors)            := GetSymbol('SDL_SetColors');
  Pointer(SDL_SetPalette)           := GetSymbol('SDL_SetPalette');
  Pointer(SDL_MapRGB)               := GetSymbol('SDL_MapRGB');
  Pointer(SDL_MapRGBA)              := GetSymbol('SDL_MapRGBA');
  Pointer(SDL_GetRGB)               := GetSymbol('SDL_GetRGB');
  Pointer(SDL_GetRGBA)              := GetSymbol('SDL_GetRGBA');
  Pointer(SDL_CreateRGBSurface)     := GetSymbol('SDL_CreateRGBSurface');
  Pointer(SDL_CreateRGBSurfaceFrom) := GetSymbol('SDL_CreateRGBSurfaceFrom');
  Pointer(SDL_FreeSurface)          := GetSymbol('SDL_FreeSurface');
  Pointer(SDL_LockSurface)          := GetSymbol('SDL_LockSurface');
  Pointer(SDL_UnlockSurface)        := GetSymbol('SDL_UnlockSurface');
  Pointer(SDL_LoadBMP_RW)           := GetSymbol('SDL_LoadBMP_RW');
  Pointer(SDL_SaveBMP_RW)           := GetSymbol('SDL_SaveBMP_RW');
  Pointer(SDL_SetColorKey)          := GetSymbol('SDL_SetColorKey');
  Pointer(SDL_SetAlpha)             := GetSymbol('SDL_SetAlpha');
  Pointer(SDL_SetClipRect)          := GetSymbol('SDL_SetClipRect');
  Pointer(SDL_GetClipRect)          := GetSymbol('SDL_GetClipRect');
  Pointer(SDL_ConvertSurface)       := GetSymbol('SDL_ConvertSurface');
  Pointer(SDL_UpperBlit)            := GetSymbol('SDL_UpperBlit');
  Pointer(SDL_LowerBlit)            := GetSymbol('SDL_LowerBlit');
  Pointer(SDL_FillRect)             := GetSymbol('SDL_FillRect');
  Pointer(SDL_DisplayFormat)        := GetSymbol('SDL_DisplayFormat');
  Pointer(SDL_DisplayFormatAlpha)   := GetSymbol('SDL_DisplayFormatAlpha');
  Pointer(SDL_CreateYUVOverlay)     := GetSymbol('SDL_CreateYUVOverlay');
  Pointer(SDL_LockYUVOverlay)       := GetSymbol('SDL_LockYUVOverlay');
  Pointer(SDL_UnlockYUVOverlay)     := GetSymbol('SDL_UnlockYUVOverlay');
  Pointer(SDL_DisplayYUVOverlay)    := GetSymbol('SDL_DisplayYUVOverlay');
  Pointer(SDL_FreeYUVOverlay)       := GetSymbol('SDL_FreeYUVOverlay');

// GL-functions
  Pointer(SDL_GL_LoadLibrary)     := GetSymbol('SDL_GL_LoadLibrary');
  Pointer(SDL_GL_GetProcAddress)  := GetSymbol('SDL_GL_GetProcAddress');
  Pointer(SDL_GL_SetAttribute)    := GetSymbol('SDL_GL_SetAttribute');
  Pointer(SDL_GL_GetAttribute)    := GetSymbol('SDL_GL_GetAttribute');
  Pointer(SDL_GL_SwapBuffers)     := GetSymbol('SDL_GL_SwapBuffers');
  Pointer(SDL_GL_UpdateRects)     := GetSymbol('SDL_GL_UpdateRects');
  Pointer(SDL_GL_Lock)            := GetSymbol('SDL_GL_Lock');
  Pointer(SDL_GL_Unlock)          := GetSymbol('SDL_GL_Unlock');

// Window manager
  Pointer(SDL_WM_GetCaption)        := GetSymbol('SDL_WM_GetCaption');
  Pointer(SDL_WM_SetCaption)        := GetSymbol('SDL_WM_SetCaption');
  Pointer(SDL_WM_SetIcon)           := GetSymbol('SDL_WM_SetIcon');
  Pointer(SDL_WM_IconifyWindow)     := GetSymbol('SDL_WM_IconifyWindow');
  Pointer(SDL_WM_ToggleFullScreen)  := GetSymbol('SDL_WM_ToggleFullScreen');
  Pointer(SDL_WM_GrabInput)         := GetSymbol('SDL_WM_GrabInput');
  Pointer(SDL_GetWMInfo)            := GetSymbol('SDL_GetWMInfo');

// Mouse-routines
  Pointer(SDL_GetMouseState)          := GetSymbol('SDL_GetMouseState');
  Pointer(SDL_GetRelativeMouseState)  := GetSymbol('SDL_GetRelativeMouseState');
  Pointer(SDL_WarpMouse)              := GetSymbol('SDL_WarpMouse');
  Pointer(SDL_CreateCursor)           := GetSymbol('SDL_CreateCursor');
  Pointer(SDL_SetCursor)              := GetSymbol('SDL_SetCursor');
  Pointer(SDL_GetCursor)              := GetSymbol('SDL_GetCursor');
  Pointer(SDL_FreeCursor)             := GetSymbol('SDL_FreeCursor');
  Pointer(SDL_ShowCursor)             := GetSymbol('SDL_ShowCursor');

// Keyboard-routines
  Pointer(SDL_EnableUNICODE)    := GetSymbol('SDL_EnableUNICODE');
  Pointer(SDL_EnableKeyRepeat)  := GetSymbol('SDL_EnableKeyRepeat');
  Pointer(SDL_GetKeyRepeat)     := GetSymbol('SDL_GetKeyRepeat');
  Pointer(SDL_GetKeyState)      := GetSymbol('SDL_GetKeyState');
  Pointer(SDL_GetModState)      := GetSymbol('SDL_GetModState');
  Pointer(SDL_SetModState)      := GetSymbol('SDL_SetModState');
  Pointer(SDL_GetKeyName)       := GetSymbol('SDL_GetKeyName');

// Active Routines
  Pointer(SDL_GetAppState)  := GetSymbol('SDL_GetAppState');

// Mutex functions
  Pointer(SDL_CreateMutex)   := GetSymbol('SDL_CreateMutex');
  Pointer(SDL_mutexP)        := GetSymbol('SDL_mutexP');
  Pointer(SDL_mutexV)        := GetSymbol('SDL_mutexV');
  Pointer(SDL_DestroyMutex)  := GetSymbol('SDL_DestroyMutex');

// Semaphore functions
  Pointer(SDL_CreateSemaphore)   := GetSymbol('SDL_CreateSemaphore');
  Pointer(SDL_DestroySemaphore)  := GetSymbol('SDL_DestroySemaphore');
  Pointer(SDL_SemWait)           := GetSymbol('SDL_SemWait');
  Pointer(SDL_SemTryWait)        := GetSymbol('SDL_SemTryWait');
  Pointer(SDL_SemWaitTimeout)    := GetSymbol('SDL_SemWaitTimeout');
  Pointer(SDL_SemPost)           := GetSymbol('SDL_SemPost');
  Pointer(SDL_SemValue)          := GetSymbol('SDL_SemValue');

// Condition variable functions
  Pointer(SDL_CreateCond)        := GetSymbol('SDL_CreateCond');
  Pointer(SDL_DestroyCond)       := GetSymbol('SDL_DestroyCond');
  Pointer(SDL_CondSignal)        := GetSymbol('SDL_CondSignal');
  Pointer(SDL_CondBroadcast)     := GetSymbol('SDL_CondBroadcast');
  Pointer(SDL_CondWait)          := GetSymbol('SDL_CondWait');
  Pointer(SDL_CondWaitTimeout)   := GetSymbol('SDL_CondWaitTimeout');

// Thread functions
  Pointer(SDL_CreateThread)  := GetSymbol('SDL_CreateThread');
  Pointer(SDL_ThreadID)      := GetSymbol('SDL_ThreadID');
  Pointer(SDL_GetThreadID)   := GetSymbol('SDL_GetThreadID');
  Pointer(SDL_WaitThread)    := GetSymbol('SDL_WaitThread');
  Pointer(SDL_KillThread)    := GetSymbol('SDL_KillThread');

  Exit( True );
end;

function RW_Stream_Seek( context: PSDL_RWops; offset: Integer; whence: Integer ) : Integer; cdecl;
var Stream  : TStream;
    SOffset : PtrUInt;
    SSize   : PtrUInt;
begin
  SOffset := PtrUInt(context^.mem.base);
  Stream  := TStream(context^.mem.here);
  SSize   := PtrUInt(context^.mem.stop);

  case whence of
    0 : Stream.Seek( SOffset+offset, soBeginning );
    1 : Stream.Seek( offset, soCurrent );
    2 : Stream.Seek( SOffset+SSize+offset, soCurrent );
  end;
  Exit( Stream.Position-SOffset );
end;

function RW_Stream_Read( context: PSDL_RWops; Ptr: Pointer; size: Integer; maxnum : Integer ): Integer; cdecl;
var Stream : TStream;
begin
  Stream := TStream(context^.mem.here);
  Exit( Stream.Read( Ptr^, Size * maxnum ) div Size );
end;

function RW_Stream_Write( context: PSDL_RWops; Ptr: Pointer; size: Integer; num: Integer ): Integer; cdecl;
var Stream : TStream;
begin
  Stream := TStream(context^.mem.here);
  Exit( Stream.Write( Ptr^, Size * num ) div Size );
end;

function RW_Stream_Close( context: PSDL_RWops ): Integer; cdecl;
var Stream : TStream;
begin
  if Context <> nil then
  begin
    Stream := TStream(context^.mem.here);
    FreeAndNil( Stream );
    SDL_FreeRW( context );
  end;
  Exit( 0 );
end;

function SDL_RWopsFromStream( Stream : TStream; Size : DWord ) : PSDL_RWops;
begin
  SDL_RWopsFromStream := SDL_AllocRW();
  if SDL_RWopsFromStream <> nil then
  begin
    SDL_RWopsFromStream^.seek := @RW_Stream_Seek;
    SDL_RWopsFromStream^.read := @RW_Stream_Read;
    SDL_RWopsFromStream^.write := @RW_Stream_Write;
    SDL_RWopsFromStream^.close := @RW_Stream_Close;
    SDL_RWopsFromStream^.mem.base := PUInt8( Stream.Position );
    SDL_RWopsFromStream^.mem.here := PUInt8( Stream );
    SDL_RWopsFromStream^.mem.stop := PUInt8( Size );
  end;
end;


finalization
  if SDL <> nil then FreeAndNil( SDL );

end.

