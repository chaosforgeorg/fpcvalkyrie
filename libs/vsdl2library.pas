unit vsdl2library;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary,
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
  SDL2DefaultPath = 'SDL2.dll';
{$ELSE}
  {$IFDEF DARWIN}
    SDL2DefaultPath = 'SDL2.framework/SDL2';
    {$linklib SDLmain}
    {$linkframework Cocoa}
    {$linkframework SDL}
    {$PASCALMAINNAME SDL_main}
  {$ELSE}
  SDL2DefaultPath = 'libSDL2-2.0.so.0';
  {$ENDIF}
{$ENDIF}

const

  SDL2_MAJOR_VERSION = 2;
  SDL2_MINOR_VERSION = 0;
  SDL2_PATCHLEVEL    = 28;

{$include vsdl2const.inc}
{$include vsdl2types.inc}

var
  SDL_Init          : function(flags: Uint32): Integer; cdecl;
  SDL_InitSubSystem : function(flags: Uint32): Integer; cdecl;
  SDL_QuitSubSystem : procedure(flags: Uint32); cdecl;
  SDL_WasInit       : function(flags: Uint32): Uint32; cdecl;
  SDL_Quit          : procedure; cdecl;

var
  SDL_RWFromFile       : function(const file_: PChar; const mode: PChar): PSDL_RWops; cdecl;
  SDL_RWFromFP         : function(fp: Pointer; autoclose: SDL_bool): PSDL_RWops; cdecl; // FILE* is translated to Pointer
  SDL_RWFromMem        : function(mem: Pointer; size: Integer): PSDL_RWops; cdecl;
  SDL_RWFromConstMem   : function(const mem: Pointer; size: Integer): PSDL_RWops; cdecl;
  SDL_AllocRW          : function: PSDL_RWops; cdecl;
  SDL_FreeRW           : procedure(area: PSDL_RWops); cdecl;
  SDL_ReadLE16         : function(src: PSDL_RWops): Uint16; cdecl;
  SDL_ReadBE16         : function(src: PSDL_RWops): Uint16; cdecl;
  SDL_ReadLE32         : function(src: PSDL_RWops): Uint32; cdecl;
  SDL_ReadBE32         : function(src: PSDL_RWops): Uint32; cdecl;
  SDL_ReadLE64         : function(src: PSDL_RWops): Uint64; cdecl;
  SDL_ReadBE64         : function(src: PSDL_RWops): Uint64; cdecl;
  SDL_WriteLE16        : function(dst: PSDL_RWops; value: Uint16): Integer; cdecl;
  SDL_WriteBE16        : function(dst: PSDL_RWops; value: Uint16): Integer; cdecl;
  SDL_WriteLE32        : function(dst: PSDL_RWops; value: Uint32): Integer; cdecl;
  SDL_WriteBE32        : function(dst: PSDL_RWops; value: Uint32): Integer; cdecl;
  SDL_WriteLE64        : function(dst: PSDL_RWops; value: Uint64): Integer; cdecl;
  SDL_WriteBE64        : function(dst: PSDL_RWops; value: Uint64): Integer; cdecl;

var
  SDL_AudioInit          : function( const driver_name: PChar ): Integer; cdecl;
  SDL_AudioQuit          : procedure; cdecl;
  SDL_OpenAudio          : function( desired, obtained: PSDL_AudioSpec): Integer; cdecl;
  SDL_GetAudioStatus     : function: Integer; cdecl;
  SDL_PauseAudio         : procedure(pause_on: Integer); cdecl;
  SDL_LoadWAV_RW         : function(src: PSDL_RWops; freesrc: Integer; spec: PSDL_AudioSpec; audio_buf: PPUInt8; audio_len: PUint32): PSDL_AudioSpec; cdecl;
  SDL_FreeWAV            : procedure(audio_buf: PUInt8); cdecl;
  SDL_BuildAudioCVT      : function( cvt: PSDL_AudioCVT; src_format: SDL_AudioFormat; src_channels: Uint8; src_rate: Integer; dst_format: SDL_AudioFormat; dst_channels: Uint8; dst_rate: Integer): Integer; cdecl;
  SDL_ConvertAudio       : function( cvt: PSDL_AudioCVT): Integer; cdecl;
  SDL_NewAudioStream     : function(src_format: SDL_AudioFormat; src_channels: Uint8; src_rate: Integer; dst_format: SDL_AudioFormat; dst_channels: Uint8; dst_rate: Integer): PSDL_AudioStream; cdecl;
  SDL_AudioStreamPut     : function(stream: PSDL_AudioStream; const buf: Pointer; len: Integer): Integer; cdecl;
  SDL_AudioStreamGet     : function(stream: PSDL_AudioStream; buf: Pointer; len: Integer): Integer; cdecl;
  SDL_AudioStreamAvailable: function(stream: PSDL_AudioStream): Integer; cdecl;
  SDL_AudioStreamFlush   : function(stream: PSDL_AudioStream): Integer; cdecl;
  SDL_AudioStreamClear   : procedure(stream: PSDL_AudioStream); cdecl;
  SDL_FreeAudioStream    : procedure(stream: PSDL_AudioStream); cdecl;
  SDL_MixAudio           : procedure(dst: PUInt8; const src: PUInt8; len: Uint32; volume: Integer); cdecl;
  SDL_LockAudio          : procedure; cdecl;
  SDL_UnlockAudio        : procedure; cdecl;
  SDL_CloseAudio         : procedure; cdecl;

// SDL CPU info functions
var
  SDL_HasRDTSC          : function: SDL_bool; cdecl;
  SDL_HasMMX            : function: SDL_bool; cdecl;
  SDL_Has3DNow          : function: SDL_bool; cdecl;
  SDL_HasSSE            : function: SDL_bool; cdecl;
  SDL_HasSSE2           : function: SDL_bool; cdecl;
  SDL_HasAltiVec        : function: SDL_bool; cdecl;

// SDL mouse functions
  SDL_SetCursor         : procedure(cursor: PSDL_Cursor); cdecl;
  SDL_GetCursor         : function: PSDL_Cursor; cdecl;
  SDL_FreeCursor        : procedure(cursor: PSDL_Cursor); cdecl;
  SDL_ShowCursor        : function(toggle: Integer): Integer; cdecl;
  SDL_GetMouseFocus     : function: PSDL_Window; cdecl;
  SDL_GetMouseState     : function(x, y: PInteger): Uint32; cdecl;
  SDL_GetGlobalMouseState: function(x, y: PInteger): Uint32; cdecl;
  SDL_WarpMouseGlobal   : function(x, y: Integer): Integer; cdecl;
  SDL_GetRelativeMouseState: function(x, y: PInteger): Uint32; cdecl;
  SDL_WarpMouseInWindow : procedure(window: PSDL_Window; x, y: Integer); cdecl;
  SDL_SetRelativeMouseMode: function(enabled: SDL_bool): Integer; cdecl;
  SDL_CaptureMouse      : function(enabled: SDL_bool): Integer; cdecl;
  SDL_GetRelativeMouseMode: function: SDL_bool; cdecl;
  SDL_CreateCursor      : function(const data, mask: PUint8; w, h, hot_x, hot_y: Integer): PSDL_Cursor; cdecl;
  SDL_CreateColorCursor : function(surface: PSDL_Surface; hot_x, hot_y: Integer): PSDL_Cursor; cdecl;
  SDL_CreateSystemCursor: function( id: Integer ): PSDL_Cursor; cdecl;
  SDL_GetDefaultCursor  : function: PSDL_Cursor; cdecl;

// SDL joystick functions
var
  SDL_LockJoysticks                 : procedure; cdecl;
  SDL_UnlockJoysticks               : procedure; cdecl;
  SDL_NumJoysticks                  : function: Integer; cdecl;
  SDL_JoystickNameForIndex          : function(device_index: Integer): PChar; cdecl;
  SDL_JoystickGetDeviceGUID         : function(device_index: Integer): SDL_JoystickGUID; cdecl;
  SDL_JoystickGetDeviceVendor       : function(device_index: Integer): Uint16; cdecl;
  SDL_JoystickGetDeviceProduct      : function(device_index: Integer): Uint16; cdecl;
  SDL_JoystickGetDeviceProductVersion: function(device_index: Integer): Uint16; cdecl;
  SDL_JoystickGetDeviceType         : function(device_index: Integer): SDL_JoystickType; cdecl;
  SDL_JoystickGetDeviceInstanceID   : function(device_index: Integer): SDL_JoystickID; cdecl;
  SDL_JoystickOpen                  : function(device_index: Integer): PSDL_Joystick; cdecl;
  SDL_JoystickFromInstanceID        : function(joyid: SDL_JoystickID): PSDL_Joystick; cdecl;
  SDL_JoystickName                  : function(joystick: PSDL_Joystick): PChar; cdecl;
  SDL_JoystickGetGUID               : function(joystick: PSDL_Joystick): SDL_JoystickGUID; cdecl;
  SDL_JoystickGetVendor             : function(joystick: PSDL_Joystick): Uint16; cdecl;
  SDL_JoystickGetProduct            : function(joystick: PSDL_Joystick): Uint16; cdecl;
  SDL_JoystickGetProductVersion     : function(joystick: PSDL_Joystick): Uint16; cdecl;
  SDL_JoystickGetType               : function(joystick: PSDL_Joystick): SDL_JoystickType; cdecl;
  SDL_JoystickGetGUIDString         : procedure(guid: SDL_JoystickGUID; pszGUID: PChar; cbGUID: Integer); cdecl;
  SDL_JoystickGetGUIDFromString     : function(const pchGUID: PChar): SDL_JoystickGUID; cdecl;
  SDL_JoystickGetAttached           : function(joystick: PSDL_Joystick): SDL_bool; cdecl;
  SDL_JoystickInstanceID            : function(joystick: PSDL_Joystick): SDL_JoystickID; cdecl;
  SDL_JoystickNumAxes               : function(joystick: PSDL_Joystick): Integer; cdecl;
  SDL_JoystickNumBalls              : function(joystick: PSDL_Joystick): Integer; cdecl;
  SDL_JoystickNumHats               : function(joystick: PSDL_Joystick): Integer; cdecl;
  SDL_JoystickNumButtons            : function(joystick: PSDL_Joystick): Integer; cdecl;
  SDL_JoystickUpdate                : procedure; cdecl;
  SDL_JoystickEventState            : function(state: Integer): Integer; cdecl;
  SDL_JoystickGetAxis               : function(joystick: PSDL_Joystick; axis: Integer): Sint16; cdecl;
  SDL_JoystickGetAxisInitialState   : function(joystick: PSDL_Joystick; axis: Integer; state: PSint16): SDL_bool; cdecl;
  SDL_JoystickGetHat                : function(joystick: PSDL_Joystick; hat: Integer): Uint8; cdecl;
  SDL_JoystickGetBall               : function(joystick: PSDL_Joystick; ball: Integer; dx, dy: PInteger): Integer; cdecl;
  SDL_JoystickGetButton             : function(joystick: PSDL_Joystick; button: Integer): Uint8; cdecl;
  SDL_JoystickClose                 : procedure(joystick: PSDL_Joystick); cdecl;
  SDL_JoystickCurrentPowerLevel     : function(joystick: PSDL_Joystick): SDL_JoystickPowerLevel; cdecl;

// SDL timer functions
  SDL_GetTicks                      : function: Uint32; cdecl;
  SDL_Delay                         : procedure(ms: Uint32); cdecl;
  SDL_AddTimer                      : function(interval: Uint32; callback: SDL_TimerCallback; param: Pointer): SDL_TimerID; cdecl;
  SDL_RemoveTimer                   : function(t: SDL_TimerID): SDL_bool; cdecl;

// SDL event functions
  SDL_PumpEvents                    : procedure; cdecl;
  SDL_PollEvent                     : function(event: PSDL_Event): Integer; cdecl;
  SDL_WaitEvent                     : function(event: PSDL_Event): Integer; cdecl;
  SDL_PushEvent                     : function(event: PSDL_Event): Integer; cdecl;

// SDL video/surface functions
var
  SDL_CreateRGBSurface       : function(flags: Uint32; width, height, depth: Integer; Rmask, Gmask, Bmask, Amask: Uint32): PSDL_Surface; cdecl;
  SDL_CreateRGBSurfaceFrom   : function(pixels: Pointer; width, height, depth, pitch: Integer; Rmask, Gmask, Bmask, Amask: Uint32): PSDL_Surface; cdecl;
  SDL_FreeSurface            : procedure(surface: PSDL_Surface); cdecl;
  SDL_LockSurface            : function(surface: PSDL_Surface): Integer; cdecl;
  SDL_UnlockSurface          : procedure(surface: PSDL_Surface); cdecl;
  SDL_SaveBMP_RW             : function(surface: PSDL_Surface; dst: PSDL_RWops; freedst: Integer): Integer; cdecl;
  SDL_LoadBMP_RW             : function(src: PSDL_RWops; freesrc: Integer): PSDL_Surface; cdecl;
  SDL_SaveBMP                : function(surface: PSDL_Surface; const file_: PChar ): Integer; cdecl;
  SDL_LoadBMP                : function(const file_: PChar): PSDL_Surface; cdecl;
  SDL_SetClipRect            : function(surface: PSDL_Surface; const rect: PSDL_Rect): SDL_bool; cdecl;
  SDL_GetClipRect            : procedure(surface: PSDL_Surface; rect: PSDL_Rect); cdecl;
  SDL_ConvertSurface         : function(src: PSDL_Surface; fmt: PSDL_PixelFormat; flags: Uint32): PSDL_Surface; cdecl;

// SDL OpenGL functions
  SDL_GL_LoadLibrary         : function(const path: PChar): Integer; cdecl;
  SDL_GL_GetProcAddress      : function(const proc: PChar): Pointer; cdecl;
  SDL_GL_SetAttribute        : function(attr: SDL_GLattr; value: Integer): Integer; cdecl;
  SDL_GL_GetAttribute        : function(attr: SDL_GLattr; value: PInteger): Integer; cdecl;

// More SDL video functions
  SDL_GetNumVideoDrivers     : function: Integer; cdecl;
  SDL_GetVideoDriver         : function(index: Integer): PChar; cdecl;
  SDL_VideoInit              : function(const driver_name: PChar): Integer; cdecl;
  SDL_VideoQuit              : procedure; cdecl;
  SDL_GetCurrentVideoDriver  : function: PChar; cdecl;
  SDL_GetNumVideoDisplays    : function: Integer; cdecl;
  SDL_GetDisplayName         : function(displayIndex: Integer): PChar; cdecl;
  SDL_GetDisplayBounds       : function(displayIndex: Integer; rect: PSDL_Rect): Integer; cdecl;
  SDL_GetDisplayDPI          : function(displayIndex: Integer; ddpi, hdpi, vdpi: PSingle): Integer; cdecl;
  SDL_GetDisplayUsableBounds : function(displayIndex: Integer; rect: PSDL_Rect): Integer; cdecl;
  SDL_GetNumDisplayModes     : function(displayIndex: Integer): Integer; cdecl;
  SDL_GetDisplayMode         : function(displayIndex, modeIndex: Integer; mode: PSDL_DisplayMode): Integer; cdecl;
  SDL_GetDesktopDisplayMode  : function(displayIndex: Integer; mode: PSDL_DisplayMode): Integer; cdecl;
  SDL_GetCurrentDisplayMode  : function(displayIndex: Integer; mode: PSDL_DisplayMode): Integer; cdecl;
  SDL_GetClosestDisplayMode  : function(displayIndex: Integer; const mode: PSDL_DisplayMode; closest: PSDL_DisplayMode): PSDL_DisplayMode; cdecl;
  SDL_GetWindowDisplayIndex  : function(window: PSDL_Window): Integer; cdecl;
  SDL_SetWindowDisplayMode   : function(window: PSDL_Window; const mode: PSDL_DisplayMode): Integer; cdecl;
  SDL_GetWindowDisplayMode   : function(window: PSDL_Window; mode: PSDL_DisplayMode): Integer; cdecl;
  SDL_GetWindowPixelFormat   : function(window: PSDL_Window): Uint32; cdecl;
  SDL_CreateWindow           : function(const title: PChar; x, y, w, h: Integer; flags: Uint32): PSDL_Window; cdecl;
  SDL_CreateWindowFrom       : function(const data: Pointer): PSDL_Window; cdecl;
  SDL_GetWindowID            : function(window: PSDL_Window): Uint32; cdecl;
  SDL_GetWindowFromID        : function(id: Uint32): PSDL_Window; cdecl;
  SDL_GetWindowFlags         : function(window: PSDL_Window): Uint32; cdecl;
  SDL_SetWindowTitle         : procedure(window: PSDL_Window; const title: PChar); cdecl;
  SDL_GetWindowTitle         : function(window: PSDL_Window): PChar; cdecl;
  SDL_SetWindowIcon          : procedure(window: PSDL_Window; icon: PSDL_Surface); cdecl;
  SDL_SetWindowData          : function(window: PSDL_Window; const name: PChar; userdata: Pointer): Pointer; cdecl;
  SDL_GetWindowData          : function(window: PSDL_Window; const name: PChar): Pointer; cdecl;
  SDL_SetWindowPosition      : procedure(window: PSDL_Window; x, y: Integer); cdecl;
  SDL_GetWindowPosition      : procedure(window: PSDL_Window; x, y: PInteger); cdecl;
  SDL_SetWindowSize          : procedure(window: PSDL_Window; w, h: Integer); cdecl;
  SDL_GetWindowSize          : procedure(window: PSDL_Window; w, h: PInteger); cdecl;
  SDL_GetWindowBordersSize   : procedure(window: PSDL_Window; top, left, bottom, right: PInteger); cdecl;
  SDL_SetWindowMinimumSize   : procedure(window: PSDL_Window; min_w, min_h: Integer); cdecl;
  SDL_GetWindowMinimumSize   : procedure(window: PSDL_Window; w, h: PInteger); cdecl;
  SDL_SetWindowMaximumSize   : procedure(window: PSDL_Window; max_w, max_h: Integer); cdecl;
  SDL_GetWindowMaximumSize   : procedure(window: PSDL_Window; w, h: PInteger); cdecl;
  SDL_SetWindowBordered      : procedure(window: PSDL_Window; bordered: SDL_bool); cdecl;
  SDL_SetWindowResizable     : procedure(window: PSDL_Window; resizable: SDL_bool); cdecl;
  SDL_ShowWindow             : procedure(window: PSDL_Window); cdecl;
  SDL_HideWindow             : procedure(window: PSDL_Window); cdecl;
  SDL_RaiseWindow            : procedure(window: PSDL_Window); cdecl;
  SDL_MaximizeWindow         : procedure(window: PSDL_Window); cdecl;
  SDL_MinimizeWindow         : procedure(window: PSDL_Window); cdecl;
  SDL_RestoreWindow          : procedure(window: PSDL_Window); cdecl;
  SDL_SetWindowFullscreen    : function(window: PSDL_Window; flags: Uint32): Integer; cdecl;
  SDL_GetWindowSurface       : function(window: PSDL_Window): PSDL_Surface; cdecl;
  SDL_UpdateWindowSurface    : function(window: PSDL_Window): Integer; cdecl;
  SDL_UpdateWindowSurfaceRects: function(window: PSDL_Window; const rects: PSDL_Rect; numrects: Integer): Integer; cdecl;
  SDL_SetWindowGrab          : procedure(window: PSDL_Window; grabbed: SDL_bool); cdecl;
  SDL_GetWindowGrab          : function(window: PSDL_Window): SDL_bool; cdecl;
  SDL_GetGrabbedWindow       : function: PSDL_Window; cdecl;
  SDL_SetWindowBrightness    : function(window: PSDL_Window; brightness: Single): Integer; cdecl;
  SDL_GetWindowBrightness    : function(window: PSDL_Window): Single; cdecl;
  SDL_SetWindowOpacity       : function(window: PSDL_Window; opacity: Single): Integer; cdecl;
  SDL_GetWindowOpacity       : function(window: PSDL_Window; out_opacity: PSingle): Integer; cdecl;
  SDL_SetWindowModalFor      : function(window: PSDL_Window; parent_window: PSDL_Window): Integer; cdecl;
  SDL_SetWindowInputFocus    : function(window: PSDL_Window): Integer; cdecl;
  SDL_SetWindowGammaRamp     : function(window: PSDL_Window; const red, green, blue: PUint16): Integer; cdecl;
  SDL_GetWindowGammaRamp     : function(window: PSDL_Window; red, green, blue: PUint16): Integer; cdecl;
  SDL_DestroyWindow          : procedure(window: PSDL_Window); cdecl;
  SDL_IsScreenSaverEnabled   : function: SDL_bool; cdecl;
  SDL_EnableScreenSaver      : procedure; cdecl;
  SDL_DisableScreenSaver     : procedure; cdecl;
  SDL_GL_UnloadLibrary       : procedure; cdecl;
  SDL_GL_ExtensionSupported  : function(const extension: PChar): SDL_bool; cdecl;
  SDL_GL_ResetAttributes     : procedure; cdecl;
  SDL_GL_CreateContext       : function(window: PSDL_Window): SDL_GLContext; cdecl;
  SDL_GL_MakeCurrent         : function(window: PSDL_Window; context: SDL_GLContext): Integer; cdecl;
  SDL_GL_GetCurrentWindow    : function: PSDL_Window; cdecl;
  SDL_GL_GetCurrentContext   : function: SDL_GLContext; cdecl;
  SDL_GL_GetDrawableSize     : procedure(window: PSDL_Window; w, h: PInteger); cdecl;
  SDL_GL_SetSwapInterval     : function(interval: Integer): Integer; cdecl;
  SDL_GL_GetSwapInterval     : function: Integer; cdecl;
  SDL_GL_SwapWindow          : procedure(window: PSDL_Window); cdecl;
  SDL_GL_DeleteContext       : procedure(context: SDL_GLContext); cdecl;

// SDL Vulkan functions
var
  SDL_Vulkan_LoadLibrary             : function(const path: PChar): Integer; cdecl;
  SDL_Vulkan_GetVkGetInstanceProcAddr: function: Pointer; cdecl;
  SDL_Vulkan_UnloadLibrary           : procedure; cdecl;
  SDL_Vulkan_GetInstanceExtensions   : function(window: PSDL_Window; pCount: PDWord; pNames: PPChar): SDL_bool; cdecl;
  SDL_Vulkan_CreateSurface           : function(window: PSDL_Window; instance: PVkInstance; surface: PPVkSurfaceKHR): SDL_bool; cdecl;
  SDL_Vulkan_GetDrawableSize         : procedure(window: PSDL_Window; w, h: PInteger); cdecl;

// SDL audio functions
  SDL_GetNumAudioDrivers             : function: Integer; cdecl;
  SDL_GetAudioDriver                 : function(index: Integer): PChar; cdecl;
  SDL_GetCurrentAudioDriver          : function: PChar; cdecl;
  SDL_GetNumAudioDevices             : function(iscapture: Integer): Integer; cdecl;
  SDL_GetAudioDeviceName             : function(index, iscapture: Integer): PChar; cdecl;
  SDL_OpenAudioDevice                : function(const device: PChar; iscapture: Integer; const desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec; allowed_changes: Integer): SDL_AudioDeviceID; cdecl;
  SDL_GetAudioDeviceStatus           : function(dev: SDL_AudioDeviceID): SDL_AudioStatus; cdecl;
  SDL_PauseAudioDevice               : procedure(dev: SDL_AudioDeviceID; pause_on: Integer); cdecl;
  SDL_MixAudioFormat                 : procedure(dst: PUint8; const src: PUint8; format: SDL_AudioFormat; len: Uint32; volume: Integer); cdecl;
  SDL_QueueAudio                     : function(dev: SDL_AudioDeviceID; const data: Pointer; len: Uint32): Integer; cdecl;
  SDL_DequeueAudio                   : function(dev: SDL_AudioDeviceID; data: Pointer; len: Uint32): Uint32; cdecl;
  SDL_GetQueuedAudioSize             : function(dev: SDL_AudioDeviceID): Uint32; cdecl;
  SDL_ClearQueuedAudio               : procedure(dev: SDL_AudioDeviceID); cdecl;
  SDL_LockAudioDevice                : procedure(dev: SDL_AudioDeviceID); cdecl;
  SDL_UnlockAudioDevice              : procedure(dev: SDL_AudioDeviceID); cdecl;
  SDL_CloseAudioDevice               : procedure(dev: SDL_AudioDeviceID); cdecl;

// SDL CPU info functions
  SDL_GetCPUCount                    : function: Integer; cdecl;
  SDL_GetCPUCacheLineSize            : function: Integer; cdecl;
  SDL_HasSSE3                        : function: SDL_bool; cdecl;
  SDL_HasSSE41                       : function: SDL_bool; cdecl;
  SDL_HasSSE42                       : function: SDL_bool; cdecl;
  SDL_HasAVX                         : function: SDL_bool; cdecl;
  SDL_HasAVX2                        : function: SDL_bool; cdecl;
  SDL_HasNEON                        : function: SDL_bool; cdecl;
  SDL_GetSystemRAM                   : function: Integer; cdecl;

// SDL error functions
  SDL_SetError                       : function(const fmt: PChar): Integer; cdecl;
  SDL_Error                          : function(code: SDL_errorcode): Integer; cdecl;
  SDL_GetError                       : function: PChar; cdecl;
  SDL_ClearError                     : procedure; cdecl;

// SDL keyboard functions
  SDL_GetKeyboardFocus               : function: PSDL_Window; cdecl;
  SDL_GetKeyboardState               : function(numkeys: PInteger): PUint8; cdecl;
  SDL_GetModState                    : function: SDL_Keymod; cdecl;
  SDL_SetModState                    : procedure(modstate: SDL_Keymod); cdecl;
  SDL_GetKeyFromScancode             : function(scancode: SDL_Scancode): SDL_Keycode; cdecl;
  SDL_GetScancodeFromKey             : function(key: SDL_Keycode): SDL_Scancode; cdecl;
  SDL_GetScancodeName                : function(scancode: SDL_Scancode): PChar; cdecl;
  SDL_GetScancodeFromName            : function(const name: PChar): SDL_Scancode; cdecl;
  SDL_GetKeyName                     : function(key: SDL_Keycode): PChar; cdecl;
  SDL_GetKeyFromName                 : function(const name: PChar): SDL_Keycode; cdecl;
  SDL_StartTextInput                 : procedure; cdecl;
  SDL_IsTextInputActive              : function: SDL_bool; cdecl;
  SDL_StopTextInput                  : procedure; cdecl;
  SDL_SetTextInputRect               : procedure(rect: PSDL_Rect); cdecl;
  SDL_HasScreenKeyboardSupport       : function: SDL_bool; cdecl;
  SDL_IsScreenKeyboardShown          : function(window: PSDL_Window): SDL_bool; cdecl;

// SDL performance functions
  SDL_GetPerformanceCounter          : function: Uint64; cdecl;
  SDL_GetPerformanceFrequency        : function: Uint64; cdecl;

// SDL version functions
  SDL_GetVersion                     : procedure(ver: PSDL_version); cdecl;
  SDL_GetRevision                    : function: PChar; cdecl;
  SDL_GetRevisionNumber              : function: Integer; cdecl;

// SDL event functions
var
  SDL_PeepEvents                   : function(events: PSDL_Event; numevents: Integer; action: SDL_eventaction; minType, maxType: Uint32): Integer; cdecl;
  SDL_HasEvent                     : function(&type: Uint32): SDL_bool; cdecl;
  SDL_HasEvents                    : function(minType, maxType: Uint32): SDL_bool; cdecl;
  SDL_FlushEvent                   : procedure(&type: Uint32); cdecl;
  SDL_FlushEvents                  : procedure(minType, maxType: Uint32); cdecl;
  SDL_WaitEventTimeout             : function(event: PSDL_Event; timeout: Integer): Integer; cdecl;
  SDL_SetEventFilter               : procedure(filter: SDL_EventFilter; userdata: Pointer); cdecl;
  SDL_GetEventFilter               : function(filter: PSDL_EventFilter; userdata: PPointer): SDL_bool; cdecl;
  SDL_AddEventWatch                : procedure(filter: SDL_EventFilter; userdata: Pointer); cdecl;
  SDL_DelEventWatch                : procedure(filter: SDL_EventFilter; userdata: Pointer); cdecl;
  SDL_FilterEvents                 : procedure(filter: SDL_EventFilter; userdata: Pointer); cdecl;
  SDL_EventState                   : function(&type: Uint32; state: Integer): Uint8; cdecl;
  SDL_RegisterEvents               : function(numevents: Integer): Uint32; cdecl;

// SDL clipboard functions
  SDL_SetClipboardText             : function(const text: PChar): Integer; cdecl;
  SDL_GetClipboardText             : function: PChar; cdecl;
  SDL_HasClipboardText             : function: SDL_bool; cdecl;

// SDL game controller functions
  SDL_GameControllerAddMappingsFromRW: function(rw: PSDL_RWops; freerw: Integer): Integer; cdecl;
  SDL_GameControllerAddMapping     : function(const mappingString: PChar): Integer; cdecl;
  SDL_GameControllerNumMappings    : function: Integer; cdecl;
  SDL_GameControllerMappingForIndex: function(mapping_index: Integer): PChar; cdecl;
  SDL_GameControllerMappingForGUID : function(guid: SDL_JoystickGUID): PChar; cdecl;
  SDL_GameControllerMapping        : function(gamecontroller: PSDL_GameController): PChar; cdecl;
  SDL_IsGameController             : function(joystick_index: Integer): SDL_bool; cdecl;
  SDL_GameControllerNameForIndex   : function(joystick_index: Integer): PChar; cdecl;
  SDL_GameControllerOpen           : function(joystick_index: Integer): PSDL_GameController; cdecl;
  SDL_GameControllerFromInstanceID : function(joyid: SDL_JoystickID): PSDL_GameController; cdecl;
  SDL_GameControllerName           : function(gamecontroller: PSDL_GameController): PChar; cdecl;
  SDL_GameControllerGetVendor      : function(gamecontroller: PSDL_GameController): Uint16; cdecl;
  SDL_GameControllerGetProduct     : function(gamecontroller: PSDL_GameController): Uint16; cdecl;
  SDL_GameControllerGetProductVersion: function(gamecontroller: PSDL_GameController): Uint16; cdecl;
  SDL_GameControllerGetAttached    : function(gamecontroller: PSDL_GameController): SDL_bool; cdecl;
  SDL_GameControllerGetJoystick    : function(gamecontroller: PSDL_GameController): PSDL_Joystick; cdecl;
  SDL_GameControllerEventState     : function(state: Integer): Integer; cdecl;
  SDL_GameControllerUpdate         : procedure; cdecl;
  SDL_GameControllerGetAxisFromString: function(const pchString: PChar): SDL_GameControllerAxis; cdecl;
  SDL_GameControllerGetStringForAxis: function(axis: SDL_GameControllerAxis): PChar; cdecl;
  SDL_GameControllerGetBindForAxis : function(gamecontroller: PSDL_GameController; axis: SDL_GameControllerAxis): SDL_GameControllerButtonBind; cdecl;
  SDL_GameControllerGetAxis        : function(gamecontroller: PSDL_GameController; axis: SDL_GameControllerAxis): Sint16; cdecl;
  SDL_GameControllerGetButtonFromString: function(const pchString: PChar): SDL_GameControllerButton; cdecl;
  SDL_GameControllerGetStringForButton: function(button: SDL_GameControllerButton): PChar; cdecl;
  SDL_GameControllerGetBindForButton: function(gamecontroller: PSDL_GameController; button: SDL_GameControllerButton): SDL_GameControllerButtonBind; cdecl;
  SDL_GameControllerGetButton      : function(gamecontroller: PSDL_GameController; button: SDL_GameControllerButton): Uint8; cdecl;
  SDL_GameControllerClose          : procedure(gamecontroller: PSDL_GameController); cdecl;

// SDL hints functions
  SDL_SetHintWithPriority          : function(const name, value: PChar; priority: SDL_HintPriority): SDL_bool; cdecl;
  SDL_SetHint                      : function(const name, value: PChar): SDL_bool; cdecl;
  SDL_GetHint                      : function(const name: PChar): PChar; cdecl;
  SDL_GetHintBoolean               : function(const name: PChar; default_value: SDL_bool): SDL_bool; cdecl;
  SDL_AddHintCallback              : procedure(const name: PChar; callback: SDL_HintCallback; userdata: Pointer); cdecl;
  SDL_DelHintCallback              : procedure(const name: PChar; callback: SDL_HintCallback; userdata: Pointer); cdecl;
  SDL_ClearHints                   : procedure; cdecl;

// SDL main functions
  SDL_SetMainReady                 : procedure; cdecl;

// SDL message box functions
  SDL_ShowMessageBox               : function(const messageboxdata: PSDL_MessageBoxData; buttonid: PInteger): Integer; cdecl;
  SDL_ShowSimpleMessageBox         : function(flags: Uint32; const title, message: PChar; window: PSDL_Window): Integer; cdecl;

// SDL pixels functions
  SDL_GetPixelFormatName           : function(format: Uint32): PChar; cdecl;
  SDL_PixelFormatEnumToMasks       : function(format: Uint32; bpp: PInteger; Rmask, Gmask, Bmask, Amask: PUint32): SDL_bool; cdecl;
  SDL_MasksToPixelFormatEnum       : function(bpp: Integer; Rmask, Gmask, Bmask, Amask: Uint32): Uint32; cdecl;
  SDL_AllocFormat                  : function(pixel_format: Uint32): PSDL_PixelFormat; cdecl;
  SDL_FreeFormat                   : procedure(format: PSDL_PixelFormat); cdecl;
  SDL_AllocPalette                 : function(ncolors: Integer): PSDL_Palette; cdecl;
  SDL_SetPixelFormatPalette        : function(format: PSDL_PixelFormat; palette: PSDL_Palette): Integer; cdecl;
  SDL_SetPaletteColors             : function(palette: PSDL_Palette; const colors: PSDL_Color; firstcolor, ncolors: Integer): Integer; cdecl;
  SDL_FreePalette                  : procedure(palette: PSDL_Palette); cdecl;
  SDL_MapRGB                       : function(const format: PSDL_PixelFormat; r, g, b: Uint8): Uint32; cdecl;
  SDL_MapRGBA                      : function(const format: PSDL_PixelFormat; r, g, b, a: Uint8): Uint32; cdecl;
  SDL_GetRGB                       : procedure(pixel: Uint32; const format: PSDL_PixelFormat; r, g, b: PUint8); cdecl;
  SDL_GetRGBA                      : procedure(pixel: Uint32; const format: PSDL_PixelFormat; r, g, b, a: PUint8); cdecl;
  SDL_CalculateGammaRamp           : procedure(gamma: Single; ramp: PUint16); cdecl;

// SDL platform functions
  SDL_GetPlatform                  : function: PChar; cdecl;

// SDL power functions
  SDL_GetPowerInfo                 : function(secs, pct: PInteger): SDL_PowerState; cdecl;

// SDL renderer functions
var
  SDL_GetNumRenderDrivers          : function: Integer; cdecl;
  SDL_GetRenderDriverInfo          : function(index: Integer; info: PSDL_RendererInfo): Integer; cdecl;
  SDL_CreateWindowAndRenderer      : function(width, height: Integer; window_flags: Uint32; window: PPSDL_Window; renderer: PPSDL_Renderer): Integer; cdecl;
  SDL_CreateRenderer               : function(window: PSDL_Window; index: Integer; flags: Uint32): PSDL_Renderer; cdecl;
  SDL_CreateSoftwareRenderer       : function(surface: PSDL_Surface): PSDL_Renderer; cdecl;
  SDL_GetRenderer                  : function(window: PSDL_Window): PSDL_Renderer; cdecl;
  SDL_GetRendererInfo              : function(renderer: PSDL_Renderer; info: PSDL_RendererInfo): Integer; cdecl;
  SDL_GetRendererOutputSize        : function(renderer: PSDL_Renderer; w, h: PInteger): Integer; cdecl;
  SDL_CreateTexture                : function(renderer: PSDL_Renderer; format: Uint32; access, w, h: Integer): PSDL_Texture; cdecl;
  SDL_CreateTextureFromSurface     : function(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture; cdecl;
  SDL_QueryTexture                 : function(texture: PSDL_Texture; format: PUint32; access, w, h: PInteger): Integer; cdecl;
  SDL_SetTextureColorMod           : function(texture: PSDL_Texture; r, g, b: Uint8): Integer; cdecl;
  SDL_GetTextureColorMod           : function(texture: PSDL_Texture; r, g, b: PUint8): Integer; cdecl;
  SDL_SetTextureAlphaMod           : function(texture: PSDL_Texture; alpha: Uint8): Integer; cdecl;
  SDL_GetTextureAlphaMod           : function(texture: PSDL_Texture; alpha: PUint8): Integer; cdecl;
  SDL_SetTextureBlendMode          : function(texture: PSDL_Texture; blendMode: SDL_BlendMode): Integer; cdecl;
  SDL_GetTextureBlendMode          : function(texture: PSDL_Texture; blendMode: PSDL_BlendMode): Integer; cdecl;
  SDL_UpdateTexture                : function(texture: PSDL_Texture; const rect: PSDL_Rect; const pixels: Pointer; pitch: Integer): Integer; cdecl;
  SDL_LockTexture                  : function(texture: PSDL_Texture; const rect: PSDL_Rect; pixels: PPointer; pitch: PInteger): Integer; cdecl;
  SDL_UnlockTexture                : procedure(texture: PSDL_Texture); cdecl;
  SDL_RenderTargetSupported        : function(renderer: PSDL_Renderer): SDL_bool; cdecl;
  SDL_SetRenderTarget              : function(renderer: PSDL_Renderer; texture: PSDL_Texture): Integer; cdecl;
  SDL_GetRenderTarget              : function(renderer: PSDL_Renderer): PSDL_Texture; cdecl;
  SDL_RenderSetLogicalSize         : function(renderer: PSDL_Renderer; w, h: Integer): Integer; cdecl;
  SDL_RenderGetLogicalSize         : procedure(renderer: PSDL_Renderer; w, h: PInteger); cdecl;
  SDL_RenderSetViewport            : function(renderer: PSDL_Renderer; const rect: PSDL_Rect): Integer; cdecl;
  SDL_RenderGetViewport            : procedure(renderer: PSDL_Renderer; rect: PSDL_Rect); cdecl;
  SDL_RenderSetClipRect            : function(renderer: PSDL_Renderer; const rect: PSDL_Rect): Integer; cdecl;
  SDL_RenderGetClipRect            : procedure(renderer: PSDL_Renderer; rect: PSDL_Rect); cdecl;
  SDL_RenderSetScale               : function(renderer: PSDL_Renderer; scaleX, scaleY: Single): Integer; cdecl;
  SDL_RenderGetScale               : procedure(renderer: PSDL_Renderer; scaleX, scaleY: PSingle); cdecl;
  SDL_SetRenderDrawColor           : function(renderer: PSDL_Renderer; r, g, b, a: Uint8): Integer; cdecl;
  SDL_GetRenderDrawColor           : function(renderer: PSDL_Renderer; r, g, b, a: PUint8): Integer; cdecl;
  SDL_SetRenderDrawBlendMode       : function(renderer: PSDL_Renderer; blendMode: SDL_BlendMode): Integer; cdecl;
  SDL_GetRenderDrawBlendMode       : function(renderer: PSDL_Renderer; blendMode: PSDL_BlendMode): Integer; cdecl;
  SDL_RenderClear                  : function(renderer: PSDL_Renderer): Integer; cdecl;
  SDL_RenderDrawPoint              : function(renderer: PSDL_Renderer; x, y: Integer): Integer; cdecl;
  SDL_RenderDrawPoints             : function(renderer: PSDL_Renderer; const points: PSDL_Point; count: Integer): Integer; cdecl;
  SDL_RenderDrawLine               : function(renderer: PSDL_Renderer; x1, y1, x2, y2: Integer): Integer; cdecl;
  SDL_RenderDrawLines              : function(renderer: PSDL_Renderer; const points: PSDL_Point; count: Integer): Integer; cdecl;
  SDL_RenderDrawRect               : function(renderer: PSDL_Renderer; const rect: PSDL_Rect): Integer; cdecl;
  SDL_RenderDrawRects              : function(renderer: PSDL_Renderer; const rects: PSDL_Rect; count: Integer): Integer; cdecl;
  SDL_RenderFillRect               : function(renderer: PSDL_Renderer; const rect: PSDL_Rect): Integer; cdecl;
  SDL_RenderFillRects              : function(renderer: PSDL_Renderer; const rects: PSDL_Rect; count: Integer): Integer; cdecl;
  SDL_RenderCopy                   : function(renderer: PSDL_Renderer; texture: PSDL_Texture; const srcrect, dstrect: PSDL_Rect): Integer; cdecl;
  SDL_RenderCopyEx                 : function(renderer: PSDL_Renderer; texture: PSDL_Texture; const srcrect, dstrect: PSDL_Rect; angle: Double; center: PSDL_Point; flip: SDL_RendererFlip): Integer; cdecl;
  SDL_RenderReadPixels             : function(renderer: PSDL_Renderer; const rect: PSDL_Rect; format: Uint32; pixels: Pointer; pitch: Integer): Integer; cdecl;
  SDL_RenderPresent                : procedure(renderer: PSDL_Renderer); cdecl;
  SDL_DestroyTexture               : procedure(texture: PSDL_Texture); cdecl;
  SDL_DestroyRenderer              : procedure(renderer: PSDL_Renderer); cdecl;
  SDL_GL_BindTexture               : function(texture: PSDL_Texture; texw, texh: PSingle): Integer; cdecl;
  SDL_GL_UnbindTexture             : function(texture: PSDL_Texture): Integer; cdecl;

// SDL shape functions
  SDL_SetWindowShape               : function(window: PSDL_Window; shape: PSDL_Surface; shape_mode: PSDL_WindowShapeMode): Integer; cdecl;
  SDL_GetShapedWindowMode          : function(window: PSDL_Window; shape_mode: PSDL_WindowShapeMode): Integer; cdecl;

// SDL surface functions
var
  SDL_SetSurfacePalette           : function(surface: PSDL_Surface; palette: PSDL_Palette): Integer; cdecl;
  SDL_SetSurfaceRLE               : function(surface: PSDL_Surface; flag: Integer): Integer; cdecl;
  SDL_SetColorKey                 : function(surface: PSDL_Surface; flag: Integer; key: Uint32): Integer; cdecl;
  SDL_GetColorKey                 : function(surface: PSDL_Surface; key: PUint32): Integer; cdecl;
  SDL_SetSurfaceColorMod          : function(surface: PSDL_Surface; r, g, b: Uint8): Integer; cdecl;
  SDL_GetSurfaceColorMod          : function(surface: PSDL_Surface; r, g, b: PUint8): Integer; cdecl;
  SDL_SetSurfaceAlphaMod          : function(surface: PSDL_Surface; alpha: Uint8): Integer; cdecl;
  SDL_GetSurfaceAlphaMod          : function(surface: PSDL_Surface; alpha: PUint8): Integer; cdecl;
  SDL_SetSurfaceBlendMode         : function(surface: PSDL_Surface; blendMode: SDL_BlendMode): Integer; cdecl;
  SDL_GetSurfaceBlendMode         : function(surface: PSDL_Surface; blendMode: PSDL_BlendMode): Integer; cdecl;
  SDL_ConvertSurfaceFormat        : function(src: PSDL_Surface; pixel_format: Uint32; flags: Uint32): PSDL_Surface; cdecl;
  SDL_ConvertPixels               : function(width, height: Integer; src_format: Uint32; src: Pointer; src_pitch: Integer; dst_format: Uint32; dst: Pointer; dst_pitch: Integer): Integer; cdecl;
  SDL_FillRect                    : function(dst: PSDL_Surface; const rect: PSDL_Rect; color: Uint32): Integer; cdecl;
  SDL_FillRects                   : function(dst: PSDL_Surface; const rects: PSDL_Rect; count: Integer; color: Uint32): Integer; cdecl;
  SDL_UpperBlit                   : function(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; cdecl;
  SDL_LowerBlit                   : function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; cdecl;
  SDL_SoftStretch                 : function(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; const dstrect: PSDL_Rect): Integer; cdecl;
  SDL_UpperBlitScaled             : function(src: PSDL_Surface; const srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; cdecl;
  SDL_LowerBlitScaled             : function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; cdecl;
  SDL_GetNumTouchDevices          : function: Integer; cdecl;
  SDL_GetTouchDevice              : function(index: Integer): SDL_TouchID; cdecl;
  SDL_GetNumTouchFingers          : function(touchID: SDL_TouchID): Integer; cdecl;
  SDL_GetTouchFinger              : function(touchID: SDL_TouchID; index: Integer): PSDL_Finger; cdecl;
  SDL_getenv                      : function(const name: PChar): PChar; cdecl;
  SDL_setenv                      : function(const name, value: PChar; overwrite: Integer): Integer; cdecl;
  SDL_isdigit                     : function(x: Integer): Integer; cdecl;
  SDL_isspace                     : function(x: Integer): Integer; cdecl;
  SDL_toupper                     : function(x: Integer): Integer; cdecl;
  SDL_tolower                     : function(x: Integer): Integer; cdecl;

// SDL haptic functions
  SDL_NumHaptics                  : function: Integer; cdecl;
  SDL_HapticName                  : function(device_index: Integer): PChar; cdecl;
  SDL_HapticOpen                  : function(device_index: Integer): PSDL_Haptic; cdecl;
  SDL_HapticOpened                : function(device_index: Integer): Integer; cdecl;
  SDL_HapticIndex                 : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_MouseIsHaptic               : function: Integer; cdecl;
  SDL_HapticOpenFromMouse         : function: PSDL_Haptic; cdecl;
  SDL_JoystickIsHaptic            : function(joystick: PSDL_Joystick): Integer; cdecl;
  SDL_HapticOpenFromJoystick      : function(joystick: PSDL_Joystick): PSDL_Haptic; cdecl;
  SDL_HapticClose                 : procedure(haptic: PSDL_Haptic); cdecl;
  SDL_HapticNumEffects            : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticNumEffectsPlaying     : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticQuery                 : function(haptic: PSDL_Haptic): Uint32; cdecl;
  SDL_HapticNumAxes               : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticEffectSupported       : function(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Integer; cdecl;
  SDL_HapticNewEffect             : function(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Integer; cdecl;
  SDL_HapticUpdateEffect          : function(haptic: PSDL_Haptic; effect: Integer; data: PSDL_HapticEffect): Integer; cdecl;
  SDL_HapticRunEffect             : function(haptic: PSDL_Haptic; effect: Integer; iterations: Uint32): Integer; cdecl;
  SDL_HapticStopEffect            : function(haptic: PSDL_Haptic; effect: Integer): Integer; cdecl;
  SDL_HapticDestroyEffect         : procedure(haptic: PSDL_Haptic; effect: Integer); cdecl;
  SDL_HapticGetEffectStatus       : function(haptic: PSDL_Haptic; effect: Integer): Integer; cdecl;
  SDL_HapticSetGain               : function(haptic: PSDL_Haptic; gain: Integer): Integer; cdecl;
  SDL_HapticSetAutocenter         : function(haptic: PSDL_Haptic; autocenter: Integer): Integer; cdecl;
  SDL_HapticPause                 : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticUnpause               : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticStopAll               : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticRumbleSupported       : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticRumbleInit            : function(haptic: PSDL_Haptic): Integer; cdecl;
  SDL_HapticRumblePlay            : function(haptic: PSDL_Haptic; strength: Single; length: Uint32): Integer; cdecl;
  SDL_HapticRumbleStop            : function(haptic: PSDL_Haptic): Integer; cdecl;

// SDL syswm functions
  SDL_GetWindowWMInfo             : function(window: PSDL_Window; info: PSDL_SysWMinfo): SDL_bool; cdecl;

var
  SDL2 : TLibrary = nil;

function LoadSDL2( const aPath : AnsiString = SDL2DefaultPath ) : Boolean;
function SDL_RWopsFromStream( aStream : TStream; aSize : DWord ) : PSDL_RWOps;
function SDL_BUTTON( Button : Integer ) : Integer;

implementation

function LoadSDL2( const aPath : AnsiString = SDL2DefaultPath ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL2.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL2 : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL2 <> nil then Exit( True );
  SDL2 := TLibrary.Load( aPath );
  if SDL2 = nil then Exit( False );
// main
  Pointer(SDL_Init)           := GetSymbol('SDL_Init');
  Pointer(SDL_InitSubSystem)  := GetSymbol('SDL_InitSubSystem');
  Pointer(SDL_QuitSubSystem)  := GetSymbol('SDL_QuitSubSystem');
  Pointer(SDL_WasInit)        := GetSymbol('SDL_WasInit');
  Pointer(SDL_Quit)           := GetSymbol('SDL_Quit');

// io handling
  Pointer(SDL_RWFromFile)      := GetSymbol('SDL_RWFromFile');
  Pointer(SDL_RWFromFP)        := GetSymbol('SDL_RWFromFP');
  Pointer(SDL_RWFromMem)       := GetSymbol('SDL_RWFromMem');
  Pointer(SDL_RWFromConstMem)  := GetSymbol('SDL_RWFromConstMem');
  Pointer(SDL_AllocRW)         := GetSymbol('SDL_AllocRW');
  Pointer(SDL_FreeRW)          := GetSymbol('SDL_FreeRW');
  Pointer(SDL_ReadLE16)        := GetSymbol('SDL_ReadLE16');
  Pointer(SDL_ReadBE16)        := GetSymbol('SDL_ReadBE16');
  Pointer(SDL_ReadLE32)        := GetSymbol('SDL_ReadLE32');
  Pointer(SDL_ReadBE32)        := GetSymbol('SDL_ReadBE32');
  Pointer(SDL_ReadLE64)        := GetSymbol('SDL_ReadLE64');
  Pointer(SDL_ReadBE64)        := GetSymbol('SDL_ReadBE64');
  Pointer(SDL_WriteLE16)       := GetSymbol('SDL_WriteLE16');
  Pointer(SDL_WriteBE16)       := GetSymbol('SDL_WriteBE16');
  Pointer(SDL_WriteLE32)       := GetSymbol('SDL_WriteLE32');
  Pointer(SDL_WriteBE32)       := GetSymbol('SDL_WriteBE32');
  Pointer(SDL_WriteLE64)       := GetSymbol('SDL_WriteLE64');
  Pointer(SDL_WriteBE64)       := GetSymbol('SDL_WriteBE64');

  //* SDL_audio.h functions */
  Pointer(SDL_AudioInit) := GetSymbol('SDL_AudioInit');
  Pointer(SDL_AudioQuit) := GetSymbol('SDL_AudioQuit');
  Pointer(SDL_OpenAudio) := GetSymbol('SDL_OpenAudio');
  Pointer(SDL_GetAudioStatus) := GetSymbol('SDL_GetAudioStatus');
  Pointer(SDL_PauseAudio) := GetSymbol('SDL_PauseAudio');
  Pointer(SDL_LoadWAV_RW) := GetSymbol('SDL_LoadWAV_RW');
  Pointer(SDL_FreeWAV) := GetSymbol('SDL_FreeWAV');
  Pointer(SDL_BuildAudioCVT) := GetSymbol('SDL_BuildAudioCVT');
  Pointer(SDL_ConvertAudio) := GetSymbol('SDL_ConvertAudio');
  Pointer(SDL_NewAudioStream) := GetSymbol('SDL_NewAudioStream');
  Pointer(SDL_AudioStreamPut) := GetSymbol('SDL_AudioStreamPut');
  Pointer(SDL_AudioStreamGet) := GetSymbol('SDL_AudioStreamGet');
  Pointer(SDL_AudioStreamAvailable) := GetSymbol('SDL_AudioStreamAvailable');
  Pointer(SDL_AudioStreamFlush) := GetSymbol('SDL_AudioStreamFlush');
  Pointer(SDL_AudioStreamClear) := GetSymbol('SDL_AudioStreamClear');
  Pointer(SDL_FreeAudioStream) := GetSymbol('SDL_FreeAudioStream');
  Pointer(SDL_MixAudio) := GetSymbol('SDL_MixAudio');
  Pointer(SDL_LockAudio) := GetSymbol('SDL_LockAudio');
  Pointer(SDL_UnlockAudio) := GetSymbol('SDL_UnlockAudio');
  Pointer(SDL_CloseAudio) := GetSymbol('SDL_CloseAudio');

  //* SDL_cpuinfo.h functions */
  Pointer(SDL_HasRDTSC) := GetSymbol('SDL_HasRDTSC');
  Pointer(SDL_HasMMX) := GetSymbol('SDL_HasMMX');
  Pointer(SDL_Has3DNow) := GetSymbol('SDL_Has3DNow');
  Pointer(SDL_HasSSE) := GetSymbol('SDL_HasSSE');
  Pointer(SDL_HasSSE2) := GetSymbol('SDL_HasSSE2');
  Pointer(SDL_HasAltiVec) := GetSymbol('SDL_HasAltiVec');


  //* SDL_mouse.h functions */
  Pointer(SDL_SetCursor) := GetSymbol('SDL_SetCursor');
  Pointer(SDL_GetCursor) := GetSymbol('SDL_GetCursor');
  Pointer(SDL_FreeCursor) := GetSymbol('SDL_FreeCursor');
  Pointer(SDL_ShowCursor) := GetSymbol('SDL_ShowCursor');
  Pointer(SDL_GetMouseFocus) := GetSymbol('SDL_GetMouseFocus');
  Pointer(SDL_GetMouseState) := GetSymbol('SDL_GetMouseState');
  Pointer(SDL_GetGlobalMouseState) := GetSymbol('SDL_GetGlobalMouseState');
  Pointer(SDL_WarpMouseGlobal) := GetSymbol('SDL_WarpMouseGlobal');
  Pointer(SDL_GetRelativeMouseState) := GetSymbol('SDL_GetRelativeMouseState');
  Pointer(SDL_WarpMouseInWindow) := GetSymbol('SDL_WarpMouseInWindow');
  Pointer(SDL_SetRelativeMouseMode) := GetSymbol('SDL_SetRelativeMouseMode');
  Pointer(SDL_CaptureMouse) := GetSymbol('SDL_CaptureMouse');
  Pointer(SDL_GetRelativeMouseMode) := GetSymbol('SDL_GetRelativeMouseMode');
  Pointer(SDL_CreateCursor) := GetSymbol('SDL_CreateCursor');
  Pointer(SDL_CreateColorCursor) := GetSymbol('SDL_CreateColorCursor');
  Pointer(SDL_CreateSystemCursor) := GetSymbol('SDL_CreateSystemCursor');
  Pointer(SDL_GetDefaultCursor) := GetSymbol('SDL_GetDefaultCursor');


  //* SDL_joystick.h functions */
  Pointer(SDL_LockJoysticks) := GetSymbol('SDL_LockJoysticks');
  Pointer(SDL_UnlockJoysticks) := GetSymbol('SDL_UnlockJoysticks');

  Pointer(SDL_NumJoysticks) := GetSymbol('SDL_NumJoysticks');

  Pointer(SDL_JoystickNameForIndex) := GetSymbol('SDL_JoystickNameForIndex');
  Pointer(SDL_JoystickGetDeviceGUID) := GetSymbol('SDL_JoystickGetDeviceGUID');
  Pointer(SDL_JoystickGetDeviceVendor) := GetSymbol('SDL_JoystickGetDeviceVendor');
  Pointer(SDL_JoystickGetDeviceProduct) := GetSymbol('SDL_JoystickGetDeviceProduct');
  Pointer(SDL_JoystickGetDeviceProductVersion) := GetSymbol('SDL_JoystickGetDeviceProductVersion');
  Pointer(SDL_JoystickGetDeviceType) := GetSymbol('SDL_JoystickGetDeviceType');
  Pointer(SDL_JoystickGetDeviceInstanceID) := GetSymbol('SDL_JoystickGetDeviceInstanceID');
  Pointer(SDL_JoystickOpen) := GetSymbol('SDL_JoystickOpen');
  Pointer(SDL_JoystickFromInstanceID) := GetSymbol('SDL_JoystickFromInstanceID');
  Pointer(SDL_JoystickName) := GetSymbol('SDL_JoystickName');
  Pointer(SDL_JoystickGetGUID) := GetSymbol('SDL_JoystickGetGUID');
  Pointer(SDL_JoystickGetVendor) := GetSymbol('SDL_JoystickGetVendor');
  Pointer(SDL_JoystickGetProduct) := GetSymbol('SDL_JoystickGetProduct');
  Pointer(SDL_JoystickGetProductVersion) := GetSymbol('SDL_JoystickGetProductVersion');
  Pointer(SDL_JoystickGetType) := GetSymbol('SDL_JoystickGetType');
  Pointer(SDL_JoystickGetGUIDString) := GetSymbol('SDL_JoystickGetGUIDString');

  Pointer(SDL_JoystickGetGUIDFromString) := GetSymbol('SDL_JoystickGetGUIDFromString');
  Pointer(SDL_JoystickGetAttached) := GetSymbol('SDL_JoystickGetAttached');
  Pointer(SDL_JoystickInstanceID) := GetSymbol('SDL_JoystickInstanceID');


  Pointer(SDL_JoystickNumAxes) := GetSymbol('SDL_JoystickNumAxes');
  Pointer(SDL_JoystickNumBalls) := GetSymbol('SDL_JoystickNumBalls');
  Pointer(SDL_JoystickNumHats) := GetSymbol('SDL_JoystickNumHats');
  Pointer(SDL_JoystickNumButtons) := GetSymbol('SDL_JoystickNumButtons');
  Pointer(SDL_JoystickUpdate) := GetSymbol('SDL_JoystickUpdate');
  Pointer(SDL_JoystickEventState) := GetSymbol('SDL_JoystickEventState');
  Pointer(SDL_JoystickGetAxis) := GetSymbol('SDL_JoystickGetAxis');
  Pointer(SDL_JoystickGetAxisInitialState) := GetSymbol('SDL_JoystickGetAxisInitialState');

  Pointer(SDL_JoystickGetHat) := GetSymbol('SDL_JoystickGetHat');
  Pointer(SDL_JoystickGetBall) := GetSymbol('SDL_JoystickGetBall');
  Pointer(SDL_JoystickGetButton) := GetSymbol('SDL_JoystickGetButton');
  Pointer(SDL_JoystickClose) := GetSymbol('SDL_JoystickClose');
  Pointer(SDL_JoystickCurrentPowerLevel) := GetSymbol('SDL_JoystickCurrentPowerLevel');

  //* SDL_timer.h functions */
  Pointer(SDL_GetTicks) := GetSymbol('SDL_GetTicks');
  Pointer(SDL_Delay) := GetSymbol('SDL_Delay');
  Pointer(SDL_AddTimer) := GetSymbol('SDL_AddTimer');
  Pointer(SDL_RemoveTimer) := GetSymbol('SDL_RemoveTimer');

  //* SDL_event.h functions */
  Pointer(SDL_PumpEvents) := GetSymbol('SDL_PumpEvents');
  Pointer(SDL_PollEvent) := GetSymbol('SDL_PollEvent');
  Pointer(SDL_WaitEvent) := GetSymbol('SDL_WaitEvent');
  Pointer(SDL_PushEvent) := GetSymbol('SDL_PushEvent');

  //* SDL_video/SDL_surface */
  Pointer(SDL_CreateRGBSurface) := GetSymbol('SDL_CreateRGBSurface');
  Pointer(SDL_CreateRGBSurfaceFrom) := GetSymbol('SDL_CreateRGBSurfaceFrom');
  Pointer(SDL_FreeSurface) := GetSymbol('SDL_FreeSurface');
  Pointer(SDL_LockSurface) := GetSymbol('SDL_LockSurface');
  Pointer(SDL_UnlockSurface) := GetSymbol('SDL_UnlockSurface');
  Pointer(SDL_SaveBMP_RW)  := GetSymbol('SDL_SaveBMP_RW');
  Pointer(SDL_LoadBMP_RW)  := GetSymbol('SDL_LoadBMP_RW');
  Pointer(SDL_SaveBMP)     := GetSymbol('SDL_SaveBMP_RW');
  Pointer(SDL_LoadBMP)     := GetSymbol('SDL_LoadBMP_RW');
  Pointer(SDL_SetClipRect) := GetSymbol('SDL_SetClipRect');
  Pointer(SDL_GetClipRect) := GetSymbol('SDL_GetClipRect');
  Pointer(SDL_ConvertSurface) := GetSymbol('SDL_ConvertSurface');

  Pointer(SDL_GL_LoadLibrary) := GetSymbol('SDL_GL_LoadLibrary');
  Pointer(SDL_GL_GetProcAddress) := GetSymbol('SDL_GL_GetProcAddress');
  Pointer(SDL_GL_SetAttribute) := GetSymbol('SDL_GL_SetAttribute');
  Pointer(SDL_GL_GetAttribute) := GetSymbol('SDL_GL_GetAttribute');

  //* SDL_video.h defines */
  Pointer(SDL_GetNumVideoDrivers) := GetSymbol('SDL_GetNumVideoDrivers');
  Pointer(SDL_GetVideoDriver) := GetSymbol('SDL_GetVideoDriver');
  Pointer(SDL_VideoInit) := GetSymbol('SDL_VideoInit');
  Pointer(SDL_VideoQuit) := GetSymbol('SDL_VideoQuit');
  Pointer(SDL_GetCurrentVideoDriver) := GetSymbol('SDL_GetCurrentVideoDriver');
  Pointer(SDL_GetNumVideoDisplays) := GetSymbol('SDL_GetNumVideoDisplays');
  Pointer(SDL_GetDisplayName) := GetSymbol('SDL_GetDisplayName');
  Pointer(SDL_GetDisplayBounds) := GetSymbol('SDL_GetDisplayBounds');
  Pointer(SDL_GetDisplayDPI) := GetSymbol('SDL_GetDisplayDPI');
  Pointer(SDL_GetDisplayUsableBounds) := GetSymbol('SDL_GetDisplayUsableBounds');
  Pointer(SDL_GetNumDisplayModes) := GetSymbol('SDL_GetNumDisplayModes');
  Pointer(SDL_GetDisplayMode) := GetSymbol('SDL_GetDisplayMode');
  Pointer(SDL_GetDesktopDisplayMode) := GetSymbol('SDL_GetDesktopDisplayMode');
  Pointer(SDL_GetCurrentDisplayMode) := GetSymbol('SDL_GetCurrentDisplayMode');
  Pointer(SDL_GetClosestDisplayMode) := GetSymbol('SDL_GetClosestDisplayMode');
  Pointer(SDL_GetWindowDisplayIndex) := GetSymbol('SDL_GetWindowDisplayIndex');
  Pointer(SDL_SetWindowDisplayMode) := GetSymbol('SDL_SetWindowDisplayMode');
  Pointer(SDL_GetWindowDisplayMode) := GetSymbol('SDL_GetWindowDisplayMode');
  Pointer(SDL_GetWindowPixelFormat) := GetSymbol('SDL_GetWindowPixelFormat');
  Pointer(SDL_CreateWindow) := GetSymbol('SDL_CreateWindow');
  Pointer(SDL_CreateWindowFrom) := GetSymbol('SDL_CreateWindowFrom');
  Pointer(SDL_GetWindowID) := GetSymbol('SDL_GetWindowID');
  Pointer(SDL_GetWindowFromID) := GetSymbol('SDL_GetWindowFromID');
  Pointer(SDL_GetWindowFlags) := GetSymbol('SDL_GetWindowFlags');
  Pointer(SDL_SetWindowTitle) := GetSymbol('SDL_SetWindowTitle');
  Pointer(SDL_GetWindowTitle) := GetSymbol('SDL_GetWindowTitle');
  Pointer(SDL_SetWindowIcon) := GetSymbol('SDL_SetWindowIcon');
  Pointer(SDL_SetWindowData) := GetSymbol('SDL_SetWindowData');
  Pointer(SDL_GetWindowData) := GetSymbol('SDL_GetWindowData');
  Pointer(SDL_SetWindowPosition) := GetSymbol('SDL_SetWindowPosition');
  Pointer(SDL_GetWindowPosition) := GetSymbol('SDL_GetWindowPosition');
  Pointer(SDL_SetWindowSize) := GetSymbol('SDL_SetWindowSize');
  Pointer(SDL_GetWindowSize) := GetSymbol('SDL_GetWindowSize');
  Pointer(SDL_GetWindowBordersSize) := GetSymbol('SDL_GetWindowBordersSize');
  Pointer(SDL_SetWindowMinimumSize) := GetSymbol('SDL_SetWindowMinimumSize');
  Pointer(SDL_GetWindowMinimumSize) := GetSymbol('SDL_GetWindowMinimumSize');
  Pointer(SDL_SetWindowMaximumSize) := GetSymbol('SDL_SetWindowMaximumSize');
  Pointer(SDL_GetWindowMaximumSize) := GetSymbol('SDL_GetWindowMaximumSize');
  Pointer(SDL_SetWindowBordered) := GetSymbol('SDL_SetWindowBordered');
  Pointer(SDL_SetWindowResizable) := GetSymbol('SDL_SetWindowResizable');
  Pointer(SDL_ShowWindow) := GetSymbol('SDL_ShowWindow');
  Pointer(SDL_HideWindow) := GetSymbol('SDL_HideWindow');
  Pointer(SDL_RaiseWindow) := GetSymbol('SDL_RaiseWindow');
  Pointer(SDL_MaximizeWindow) := GetSymbol('SDL_MaximizeWindow');
  Pointer(SDL_MinimizeWindow) := GetSymbol('SDL_MinimizeWindow');
  Pointer(SDL_RestoreWindow) := GetSymbol('SDL_RestoreWindow');
  Pointer(SDL_SetWindowFullscreen) := GetSymbol('SDL_SetWindowFullscreen');
  Pointer(SDL_GetWindowSurface) := GetSymbol('SDL_GetWindowSurface');
  Pointer(SDL_UpdateWindowSurface) := GetSymbol('SDL_UpdateWindowSurface');
  Pointer(SDL_UpdateWindowSurfaceRects) := GetSymbol('SDL_UpdateWindowSurfaceRects');
  Pointer(SDL_SetWindowGrab) := GetSymbol('SDL_SetWindowGrab');
  Pointer(SDL_GetWindowGrab) := GetSymbol('SDL_GetWindowGrab');
  Pointer(SDL_GetGrabbedWindow) := GetSymbol('SDL_GetGrabbedWindow');
  Pointer(SDL_SetWindowBrightness) := GetSymbol('SDL_SetWindowBrightness');
  Pointer(SDL_GetWindowBrightness) := GetSymbol('SDL_GetWindowBrightness');
  Pointer(SDL_SetWindowOpacity) := GetSymbol('SDL_SetWindowOpacity');
  Pointer(SDL_GetWindowOpacity) := GetSymbol('SDL_GetWindowOpacity');
  Pointer(SDL_SetWindowModalFor) := GetSymbol('SDL_SetWindowModalFor');
  Pointer(SDL_SetWindowInputFocus) := GetSymbol('SDL_SetWindowInputFocus');
  Pointer(SDL_SetWindowGammaRamp) := GetSymbol('SDL_SetWindowGammaRamp');
  Pointer(SDL_GetWindowGammaRamp) := GetSymbol('SDL_GetWindowGammaRamp');
  // Pointer(SDL_HitTest) := GetSymbol('SDL_HitTest');
  // Pointer(SDL_SetWindowHitTest) := GetSymbol('SDL_SetWindowHitTest');
  Pointer(SDL_DestroyWindow) := GetSymbol('SDL_DestroyWindow');
  Pointer(SDL_IsScreenSaverEnabled) := GetSymbol('SDL_IsScreenSaverEnabled');
  Pointer(SDL_EnableScreenSaver) := GetSymbol('SDL_EnableScreenSaver');
  Pointer(SDL_DisableScreenSaver) := GetSymbol('SDL_DisableScreenSaver');
  Pointer(SDL_GL_UnloadLibrary) := GetSymbol('SDL_GL_UnloadLibrary');
  Pointer(SDL_GL_ExtensionSupported) := GetSymbol('SDL_GL_ExtensionSupported');
  Pointer(SDL_GL_ResetAttributes) := GetSymbol('SDL_GL_ResetAttributes');
  Pointer(SDL_GL_CreateContext) := GetSymbol('SDL_GL_CreateContext');
  Pointer(SDL_GL_MakeCurrent) := GetSymbol('SDL_GL_MakeCurrent');
  Pointer(SDL_GL_GetCurrentWindow) := GetSymbol('SDL_GL_GetCurrentWindow');
  Pointer(SDL_GL_GetCurrentContext) := GetSymbol('SDL_GL_GetCurrentContext');
  Pointer(SDL_GL_GetDrawableSize) := GetSymbol('SDL_GL_GetDrawableSize');
  Pointer(SDL_GL_SetSwapInterval) := GetSymbol('SDL_GL_SetSwapInterval');
  Pointer(SDL_GL_GetSwapInterval) := GetSymbol('SDL_GL_GetSwapInterval');
  Pointer(SDL_GL_SwapWindow) := GetSymbol('SDL_GL_SwapWindow');
  Pointer(SDL_GL_DeleteContext) := GetSymbol('SDL_GL_DeleteContext');

  //* SDL_vulkan.h functions */
  Pointer(SDL_Vulkan_LoadLibrary) := GetSymbol('SDL_Vulkan_LoadLibrary');
  Pointer(SDL_Vulkan_GetVkGetInstanceProcAddr) := GetSymbol('SDL_Vulkan_GetVkGetInstanceProcAddr');
  Pointer(SDL_Vulkan_UnloadLibrary) := GetSymbol('SDL_Vulkan_UnloadLibrary');
  Pointer(SDL_Vulkan_GetInstanceExtensions) := GetSymbol('SDL_Vulkan_GetInstanceExtensions');
  Pointer(SDL_Vulkan_CreateSurface) := GetSymbol('SDL_Vulkan_CreateSurface');
  Pointer(SDL_Vulkan_GetDrawableSize) := GetSymbol('SDL_Vulkan_GetDrawableSize');

  //* SDL_audio.h functions */
  Pointer(SDL_GetNumAudioDrivers) := GetSymbol('SDL_GetNumAudioDrivers');
  Pointer(SDL_GetAudioDriver) := GetSymbol('SDL_GetAudioDriver');
  Pointer(SDL_GetCurrentAudioDriver) := GetSymbol('SDL_GetCurrentAudioDriver');
  Pointer(SDL_GetNumAudioDevices) := GetSymbol('SDL_GetNumAudioDevices');
  Pointer(SDL_GetAudioDeviceName) := GetSymbol('SDL_GetAudioDeviceName');
  Pointer(SDL_OpenAudioDevice) := GetSymbol('SDL_OpenAudioDevice');
  Pointer(SDL_GetAudioDeviceStatus) := GetSymbol('SDL_GetAudioDeviceStatus');
  Pointer(SDL_PauseAudioDevice) := GetSymbol('SDL_PauseAudioDevice');
  Pointer(SDL_MixAudioFormat) := GetSymbol('SDL_MixAudioFormat');
  Pointer(SDL_QueueAudio) := GetSymbol('SDL_QueueAudio');
  Pointer(SDL_DequeueAudio) := GetSymbol('SDL_DequeueAudio');
  Pointer(SDL_GetQueuedAudioSize) := GetSymbol('SDL_GetQueuedAudioSize');
  Pointer(SDL_ClearQueuedAudio) := GetSymbol('SDL_ClearQueuedAudio');

  Pointer(SDL_LockAudioDevice) := GetSymbol('SDL_LockAudioDevice');
  Pointer(SDL_UnlockAudioDevice) := GetSymbol('SDL_UnlockAudioDevice');
  Pointer(SDL_CloseAudioDevice) := GetSymbol('SDL_CloseAudioDevice');
  // Pointer(SDL_AudioDeviceConnected) := GetSymbol('SDL_AudioDeviceConnected');

  //* SDL_cpuinfo.h functions */
  Pointer(SDL_GetCPUCount) := GetSymbol('SDL_GetCPUCount');
  Pointer(SDL_GetCPUCacheLineSize) := GetSymbol('SDL_GetCPUCacheLineSize');
  Pointer(SDL_HasSSE3) := GetSymbol('SDL_HasSSE3');
  Pointer(SDL_HasSSE41) := GetSymbol('SDL_HasSSE41');
  Pointer(SDL_HasSSE42) := GetSymbol('SDL_HasSSE42');
  Pointer(SDL_HasAVX) := GetSymbol('SDL_HasAVX');
  Pointer(SDL_HasAVX2) := GetSymbol('SDL_HasAVX2');
  Pointer(SDL_HasNEON) := GetSymbol('SDL_HasNEON');
  Pointer(SDL_GetSystemRAM) := GetSymbol('SDL_GetSystemRAM');

  //* SDL_error.h functions */
  Pointer(SDL_SetError) := GetSymbol('SDL_SetError');
  Pointer(SDL_Error) := GetSymbol('SDL_Error');
  Pointer(SDL_GetError) := GetSymbol('SDL_GetError');
  Pointer(SDL_ClearError) := GetSymbol('SDL_ClearError');

  //* SDL_keyboard.h functions */
  Pointer(SDL_GetKeyboardFocus) := GetSymbol('SDL_GetKeyboardFocus');
  Pointer(SDL_GetKeyboardState) := GetSymbol('SDL_GetKeyboardState');
  Pointer(SDL_GetModState) := GetSymbol('SDL_GetModState');
  Pointer(SDL_SetModState) := GetSymbol('SDL_SetModState');
  Pointer(SDL_GetKeyFromScancode) := GetSymbol('SDL_GetKeyFromScancode');
  Pointer(SDL_GetScancodeFromKey) := GetSymbol('SDL_GetScancodeFromKey');
  Pointer(SDL_GetScancodeName) := GetSymbol('SDL_GetScancodeName');
  Pointer(SDL_GetScancodeFromName) := GetSymbol('SDL_GetScancodeFromName');
  Pointer(SDL_GetKeyName) := GetSymbol('SDL_GetKeyName');
  Pointer(SDL_GetKeyFromName) := GetSymbol('SDL_GetKeyFromName');
  Pointer(SDL_StartTextInput) := GetSymbol('SDL_StartTextInput');
  Pointer(SDL_IsTextInputActive) := GetSymbol('SDL_IsTextInputActive');
  Pointer(SDL_StopTextInput) := GetSymbol('SDL_StopTextInput');
  Pointer(SDL_SetTextInputRect) := GetSymbol('SDL_SetTextInputRect');
  Pointer(SDL_HasScreenKeyboardSupport) := GetSymbol('SDL_HasScreenKeyboardSupport');
  Pointer(SDL_IsScreenKeyboardShown) := GetSymbol('SDL_IsScreenKeyboardShown');


  Pointer(SDL_GetPerformanceCounter) := GetSymbol('SDL_GetPerformanceCounter');
  Pointer(SDL_GetPerformanceFrequency) := GetSymbol('SDL_GetPerformanceFrequency');

  //* SDL_version.h functions */
  Pointer(SDL_GetVersion) := GetSymbol('SDL_GetVersion');
  Pointer(SDL_GetRevision) := GetSymbol('SDL_GetRevision');
  Pointer(SDL_GetRevisionNumber) := GetSymbol('SDL_GetRevisionNumber');

  //* SDL_event.h functions */
  Pointer(SDL_PeepEvents) := GetSymbol('SDL_PeepEvents');
  Pointer(SDL_HasEvent) := GetSymbol('SDL_HasEvent');
  Pointer(SDL_HasEvents) := GetSymbol('SDL_HasEvents');
  Pointer(SDL_FlushEvent) := GetSymbol('SDL_FlushEvent');
  Pointer(SDL_FlushEvents) := GetSymbol('SDL_FlushEvents');
  Pointer(SDL_WaitEventTimeout) := GetSymbol('SDL_WaitEventTimeout');
  Pointer(SDL_SetEventFilter) := GetSymbol('SDL_SetEventFilter');
  Pointer(SDL_GetEventFilter) := GetSymbol('SDL_GetEventFilter');
  Pointer(SDL_AddEventWatch) := GetSymbol('SDL_AddEventWatch');
  Pointer(SDL_DelEventWatch) := GetSymbol('SDL_DelEventWatch');
  Pointer(SDL_FilterEvents) := GetSymbol('SDL_FilterEvents');
  Pointer(SDL_EventState) := GetSymbol('SDL_EventState');
  Pointer(SDL_RegisterEvents) := GetSymbol('SDL_RegisterEvents');

  //* SDL_clipboard.h functions */
  Pointer(SDL_SetClipboardText) := GetSymbol('SDL_SetClipboardText');
  Pointer(SDL_GetClipboardText) := GetSymbol('SDL_GetClipboardText');
  Pointer(SDL_HasClipboardText) := GetSymbol('SDL_HasClipboardText');

  //* SDL_gamecontroller.h functions */
  Pointer(SDL_GameControllerAddMappingsFromRW) := GetSymbol('SDL_GameControllerAddMappingsFromRW');
  Pointer(SDL_GameControllerAddMapping) := GetSymbol('SDL_GameControllerAddMapping');
  Pointer(SDL_GameControllerNumMappings) := GetSymbol('SDL_GameControllerNumMappings');
  Pointer(SDL_GameControllerMappingForIndex) := GetSymbol('SDL_GameControllerMappingForIndex');
  Pointer(SDL_GameControllerMappingForGUID) := GetSymbol('SDL_GameControllerMappingForGUID');
  Pointer(SDL_GameControllerMapping) := GetSymbol('SDL_GameControllerMapping');
  Pointer(SDL_IsGameController) := GetSymbol('SDL_IsGameController');
  Pointer(SDL_GameControllerNameForIndex) := GetSymbol('SDL_GameControllerNameForIndex');
  Pointer(SDL_GameControllerOpen) := GetSymbol('SDL_GameControllerOpen');
  Pointer(SDL_GameControllerFromInstanceID) := GetSymbol('SDL_GameControllerFromInstanceID');
  Pointer(SDL_GameControllerName) := GetSymbol('SDL_GameControllerName');
  Pointer(SDL_GameControllerGetVendor) := GetSymbol('SDL_GameControllerGetVendor');
  Pointer(SDL_GameControllerGetProduct) := GetSymbol('SDL_GameControllerGetProduct');
  Pointer(SDL_GameControllerGetProductVersion) := GetSymbol('SDL_GameControllerGetProductVersion');
  Pointer(SDL_GameControllerGetAttached) := GetSymbol('SDL_GameControllerGetAttached');
  Pointer(SDL_GameControllerGetJoystick) := GetSymbol('SDL_GameControllerGetJoystick');
  Pointer(SDL_GameControllerEventState) := GetSymbol('SDL_GameControllerEventState');
  Pointer(SDL_GameControllerUpdate) := GetSymbol('SDL_GameControllerUpdate');
  Pointer(SDL_GameControllerGetAxisFromString) := GetSymbol('SDL_GameControllerGetAxisFromString');
  Pointer(SDL_GameControllerGetStringForAxis) := GetSymbol('SDL_GameControllerGetStringForAxis');
  Pointer(SDL_GameControllerGetBindForAxis) := GetSymbol('SDL_GameControllerGetBindForAxis');
  Pointer(SDL_GameControllerGetAxis) := GetSymbol('SDL_GameControllerGetAxis');
  Pointer(SDL_GameControllerGetButtonFromString) := GetSymbol('SDL_GameControllerGetButtonFromString');
  Pointer(SDL_GameControllerGetStringForButton) := GetSymbol('SDL_GameControllerGetStringForButton');
  Pointer(SDL_GameControllerGetBindForButton) := GetSymbol('SDL_GameControllerGetBindForButton');
  Pointer(SDL_GameControllerGetButton) := GetSymbol('SDL_GameControllerGetButton');
  Pointer(SDL_GameControllerClose) := GetSymbol('SDL_GameControllerClose');

  //* SDL_hints.h functions */
  Pointer(SDL_SetHintWithPriority) := GetSymbol('SDL_SetHintWithPriority');
  Pointer(SDL_SetHint) := GetSymbol('SDL_SetHint');
  Pointer(SDL_GetHint) := GetSymbol('SDL_GetHint');
  Pointer(SDL_GetHintBoolean) := GetSymbol('SDL_GetHintBoolean');
  Pointer(SDL_AddHintCallback) := GetSymbol('SDL_AddHintCallback');
  Pointer(SDL_DelHintCallback) := GetSymbol('SDL_DelHintCallback');
  Pointer(SDL_ClearHints) := GetSymbol('SDL_ClearHints');

  //* SDL_main.h functions */
  Pointer(SDL_SetMainReady) := GetSymbol('SDL_SetMainReady');
  // Pointer(SDL_RegisterApp) := GetSymbol('SDL_RegisterApp');
  // Pointer(SDL_UnregisterApp) := GetSymbol('SDL_UnregisterApp');

  //* SDL_messagebox.h functions */
  Pointer(SDL_ShowMessageBox) := GetSymbol('SDL_ShowMessageBox');
  Pointer(SDL_ShowSimpleMessageBox) := GetSymbol('SDL_ShowSimpleMessageBox');

  //* SDL_pixels.h functions */
  Pointer(SDL_GetPixelFormatName) := GetSymbol('SDL_GetPixelFormatName');
  Pointer(SDL_PixelFormatEnumToMasks) := GetSymbol('SDL_PixelFormatEnumToMasks');
  Pointer(SDL_MasksToPixelFormatEnum) := GetSymbol('SDL_MasksToPixelFormatEnum');
  Pointer(SDL_AllocFormat) := GetSymbol('SDL_AllocFormat');
  Pointer(SDL_FreeFormat) := GetSymbol('SDL_FreeFormat');
  Pointer(SDL_AllocPalette) := GetSymbol('SDL_AllocPalette');
  Pointer(SDL_SetPixelFormatPalette) := GetSymbol('SDL_SetPixelFormatPalette');
  Pointer(SDL_SetPaletteColors) := GetSymbol('SDL_SetPaletteColors');
  Pointer(SDL_FreePalette) := GetSymbol('SDL_FreePalette');
  Pointer(SDL_MapRGB) := GetSymbol('SDL_MapRGB');
  Pointer(SDL_MapRGBA) := GetSymbol('SDL_MapRGBA');
  Pointer(SDL_GetRGB) := GetSymbol('SDL_GetRGB');
  Pointer(SDL_GetRGBA) := GetSymbol('SDL_GetRGBA');
  Pointer(SDL_CalculateGammaRamp) := GetSymbol('SDL_CalculateGammaRamp');

  //* SDL_platform.h functions */
  Pointer(SDL_GetPlatform) := GetSymbol('SDL_GetPlatform');

  //* SDL_power.h functions */
  Pointer(SDL_GetPowerInfo) := GetSymbol('SDL_GetPowerInfo');

  //* SDL_renderer.h functions */
  Pointer(SDL_GetNumRenderDrivers) := GetSymbol('SDL_GetNumRenderDrivers');
  Pointer(SDL_GetRenderDriverInfo) := GetSymbol('SDL_GetRenderDriverInfo');
  Pointer(SDL_CreateWindowAndRenderer) := GetSymbol('SDL_CreateWindowAndRenderer');
  Pointer(SDL_CreateRenderer) := GetSymbol('SDL_CreateRenderer');
  Pointer(SDL_CreateSoftwareRenderer) := GetSymbol('SDL_CreateSoftwareRenderer');
  Pointer(SDL_GetRenderer) := GetSymbol('SDL_GetRenderer');
  Pointer(SDL_GetRendererInfo) := GetSymbol('SDL_GetRendererInfo');
  Pointer(SDL_GetRendererOutputSize) := GetSymbol('SDL_GetRendererOutputSize');
  Pointer(SDL_CreateTexture) := GetSymbol('SDL_CreateTexture');
  Pointer(SDL_CreateTextureFromSurface) := GetSymbol('SDL_CreateTextureFromSurface');
  Pointer(SDL_QueryTexture) := GetSymbol('SDL_QueryTexture');
  Pointer(SDL_SetTextureColorMod) := GetSymbol('SDL_SetTextureColorMod');
  Pointer(SDL_GetTextureColorMod) := GetSymbol('SDL_GetTextureColorMod');
  Pointer(SDL_SetTextureAlphaMod) := GetSymbol('SDL_SetTextureAlphaMod');
  Pointer(SDL_GetTextureAlphaMod) := GetSymbol('SDL_GetTextureAlphaMod');
  Pointer(SDL_SetTextureBlendMode) := GetSymbol('SDL_SetTextureBlendMode');
  Pointer(SDL_GetTextureBlendMode) := GetSymbol('SDL_GetTextureBlendMode');
  Pointer(SDL_UpdateTexture) := GetSymbol('SDL_UpdateTexture');
  Pointer(SDL_LockTexture) := GetSymbol('SDL_LockTexture');
  Pointer(SDL_UnlockTexture) := GetSymbol('SDL_UnlockTexture');
  Pointer(SDL_RenderTargetSupported) := GetSymbol('SDL_RenderTargetSupported');
  Pointer(SDL_SetRenderTarget) := GetSymbol('SDL_SetRenderTarget');
  Pointer(SDL_GetRenderTarget) := GetSymbol('SDL_GetRenderTarget');
  Pointer(SDL_RenderSetLogicalSize) := GetSymbol('SDL_RenderSetLogicalSize');
  Pointer(SDL_RenderGetLogicalSize) := GetSymbol('SDL_RenderGetLogicalSize');
  Pointer(SDL_RenderSetViewport) := GetSymbol('SDL_RenderSetViewport');
  Pointer(SDL_RenderGetViewport) := GetSymbol('SDL_RenderGetViewport');
  Pointer(SDL_RenderSetClipRect) := GetSymbol('SDL_RenderSetClipRect');
  Pointer(SDL_RenderGetClipRect) := GetSymbol('SDL_RenderGetClipRect');
  Pointer(SDL_RenderSetScale) := GetSymbol('SDL_RenderSetScale');
  Pointer(SDL_RenderGetScale) := GetSymbol('SDL_RenderGetScale');
  Pointer(SDL_SetRenderDrawColor) := GetSymbol('SDL_SetRenderDrawColor');
  Pointer(SDL_GetRenderDrawColor) := GetSymbol('SDL_GetRenderDrawColor');
  Pointer(SDL_SetRenderDrawBlendMode) := GetSymbol('SDL_SetRenderDrawBlendMode');
  Pointer(SDL_GetRenderDrawBlendMode) := GetSymbol('SDL_GetRenderDrawBlendMode');
  Pointer(SDL_RenderClear) := GetSymbol('SDL_RenderClear');
  Pointer(SDL_RenderDrawPoint) := GetSymbol('SDL_RenderDrawPoint');
  Pointer(SDL_RenderDrawPoints) := GetSymbol('SDL_RenderDrawPoints');
  Pointer(SDL_RenderDrawLine) := GetSymbol('SDL_RenderDrawLine');
  Pointer(SDL_RenderDrawLines) := GetSymbol('SDL_RenderDrawLines');
  Pointer(SDL_RenderDrawRect) := GetSymbol('SDL_RenderDrawRect');
  Pointer(SDL_RenderDrawRects) := GetSymbol('SDL_RenderDrawRects');
  Pointer(SDL_RenderFillRect) := GetSymbol('SDL_RenderFillRect');
  Pointer(SDL_RenderFillRects) := GetSymbol('SDL_RenderFillRects');
  Pointer(SDL_RenderCopy) := GetSymbol('SDL_RenderCopy');
  Pointer(SDL_RenderCopyEx) := GetSymbol('SDL_RenderCopyEx');
  Pointer(SDL_RenderReadPixels) := GetSymbol('SDL_RenderReadPixels');
  Pointer(SDL_RenderPresent) := GetSymbol('SDL_RenderPresent');
  Pointer(SDL_DestroyTexture) := GetSymbol('SDL_DestroyTexture');
  Pointer(SDL_DestroyRenderer) := GetSymbol('SDL_DestroyRenderer');
  Pointer(SDL_GL_BindTexture) := GetSymbol('SDL_GL_BindTexture');
  Pointer(SDL_GL_UnbindTexture) := GetSymbol('SDL_GL_UnbindTexture');

  //* SDL_shape.h functions */
  Pointer(SDL_SetWindowShape) := GetSymbol('SDL_SetWindowShape');
  Pointer(SDL_GetShapedWindowMode) := GetSymbol('SDL_GetShapedWindowMode');

  //* SDL_surface.h functions */
  Pointer(SDL_SetSurfacePalette) := GetSymbol('SDL_SetSurfacePalette');
  Pointer(SDL_SetSurfaceRLE) := GetSymbol('SDL_SetSurfaceRLE');
  Pointer(SDL_SetColorKey) := GetSymbol('SDL_SetColorKey');
  Pointer(SDL_GetColorKey) := GetSymbol('SDL_GetColorKey');
  Pointer(SDL_SetSurfaceColorMod) := GetSymbol('SDL_SetSurfaceColorMod');
  Pointer(SDL_GetSurfaceColorMod) := GetSymbol('SDL_GetSurfaceColorMod');
  Pointer(SDL_SetSurfaceAlphaMod) := GetSymbol('SDL_SetSurfaceAlphaMod');
  Pointer(SDL_GetSurfaceAlphaMod) := GetSymbol('SDL_GetSurfaceAlphaMod');
  Pointer(SDL_SetSurfaceBlendMode) := GetSymbol('SDL_SetSurfaceBlendMode');
  Pointer(SDL_GetSurfaceBlendMode) := GetSymbol('SDL_GetSurfaceBlendMode');

  Pointer(SDL_ConvertSurfaceFormat) := GetSymbol('SDL_ConvertSurfaceFormat');
  Pointer(SDL_ConvertPixels) := GetSymbol('SDL_ConvertPixels');
  Pointer(SDL_FillRect) := GetSymbol('SDL_FillRect');
  Pointer(SDL_FillRects) := GetSymbol('SDL_FillRects');
  Pointer(SDL_UpperBlit) := GetSymbol('SDL_UpperBlit');
  Pointer(SDL_LowerBlit) := GetSymbol('SDL_LowerBlit');
  Pointer(SDL_SoftStretch) := GetSymbol('SDL_SoftStretch');
  Pointer(SDL_UpperBlitScaled) := GetSymbol('SDL_UpperBlitScaled');
  Pointer(SDL_LowerBlitScaled) := GetSymbol('SDL_LowerBlitScaled');
  Pointer(SDL_GetNumTouchDevices) := GetSymbol('SDL_GetNumTouchDevices');
  Pointer(SDL_GetTouchDevice) := GetSymbol('SDL_GetTouchDevice');
  Pointer(SDL_GetNumTouchFingers) := GetSymbol('SDL_GetNumTouchFingers');
  Pointer(SDL_GetTouchFinger) := GetSymbol('SDL_GetTouchFinger');

  Pointer(SDL_getenv) := GetSymbol('SDL_getenv');
  Pointer(SDL_setenv) := GetSymbol('SDL_setenv');
  Pointer(SDL_isdigit) := GetSymbol('SDL_isdigit');
  Pointer(SDL_isspace) := GetSymbol('SDL_isspace');
  Pointer(SDL_toupper) := GetSymbol('SDL_toupper');
  Pointer(SDL_tolower) := GetSymbol('SDL_tolower');

  //* SDL_haptic.h functions */
  Pointer(SDL_NumHaptics) := GetSymbol('SDL_NumHaptics');
  Pointer(SDL_HapticName) := GetSymbol('SDL_HapticName');
  Pointer(SDL_HapticOpen) := GetSymbol('SDL_HapticOpen');
  Pointer(SDL_HapticOpened) := GetSymbol('SDL_HapticOpened');
  Pointer(SDL_HapticIndex) := GetSymbol('SDL_HapticIndex');
  Pointer(SDL_MouseIsHaptic) := GetSymbol('SDL_MouseIsHaptic');
  Pointer(SDL_HapticOpenFromMouse) := GetSymbol('SDL_HapticOpenFromMouse');
  Pointer(SDL_JoystickIsHaptic) := GetSymbol('SDL_JoystickIsHaptic');
  Pointer(SDL_HapticOpenFromJoystick) := GetSymbol('SDL_HapticOpenFromJoystick');
  Pointer(SDL_HapticClose) := GetSymbol('SDL_HapticClose');
  Pointer(SDL_HapticNumEffects) := GetSymbol('SDL_HapticNumEffects');
  Pointer(SDL_HapticNumEffectsPlaying) := GetSymbol('SDL_HapticNumEffectsPlaying');
  Pointer(SDL_HapticQuery) := GetSymbol('SDL_HapticQuery');
  Pointer(SDL_HapticNumAxes) := GetSymbol('SDL_HapticNumAxes');
  Pointer(SDL_HapticEffectSupported) := GetSymbol('SDL_HapticEffectSupported');
  Pointer(SDL_HapticNewEffect) := GetSymbol('SDL_HapticNewEffect');
  Pointer(SDL_HapticUpdateEffect) := GetSymbol('SDL_HapticUpdateEffect');
  Pointer(SDL_HapticRunEffect) := GetSymbol('SDL_HapticRunEffect');
  Pointer(SDL_HapticStopEffect) := GetSymbol('SDL_HapticStopEffect');
  Pointer(SDL_HapticDestroyEffect) := GetSymbol('SDL_HapticDestroyEffect');
  Pointer(SDL_HapticGetEffectStatus) := GetSymbol('SDL_HapticGetEffectStatus');
  Pointer(SDL_HapticSetGain) := GetSymbol('SDL_HapticSetGain');
  Pointer(SDL_HapticSetAutocenter) := GetSymbol('SDL_HapticSetAutocenter');
  Pointer(SDL_HapticPause) := GetSymbol('SDL_HapticPause');
  Pointer(SDL_HapticUnpause) := GetSymbol('SDL_HapticUnpause');
  Pointer(SDL_HapticStopAll) := GetSymbol('SDL_HapticStopAll');
  Pointer(SDL_HapticRumbleSupported) := GetSymbol('SDL_HapticRumbleSupported');
  Pointer(SDL_HapticRumbleInit) := GetSymbol('SDL_HapticRumbleInit');
  Pointer(SDL_HapticRumblePlay) := GetSymbol('SDL_HapticRumblePlay');
  Pointer(SDL_HapticRumbleStop) := GetSymbol('SDL_HapticRumbleStop');

  ///* SDL_sysmw functions */
  Pointer(SDL_GetWindowWMInfo) := GetSymbol('SDL_GetWindowWMInfo');

  Exit( True );
end;

function RW_Stream_Size( context: PSDL_RWops ): SInt64; cdecl;
begin
  Exit( SInt64( QWord(context^.hidden.unknown.data2) and $FFFFFFFF ) );
end;

function RW_Stream_Seek( context: PSDL_RWops; offset: SInt64; whence: Integer ) : SInt64; cdecl;
var iStream : TStream;
    iOffset : DWord;
    iSize   : DWord;
begin
  iStream := TStream(context^.hidden.unknown.data1);
  iOffset := DWord( QWord(context^.hidden.unknown.data2) shr 32 );
  iSize   := DWord( QWord(context^.hidden.unknown.data2) and $FFFFFFFF );

  case whence of
    0 : iStream.Seek( iOffset+offset, soBeginning );
    1 : iStream.Seek( offset, soCurrent );
    2 : iStream.Seek( iOffset+iSize+offset, soCurrent );
  end;

  Exit( iStream.Position-iOffset );
end;

function RW_Stream_Read( context: PSDL_RWops; Ptr: Pointer; size, maxnum : size_t ): size_t; cdecl;
var iStream : TStream;
begin
  iStream := TStream(context^.hidden.unknown.data1);
  Exit( iStream.Read( Ptr^, size * maxnum ) div size );
end;

function RW_Stream_Write( context: PSDL_RWops; const Ptr: Pointer; size, num: size_t ): size_t; cdecl;
var iStream : TStream;
begin
  iStream := TStream(context^.hidden.unknown.data1);
  Exit( iStream.Write( Ptr^, size * num ) div size );
end;

function RW_Stream_Close( context: PSDL_RWops ): Integer; cdecl;
var iStream : TStream;
begin
  if Context <> nil then
  begin
    iStream := TStream(context^.hidden.unknown.data1);
    FreeAndNil( iStream );
    SDL_FreeRW( context );
  end;
  Exit( 0 );
end;

function SDL_RWopsFromStream( aStream : TStream; aSize : DWord ) : PSDL_RWops;
begin
  SDL_RWopsFromStream := SDL_AllocRW();
  if SDL_RWopsFromStream <> nil then
  begin
    SDL_RWopsFromStream^.size := @RW_Stream_Size;
    SDL_RWopsFromStream^.seek := @RW_Stream_Seek;
    SDL_RWopsFromStream^.read := @RW_Stream_Read;
    SDL_RWopsFromStream^.write := @RW_Stream_Write;
    SDL_RWopsFromStream^.close := @RW_Stream_Close;
    SDL_RWopsFromStream^.typ   := SDL_RWOPS_UNKNOWN;
    SDL_RWopsFromStream^.hidden.unknown.data1 := Pointer( aStream );
    SDL_RWopsFromStream^.hidden.unknown.data2 := Pointer( QWord(aStream.Position) shl 32 or aSize );
  end;
end;

function SDL_BUTTON( Button : Integer ) : Integer;
begin
  Result := SDL_PRESSED shl ( Button - 1 );
end;

finalization
  if SDL2 <> nil then FreeAndNil( SDL2 );

end.

