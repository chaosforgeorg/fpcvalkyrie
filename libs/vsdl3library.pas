{$include ../src/valkyrie.inc}
unit vsdl3library;
{$PACKRECORDS C}
{$MINENUMSIZE 4}
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
  SDL3DefaultPath = 'SDL3.dll';
{$ELSE}
  {$IFDEF DARWIN}
    SDL3DefaultPath = 'SDL3.framework/SDL3';
    {$linklib SDLmain}
    {$linkframework Cocoa}
    {$linkframework SDL}
    {$PASCALMAINNAME SDL_main}
  {$ELSE}
  SDL3DefaultPath = 'libSDL3-3.0.so.0';
  {$ENDIF}
{$ENDIF}

const

  SDL_MAJOR_VERSION = 3;
  SDL_MINOR_VERSION = 2;
  SDL_PATCHLEVEL    = 20;

{$include vsdl3types.inc}
{$include vsdl3const.inc}

{ SDL_init.h }
var
  SDL_Init                      : function(flags: SDL_InitFlags): SDL_bool; cdecl;
  SDL_InitSubSystem             : function(flags: SDL_InitFlags): SDL_bool; cdecl;
  SDL_QuitSubSystem             : procedure(flags: SDL_InitFlags); cdecl;
  SDL_WasInit                   : function(flags: SDL_InitFlags): SDL_InitFlags; cdecl;
  SDL_Quit                      : procedure; cdecl;
  SDL_IsMainThread              : function: SDL_bool; cdecl;
  SDL_RunOnMainThread           : function(callback: SDL_MainThreadCallback; userdata: Pointer; wait_complete: SDL_bool): SDL_bool; cdecl;
  SDL_SetAppMetadata            : function( const appname, appversion, appidentifier: PAnsiChar ): SDL_bool; cdecl;
  SDL_SetAppMetadataProperty    : function( const name, value: PAnsiChar ): SDL_bool; cdecl;
  SDL_GetAppMetadataProperty    : function( const name: PAnsiChar ): PAnsiChar; cdecl;

{ SDL_error.h }
var
  SDL_SetError       : function(const fmt: PAnsiChar): SDL_bool; cdecl; varargs;
  SDL_OutOfMemory    : function: SDL_bool; cdecl;
  SDL_GetError       : function: PAnsiChar; cdecl;
  SDL_ClearError     : function: SDL_bool; cdecl;

{ SDL_keyboard.h }
var
  SDL_HasKeyboard                   : function: SDL_bool; cdecl;
  SDL_GetKeyboards                  : function(count: PSint32): PSDL_KeyboardID; cdecl;
  SDL_GetKeyboardNameForID          : function(instance_id: SDL_KeyboardID): PAnsiChar; cdecl;
  SDL_GetKeyboardFocus              : function: PSDL_Window; cdecl;

  SDL_GetKeyboardState              : function(numkeys: Sint32): PSDL_bool; cdecl;
  SDL_ResetKeyboard                 : procedure; cdecl;

  SDL_GetModState                   : function: SDL_Keymod; cdecl;
  SDL_SetModState                   : procedure(modstate: SDL_Keymod); cdecl;

  SDL_GetKeyFromScancode            : function(scancode: SDL_Scancode; modstate: SDL_Keymod; key_event: SDL_bool): SDL_Keycode; cdecl;
  SDL_GetScancodeFromKey            : function(key: SDL_Keycode; modstate: PSDL_Keymod): SDL_Scancode; cdecl;

  SDL_SetScancodeName               : function(scancode: SDL_Scancode; const name: PAnsiChar): SDL_bool; cdecl;
  SDL_GetScancodeName               : function(scancode: SDL_Scancode): PAnsiChar; cdecl;
  SDL_GetScancodeFromName           : function(const name: PAnsiChar): SDL_Scancode; cdecl;

  SDL_GetKeyName                    : function(key: SDL_Keycode): PAnsiChar; cdecl;
  SDL_GetKeyFromName                : function(const name: PAnsiChar): SDL_Keycode; cdecl;

  SDL_StartTextInput                : function(window: PSDL_Window): SDL_bool; cdecl;
  SDL_StartTextInputWithProperties  : function(window: PSDL_Window; props: SDL_PropertiesID): SDL_bool; cdecl;
  SDL_TextInputActive               : function(window: PSDL_Window): SDL_bool; cdecl;
  SDL_StopTextInput                 : function(window: PSDL_Window): SDL_bool; cdecl;
  SDL_ClearComposition              : function(window: PSDL_Window): SDL_bool; cdecl;
  SDL_SetTextInputArea              : function(window: PSDL_Window; const rect: PSDL_Rect; cursor: Sint32): SDL_bool; cdecl;
  SDL_GetTextInputArea              : function(window: PSDL_Window; rect: PSDL_Rect; cursor: PSint32): SDL_bool; cdecl;

  SDL_HasScreenKeyboardSupport      : function: SDL_bool; cdecl;
  SDL_ScreenKeyboardShown           : function(window: PSDL_Window): SDL_bool; cdecl;

{ SDL_mouse.h }
var
  SDL_HasMouse                    : function: SDL_bool; cdecl;
  SDL_GetMice                     : function(count: PSint32): PSDL_MouseID; cdecl;
  SDL_GetMouseNameForID           : function(instance_id: SDL_MouseID): PAnsiChar; cdecl;
  SDL_GetMouseFocus               : function: PSDL_Window; cdecl;

  SDL_GetMouseState               : function(x: PSingle; y: PSingle): SDL_MouseButtonFlags; cdecl;
  SDL_GetGlobalMouseState         : function(x: PSingle; y: PSingle): SDL_MouseButtonFlags; cdecl;
  SDL_GetRelativeMouseState       : function(x: PSingle; y: PSingle): SDL_MouseButtonFlags; cdecl;

  SDL_WarpMouseInWindow           : procedure(window: PSDL_Window; x: Single; y: Single); cdecl;
  SDL_WarpMouseGlobal             : function(x: Single; y: Single): SDL_bool; cdecl;

  SDL_SetWindowRelativeMouseMode  : function(window: PSDL_Window; enabled: SDL_bool): SDL_bool; cdecl;
  SDL_GetWindowRelativeMouseMode  : function(window: PSDL_Window): SDL_bool; cdecl;

  SDL_CaptureMouse                : function(enabled: SDL_bool): SDL_bool; cdecl;

  SDL_CreateCursor                : function(const data: PUint8; const mask: PUint8; w, h, hot_x, hot_y: Sint32): PSDL_Cursor; cdecl;
  SDL_CreateColorCursor           : function(surface: PSDL_Surface; hot_x, hot_y: Sint32): PSDL_Cursor; cdecl;
  SDL_CreateSystemCursor          : function(id: SDL_SystemCursor): PSDL_Cursor; cdecl;

  SDL_SetCursor                   : function(cursor: PSDL_Cursor): SDL_bool; cdecl;
  SDL_GetCursor                   : function: PSDL_Cursor; cdecl;
  SDL_GetDefaultCursor            : function: PSDL_Cursor; cdecl;
  SDL_DestroyCursor               : procedure(cursor: PSDL_Cursor); cdecl;

  SDL_ShowCursor                  : function: SDL_bool; cdecl;
  SDL_HideCursor                  : function: SDL_bool; cdecl;
  SDL_CursorVisible               : function: SDL_bool; cdecl;

{ SDL_gamepad.h }
var
  SDL_HasGamepad                          : function: SDL_bool; cdecl;
  SDL_GetGamepads                         : function(count: PInteger): PSDL_JoystickID; cdecl;
  SDL_IsGamepad                           : function(instance_id: SDL_JoystickID): SDL_bool; cdecl;

  SDL_GetGamepadNameForID                 : function(instance_id: SDL_JoystickID): PAnsiChar; cdecl;
  SDL_GetGamepadPathForID                 : function(instance_id: SDL_JoystickID): PAnsiChar; cdecl;
  SDL_GetGamepadPlayerIndexForID          : function(instance_id: SDL_JoystickID): Integer; cdecl;
  SDL_GetGamepadVendorForID               : function(instance_id: SDL_JoystickID): Uint16; cdecl;
  SDL_GetGamepadProductForID              : function(instance_id: SDL_JoystickID): Uint16; cdecl;
  SDL_GetGamepadProductVersionForID       : function(instance_id: SDL_JoystickID): Uint16; cdecl;
  SDL_GetGamepadTypeForID                 : function(instance_id: SDL_JoystickID): SDL_GamepadType; cdecl;
  SDL_GetRealGamepadTypeForID             : function(instance_id: SDL_JoystickID): SDL_GamepadType; cdecl;
  SDL_GetGamepadMappingForID              : function(instance_id: SDL_JoystickID): PAnsiChar; cdecl;
  SDL_OpenGamepad                         : function(instance_id: SDL_JoystickID): PSDL_Gamepad; cdecl;
  SDL_GetGamepadFromID                    : function(instance_id: SDL_JoystickID): PSDL_Gamepad; cdecl;
  SDL_GetGamepadFromPlayerIndex           : function(player_index: Integer): PSDL_Gamepad; cdecl;
  SDL_CloseGamepad                        : procedure(gamepad: PSDL_Gamepad); cdecl;

  SDL_GetGamepadID                        : function(gamepad: PSDL_Gamepad): SDL_JoystickID; cdecl;
  SDL_GetGamepadName                      : function(gamepad: PSDL_Gamepad): PAnsiChar; cdecl;
  SDL_GetGamepadPath                      : function(gamepad: PSDL_Gamepad): PAnsiChar; cdecl;
  SDL_GetGamepadType                      : function(gamepad: PSDL_Gamepad): SDL_GamepadType; cdecl;
  SDL_GetRealGamepadType                  : function(gamepad: PSDL_Gamepad): SDL_GamepadType; cdecl;
  SDL_GetGamepadPlayerIndex               : function(gamepad: PSDL_Gamepad): Integer; cdecl;
  SDL_SetGamepadPlayerIndex               : function(gamepad: PSDL_Gamepad; player_index: Integer): SDL_bool; cdecl;
  SDL_GetGamepadVendor                    : function(gamepad: PSDL_Gamepad): Uint16; cdecl;
  SDL_GetGamepadProduct                   : function(gamepad: PSDL_Gamepad): Uint16; cdecl;
  SDL_GetGamepadProductVersion            : function(gamepad: PSDL_Gamepad): Uint16; cdecl;
  SDL_GetGamepadSerial                    : function(gamepad: PSDL_Gamepad): PAnsiChar; cdecl;
  SDL_GetGamepadProperties                : function(gamepad: PSDL_Gamepad): SDL_PropertiesID; cdecl;
  //SDL_GetGamepadConnectionState           : function(gamepad: PSDL_Gamepad): SDL_JoystickConnectionState; cdecl;
  //SDL_GetGamepadPowerInfo                 : function(gamepad: PSDL_Gamepad; percent: PInteger): SDL_PowerState; cdecl;
  SDL_GamepadConnected                    : function(gamepad: PSDL_Gamepad): SDL_bool; cdecl;

  SDL_SetGamepadEventsEnabled             : procedure(enabled: SDL_bool); cdecl;
  SDL_GamepadEventsEnabled                : function: SDL_bool; cdecl;
  SDL_UpdateGamepads                      : procedure; cdecl;

  SDL_GamepadHasAxis                      : function(gamepad: PSDL_Gamepad; axis: SDL_GamepadAxis): SDL_bool; cdecl;
  SDL_GetGamepadAxis                      : function(gamepad: PSDL_Gamepad; axis: SDL_GamepadAxis): Sint16; cdecl;
  SDL_GamepadHasButton                    : function(gamepad: PSDL_Gamepad; button: SDL_GamepadButton): SDL_bool; cdecl;
  SDL_GetGamepadButton                    : function(gamepad: PSDL_Gamepad; button: SDL_GamepadButton): SDL_bool; cdecl;
  SDL_GetGamepadButtonLabelForType        : function(aType: SDL_GamepadType; button: SDL_GamepadButton): SDL_GamepadButtonLabel; cdecl;
  SDL_GetGamepadButtonLabel               : function(gamepad: PSDL_Gamepad; button: SDL_GamepadButton): SDL_GamepadButtonLabel; cdecl;

  SDL_RumbleGamepad                       : function(gamepad: PSDL_Gamepad; low_frequency_rumble, high_frequency_rumble: Uint16; duration_ms: Uint32): SDL_bool; cdecl;
  SDL_RumbleGamepadTriggers               : function(gamepad: PSDL_Gamepad; left_rumble, right_rumble: Uint16; duration_ms: Uint32): SDL_bool; cdecl;
  SDL_SetGamepadLED                       : function(gamepad: PSDL_Gamepad; red, green, blue: Uint8): SDL_bool; cdecl;
  SDL_SendGamepadEffect                   : function(gamepad: PSDL_Gamepad; const data: Pointer; size: Integer): SDL_bool; cdecl;

  SDL_AddGamepadMapping                   : function(const mapping: PAnsiChar): Integer; cdecl;
  SDL_AddGamepadMappingsFromIO            : function(src: PSDL_IOStream; closeio: SDL_bool): Integer; cdecl;
  SDL_AddGamepadMappingsFromFile          : function(const file_: PAnsiChar): Integer; cdecl;
  SDL_ReloadGamepadMappings               : function: SDL_bool; cdecl;
  SDL_GetGamepadMappings                  : function(count: PInteger): PPAnsiChar; cdecl;
  //SDL_GetGamepadMappingForGUID            : function(guid: SDL_GUID): PAnsiChar; cdecl;
  SDL_GetGamepadMapping                   : function(gamepad: PSDL_Gamepad): PAnsiChar; cdecl;
  SDL_SetGamepadMapping                   : function(instance_id: SDL_JoystickID; const mapping: PAnsiChar): SDL_bool; cdecl;

  SDL_GetGamepadTypeFromString            : function(const str: PAnsiChar): SDL_GamepadType; cdecl;
  SDL_GetGamepadStringForType             : function(aType: SDL_GamepadType): PAnsiChar; cdecl;
  SDL_GetGamepadAxisFromString            : function(const str: PAnsiChar): SDL_GamepadAxis; cdecl;
  SDL_GetGamepadStringForAxis             : function(axis: SDL_GamepadAxis): PAnsiChar; cdecl;
  SDL_GetGamepadButtonFromString          : function(const str: PAnsiChar): SDL_GamepadButton; cdecl;
  SDL_GetGamepadStringForButton           : function(button: SDL_GamepadButton): PAnsiChar; cdecl;

{ SDL_surface.h}
var
  SDL_CreateSurfaceFrom                 : function(width, height: LongInt; src_format: SDL_PixelFormat; pixels : Pointer; pitch : LongInt ): PSDL_Surface; cdecl;
  SDL_ConvertSurface                    : function( src : PSDL_Surface; dst_format: SDL_PixelFormat ): PSDL_Surface; cdecl;
  SDL_ConvertPixels                     : function(width, height: LongInt; src_format: SDL_PixelFormat; const src: Pointer; src_pitch: LongInt;
                                                   dst_format: SDL_PixelFormat; dst: Pointer; dst_pitch: LongInt): SDL_bool; cdecl;
  SDL_DestroySurface                    : procedure(surface: PSDL_Surface); cdecl;

{ SDL_video.h }
var
  SDL_GetNumVideoDrivers          : function: LongInt; cdecl;
  SDL_GetVideoDriver              : function(index: LongInt): PAnsiChar; cdecl;
  SDL_GetCurrentVideoDriver       : function: PAnsiChar; cdecl;
  SDL_GetSystemTheme              : function: SDL_SystemTheme; cdecl;

  SDL_GetDisplays                 : function(count: PLongInt): PSDL_DisplayID; cdecl;
  SDL_GetPrimaryDisplay           : function: SDL_DisplayID; cdecl;
  SDL_GetDisplayProperties        : function(displayID: SDL_DisplayID): SDL_PropertiesID; cdecl;
  SDL_GetDisplayName              : function(displayID: SDL_DisplayID): PAnsiChar; cdecl;
  SDL_GetDisplayBounds            : function(displayID: SDL_DisplayID; rect: PSDL_Rect): SDL_Bool; cdecl;
  SDL_GetDisplayUsableBounds      : function(displayID: SDL_DisplayID; rect: PSDL_Rect): SDL_Bool; cdecl;
  SDL_GetNaturalDisplayOrientation: function(displayID: SDL_DisplayID): SDL_DisplayOrientation; cdecl;
  SDL_GetCurrentDisplayOrientation: function(displayID: SDL_DisplayID): SDL_DisplayOrientation; cdecl;
  SDL_GetDisplayContentScale      : function(displayID: SDL_DisplayID): Single; cdecl;
  SDL_GetFullscreenDisplayModes   : function(displayID: SDL_DisplayID; count: PLongInt): PPSDL_DisplayMode; cdecl;
  SDL_GetClosestFullscreenDisplayMode
                                  : function(displayID: SDL_DisplayID; w, h: LongInt; refresh_rate: Single;
                                             include_high_density_modes: SDL_Bool; closest: PSDL_DisplayMode): SDL_Bool; cdecl;
  SDL_GetDesktopDisplayMode       : function(displayID: SDL_DisplayID): PSDL_DisplayMode; cdecl;
  SDL_GetCurrentDisplayMode       : function(displayID: SDL_DisplayID): PSDL_DisplayMode; cdecl;
  SDL_GetDisplayForPoint          : function(const point: PSDL_Point): SDL_DisplayID; cdecl;
  SDL_GetDisplayForRect           : function(const rect: PSDL_Rect): SDL_DisplayID; cdecl;
  SDL_GetDisplayForWindow         : function(window: PSDL_Window): SDL_DisplayID; cdecl;

  SDL_GetWindowPixelDensity       : function(window: PSDL_Window): Single; cdecl;
  SDL_GetWindowDisplayScale       : function(window: PSDL_Window): Single; cdecl;
  SDL_SetWindowFullscreenMode     : function(window: PSDL_Window; const mode: PSDL_DisplayMode): SDL_Bool; cdecl;
  SDL_GetWindowFullscreenMode     : function(window: PSDL_Window): PSDL_DisplayMode; cdecl;

  SDL_GetWindowICCProfile         : function(window: PSDL_Window; size: PSizeT): Pointer; cdecl;
  SDL_GetWindowPixelFormat        : function(window: PSDL_Window): SDL_PixelFormat; cdecl;
  SDL_GetWindows                  : function(count: PLongInt): PPSDL_Window; cdecl;

  SDL_CreateWindow                : function(const title: PAnsiChar; w, h: LongInt; flags: SDL_WindowFlags): PSDL_Window; cdecl;
  SDL_CreatePopupWindow           : function(parent: PSDL_Window; offset_x, offset_y, w, h: LongInt; flags: SDL_WindowFlags): PSDL_Window; cdecl;
  SDL_CreateWindowWithProperties  : function(props: SDL_PropertiesID): PSDL_Window; cdecl;

  SDL_GetWindowID                 : function(window: PSDL_Window): SDL_WindowID; cdecl;
  SDL_GetWindowFromID             : function(id: SDL_WindowID): PSDL_Window; cdecl;
  SDL_GetWindowParent             : function(window: PSDL_Window): PSDL_Window; cdecl;
  SDL_GetWindowProperties         : function(window: PSDL_Window): SDL_PropertiesID; cdecl;
  SDL_GetWindowFlags              : function(window: PSDL_Window): SDL_WindowFlags; cdecl;

  SDL_SetWindowTitle              : function(window: PSDL_Window; const title: PAnsiChar): SDL_Bool; cdecl;
  SDL_GetWindowTitle              : function(window: PSDL_Window): PAnsiChar; cdecl;
  SDL_SetWindowIcon               : function(window: PSDL_Window; icon: PSDL_Surface): SDL_Bool; cdecl;

  SDL_SetWindowPosition           : function(window: PSDL_Window; x, y: LongInt): SDL_Bool; cdecl;
  SDL_GetWindowPosition           : function(window: PSDL_Window; x, y: PLongInt): SDL_Bool; cdecl;

  SDL_SetWindowSize               : function(window: PSDL_Window; w, h: LongInt): SDL_Bool; cdecl;
  SDL_GetWindowSize               : function(window: PSDL_Window; w, h: PLongInt): SDL_Bool; cdecl;
  SDL_GetWindowSafeArea           : function(window: PSDL_Window; rect: PSDL_Rect): SDL_Bool; cdecl;

  SDL_SetWindowAspectRatio        : function(window: PSDL_Window; min_aspect, max_aspect: Single): SDL_Bool; cdecl;
  SDL_GetWindowAspectRatio        : function(window: PSDL_Window; min_aspect, max_aspect: PSingle): SDL_Bool; cdecl;

  SDL_GetWindowBordersSize        : function(window: PSDL_Window; top, left, bottom, right: PLongInt): SDL_Bool; cdecl;
  SDL_GetWindowSizeInPixels       : function(window: PSDL_Window; w, h: PLongInt): SDL_Bool; cdecl;

  SDL_SetWindowMinimumSize        : function(window: PSDL_Window; min_w, min_h: LongInt): SDL_Bool; cdecl;
  SDL_GetWindowMinimumSize        : function(window: PSDL_Window; w, h: PLongInt): SDL_Bool; cdecl;
  SDL_SetWindowMaximumSize        : function(window: PSDL_Window; max_w, max_h: LongInt): SDL_Bool; cdecl;
  SDL_GetWindowMaximumSize        : function(window: PSDL_Window; w, h: PLongInt): SDL_Bool; cdecl;

  SDL_SetWindowBordered           : function(window: PSDL_Window; bordered: SDL_Bool): SDL_Bool; cdecl;
  SDL_SetWindowResizable          : function(window: PSDL_Window; resizable: SDL_Bool): SDL_Bool; cdecl;
  SDL_SetWindowAlwaysOnTop        : function(window: PSDL_Window; on_top: SDL_Bool): SDL_Bool; cdecl;

  SDL_ShowWindow                  : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_HideWindow                  : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_RaiseWindow                 : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_MaximizeWindow              : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_MinimizeWindow              : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_RestoreWindow               : function(window: PSDL_Window): SDL_Bool; cdecl;

  SDL_SetWindowFullscreen         : function(window: PSDL_Window; fullscreen: SDL_Bool): SDL_Bool; cdecl;
  SDL_SyncWindow                  : function(window: PSDL_Window): SDL_Bool; cdecl;

  SDL_WindowHasSurface            : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_GetWindowSurface            : function(window: PSDL_Window): PSDL_Surface; cdecl;

  SDL_SetWindowSurfaceVSync       : function(window: PSDL_Window; vsync: LongInt): SDL_Bool; cdecl;
  SDL_GetWindowSurfaceVSync       : function(window: PSDL_Window; vsync: PLongInt): SDL_Bool; cdecl;

  SDL_UpdateWindowSurface         : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_UpdateWindowSurfaceRects    : function(window: PSDL_Window; const rects: PSDL_Rect; numrects: LongInt): SDL_Bool; cdecl;
  SDL_DestroyWindowSurface        : function(window: PSDL_Window): SDL_Bool; cdecl;

  SDL_SetWindowKeyboardGrab       : function(window: PSDL_Window; grabbed: SDL_Bool): SDL_Bool; cdecl;
  SDL_SetWindowMouseGrab          : function(window: PSDL_Window; grabbed: SDL_Bool): SDL_Bool; cdecl;
  SDL_GetWindowKeyboardGrab       : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_GetWindowMouseGrab          : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_GetGrabbedWindow            : function: PSDL_Window; cdecl;

  SDL_SetWindowMouseRect          : function(window: PSDL_Window; const rect: PSDL_Rect): SDL_Bool; cdecl;
  SDL_GetWindowMouseRect          : function(window: PSDL_Window): PSDL_Rect; cdecl;

  SDL_SetWindowOpacity            : function(window: PSDL_Window; opacity: Single): SDL_Bool; cdecl;
  SDL_GetWindowOpacity            : function(window: PSDL_Window): Single; cdecl;

  SDL_SetWindowParent             : function(window, parent: PSDL_Window): SDL_Bool; cdecl;
  SDL_SetWindowModal              : function(window: PSDL_Window; modal: SDL_Bool): SDL_Bool; cdecl;
  SDL_SetWindowFocusable          : function(window: PSDL_Window; focusable: SDL_Bool): SDL_Bool; cdecl;

  SDL_ShowWindowSystemMenu        : function(window: PSDL_Window; x, y: LongInt): SDL_Bool; cdecl;

  SDL_SetWindowHitTest            : function(window: PSDL_Window; callback: SDL_HitTest; callback_data: Pointer): SDL_Bool; cdecl;

  SDL_SetWindowShape              : function(window: PSDL_Window; shape: PSDL_Surface): SDL_Bool; cdecl;
  SDL_FlashWindow                 : function(window: PSDL_Window; operation: SDL_FlashOperation): SDL_Bool; cdecl;

  SDL_DestroyWindow               : procedure(window: PSDL_Window); cdecl;

  SDL_ScreenSaverEnabled          : function: SDL_Bool; cdecl;
  SDL_EnableScreenSaver           : function: SDL_Bool; cdecl;
  SDL_DisableScreenSaver          : function: SDL_Bool; cdecl;

  SDL_GL_LoadLibrary              : function(const path: PAnsiChar): SDL_Bool; cdecl;
  SDL_GL_GetProcAddress           : function(const proc: PAnsiChar): Pointer; cdecl;
  SDL_EGL_GetProcAddress          : function(const proc: PAnsiChar): Pointer; cdecl;
  SDL_GL_UnloadLibrary            : procedure; cdecl;

  SDL_GL_ExtensionSupported       : function(const extension: PAnsiChar): SDL_Bool; cdecl;
  SDL_GL_ResetAttributes          : procedure; cdecl;
  SDL_GL_SetAttribute             : function(attr: SDL_GLAttr; value: LongInt): SDL_Bool; cdecl;
  SDL_GL_GetAttribute             : function(attr: SDL_GLAttr; value: PLongInt): SDL_Bool; cdecl;

  SDL_GL_CreateContext            : function(window: PSDL_Window): SDL_GLContext; cdecl;
  SDL_GL_MakeCurrent              : function(window: PSDL_Window; context: SDL_GLContext): SDL_Bool; cdecl;
  SDL_GL_GetCurrentWindow         : function: PSDL_Window; cdecl;
  SDL_GL_GetCurrentContext        : function: SDL_GLContext; cdecl;

  SDL_EGL_GetCurrentDisplay       : function: SDL_EGLDisplay; cdecl;
  SDL_EGL_GetCurrentConfig        : function: SDL_EGLConfig; cdecl;
  SDL_EGL_GetWindowSurface        : function(window: PSDL_Window): SDL_EGLSurface; cdecl;

  SDL_EGL_SetAttributeCallbacks   : procedure(platformAttribCallback: SDL_EGLAttribArrayCallback;
                                              surfaceAttribCallback: SDL_EGLIntArrayCallback;
                                              contextAttribCallback: SDL_EGLIntArrayCallback;
                                              userdata: Pointer); cdecl;

  SDL_GL_SetSwapInterval          : function(interval: LongInt): SDL_Bool; cdecl;
  SDL_GL_GetSwapInterval          : function(interval: PLongInt): SDL_Bool; cdecl;
  SDL_GL_SwapWindow               : function(window: PSDL_Window): SDL_Bool; cdecl;
  SDL_GL_DestroyContext           : function(context: SDL_GLContext): SDL_Bool; cdecl;

var
  SDL_GetTicks               : function: UInt64; cdecl;
  SDL_GetTicksNS             : function: UInt64; cdecl;
  SDL_GetPerformanceCounter  : function: UInt64; cdecl;
  SDL_GetPerformanceFrequency: function: UInt64; cdecl;
  SDL_Delay                  : procedure(ms: UInt32); cdecl;
  SDL_DelayNS                : procedure(ns: UInt64); cdecl;
  SDL_DelayPrecise           : procedure(ns: UInt64); cdecl;
  SDL_AddTimer               : function(interval: UInt32; callback: SDL_TimerCallback;   userdata: Pointer): SDL_TimerID; cdecl;
  SDL_AddTimerNS             : function(interval: UInt64; callback: SDL_NSTimerCallback; userdata: Pointer): SDL_TimerID; cdecl;
  SDL_RemoveTimer            : function(id: SDL_TimerID): Boolean; cdecl;

var
  SDL_PumpEvents            : procedure; cdecl;
  SDL_PeepEvents            : function(events: PSDL_Event; numevents: Integer;
                                       action: SDL_EventAction; minType, maxType: UInt32): Integer; cdecl;
  SDL_HasEvent              : function(type_: UInt32): Boolean; cdecl;
  SDL_HasEvents             : function(minType, maxType: UInt32): Boolean; cdecl;
  SDL_FlushEvent            : procedure(type_: UInt32); cdecl;
  SDL_FlushEvents           : procedure(minType, maxType: UInt32); cdecl;
  SDL_PollEvent             : function(event: PSDL_Event): Boolean; cdecl;
  SDL_WaitEvent             : function(event: PSDL_Event): Boolean; cdecl;
  SDL_WaitEventTimeout      : function(event: PSDL_Event; timeoutMS: Sint32): Boolean; cdecl;
  SDL_PushEvent             : function(event: PSDL_Event): Boolean; cdecl;
  SDL_SetEventFilter        : procedure(filter: SDL_EventFilter; userdata: Pointer); cdecl;
  SDL_GetEventFilter        : function(var filter: SDL_EventFilter; var userdata: Pointer): Boolean; cdecl;
  SDL_AddEventWatch         : function(filter: SDL_EventFilter; userdata: Pointer): Boolean; cdecl;
  SDL_RemoveEventWatch      : procedure(filter: SDL_EventFilter; userdata: Pointer); cdecl;
  SDL_FilterEvents          : procedure(filter: SDL_EventFilter; userdata: Pointer); cdecl;
  SDL_SetEventEnabled       : procedure(type_: UInt32; enabled: Boolean); cdecl;
  SDL_EventEnabled          : function(type_: UInt32): Boolean; cdecl;
  SDL_RegisterEvents        : function(numevents: Integer): UInt32; cdecl;
  SDL_GetWindowFromEvent    : function(const event: PSDL_Event): PSDL_Window; cdecl;

{ SDL_iostream.h }
var
  SDL_IOFromFile        : function(const file_: PAnsiChar; const mode: PAnsiChar): PSDL_IOStream; cdecl;
  SDL_IOFromMem         : function(mem: Pointer; size: SizeT): PSDL_IOStream; cdecl;
  SDL_IOFromConstMem    : function(const mem: Pointer; size: SizeT): PSDL_IOStream; cdecl;
  SDL_IOFromDynamicMem  : function: PSDL_IOStream; cdecl;

  SDL_OpenIO            : function(const iface: PSDL_IOStreamInterface; userdata: Pointer): PSDL_IOStream; cdecl;
  SDL_CloseIO           : function(context: PSDL_IOStream): SDL_bool; cdecl;

  SDL_LoadFile_IO       : function(src: PSDL_IOStream; datasize: PSizeT; closeio: SDL_bool): Pointer; cdecl;
  SDL_LoadFile          : function(const file_: PAnsiChar; datasize: PSizeT): Pointer; cdecl;
  SDL_SaveFile_IO       : function(src: PSDL_IOStream; const data: Pointer; datasize: SizeT; closeio: SDL_bool): SDL_bool; cdecl;
  SDL_SaveFile          : function(const file_: PAnsiChar; const data: Pointer; datasize: SizeT): SDL_bool; cdecl;

  SDL_GetBooleanProperty: function( props : SDL_PropertiesID; const name : PAnsiChar; default : Boolean ) : Boolean;
  SDL_SetNumberProperty : function( props : SDL_PropertiesID; const name : PAnsiChar; value : Sint32 ) : Boolean;
  SDL_CreateProperties  : function : SDL_PropertiesID;
  SDL_DestroyProperties : procedure( props : SDL_PropertiesID );
  SDL_free              : procedure(data: Pointer); cdecl;

var
  SDL3 : TLibrary = nil;

function LoadSDL3( const aPath : AnsiString = SDL3DefaultPath ) : Boolean;

function SDL_IOFromStream(aStream: TStream; aSize: Int64; aOwnsStream: SDL_bool; aReadOnly: SDL_bool = False): PSDL_IOStream;
function SDL_BUTTON_MASK(button: Integer): UInt32; inline;
function SDL_WINDOWPOS_UNDEFINED_DISPLAY(x: Integer): Integer; inline;
function SDL_WINDOWPOS_CENTERED_DISPLAY(x: Integer): Integer; inline;

const
  SDL_WINDOWPOS_UNDEFINED = SDL_WINDOWPOS_UNDEFINED_MASK or 0;
  SDL_WINDOWPOS_CENTERED  = SDL_WINDOWPOS_CENTERED_MASK  or 0;

implementation

function LoadSDL3( const aPath : AnsiString = SDL3DefaultPath ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL3.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL3 : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL3 <> nil then Exit( True );
  SDL3 := TLibrary.Load( aPath );
  if SDL3 = nil then Exit( False );

// SDL_init.h
  Pointer(SDL_Init)                   := GetSymbol('SDL_Init');
  Pointer(SDL_InitSubSystem)          := GetSymbol('SDL_InitSubSystem');
  Pointer(SDL_QuitSubSystem)          := GetSymbol('SDL_QuitSubSystem');
  Pointer(SDL_WasInit)                := GetSymbol('SDL_WasInit');
  Pointer(SDL_Quit)                   := GetSymbol('SDL_Quit');
  Pointer(SDL_IsMainThread)           := GetSymbol('SDL_IsMainThread');
  Pointer(SDL_RunOnMainThread)        := GetSymbol('SDL_RunOnMainThread');
  Pointer(SDL_SetAppMetadata)         := GetSymbol('SDL_SetAppMetadata');
  Pointer(SDL_SetAppMetadataProperty) := GetSymbol('SDL_SetAppMetadataProperty');
  Pointer(SDL_GetAppMetadataProperty) := GetSymbol('SDL_GetAppMetadataProperty');

// SDL_error.h
  Pointer(SDL_SetError)    := GetSymbol('SDL_SetError');
  Pointer(SDL_OutOfMemory) := GetSymbol('SDL_OutOfMemory');
  Pointer(SDL_GetError)    := GetSymbol('SDL_GetError');
  Pointer(SDL_ClearError)  := GetSymbol('SDL_ClearError');

// SDL_keyboard.h
  Pointer(SDL_HasKeyboard)                  := GetSymbol('SDL_HasKeyboard');
  Pointer(SDL_GetKeyboards)                 := GetSymbol('SDL_GetKeyboards');
  Pointer(SDL_GetKeyboardNameForID)         := GetSymbol('SDL_GetKeyboardNameForID');
  Pointer(SDL_GetKeyboardFocus)             := GetSymbol('SDL_GetKeyboardFocus');

  Pointer(SDL_GetKeyboardState)             := GetSymbol('SDL_GetKeyboardState');
  Pointer(SDL_ResetKeyboard)                := GetSymbol('SDL_ResetKeyboard');

  Pointer(SDL_GetModState)                  := GetSymbol('SDL_GetModState');
  Pointer(SDL_SetModState)                  := GetSymbol('SDL_SetModState');

  Pointer(SDL_GetKeyFromScancode)           := GetSymbol('SDL_GetKeyFromScancode');
  Pointer(SDL_GetScancodeFromKey)           := GetSymbol('SDL_GetScancodeFromKey');

  Pointer(SDL_SetScancodeName)              := GetSymbol('SDL_SetScancodeName');
  Pointer(SDL_GetScancodeName)              := GetSymbol('SDL_GetScancodeName');
  Pointer(SDL_GetScancodeFromName)          := GetSymbol('SDL_GetScancodeFromName');

  Pointer(SDL_GetKeyName)                   := GetSymbol('SDL_GetKeyName');
  Pointer(SDL_GetKeyFromName)               := GetSymbol('SDL_GetKeyFromName');

  Pointer(SDL_StartTextInput)               := GetSymbol('SDL_StartTextInput');
  Pointer(SDL_StartTextInputWithProperties) := GetSymbol('SDL_StartTextInputWithProperties');
  Pointer(SDL_TextInputActive)              := GetSymbol('SDL_TextInputActive');
  Pointer(SDL_StopTextInput)                := GetSymbol('SDL_StopTextInput');
  Pointer(SDL_ClearComposition)             := GetSymbol('SDL_ClearComposition');
  Pointer(SDL_SetTextInputArea)             := GetSymbol('SDL_SetTextInputArea');
  Pointer(SDL_GetTextInputArea)             := GetSymbol('SDL_GetTextInputArea');

  Pointer(SDL_HasScreenKeyboardSupport)     := GetSymbol('SDL_HasScreenKeyboardSupport');
  Pointer(SDL_ScreenKeyboardShown)          := GetSymbol('SDL_ScreenKeyboardShown');

// SDL_mouse.h
  Pointer(SDL_HasMouse)                   := GetSymbol('SDL_HasMouse');
  Pointer(SDL_GetMice)                    := GetSymbol('SDL_GetMice');
  Pointer(SDL_GetMouseNameForID)          := GetSymbol('SDL_GetMouseNameForID');
  Pointer(SDL_GetMouseFocus)              := GetSymbol('SDL_GetMouseFocus');

  Pointer(SDL_GetMouseState)              := GetSymbol('SDL_GetMouseState');
  Pointer(SDL_GetGlobalMouseState)        := GetSymbol('SDL_GetGlobalMouseState');
  Pointer(SDL_GetRelativeMouseState)      := GetSymbol('SDL_GetRelativeMouseState');

  Pointer(SDL_WarpMouseInWindow)          := GetSymbol('SDL_WarpMouseInWindow');
  Pointer(SDL_WarpMouseGlobal)            := GetSymbol('SDL_WarpMouseGlobal');

  Pointer(SDL_SetWindowRelativeMouseMode) := GetSymbol('SDL_SetWindowRelativeMouseMode');
  Pointer(SDL_GetWindowRelativeMouseMode) := GetSymbol('SDL_GetWindowRelativeMouseMode');

  Pointer(SDL_CaptureMouse)               := GetSymbol('SDL_CaptureMouse');

  Pointer(SDL_CreateCursor)               := GetSymbol('SDL_CreateCursor');
  Pointer(SDL_CreateColorCursor)          := GetSymbol('SDL_CreateColorCursor');
  Pointer(SDL_CreateSystemCursor)         := GetSymbol('SDL_CreateSystemCursor');

  Pointer(SDL_SetCursor)                  := GetSymbol('SDL_SetCursor');
  Pointer(SDL_GetCursor)                  := GetSymbol('SDL_GetCursor');
  Pointer(SDL_GetDefaultCursor)           := GetSymbol('SDL_GetDefaultCursor');
  Pointer(SDL_DestroyCursor)              := GetSymbol('SDL_DestroyCursor');

  Pointer(SDL_ShowCursor)                 := GetSymbol('SDL_ShowCursor');
  Pointer(SDL_HideCursor)                 := GetSymbol('SDL_HideCursor');
  Pointer(SDL_CursorVisible)              := GetSymbol('SDL_CursorVisible');

// SDL_gamepad.h
  Pointer(SDL_HasGamepad)                   := GetSymbol('SDL_HasGamepad');
  Pointer(SDL_GetGamepads)                  := GetSymbol('SDL_GetGamepads');
  Pointer(SDL_IsGamepad)                    := GetSymbol('SDL_IsGamepad');

  Pointer(SDL_GetGamepadNameForID)          := GetSymbol('SDL_GetGamepadNameForID');
  Pointer(SDL_GetGamepadPathForID)          := GetSymbol('SDL_GetGamepadPathForID');
  Pointer(SDL_GetGamepadPlayerIndexForID)   := GetSymbol('SDL_GetGamepadPlayerIndexForID');
//  Pointer(SDL_GetGamepadGUIDForID)          := GetSymbol('SDL_GetGamepadGUIDForID');
  Pointer(SDL_GetGamepadVendorForID)        := GetSymbol('SDL_GetGamepadVendorForID');
  Pointer(SDL_GetGamepadProductForID)       := GetSymbol('SDL_GetGamepadProductForID');
  Pointer(SDL_GetGamepadProductVersionForID):= GetSymbol('SDL_GetGamepadProductVersionForID');
  Pointer(SDL_GetGamepadTypeForID)          := GetSymbol('SDL_GetGamepadTypeForID');
  Pointer(SDL_GetRealGamepadTypeForID)      := GetSymbol('SDL_GetRealGamepadTypeForID');
  Pointer(SDL_GetGamepadMappingForID)       := GetSymbol('SDL_GetGamepadMappingForID');
  Pointer(SDL_GetGamepadProperties)         := GetSymbol('SDL_GetGamepadProperties');
  Pointer(SDL_OpenGamepad)                  := GetSymbol('SDL_OpenGamepad');
  Pointer(SDL_GetGamepadFromID)             := GetSymbol('SDL_GetGamepadFromID');
  Pointer(SDL_GetGamepadFromPlayerIndex)    := GetSymbol('SDL_GetGamepadFromPlayerIndex');
  Pointer(SDL_CloseGamepad)                 := GetSymbol('SDL_CloseGamepad');

  Pointer(SDL_GetGamepadID)                 := GetSymbol('SDL_GetGamepadID');
  Pointer(SDL_GetGamepadName)               := GetSymbol('SDL_GetGamepadName');
  Pointer(SDL_GetGamepadPath)               := GetSymbol('SDL_GetGamepadPath');
  Pointer(SDL_GetGamepadType)               := GetSymbol('SDL_GetGamepadType');
  Pointer(SDL_GetRealGamepadType)           := GetSymbol('SDL_GetRealGamepadType');
  Pointer(SDL_GetGamepadPlayerIndex)        := GetSymbol('SDL_GetGamepadPlayerIndex');
  Pointer(SDL_SetGamepadPlayerIndex)        := GetSymbol('SDL_SetGamepadPlayerIndex');
  Pointer(SDL_GetGamepadVendor)             := GetSymbol('SDL_GetGamepadVendor');
  Pointer(SDL_GetGamepadProduct)            := GetSymbol('SDL_GetGamepadProduct');
  Pointer(SDL_GetGamepadProductVersion)     := GetSymbol('SDL_GetGamepadProductVersion');
  Pointer(SDL_GetGamepadSerial)             := GetSymbol('SDL_GetGamepadSerial');
//  Pointer(SDL_GetGamepadConnectionState)    := GetSymbol('SDL_GetGamepadConnectionState');
//  Pointer(SDL_GetGamepadPowerInfo)          := GetSymbol('SDL_GetGamepadPowerInfo');
  Pointer(SDL_GamepadConnected)             := GetSymbol('SDL_GamepadConnected');

  Pointer(SDL_SetGamepadEventsEnabled)      := GetSymbol('SDL_SetGamepadEventsEnabled');
  Pointer(SDL_GamepadEventsEnabled)         := GetSymbol('SDL_GamepadEventsEnabled');
  Pointer(SDL_UpdateGamepads)               := GetSymbol('SDL_UpdateGamepads');

  Pointer(SDL_GamepadHasAxis)               := GetSymbol('SDL_GamepadHasAxis');
  Pointer(SDL_GetGamepadAxis)               := GetSymbol('SDL_GetGamepadAxis');
  Pointer(SDL_GamepadHasButton)             := GetSymbol('SDL_GamepadHasButton');
  Pointer(SDL_GetGamepadButton)             := GetSymbol('SDL_GetGamepadButton');
  Pointer(SDL_GetGamepadButtonLabelForType) := GetSymbol('SDL_GetGamepadButtonLabelForType');
  Pointer(SDL_GetGamepadButtonLabel)        := GetSymbol('SDL_GetGamepadButtonLabel');

  Pointer(SDL_RumbleGamepad)                := GetSymbol('SDL_RumbleGamepad');
  Pointer(SDL_RumbleGamepadTriggers)        := GetSymbol('SDL_RumbleGamepadTriggers');
  Pointer(SDL_SetGamepadLED)                := GetSymbol('SDL_SetGamepadLED');
  Pointer(SDL_SendGamepadEffect)            := GetSymbol('SDL_SendGamepadEffect');

  Pointer(SDL_AddGamepadMapping)            := GetSymbol('SDL_AddGamepadMapping');
  Pointer(SDL_AddGamepadMappingsFromIO)     := GetSymbol('SDL_AddGamepadMappingsFromIO');
  Pointer(SDL_AddGamepadMappingsFromFile)   := GetSymbol('SDL_AddGamepadMappingsFromFile');
  Pointer(SDL_ReloadGamepadMappings)        := GetSymbol('SDL_ReloadGamepadMappings');
  Pointer(SDL_GetGamepadMappings)           := GetSymbol('SDL_GetGamepadMappings');
//  Pointer(SDL_GetGamepadMappingForGUID)     := GetSymbol('SDL_GetGamepadMappingForGUID');
  Pointer(SDL_GetGamepadMapping)            := GetSymbol('SDL_GetGamepadMapping');
  Pointer(SDL_SetGamepadMapping)            := GetSymbol('SDL_SetGamepadMapping');

  Pointer(SDL_GetGamepadTypeFromString)     := GetSymbol('SDL_GetGamepadTypeFromString');
  Pointer(SDL_GetGamepadStringForType)      := GetSymbol('SDL_GetGamepadStringForType');
  Pointer(SDL_GetGamepadAxisFromString)     := GetSymbol('SDL_GetGamepadAxisFromString');
  Pointer(SDL_GetGamepadStringForAxis)      := GetSymbol('SDL_GetGamepadStringForAxis');
  Pointer(SDL_GetGamepadButtonFromString)   := GetSymbol('SDL_GetGamepadButtonFromString');
  Pointer(SDL_GetGamepadStringForButton)    := GetSymbol('SDL_GetGamepadStringForButton');

// SDL_surface.h
  Pointer(SDL_CreateSurfaceFrom)            := GetSymbol('SDL_CreateSurfaceFrom');
  Pointer(SDL_ConvertSurface)               := GetSymbol('SDL_ConvertSurface');
  Pointer(SDL_ConvertPixels)                := GetSymbol('SDL_ConvertPixels');
  Pointer(SDL_DestroySurface)               := GetSymbol('SDL_DestroySurface');

// SDL_video.h
  Pointer(SDL_GetNumVideoDrivers)           := GetSymbol('SDL_GetNumVideoDrivers');
  Pointer(SDL_GetVideoDriver)               := GetSymbol('SDL_GetVideoDriver');
  Pointer(SDL_GetCurrentVideoDriver)        := GetSymbol('SDL_GetCurrentVideoDriver');
  Pointer(SDL_GetSystemTheme)               := GetSymbol('SDL_GetSystemTheme');

  Pointer(SDL_GetDisplays)                  := GetSymbol('SDL_GetDisplays');
  Pointer(SDL_GetPrimaryDisplay)            := GetSymbol('SDL_GetPrimaryDisplay');
  Pointer(SDL_GetDisplayProperties)         := GetSymbol('SDL_GetDisplayProperties');
  Pointer(SDL_GetDisplayName)               := GetSymbol('SDL_GetDisplayName');
  Pointer(SDL_GetDisplayBounds)             := GetSymbol('SDL_GetDisplayBounds');
  Pointer(SDL_GetDisplayUsableBounds)       := GetSymbol('SDL_GetDisplayUsableBounds');
  Pointer(SDL_GetNaturalDisplayOrientation) := GetSymbol('SDL_GetNaturalDisplayOrientation');
  Pointer(SDL_GetCurrentDisplayOrientation) := GetSymbol('SDL_GetCurrentDisplayOrientation');
  Pointer(SDL_GetDisplayContentScale)       := GetSymbol('SDL_GetDisplayContentScale');
  Pointer(SDL_GetFullscreenDisplayModes)    := GetSymbol('SDL_GetFullscreenDisplayModes');
  Pointer(SDL_GetClosestFullscreenDisplayMode) := GetSymbol('SDL_GetClosestFullscreenDisplayMode');
  Pointer(SDL_GetDesktopDisplayMode)        := GetSymbol('SDL_GetDesktopDisplayMode');
  Pointer(SDL_GetCurrentDisplayMode)        := GetSymbol('SDL_GetCurrentDisplayMode');
  Pointer(SDL_GetDisplayForPoint)           := GetSymbol('SDL_GetDisplayForPoint');
  Pointer(SDL_GetDisplayForRect)            := GetSymbol('SDL_GetDisplayForRect');
  Pointer(SDL_GetDisplayForWindow)          := GetSymbol('SDL_GetDisplayForWindow');

  Pointer(SDL_GetWindowPixelDensity)        := GetSymbol('SDL_GetWindowPixelDensity');
  Pointer(SDL_GetWindowDisplayScale)        := GetSymbol('SDL_GetWindowDisplayScale');
  Pointer(SDL_SetWindowFullscreenMode)      := GetSymbol('SDL_SetWindowFullscreenMode');
  Pointer(SDL_GetWindowFullscreenMode)      := GetSymbol('SDL_GetWindowFullscreenMode');

  Pointer(SDL_GetWindowICCProfile)          := GetSymbol('SDL_GetWindowICCProfile');
  Pointer(SDL_GetWindowPixelFormat)         := GetSymbol('SDL_GetWindowPixelFormat');
  Pointer(SDL_GetWindows)                   := GetSymbol('SDL_GetWindows');

  Pointer(SDL_CreateWindow)                 := GetSymbol('SDL_CreateWindow');
  Pointer(SDL_CreatePopupWindow)            := GetSymbol('SDL_CreatePopupWindow');
  Pointer(SDL_CreateWindowWithProperties)   := GetSymbol('SDL_CreateWindowWithProperties');

  Pointer(SDL_GetWindowID)                  := GetSymbol('SDL_GetWindowID');
  Pointer(SDL_GetWindowFromID)              := GetSymbol('SDL_GetWindowFromID');
  Pointer(SDL_GetWindowParent)              := GetSymbol('SDL_GetWindowParent');
  Pointer(SDL_GetWindowProperties)          := GetSymbol('SDL_GetWindowProperties');
  Pointer(SDL_GetWindowFlags)               := GetSymbol('SDL_GetWindowFlags');

  Pointer(SDL_SetWindowTitle)               := GetSymbol('SDL_SetWindowTitle');
  Pointer(SDL_GetWindowTitle)               := GetSymbol('SDL_GetWindowTitle');
  Pointer(SDL_SetWindowIcon)                := GetSymbol('SDL_SetWindowIcon');

  Pointer(SDL_SetWindowPosition)            := GetSymbol('SDL_SetWindowPosition');
  Pointer(SDL_GetWindowPosition)            := GetSymbol('SDL_GetWindowPosition');

  Pointer(SDL_SetWindowSize)                := GetSymbol('SDL_SetWindowSize');
  Pointer(SDL_GetWindowSize)                := GetSymbol('SDL_GetWindowSize');
  Pointer(SDL_GetWindowSafeArea)            := GetSymbol('SDL_GetWindowSafeArea');

  Pointer(SDL_SetWindowAspectRatio)         := GetSymbol('SDL_SetWindowAspectRatio');
  Pointer(SDL_GetWindowAspectRatio)         := GetSymbol('SDL_GetWindowAspectRatio');

  Pointer(SDL_GetWindowBordersSize)         := GetSymbol('SDL_GetWindowBordersSize');
  Pointer(SDL_GetWindowSizeInPixels)        := GetSymbol('SDL_GetWindowSizeInPixels');

  Pointer(SDL_SetWindowMinimumSize)         := GetSymbol('SDL_SetWindowMinimumSize');
  Pointer(SDL_GetWindowMinimumSize)         := GetSymbol('SDL_GetWindowMinimumSize');
  Pointer(SDL_SetWindowMaximumSize)         := GetSymbol('SDL_SetWindowMaximumSize');
  Pointer(SDL_GetWindowMaximumSize)         := GetSymbol('SDL_GetWindowMaximumSize');

  Pointer(SDL_SetWindowBordered)            := GetSymbol('SDL_SetWindowBordered');
  Pointer(SDL_SetWindowResizable)           := GetSymbol('SDL_SetWindowResizable');
  Pointer(SDL_SetWindowAlwaysOnTop)         := GetSymbol('SDL_SetWindowAlwaysOnTop');

  Pointer(SDL_ShowWindow)                   := GetSymbol('SDL_ShowWindow');
  Pointer(SDL_HideWindow)                   := GetSymbol('SDL_HideWindow');
  Pointer(SDL_RaiseWindow)                  := GetSymbol('SDL_RaiseWindow');
  Pointer(SDL_MaximizeWindow)               := GetSymbol('SDL_MaximizeWindow');
  Pointer(SDL_MinimizeWindow)               := GetSymbol('SDL_MinimizeWindow');
  Pointer(SDL_RestoreWindow)                := GetSymbol('SDL_RestoreWindow');

  Pointer(SDL_SetWindowFullscreen)          := GetSymbol('SDL_SetWindowFullscreen');
  Pointer(SDL_SyncWindow)                   := GetSymbol('SDL_SyncWindow');

  Pointer(SDL_WindowHasSurface)             := GetSymbol('SDL_WindowHasSurface');
  Pointer(SDL_GetWindowSurface)             := GetSymbol('SDL_GetWindowSurface');

  Pointer(SDL_SetWindowSurfaceVSync)        := GetSymbol('SDL_SetWindowSurfaceVSync');
  Pointer(SDL_GetWindowSurfaceVSync)        := GetSymbol('SDL_GetWindowSurfaceVSync');

  Pointer(SDL_UpdateWindowSurface)          := GetSymbol('SDL_UpdateWindowSurface');
  Pointer(SDL_UpdateWindowSurfaceRects)     := GetSymbol('SDL_UpdateWindowSurfaceRects');
  Pointer(SDL_DestroyWindowSurface)         := GetSymbol('SDL_DestroyWindowSurface');

  Pointer(SDL_SetWindowKeyboardGrab)        := GetSymbol('SDL_SetWindowKeyboardGrab');
  Pointer(SDL_SetWindowMouseGrab)           := GetSymbol('SDL_SetWindowMouseGrab');
  Pointer(SDL_GetWindowKeyboardGrab)        := GetSymbol('SDL_GetWindowKeyboardGrab');
  Pointer(SDL_GetWindowMouseGrab)           := GetSymbol('SDL_GetWindowMouseGrab');
  Pointer(SDL_GetGrabbedWindow)             := GetSymbol('SDL_GetGrabbedWindow');

  Pointer(SDL_SetWindowMouseRect)           := GetSymbol('SDL_SetWindowMouseRect');
  Pointer(SDL_GetWindowMouseRect)           := GetSymbol('SDL_GetWindowMouseRect');

  Pointer(SDL_SetWindowOpacity)             := GetSymbol('SDL_SetWindowOpacity');
  Pointer(SDL_GetWindowOpacity)             := GetSymbol('SDL_GetWindowOpacity');

  Pointer(SDL_SetWindowParent)              := GetSymbol('SDL_SetWindowParent');
  Pointer(SDL_SetWindowModal)               := GetSymbol('SDL_SetWindowModal');
  Pointer(SDL_SetWindowFocusable)           := GetSymbol('SDL_SetWindowFocusable');

  Pointer(SDL_ShowWindowSystemMenu)         := GetSymbol('SDL_ShowWindowSystemMenu');

  Pointer(SDL_SetWindowHitTest)             := GetSymbol('SDL_SetWindowHitTest');

  Pointer(SDL_SetWindowShape)               := GetSymbol('SDL_SetWindowShape');
  Pointer(SDL_FlashWindow)                  := GetSymbol('SDL_FlashWindow');

  Pointer(SDL_DestroyWindow)                := GetSymbol('SDL_DestroyWindow');

  Pointer(SDL_ScreenSaverEnabled)           := GetSymbol('SDL_ScreenSaverEnabled');
  Pointer(SDL_EnableScreenSaver)            := GetSymbol('SDL_EnableScreenSaver');
  Pointer(SDL_DisableScreenSaver)           := GetSymbol('SDL_DisableScreenSaver');

  Pointer(SDL_GL_LoadLibrary)               := GetSymbol('SDL_GL_LoadLibrary');
  Pointer(SDL_GL_GetProcAddress)            := GetSymbol('SDL_GL_GetProcAddress');
  Pointer(SDL_EGL_GetProcAddress)           := GetSymbol('SDL_EGL_GetProcAddress');
  Pointer(SDL_GL_UnloadLibrary)             := GetSymbol('SDL_GL_UnloadLibrary');

  Pointer(SDL_GL_ExtensionSupported)        := GetSymbol('SDL_GL_ExtensionSupported');
  Pointer(SDL_GL_ResetAttributes)           := GetSymbol('SDL_GL_ResetAttributes');
  Pointer(SDL_GL_SetAttribute)              := GetSymbol('SDL_GL_SetAttribute');
  Pointer(SDL_GL_GetAttribute)              := GetSymbol('SDL_GL_GetAttribute');

  Pointer(SDL_GL_CreateContext)             := GetSymbol('SDL_GL_CreateContext');
  Pointer(SDL_GL_MakeCurrent)               := GetSymbol('SDL_GL_MakeCurrent');
  Pointer(SDL_GL_GetCurrentWindow)          := GetSymbol('SDL_GL_GetCurrentWindow');
  Pointer(SDL_GL_GetCurrentContext)         := GetSymbol('SDL_GL_GetCurrentContext');

  Pointer(SDL_GL_SetSwapInterval)           := GetSymbol('SDL_GL_SetSwapInterval');
  Pointer(SDL_GL_GetSwapInterval)           := GetSymbol('SDL_GL_GetSwapInterval');
  Pointer(SDL_GL_SwapWindow)                := GetSymbol('SDL_GL_SwapWindow');
  Pointer(SDL_GL_DestroyContext)            := GetSymbol('SDL_GL_DestroyContext');

// SDL_timer.h
  Pointer(SDL_GetTicks)                := GetSymbol('SDL_GetTicks');
  Pointer(SDL_GetTicksNS)              := GetSymbol('SDL_GetTicksNS');
  Pointer(SDL_GetPerformanceCounter)   := GetSymbol('SDL_GetPerformanceCounter');
  Pointer(SDL_GetPerformanceFrequency) := GetSymbol('SDL_GetPerformanceFrequency');
  Pointer(SDL_Delay)                   := GetSymbol('SDL_Delay');
  Pointer(SDL_DelayNS)                 := GetSymbol('SDL_DelayNS');
  Pointer(SDL_DelayPrecise)            := GetSymbol('SDL_DelayPrecise');
  Pointer(SDL_AddTimer)                := GetSymbol('SDL_AddTimer');
  Pointer(SDL_AddTimerNS)              := GetSymbol('SDL_AddTimerNS');
  Pointer(SDL_RemoveTimer)             := GetSymbol('SDL_RemoveTimer');

// SDL_events.h
  Pointer(SDL_PumpEvents)         := GetSymbol('SDL_PumpEvents');
  Pointer(SDL_PeepEvents)         := GetSymbol('SDL_PeepEvents');
  Pointer(SDL_HasEvent)           := GetSymbol('SDL_HasEvent');
  Pointer(SDL_HasEvents)          := GetSymbol('SDL_HasEvents');
  Pointer(SDL_FlushEvent)         := GetSymbol('SDL_FlushEvent');
  Pointer(SDL_FlushEvents)        := GetSymbol('SDL_FlushEvents');
  Pointer(SDL_PollEvent)          := GetSymbol('SDL_PollEvent');
  Pointer(SDL_WaitEvent)          := GetSymbol('SDL_WaitEvent');
  Pointer(SDL_WaitEventTimeout)   := GetSymbol('SDL_WaitEventTimeout');
  Pointer(SDL_PushEvent)          := GetSymbol('SDL_PushEvent');
  Pointer(SDL_SetEventFilter)     := GetSymbol('SDL_SetEventFilter');
  Pointer(SDL_GetEventFilter)     := GetSymbol('SDL_GetEventFilter');
  Pointer(SDL_AddEventWatch)      := GetSymbol('SDL_AddEventWatch');
  Pointer(SDL_RemoveEventWatch)   := GetSymbol('SDL_RemoveEventWatch');
  Pointer(SDL_FilterEvents)       := GetSymbol('SDL_FilterEvents');
  Pointer(SDL_SetEventEnabled)    := GetSymbol('SDL_SetEventEnabled');
  Pointer(SDL_EventEnabled)       := GetSymbol('SDL_EventEnabled');
  Pointer(SDL_RegisterEvents)     := GetSymbol('SDL_RegisterEvents');
  Pointer(SDL_GetWindowFromEvent) := GetSymbol('SDL_GetWindowFromEvent');

// SDL_iostream.h
  Pointer(SDL_IOFromFile)        := GetSymbol('SDL_IOFromFile');
  Pointer(SDL_IOFromMem)         := GetSymbol('SDL_IOFromMem');
  Pointer(SDL_IOFromConstMem)    := GetSymbol('SDL_IOFromConstMem');
  Pointer(SDL_IOFromDynamicMem)  := GetSymbol('SDL_IOFromDynamicMem');

  Pointer(SDL_OpenIO)            := GetSymbol('SDL_OpenIO');
  Pointer(SDL_CloseIO)           := GetSymbol('SDL_CloseIO');

  Pointer(SDL_LoadFile_IO)       := GetSymbol('SDL_LoadFile_IO');
  Pointer(SDL_LoadFile)          := GetSymbol('SDL_LoadFile');
  Pointer(SDL_SaveFile_IO)       := GetSymbol('SDL_SaveFile_IO');
  Pointer(SDL_SaveFile)          := GetSymbol('SDL_SaveFile');

  Pointer(SDL_GetBooleanProperty):= GetSymbol('SDL_GetBooleanProperty');
  Pointer(SDL_SetNumberProperty) := GetSymbol('SDL_SetNumberProperty');
  Pointer(SDL_CreateProperties)  := GetSymbol('SDL_CreateProperties');
  Pointer(SDL_DestroyProperties) := GetSymbol('SDL_DestroyProperties');

  Pointer(SDL_free)              := GetSymbol('SDL_free');

  Exit( True );
end;

type PSDLStreamUser = ^TSDLStreamUser;
     TSDLStreamUser = record
       Stream     : TStream;
       BaseOffset : Int64;
       Size       : Int64;
       OwnsStream : SDL_bool;
       ReadOnly   : SDL_bool;
     end;

function IO_Stream_Size( userdata: Pointer ): Sint64; cdecl;
var U : PSDLStreamUser;
begin
  U := PSDLStreamUser(userdata);
  try
    if U^.Size >= 0 then
      Exit(U^.Size)
    else
      Exit(U^.Stream.Size - U^.BaseOffset);
  except
    Exit(-1);
  end;
end;

function IO_Stream_Seek(userdata: Pointer; offset: Sint64; whence: SDL_IOWhence): Sint64; cdecl;
var
  U        : PSDLStreamUser;
  Target   : Int64;
  NewPos   : Int64;
begin
  U := PSDLStreamUser(userdata);
  try
    case whence of
      SDL_IO_SEEK_SET: Target := U^.BaseOffset + offset;
      SDL_IO_SEEK_CUR: Target := U^.Stream.Position + offset;
      SDL_IO_SEEK_END: Target := U^.BaseOffset + U^.Size + offset;
    else
      Exit(-1);
    end;

    if Target < 0 then
      Target := 0;

    NewPos := U^.Stream.Seek(Target, soBeginning);
    Exit(NewPos - U^.BaseOffset);
  except
    Exit(-1);
  end;
end;

function IO_Stream_Read(userdata: Pointer; ptr: Pointer; size: size_t; status: PSDL_IOStatus): size_t; cdecl;
var
  U         : PSDLStreamUser;
  Remaining : Int64;
  ToRead    : size_t;
  Got       : size_t;
begin
  U := PSDLStreamUser(userdata);
  try
    Remaining := (U^.BaseOffset + U^.Size) - U^.Stream.Position;
    if Remaining <= 0 then
    begin
      if status <> nil then status^ := SDL_IO_STATUS_EOF;
      Exit(0);
    end;

    if size > size_t(Remaining) then
      ToRead := size_t(Remaining)
    else
      ToRead := size;

    Got := U^.Stream.Read(ptr^, ToRead);

    if (Got = 0) or (Got < ToRead) then
    begin
      if U^.Stream.Position >= (U^.BaseOffset + U^.Size) then
      begin
        if status <> nil then status^ := SDL_IO_STATUS_EOF;
      end
      else
      begin
        if status <> nil then status^ := SDL_IO_STATUS_ERROR;
      end;
    end;

    Exit(Got);
  except
    if status <> nil then status^ := SDL_IO_STATUS_ERROR;
    Exit(0);
  end;
end;

function IO_Stream_Write(userdata: Pointer; const ptr: Pointer; size: size_t; status: PSDL_IOStatus): size_t; cdecl;
var
  U         : PSDLStreamUser;
  Remaining : Int64;
  ToWrite   : size_t;
  Put       : size_t;
begin
  U := PSDLStreamUser(userdata);
  try
    if U^.ReadOnly then
    begin
      if status <> nil then status^ := SDL_IO_STATUS_READONLY;
      Exit(0);
    end;

    Remaining := (U^.BaseOffset + U^.Size) - U^.Stream.Position;
    if Remaining <= 0 then
    begin
      if status <> nil then status^ := SDL_IO_STATUS_EOF;
      Exit(0);
    end;

    if size > size_t(Remaining) then
      ToWrite := size_t(Remaining)
    else
      ToWrite := size;

    Put := U^.Stream.Write(ptr^, ToWrite);

    if (Put = 0) or (Put < ToWrite) then
    begin
      if U^.Stream.Position >= (U^.BaseOffset + U^.Size) then
      begin
        if status <> nil then status^ := SDL_IO_STATUS_EOF;
      end
      else
      begin
        if status <> nil then status^ := SDL_IO_STATUS_ERROR;
      end;
    end;

    Exit(Put);
  except
    if status <> nil then status^ := SDL_IO_STATUS_ERROR;
    Exit(0);
  end;
end;

function IO_Stream_Flush(userdata: Pointer; status: PSDL_IOStatus): SDL_bool; cdecl;
begin
  Result := True;
end;

function IO_Stream_Close(userdata: Pointer): SDL_bool; cdecl;
var
  U: PSDLStreamUser;
begin
  try
    U := PSDLStreamUser(userdata);
    if U <> nil then
    begin
      if U^.OwnsStream and (U^.Stream <> nil) then
        U^.Stream.Free;
      FreeMem(U);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function SDL_IOFromStream(aStream: TStream; aSize: Int64; aOwnsStream: SDL_bool; aReadOnly: SDL_bool = False): PSDL_IOStream;
var
  Iface : SDL_IOStreamInterface;
  U     : PSDLStreamUser;
begin
  Result := nil;
  if (aStream = nil) or (SDL_OpenIO = nil) then Exit;

  GetMem(U, SizeOf(TSDLStreamUser));
  U^.Stream     := aStream;
  U^.BaseOffset := aStream.Position;
  if aSize >= 0 then
    U^.Size := aSize
  else
    U^.Size := aStream.Size - U^.BaseOffset;
  U^.OwnsStream := aOwnsStream;
  U^.ReadOnly   := aReadOnly;

  FillChar(Iface, 0, SizeOf(Iface));
  Iface.version := SizeOf(SDL_IOStreamInterface);
  Iface.size    := @IO_Stream_Size;
  Iface.seek    := @IO_Stream_Seek;
  Iface.read    := @IO_Stream_Read;
  Iface.write   := @IO_Stream_Write;
  Iface.flush   := @IO_Stream_Flush;
  Iface.close   := @IO_Stream_Close;

  Result := SDL_OpenIO(@Iface, U);
  if Result = nil then
  begin
    FreeMem(U);
  end;
end;

function SDL_BUTTON_MASK( button: Integer ) : UInt32; inline;
begin
  Result := 1 shl (button - 1);
end;

function SDL_WINDOWPOS_UNDEFINED_DISPLAY( x: Integer ): Integer; inline;
begin
  Result := SDL_WINDOWPOS_UNDEFINED_MASK or x;
end;

function SDL_WINDOWPOS_CENTERED_DISPLAY( x: Integer ): Integer; inline;
begin
  Result := SDL_WINDOWPOS_CENTERED_MASK or x;
end;

finalization
  Assert( SizeOf(SDL_Event) = 128 );

  if SDL3 <> nil then FreeAndNil( SDL3 );

end.

