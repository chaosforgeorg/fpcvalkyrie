type
  UInt8       = Byte;
  SInt8       = ShortInt;
  UInt16      = Word;
  SInt16      = SmallInt;
  UInt32      = LongWord;
  SInt32      = LongInt;
  UInt64      = QWord;
  SInt64      = Int64;
  SDL_bool    = Boolean;
  PSDL_bool   = ^SDL_bool;
  SizeT       = ptruint;
  PSizeT      = ^SizeT;

  PUInt8      = ^UInt8;
  PSInt8      = ^SInt8;
  PUInt16     = ^UInt16;
  PSInt16     = ^SInt16;
  PUInt32     = ^UInt32;
  PSInt32     = ^SInt32;
  PUInt64     = ^UInt64;
  PSInt64     = ^SInt64;

  SDL_PropertiesID = Uint32;
  SDL_JoystickID   = Sint32;
  PSDL_JoystickID  = ^SDL_JoystickID;
  SDL_DisplayID    = UInt32;
  PSDL_DisplayID   = ^SDL_DisplayID;
  SDL_WindowID     = UInt32;

type PSDL_Window  = ^SDL_Window;
     PPSDL_Window = ^PSDL_Window;
     SDL_Window   = record end;

  SDL_Rect = record
    x, y, w, h: Sint32;
  end;
  PSDL_Rect = ^SDL_Rect;

  SDL_Point = record
    x, y : Sint32;
  end;
  PSDL_Point = ^SDL_Point;

{ SDL_init.h }

type SDL_AppResult = (
  SDL_APP_CONTINUE = 0,
  SDL_APP_SUCCESS  = 1,
  SDL_APP_FAILURE  = 2
);

type SDL_InitFlags = Uint32;

const
  SDL_PROP_APP_METADATA_NAME_STRING        = 'SDL.app.metadata.name';
  SDL_PROP_APP_METADATA_VERSION_STRING     = 'SDL.app.metadata.version';
  SDL_PROP_APP_METADATA_IDENTIFIER_STRING  = 'SDL.app.metadata.identifier';
  SDL_PROP_APP_METADATA_CREATOR_STRING     = 'SDL.app.metadata.creator';
  SDL_PROP_APP_METADATA_COPYRIGHT_STRING   = 'SDL.app.metadata.copyright';
  SDL_PROP_APP_METADATA_URL_STRING         = 'SDL.app.metadata.url';
  SDL_PROP_APP_METADATA_TYPE_STRING        = 'SDL.app.metadata.type';

{ SDL_keycode.h }

type
  SDL_Keycode = Uint32;
  SDL_Keymod  = Uint16;
  PSDL_Keymod = ^SDL_Keymod;

{ SDL_scancode.h }

type
  SDL_Scancode = LongInt;


{ SDL_keyboard.h }
  
type
  SDL_KeyboardID  = Uint32;
  PSDL_KeyboardID = ^SDL_KeyboardID;

  SDL_TextInputType = (
    SDL_TEXTINPUT_TYPE_TEXT,
    SDL_TEXTINPUT_TYPE_TEXT_NAME,
    SDL_TEXTINPUT_TYPE_TEXT_EMAIL,
    SDL_TEXTINPUT_TYPE_TEXT_USERNAME,
    SDL_TEXTINPUT_TYPE_TEXT_PASSWORD_HIDDEN,
    SDL_TEXTINPUT_TYPE_TEXT_PASSWORD_VISIBLE,
    SDL_TEXTINPUT_TYPE_NUMBER,
    SDL_TEXTINPUT_TYPE_NUMBER_PASSWORD_HIDDEN,
    SDL_TEXTINPUT_TYPE_NUMBER_PASSWORD_VISIBLE
  );

  SDL_Capitalization = (
    SDL_CAPITALIZE_NONE,
    SDL_CAPITALIZE_SENTENCES,
    SDL_CAPITALIZE_WORDS,
    SDL_CAPITALIZE_LETTERS
  );

{ SDL_mouse.h }

type
  SDL_MouseID           = Uint32;
  PSDL_MouseID          = ^SDL_MouseID;

  SDL_MouseButtonFlags  = Uint32;

  SDL_Cursor            = Pointer;
  PSDL_Cursor           = ^SDL_Cursor;

type SDL_SystemCursor = (
  SDL_SYSTEM_CURSOR_DEFAULT,
  SDL_SYSTEM_CURSOR_TEXT,
  SDL_SYSTEM_CURSOR_WAIT,
  SDL_SYSTEM_CURSOR_CROSSHAIR,
  SDL_SYSTEM_CURSOR_PROGRESS,
  SDL_SYSTEM_CURSOR_NWSE_RESIZE,
  SDL_SYSTEM_CURSOR_NESW_RESIZE,
  SDL_SYSTEM_CURSOR_EW_RESIZE,
  SDL_SYSTEM_CURSOR_NS_RESIZE,
  SDL_SYSTEM_CURSOR_MOVE,
  SDL_SYSTEM_CURSOR_NOT_ALLOWED,
  SDL_SYSTEM_CURSOR_POINTER,
  SDL_SYSTEM_CURSOR_NW_RESIZE,
  SDL_SYSTEM_CURSOR_N_RESIZE,
  SDL_SYSTEM_CURSOR_NE_RESIZE,
  SDL_SYSTEM_CURSOR_E_RESIZE,
  SDL_SYSTEM_CURSOR_SE_RESIZE,
  SDL_SYSTEM_CURSOR_S_RESIZE,
  SDL_SYSTEM_CURSOR_SW_RESIZE,
  SDL_SYSTEM_CURSOR_W_RESIZE,
  SDL_SYSTEM_CURSOR_COUNT
);

type SDL_MouseWheelDirection = (
  SDL_MOUSEWHEEL_NORMAL,
  SDL_MOUSEWHEEL_FLIPPED
);

type PSDL_Gamepad = Pointer;
type SDL_GamepadType = (
  SDL_GAMEPAD_TYPE_UNKNOWN                = 0,
  SDL_GAMEPAD_TYPE_STANDARD               = 1,
  SDL_GAMEPAD_TYPE_XBOX360                = 2,
  SDL_GAMEPAD_TYPE_XBOXONE                = 3,
  SDL_GAMEPAD_TYPE_PS3                    = 4,
  SDL_GAMEPAD_TYPE_PS4                    = 5,
  SDL_GAMEPAD_TYPE_PS5                    = 6,
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_PRO    = 7,
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_LEFT  = 8,
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_RIGHT = 9,
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_PAIR  = 10,
  SDL_GAMEPAD_TYPE_COUNT                  = 11
);

type SDL_GamepadButton = (
  SDL_GAMEPAD_BUTTON_INVALID              = -1,
  SDL_GAMEPAD_BUTTON_SOUTH                = 0,
  SDL_GAMEPAD_BUTTON_EAST                 = 1,
  SDL_GAMEPAD_BUTTON_WEST                 = 2,
  SDL_GAMEPAD_BUTTON_NORTH                = 3,
  SDL_GAMEPAD_BUTTON_BACK                 = 4,
  SDL_GAMEPAD_BUTTON_GUIDE                = 5,
  SDL_GAMEPAD_BUTTON_START                = 6,
  SDL_GAMEPAD_BUTTON_LEFT_STICK           = 7,
  SDL_GAMEPAD_BUTTON_RIGHT_STICK          = 8,
  SDL_GAMEPAD_BUTTON_LEFT_SHOULDER        = 9,
  SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER       = 10,
  SDL_GAMEPAD_BUTTON_DPAD_UP              = 11,
  SDL_GAMEPAD_BUTTON_DPAD_DOWN            = 12,
  SDL_GAMEPAD_BUTTON_DPAD_LEFT            = 13,
  SDL_GAMEPAD_BUTTON_DPAD_RIGHT           = 14,
  SDL_GAMEPAD_BUTTON_MISC1                = 15,
  SDL_GAMEPAD_BUTTON_RIGHT_PADDLE1        = 16,
  SDL_GAMEPAD_BUTTON_LEFT_PADDLE1         = 17,
  SDL_GAMEPAD_BUTTON_RIGHT_PADDLE2        = 18,
  SDL_GAMEPAD_BUTTON_LEFT_PADDLE2         = 19,
  SDL_GAMEPAD_BUTTON_TOUCHPAD             = 20,
  SDL_GAMEPAD_BUTTON_MISC2                = 21,
  SDL_GAMEPAD_BUTTON_MISC3                = 22,
  SDL_GAMEPAD_BUTTON_MISC4                = 23,
  SDL_GAMEPAD_BUTTON_MISC5                = 24,
  SDL_GAMEPAD_BUTTON_MISC6                = 25,
  SDL_GAMEPAD_BUTTON_COUNT                = 26
);

type SDL_GamepadAxis = (
  SDL_GAMEPAD_AXIS_INVALID                = -1,
  SDL_GAMEPAD_AXIS_LEFTX                  = 0,
  SDL_GAMEPAD_AXIS_LEFTY                  = 1,
  SDL_GAMEPAD_AXIS_RIGHTX                 = 2,
  SDL_GAMEPAD_AXIS_RIGHTY                 = 3,
  SDL_GAMEPAD_AXIS_LEFT_TRIGGER           = 4,
  SDL_GAMEPAD_AXIS_RIGHT_TRIGGER          = 5,
  SDL_GAMEPAD_AXIS_COUNT                  = 6
);

type SDL_GamepadButtonLabel = (
  SDL_GAMEPAD_BUTTON_LABEL_UNKNOWN        = 0,
  SDL_GAMEPAD_BUTTON_LABEL_A              = 1,
  SDL_GAMEPAD_BUTTON_LABEL_B              = 2,
  SDL_GAMEPAD_BUTTON_LABEL_X              = 3,
  SDL_GAMEPAD_BUTTON_LABEL_Y              = 4,
  SDL_GAMEPAD_BUTTON_LABEL_CROSS          = 5,
  SDL_GAMEPAD_BUTTON_LABEL_CIRCLE         = 6,
  SDL_GAMEPAD_BUTTON_LABEL_SQUARE         = 7,
  SDL_GAMEPAD_BUTTON_LABEL_TRIANGLE       = 8
);

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ELSE}
  {$MINENUMSIZE 4}
  {$ALIGN 8}
{$ENDIF}

type
// Common header used by all events
SDL_CommonEvent = record
  _type: UInt32;    // Uint32: not limited to SDL_EventType
  reserved: UInt32;
  timestamp: UInt64; // ns
end;

// Display
SDL_DisplayEvent = record
  _type: UInt32; // SDL_EventType
  reserved: UInt32;
  timestamp: UInt64;
  displayID: SDL_DisplayID;
  data1: Int32;
  data2: Int32;
end;

// Window
SDL_WindowEvent = record
  _type: UInt32; // SDL_EventType
  reserved: UInt32;
  timestamp: UInt64;
  windowID: SDL_WindowID;
  data1: Int32;
  data2: Int32;
end;

// Keyboard device add/remove
SDL_KeyboardDeviceEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: SDL_KeyboardID;
end;

// Keyboard key down/up
SDL_KeyboardEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: SDL_WindowID;
  which: SDL_KeyboardID;
  scancode: SDL_Scancode;
  key: SDL_Keycode;
  mod_: SDL_Keymod;
  raw: UInt16;          // platform scancode
  down: SDL_Bool;        // pressed?
  repeat_: SDL_Bool;     // key repeat?
end;

// Text editing (IME composition)
SDL_TextEditingEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  text: PAnsiChar; // UTF-8 (const char*)
  start: Int32;    // -1 if not set
  length_: Int32;  // -1 if not set
end;

// Text editing candidates (IME)
SDL_TextEditingCandidatesEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  candidates: ^PAnsiChar; // const char * const *
  num_candidates: Int32;
  selected_candidate: Int32; // -1 if none
  horizontal: SDL_Bool;       // true=horizontal list
  padding1: UInt8;
  padding2: UInt8;
  padding3: UInt8;
end;

// Text input
SDL_TextInputEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  text: PAnsiChar; // UTF-8
end;

// Mouse device add/remove
SDL_MouseDeviceEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: SDL_MouseID;
end;

// Mouse motion
SDL_MouseMotionEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: SDL_MouseID;
  state: SDL_MouseButtonFlags;
  x: Single; y: Single;     // window-relative
  xrel: Single; yrel: Single;
end;

// Mouse button
SDL_MouseButtonEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: SDL_MouseID;
  button: UInt8;     // 1..5
  down: SDL_bool;
  clicks: UInt8;     // 1=single, 2=double...
  padding: UInt8;
  x: Single;
  y: Single;
end;

// Mouse wheel
SDL_MouseWheelEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: SDL_MouseID;
  x: Single;         // horizontal amount
  y: Single;         // vertical amount
  direction: UInt32; // SDL_MouseWheelDirection
  mouse_x: Single;
  mouse_y: Single;
  integer_x: Int32;  // whole-tick accumulation (3.2.12+)
  integer_y: Int32;
end;

// Joystick axis
SDL_JoyAxisEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: SDL_JoystickID;
  axis: UInt8;
  padding1: UInt8;
  padding2: UInt8;
  padding3: UInt8;
  value: Int16;     // -32768..32767
  padding4: UInt16;
end;

// Joystick ball
SDL_JoyBallEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: Int32;
  ball: UInt8;
  padding1: UInt8;
  padding2: UInt8;
  padding3: UInt8;
  xrel: Int16;
  yrel: Int16;
end;

// Joystick hat
SDL_JoyHatEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: Int32;
  hat: UInt8;
  value: UInt8; // SDL_HAT_* value
  padding1: UInt8;
  padding2: UInt8;
end;

// Joystick button
SDL_JoyButtonEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: Int32;
  button: UInt8;
  down: SDL_bool;
  padding1: UInt8;
  padding2: UInt8;
end;

// Joystick device add/remove/update-complete
SDL_JoyDeviceEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: SDL_JoystickID;
end;

// Joystick battery
SDL_JoyBatteryEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: Int32;
  state: Int32;  // SDL_PowerState
  percent: Int32;
end;

// Gamepad axis
SDL_GamepadAxisEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: SDL_JoystickID;
  axis: UInt8;  // SDL_GamepadAxis
  padding1: UInt8;
  padding2: UInt8;
  padding3: UInt8;
  value: Int16; // -32768..32767 (triggers 0..32767)
  padding4: UInt16;
end;

// Gamepad button
SDL_GamepadButtonEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: SDL_JoystickID;
  button: UInt8; // SDL_GamepadButton
  down: SDL_Bool;
  padding1: UInt8;
  padding2: UInt8;
end;

// Gamepad device add/remove/remapped/update-complete/steam-handle
SDL_GamepadDeviceEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: SDL_JoystickID;
end;

// Gamepad touchpad
SDL_GamepadTouchpadEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: Int32;
  touchpad: Int32;
  finger: Int32;
  x: Single; // 0..1
  y: Single; // 0..1
  pressure: Single; // 0..1
end;

// Gamepad sensor
SDL_GamepadSensorEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: Int32;
  sensor: Int32;        // SDL_SensorType
  data: array[0..2] of Single; // up to 3 values
  sensor_timestamp: UInt64;    // ns (not necessarily system clock)
end;

// Audio device hotplug/format change
SDL_AudioDeviceEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: UInt32; // SDL_AudioDeviceID
  recording: SDL_Bool; // true=input, false=output
  padding1: UInt8;
  padding2: UInt8;
  padding3: UInt8;
end;

// Camera hotplug/approval/denial
SDL_CameraDeviceEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: UInt32; // SDL_CameraID
end;

// Renderer reset/lost
SDL_RenderEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32; // window containing renderer
end;

// Touch finger
SDL_TouchFingerEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  touchID: Int64;   // SDL_TouchID
  fingerID: Int64;  // SDL_FingerID
  x: Single; y: Single;      // 0..1
  dx: Single; dy: Single;    // -1..1
  pressure: Single;          // 0..1
  windowID: UInt32;
end;

// Pen proximity (in/out)
SDL_PenProximityEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: UInt32; // SDL_PenID
end;

// Pen motion
SDL_PenMotionEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: UInt32; // SDL_PenID
  pen_state: UInt32; // SDL_PenInputFlags
  x: Single;
  y: Single;
end;

// Pen touch (down/up)
SDL_PenTouchEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: UInt32; // SDL_PenID
  pen_state: UInt32; // SDL_PenInputFlags
  x: Single;
  y: Single;
  eraser: SDL_Bool;
  down: SDL_Bool;
end;

// Pen button
SDL_PenButtonEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: UInt32; // SDL_PenID
  pen_state: UInt32; // SDL_PenInputFlags
  x: Single;
  y: Single;
  button: UInt8; // first button is 1
  down: SDL_Bool;
end;

// Pen axis (pressure/tilt/etc)
SDL_PenAxisEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  which: UInt32; // SDL_PenID
  pen_state: UInt32; // SDL_PenInputFlags
  x: Single;
  y: Single;
  axis: UInt32;  // SDL_PenAxis
  value: Single;
end;

// Drag & drop
SDL_DropEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  x: Single;
  y: Single;
  source: PAnsiChar; // may be nil
  data: PAnsiChar;   // file path or text (or nil)
end;

// Clipboard changed
SDL_ClipboardEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  owner: SDL_Bool;          // internal change?
  num_mime_types: Int32;
  mime_types: ^PAnsiChar;  // array of mime strings
end;

// Sensor (generic)
SDL_SensorEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
  which: Int32;                // SDL_SensorID
  data: array[0..5] of Single; // up to 6 values
  sensor_timestamp: UInt64;    // ns
end;

// Quit
SDL_QuitEvent = record
  _type: UInt32;
  reserved: UInt32;
  timestamp: UInt64;
end;

// User event (custom)
SDL_UserEvent = record
  _type: UInt32;    // Uint32 (SDL_EVENT_USER..)
  reserved: UInt32;
  timestamp: UInt64;
  windowID: UInt32;
  code: Int32;
  data1: Pointer;
  data2: Pointer;
end;

// The big union
SDL_Event = record
  case Integer of
    0: ( _type: UInt32 );
    1: ( common: SDL_CommonEvent );
    2: ( display: SDL_DisplayEvent );
    3: ( window: SDL_WindowEvent );
    4: ( kdevice: SDL_KeyboardDeviceEvent );
    5: ( key: SDL_KeyboardEvent );
    6: ( edit: SDL_TextEditingEvent );
    7: ( edit_candidates: SDL_TextEditingCandidatesEvent );
    8: ( text: SDL_TextInputEvent );
    9: ( mdevice: SDL_MouseDeviceEvent );
    10: ( motion: SDL_MouseMotionEvent );
    11: ( button: SDL_MouseButtonEvent );
    12: ( wheel: SDL_MouseWheelEvent );
    13: ( jdevice: SDL_JoyDeviceEvent );
    14: ( jaxis: SDL_JoyAxisEvent );
    15: ( jball: SDL_JoyBallEvent );
    16: ( jhat: SDL_JoyHatEvent );
    17: ( jbutton: SDL_JoyButtonEvent );
    18: ( jbattery: SDL_JoyBatteryEvent );
    19: ( gdevice: SDL_GamepadDeviceEvent );
    20: ( gaxis: SDL_GamepadAxisEvent );
    21: ( gbutton: SDL_GamepadButtonEvent );
    22: ( gtouchpad: SDL_GamepadTouchpadEvent );
    23: ( gsensor: SDL_GamepadSensorEvent );
    24: ( adevice: SDL_AudioDeviceEvent );
    25: ( cdevice: SDL_CameraDeviceEvent );
    26: ( sensor: SDL_SensorEvent );
    27: ( quit: SDL_QuitEvent );
    28: ( user: SDL_UserEvent );
    29: ( tfinger: SDL_TouchFingerEvent );
    30: ( pproximity: SDL_PenProximityEvent );
    31: ( ptouch: SDL_PenTouchEvent );
    32: ( pmotion: SDL_PenMotionEvent );
    33: ( pbutton: SDL_PenButtonEvent );
    34: ( paxis: SDL_PenAxisEvent );
    35: ( render: SDL_RenderEvent );
    36: ( drop: SDL_DropEvent );
    37: ( clipboard: SDL_ClipboardEvent );
    99: ( padding: array[0..127] of UInt8 ); // ABI size = 128
end;

{ SDL_pixels.h }
const
  SDL_ALPHA_OPAQUE              = 255;
  SDL_ALPHA_OPAQUE_FLOAT        = 1.0;
  SDL_ALPHA_TRANSPARENT         = 0;
  SDL_ALPHA_TRANSPARENT_FLOAT   = 0.0;

type
SDL_PixelType = (
  SDL_PIXELTYPE_UNKNOWN = 0,
  SDL_PIXELTYPE_INDEX1,
  SDL_PIXELTYPE_INDEX4,
  SDL_PIXELTYPE_INDEX8,
  SDL_PIXELTYPE_PACKED8,
  SDL_PIXELTYPE_PACKED16,
  SDL_PIXELTYPE_PACKED32,
  SDL_PIXELTYPE_ARRAYU8,
  SDL_PIXELTYPE_ARRAYU16,
  SDL_PIXELTYPE_ARRAYU32,
  SDL_PIXELTYPE_ARRAYF16,
  SDL_PIXELTYPE_ARRAYF32,
  // sdl2-compat addendum:
  SDL_PIXELTYPE_INDEX2
);

SDL_BitmapOrder = (
  SDL_BITMAPORDER_NONE = 0,
  SDL_BITMAPORDER_4321,
  SDL_BITMAPORDER_1234
);

SDL_PackedOrder = (
  SDL_PACKEDORDER_NONE = 0,
  SDL_PACKEDORDER_XRGB,
  SDL_PACKEDORDER_RGBX,
  SDL_PACKEDORDER_ARGB,
  SDL_PACKEDORDER_RGBA,
  SDL_PACKEDORDER_XBGR,
  SDL_PACKEDORDER_BGRX,
  SDL_PACKEDORDER_ABGR,
  SDL_PACKEDORDER_BGRA
);

SDL_ArrayOrder = (
  SDL_ARRAYORDER_NONE = 0,
  SDL_ARRAYORDER_RGB,
  SDL_ARRAYORDER_RGBA,
  SDL_ARRAYORDER_ARGB,
  SDL_ARRAYORDER_BGR,
  SDL_ARRAYORDER_BGRA,
  SDL_ARRAYORDER_ABGR
);

SDL_PackedLayout = (
  SDL_PACKEDLAYOUT_NONE = 0,
  SDL_PACKEDLAYOUT_332,
  SDL_PACKEDLAYOUT_4444,
  SDL_PACKEDLAYOUT_1555,
  SDL_PACKEDLAYOUT_5551,
  SDL_PACKEDLAYOUT_565,
  SDL_PACKEDLAYOUT_8888,
  SDL_PACKEDLAYOUT_2101010,
  SDL_PACKEDLAYOUT_1010102
);

type
  SDL_PixelFormat = Uint32;

type
SDL_ColorType = (
  SDL_COLOR_TYPE_UNKNOWN = 0,
  SDL_COLOR_TYPE_RGB     = 1,
  SDL_COLOR_TYPE_YCBCR   = 2
);

SDL_ColorRange = (
  SDL_COLOR_RANGE_UNKNOWN = 0,
  SDL_COLOR_RANGE_LIMITED = 1,
  SDL_COLOR_RANGE_FULL    = 2
);

SDL_ColorPrimaries = (
  SDL_COLOR_PRIMARIES_UNKNOWN      = 0,
  SDL_COLOR_PRIMARIES_BT709        = 1,
  SDL_COLOR_PRIMARIES_UNSPECIFIED  = 2,
  SDL_COLOR_PRIMARIES_BT470M       = 4,
  SDL_COLOR_PRIMARIES_BT470BG      = 5,
  SDL_COLOR_PRIMARIES_BT601        = 6,
  SDL_COLOR_PRIMARIES_SMPTE240     = 7,
  SDL_COLOR_PRIMARIES_GENERIC_FILM = 8,
  SDL_COLOR_PRIMARIES_BT2020       = 9,
  SDL_COLOR_PRIMARIES_XYZ          = 10,
  SDL_COLOR_PRIMARIES_SMPTE431     = 11,
  SDL_COLOR_PRIMARIES_SMPTE432     = 12,
  SDL_COLOR_PRIMARIES_EBU3213      = 22,
  SDL_COLOR_PRIMARIES_CUSTOM       = 31
);

SDL_TransferCharacteristics = (
  SDL_TRANSFER_CHARACTERISTICS_UNKNOWN     = 0,
  SDL_TRANSFER_CHARACTERISTICS_BT709       = 1,
  SDL_TRANSFER_CHARACTERISTICS_UNSPECIFIED = 2,
  SDL_TRANSFER_CHARACTERISTICS_GAMMA22     = 4,
  SDL_TRANSFER_CHARACTERISTICS_GAMMA28     = 5,
  SDL_TRANSFER_CHARACTERISTICS_BT601       = 6,
  SDL_TRANSFER_CHARACTERISTICS_SMPTE240    = 7,
  SDL_TRANSFER_CHARACTERISTICS_LINEAR      = 8,
  SDL_TRANSFER_CHARACTERISTICS_LOG100      = 9,
  SDL_TRANSFER_CHARACTERISTICS_LOG100_SQRT10 = 10,
  SDL_TRANSFER_CHARACTERISTICS_IEC61966    = 11,
  SDL_TRANSFER_CHARACTERISTICS_BT1361      = 12,
  SDL_TRANSFER_CHARACTERISTICS_SRGB        = 13,
  SDL_TRANSFER_CHARACTERISTICS_BT2020_10BIT = 14,
  SDL_TRANSFER_CHARACTERISTICS_BT2020_12BIT = 15,
  SDL_TRANSFER_CHARACTERISTICS_PQ          = 16,
  SDL_TRANSFER_CHARACTERISTICS_SMPTE428    = 17,
  SDL_TRANSFER_CHARACTERISTICS_HLG         = 18,
  SDL_TRANSFER_CHARACTERISTICS_CUSTOM      = 31
);

SDL_MatrixCoefficients = (
  SDL_MATRIX_COEFFICIENTS_IDENTITY         = 0,
  SDL_MATRIX_COEFFICIENTS_BT709            = 1,
  SDL_MATRIX_COEFFICIENTS_UNSPECIFIED      = 2,
  SDL_MATRIX_COEFFICIENTS_FCC              = 4,
  SDL_MATRIX_COEFFICIENTS_BT470BG          = 5,
  SDL_MATRIX_COEFFICIENTS_BT601            = 6,
  SDL_MATRIX_COEFFICIENTS_SMPTE240         = 7,
  SDL_MATRIX_COEFFICIENTS_YCGCO            = 8,
  SDL_MATRIX_COEFFICIENTS_BT2020_NCL       = 9,
  SDL_MATRIX_COEFFICIENTS_BT2020_CL        = 10,
  SDL_MATRIX_COEFFICIENTS_SMPTE2085        = 11,
  SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL = 12,
  SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL  = 13,
  SDL_MATRIX_COEFFICIENTS_ICTCP            = 14,
  SDL_MATRIX_COEFFICIENTS_CUSTOM           = 31
);

SDL_ChromaLocation = (
  SDL_CHROMA_LOCATION_NONE    = 0,
  SDL_CHROMA_LOCATION_LEFT    = 1,
  SDL_CHROMA_LOCATION_CENTER  = 2,
  SDL_CHROMA_LOCATION_TOPLEFT = 3
);

type
SDL_Color = packed record
  r, g, b, a: Uint8;
end;
PSDL_Color = ^SDL_Color;

SDL_FColor = packed record
  r, g, b, a: Single;
end;

{ SDL_surface.h }
type
  SDL_SurfaceFlags = UInt32;

type
  SDL_ScaleMode = (
    SDL_SCALEMODE_INVALID = -1,
    SDL_SCALEMODE_NEAREST = 0,
    SDL_SCALEMODE_LINEAR  = 1
  );

  SDL_FlipMode = (
    SDL_FLIP_NONE = 0,
    SDL_FLIP_HORIZONTAL = 1,
    SDL_FLIP_VERTICAL = 2
  );

  PSDL_Surface = ^SDL_Surface;
  PPSDL_Surface = ^PSDL_Surface;
  SDL_Surface = record
    flags   : SDL_SurfaceFlags;   // read-only
    format  : SDL_PixelFormat;    // read-only
    w       : Integer;            // read-only
    h       : Integer;            // read-only
    pitch   : Integer;            // read-only
    pixels  : Pointer;            // writable if non-nil
    refcount: Integer;            // app refcount
    reserved: Pointer;            // internal
  end;

{ SDL_video.h }
type SDL_SystemTheme = (
  SDL_SYSTEM_THEME_UNKNOWN,
  SDL_SYSTEM_THEME_LIGHT,
  SDL_SYSTEM_THEME_DARK
);
PSDL_DisplayModeData = ^SDL_DisplayModeData;
SDL_DisplayModeData  = record end;

PSDL_DisplayMode = ^SDL_DisplayMode;
SDL_DisplayMode = record
  displayID                : SDL_DisplayID;
  format                   : SDL_PixelFormat;
  w                        : LongInt;
  h                        : LongInt;
  pixel_density            : Single;
  refresh_rate             : Single;
  refresh_rate_numerator   : LongInt;
  refresh_rate_denominator : LongInt;
  internal                 : PSDL_DisplayModeData;  { Private }
end;

SDL_DisplayOrientation = (
  SDL_ORIENTATION_UNKNOWN,
  SDL_ORIENTATION_LANDSCAPE,
  SDL_ORIENTATION_LANDSCAPE_FLIPPED,
  SDL_ORIENTATION_PORTRAIT,
  SDL_ORIENTATION_PORTRAIT_FLIPPED
);

SDL_WindowFlags = QWord;

SDL_FlashOperation = (
  SDL_FLASH_CANCEL,
  SDL_FLASH_BRIEFLY,
  SDL_FLASH_UNTIL_FOCUSED
);

SDL_GLContext  = Pointer;
SDL_EGLDisplay = Pointer;
SDL_EGLConfig  = Pointer;
SDL_EGLSurface = Pointer;

SDL_EGLAttrib = PtrInt;
SDL_EGLint    = LongInt;

PSDL_EGLAttrib = ^SDL_EGLAttrib;
PSDL_EGLint    = ^SDL_EGLint;

SDL_EGLAttribArrayCallback = function(userdata: Pointer): PSDL_EGLAttrib; cdecl;
SDL_EGLIntArrayCallback    = function(userdata: Pointer; display: SDL_EGLDisplay; config: SDL_EGLConfig): PSDL_EGLint; cdecl;

SDL_GLAttr = (
  SDL_GL_RED_SIZE,
  SDL_GL_GREEN_SIZE,
  SDL_GL_BLUE_SIZE,
  SDL_GL_ALPHA_SIZE,
  SDL_GL_BUFFER_SIZE,
  SDL_GL_DOUBLEBUFFER,
  SDL_GL_DEPTH_SIZE,
  SDL_GL_STENCIL_SIZE,
  SDL_GL_ACCUM_RED_SIZE,
  SDL_GL_ACCUM_GREEN_SIZE,
  SDL_GL_ACCUM_BLUE_SIZE,
  SDL_GL_ACCUM_ALPHA_SIZE,
  SDL_GL_STEREO,
  SDL_GL_MULTISAMPLEBUFFERS,
  SDL_GL_MULTISAMPLESAMPLES,
  SDL_GL_ACCELERATED_VISUAL,
  SDL_GL_RETAINED_BACKING,
  SDL_GL_CONTEXT_MAJOR_VERSION,
  SDL_GL_CONTEXT_MINOR_VERSION,
  SDL_GL_CONTEXT_FLAGS,
  SDL_GL_CONTEXT_PROFILE_MASK,
  SDL_GL_SHARE_WITH_CURRENT_CONTEXT,
  SDL_GL_FRAMEBUFFER_SRGB_CAPABLE,
  SDL_GL_CONTEXT_RELEASE_BEHAVIOR,
  SDL_GL_CONTEXT_RESET_NOTIFICATION,
  SDL_GL_CONTEXT_NO_ERROR,
  SDL_GL_FLOATBUFFERS,
  SDL_GL_EGL_PLATFORM
);

SDL_GLProfile                   = UInt32;
SDL_GLContextFlag               = UInt32;
SDL_GLContextReleaseFlag        = UInt32;
SDL_GLContextResetNotification  = UInt32;

SDL_HitTestResult = (
  SDL_HITTEST_NORMAL,
  SDL_HITTEST_DRAGGABLE,
  SDL_HITTEST_RESIZE_TOPLEFT,
  SDL_HITTEST_RESIZE_TOP,
  SDL_HITTEST_RESIZE_TOPRIGHT,
  SDL_HITTEST_RESIZE_RIGHT,
  SDL_HITTEST_RESIZE_BOTTOMRIGHT,
  SDL_HITTEST_RESIZE_BOTTOM,
  SDL_HITTEST_RESIZE_BOTTOMLEFT,
  SDL_HITTEST_RESIZE_LEFT
);

SDL_HitTest = function(win: PSDL_Window; const area: PSDL_Point; data: Pointer): SDL_HitTestResult; cdecl;

PPSDL_DisplayMode = ^PSDL_DisplayMode;

type
  SDL_BlendMode = UInt32;
  PSDL_BlendMode = ^SDL_BlendMode;

type
  SDL_BlendOperation = (
    SDL_BLENDOPERATION_ADD          = $1,
    SDL_BLENDOPERATION_SUBTRACT     = $2,
    SDL_BLENDOPERATION_REV_SUBTRACT = $3,
    SDL_BLENDOPERATION_MINIMUM      = $4,
    SDL_BLENDOPERATION_MAXIMUM      = $5
  );

  SDL_BlendFactor = (
    SDL_BLENDFACTOR_ZERO                = $1,
    SDL_BLENDFACTOR_ONE                 = $2,
    SDL_BLENDFACTOR_SRC_COLOR           = $3,
    SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR = $4,
    SDL_BLENDFACTOR_SRC_ALPHA           = $5,
    SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = $6,
    SDL_BLENDFACTOR_DST_COLOR           = $7,
    SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR = $8,
    SDL_BLENDFACTOR_DST_ALPHA           = $9,
    SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA = $A
  );


type
  SDL_TimerID        = UInt32;
  SDL_TimerCallback  = function(userdata: Pointer; timerID: SDL_TimerID; interval: UInt32): UInt32; cdecl;
  SDL_NSTimerCallback= function(userdata: Pointer; timerID: SDL_TimerID; interval: UInt64): UInt64; cdecl;


type SDL_EventAction = (
    SDL_ADDEVENT  = 0,
    SDL_PEEKEVENT = 1,
    SDL_GETEVENT  = 2
);

{ SDL_iostream.h }

type SDL_IOStatus = (
  SDL_IO_STATUS_READY = 0,
  SDL_IO_STATUS_ERROR = 1,
  SDL_IO_STATUS_EOF = 2,
  SDL_IO_STATUS_NOT_READY = 3,
  SDL_IO_STATUS_READONLY = 4,
  SDL_IO_STATUS_WRITEONLY = 5
);
PSDL_IOStatus = ^SDL_IOStatus;

type SDL_IOWhence = (
  SDL_IO_SEEK_SET = 0,
  SDL_IO_SEEK_CUR = 1,
  SDL_IO_SEEK_END = 2
);

type
  PSDL_IOStream = ^SDL_IOStream;
  SDL_IOStream  = record end;

type
  PSDL_IOStreamInterface = ^SDL_IOStreamInterface;
  SDL_IOStreamInterface = record
    version : Uint32;
    size    : function( userdata : Pointer ): Sint64; cdecl;
    seek    : function( userdata : Pointer; offset: Sint64; whence: SDL_IOWhence ): Sint64; cdecl;
    read    : function( userdata : Pointer; ptr: Pointer; size: SizeT; status: PSDL_IOStatus ): SizeT; cdecl;
    write   : function( userdata : Pointer; const ptr: Pointer; size: SizeT; status: PSDL_IOStatus ): SizeT; cdecl;
    flush   : function( userdata : Pointer; status: PSDL_IOStatus ): SDL_bool; cdecl;
    close   : function( userdata : Pointer ): SDL_bool; cdecl;
  end;


PSDL_Event   = ^SDL_Event;
type SDL_EventFilter = function(userdata: Pointer; event : PSDL_Event): vsdl3library.SDL_bool; cdecl;

type
  SDL_AppInit_func       = function( appstate : PPointer; argc: Sint32; argv: PPAnsiChar): SDL_AppResult; cdecl;
  SDL_AppIterate_func    = function( appstate : Pointer): SDL_AppResult; cdecl;
  SDL_AppEvent_func      = function( appstate : Pointer; event: PSDL_Event): SDL_AppResult; cdecl;
  SDL_AppQuit_func       = procedure( appstate : Pointer; result: SDL_AppResult); cdecl;
  SDL_MainThreadCallback = procedure( userdata : Pointer); cdecl;

