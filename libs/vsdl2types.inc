//SDL_types.h types
const
  SDL_FALSE = 0;
  SDL_TRUE  = 1;

type
  THandle     = Cardinal;
  TSDL_Bool   = LongBool;

  UInt8       = Byte;
  SInt8       = ShortInt;
  UInt16      = Word;
  SInt16      = SmallInt;
  UInt32      = LongWord;
  SInt32      = LongInt;
  UInt64      = Int64;
  SInt64      = QWord;

  PUInt8      = ^UInt8;
  PSInt8      = ^SInt8;
  PUInt16     = ^UInt16;
  PSInt16     = ^SInt16;
  PUInt32     = ^UInt32;
  PSInt32     = ^SInt32;
  PUInt64     = ^UInt64;
  PSInt64     = ^SInt64;

  TUInt8Array = array [0..MAXINT shr 1] of UInt8;
  PUInt8Array = ^TUInt8Array;

// SDL_rwops.h types
  PSDL_RWops = ^TSDL_RWops;
  RWopsSizeFunc = function(context: PSDL_RWops): SInt64; cdecl;
  RWopsSeekFunc = function(context: PSDL_RWops; offset: SInt64; whence: Integer): SInt64; cdecl;
  RWopsReadFunc = function(context: PSDL_RWops; ptr: Pointer; size, maxnum: size_t): size_t; cdecl;
  RWopsWriteFunc = function(context: PSDL_RWops; const ptr: Pointer; size, num: size_t): size_t; cdecl;
  RWopsCloseFunc = function(context: PSDL_RWops): Integer; cdecl;

  TSDL_RWopsHiddenWin32IO = record
    append: Integer;
    h: Pointer;
    buffer: record
      data: Pointer;
      size: Integer;
      left: Integer;
    end;
  end;

  TSDL_RWopsHiddenStdio = record
    autoclose: Integer;
    fp: Pointer; // FILE* in C
  end;

  TSDL_RWopsHiddenMem = record
    base: PUInt8;
    here: PUInt8;
    stop: PUInt8;
  end;

  TSDL_RWopsHiddenUnknown = record
    data1: Pointer;
    data2: Pointer;
  end;

  TSDL_RWopsHidden = record
    case Integer of
      0: (win32io: TSDL_RWopsHiddenWin32IO);
      1: (stdio: TSDL_RWopsHiddenStdio);
      2: (mem: TSDL_RWopsHiddenMem);
      3: (unknown: TSDL_RWopsHiddenUnknown);
  end;

  TSDL_RWops = record
    size: RWopsSizeFunc;
    seek: RWopsSeekFunc;
    read: RWopsReadFunc;
    write: RWopsWriteFunc;
    close: RWopsCloseFunc;
    typ: UInt32;
    hidden: TSDL_RWopsHidden;
  end;

type
  PSDL_Point   = ^SDL_Point;
  PSDL_Rect    = ^SDL_Rect;
  PSDL_Color   = ^SDL_Color;
  PSDL_Palette = ^SDL_Palette;

  SDL_Point = record
    x : Integer;
    y : Integer;
  end;

  SDL_Rect = record
    x : Integer;
    y : Integer;
    w : Integer;
    h : Integer;
  end;

  SDL_Color = record
    r : UInt8;
    g : UInt8;
    b : UInt8;
    unused : UInt8;
  end;

  SDL_Palette = record
    ncolors  : Integer;
    colors   : PSDL_Color;
    version  : UInt32;
    refcount : Integer;
  end;

type
  PSDL_PixelFormat = ^TSDL_PixelFormat;
  PSDL_Surface     = ^TSDL_Surface;
  PSDL_DisplayMode = ^TSDL_DisplayMode;

  TSDL_PixelFormat = record
    format : UInt32;
    palette : PSDL_Palette;
    BitsPerPixel : UInt8;
    BytesPerPixel : UInt8;
    padding : array[0..1] of UInt8;
    Rmask, Gmask, Bmask, Amask : UInt32;
    Rloss, Gloss, Bloss, Aloss : UInt8;
    Rshift, Gshift, Bshift, Ashift : UInt8;
    refcount : Integer;
    next : PSDL_PixelFormat;
  end;

  TSDL_DisplayMode = record
    format : UInt32;
    w, h : Integer;
    refresh_rate : Integer;
    driverdata : Pointer;
  end;

  TSDL_Surface = record
    flags : UInt32;
    format : PSDL_PixelFormat;
    w, h : Integer;
    pitch : Integer;
    pixels : Pointer;
    userdata : Pointer;
    locked : Integer;
    lock_data : Pointer;
    clip_rect : SDL_Rect;
    map : Pointer; // PSDL_BlitMap
    refcount : Integer;
  end;

type
  SDL_RendererInfo = record
    name : PChar;
    flags : UInt32;
    num_texture_formats : UInt32;
    texture_formats : array[0..15] of UInt32;
    max_texture_width : Integer;
    max_texture_height : Integer;
  end;

type
  TSDL_blit = function(src: PSDL_Surface; srcrect: PSDL_Rect;
                       dst: PSDL_Surface; dstrect: PSDL_Rect): Integer; cdecl;

type
  SDL_WindowShapeParams = record
    case Integer of
      0: (binarizationCutoff: UInt8);
      1: (colorKey: SDL_Color);
  end;

  SDL_WindowShapeMode = record
    mode : Integer;
    parameters : SDL_WindowShapeParams;
  end;

  SDL_TouchID = SInt64;
  SDL_FingerID = SInt64;

  SDL_Finger = record
    id: SDL_FingerID;
    x: Single;
    y: Single;
    pressure: Single;
  end;

 // SDL_audio.h
type
  SDL_AudioFormat   = UInt16;
  SDL_AudioDeviceID = UInt32;
  PSDL_AudioCVT     = ^TSDL_AudioCVT;
  SDL_AudioCallback = procedure( userdata : Pointer; stream : PUInt8; len : Integer ); cdecl;
  SDL_AudioFilter   = procedure( cvt : PSDL_AudioCVT; format : SDL_AudioFormat ); cdecl;

  TSDL_AudioSpec = record
    freq : Integer;
    format : SDL_AudioFormat;
    channels : UInt8;
    silence : UInt8;
    samples : UInt16;
    padding : UInt16;
    size : UInt32;
    callback : SDL_AudioCallback;
    userdata : Pointer;
  end;

  TSDL_AudioCVT = record
    needed : Integer;
    src_format : SDL_AudioFormat;
    dst_format : SDL_AudioFormat;
    rate_incr : Double;
    buf : PUInt8;
    len : Integer;
    len_cvt : Integer;
    len_mult : Integer;
    len_ratio : Double;
    filters : array[0..SDL_AUDIOCVT_MAX_FILTERS] of SDL_AudioFilter;
    filter_index : Integer;
  end;

type
  SDL_HintCallback = procedure(userdata: Pointer; name: PChar; oldValue: PChar; newValue: PChar); cdecl;

type
  PSDL_MessageBoxButtonData  = ^SDL_MessageBoxButtonData;
  PSDL_MessageBoxColorScheme = ^SDL_MessageBoxColorScheme;

  SDL_MessageBoxButtonData = record
    flags : UInt32;
    buttonid : Integer;
    text : PChar;
  end;

  SDL_MessageBoxColor = record
    r, g, b : UInt8;
  end;

  SDL_MessageBoxColorScheme = record
    colors : array[0..SDL_MESSAGEBOX_COLOR_MAX - 1] of SDL_MessageBoxColor;
  end;

  SDL_Window = record;
  PSDL_Window = ^SDL_Window;

  SDL_MessageBoxData = record
    flags : UInt32;
    window : PSDL_Window;
    title : PChar;
    message : PChar;
    numbuttons : Integer;
    buttons : PSDL_MessageBoxButtonData;
    colorScheme : PSDL_MessageBoxColorScheme;
  end;

type
  SDL_Keycode = SInt32;
  SDL_Scancode = SInt32;

  SDL_Keysym = record
    scancode : SDL_Scancode;
    sym: SDL_Keycode;
    mod_ : UInt16;
    unused: UInt32;
  end;

type
  // Forward declaration of WMcursor
  WMcursor = record;
  PWMcursor = ^WMcursor;

  SDL_Cursor = record
    area : SDL_Rect;
    hot_x : SInt16;
    hot_y : SInt16;
    data : PUInt8;
    mask : PUInt8;
    save : array[0..1] of PUInt8;
    wm_cursor : PWMcursor;
  end;

type
  // Forward declaration for SDL_Joystick and SDL_GameController
  SDL_Joystick = record;
  PSDL_Joystick = ^SDL_Joystick;

  SDL_GameController = record;
  PSDL_GameController = ^SDL_GameController;

  SDL_JoystickGUID = record
    data : array[0..15] of UInt8;
  end;

  SDL_JoystickID = SInt32;
  SDL_GameControllerButtonBind = record
    bindType: Integer;
    case Integer of
      0: (button: Integer);
      1: (axis: Integer);
      2: (hat: record
            hat: Integer;
            hat_mask: Integer;
          end);
  end;

type
  SDL_HapticDirection = record
    type_: UInt8;
    dir: array[0..2] of SInt32;
  end;

  SDL_HapticConstant = record
    type_: Uint16;
    direction: SDL_HapticDirection;

    length: Uint32;
    delay: Uint16;

    button: Uint16;
    interval: Uint16;

    level: SInt16;

    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;

  SDL_HapticPeriodic = record
    type_: Uint16;
    direction: SDL_HapticDirection;

    length: Uint32;
    delay: Uint16;

    button: Uint16;
    interval: Uint16;

    period: Uint16;
    magnitude: SInt16;
    offset: SInt16;
    phase: Uint16;

    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;

  SDL_HapticCondition = record
    type_: Uint16;
    direction: SDL_HapticDirection;

    length: Uint32;
    delay: Uint16;

    button: Uint16;
    interval: Uint16;

    right_sat: array[0..2] of Uint16;
    left_sat: array[0..2] of Uint16;
    right_coeff: array[0..2] of SInt16;
    left_coeff: array[0..2] of SInt16;
    deadband: array[0..2] of Uint16;
    center: array[0..2] of SInt16;
  end;

  SDL_HapticRamp = record
    type_: Uint16;
    direction: SDL_HapticDirection;

    length: Uint32;
    delay: Uint16;

    button: Uint16;
    interval: Uint16;

    start: SInt16;
    end_: SInt16;

    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;

  SDL_HapticLeftRight = record
    type_: Uint16;
    length: Uint32;

    large_magnitude: Uint16;
    small_magnitude: Uint16;
  end;

  SDL_HapticCustom = record
    type_: Uint16;
    direction: SDL_HapticDirection;

    length: Uint32;
    delay: Uint16;

    button: Uint16;
    interval: Uint16;

    channels: Uint8;
    period: Uint16;
    samples: Uint16;
    data: PUint16;

    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;

  SDL_HapticEffect = record
    case Integer of
      0: (type_: Uint16);
      1: (constant: SDL_HapticConstant);
      2: (periodic: SDL_HapticPeriodic);
      3: (condition: SDL_HapticCondition);
      4: (ramp: SDL_HapticRamp);
      5: (leftright: SDL_HapticLeftRight);
      6: (custom: SDL_HapticCustom);
  end;

type
  SDL_Event  = record;
  PSDL_Event = ^SDL_Event;

  // SDL_EventFilter
  SDL_EventFilter = function(userdata: Pointer; event: PSDL_Event): Integer; cdecl;

  // SDL_TimerCallback
  SDL_TimerCallback = function(interval: Uint32; param: Pointer): Uint32; cdecl;

  // SDL_TimerID
  SDL_TimerID = Integer;

  // SDL_version
  SDL_version = record
    major: Uint8;
    minor: Uint8;
    patch: Uint8;
  end;

type
  SDL_SysWMinfo = record
    version: SDL_version;
    subsystem: Integer;
    case Integer of
      0: (
        {$IFDEF WINDOWS}
        win: record
          window: HWND;
          hdc: HDC;
          hinst: HINST;
        end;
        {$ENDIF}
      );
      1: (dummy: array[0..63] of Uint8);
  end;



