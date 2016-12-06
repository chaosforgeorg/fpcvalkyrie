type
  THandle = Cardinal;
  //SDL_types.h types
  // Basic data types

  SDL_Bool  = (SDL_FALSE, SDL_TRUE);
  TSDL_Bool = SDL_Bool;

  PUInt8Array = ^TUInt8Array;
  PUInt8 = ^UInt8;
  PPUInt8 = ^PUInt8;
  UInt8 = Byte;
  TUInt8Array = array [0..MAXINT shr 1] of UInt8;

  PUInt16 = ^UInt16;
  UInt16 = word;

  PSInt8 = ^SInt8;
  SInt8 = Shortint;

  PSInt16 = ^SInt16;
  SInt16 = smallint;

  PUInt32 = ^UInt32;
  UInt32 = Cardinal;

  SInt32 = Integer;

  PInt = ^Integer;

  PShortInt = ^ShortInt;

  PUInt64 = ^UInt64;
  UInt64 = record
    hi: UInt32;
    lo: UInt32;
  end;

  PSInt64 = ^SInt64;
  SInt64 = record
    hi: UInt32;
    lo: UInt32;
  end;

  TSDL_GrabMode = Integer;

  // SDL_error.h types
  TSDL_errorcode = (
    SDL_ENOMEM,
    SDL_EFREAD,
    SDL_EFWRITE,
    SDL_EFSEEK,
    SDL_LASTERROR);

  SDL_errorcode = TSDL_errorcode;

  TArg = record
    case Byte of
      0: (value_ptr: Pointer);
      (* #if 0 means: never
      1 :  ( value_c : Byte );
      *)
      2: (value_i: Integer);
      3: (value_f: double);
      4: (buf: array[0..ERR_MAX_STRLEN - 1] of Byte);
  end;

  PSDL_error = ^TSDL_error;
  TSDL_error = record
    { This is a numeric value corresponding to the current error }
    error: Integer;

    { This is a key used to index into a language hashtable containing
       internationalized versions of the SDL error messages.  If the key
       is not in the hashtable, or no hashtable is available, the key is
       used directly as an error message format string. }
    key: array[0..ERR_MAX_STRLEN - 1] of Byte;

    { These are the arguments for the error functions }
    argc: Integer;
    args: array[0..ERR_MAX_ARGS - 1] of TArg;
  end;

  // SDL_rwops.h types
  // This is the read/write operation structure -- very basic
  // some helper types to handle the unions
  // "packed" is only guessed

  TStdio = record
    autoclose: Integer;
   // FILE * is only defined in Kylix so we use a simple Pointer
    fp: Pointer;
  end;

  TMem = record
    base: PUInt8;
    here: PUInt8;
    stop: PUInt8;
  end;

  TUnknown = record
    data1: Pointer;
  end;

  // first declare the pointer type
  PSDL_RWops = ^TSDL_RWops;
  // now the pointer to function types
  TSeek = function( context: PSDL_RWops; offset: Integer; whence: Integer ): Integer; cdecl;
  TRead = function( context: PSDL_RWops; Ptr: Pointer; size: Integer; maxnum : Integer ): Integer;  cdecl;
  TWrite = function( context: PSDL_RWops; Ptr: Pointer; size: Integer; num: Integer ): Integer; cdecl;
  TClose = function( context: PSDL_RWops ): Integer; cdecl;
  // the variant record itself
  TSDL_RWops = record
    seek: TSeek;
    read: TRead;
    write: TWrite;
    close: TClose;
    // a keyword as name is not allowed
    type_: UInt32;
    // be warned! structure alignment may arise at this point
    case Integer of
      0: (stdio: TStdio);
      1: (mem: TMem);
      2: (unknown: TUnknown);
  end;

  SDL_RWops = TSDL_RWops;

  // SDL_timer.h types
  // Function prototype for the timer callback function
  TSDL_TimerCallback = function( interval: UInt32 ): UInt32; cdecl;

 { Function prototype for the new timer callback function.
   The callback function is passed the current timer interval and returns
   the next timer interval.  If the returned value is the same as the one
   passed in, the periodic alarm continues, otherwise a new alarm is
   scheduled.  If the callback returns 0, the periodic alarm is cancelled. }
  TSDL_NewTimerCallback = function( interval: UInt32; param: Pointer ): UInt32; cdecl;

  // Definition of the timer ID type
  PSDL_TimerID = ^TSDL_TimerID;
  TSDL_TimerID = record
    interval: UInt32;
    callback: TSDL_NewTimerCallback;
    param: Pointer;
    last_alarm: UInt32;
    next: PSDL_TimerID;
  end;
  TSDL_AudioSpecCallback = procedure( userdata: Pointer; stream: PUInt8; len: Integer ); cdecl;

  // SDL_audio.h types
  // The calculated values in this structure are calculated by SDL_OpenAudio()
  PSDL_AudioSpec = ^TSDL_AudioSpec;
  TSDL_AudioSpec = record
    freq: Integer; // DSP frequency -- samples per second
    format: UInt16; // Audio data format
    channels: UInt8; // Number of channels: 1 mono, 2 stereo
    silence: UInt8; // Audio buffer silence value (calculated)
    samples: UInt16; // Audio buffer size in samples
    padding: UInt16; // Necessary for some compile environments
    size: UInt32; // Audio buffer size in bytes (calculated)
    { This function is called when the audio device needs more data.
      'stream' is a pointer to the audio data buffer
      'len' is the length of that buffer in bytes.
      Once the callback returns, the buffer will no longer be valid.
      Stereo samples are stored in a LRLRLR ordering.}
    callback: TSDL_AudioSpecCallback;
    userdata: Pointer;
  end;

  // A structure to hold a set of audio conversion filters and buffers
  PSDL_AudioCVT = ^TSDL_AudioCVT;

  PSDL_AudioCVTFilter = ^TSDL_AudioCVTFilter;
  TSDL_AudioCVTFilter = record
    cvt: PSDL_AudioCVT;
    format: UInt16;
  end;

  PSDL_AudioCVTFilterArray = ^TSDL_AudioCVTFilterArray;
  TSDL_AudioCVTFilterArray = array[0..9] of PSDL_AudioCVTFilter;

  TSDL_AudioCVT = record
    needed: Integer; // Set to 1 if conversion possible
    src_format: UInt16; // Source audio format
    dst_format: UInt16; // Target audio format
    rate_incr: double; // Rate conversion increment
    buf: PUInt8; // Buffer to hold entire audio data
    len: Integer; // Length of original audio buffer
    len_cvt: Integer; // Length of converted audio buffer
    len_mult: Integer; // buffer must be len*len_mult big
    len_ratio: double; // Given len, final size is len*len_ratio
    filters: TSDL_AudioCVTFilterArray;
    filter_index: Integer; // Current audio conversion function
  end;

  TSDL_Audiostatus = (
    SDL_AUDIO_STOPPED,
    SDL_AUDIO_PLAYING,
    SDL_AUDIO_PAUSED
    );

  // SDL_cdrom.h types
  TSDL_CDStatus = (
    CD_ERROR,
    CD_TRAYEMPTY,
    CD_STOPPED,
    CD_PLAYING,
    CD_PAUSED );

  PSDL_CDTrack = ^TSDL_CDTrack;
  TSDL_CDTrack = record
    id: UInt8; // Track number
    type_: UInt8; // Data or audio track
    unused: UInt16;
    length: UInt32; // Length, in frames, of this track
    offset: UInt32; // Offset, in frames, from start of disk
  end;

  // This structure is only current as of the last call to SDL_CDStatus()
  PSDL_CD = ^TSDL_CD;
  TSDL_CD = record
    id: Integer; // Private drive identifier
    status: TSDL_CDStatus; // Current drive status

    // The rest of this structure is only valid if there's a CD in drive
    numtracks: Integer; // Number of tracks on disk
    cur_track: Integer; // Current track position
    cur_frame: Integer; // Current frame offset within current track
    track: array[0..SDL_MAX_TRACKS] of TSDL_CDTrack;
  end;

  //SDL_joystick.h types
  PTransAxis = ^TTransAxis;
  TTransAxis = record
    offset: Integer;
    scale: single;
  end;

  // The private structure used to keep track of a joystick
  PJoystick_hwdata = ^TJoystick_hwdata;
  TJoystick_hwdata = record
    // joystick ID
    id: Integer;
    // values used to translate device-specific coordinates into  SDL-standard ranges
    transaxis: array[0..5] of TTransAxis;
  end;

  PBallDelta = ^TBallDelta;
  TBallDelta = record
    dx: Integer;
    dy: Integer;
  end; // Current ball motion deltas

  // The SDL joystick structure
  PSDL_Joystick = ^TSDL_Joystick;
  TSDL_Joystick = record
    index: UInt8; // Device index
    name: PChar; // Joystick name - system dependent

    naxes: Integer; // Number of axis controls on the joystick
    axes: PUInt16; // Current axis states

    nhats: Integer; // Number of hats on the joystick
    hats: PUInt8; // Current hat states

    nballs: Integer; // Number of trackballs on the joystick
    balls: PBallDelta; // Current ball motion deltas

    nbuttons: Integer; // Number of buttons on the joystick
    buttons: PUInt8; // Current button states

    hwdata: PJoystick_hwdata; // Driver dependent information

    ref_count: Integer; // Reference count for multiple opens
  end;

  // SDL_verion.h types
  PSDL_version = ^TSDL_version;
  TSDL_version = record
    major: UInt8;
    minor: UInt8;
    patch: UInt8;
  end;

  // SDL_keyboard.h types
  TSDLKey = LongWord;

  TSDLMod = LongWord;

  PSDL_KeySym = ^TSDL_KeySym;
  TSDL_KeySym = record
    scancode: UInt8; // hardware specific scancode
    sym: TSDLKey; // SDL virtual keysym
    modifier: TSDLMod; // current key modifiers
    unicode: UInt16; // translated character
  end;

  // SDL_events.h types
  {Checks the event queue for messages and optionally returns them.
   If 'action' is SDL_ADDEVENT, up to 'numevents' events will be added to
   the back of the event queue.
   If 'action' is SDL_PEEKEVENT, up to 'numevents' events at the front
   of the event queue, matching 'mask', will be returned and will not
   be removed from the queue.
   If 'action' is SDL_GETEVENT, up to 'numevents' events at the front
   of the event queue, matching 'mask', will be returned and will be
   removed from the queue.
   This function returns the number of events actually stored, or -1
   if there was an error.  This function is thread-safe. }

  TSDL_EventAction = (SDL_ADDEVENT, SDL_PEEKEVENT, SDL_GETEVENT);

  // Application visibility event structure
  TSDL_ActiveEvent = record
    type_: UInt8; // SDL_ACTIVEEVENT
    gain: UInt8; // Whether given states were gained or lost (1/0)
    state: UInt8; // A mask of the focus states
  end;

  // Keyboard event structure
  TSDL_KeyboardEvent = record
    type_: UInt8; // SDL_KEYDOWN or SDL_KEYUP
    which: UInt8; // The keyboard device index
    state: UInt8; // SDL_PRESSED or SDL_RELEASED
    keysym: TSDL_KeySym;
  end;

  // Mouse motion event structure
  TSDL_MouseMotionEvent = record
    type_: UInt8; // SDL_MOUSEMOTION
    which: UInt8; // The mouse device index
    state: UInt8; // The current button state
    x, y: UInt16; // The X/Y coordinates of the mouse
    xrel: SInt16; // The relative motion in the X direction
    yrel: SInt16; // The relative motion in the Y direction
  end;

  // Mouse button event structure
  TSDL_MouseButtonEvent = record
    type_: UInt8;  // SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP
    which: UInt8;  // The mouse device index
    button: UInt8; // The mouse button index
    state: UInt8;  // SDL_PRESSED or SDL_RELEASED
    x: UInt16;     // The X coordinates of the mouse at press time
    y: UInt16;     // The Y coordinates of the mouse at press time
  end;

  // Joystick axis motion event structure
  TSDL_JoyAxisEvent = record
    type_: UInt8; // SDL_JOYAXISMOTION
    which: UInt8; // The joystick device index
    axis: UInt8; // The joystick axis index
    value: SInt16; // The axis value (range: -32768 to 32767)
  end;

  // Joystick trackball motion event structure
  TSDL_JoyBallEvent = record
    type_: UInt8; // SDL_JOYAVBALLMOTION
    which: UInt8; // The joystick device index
    ball: UInt8; // The joystick trackball index
    xrel: SInt16; // The relative motion in the X direction
    yrel: SInt16; // The relative motion in the Y direction
  end;

  // Joystick hat position change event structure
  TSDL_JoyHatEvent = record
    type_: UInt8; // SDL_JOYHATMOTION */
    which: UInt8; // The joystick device index */
    hat: UInt8; // The joystick hat index */
    value: UInt8; { The hat position value:
                    8   1   2
                    7   0   3
                    6   5   4

                    Note that zero means the POV is centered. }

  end;

  // Joystick button event structure
  TSDL_JoyButtonEvent = record
    type_: UInt8; // SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP
    which: UInt8; // The joystick device index
    button: UInt8; // The joystick button index
    state: UInt8; // SDL_PRESSED or SDL_RELEASED
  end;

  { The "window resized" event
    When you get this event, you are responsible for setting a new video
    mode with the new width and height. }
  TSDL_ResizeEvent = record
    type_: UInt8; // SDL_VIDEORESIZE
    w: Integer; // New width
    h: Integer; // New height
  end;

  // The "quit requested" event
  PSDL_QuitEvent = ^TSDL_QuitEvent;
  TSDL_QuitEvent = record
    type_: UInt8;
  end;

  // A user-defined event type
  PSDL_UserEvent = ^TSDL_UserEvent;
  TSDL_UserEvent = record
    type_: UInt8; // SDL_USEREVENT through SDL_NUMEVENTS-1
    code: Integer; // User defined event code */
    data1: Pointer; // User defined data pointer */
    data2: Pointer; // User defined data pointer */
  end;

  // The "screen redraw" event
  PSDL_ExposeEvent = ^TSDL_ExposeEvent;
  TSDL_ExposeEvent = record
    type_ : Uint8;        // SDL_VIDEOEXPOSE
  end;

 {$IFDEF UNIX}
 //These are the various supported subsystems under UNIX
  TSDL_SysWm = ( SDL_SYSWM_X11 ) ;
 {$ENDIF}

// The windows custom event structure
{$IFDEF WINDOWS}
  PSDL_SysWMmsg = ^TSDL_SysWMmsg;
  TSDL_SysWMmsg = record
    version: TSDL_version;
    h_wnd: HWND; // The window for the message
    msg: UInt; // The type of message
    w_Param: WPARAM; // WORD message parameter
    lParam: LPARAM; // LONG message parameter
  end;
{$ELSE}

{$IFDEF UNIX}
{ The Linux custom event structure }
  PSDL_SysWMmsg = ^TSDL_SysWMmsg;
  TSDL_SysWMmsg = record
    version : TSDL_version;
    subsystem : TSDL_SysWm;
    {$IFNDEF GP2X}
      {$IFNDEF DARWIN}
        event : TXEvent;
      {$ENDIF}
    {$ENDIF}
  end;
{$ELSE}
{ The generic custom event structure }
  PSDL_SysWMmsg = ^TSDL_SysWMmsg;
  TSDL_SysWMmsg = record
    version: TSDL_version;
    data: Integer;
  end;
{$ENDIF}

{$ENDIF}

// The Windows custom window manager information structure
{$IFDEF WINDOWS}
  PSDL_SysWMinfo = ^TSDL_SysWMinfo;
  TSDL_SysWMinfo = record
    version : TSDL_version;
    window : HWnd;	// The display window
  end;
{$ELSE}

// The Linux custom window manager information structure
{$IFDEF Unix}
  {$IFNDEF GP2X}
  {$IFNDEF DARWIN}
  TX11 = record
    display : PDisplay;	// The X11 display
    window : TWindow ;		// The X11 display window */
    {* These locking functions should be called around
       any X11 functions using the display variable.
       They lock the event thread, so should not be
       called around event functions or from event filters.
     *}
    lock_func : Pointer;
    unlock_func : Pointer;

    // Introduced in SDL 1.0.2
    fswindow : TWindow ;	// The X11 fullscreen window */
    wmwindow : TWindow ;	// The X11 managed input window */
  end;
  {$ENDIF}
  {$ENDIF}

  PSDL_SysWMinfo = ^TSDL_SysWMinfo;
  TSDL_SysWMinfo = record
     version : TSDL_version ;
     subsystem : TSDL_SysWm;
     {$IFNDEF GP2X}
     {$IFNDEF DARWIN}
     X11 : TX11;
     {$ENDIF}
     {$ENDIF}
  end;
{$ELSE}
  // The generic custom window manager information structure
  PSDL_SysWMinfo = ^TSDL_SysWMinfo;
  TSDL_SysWMinfo = record
    version : TSDL_version ;
    data : integer;
  end;
{$ENDIF}

{$ENDIF}

  PSDL_SysWMEvent = ^TSDL_SysWMEvent;
  TSDL_SysWMEvent = record
    type_: UInt8;
    msg: PSDL_SysWMmsg;
  end;

  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
    case UInt8 of
      SDL_NOEVENT: (type_: byte);
      SDL_ACTIVEEVENT: (active: TSDL_ActiveEvent);
      SDL_KEYDOWN, SDL_KEYUP: (key: TSDL_KeyboardEvent);
      SDL_MOUSEMOTION: (motion: TSDL_MouseMotionEvent);
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP: (button: TSDL_MouseButtonEvent );
      SDL_JOYAXISMOTION: (jaxis: TSDL_JoyAxisEvent );
      SDL_JOYBALLMOTION: (jball: TSDL_JoyBallEvent );
      SDL_JOYHATMOTION: (jhat: TSDL_JoyHatEvent );
      SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP: (jbutton: TSDL_JoyButtonEvent );
      SDL_VIDEORESIZE: (resize: TSDL_ResizeEvent );
      SDL_QUITEV: (quit: TSDL_QuitEvent );
      SDL_USEREVENT : ( user : TSDL_UserEvent );
      SDL_SYSWMEVENT: (syswm: TSDL_SysWMEvent );
  end;


{ This function sets up a filter to process all events before they
  change internal state and are posted to the internal event queue.

  The filter is protypted as: }
  TSDL_EventFilter = function( event : PSDL_Event ): Integer; cdecl;

  // SDL_video.h types
  // Useful data types
  PPSDL_Rect = ^PSDL_Rect;
  PSDL_Rect = ^TSDL_Rect;
  TSDL_Rect = record
    x, y: SInt16;
    w, h: UInt16;
  end;

  SDL_Rect = TSDL_Rect;

  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
    r: UInt8;
    g: UInt8;
    b: UInt8;
    unused: UInt8;
  end;

  PSDL_ColorArray = ^TSDL_ColorArray;
  TSDL_ColorArray = array[0..65000] of TSDL_Color;

  PSDL_Palette = ^TSDL_Palette;
  TSDL_Palette = record
    ncolors: Integer;
    colors: PSDL_ColorArray;
  end;

  // Everything in the pixel format structure is read-only
  PSDL_PixelFormat = ^TSDL_PixelFormat;
  TSDL_PixelFormat = record
    palette: PSDL_Palette;
    BitsPerPixel: UInt8;
    BytesPerPixel: UInt8;
    Rloss: UInt8;
    Gloss: UInt8;
    Bloss: UInt8;
    Aloss: UInt8;
    Rshift: UInt8;
    Gshift: UInt8;
    Bshift: UInt8;
    Ashift: UInt8;
    RMask: UInt32;
    GMask: UInt32;
    BMask: UInt32;
    AMask: UInt32;
    colorkey: UInt32; // RGB color key information
    alpha: UInt8; // Alpha value information (per-surface alpha)
  end;

  // The structure passed to the low level blit functions
  PSDL_BlitInfo = ^TSDL_BlitInfo;
  TSDL_BlitInfo = record
    s_pixels: PUInt8;
    s_width: Integer;
    s_height: Integer;
    s_skip: Integer;
    d_pixels: PUInt8;
    d_width: Integer;
    d_height: Integer;
    d_skip: Integer;
    aux_data: Pointer;
    src: PSDL_PixelFormat;
    table: PUInt8;
    dst: PSDL_PixelFormat;
  end;

  // typedef for private surface blitting functions
  PSDL_Surface = ^TSDL_Surface;

  TSDL_Blit = function( src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect ): Integer; cdecl;

  // The type definition for the low level blit functions
  //TSDL_LoBlit = procedure( info : PSDL_BlitInfo ); cdecl;

  // This is the private info structure for software accelerated blits
  {PPrivate_swaccel = ^TPrivate_swaccel;
  TPrivate_swaccel = record
    blit : TSDL_LoBlit;
    aux_data : Pointer;
  end;}

  // Blit mapping definition
  {PSDL_BlitMap = ^TSDL_BlitMap;
  TSDL_BlitMap = record
    dst : PSDL_Surface;
    identity : Integer;
    table : PUInt8;
    hw_blit : TSDL_Blit;
    sw_blit : TSDL_Blit;
    hw_data : PPrivate_hwaccel;
    sw_data : PPrivate_swaccel;

    // the version count matches the destination; mismatch indicates an invalid mapping
    format_version : Cardinal;
  end;}

  TSDL_Surface = record
    flags: UInt32; // Read-only
    format: PSDL_PixelFormat; // Read-only
    w, h: Integer; // Read-only
    pitch: UInt16; // Read-only
    pixels: Pointer; // Read-write
    offset: Integer; // Private
    hwdata: Pointer; //TPrivate_hwdata;  Hardware-specific surface info

    // clipping information:
    clip_rect: TSDL_Rect; // Read-only
    unused1: UInt32; // for binary compatibility
    // Allow recursive locks
    locked: UInt32; // Private
    // info for fast blit mapping to other surfaces
    Blitmap: Pointer; // PSDL_BlitMap; //   Private
    // format version, bumped at every change to invalidate blit maps
    format_version: Cardinal; // Private
    refcount: Integer;
  end;

  // Useful for determining the video hardware capabilities
  PSDL_VideoInfo = ^TSDL_VideoInfo;
  TSDL_VideoInfo = record
    hw_available: UInt8; // Hardware and WindowManager flags in first 2 bits ( see below )
    {hw_available: 1; // Can you create hardware surfaces
    wm_available: 1; // Can you talk to a window manager?
    UnusedBits1: 6;}
    blit_hw: UInt8; // Blit Hardware flags. See below for which bits do what
    {UnusedBits2: 1;
    blit_hw: 1; // Flag:UInt32  Accelerated blits HW --> HW
    blit_hw_CC: 1; // Flag:UInt32  Accelerated blits with Colorkey
    blit_hw_A: 1; // Flag:UInt32  Accelerated blits with Alpha
    blit_sw: 1; // Flag:UInt32  Accelerated blits SW --> HW
    blit_sw_CC: 1; // Flag:UInt32  Accelerated blits with Colorkey
    blit_sw_A: 1; // Flag:UInt32  Accelerated blits with Alpha
    blit_fill: 1; // Flag:UInt32  Accelerated color fill}
    UnusedBits3: UInt8; // Unused at this point
    video_mem: UInt32; // The total amount of video memory (in K)
    vfmt: PSDL_PixelFormat; // Value: The format of the video surface
    current_w : SInt32;	// Value: The current video mode width
	  current_h : SInt32;	// Value: The current video mode height
  end;

  // The YUV hardware video overlay
  PSDL_Overlay = ^TSDL_Overlay;
  TSDL_Overlay = record
    format: UInt32; // Overlay format
    w, h: Integer; // Width and height of overlay
    planes: Integer; // Number of planes in the overlay. Usually either 1 or 3
    pitches: PUInt16;
      // An array of pitches, one for each plane. Pitch is the length of a row in bytes.
    pixels: PPUInt8;
      // An array of pointers to the data of each plane. The overlay should be locked before these pointers are used.
    hw_overlay: UInt32;
      // This will be set to 1 if the overlay is hardware accelerated.
  end;

  // Public enumeration for setting the OpenGL window attributes.
  TSDL_GLAttr = (
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
    SDL_GL_SWAP_CONTROL);



  PSDL_Cursor = ^TSDL_Cursor;
  TSDL_Cursor = record
    area: TSDL_Rect; // The area of the mouse cursor
    hot_x, hot_y: SInt16; // The "tip" of the cursor
    data: PUInt8; // B/W cursor data
    mask: PUInt8; // B/W cursor mask
    save: array[1..2] of PUInt8; // Place to save cursor area
    wm_cursor: Pointer; // Window-manager cursor
  end;

// SDL_mutex.h types

{$IFDEF WINDOWS}
  PSDL_Mutex = ^TSDL_Mutex;
  TSDL_Mutex = record
    id: THANDLE;
  end;
{$ENDIF}

{$IFDEF UNIX}
  PSDL_Mutex = ^TSDL_Mutex;
  TSDL_mutex = record
    id: pthread_mutex_t;
  end;
{$ENDIF}

{$IFDEF NDS}
  PSDL_mutex = ^TSDL_Mutex;
  TSDL_Mutex = record
    recursive: Integer;
    Owner: UInt32;
    sem: PSDL_sem;
  end;
{$ENDIF}

PSDL_semaphore = ^TSDL_semaphore;
{$IFDEF WINDOWS}
  // WINDOWS or Machintosh
  TSDL_semaphore = record
    id: THANDLE;
    count: UInt32;
  end;
{$ELSE}
  // This should be semaphore.h
  __sem_lock_t = {packed} record { Not in header file - anonymous }
    status: Longint;
    spinlock: Integer;
  end;

  sem_t = {packed} record
    __sem_lock: __sem_lock_t;
    __sem_value: Integer;
    __sem_waiting: longint ; {_pthread_queue;}
  end;

  TSDL_semaphore = record
    sem: Pointer; //PSem_t;
  end;
{$ENDIF}

  PSDL_Sem = ^TSDL_Sem;
  TSDL_Sem = TSDL_Semaphore;

  PSDL_Cond = ^TSDL_Cond;
  TSDL_Cond = record
{$IFDEF UNIX}
    cond: pthread_cond_t;
{$ELSE}
    // Generic Cond structure
    lock: PSDL_mutex;
    waiting: Integer;
    signals: Integer;
    wait_sem: PSDL_Sem;
    wait_done: PSDL_Sem;
{$ENDIF}
  end;

  // SDL_thread.h types
{$IFDEF WINDOWS}
  TSYS_ThreadHandle = THandle;
{$ENDIF}

{$IFDEF Unix}
  TSYS_ThreadHandle = pthread_t;
{$ENDIF}

{$IFDEF NDS}
  TSYS_ThreadHandle = Integer;
{$ENDIF}

  { This is the system-independent thread info structure }
  PSDL_Thread = ^TSDL_Thread;
  TSDL_Thread = record
    threadid: UInt32;
    handle: TSYS_ThreadHandle;
    status: Integer;
    errbuf: TSDL_Error;
    data: Pointer;
  end;

  // Helper Types

  // Keyboard  State Array ( See demos for how to use )
  PKeyStateArr = ^TKeyStateArr;
  TKeyStateArr = array[0..65000] of UInt8;

  // Types required so we don't need to use Windows.pas
  PInteger = ^Integer;
  PByte = ^Byte;
  PWord = ^Word;
  PLongWord = ^Longword;

  // General arrays
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;

  PPoint = ^TPoint;
  TPoint = Types.TPoint;
  PRect = ^TRect;
  TRect = Types.TRect;

  { Generic procedure pointer }
  TProcedure = procedure;

