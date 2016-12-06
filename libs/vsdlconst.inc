const
  // SDL.h constants
  SDL_INIT_TIMER = $00000001;
  SDL_INIT_AUDIO = $00000010;
  SDL_INIT_VIDEO = $00000020;
  SDL_INIT_CDROM = $00000100;
  SDL_INIT_JOYSTICK = $00000200;
  SDL_INIT_NOPARACHUTE = $00100000; // Don't catch fatal signals
  SDL_INIT_EVENTTHREAD = $01000000; // Not supported on all OS's
  SDL_INIT_EVERYTHING = $0000FFFF;

  // SDL_error.h constants
  ERR_MAX_STRLEN = 128;
  ERR_MAX_ARGS = 5;

  // SDL_types.h constants
  SDL_PRESSED = $01;
  SDL_RELEASED = $00;

  // SDL_timer.h constants
  // This is the OS scheduler timeslice, in milliseconds
  SDL_TIMESLICE = 10;
  // This is the maximum resolution of the SDL timer on all platforms
  TIMER_RESOLUTION = 10; // Experimentally determined

  // SDL_audio.h constants
  AUDIO_U8 = $0008; // Unsigned 8-bit samples
  AUDIO_S8 = $8008; // Signed 8-bit samples
  AUDIO_U16LSB = $0010; // Unsigned 16-bit samples
  AUDIO_S16LSB = $8010; // Signed 16-bit samples
  AUDIO_U16MSB = $1010; // As above, but big-endian byte order
  AUDIO_S16MSB = $9010; // As above, but big-endian byte order
  AUDIO_U16 = AUDIO_U16LSB;
  AUDIO_S16 = AUDIO_S16LSB;

  // SDL_cdrom.h constants
  // The maximum number of CD-ROM tracks on a disk
  SDL_MAX_TRACKS = 99;
  // The types of CD-ROM track possible
  SDL_AUDIO_TRACK = $00;
  SDL_DATA_TRACK = $04;

  // Conversion functions from frames to Minute/Second/Frames and vice versa
  CD_FPS = 75;
  // SDL_byteorder.h constants
  // The two types of endianness
  SDL_LIL_ENDIAN = 1234;
  SDL_BIG_ENDIAN = 4321;

{$IFDEF ENDIAN_LITTLE}
  SDL_BYTEORDER = SDL_LIL_ENDIAN;
  // Native audio byte ordering
  AUDIO_U16SYS = AUDIO_U16LSB;
  AUDIO_S16SYS = AUDIO_S16LSB;
{$ELSE}

  SDL_BYTEORDER = SDL_BIG_ENDIAN;
  // Native audio byte ordering
  AUDIO_U16SYS = AUDIO_U16MSB;
  AUDIO_S16SYS = AUDIO_S16MSB;
{$ENDIF}


  SDL_MIX_MAXVOLUME = 128;

  // SDL_joystick.h constants
  MAX_JOYSTICKS = 2; // only 2 are supported in the multimedia API
  MAX_AXES = 6; // each joystick can have up to 6 axes
  MAX_BUTTONS = 32; // and 32 buttons
  AXIS_MIN = -32768; // minimum value for axis coordinate
  AXIS_MAX = 32767; // maximum value for axis coordinate
  JOY_AXIS_THRESHOLD = (((AXIS_MAX) - (AXIS_MIN)) / 100); // 1% motion

  { Get the current state of a POV hat on a joystick
    The return value is one of the following positions: }
  SDL_HAT_CENTERED = $00;
  SDL_HAT_UP = $01;
  SDL_HAT_RIGHT = $02;
  SDL_HAT_DOWN = $04;
  SDL_HAT_LEFT = $08;
  SDL_HAT_RIGHTUP = SDL_HAT_RIGHT or SDL_HAT_UP;
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;
  SDL_HAT_LEFTUP = SDL_HAT_LEFT or SDL_HAT_UP;
  SDL_HAT_LEFTDOWN = SDL_HAT_LEFT or SDL_HAT_DOWN;

  // SDL_events.h constants
  SDL_NOEVENT = 0; // Unused (do not remove)
  SDL_ACTIVEEVENT = 1; // Application loses/gains visibility
  SDL_KEYDOWN = 2; // Keys pressed
  SDL_KEYUP = 3; // Keys released
  SDL_MOUSEMOTION = 4; // Mouse moved
  SDL_MOUSEBUTTONDOWN = 5; // Mouse button pressed
  SDL_MOUSEBUTTONUP = 6; // Mouse button released
  SDL_JOYAXISMOTION = 7; // Joystick axis motion
  SDL_JOYBALLMOTION = 8; // Joystick trackball motion
  SDL_JOYHATMOTION = 9; // Joystick hat position change
  SDL_JOYBUTTONDOWN = 10; // Joystick button pressed
  SDL_JOYBUTTONUP = 11; // Joystick button released
  SDL_QUITEV = 12; // User-requested quit ( Changed due to procedure conflict )
  SDL_SYSWMEVENT = 13; // System specific event
  SDL_EVENT_RESERVEDA = 14; // Reserved for future use..
  SDL_EVENT_RESERVED = 15; // Reserved for future use..
  SDL_VIDEORESIZE = 16; // User resized video mode
  SDL_VIDEOEXPOSE = 17; // Screen needs to be redrawn
  SDL_EVENT_RESERVED2 = 18; // Reserved for future use..
  SDL_EVENT_RESERVED3 = 19; // Reserved for future use..
  SDL_EVENT_RESERVED4 = 20; // Reserved for future use..
  SDL_EVENT_RESERVED5 = 21; // Reserved for future use..
  SDL_EVENT_RESERVED6 = 22; // Reserved for future use..
  SDL_EVENT_RESERVED7 = 23; // Reserved for future use..
  // Events SDL_USEREVENT through SDL_MAXEVENTS-1 are for your use
  SDL_USEREVENT = 24;
  // This last event is only for bounding internal arrays
  // It is the number of bits in the event mask datatype -- UInt32
  SDL_NUMEVENTS = 32;

  SDL_ALLEVENTS = $FFFFFFFF;

  SDL_ACTIVEEVENTMASK = 1 shl SDL_ACTIVEEVENT;
  SDL_KEYDOWNMASK = 1 shl SDL_KEYDOWN;
  SDL_KEYUPMASK = 1 shl SDL_KEYUP;
  SDL_MOUSEMOTIONMASK = 1 shl SDL_MOUSEMOTION;
  SDL_MOUSEBUTTONDOWNMASK = 1 shl SDL_MOUSEBUTTONDOWN;
  SDL_MOUSEBUTTONUPMASK = 1 shl SDL_MOUSEBUTTONUP;
  SDL_MOUSEEVENTMASK = 1 shl SDL_MOUSEMOTION or
    1 shl SDL_MOUSEBUTTONDOWN or
    1 shl SDL_MOUSEBUTTONUP;
  SDL_JOYAXISMOTIONMASK = 1 shl SDL_JOYAXISMOTION;
  SDL_JOYBALLMOTIONMASK = 1 shl SDL_JOYBALLMOTION;
  SDL_JOYHATMOTIONMASK = 1 shl SDL_JOYHATMOTION;
  SDL_JOYBUTTONDOWNMASK = 1 shl SDL_JOYBUTTONDOWN;
  SDL_JOYBUTTONUPMASK = 1 shl SDL_JOYBUTTONUP;
  SDL_JOYEVENTMASK = 1 shl SDL_JOYAXISMOTION or
    1 shl SDL_JOYBALLMOTION or
    1 shl SDL_JOYHATMOTION or
    1 shl SDL_JOYBUTTONDOWN or
    1 shl SDL_JOYBUTTONUP;
  SDL_VIDEORESIZEMASK = 1 shl SDL_VIDEORESIZE;
  SDL_QUITMASK = 1 shl SDL_QUITEV;
  SDL_SYSWMEVENTMASK = 1 shl SDL_SYSWMEVENT;

  { This function allows you to set the state of processing certain events.
    If 'state' is set to SDL_IGNORE, that event will be automatically dropped
    from the event queue and will not event be filtered.
    If 'state' is set to SDL_ENABLE, that event will be processed normally.
    If 'state' is set to SDL_QUERY, SDL_EventState() will return the
    current processing state of the specified event. }

  SDL_QUERY = -1;
  SDL_IGNORE = 0;
  SDL_DISABLE = 0;
  SDL_ENABLE = 1;

  //SDL_keyboard.h constants
  // This is the mask which refers to all hotkey bindings
  SDL_ALL_HOTKEYS = $FFFFFFFF;

{ Enable/Disable keyboard repeat.  Keyboard repeat defaults to off.
  'delay' is the initial delay in ms between the time when a key is
  pressed, and keyboard repeat begins.
  'interval' is the time in ms between keyboard repeat events. }

  SDL_DEFAULT_REPEAT_DELAY = 500;
  SDL_DEFAULT_REPEAT_INTERVAL = 30;

  // The keyboard syms have been cleverly chosen to map to ASCII
  SDLK_UNKNOWN = 0;
  SDLK_FIRST = 0;
  SDLK_BACKSPACE = 8;
  SDLK_TAB = 9;
  SDLK_CLEAR = 12;
  SDLK_RETURN = 13;
  SDLK_PAUSE = 19;
  SDLK_ESCAPE = 27;
  SDLK_SPACE = 32;
  SDLK_EXCLAIM = 33;
  SDLK_QUOTEDBL = 34;
  SDLK_HASH = 35;
  SDLK_DOLLAR = 36;
  SDLK_AMPERSAND = 38;
  SDLK_QUOTE = 39;
  SDLK_LEFTPAREN = 40;
  SDLK_RIGHTPAREN = 41;
  SDLK_ASTERISK = 42;
  SDLK_PLUS = 43;
  SDLK_COMMA = 44;
  SDLK_MINUS = 45;
  SDLK_PERIOD = 46;
  SDLK_SLASH = 47;
  SDLK_0 = 48;
  SDLK_1 = 49;
  SDLK_2 = 50;
  SDLK_3 = 51;
  SDLK_4 = 52;
  SDLK_5 = 53;
  SDLK_6 = 54;
  SDLK_7 = 55;
  SDLK_8 = 56;
  SDLK_9 = 57;
  SDLK_COLON = 58;
  SDLK_SEMICOLON = 59;
  SDLK_LESS = 60;
  SDLK_EQUALS = 61;
  SDLK_GREATER = 62;
  SDLK_QUESTION = 63;
  SDLK_AT = 64;

  { Skip uppercase letters }

  SDLK_LEFTBRACKET = 91;
  SDLK_BACKSLASH = 92;
  SDLK_RIGHTBRACKET = 93;
  SDLK_CARET = 94;
  SDLK_UNDERSCORE = 95;
  SDLK_BACKQUOTE = 96;
  SDLK_a = 97;
  SDLK_b = 98;
  SDLK_c = 99;
  SDLK_d = 100;
  SDLK_e = 101;
  SDLK_f = 102;
  SDLK_g = 103;
  SDLK_h = 104;
  SDLK_i = 105;
  SDLK_j = 106;
  SDLK_k = 107;
  SDLK_l = 108;
  SDLK_m = 109;
  SDLK_n = 110;
  SDLK_o = 111;
  SDLK_p = 112;
  SDLK_q = 113;
  SDLK_r = 114;
  SDLK_s = 115;
  SDLK_t = 116;
  SDLK_u = 117;
  SDLK_v = 118;
  SDLK_w = 119;
  SDLK_x = 120;
  SDLK_y = 121;
  SDLK_z = 122;
  SDLK_DELETE = 127;
  // End of ASCII mapped keysyms

  // International keyboard syms
  SDLK_WORLD_0 = 160; // 0xA0
  SDLK_WORLD_1 = 161;
  SDLK_WORLD_2 = 162;
  SDLK_WORLD_3 = 163;
  SDLK_WORLD_4 = 164;
  SDLK_WORLD_5 = 165;
  SDLK_WORLD_6 = 166;
  SDLK_WORLD_7 = 167;
  SDLK_WORLD_8 = 168;
  SDLK_WORLD_9 = 169;
  SDLK_WORLD_10 = 170;
  SDLK_WORLD_11 = 171;
  SDLK_WORLD_12 = 172;
  SDLK_WORLD_13 = 173;
  SDLK_WORLD_14 = 174;
  SDLK_WORLD_15 = 175;
  SDLK_WORLD_16 = 176;
  SDLK_WORLD_17 = 177;
  SDLK_WORLD_18 = 178;
  SDLK_WORLD_19 = 179;
  SDLK_WORLD_20 = 180;
  SDLK_WORLD_21 = 181;
  SDLK_WORLD_22 = 182;
  SDLK_WORLD_23 = 183;
  SDLK_WORLD_24 = 184;
  SDLK_WORLD_25 = 185;
  SDLK_WORLD_26 = 186;
  SDLK_WORLD_27 = 187;
  SDLK_WORLD_28 = 188;
  SDLK_WORLD_29 = 189;
  SDLK_WORLD_30 = 190;
  SDLK_WORLD_31 = 191;
  SDLK_WORLD_32 = 192;
  SDLK_WORLD_33 = 193;
  SDLK_WORLD_34 = 194;
  SDLK_WORLD_35 = 195;
  SDLK_WORLD_36 = 196;
  SDLK_WORLD_37 = 197;
  SDLK_WORLD_38 = 198;
  SDLK_WORLD_39 = 199;
  SDLK_WORLD_40 = 200;
  SDLK_WORLD_41 = 201;
  SDLK_WORLD_42 = 202;
  SDLK_WORLD_43 = 203;
  SDLK_WORLD_44 = 204;
  SDLK_WORLD_45 = 205;
  SDLK_WORLD_46 = 206;
  SDLK_WORLD_47 = 207;
  SDLK_WORLD_48 = 208;
  SDLK_WORLD_49 = 209;
  SDLK_WORLD_50 = 210;
  SDLK_WORLD_51 = 211;
  SDLK_WORLD_52 = 212;
  SDLK_WORLD_53 = 213;
  SDLK_WORLD_54 = 214;
  SDLK_WORLD_55 = 215;
  SDLK_WORLD_56 = 216;
  SDLK_WORLD_57 = 217;
  SDLK_WORLD_58 = 218;
  SDLK_WORLD_59 = 219;
  SDLK_WORLD_60 = 220;
  SDLK_WORLD_61 = 221;
  SDLK_WORLD_62 = 222;
  SDLK_WORLD_63 = 223;
  SDLK_WORLD_64 = 224;
  SDLK_WORLD_65 = 225;
  SDLK_WORLD_66 = 226;
  SDLK_WORLD_67 = 227;
  SDLK_WORLD_68 = 228;
  SDLK_WORLD_69 = 229;
  SDLK_WORLD_70 = 230;
  SDLK_WORLD_71 = 231;
  SDLK_WORLD_72 = 232;
  SDLK_WORLD_73 = 233;
  SDLK_WORLD_74 = 234;
  SDLK_WORLD_75 = 235;
  SDLK_WORLD_76 = 236;
  SDLK_WORLD_77 = 237;
  SDLK_WORLD_78 = 238;
  SDLK_WORLD_79 = 239;
  SDLK_WORLD_80 = 240;
  SDLK_WORLD_81 = 241;
  SDLK_WORLD_82 = 242;
  SDLK_WORLD_83 = 243;
  SDLK_WORLD_84 = 244;
  SDLK_WORLD_85 = 245;
  SDLK_WORLD_86 = 246;
  SDLK_WORLD_87 = 247;
  SDLK_WORLD_88 = 248;
  SDLK_WORLD_89 = 249;
  SDLK_WORLD_90 = 250;
  SDLK_WORLD_91 = 251;
  SDLK_WORLD_92 = 252;
  SDLK_WORLD_93 = 253;
  SDLK_WORLD_94 = 254;
  SDLK_WORLD_95 = 255; // 0xFF

  // Numeric keypad
  SDLK_KP0 = 256;
  SDLK_KP1 = 257;
  SDLK_KP2 = 258;
  SDLK_KP3 = 259;
  SDLK_KP4 = 260;
  SDLK_KP5 = 261;
  SDLK_KP6 = 262;
  SDLK_KP7 = 263;
  SDLK_KP8 = 264;
  SDLK_KP9 = 265;
  SDLK_KP_PERIOD = 266;
  SDLK_KP_DIVIDE = 267;
  SDLK_KP_MULTIPLY = 268;
  SDLK_KP_MINUS = 269;
  SDLK_KP_PLUS = 270;
  SDLK_KP_ENTER = 271;
  SDLK_KP_EQUALS = 272;

  // Arrows + Home/End pad
  SDLK_UP = 273;
  SDLK_DOWN = 274;
  SDLK_RIGHT = 275;
  SDLK_LEFT = 276;
  SDLK_INSERT = 277;
  SDLK_HOME = 278;
  SDLK_END = 279;
  SDLK_PAGEUP = 280;
  SDLK_PAGEDOWN = 281;

  // Function keys
  SDLK_F1 = 282;
  SDLK_F2 = 283;
  SDLK_F3 = 284;
  SDLK_F4 = 285;
  SDLK_F5 = 286;
  SDLK_F6 = 287;
  SDLK_F7 = 288;
  SDLK_F8 = 289;
  SDLK_F9 = 290;
  SDLK_F10 = 291;
  SDLK_F11 = 292;
  SDLK_F12 = 293;
  SDLK_F13 = 294;
  SDLK_F14 = 295;
  SDLK_F15 = 296;

  // Key state modifier keys
  SDLK_NUMLOCK = 300;
  SDLK_CAPSLOCK = 301;
  SDLK_SCROLLOCK = 302;
  SDLK_RSHIFT = 303;
  SDLK_LSHIFT = 304;
  SDLK_RCTRL = 305;
  SDLK_LCTRL = 306;
  SDLK_RALT = 307;
  SDLK_LALT = 308;
  SDLK_RMETA = 309;
  SDLK_LMETA = 310;
  SDLK_LSUPER = 311; // Left "Windows" key
  SDLK_RSUPER = 312; // Right "Windows" key
  SDLK_MODE = 313; // "Alt Gr" key
  SDLK_COMPOSE = 314; // Multi-key compose key

  // Miscellaneous function keys
  SDLK_HELP = 315;
  SDLK_PRINT = 316;
  SDLK_SYSREQ = 317;
  SDLK_BREAK = 318;
  SDLK_MENU = 319;
  SDLK_POWER = 320; // Power Macintosh power key
  SDLK_EURO = 321; // Some european keyboards

{$IFDEF GP2X}
SDLK_GP2X_UP = 0;
SDLK_GP2X_UPLEFT = 1;
SDLK_GP2X_LEFT = 2;
SDLK_GP2X_DOWNLEFT = 3;
SDLK_GP2X_DOWN = 4;
SDLK_GP2X_DOWNRIGHT = 5;
SDLK_GP2X_RIGHT = 6;
SDLK_GP2X_UPRIGHT = 7;
SDLK_GP2X_START = 8;
SDLK_GP2X_SELECT = 9;
SDLK_GP2X_L = 10;
SDLK_GP2X_R = 11;
SDLK_GP2X_A = 12;
SDLK_GP2X_B = 13;
SDLK_GP2X_Y = 14;
SDLK_GP2X_X = 15;
SDLK_GP2X_VOLUP = 16;
SDLK_GP2X_VOLDOWN = 17;
SDLK_GP2X_CLICK = 18;
{$ENDIF}

  // Enumeration of valid key mods (possibly OR'd together)
  KMOD_NONE = $0000;
  KMOD_LSHIFT = $0001;
  KMOD_RSHIFT = $0002;
  KMOD_LCTRL = $0040;
  KMOD_RCTRL = $0080;
  KMOD_LALT = $0100;
  KMOD_RALT = $0200;
  KMOD_LMETA = $0400;
  KMOD_RMETA = $0800;
  KMOD_NUM = $1000;
  KMOD_CAPS = $2000;
  KMOD_MODE = $4000;
  KMOD_RESERVED = $8000;

  KMOD_CTRL = (KMOD_LCTRL or KMOD_RCTRL);
  KMOD_SHIFT = (KMOD_LSHIFT or KMOD_RSHIFT);
  KMOD_ALT = (KMOD_LALT or KMOD_RALT);
  KMOD_META = (KMOD_LMETA or KMOD_RMETA);

  //SDL_video.h constants
  // Transparency definitions: These define alpha as the opacity of a surface */
  SDL_ALPHA_OPAQUE = 255;
  SDL_ALPHA_TRANSPARENT = 0;

  // These are the currently supported flags for the SDL_surface
  // Available for SDL_CreateRGBSurface() or SDL_SetVideoMode()
  SDL_SWSURFACE = $00000000; // Surface is in system memory
  SDL_HWSURFACE = $00000001; // Surface is in video memory
  SDL_ASYNCBLIT = $00000004; // Use asynchronous blits if possible
  // Available for SDL_SetVideoMode()
  SDL_ANYFORMAT = $10000000; // Allow any video depth/pixel-format
  SDL_HWPALETTE = $20000000; // Surface has exclusive palette
  SDL_DOUBLEBUF = $40000000; // Set up double-buffered video mode
  SDL_FULLSCREEN = $80000000; // Surface is a full screen display
  SDL_OPENGL = $00000002; // Create an OpenGL rendering context
  SDL_OPENGLBLIT = $00000002; // Create an OpenGL rendering context
  SDL_RESIZABLE = $00000010; // This video mode may be resized
  SDL_NOFRAME = $00000020; // No window caption or edge frame
  // Used internally (read-only)
  SDL_HWACCEL = $00000100; // Blit uses hardware acceleration
  SDL_SRCCOLORKEY = $00001000; // Blit uses a source color key
  SDL_RLEACCELOK = $00002000; // Private flag
  SDL_RLEACCEL = $00004000; // Colorkey blit is RLE accelerated
  SDL_SRCALPHA = $00010000; // Blit uses source alpha blending
  SDL_SRCCLIPPING = $00100000; // Blit uses source clipping
  SDL_PREALLOC = $01000000; // Surface uses preallocated memory

  { The most common video overlay formats.
    For an explanation of these pixel formats, see:
    http://www.webartz.com/fourcc/indexyuv.htm

   For information on the relationship between color spaces, see:
   http://www.neuro.sfc.keio.ac.jp/~aly/polygon/info/color-space-faq.html }

  SDL_YV12_OVERLAY = $32315659; // Planar mode: Y + V + U  (3 planes)
  SDL_IYUV_OVERLAY = $56555949; // Planar mode: Y + U + V  (3 planes)
  SDL_YUY2_OVERLAY = $32595559; // Packed mode: Y0+U0+Y1+V0 (1 plane)
  SDL_UYVY_OVERLAY = $59565955; // Packed mode: U0+Y0+V0+Y1 (1 plane)
  SDL_YVYU_OVERLAY = $55595659; // Packed mode: Y0+V0+Y1+U0 (1 plane)

  // flags for SDL_SetPalette()
  SDL_LOGPAL = $01;
  SDL_PHYSPAL = $02;

  //SDL_mouse.h constants
  { Used as a mask when testing buttons in buttonstate
    Button 1:	Left mouse button
    Button 2:	Middle mouse button
    Button 3:	Right mouse button
    Button 4:	Mouse Wheel Up (may also be a real button)
    Button 5:	Mouse Wheel Down (may also be a real button)
    Button 6:	Mouse X1 (may also be a real button)
    Button 7:	Mouse X2 (may also be a real button)
  }
  SDL_BUTTON_LEFT      = 1;
  SDL_BUTTON_MIDDLE    = 2;
  SDL_BUTTON_RIGHT     = 3;
  SDL_BUTTON_WHEELUP   = 4;
  SDL_BUTTON_WHEELDOWN = 5;
  SDL_BUTTON_X1        = 6;
  SDL_BUTTON_X2        = 7;

  SDL_BUTTON_LMASK = SDL_PRESSED shl (SDL_BUTTON_LEFT - 1);
  SDL_BUTTON_MMASK = SDL_PRESSED shl (SDL_BUTTON_MIDDLE - 1);
  SDL_BUTTON_RMASK = SDL_PRESSED shl (SDL_BUTTON_RIGHT - 1);
  SDL_BUTTON_X1MASK = SDL_PRESSED shl (SDL_BUTTON_X1 - 1);
  SDL_BUTTON_X2MASK = SDL_PRESSED shl (SDL_BUTTON_X2 - 1);

  // SDL_active.h constants
  // The available application states
  SDL_APPMOUSEFOCUS = $01; // The app has mouse coverage
  SDL_APPINPUTFOCUS = $02; // The app has input focus
  SDL_APPACTIVE = $04; // The application is active

  // SDL_mutex.h constants
  // Synchronization functions which can time out return this value
  //  they time out.

  SDL_MUTEX_TIMEDOUT = 1;

  // This is the timeout value which corresponds to never time out
  SDL_MUTEX_MAXWAIT = not Cardinal(0);

  {TSDL_GrabMode = (
    SDL_GRAB_QUERY,
    SDL_GRAB_OFF,
    SDL_GRAB_ON,
    SDL_GRAB_FULLSCREEN	); // Used internally}
  SDL_GRAB_QUERY = -1;
  SDL_GRAB_OFF   = 0;
  SDL_GRAB_ON    = 1;
  //SDL_GRAB_FULLSCREEN // Used internally

