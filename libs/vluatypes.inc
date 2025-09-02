type
  lua_State  = record end;
  Plua_State = ^lua_State;
  size_t     = PtrUint;
  psize_t    = ^size_t;
  va_list    = Pointer;

type
  lua_CFunction = function (L : Plua_State) : LongInt; cdecl;
  lua_Reader    = function (L : Plua_State; ud : Pointer; sz : psize_t) : PChar; cdecl;
  lua_Writer    = function (L : Plua_State; const p : Pointer; sz : size_t; ud : Pointer) : LongInt; cdecl;
  lua_Alloc     = function (ud, ptr : Pointer; osize, nsize : size_t) : Pointer; cdecl;

type
  lua_Number    = double;
  lua_Integer   = int64;
  lua_Unsigned  = longword;
  Plua_Number   = ^lua_Number;
  Plua_Integer  = ^lua_Integer;
  Plua_Unsigned = ^lua_Unsigned;

type
  // Debug API
  lua_Debug = record
    event            : LongInt;
    name             : PAnsiChar; // (n)
    namewhat         : PAnsiChar; // (n) 'global','local','field','method'
    what             : PAnsiChar; // (S) 'Lua','C','main','tail'
    source           : PAnsiChar; // (S)
    currentline      : LongInt;   // (l)
    linedefined      : LongInt;   // (S)
    lastlinedefined  : LongInt;   // (S)
    nups             : Byte;      // (u)
    nparams          : Byte;      // (u)
    isvararg         : AnsiChar;  // (u)
    istailcall       : AnsiChar;  // (t)
    short_src        : array[0..LUA_IDSIZE-1] of AnsiChar; // (S)
    // private
    i_ci             : Pointer;
  end;
  Plua_Debug = ^lua_Debug;

  lua_Hook = procedure (aL: Plua_State; aAR: Plua_Debug); cdecl;

  // luaL helpers
  luaL_Reg = record
    name: PAnsiChar;
    func: lua_CFunction;
  end;
  PluaL_Reg = ^luaL_Reg;

  luaL_Buffer = record
    b    : PAnsiChar;
    size : size_t;
    n    : size_t;
    L    : Plua_State;
    initb: array[0..LUAL_BUFFERSIZE-1] of AnsiChar;
  end;
  PluaL_Buffer = ^luaL_Buffer;

  PFILE = Pointer; // opaque FILE*
  luaL_Stream = record
    f      : PFILE;
    closef : lua_CFunction;
  end;
  PluaL_Stream = ^luaL_Stream;

