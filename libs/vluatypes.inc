type
  lua_State = record end;
  Plua_State = ^lua_State;

type
  lua_CFunction = function (L : Plua_State) : Integer; cdecl;
  lua_Reader    = function (L : Plua_State; ud : Pointer; sz : PLongWord) : PChar; cdecl;
  lua_Writer    = function (L : Plua_State; const p : Pointer; sz : LongWord; ud : Pointer) : Integer; cdecl;
  lua_Alloc     = function (ud, ptr : Pointer; osize, nsize : LongWord) : Pointer; cdecl;

type
  lua_Number = Double;
  lua_Integer = Integer;

type
  lua_Chuckreader = type lua_Reader;
  lua_Chuckwriter = type lua_Writer;

type
  lua_Debug = packed record
    event : Integer;
    name : PChar;          (* (n) *)
    namewhat : PChar;      (* (n) `global', `local', `field', `method' *)
    what : PChar;          (* (S) `Lua', `C', `main', `tail' *)
    source : PChar;        (* (S) *)
    currentline : Integer; (* (l) *)
    nups : Integer;        (* (u) number of upvalues *)
    linedefined : Integer; (* (S) *)
    short_src : array [0..LUA_IDSIZE-1] of Char; (* (S) *)
    (* private part *)
    i_ci : Integer;        (* active function *)
  end;
  Plua_Debug = ^lua_Debug;

  (* Functions to be called by the debuger in specific events *)
  lua_Hook = procedure (L : Plua_State; ar : Plua_Debug); cdecl;

type
  luaL_Reg = packed record
    name : PChar;
    func : lua_CFunction;
  end;
  PluaL_Reg = ^luaL_Reg;

type
  luaL_Buffer = packed record
    p : PChar;       (* current position in buffer *)
    lvl : Integer;   (* number of strings in the stack (level) *)
    L : Plua_State;
    buffer : array [0..LUAL_BUFFERSIZE-1] of Char;
  end;
  PluaL_Buffer = ^luaL_Buffer;

