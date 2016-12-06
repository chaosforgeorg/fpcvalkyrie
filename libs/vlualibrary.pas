unit vlualibrary;
{$include ../src/valkyrie.inc}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary;

type
  ELuaException         = class(Exception);

const
{$IFDEF LUA_DYNAMIC}
  {$IFDEF WINDOWS}
    LuaDefaultPath = 'lua5.1.dll';
  {$ELSE}
    {$IFDEF DARWIN}
      LuaDefaultPath = 'lua5.1.dylib';
    {$ELSE}
      LuaDefaultPath = 'lua5.1.so';
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  LuaDefaultPath = 'lua5.1.a';
  {$linklib lua5.1.a}
{$ENDIF}

{$include vluaconst.inc}
{$include vluatypes.inc}

{$DEFINE calldecl := cdecl}

{$IFDEF LUA_DYNAMIC}
var
  lua_newstate : function (f : lua_Alloc; ud : Pointer) : Plua_State; calldecl;
  lua_close : procedure (L: Plua_State); calldecl;
  lua_newthread : function (L : Plua_State) : Plua_State; calldecl;
  lua_atpanic : function (L : Plua_State; panicf : lua_CFunction) : lua_CFunction; calldecl;

  lua_gettop : function (L : Plua_State) : Integer; calldecl;
  lua_settop : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_pushvalue : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_remove : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_insert : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_replace : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_checkstack : function (L : Plua_State; sz : Integer) : LongBool; calldecl;
  lua_xmove : procedure (src, dest : Plua_State; n : Integer); calldecl;

  lua_isnumber : function (L : Plua_State; idx : Integer) : LongBool; calldecl;
  lua_isstring : function (L : Plua_State; idx : Integer) : LongBool; calldecl;
  lua_iscfunction : function (L : Plua_State; idx : Integer) : LongBool; calldecl;
  lua_isuserdata : function (L : Plua_State; idx : Integer) : LongBool; calldecl;
  lua_type : function (L : Plua_State; idx : Integer) : Integer; calldecl;
  lua_typename : function (L : Plua_State; tp : Integer) : PChar; calldecl;

  lua_equal : function (L : Plua_State; idx1, idx2 : Integer) : LongBool; calldecl;
  lua_rawequal : function (L : Plua_State; idx1, idx2 : Integer) : LongBool; calldecl;
  lua_lessthan : function (L : Plua_State; idx1, idx2 : Integer) : LongBool; calldecl;

  lua_tonumber : function (L : Plua_State; idx : Integer) : lua_Number; calldecl;
  lua_tointeger : function (L : Plua_State; idx : Integer) : lua_Integer; calldecl;
  lua_toboolean : function (L : Plua_State; idx : Integer) : LongBool; calldecl;
  lua_tolstring : function (L : Plua_State; idx : Integer; len : PLongWord) : PChar; calldecl;
  lua_objlen : function (L : Plua_State; idx : Integer) : LongWord; calldecl;
  lua_tocfunction : function (L : Plua_State; idx : Integer) : lua_CFunction; calldecl;
  lua_touserdata : function (L : Plua_State; idx : Integer) : Pointer; calldecl;
  lua_tothread : function (L : Plua_State; idx : Integer) : Plua_State; calldecl;
  lua_topointer : function (L : Plua_State; idx : Integer) : Pointer; calldecl;

  lua_pushnil : procedure (L : Plua_State); calldecl;
  lua_pushnumber : procedure (L : Plua_State; n : lua_Number); calldecl;
  lua_pushinteger : procedure (L : Plua_State; n : lua_Integer); calldecl;
  lua_pushlstring : procedure (L : Plua_State; const s : PChar; ls : LongWord); calldecl;
  lua_pushstring : procedure (L : Plua_State; const s : PChar); calldecl;
  lua_pushvfstring : function (L : Plua_State; const fmt : PChar; argp : Pointer) : PChar; calldecl;
  lua_pushfstring : function (L : Plua_State; const fmt : PChar) : PChar; varargs; calldecl;
  lua_pushcclosure : procedure (L : Plua_State; fn : lua_CFunction; n : Integer); calldecl;
  lua_pushboolean : procedure (L : Plua_State; b : LongBool); calldecl;
  lua_pushlightuserdata : procedure (L : Plua_State; p : Pointer); calldecl;
  lua_pushthread : function (L : Plua_state) : Cardinal; calldecl;

  lua_gettable : procedure (L : Plua_State ; idx : Integer); calldecl;
  lua_getfield : procedure (L : Plua_State; idx : Integer; k : PChar); calldecl;
  lua_rawget : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_rawgeti : procedure (L : Plua_State; idx, n : Integer); calldecl;
  lua_createtable : procedure (L : Plua_State; narr, nrec : Integer); calldecl;
  lua_newuserdata : function (L : Plua_State; sz : LongWord) : Pointer; calldecl;
  lua_getmetatable : function (L : Plua_State; objindex : Integer) : LongBool; calldecl;
  lua_getfenv : procedure (L : Plua_State; idx : Integer); calldecl;

  lua_settable : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_setfield : procedure (L : Plua_State; idx : Integer; const k : PChar); calldecl;
  lua_rawset : procedure (L : Plua_State; idx : Integer); calldecl;
  lua_rawseti : procedure (L : Plua_State; idx , n: Integer); calldecl;
  lua_setmetatable : function (L : Plua_State; objindex : Integer): LongBool; calldecl;
  lua_setfenv : function (L : Plua_State; idx : Integer): LongBool; calldecl;

  lua_call : procedure (L : Plua_State; nargs, nresults : Integer); calldecl;
  lua_pcall : function (L : Plua_State; nargs, nresults, errfunc : Integer) : Integer; calldecl;
  lua_cpcall : function (L : Plua_State; func : lua_CFunction; ud : Pointer) : Integer; calldecl;
  lua_load : function (L : Plua_State; reader : lua_Reader; dt : Pointer; const chunkname : PChar) : Integer; calldecl;
  lua_dump : function (L : Plua_State; writer : lua_Writer; data: Pointer) : Integer; calldecl;

  lua_yield : function (L : Plua_State; nresults : Integer) : Integer; calldecl;
  lua_resume : function (L : Plua_State; narg : Integer) : Integer; calldecl;
  lua_status : function (L : Plua_State) : Integer; calldecl;

  lua_gc : function (L : Plua_State; what, data : Integer) : Integer; calldecl;

  lua_error : function (L : Plua_State) : Integer; calldecl;
  lua_next : function (L : Plua_State; idx : Integer) : Integer; calldecl;
  lua_concat : procedure (L : Plua_State; n : Integer); calldecl;

  lua_getallocf : function (L : Plua_State; ud : PPointer) : lua_Alloc; calldecl;
  lua_setallocf : procedure (L : Plua_State; f : lua_Alloc; ud : Pointer); calldecl;

  lua_getstack : function (L : Plua_State; level : Integer; ar : Plua_Debug) : Integer; calldecl;
  lua_getinfo : function (L : Plua_State; const what : PChar; ar: Plua_Debug): Integer; calldecl;
  lua_getlocal : function (L : Plua_State; ar : Plua_Debug; n : Integer) : PChar; calldecl;
  lua_setlocal : function (L : Plua_State; ar : Plua_Debug; n : Integer) : PChar; calldecl;
  lua_getupvalue : function (L : Plua_State; funcindex, n : Integer) : PChar; calldecl;
  lua_setupvalue : function (L : Plua_State; funcindex, n : Integer) : PChar; calldecl;

  lua_sethook : function (L : Plua_State; func : lua_Hook; mask, count: Integer): Integer; calldecl;
  lua_gethookmask : function (L : Plua_State) : Integer; calldecl;
  lua_gethookcount : function (L : Plua_State) : Integer; calldecl;

  luaopen_base : function (L : Plua_State) : Integer; calldecl;
  luaopen_table : function (L : Plua_State) : Integer; calldecl;
  luaopen_io : function (L : Plua_State) : Integer; calldecl;
  luaopen_os : function (L : Plua_State) : Integer; calldecl;
  luaopen_string : function (L : Plua_State) : Integer; calldecl;
  luaopen_math : function (L : Plua_State) : Integer; calldecl;
  luaopen_debug : function (L : Plua_State) : Integer; calldecl;
  luaopen_package : function (L : Plua_State) : Integer; calldecl;
  luaL_openlibs : procedure (L : Plua_State); calldecl;

  luaL_openlib : procedure (L : Plua_State; const libname : PChar; const lr : PluaL_Reg; nup : Integer); calldecl;
  luaL_register : procedure (L : Plua_State; const libname : PChar; const lr : PluaL_Reg); calldecl;
  luaL_getmetafield : function (L : Plua_State; obj : Integer; const e : PChar) : Integer; calldecl;
  luaL_callmeta : function (L : Plua_State; obj : Integer; const e : PChar) : Integer; calldecl;
// removed - doesn't exists in Lua 5.2
//  luaL_typerror : function (L : Plua_State; narg : Integer; const tname : PChar) : Integer; calldecl;
  luaL_argerror : function (L : Plua_State; numarg : Integer; const extramsg : PChar) : Integer; calldecl;
  luaL_checklstring : function (L : Plua_State; numArg : Integer; ls : PLongWord) : PChar; calldecl;
  luaL_optlstring : function (L : Plua_State; numArg : Integer; const def: PChar; ls: PLongWord) : PChar; calldecl;
  luaL_checknumber : function (L : Plua_State; numArg : Integer) : lua_Number; calldecl;
  luaL_optnumber : function (L : Plua_State; nArg : Integer; def : lua_Number) : lua_Number; calldecl;

  luaL_checkinteger : function (L : Plua_State; numArg : Integer) : lua_Integer; calldecl;
  luaL_optinteger : function (L : Plua_State; nArg : Integer; def : lua_Integer) : lua_Integer; calldecl;

  luaL_checkstack : procedure (L : Plua_State; sz : Integer; const msg : PChar); calldecl;
  luaL_checktype : procedure (L : Plua_State; narg, t : Integer); calldecl;
  luaL_checkany : procedure (L : Plua_State; narg : Integer); calldecl;

  luaL_newmetatable : function (L : Plua_State; const tname : PChar) : Integer; calldecl;
  luaL_checkudata : function (L : Plua_State; ud : Integer; const tname : PChar) : Pointer; calldecl;
  luaL_where : procedure (L : Plua_State; lvl : Integer); calldecl;
  luaL_error : function (L : Plua_State; const fmt : PChar) : Integer; varargs; calldecl;
  luaL_checkoption : function (L : Plua_State; narg : Integer; const def : PChar; const lst : array of PChar) : Integer; calldecl;
  luaL_ref : function (L : Plua_State; t : Integer) : Integer; calldecl;
  luaL_unref : procedure (L : Plua_State; t, ref : Integer); calldecl;

  luaL_loadfile : function (L : Plua_State; const filename : PChar) : Integer; calldecl;
  luaL_loadbuffer : function (L : Plua_State; const buff : PChar; sz : LongWord; const name: PChar) : Integer; calldecl;
  luaL_loadstring : function (L : Plua_State; const s : Pchar) : Integer; calldecl;
  luaL_newstate : function : Plua_State; calldecl;
  luaL_gsub : function (L : Plua_State; const s, p, r : PChar) : PChar; calldecl;
  luaL_findtable : function (L : Plua_State; idx : Integer; const fname : PChar; szhint : Integer) : PChar; calldecl;

  luaL_buffinit : procedure (L : Plua_State; B : PluaL_Buffer); calldecl;
  luaL_prepbuffer : function (B : PluaL_Buffer) : PChar; calldecl;
  luaL_addlstring : procedure (B : PluaL_Buffer; const s : PChar; ls : LongWord); calldecl;
  luaL_addstring : procedure (B : PluaL_Buffer; const s : PChar); calldecl;
  luaL_addvalue : procedure (B : PluaL_Buffer); calldecl;
  luaL_pushresult : procedure (B : PluaL_Buffer); calldecl;
{$ELSE}
function  lua_newstate(f : lua_Alloc; ud : Pointer) : Plua_State; calldecl; external;
procedure lua_close(L: Plua_State); calldecl; external;
function  lua_newthread(L : Plua_State) : Plua_State; calldecl; external;
function  lua_atpanic(L : Plua_State; panicf : lua_CFunction) : lua_CFunction; calldecl; external;

function  lua_gettop(L : Plua_State) : Integer; calldecl; external;
procedure lua_settop(L : Plua_State; idx : Integer); calldecl; external;
procedure lua_pushvalue(L : Plua_State; idx : Integer); calldecl; external;
procedure lua_remove(L : Plua_State; idx : Integer); calldecl; external;
procedure lua_insert(L : Plua_State; idx : Integer); calldecl; external;
procedure lua_replace(L : Plua_State; idx : Integer); calldecl; external;
function  lua_checkstack(L : Plua_State; sz : Integer) : LongBool; calldecl; external;
procedure lua_xmove(src, dest : Plua_State; n : Integer); calldecl; external;

function lua_isnumber(L : Plua_State; idx : Integer) : LongBool; calldecl; external;
function lua_isstring(L : Plua_State; idx : Integer) : LongBool; calldecl; external;
function lua_iscfunction(L : Plua_State; idx : Integer) : LongBool; calldecl; external;
function lua_isuserdata(L : Plua_State; idx : Integer) : LongBool; calldecl; external;
function lua_type(L : Plua_State; idx : Integer) : Integer; calldecl; external;
function lua_typename(L : Plua_State; tp : Integer) : PChar; calldecl; external;

function lua_equal(L : Plua_State; idx1, idx2 : Integer) : LongBool; calldecl; external;
function lua_rawequal(L : Plua_State; idx1, idx2 : Integer) : LongBool; calldecl; external;
function lua_lessthan(L : Plua_State; idx1, idx2 : Integer) : LongBool; calldecl; external;

function lua_tonumber(L : Plua_State; idx : Integer) : lua_Number; calldecl; external;
function lua_tointeger(L : Plua_State; idx : Integer) : lua_Integer; calldecl; external;
function lua_toboolean(L : Plua_State; idx : Integer) : LongBool; calldecl; external;
function lua_tolstring(L : Plua_State; idx : Integer; len : PLongWord) : PChar; calldecl; external;
function lua_objlen(L : Plua_State; idx : Integer) : LongWord; calldecl; external;
function lua_tocfunction(L : Plua_State; idx : Integer) : lua_CFunction; calldecl; external;
function lua_touserdata(L : Plua_State; idx : Integer) : Pointer; calldecl; external;
function lua_tothread(L : Plua_State; idx : Integer) : Plua_State; calldecl; external;
function lua_topointer(L : Plua_State; idx : Integer) : Pointer; calldecl; external;

procedure lua_pushnil(L : Plua_State); calldecl; external;
procedure lua_pushnumber(L : Plua_State; n : lua_Number); calldecl; external;
procedure lua_pushinteger(L : Plua_State; n : lua_Integer); calldecl; external;
procedure lua_pushlstring(L : Plua_State; const s : PChar; ls : LongWord); calldecl; external;
procedure lua_pushstring(L : Plua_State; const s : PChar); calldecl; external;
function  lua_pushvfstring(L : Plua_State; const fmt : PChar; argp : Pointer) : PChar; calldecl; external;
function  lua_pushfstring(L : Plua_State; const fmt : PChar) : PChar; varargs; calldecl; external;
procedure lua_pushcclosure(L : Plua_State; fn : lua_CFunction; n : Integer); calldecl; external;
procedure lua_pushboolean(L : Plua_State; b : LongBool); calldecl; external;
procedure lua_pushlightuserdata(L : Plua_State; p : Pointer); calldecl; external;
function  lua_pushthread(L : Plua_state) : Cardinal; calldecl; external;

procedure lua_gettable(L : Plua_State ; idx : Integer); calldecl; external;
procedure lua_getfield(L : Plua_State; idx : Integer; k : PChar); calldecl; external;
procedure lua_rawget(L : Plua_State; idx : Integer); calldecl; external;
procedure lua_rawgeti(L : Plua_State; idx, n : Integer); calldecl; external;
procedure lua_createtable(L : Plua_State; narr, nrec : Integer); calldecl; external;
function  lua_newuserdata(L : Plua_State; sz : LongWord) : Pointer; calldecl; external;
function  lua_getmetatable(L : Plua_State; objindex : Integer) : LongBool; calldecl; external;
procedure lua_getfenv(L : Plua_State; idx : Integer); calldecl; external;

procedure lua_settable(L : Plua_State; idx : Integer); calldecl; external;
procedure lua_setfield(L : Plua_State; idx : Integer; const k : PChar); calldecl; external;
procedure lua_rawset(L : Plua_State; idx : Integer); calldecl; external;
procedure lua_rawseti(L : Plua_State; idx , n: Integer); calldecl; external;
function lua_setmetatable(L : Plua_State; objindex : Integer): LongBool; calldecl; external;
function lua_setfenv(L : Plua_State; idx : Integer): LongBool; calldecl; external;

procedure lua_call(L : Plua_State; nargs, nresults : Integer); calldecl; external;
function  lua_pcall(L : Plua_State; nargs, nresults, errfunc : Integer) : Integer; calldecl; external;
function  lua_cpcall(L : Plua_State; func : lua_CFunction; ud : Pointer) : Integer; calldecl; external;
function  lua_load(L : Plua_State; reader : lua_Reader; dt : Pointer; const chunkname : PChar) : Integer; calldecl; external;
function lua_dump(L : Plua_State; writer : lua_Writer; data: Pointer) : Integer; calldecl; external;

function lua_yield(L : Plua_State; nresults : Integer) : Integer; calldecl; external;
function lua_resume(L : Plua_State; narg : Integer) : Integer; calldecl; external;
function lua_status(L : Plua_State) : Integer; calldecl; external;

function lua_gc(L : Plua_State; what, data : Integer) : Integer; calldecl; external;

function lua_error(L : Plua_State) : Integer; calldecl; external;
function lua_next(L : Plua_State; idx : Integer) : Integer; calldecl; external;
procedure lua_concat(L : Plua_State; n : Integer); calldecl; external;

function  lua_getallocf(L : Plua_State; ud : PPointer) : lua_Alloc; calldecl; external;
procedure lua_setallocf(L : Plua_State; f : lua_Alloc; ud : Pointer); calldecl; external;

function lua_getstack(L : Plua_State; level : Integer; ar : Plua_Debug) : Integer; calldecl; external;
function lua_getinfo(L : Plua_State; const what : PChar; ar: Plua_Debug): Integer; calldecl; external;
function lua_getlocal(L : Plua_State; ar : Plua_Debug; n : Integer) : PChar; calldecl; external;
function lua_setlocal(L : Plua_State; ar : Plua_Debug; n : Integer) : PChar; calldecl; external;
function lua_getupvalue(L : Plua_State; funcindex, n : Integer) : PChar; calldecl; external;
function lua_setupvalue(L : Plua_State; funcindex, n : Integer) : PChar; calldecl; external;

function lua_sethook(L : Plua_State; func : lua_Hook; mask, count: Integer): Integer; calldecl; external;
function lua_gethookmask(L : Plua_State) : Integer; calldecl; external;
function lua_gethookcount(L : Plua_State) : Integer; calldecl; external;

function luaopen_base(L : Plua_State) : Integer; calldecl; external;
function luaopen_table(L : Plua_State) : Integer; calldecl; external;
function luaopen_io(L : Plua_State) : Integer; calldecl; external;
function luaopen_os(L : Plua_State) : Integer; calldecl; external;
function luaopen_string(L : Plua_State) : Integer; calldecl; external;
function luaopen_math(L : Plua_State) : Integer; calldecl; external;
function luaopen_debug(L : Plua_State) : Integer; calldecl; external;
function luaopen_package(L : Plua_State) : Integer; calldecl; external;
procedure luaL_openlibs(L : Plua_State); calldecl; external;

procedure luaL_openlib(L : Plua_State; const libname : PChar; const lr : PluaL_Reg; nup : Integer); calldecl; external;
procedure luaL_register(L : Plua_State; const libname : PChar; const lr : PluaL_Reg); calldecl; external;
function luaL_getmetafield(L : Plua_State; obj : Integer; const e : PChar) : Integer; calldecl; external;
function luaL_callmeta(L : Plua_State; obj : Integer; const e : PChar) : Integer; calldecl; external;
// removed - doesn't exists in Lua 5.2
//function luaL_typerror(L : Plua_State; narg : Integer; const tname : PChar) : Integer; calldecl; external;
function luaL_argerror(L : Plua_State; numarg : Integer; const extramsg : PChar) : Integer; calldecl; external;
function luaL_checklstring(L : Plua_State; numArg : Integer; ls : PLongWord) : PChar; calldecl; external;
function luaL_optlstring(L : Plua_State; numArg : Integer; const def: PChar; ls: PLongWord) : PChar; calldecl; external;
function luaL_checknumber(L : Plua_State; numArg : Integer) : lua_Number; calldecl; external;
function luaL_optnumber(L : Plua_State; nArg : Integer; def : lua_Number) : lua_Number; calldecl; external;

function luaL_checkinteger(L : Plua_State; numArg : Integer) : lua_Integer; calldecl; external;
function luaL_optinteger(L : Plua_State; nArg : Integer; def : lua_Integer) : lua_Integer; calldecl; external;

procedure luaL_checkstack(L : Plua_State; sz : Integer; const msg : PChar); calldecl; external;
procedure luaL_checktype(L : Plua_State; narg, t : Integer); calldecl; external;
procedure luaL_checkany(L : Plua_State; narg : Integer); calldecl; external;

function luaL_newmetatable(L : Plua_State; const tname : PChar) : Integer; calldecl; external;
function luaL_checkudata(L : Plua_State; ud : Integer; const tname : PChar) : Pointer; calldecl; external;
procedure luaL_where(L : Plua_State; lvl : Integer); calldecl; external;
function  luaL_error(L : Plua_State; const fmt : PChar) : Integer; varargs; calldecl; external;
function luaL_checkoption(L : Plua_State; narg : Integer; const def : PChar; const lst : array of PChar) : Integer; calldecl; external;
function  luaL_ref(L : Plua_State; t : Integer) : Integer; calldecl; external;
procedure luaL_unref(L : Plua_State; t, ref : Integer); calldecl; external;

function luaL_loadfile(L : Plua_State; const filename : PChar) : Integer; calldecl; external;
function luaL_loadbuffer(L : Plua_State; const buff : PChar; sz : LongWord; const name: PChar) : Integer; calldecl; external;
function luaL_loadstring(L : Plua_State; const s : Pchar) : Integer; calldecl; external;
function luaL_newstate : Plua_State; calldecl; external;
function luaL_gsub(L : Plua_State; const s, p, r : PChar) : PChar; calldecl; external;
function luaL_findtable(L : Plua_State; idx : Integer; const fname : PChar; szhint : Integer) : PChar; calldecl; external;

procedure luaL_buffinit(L : Plua_State; B : PluaL_Buffer); calldecl; external;
function  luaL_prepbuffer(B : PluaL_Buffer) : PChar; calldecl; external;
procedure luaL_addlstring(B : PluaL_Buffer; const s : PChar; ls : LongWord); calldecl; external;
procedure luaL_addstring(B : PluaL_Buffer; const s : PChar); calldecl; external;
procedure luaL_addvalue(B : PluaL_Buffer); calldecl; external;
procedure luaL_pushresult(B : PluaL_Buffer); calldecl; external;
{$ENDIF}

function lua_upvalueindex(idx : Integer) : Integer;
procedure lua_pop(L : Plua_State; n : Integer);
procedure lua_newtable(L : Plua_State);
procedure lua_register(L : Plua_State; n : AnsiString; f : lua_CFunction);
procedure lua_pushcfunction(L : Plua_State; f : lua_CFunction);
function  lua_strlen(L : Plua_State; idx : Integer) : Integer;
function lua_isfunction(L : Plua_State; n : Integer) : Boolean;
function lua_istable(L : Plua_State; n : Integer) : Boolean;
function lua_islightuserdata(L : Plua_State; n : Integer) : Boolean;
function lua_isnil(L : Plua_State; n : Integer) : Boolean;
function lua_isboolean(L : Plua_State; n : Integer) : Boolean;
function lua_isthread(L : Plua_State; n : Integer) : Boolean;
function lua_isnone(L : Plua_State; n : Integer) : Boolean;
function lua_isnoneornil(L : Plua_State; n : Integer) : Boolean;
procedure lua_pushliteral(L : Plua_State; s : Ansistring);
procedure lua_pushansistring(L: Plua_State; s: Ansistring);
procedure lua_setglobal(L : Plua_State; s : AnsiString);
procedure lua_getglobal(L: Plua_State; s : AnsiString);
function lua_tostring(L : Plua_State; idx : Integer) : AnsiString;
function lua_absindex(L: Plua_State; idx: Integer): Integer;
function lua_open : Plua_State;
procedure lua_getregistry(L : Plua_State);
function lua_getgccount(L : Plua_State) : Integer;
function luaL_getn(L : Plua_State; idx : Integer) : Integer;
function luaL_argcheck(L : Plua_State; cond : Boolean; numarg : Integer; extramsg : PChar): Integer;
function luaL_checkstring(L : Plua_State; n : Integer) : PChar;
function luaL_optstring(L : Plua_State; n : Integer; d : PChar) : PChar;
function luaL_checkint(L : Plua_State; n : Integer) : Integer;
function luaL_optint(L : Plua_State; n, d : Integer): Integer;
function luaL_checklong(L : Plua_State; n : LongInt) : LongInt;
function luaL_optlong(L : Plua_State; n : Integer; d : LongInt) : LongInt;
function luaL_typename(L : Plua_State; idx : Integer) : PChar;
function luaL_dofile(L : Plua_State; fn : PChar) : Integer;
function luaL_dostring(L : Plua_State; s : PChar) : Integer;

procedure luaL_getmetatable(L : Plua_State; n : PChar);
procedure luaL_addchar(B : PluaL_Buffer; c : Char);
procedure luaL_addsize(B : PluaL_Buffer; n : Integer);
function luaL_testudata(L : Plua_State; ud : Integer; const tname : PChar) : Pointer;

function lua_ref(L : Plua_State; lock : boolean) : Integer;
procedure lua_unref(L : Plua_State; ref : Integer);
procedure lua_getref(L : Plua_State; ref : Integer);

// these functions are introduced to prevent the usage of deprecated LUA_GLOBALSINDEX
procedure lua_push_global( L : PLua_State );
procedure lua_rawset_global( L : PLua_State );
procedure lua_rawget_global( L : PLua_State );

var
  Lua : TLibrary = nil;

function LoadLua( const aPath : AnsiString = LuaDefaultPath ) : Boolean;

implementation

function LoadLua ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := Lua.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'Lua : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  {$IFDEF LUA_STATIC}
  Exit( True );
  {$ELSE}

  if Lua <> nil then Exit( True );
  Lua := TLibrary.Load( aPath );
  if Lua = nil then Exit( False );

  Pointer(lua_newstate)  := GetSymbol('lua_newstate');
  Pointer(lua_close)  := GetSymbol('lua_close');
  Pointer(lua_newthread)  := GetSymbol('lua_newthread');
  Pointer(lua_atpanic)  := GetSymbol('lua_atpanic');

  Pointer(lua_gettop)  := GetSymbol('lua_gettop');
  Pointer(lua_settop)  := GetSymbol('lua_settop');
  Pointer(lua_pushvalue)  := GetSymbol('lua_pushvalue');
  Pointer(lua_remove)  := GetSymbol('lua_remove');
  Pointer(lua_insert)  := GetSymbol('lua_insert');
  Pointer(lua_replace)  := GetSymbol('lua_replace');
  Pointer(lua_checkstack)  := GetSymbol('lua_checkstack');
  Pointer(lua_xmove)  := GetSymbol('lua_xmove');

  Pointer(lua_isnumber)  := GetSymbol('lua_isnumber');
  Pointer(lua_isstring)  := GetSymbol('lua_isstring');
  Pointer(lua_iscfunction)  := GetSymbol('lua_iscfunction');
  Pointer(lua_isuserdata)  := GetSymbol('lua_isuserdata');
  Pointer(lua_type)  := GetSymbol('lua_type');
  Pointer(lua_typename)  := GetSymbol('lua_typename');

  Pointer(lua_equal)  := GetSymbol('lua_equal');
  Pointer(lua_rawequal)  := GetSymbol('lua_rawequal');
  Pointer(lua_lessthan)  := GetSymbol('lua_lessthan');

  Pointer(lua_tonumber)  := GetSymbol('lua_tonumber');
  Pointer(lua_tointeger)  := GetSymbol('lua_tointeger');
  Pointer(lua_toboolean)  := GetSymbol('lua_toboolean');
  Pointer(lua_tolstring)  := GetSymbol('lua_tolstring');
  Pointer(lua_objlen)  := GetSymbol('lua_objlen');
  Pointer(lua_tocfunction)  := GetSymbol('lua_tocfunction');
  Pointer(lua_touserdata)  := GetSymbol('lua_touserdata');
  Pointer(lua_tothread)  := GetSymbol('lua_tothread');
  Pointer(lua_topointer)  := GetSymbol('lua_topointer');

  Pointer(lua_pushnil)  := GetSymbol('lua_pushnil');
  Pointer(lua_pushnumber)  := GetSymbol('lua_pushnumber');
  Pointer(lua_pushinteger)  := GetSymbol('lua_pushinteger');
  Pointer(lua_pushlstring)  := GetSymbol('lua_pushlstring');
  Pointer(lua_pushstring)  := GetSymbol('lua_pushstring');
  Pointer(lua_pushvfstring)  := GetSymbol('lua_pushvfstring');
  Pointer(lua_pushfstring)  := GetSymbol('lua_pushfstring');
  Pointer(lua_pushcclosure)  := GetSymbol('lua_pushcclosure');
  Pointer(lua_pushboolean)  := GetSymbol('lua_pushboolean');
  Pointer(lua_pushlightuserdata)  := GetSymbol('lua_pushlightuserdata');
  Pointer(lua_pushthread)  := GetSymbol('lua_pushthread');

  Pointer(lua_gettable)  := GetSymbol('lua_gettable');
  Pointer(lua_getfield)  := GetSymbol('lua_getfield');
  Pointer(lua_rawget)  := GetSymbol('lua_rawget');
  Pointer(lua_rawgeti)  := GetSymbol('lua_rawgeti');
  Pointer(lua_createtable)  := GetSymbol('lua_createtable');
  Pointer(lua_newuserdata)  := GetSymbol('lua_newuserdata');
  Pointer(lua_getmetatable)  := GetSymbol('lua_getmetatable');
  Pointer(lua_getfenv)  := GetSymbol('lua_getfenv');

  Pointer(lua_settable)  := GetSymbol('lua_settable');
  Pointer(lua_setfield)  := GetSymbol('lua_setfield');
  Pointer(lua_rawset)  := GetSymbol('lua_rawset');
  Pointer(lua_rawseti)  := GetSymbol('lua_rawseti');
  Pointer(lua_setmetatable)  := GetSymbol('lua_setmetatable');
  Pointer(lua_setfenv)  := GetSymbol('lua_setfenv');

  Pointer(lua_call)  := GetSymbol('lua_call');
  Pointer(lua_pcall)  := GetSymbol('lua_pcall');
  Pointer(lua_cpcall)  := GetSymbol('lua_cpcall');
  Pointer(lua_load)  := GetSymbol('lua_load');
  Pointer(lua_dump)  := GetSymbol('lua_dump');

  Pointer(lua_yield)  := GetSymbol('lua_yield');
  Pointer(lua_resume)  := GetSymbol('lua_resume');
  Pointer(lua_status)  := GetSymbol('lua_status');

  Pointer(lua_gc)  := GetSymbol('lua_gc');

  Pointer(lua_error)  := GetSymbol('lua_error');
  Pointer(lua_next)  := GetSymbol('lua_next');
  Pointer(lua_concat)  := GetSymbol('lua_concat');

  Pointer(lua_getallocf)  := GetSymbol('lua_getallocf');
  Pointer(lua_setallocf)  := GetSymbol('lua_setallocf');

  Pointer(lua_getstack)  := GetSymbol('lua_getstack');
  Pointer(lua_getinfo)  := GetSymbol('lua_getinfo');
  Pointer(lua_getlocal)  := GetSymbol('lua_getlocal');
  Pointer(lua_setlocal)  := GetSymbol('lua_setlocal');
  Pointer(lua_getupvalue)  := GetSymbol('lua_getupvalue');
  Pointer(lua_setupvalue)  := GetSymbol('lua_setupvalue');

  Pointer(lua_sethook)  := GetSymbol('lua_sethook');
  Pointer(lua_gethookmask)  := GetSymbol('lua_gethookmask');
  Pointer(lua_gethookcount)  := GetSymbol('lua_gethookcount');

  Pointer(luaopen_base)  := GetSymbol('luaopen_base');
  Pointer(luaopen_table)  := GetSymbol('luaopen_table');
  Pointer(luaopen_io)  := GetSymbol('luaopen_io');
  Pointer(luaopen_os)  := GetSymbol('luaopen_os');
  Pointer(luaopen_string)  := GetSymbol('luaopen_string');
  Pointer(luaopen_math)  := GetSymbol('luaopen_math');
  Pointer(luaopen_debug)  := GetSymbol('luaopen_debug');
  Pointer(luaopen_package)  := GetSymbol('luaopen_package');
  Pointer(luaL_openlibs)  := GetSymbol('luaL_openlibs');

  Pointer(luaL_openlib)  := GetSymbol('luaL_openlib');
  Pointer(luaL_register)  := GetSymbol('luaL_register');
  Pointer(luaL_getmetafield)  := GetSymbol('luaL_getmetafield');
  Pointer(luaL_callmeta)  := GetSymbol('luaL_callmeta');
// removed - doesn't exists in Lua 5.2
//  Pointer(luaL_typerror)  := GetSymbol('luaL_typerror');
  Pointer(luaL_argerror)  := GetSymbol('luaL_argerror');
  Pointer(luaL_checklstring)  := GetSymbol('luaL_checklstring');
  Pointer(luaL_optlstring)  := GetSymbol('luaL_optlstring');
  Pointer(luaL_checknumber)  := GetSymbol('luaL_checknumber');
  Pointer(luaL_optnumber)  := GetSymbol('luaL_optnumber');

  Pointer(luaL_checkinteger)  := GetSymbol('luaL_checkinteger');
  Pointer(luaL_optinteger)  := GetSymbol('luaL_optinteger');

  Pointer(luaL_checkstack)  := GetSymbol('luaL_checkstack');
  Pointer(luaL_checktype)  := GetSymbol('luaL_checktype');
  Pointer(luaL_checkany)  := GetSymbol('luaL_checkany');

  Pointer(luaL_newmetatable)  := GetSymbol('luaL_newmetatable');
  Pointer(luaL_checkudata)  := GetSymbol('luaL_checkudata');
  Pointer(luaL_where)  := GetSymbol('luaL_where');
  Pointer(luaL_error)  := GetSymbol('luaL_error');
  Pointer(luaL_checkoption)  := GetSymbol('luaL_checkoption');
  Pointer(luaL_ref)  := GetSymbol('luaL_ref');
  Pointer(luaL_unref)  := GetSymbol('luaL_unref');

  Pointer(luaL_loadfile)  := GetSymbol('luaL_loadfile');
  Pointer(luaL_loadbuffer)  := GetSymbol('luaL_loadbuffer');
  Pointer(luaL_loadstring)  := GetSymbol('luaL_loadstring');
  Pointer(luaL_newstate)  := GetSymbol('luaL_newstate');
  Pointer(luaL_gsub)  := GetSymbol('luaL_gsub');
  Pointer(luaL_findtable)  := GetSymbol('luaL_findtable');

  Pointer(luaL_buffinit)  := GetSymbol('luaL_buffinit');
  Pointer(luaL_prepbuffer)  := GetSymbol('luaL_prepbuffer');
  Pointer(luaL_addlstring)  := GetSymbol('luaL_addlstring');
  Pointer(luaL_addstring)  := GetSymbol('luaL_addstring');
  Pointer(luaL_addvalue)  := GetSymbol('luaL_addvalue');
  Pointer(luaL_pushresult)  := GetSymbol('luaL_pushresult');
  {$ENDIF}
  Exit( True );
end;

function lua_upvalueindex(idx : Integer) : Integer;
begin
  lua_upvalueindex := LUA_GLOBALSINDEX - idx;
end;

procedure lua_pop(L : Plua_State; n : Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L : Plua_State);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L : Plua_State; n : AnsiString; f : lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, PChar(n) );
end;

procedure lua_pushcfunction(L : Plua_State; f : lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function  lua_strlen(L : Plua_State; idx : Integer) : Integer;
begin
  lua_strlen := lua_objlen(L, idx);
end;

function lua_isfunction(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isfunction := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L : Plua_State; n : Integer) : Boolean;
begin
  lua_istable := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L : Plua_State; n : Integer) : Boolean;
begin
  lua_islightuserdata := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnil := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isboolean := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isthread := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnone := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnoneornil := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L : Plua_State; s : Ansistring);
begin
  lua_pushlstring(L, PChar(s), Length(s) );
end;

procedure lua_pushansistring(L: Plua_State; s: Ansistring);
begin
  lua_pushstring(L, PChar(s));
end;

procedure lua_setglobal(L : Plua_State; s : AnsiString);
begin
  lua_setfield(L, LUA_GLOBALSINDEX, PChar(s));
end;

procedure lua_getglobal(L: Plua_State; s : AnsiString);
begin
  lua_getfield(L, LUA_GLOBALSINDEX, PChar(s));
end;

function lua_tostring(L : Plua_State; idx : Integer) : AnsiString;
var size  : Integer;
    ltype : Integer;
begin
  ltype := lua_type( L, idx );
  if (ltype <> LUA_TSTRING) and (ltype <> LUA_TNUMBER) then Exit('');
  size := lua_strlen( L, idx );
  SetLength( Result, size );
  if size > 0 then Move( lua_tolstring( L, idx, nil )^, Result[1], size );
end;

function lua_absindex(L: Plua_State; idx: Integer): Integer;
begin
  if (idx > -1) or ((idx = LUA_GLOBALSINDEX) or (idx = LUA_REGISTRYINDEX)) then Exit( idx );
  Exit( idx + lua_gettop(L) + 1 );
end;

function lua_open : Plua_State;
begin
  LoadLua;
  lua_open := luaL_newstate();
end;

procedure lua_getregistry(L : Plua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

function lua_getgccount(L : Plua_State) : Integer;
begin
  lua_getgccount := lua_gc(L, LUA_GCCOUNT, 0);
end;

function luaL_getn(L : Plua_State; idx : Integer) : Integer;
begin
  luaL_getn := lua_objlen(L, idx);
end;

function luaL_argcheck(L : Plua_State; cond : Boolean; numarg : Integer;
                       extramsg : PChar): Integer;
begin
  if not cond then
    luaL_argcheck := luaL_argerror(L, numarg, extramsg)
  else
    luaL_argcheck := 0;
end;

function luaL_checkstring(L : Plua_State; n : Integer) : PChar;
begin
  luaL_checkstring := luaL_checklstring(L, n, nil);
end;

function luaL_optstring(L : Plua_State; n : Integer; d : PChar) : PChar;
begin
  luaL_optstring := luaL_optlstring(L, n, d, nil);
end;

function luaL_checkint(L : Plua_State; n : Integer) : Integer;
begin
  luaL_checkint := luaL_checkinteger(L, n);
end;

function luaL_optint(L : Plua_State; n, d : Integer): Integer;
begin
  luaL_optint := luaL_optinteger(L, n, d);
end;

function luaL_checklong(L : Plua_State; n : LongInt) : LongInt;
begin
  luaL_checklong := luaL_checkinteger(L, n);
end;

function luaL_optlong(L : Plua_State; n : Integer; d : LongInt) : LongInt;
begin
  luaL_optlong := luaL_optinteger(L, n, d);
end;

function luaL_typename(L : Plua_State; idx : Integer) : PChar;
begin
  luaL_typename := lua_typename( L, lua_type(L, idx) );
end;

function luaL_dofile(L : Plua_State; fn : PChar) : Integer;
Var
  Res : Integer;
begin
  Res := luaL_loadfile(L, fn);
  if Res = 0 then
    Res := lua_pcall(L, 0, 0, 0);
  Result := Res;
end;

function luaL_dostring(L : Plua_State; s : PChar) : Integer;
Var
  Res : Integer;
begin
  Res := luaL_loadstring(L, s);
  if Res = 0 then
    Res := lua_pcall(L, 0, 0, 0);
  Result := Res;
end;

procedure luaL_getmetatable(L : Plua_State; n : PChar);
begin
  lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

procedure luaL_addchar(B : PluaL_Buffer; c : Char);
begin
  if not(B^.p < B^.buffer + LUAL_BUFFERSIZE) then
    luaL_prepbuffer(B);
  B^.p^ := c;
  Inc(B^.p);
end;

procedure luaL_addsize(B : PluaL_Buffer; n : Integer);
begin
  Inc(B^.p, n);
end;

function lua_ref(L : Plua_State; lock : boolean) : Integer;
begin
  if lock then
    lua_ref := luaL_ref(L, LUA_REGISTRYINDEX)
  else begin
    lua_pushstring(L, 'unlocked references are obsolete');
    lua_error(L);
    lua_ref := 0;
  end;
end;

procedure lua_unref(L : Plua_State; ref : Integer);
begin
  luaL_unref(L, LUA_REGISTRYINDEX, ref);
end;

procedure lua_getref(L : Plua_State; ref : Integer);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
end;

procedure lua_push_global(L: PLua_State);
begin
  // lua_pushglobaltable( L ); // 5.2
  lua_pushvalue( L, LUA_GLOBALSINDEX );
end;

procedure lua_rawset_global(L: PLua_State);
begin
  // lua_pushglobaltable( L ); // 5.2
  lua_pushvalue( L, LUA_GLOBALSINDEX );
  lua_insert( L, -3 );
  lua_rawset( L, -3 );
  lua_pop( L, 1 );
end;

procedure lua_rawget_global(L: PLua_State);
begin
  // lua_pushglobaltable( L ); // 5.2
  lua_pushvalue( L, LUA_GLOBALSINDEX );
  lua_insert( L ,-2 );
  lua_rawget( L, -2 );
  lua_insert( L ,-2 );
  lua_pop( L, 1 );
end;

function luaL_testudata(L : Plua_State; ud : Integer; const tname : PChar) : Pointer;
var p : Pointer;
begin
  p := lua_touserdata(L, ud);
  if p <> nil then   //* value is a userdata? */
    if lua_getmetatable(L, ud) then   //* does it have a metatable? */
    begin
      lua_getfield(L, LUA_REGISTRYINDEX, tname);  //* get correct metatable */
      if lua_rawequal(L, -1, -2) then //* does it have the correct mt? */
      begin
        lua_pop(L, 2);  //* remove both metatables */
        Exit( p );
      end;
      lua_pop(L, 2);  //* remove both metatables */
    end;
  Exit( nil );
end;

end.

