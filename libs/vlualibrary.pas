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
    LuaDefaultPath = 'lua52.dll';
  {$ELSE}
    {$IFDEF DARWIN}
      LuaDefaultPath = 'lua52.dylib';
    {$ELSE}
      LuaDefaultPath = 'lua52.so';
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  LuaDefaultPath = 'lua52.a';
  {$linklib lua5.1.a}
{$ENDIF}

{$include vluaconst.inc}
{$include vluatypes.inc}

{$DEFINE calldecl := cdecl}

{$IFDEF LUA_DYNAMIC}
var
  { state manipulation }
  lua_newstate     : function (aF: lua_Alloc; aUD: Pointer): Plua_State; cdecl;
  lua_close        : procedure (aL: Plua_State); cdecl;
  lua_newthread    : function (aL: Plua_State): Plua_State; cdecl;
  lua_atpanic      : function (aL: Plua_State; aPanicF: lua_CFunction): lua_CFunction; cdecl;
  lua_version      : function (aL: Plua_State): Plua_Number; cdecl;

  { basic stack manipulation }
  lua_absindex     : function (aL: Plua_State; aIdx: LongInt): LongInt; cdecl;
  lua_gettop       : function (aL: Plua_State): LongInt; cdecl;
  lua_settop       : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_pushvalue    : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_remove       : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_insert       : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_replace      : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_copy         : procedure (aL: Plua_State; aFromIdx, aToIdx: LongInt); cdecl;
  lua_checkstack   : function (aL: Plua_State; aSz: LongInt): LongInt; cdecl;
  lua_xmove        : procedure (aFrom, aTo: Plua_State; aN: LongInt); cdecl;

  { access (stack -> C) }
  lua_isnumber     : function (aL: Plua_State; aIdx: LongInt): LongBool; cdecl;
  lua_isstring     : function (aL: Plua_State; aIdx: LongInt): LongBool; cdecl;
  lua_iscfunction  : function (aL: Plua_State; aIdx: LongInt): LongBool; cdecl;
  lua_isuserdata   : function (aL: Plua_State; aIdx: LongInt): LongBool; cdecl;
  lua_type         : function (aL: Plua_State; aIdx: LongInt): LongInt; cdecl;
  lua_typename     : function (aL: Plua_State; aTp: LongInt): PAnsiChar; cdecl;

  lua_tonumberx    : function (aL: Plua_State; aIdx: LongInt; aIsNum: PLongInt): lua_Number; cdecl;
  lua_tointegerx   : function (aL: Plua_State; aIdx: LongInt; aIsNum: PLongInt): lua_Integer; cdecl;
  lua_tounsignedx  : function (aL: Plua_State; aIdx: LongInt; aIsNum: PLongInt): lua_Unsigned; cdecl;
  lua_toboolean    : function (aL: Plua_State; aIdx: LongInt): LongBool; cdecl;
  lua_tolstring    : function (aL: Plua_State; aIdx: LongInt; aLen: Psize_t): PAnsiChar; cdecl;
  lua_rawlen       : function (aL: Plua_State; aIdx: LongInt): size_t; cdecl;
  lua_tocfunction  : function (aL: Plua_State; aIdx: LongInt): lua_CFunction; cdecl;
  lua_touserdata   : function (aL: Plua_State; aIdx: LongInt): Pointer; cdecl;
  lua_tothread     : function (aL: Plua_State; aIdx: LongInt): Plua_State; cdecl;
  lua_topointer    : function (aL: Plua_State; aIdx: LongInt): Pointer; cdecl;

  { arithmetic / compare }
  lua_arith        : procedure (aL: Plua_State; aOp: LongInt); cdecl;

  lua_rawequal     : function (aL: Plua_State; aIdx1, aIdx2: LongInt): LongBool; cdecl;
  lua_compare      : function (aL: Plua_State; aIdx1, aIdx2, aOp: LongInt): LongInt; cdecl;

  { push (C -> stack) }
  lua_pushnil           : procedure (aL: Plua_State); cdecl;
  lua_pushnumber        : procedure (aL: Plua_State; aN: lua_Number); cdecl;
  lua_pushinteger       : procedure (aL: Plua_State; aN: lua_Integer); cdecl;
  lua_pushunsigned      : procedure (aL: Plua_State; aN: lua_Unsigned); cdecl;
  lua_pushlstring       : function  (aL: Plua_State; aSt: PAnsiChar; aLen: size_t): PAnsiChar; cdecl;
  lua_pushstring        : function  (aL: Plua_State; aSt: PAnsiChar): PAnsiChar; cdecl;
  lua_pushvfstring      : function  (aL: Plua_State; aFmt: PAnsiChar; aArgs: va_list): PAnsiChar; cdecl;
  lua_pushfstring       : function  (aL: Plua_State; aFmt: PAnsiChar): PAnsiChar; cdecl; varargs;
  lua_pushcclosure      : procedure (aL: Plua_State; aFn: lua_CFunction; aN: LongInt); cdecl;
  lua_pushboolean       : procedure (aL: Plua_State; aB: LongBool); cdecl;
  lua_pushlightuserdata : procedure (aL: Plua_State; aP: Pointer); cdecl;
  lua_pushthread        : function  (aL: Plua_State): LongInt; cdecl;

  { get (Lua -> stack) }
  lua_getglobal_    : procedure (aL: Plua_State; aVar: PAnsiChar); cdecl;
  lua_gettable      : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_getfield      : procedure (aL: Plua_State; aIdx: LongInt; aKey: PAnsiChar); cdecl;
  lua_rawget        : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_rawgeti       : procedure (aL: Plua_State; aIdx, aN: LongInt); cdecl;
  lua_rawgetp       : procedure (aL: Plua_State; aIdx: LongInt; aP: Pointer); cdecl;
  lua_createtable   : procedure (aL: Plua_State; aNarr, aNRec: LongInt); cdecl;
  lua_newuserdata   : function  (aL: Plua_State; aSz: size_t): Pointer; cdecl;
  lua_getmetatable  : function  (aL: Plua_State; aObjIndex: LongInt): LongBool; cdecl;
  lua_getuservalue  : procedure (aL: Plua_State; aIdx: LongInt); cdecl;

  { set (stack -> Lua) }
  lua_setglobal_    : procedure (aL: Plua_State; aVar: PAnsiChar); cdecl;
  lua_settable      : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_setfield      : procedure (aL: Plua_State; aIdx: LongInt; aKey: PAnsiChar); cdecl;
  lua_rawset        : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_rawseti       : procedure (aL: Plua_State; aIdx, aN: LongInt); cdecl;
  lua_rawsetp       : procedure (aL: Plua_State; aIdx: LongInt; aP: Pointer); cdecl;
  lua_setmetatable  : function  (aL: Plua_State; aObjIndex: LongInt): LongInt; cdecl;
  lua_setuservalue  : procedure (aL: Plua_State; aIdx: LongInt); cdecl;

  { load & call }
  lua_callk         : procedure (aL: Plua_State; aNArgs, aNResults, aCtx: LongInt; aK: lua_CFunction); cdecl;
  lua_getctx        : function  (aL: Plua_State; aCtx: PLongInt): LongInt; cdecl;
  lua_pcallk        : function  (aL: Plua_State; aNArgs, aNResults, aErrFunc, aCtx: LongInt; aK: lua_CFunction): LongInt; cdecl;
  lua_load          : function  (aL: Plua_State; aReader: lua_Reader; aDt: Pointer; aChunkName, aMode: PAnsiChar): LongInt; cdecl;
  lua_dump          : function  (aL: Plua_State; aWriter: lua_Writer; aData: Pointer): LongInt; cdecl;

  { coroutines }
  lua_yieldk        : function  (aL: Plua_State; aNResults, aCtx: LongInt; aK: lua_CFunction): LongInt; cdecl;
  lua_resume        : function  (aL, aFrom: Plua_State; aNArg: LongInt): LongInt; cdecl;
  lua_status        : function  (aL: Plua_State): LongInt; cdecl;

  { GC }
  lua_gc            : function  (aL: Plua_State; aWhat, aData: LongInt): LongInt; cdecl;

  { misc }
  lua_error         : function  (aL: Plua_State): LongInt; cdecl;
  lua_next          : function  (aL: Plua_State; aIdx: LongInt): LongInt; cdecl;
  lua_concat        : procedure (aL: Plua_State; aN: LongInt); cdecl;
  lua_len           : procedure (aL: Plua_State; aIdx: LongInt); cdecl;
  lua_getallocf     : function  (aL: Plua_State; var aUD: Pointer): lua_Alloc; cdecl;
  lua_setallocf     : procedure (aL: Plua_State; aF: lua_Alloc; aUD: Pointer); cdecl;

  { Debug API }
  lua_getstack      : function (aL: Plua_State; aLevel: LongInt; aAR: Plua_Debug): LongInt; cdecl;
  lua_getinfo       : function (aL: Plua_State; aWhat: PAnsiChar; aAR: Plua_Debug): LongInt; cdecl;
  lua_getlocal      : function (aL: Plua_State; const aAR: lua_Debug; aN: LongInt): PAnsiChar; cdecl;
  lua_setlocal      : function (aL: Plua_State; const aAR: lua_Debug; aN: LongInt): PAnsiChar; cdecl;
  lua_getupvalue    : function (aL: Plua_State; aFuncIndex, aN: LongInt): PAnsiChar; cdecl;
  lua_setupvalue    : function (aL: Plua_State; aFuncIndex, aN: LongInt): PAnsiChar; cdecl;
  lua_upvalueid     : function (aL: Plua_State; aFIdx, aN: LongInt): Pointer; cdecl;
  lua_upvaluejoin   : procedure(aL: Plua_State; aFIdx1, aN1, aFIdx2, aN2: LongInt); cdecl;
  lua_sethook       : function (aL: Plua_State; aFunc: lua_Hook; aMask, aCount: LongInt): LongInt; cdecl;
  lua_gethook       : function (aL: Plua_State): lua_Hook; cdecl;
  lua_gethookmask   : function (aL: Plua_State): LongInt; cdecl;
  lua_gethookcount  : function (aL: Plua_State): LongInt; cdecl;

  { luaL_* }
  luaL_checkversion_ : procedure (aL: Plua_State; aVer: lua_Number); cdecl;

  luaL_getmetafield  : function (aL: Plua_State; aObj: LongInt; aE: PAnsiChar): LongInt; cdecl;
  luaL_callmeta      : function (aL: Plua_State; aObj: LongInt; aE: PAnsiChar): LongInt; cdecl;
  luaL_tolstring     : function (aL: Plua_State; aIdx: LongInt; aLen: Psize_t): PAnsiChar; cdecl;
  luaL_argerror      : function (aL: Plua_State; aNumArg: LongInt; aExtraMsg: PAnsiChar): LongInt; cdecl;
  luaL_checklstring  : function (aL: Plua_State; aNumArg: LongInt; aLen: Psize_t): PAnsiChar; cdecl;
  luaL_optlstring    : function (aL: Plua_State; aNumArg: LongInt; aDef: PAnsiChar; aLen: Psize_t): PAnsiChar; cdecl;
  luaL_checknumber   : function (aL: Plua_State; aNumArg: LongInt): lua_Number; cdecl;
  luaL_optnumber     : function (aL: Plua_State; aNArg: LongInt; aDef: lua_Number): lua_Number; cdecl;

  luaL_checkinteger  : function (aL: Plua_State; aNumArg: LongInt): lua_Integer; cdecl;
  luaL_optinteger    : function (aL: Plua_State; aNArg: LongInt; aDef: lua_Integer): lua_Integer; cdecl;
  luaL_checkunsigned : function (aL: Plua_State; aNumArg: LongInt): lua_Unsigned; cdecl;
  luaL_optunsigned   : function (aL: Plua_State; aNumArg: LongInt; aDef: lua_Unsigned): lua_Unsigned; cdecl;

  luaL_checkstack    : procedure (aL: Plua_State; aSz: LongInt; aMsg: PAnsiChar); cdecl;
  luaL_checktype     : procedure (aL: Plua_State; aNArg, aT: LongInt); cdecl;
  luaL_checkany      : procedure (aL: Plua_State; aNArg: LongInt); cdecl;

  luaL_newmetatable  : function (aL: Plua_State; aTName: PAnsiChar): LongInt; cdecl;
  luaL_setmetatable  : procedure(aL: Plua_State; aTName: PAnsiChar); cdecl;
  luaL_testudata     : function (aL: Plua_State; aUD: LongInt; aTName: PAnsiChar): Pointer; cdecl;
  luaL_checkudata    : function (aL: Plua_State; aUD: LongInt; aTName: PAnsiChar): Pointer; cdecl;

  luaL_where         : procedure (aL: Plua_State; aLvl: LongInt); cdecl;
  luaL_error         : function  (aL: Plua_State; aFmt: PAnsiChar): LongInt; cdecl; varargs;

  luaL_checkoption   : function (aL: Plua_State; aNArg: LongInt; aDef: PAnsiChar; aList: PPAnsiChar): LongInt; cdecl;

  luaL_fileresult    : function (aL: Plua_State; aStat: LongInt; aFName: PAnsiChar): LongInt; cdecl;
  luaL_execresult    : function (aL: Plua_State; aStat: LongInt): LongInt; cdecl;

  luaL_ref           : function (aL: Plua_State; aT: LongInt): LongInt; cdecl;
  luaL_unref         : procedure(aL: Plua_State; aT, aRef: LongInt); cdecl;

  luaL_loadfilex     : function (aL: Plua_State; aFileName, aMode: PAnsiChar): LongInt; cdecl;
  luaL_loadbufferx   : function (aL: Plua_State; aBuff: PAnsiChar; aSz: size_t; aName, aMode: PAnsiChar): LongInt; cdecl;
  luaL_loadstring    : function (aL: Plua_State; aSt: PAnsiChar): LongInt; cdecl;

  luaL_newstate      : function: Plua_State; cdecl;

  luaL_len           : function (aL: Plua_State; aIdx: LongInt): LongInt; cdecl;

  luaL_gsub          : function (aL: Plua_State; aSt, aP, aR: PAnsiChar): PAnsiChar; cdecl;

  luaL_setfuncs      : procedure(aL: Plua_State; aLReg: PluaL_Reg; aNUp: LongInt); cdecl;

  luaL_getsubtable   : function (aL: Plua_State; aIdx: LongInt; aFName: PAnsiChar): LongInt; cdecl;

  luaL_traceback     : procedure(aL, aL1: Plua_State; aMsg: PAnsiChar; aLevel: LongInt); cdecl;

  luaL_requiref      : procedure(aL: Plua_State; aModName: PAnsiChar; aOpenF: lua_CFunction; aGlb: LongInt); cdecl;

  { luaL buffer API }
  luaL_buffinit      : procedure (aL: Plua_State; aB: PluaL_Buffer); cdecl;
  luaL_prepbuffsize  : function  (aB: PluaL_Buffer; aSz: size_t): PAnsiChar; cdecl;
  luaL_addlstring    : procedure (aB: PluaL_Buffer; aSt: PAnsiChar; aLen: size_t); cdecl;
  luaL_addstring     : procedure (aB: PluaL_Buffer; aSt: PAnsiChar); cdecl;
  luaL_addvalue      : procedure (aB: PluaL_Buffer); cdecl;
  luaL_pushresult    : procedure (aB: PluaL_Buffer); cdecl;
  luaL_pushresultsize: procedure (aB: PluaL_Buffer; aSz: size_t); cdecl;
  luaL_buffinitsize  : function  (aL: Plua_State; aB: PluaL_Buffer; aSz: size_t): PAnsiChar; cdecl;

  { base libs }
  luaopen_base     : function (aL: Plua_State): LongInt; cdecl;
  luaopen_coroutine: function (aL: Plua_State): LongInt; cdecl;
  luaopen_table    : function (aL: Plua_State): LongInt; cdecl;
  luaopen_io       : function (aL: Plua_State): LongInt; cdecl;
  luaopen_os       : function (aL: Plua_State): LongInt; cdecl;
  luaopen_string   : function (aL: Plua_State): LongInt; cdecl;
  luaopen_bit32    : function (aL: Plua_State): LongInt; cdecl;
  luaopen_math     : function (aL: Plua_State): LongInt; cdecl;
  luaopen_debug    : function (aL: Plua_State): LongInt; cdecl;
  luaopen_package  : function (aL: Plua_State): LongInt; cdecl;

  luaL_openlibs    : procedure (aL: Plua_State); cdecl;
{$ELSE}
{ state manipulation }
function lua_newstate  (aF: lua_Alloc; aUD: Pointer): Plua_State; calldecl; external;
procedure lua_close  (aL: Plua_State); calldecl; external;
function lua_newthread  (aL: Plua_State): Plua_State; calldecl; external;
function lua_atpanic  (aL: Plua_State; aPanicF: lua_CFunction): lua_CFunction; calldecl; external;
function lua_version  (aL: Plua_State): ^lua_Number; calldecl; external;

{ basic stack manipulation }
function lua_absindex  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
function lua_gettop  (aL: Plua_State): LongInt; calldecl; external;
procedure lua_settop  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_pushvalue  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_remove  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_insert  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_replace  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_copy  (aL: Plua_State; aFromIdx, aToIdx: LongInt); calldecl; external;
function lua_checkstack  (aL: Plua_State; aSz: LongInt): LongInt; calldecl; external;
procedure lua_xmove  (aFrom, aTo: Plua_State; aN: LongInt); calldecl; external;

{ access (stack -> C) }
function lua_isnumber  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
function lua_isstring  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
function lua_iscfunction  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
function lua_isuserdata  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
function lua_type  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
function lua_typename  (aL: Plua_State; aTp: LongInt): PAnsiChar; calldecl; external;

function lua_tonumberx  (aL: Plua_State; aIdx: LongInt; aIsNum: PLongInt): lua_Number; calldecl; external;
function lua_tointegerx  (aL: Plua_State; aIdx: LongInt; aIsNum: PLongInt): lua_Integer; calldecl; external;
function lua_tounsignedx  (aL: Plua_State; aIdx: LongInt; aIsNum: PLongInt): lua_Unsigned; calldecl; external;
function lua_toboolean  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
function lua_tolstring  (aL: Plua_State; aIdx: LongInt; aLen: Psize_t): PAnsiChar; calldecl; external;
function lua_rawlen  (aL: Plua_State; aIdx: LongInt): size_t; calldecl; external;
function lua_tocfunction  (aL: Plua_State; aIdx: LongInt): lua_CFunction; calldecl; external;
function lua_touserdata  (aL: Plua_State; aIdx: LongInt): Pointer; calldecl; external;
function lua_tothread  (aL: Plua_State; aIdx: LongInt): Plua_State; calldecl; external;
function lua_topointer  (aL: Plua_State; aIdx: LongInt): Pointer; calldecl; external;

{ arithmetic / compare }
procedure lua_arith  (aL: Plua_State; aOp: LongInt); calldecl; external;

function lua_rawequal  (aL: Plua_State; aIdx1, aIdx2: LongInt): LongInt; calldecl; external;
function lua_compare  (aL: Plua_State; aIdx1, aIdx2, aOp: LongInt): LongInt; calldecl; external;

{ push (C -> stack) }
procedure lua_pushnil  (aL: Plua_State); calldecl; external;
procedure lua_pushnumber  (aL: Plua_State; aN: lua_Number); calldecl; external;
procedure lua_pushinteger  (aL: Plua_State; aN: lua_Integer); calldecl; external;
procedure lua_pushunsigned  (aL: Plua_State; aN: lua_Unsigned); calldecl; external;
function lua_pushlstring   (aL: Plua_State; aSt: PAnsiChar; aLen: size_t): PAnsiChar; calldecl; external;
function lua_pushstring   (aL: Plua_State; aSt: PAnsiChar): PAnsiChar; calldecl; external;
function lua_pushvfstring   (aL: Plua_State; aFmt: PAnsiChar; aArgs: va_list): PAnsiChar; calldecl; external;
function lua_pushfstring   (aL: Plua_State; aFmt: PAnsiChar): PAnsiChar; calldecl; external; varargs;
procedure lua_pushcclosure  (aL: Plua_State; aFn: lua_CFunction; aN: LongInt); calldecl; external;
procedure lua_pushboolean  (aL: Plua_State; aB: LongInt); calldecl; external;
procedure lua_pushlightuserdata  (aL: Plua_State; aP: Pointer); calldecl; external;
function lua_pushthread   (aL: Plua_State): LongInt; calldecl; external;

{ get (Lua -> stack) }
procedure lua_getglobal  (aL: Plua_State; aVar: PAnsiChar); calldecl; external;
procedure lua_gettable  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_getfield  (aL: Plua_State; aIdx: LongInt; aKey: PAnsiChar); calldecl; external;
procedure lua_rawget  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_rawgeti  (aL: Plua_State; aIdx, aN: LongInt); calldecl; external;
procedure lua_rawgetp  (aL: Plua_State; aIdx: LongInt; aP: Pointer); calldecl; external;
procedure lua_createtable  (aL: Plua_State; aNarr, aNRec: LongInt); calldecl; external;
function lua_newuserdata   (aL: Plua_State; aSz: size_t): Pointer; calldecl; external;
function lua_getmetatable   (aL: Plua_State; aObjIndex: LongInt): LongInt; calldecl; external;
procedure lua_getuservalue  (aL: Plua_State; aIdx: LongInt); calldecl; external;

{ set (stack -> Lua) }
procedure lua_setglobal  (aL: Plua_State; aVar: PAnsiChar); calldecl; external;
procedure lua_settable  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_setfield  (aL: Plua_State; aIdx: LongInt; aKey: PAnsiChar); calldecl; external;
procedure lua_rawset  (aL: Plua_State; aIdx: LongInt); calldecl; external;
procedure lua_rawseti  (aL: Plua_State; aIdx, aN: LongInt); calldecl; external;
procedure lua_rawsetp  (aL: Plua_State; aIdx: LongInt; aP: Pointer); calldecl; external;
function lua_setmetatable   (aL: Plua_State; aObjIndex: LongInt): LongInt; calldecl; external;
procedure lua_setuservalue  (aL: Plua_State; aIdx: LongInt); calldecl; external;

{ load & call }
procedure lua_callk  (aL: Plua_State; aNArgs, aNResults, aCtx: LongInt; aK: lua_CFunction); calldecl; external;
function lua_getctx   (aL: Plua_State; aCtx: PLongInt): LongInt; calldecl; external;
function lua_pcallk   (aL: Plua_State; aNArgs, aNResults, aErrFunc, aCtx: LongInt; aK: lua_CFunction): LongInt; calldecl; external;
function lua_load   (aL: Plua_State; aReader: lua_Reader; aDt: Pointer; aChunkName, aMode: PAnsiChar): LongInt; calldecl; external;
function lua_dump   (aL: Plua_State; aWriter: lua_Writer; aData: Pointer): LongInt; calldecl; external;

{ coroutines }
function lua_yieldk   (aL: Plua_State; aNResults, aCtx: LongInt; aK: lua_CFunction): LongInt; calldecl; external;
function lua_resume   (aL, aFrom: Plua_State; aNArg: LongInt): LongInt; calldecl; external;
function lua_status   (aL: Plua_State): LongInt; calldecl; external;

{ GC }
function lua_gc   (aL: Plua_State; aWhat, aData: LongInt): LongInt; calldecl; external;

{ misc }
function lua_error   (aL: Plua_State): LongInt; calldecl; external;
function lua_next   (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;
procedure lua_concat  (aL: Plua_State; aN: LongInt); calldecl; external;
procedure lua_len  (aL: Plua_State; aIdx: LongInt); calldecl; external;
function lua_getallocf   (aL: Plua_State; var aUD: Pointer): lua_Alloc; calldecl; external;
procedure lua_setallocf  (aL: Plua_State; aF: lua_Alloc; aUD: Pointer); calldecl; external;

{ Debug API }
function lua_getstack  (aL: Plua_State; aLevel: LongInt; aAR: Plua_Debug): LongInt; calldecl; external;
function lua_getinfo  (aL: Plua_State; aWhat: PAnsiChar; aAR: Plua_Debug): LongInt; calldecl; external;
function lua_getlocal  (aL: Plua_State; const aAR: lua_Debug; aN: LongInt): PAnsiChar; calldecl; external;
function lua_setlocal  (aL: Plua_State; const aAR: lua_Debug; aN: LongInt): PAnsiChar; calldecl; external;
function lua_getupvalue  (aL: Plua_State; aFuncIndex, aN: LongInt): PAnsiChar; calldecl; external;
function lua_setupvalue  (aL: Plua_State; aFuncIndex, aN: LongInt): PAnsiChar; calldecl; external;
function lua_upvalueid  (aL: Plua_State; aFIdx, aN: LongInt): Pointer; calldecl; external;
procedure lua_upvaluejoin (aL: Plua_State; aFIdx1, aN1, aFIdx2, aN2: LongInt); calldecl; external;
function lua_sethook  (aL: Plua_State; aFunc: lua_Hook; aMask, aCount: LongInt): LongInt; calldecl; external;
function lua_gethook  (aL: Plua_State): lua_Hook; calldecl; external;
function lua_gethookmask  (aL: Plua_State): LongInt; calldecl; external;
function lua_gethookcount  (aL: Plua_State): LongInt; calldecl; external;

{ luaL_* }
procedure luaL_checkversion_  (aL: Plua_State; aVer: lua_Number); calldecl; external;

function luaL_getmetafield  (aL: Plua_State; aObj: LongInt; aE: PAnsiChar): LongInt; calldecl; external;
function luaL_callmeta  (aL: Plua_State; aObj: LongInt; aE: PAnsiChar): LongInt; calldecl; external;
function luaL_tolstring  (aL: Plua_State; aIdx: LongInt; aLen: Psize_t): PAnsiChar; calldecl; external;
function luaL_argerror  (aL: Plua_State; aNumArg: LongInt; aExtraMsg: PAnsiChar): LongInt; calldecl; external;
function luaL_checklstring  (aL: Plua_State; aNumArg: LongInt; aLen: Psize_t): PAnsiChar; calldecl; external;
function luaL_optlstring  (aL: Plua_State; aNumArg: LongInt; aDef: PAnsiChar; aLen: Psize_t): PAnsiChar; calldecl; external;
function luaL_checknumber  (aL: Plua_State; aNumArg: LongInt): lua_Number; calldecl; external;
function luaL_optnumber  (aL: Plua_State; aNArg: LongInt; aDef: lua_Number): lua_Number; calldecl; external;

function luaL_checkinteger  (aL: Plua_State; aNumArg: LongInt): lua_Integer; calldecl; external;
function luaL_optinteger  (aL: Plua_State; aNArg: LongInt; aDef: lua_Integer): lua_Integer; calldecl; external;
function luaL_checkunsigned  (aL: Plua_State; aNumArg: LongInt): lua_Unsigned; calldecl; external;
function luaL_optunsigned  (aL: Plua_State; aNumArg: LongInt; aDef: lua_Unsigned): lua_Unsigned; calldecl; external;

procedure luaL_checkstack  (aL: Plua_State; aSz: LongInt; aMsg: PAnsiChar); calldecl; external;
procedure luaL_checktype  (aL: Plua_State; aNArg, aT: LongInt); calldecl; external;
procedure luaL_checkany  (aL: Plua_State; aNArg: LongInt); calldecl; external;

function luaL_newmetatable  (aL: Plua_State; aTName: PAnsiChar): LongInt; calldecl; external;
procedure luaL_setmetatable (aL: Plua_State; aTName: PAnsiChar); calldecl; external;
function luaL_testudata  (aL: Plua_State; aUD: LongInt; aTName: PAnsiChar): Pointer; calldecl; external;
function luaL_checkudata  (aL: Plua_State; aUD: LongInt; aTName: PAnsiChar): Pointer; calldecl; external;

procedure luaL_where  (aL: Plua_State; aLvl: LongInt); calldecl; external;
function luaL_error   (aL: Plua_State; aFmt: PAnsiChar): LongInt; calldecl; external; varargs;

function luaL_checkoption  (aL: Plua_State; aNArg: LongInt; aDef: PAnsiChar; aList: ^PAnsiChar): LongInt; calldecl; external;

function luaL_fileresult  (aL: Plua_State; aStat: LongInt; aFName: PAnsiChar): LongInt; calldecl; external;
function luaL_execresult  (aL: Plua_State; aStat: LongInt): LongInt; calldecl; external;

function luaL_ref  (aL: Plua_State; aT: LongInt): LongInt; calldecl; external;
procedure luaL_unref (aL: Plua_State; aT, aRef: LongInt); calldecl; external;

function luaL_loadfilex  (aL: Plua_State; aFileName, aMode: PAnsiChar): LongInt; calldecl; external;
function luaL_loadbufferx  (aL: Plua_State; aBuff: PAnsiChar; aSz: size_t; aName, aMode: PAnsiChar): LongInt; calldecl; external;
function luaL_loadstring  (aL: Plua_State; aSt: PAnsiChar): LongInt; calldecl; external;

function luaL_newstate : Plua_State; calldecl; external;

function luaL_len  (aL: Plua_State; aIdx: LongInt): LongInt; calldecl; external;

function luaL_gsub  (aL: Plua_State; aS, aP, aR: PAnsiChar): PAnsiChar; calldecl; external;

procedure luaL_setfuncs (aL: Plua_State; aLReg: PluaL_Reg; aNUp: LongInt); calldecl; external;

function luaL_getsubtable  (aL: Plua_State; aIdx: LongInt; aFName: PAnsiChar): LongInt; calldecl; external;

procedure luaL_traceback (aL, aL1: Plua_State; aMsg: PAnsiChar; aLevel: LongInt); calldecl; external;

procedure luaL_requiref (aL: Plua_State; aModName: PAnsiChar; aOpenF: lua_CFunction; aGlb: LongInt); calldecl; external;

{ luaL buffer API }
procedure luaL_buffinit  (aL: Plua_State; aB: PluaL_Buffer); calldecl; external;
function luaL_prepbuffsize   (aB: PluaL_Buffer; aSz: size_t): PAnsiChar; calldecl; external;
procedure luaL_addlstring  (aB: PluaL_Buffer; aSt: PAnsiChar; aLen: size_t); calldecl; external;
procedure luaL_addstring  (aB: PluaL_Buffer; aSt: PAnsiChar); calldecl; external;
procedure luaL_addvalue  (aB: PluaL_Buffer); calldecl; external;
procedure luaL_pushresult  (aB: PluaL_Buffer); calldecl; external;
procedure luaL_pushresultsize  (aB: PluaL_Buffer; aSz: size_t); calldecl; external;
function luaL_buffinitsize   (aL: Plua_State; aB: PluaL_Buffer; aSz: size_t): PAnsiChar; calldecl; external;

{ base libs }
function luaopen_base  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_coroutine  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_table  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_io  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_os  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_string  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_bit32  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_math  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_debug  (aL: Plua_State): LongInt; calldecl; external;
function luaopen_package  (aL: Plua_State): LongInt; calldecl; external;

procedure luaL_openlibs  (aL: Plua_State); calldecl; external;
{$ENDIF}

function lua_upvalueindex(const aI : Integer) : Integer; inline;
function lua_tonumber(L: Plua_State; i: LongInt): lua_Number; inline;
function lua_tointeger(L: Plua_State; i: LongInt): lua_Integer; inline;
function lua_tounsigned(L: Plua_State; i: LongInt): lua_Unsigned; inline;
procedure lua_pop( L : Plua_State; n : Integer ); inline;
procedure lua_newtable( L : Plua_State ); inline;

function lua_isfunction( L : Plua_State; n : Integer ) : Boolean; inline;
function lua_istable( L : Plua_State; n : Integer ) : Boolean; inline;
function lua_islightuserdata( L : Plua_State; n : Integer ) : Boolean; inline;
function lua_isnil( L : Plua_State; n : Integer ) : Boolean; inline;
function lua_isboolean( L : Plua_State; n : Integer ) : Boolean; inline;
function lua_isthread( L : Plua_State; n : Integer ) : Boolean; inline;
function lua_isnone( L : Plua_State; n : Integer ) : Boolean; inline;
function lua_isnoneornil( L : Plua_State; n : Integer ) : Boolean; inline;

procedure lua_pushansistring( L: Plua_State; const s: Ansistring ); inline;
function lua_tostring( L : Plua_State; idx : Integer ) : AnsiString;

procedure lua_pushglobaltable( L : PLua_State );
procedure lua_rawset_global( L : PLua_State );
procedure lua_rawget_global( L : PLua_State );
procedure lua_pushcfunction( L : Plua_State; f : lua_CFunction );

procedure luaL_getmetatable( L : PLua_State; name : PAnsiChar ); inline;
function luaL_dofile( L : Plua_State; fn : PAnsiChar ) : Integer; inline;

// compatibility
function lua_strlen( aL: Plua_State; aIdx: LongInt): size_t; inline;
function lua_objlen( aL: Plua_State; aIdx: LongInt): size_t; inline;
function lua_pcall( aL: Plua_State; aNArgs, aNResults, aErrFunc : LongInt ) : LongInt; inline;
procedure lua_call( aL: Plua_State; aNArgs, aNResults : LongInt ); inline;
function luaL_loadfile( aL: Plua_State; aFileName : PAnsiChar): LongInt; inline;
function luaL_loadbuffer( aL: Plua_State; aBuff: PAnsiChar; aSz: size_t; aName : PAnsiChar): LongInt; inline;
procedure lua_getglobal( aL: Plua_State; const aVar : Ansistring ); inline;
procedure lua_setglobal( aL: Plua_State; const aVar : Ansistring ); inline;
function lua_open : Plua_State;
procedure lua_register( aL : Plua_State; const aName : AnsiString; aF : lua_CFunction );

              {
procedure lua_getregistry(L : Plua_State);
function lua_getgccount(L : Plua_State) : Integer;
function luaL_getn(L : Plua_State; idx : Integer) : Integer;
function luaL_argcheck(L : Plua_State; cond : Boolean; numarg : Integer; extramsg : PChar): Integer;
function luaL_typename(L : Plua_State; idx : Integer) : PChar;
function luaL_dofile(L : Plua_State; fn : PChar) : Integer;
function luaL_dostring(L : Plua_State; s : PChar) : Integer;

procedure luaL_getmetatable(L : Plua_State; n : PChar);
procedure luaL_addchar(B : PluaL_Buffer; c : Char);
procedure luaL_addsize(B : PluaL_Buffer; n : Integer);

function lua_ref(L : Plua_State; lock : boolean) : Integer;
procedure lua_unref(L : Plua_State; ref : Integer);
procedure lua_getref(L : Plua_State; ref : Integer);

// these functions are introduced to prevent the usage of deprecated LUA_GLOBALSINDEX
}
var
  Lua : TLibrary = nil;

function LoadLua( const aPath : AnsiString = LuaDefaultPath ) : Boolean;

implementation

function lua_upvalueindex( const aI: LongInt ): LongInt; inline;
begin
  Result := LUA_REGISTRYINDEX - aI;
end;

function lua_tonumber(L: Plua_State; i: LongInt): lua_Number; inline;
begin
  Result := lua_tonumberx(L, i, nil);
end;

function lua_tointeger(L: Plua_State; i: LongInt): lua_Integer; inline;
begin
  Result := lua_tointegerx(L, i, nil);
end;

function lua_tounsigned(L: Plua_State; i: LongInt): lua_Unsigned; inline;
begin
  Result := lua_tounsignedx(L, i, nil);
end;

procedure lua_pop( L : Plua_State; n : Integer ); inline;
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable( L : Plua_State ); inline;
begin
  lua_createtable( L, 0, 0 );
end;


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

  Pointer(lua_newstate)      := GetSymbol( 'lua_newstate' );
  Pointer(lua_close)         := GetSymbol( 'lua_close' );
  Pointer(lua_newthread)     := GetSymbol( 'lua_newthread' );
  Pointer(lua_atpanic)       := GetSymbol( 'lua_atpanic' );
  Pointer(lua_version)       := GetSymbol( 'lua_version' );

  Pointer(lua_absindex)      := GetSymbol( 'lua_absindex' );
  Pointer(lua_gettop)        := GetSymbol( 'lua_gettop' );
  Pointer(lua_settop)        := GetSymbol( 'lua_settop' );
  Pointer(lua_pushvalue)     := GetSymbol( 'lua_pushvalue' );
  Pointer(lua_remove)        := GetSymbol( 'lua_remove' );
  Pointer(lua_insert)        := GetSymbol( 'lua_insert' );
  Pointer(lua_replace)       := GetSymbol( 'lua_replace' );
  Pointer(lua_copy)          := GetSymbol( 'lua_copy' );
  Pointer(lua_checkstack)    := GetSymbol( 'lua_checkstack' );
  Pointer(lua_xmove)         := GetSymbol( 'lua_xmove' );

  Pointer(lua_isnumber)      := GetSymbol( 'lua_isnumber' );
  Pointer(lua_isstring)      := GetSymbol( 'lua_isstring' );
  Pointer(lua_iscfunction)   := GetSymbol( 'lua_iscfunction' );
  Pointer(lua_isuserdata)    := GetSymbol( 'lua_isuserdata' );
  Pointer(lua_type)          := GetSymbol( 'lua_type' );
  Pointer(lua_typename)      := GetSymbol( 'lua_typename' );

  Pointer(lua_tonumberx)     := GetSymbol( 'lua_tonumberx' );
  Pointer(lua_tointegerx)    := GetSymbol( 'lua_tointegerx' );
  Pointer(lua_tounsignedx)   := GetSymbol( 'lua_tounsignedx' );
  Pointer(lua_toboolean)     := GetSymbol( 'lua_toboolean' );
  Pointer(lua_tolstring)     := GetSymbol( 'lua_tolstring' );
  Pointer(lua_rawlen)        := GetSymbol( 'lua_rawlen' );
  Pointer(lua_tocfunction)   := GetSymbol( 'lua_tocfunction' );
  Pointer(lua_touserdata)    := GetSymbol( 'lua_touserdata' );
  Pointer(lua_tothread)      := GetSymbol( 'lua_tothread' );
  Pointer(lua_topointer)     := GetSymbol( 'lua_topointer' );

  Pointer(lua_arith)         := GetSymbol( 'lua_arith' );
  Pointer(lua_rawequal)      := GetSymbol( 'lua_rawequal' );
  Pointer(lua_compare)       := GetSymbol( 'lua_compare' );

  Pointer(lua_pushnil)           := GetSymbol( 'lua_pushnil' );
  Pointer(lua_pushnumber)        := GetSymbol( 'lua_pushnumber' );
  Pointer(lua_pushinteger)       := GetSymbol( 'lua_pushinteger' );
  Pointer(lua_pushunsigned)      := GetSymbol( 'lua_pushunsigned' );
  Pointer(lua_pushlstring)       := GetSymbol( 'lua_pushlstring' );
  Pointer(lua_pushstring)        := GetSymbol( 'lua_pushstring' );
  Pointer(lua_pushvfstring)      := GetSymbol( 'lua_pushvfstring' );
  Pointer(lua_pushfstring)       := GetSymbol( 'lua_pushfstring' );
  Pointer(lua_pushcclosure)      := GetSymbol( 'lua_pushcclosure' );
  Pointer(lua_pushboolean)       := GetSymbol( 'lua_pushboolean' );
  Pointer(lua_pushlightuserdata) := GetSymbol( 'lua_pushlightuserdata' );
  Pointer(lua_pushthread)        := GetSymbol( 'lua_pushthread' );

  Pointer(lua_getglobal_)    := GetSymbol( 'lua_getglobal' );
  Pointer(lua_gettable)      := GetSymbol( 'lua_gettable' );
  Pointer(lua_getfield)      := GetSymbol( 'lua_getfield' );
  Pointer(lua_rawget)        := GetSymbol( 'lua_rawget' );
  Pointer(lua_rawgeti)       := GetSymbol( 'lua_rawgeti' );
  Pointer(lua_rawgetp)       := GetSymbol( 'lua_rawgetp' );
  Pointer(lua_createtable)   := GetSymbol( 'lua_createtable' );
  Pointer(lua_newuserdata)   := GetSymbol( 'lua_newuserdata' );
  Pointer(lua_getmetatable)  := GetSymbol( 'lua_getmetatable' );
  Pointer(lua_getuservalue)  := GetSymbol( 'lua_getuservalue' );

  Pointer(lua_setglobal_)    := GetSymbol( 'lua_setglobal' );
  Pointer(lua_settable)      := GetSymbol( 'lua_settable' );
  Pointer(lua_setfield)      := GetSymbol( 'lua_setfield' );
  Pointer(lua_rawset)        := GetSymbol( 'lua_rawset' );
  Pointer(lua_rawseti)       := GetSymbol( 'lua_rawseti' );
  Pointer(lua_rawsetp)       := GetSymbol( 'lua_rawsetp' );
  Pointer(lua_setmetatable)  := GetSymbol( 'lua_setmetatable' );
  Pointer(lua_setuservalue)  := GetSymbol( 'lua_setuservalue' );

  Pointer(lua_callk)         := GetSymbol( 'lua_callk' );
  Pointer(lua_getctx)        := GetSymbol( 'lua_getctx' );
  Pointer(lua_pcallk)        := GetSymbol( 'lua_pcallk' );
  Pointer(lua_load)          := GetSymbol( 'lua_load' );
  Pointer(lua_dump)          := GetSymbol( 'lua_dump' );

  Pointer(lua_yieldk)        := GetSymbol( 'lua_yieldk' );
  Pointer(lua_resume)        := GetSymbol( 'lua_resume' );
  Pointer(lua_status)        := GetSymbol( 'lua_status' );

  Pointer(lua_gc)            := GetSymbol( 'lua_gc' );

  Pointer(lua_error)         := GetSymbol( 'lua_error' );
  Pointer(lua_next)          := GetSymbol( 'lua_next' );
  Pointer(lua_concat)        := GetSymbol( 'lua_concat' );
  Pointer(lua_len)           := GetSymbol( 'lua_len' );
  Pointer(lua_getallocf)     := GetSymbol( 'lua_getallocf' );
  Pointer(lua_setallocf)     := GetSymbol( 'lua_setallocf' );

  Pointer(lua_getstack)      := GetSymbol( 'lua_getstack' );
  Pointer(lua_getinfo)       := GetSymbol( 'lua_getinfo' );
  Pointer(lua_getlocal)      := GetSymbol( 'lua_getlocal' );
  Pointer(lua_setlocal)      := GetSymbol( 'lua_setlocal' );
  Pointer(lua_getupvalue)    := GetSymbol( 'lua_getupvalue' );
  Pointer(lua_setupvalue)    := GetSymbol( 'lua_setupvalue' );
  Pointer(lua_upvalueid)     := GetSymbol( 'lua_upvalueid' );
  Pointer(lua_upvaluejoin)   := GetSymbol( 'lua_upvaluejoin' );
  Pointer(lua_sethook)       := GetSymbol( 'lua_sethook' );
  Pointer(lua_gethook)       := GetSymbol( 'lua_gethook' );
  Pointer(lua_gethookmask)   := GetSymbol( 'lua_gethookmask' );
  Pointer(lua_gethookcount)  := GetSymbol( 'lua_gethookcount' );

  Pointer(luaL_checkversion_) := GetSymbol( 'luaL_checkversion_' );
  Pointer(luaL_getmetafield)  := GetSymbol( 'luaL_getmetafield' );
  Pointer(luaL_callmeta)      := GetSymbol( 'luaL_callmeta' );
  Pointer(luaL_tolstring)     := GetSymbol( 'luaL_tolstring' );
  Pointer(luaL_argerror)      := GetSymbol( 'luaL_argerror' );
  Pointer(luaL_checklstring)  := GetSymbol( 'luaL_checklstring' );
  Pointer(luaL_optlstring)    := GetSymbol( 'luaL_optlstring' );
  Pointer(luaL_checknumber)   := GetSymbol( 'luaL_checknumber' );
  Pointer(luaL_optnumber)     := GetSymbol( 'luaL_optnumber' );
  Pointer(luaL_checkinteger)  := GetSymbol( 'luaL_checkinteger' );
  Pointer(luaL_optinteger)    := GetSymbol( 'luaL_optinteger' );
  Pointer(luaL_checkunsigned) := GetSymbol( 'luaL_checkunsigned' );
  Pointer(luaL_optunsigned)   := GetSymbol( 'luaL_optunsigned' );
  Pointer(luaL_checkstack)    := GetSymbol( 'luaL_checkstack' );
  Pointer(luaL_checktype)     := GetSymbol( 'luaL_checktype' );
  Pointer(luaL_checkany)      := GetSymbol( 'luaL_checkany' );
  Pointer(luaL_newmetatable)  := GetSymbol( 'luaL_newmetatable' );
  Pointer(luaL_setmetatable)  := GetSymbol( 'luaL_setmetatable' );
  Pointer(luaL_testudata)     := GetSymbol( 'luaL_testudata' );
  Pointer(luaL_checkudata)    := GetSymbol( 'luaL_checkudata' );
  Pointer(luaL_where)         := GetSymbol( 'luaL_where' );
  Pointer(luaL_error)         := GetSymbol( 'luaL_error' );
  Pointer(luaL_checkoption)   := GetSymbol( 'luaL_checkoption' );
  Pointer(luaL_fileresult)    := GetSymbol( 'luaL_fileresult' );
  Pointer(luaL_execresult)    := GetSymbol( 'luaL_execresult' );
  Pointer(luaL_ref)           := GetSymbol( 'luaL_ref' );
  Pointer(luaL_unref)         := GetSymbol( 'luaL_unref' );
  Pointer(luaL_loadfilex)     := GetSymbol( 'luaL_loadfilex' );
  Pointer(luaL_loadbufferx)   := GetSymbol( 'luaL_loadbufferx' );
  Pointer(luaL_loadstring)    := GetSymbol( 'luaL_loadstring' );
  Pointer(luaL_newstate)      := GetSymbol( 'luaL_newstate' );
  Pointer(luaL_len)           := GetSymbol( 'luaL_len' );
  Pointer(luaL_gsub)          := GetSymbol( 'luaL_gsub' );
  Pointer(luaL_setfuncs)      := GetSymbol( 'luaL_setfuncs' );
  Pointer(luaL_getsubtable)   := GetSymbol( 'luaL_getsubtable' );
  Pointer(luaL_traceback)     := GetSymbol( 'luaL_traceback' );
  Pointer(luaL_requiref)      := GetSymbol( 'luaL_requiref' );

  Pointer(luaL_buffinit)       := GetSymbol( 'luaL_buffinit' );
  Pointer(luaL_prepbuffsize)   := GetSymbol( 'luaL_prepbuffsize' );
  Pointer(luaL_addlstring)     := GetSymbol( 'luaL_addlstring' );
  Pointer(luaL_addstring)      := GetSymbol( 'luaL_addstring' );
  Pointer(luaL_addvalue)       := GetSymbol( 'luaL_addvalue' );
  Pointer(luaL_pushresult)     := GetSymbol( 'luaL_pushresult' );
  Pointer(luaL_pushresultsize) := GetSymbol( 'luaL_pushresultsize' );
  Pointer(luaL_buffinitsize)   := GetSymbol( 'luaL_buffinitsize' );

  Pointer(luaopen_base)      := GetSymbol( 'luaopen_base' );
  Pointer(luaopen_coroutine) := GetSymbol( 'luaopen_coroutine' );
  Pointer(luaopen_table)     := GetSymbol( 'luaopen_table' );
  Pointer(luaopen_io)        := GetSymbol( 'luaopen_io' );
  Pointer(luaopen_os)        := GetSymbol( 'luaopen_os' );
  Pointer(luaopen_string)    := GetSymbol( 'luaopen_string' );
  Pointer(luaopen_bit32)     := GetSymbol( 'luaopen_bit32' );
  Pointer(luaopen_math)      := GetSymbol( 'luaopen_math' );
  Pointer(luaopen_debug)     := GetSymbol( 'luaopen_debug' );
  Pointer(luaopen_package)   := GetSymbol( 'luaopen_package' );

  Pointer(luaL_openlibs)     := GetSymbol( 'luaL_openlibs' );
  {$ENDIF}
  Exit( True );
end;

function lua_isfunction( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_isfunction := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_istable := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_islightuserdata := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_isnil := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_isboolean := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_isthread := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_isnone := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil( L : Plua_State; n : Integer ) : Boolean; inline;
begin
  lua_isnoneornil := lua_type(L, n) <= 0;
end;


{


procedure lua_register(L : Plua_State; n : AnsiString; f : lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, PChar(n) );
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


function luaL_typename(L : Plua_State; idx : Integer) : PChar;
begin
  luaL_typename := lua_typename( L, lua_type(L, idx) );
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

}

procedure lua_pushansistring( L: Plua_State; const s: Ansistring ); inline;
begin
  lua_pushstring(L, PAnsiChar(s));
end;

function lua_tostring( L : Plua_State; idx : Integer ) : AnsiString;
var size  : Integer;
    ltype : Integer;
begin
  ltype := lua_type( L, idx );
  if (ltype <> LUA_TSTRING) and (ltype <> LUA_TNUMBER) then Exit('');
  size := lua_rawlen( L, idx );
  SetLength( Result, size );
  if size > 0 then Move( lua_tolstring( L, idx, nil )^, Result[1], size );
end;

procedure lua_pushglobaltable(L: PLua_State);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
end;

procedure lua_rawset_global(L: PLua_State);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
  lua_insert( L, -3 );
  lua_rawset( L, -3 );
  lua_pop( L, 1 );
end;

procedure lua_rawget_global(L: PLua_State);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
  lua_insert( L ,-2 );
  lua_rawget( L, -2 );
  lua_remove( L ,-2 );
end;

procedure lua_pushcfunction( L : Plua_State; f : lua_CFunction );
begin
  lua_pushcclosure( L, f, 0 );
end;

procedure luaL_getmetatable( L : PLua_State; name : PAnsiChar ); inline;
begin
  lua_getfield( L, LUA_REGISTRYINDEX, name );
end;

function luaL_dofile( L : Plua_State; fn : PAnsiChar ) : Integer; inline;
begin
  Result := luaL_loadfilex(L, fn, nil );
  if Result = 0 then
    Result := lua_pcallk(L, 0, 0, 0, 0, nil );
end;

function lua_strlen( aL: Plua_State; aIdx: LongInt): size_t; inline;
begin
  Exit( lua_rawlen( aL, aIdx ) );
end;

function lua_objlen( aL: Plua_State; aIdx: LongInt): size_t; inline;
begin
  Exit( lua_rawlen( aL, aIdx ) );
end;

function lua_pcall( aL: Plua_State; aNArgs, aNResults, aErrFunc : LongInt ): LongInt; inline;
begin
  Exit( lua_pcallk( aL, aNArgs, aNResults, aErrFunc, 0, nil ) );
end;

procedure lua_call( aL: Plua_State; aNArgs, aNResults : LongInt ); inline;
begin
  lua_callk( aL, aNArgs, aNResults, 0, nil );
end;

function luaL_loadfile( aL: Plua_State; aFileName : PAnsiChar): LongInt; inline;
begin
  Exit( luaL_loadfilex( aL, aFileName, nil ) );
end;

function luaL_loadbuffer( aL: Plua_State; aBuff: PAnsiChar; aSz: size_t; aName : PAnsiChar): LongInt; inline;
begin
  Exit( luaL_loadbufferx( aL, aBuff, aSz, aName, nil ) );
end;

procedure lua_getglobal( aL: Plua_State; const aVar : Ansistring ); inline;
begin
  lua_getglobal_( aL, PAnsiChar( aVar ) );
end;

procedure lua_setglobal( aL: Plua_State; const aVar : Ansistring ); inline;
begin
  lua_setglobal_( aL, PAnsiChar( aVar ) );
end;

function lua_open : Plua_State;
begin
  LoadLua;
  Exit( luaL_newstate() );
end;

procedure lua_register( aL : Plua_State; const aName : AnsiString; aF : lua_CFunction );
begin
  lua_pushcfunction( aL, aF );
  lua_setglobal( aL, PAnsiChar(aName) );
end;

finalization

if Lua <> nil then FreeAndNil( Lua );

end.

