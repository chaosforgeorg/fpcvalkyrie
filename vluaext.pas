{$INCLUDE valkyrie.inc}
unit vluaext;
interface
uses sysutils, classes, variants, vvector, vlualibrary, typinfo;

type TOpenByteArray    = array of Byte;
type TOpenWordArray    = array of Word;
type TOpenDWordArray   = array of DWord;
type TOpenIntegerArray = array of Integer;
type TOpenStringArray  = array of AnsiString;
type TOpenFloatArray   = array of Single;


function  vlua_tostring( L : Plua_State; idx : Integer) : AnsiString;
function  vlua_absindex( L: Plua_State; idx: Integer): Integer;
function  vlua_functionexists( L: PLua_State; func: AnsiString; idx: Integer ): boolean;

procedure vlua_pushvariant( L: PLua_State; v: Variant);
function  vlua_tovariant( L: Plua_State; idx: Integer): Variant; cdecl;
function  vlua_tovariant( L : Plua_State; idx : Integer; default : Variant ) : Variant;
procedure vlua_pushvarrec( L: PLua_State; pv: PVarRec);
function  vlua_getvarrecfield( L: PLua_State; idx: Integer; pv: PVarRec) : Boolean;
function  vlua_tabletovararray( L: Plua_State; idx: Integer): Variant;

function  vlua_callfunction(L : PLua_State; name : AnsiString; const args : Array of Variant; idx : Integer = 0) : Variant;
function  vlua_testudata(L : Plua_State; ud : Integer; const tname : PChar) : Pointer;

function  vlua_tobytearray(L: Plua_State; idx: Integer): TOpenByteArray;
function  vlua_towordarray(L: Plua_State; idx : Integer ) : TOpenWordArray;
function  vlua_todwordarray(L: Plua_State; idx : Integer ) : TOpenDWordArray;
function  vlua_tointegerarray(L: Plua_State; idx : Integer ) : TOpenIntegerArray;
function  vlua_tostringarray(L: Plua_State; idx : Integer ) : TOpenStringArray;
function  vlua_tofloatarray(L: Plua_State; idx : Integer ) : TOpenFloatArray;

procedure vlua_deepcopy(L: Plua_State; index: Integer);
procedure vlua_shallowcopy(L: Plua_State; index: Integer);
procedure vlua_shallowmerge(L: Plua_State; index : Integer);

procedure vlua_tostream(L: Plua_State; index : Integer; out_stream: TStream);
procedure vlua_pushfromstream(L: Plua_State; in_stream: TStream);
procedure vlua_tabletostream(L: Plua_State; index : Integer; out_stream: TStream);
procedure vlua_tablefromstream(L: Plua_State; index : Integer; in_stream: TStream);

function  vlua_getpath( L: Plua_State; const path : AnsiString; idx : Integer = 0; max : Integer = -1 ) : Boolean;
function  vlua_getpath( L: Plua_State; const path : array of const; idx : Integer = 0; max : Integer = -1 ) : Boolean;

procedure vlua_gettableorcreate( L: Plua_State; const table : AnsiString );

procedure vlua_register( L: Plua_State; const funcname : AnsiString; func : lua_CFunction; idx : Integer = 0 );
procedure vlua_register( L: Plua_State; const lr : PluaL_Reg; idx : Integer = 0 );
procedure vlua_register( L: Plua_State; const libname, funcname : AnsiString; func : lua_CFunction );
procedure vlua_register( L: Plua_State; const libname : AnsiString; const lr : PluaL_Reg );

function vlua_loadstream( L: Plua_State; Stream : TStream; Size : DWord = 0; StreamName : AnsiString = '' ) : Integer;
procedure vlua_registerenumvalues( L : Plua_State; idx : Integer; enuminfo : PTypeInfo; UpperCase : Boolean = false );

procedure vlua_table_toset(L: Plua_State; idx: Integer);
procedure vlua_table_tokeyset( L: Plua_State; index : Integer );

function  vlua_tovec2f(L: Plua_State; idx: Integer): TVec2f;
function  vlua_tovec3f(L: Plua_State; idx: Integer): TVec3f;
function  vlua_tovec4f(L: Plua_State; idx: Integer): TVec4f;

function  vlua_tovec2i(L: Plua_State; idx: Integer): TVec2i;
function  vlua_tovec3i(L: Plua_State; idx: Integer): TVec3i;
function  vlua_tovec4i(L: Plua_State; idx: Integer): TVec4i;

function  vlua_tovec2b(L: Plua_State; idx: Integer): TVec2b;
function  vlua_tovec3b(L: Plua_State; idx: Integer): TVec3b;
function  vlua_tovec4b(L: Plua_State; idx: Integer): TVec4b;

implementation

uses strutils;

function vlua_tostring(L : Plua_State; idx : Integer) : AnsiString;
var size  : Integer;
    ltype : Integer;
begin
  ltype := lua_type( L, idx );
  if (ltype <> LUA_TSTRING) and (ltype <> LUA_TNUMBER) then Exit('');
  size := lua_strlen( L, idx );
  SetLength( Result, size );
  if size > 0 then Move( lua_tolstring( L, idx, nil )^, Result[1], size );
end;

function vlua_absindex(L: Plua_State; idx: Integer): Integer;
begin
  if (idx > -1) or (idx = LUA_REGISTRYINDEX) then Exit( idx );
  Exit( idx + lua_gettop(L) + 1 );
end;

function vlua_functionexists(L: PLua_State; func: AnsiString; idx: Integer): boolean;
begin
  lua_pushansistring( L, func );
  lua_rawget( L, idx );
  if lua_isnil( L, lua_gettop(L) )
    then Result := False
    else Result := lua_isfunction( L, lua_gettop(L) );
  lua_pop( L, 1 );
end;

procedure vlua_pushvariant(L: PLua_State; v: Variant);
var size, count : LongWord;
begin
  case VarType(v) of
    varEmpty,
    varNull    : lua_pushnil(L);
    varBoolean : lua_pushboolean(L, v);
    varStrArg,
    varOleStr,
    varString  : lua_pushansistring(L, v);
    varDate    : lua_pushansistring(L, DateTimeToStr(VarToDateTime(v)));
    varArray   : begin
                   size := VarArrayHighBound(v, 1);
                   lua_newtable(L);
                   for count := 0 to size do
                     begin
                       lua_pushinteger(L, count+1);
                       vlua_pushvariant(L, v[count]);
                       lua_settable(L, -3);
                     end;
                 end;
  else
    lua_pushnumber(L, Double(VarAsType(v, varDouble)));
  end;
end;

function vlua_tovariant(L: Plua_State; idx: Integer): Variant; cdecl;
var typ : Integer;
    num : Double;
begin
  typ := lua_type(L, idx);
  case typ of
    LUA_TSTRING          : Result := VarAsType(lua_tostring(L, idx), varString);
    LUA_TNONE,
    LUA_TNIL             : Result := NULL;
    LUA_TBOOLEAN         : Result := VarAsType(lua_toboolean(L, idx), varBoolean);
    LUA_TNUMBER          : begin
                             num := lua_tonumber(L, idx);
                             if Abs(num) > MaxInt then
                               Result := VarAsType(num, varDouble)
                             else
                               begin
                                 if Frac(num)<>0 then
                                   Result := VarAsType( num, varDouble )
                                 else
                                   Result := Round( Double(VarAsType(num, varDouble)) );
                               end;
                           end;
    LUA_TTABLE           : result := vlua_tabletovararray(L, idx);
  else
    result := NULL;
  end;
end;

function vlua_tovariant ( L : Plua_State; idx : Integer; default : Variant ) : Variant;
begin
  vlua_tovariant := vlua_tovariant( L, idx );
  if vlua_tovariant = NULL then vlua_tovariant := default;
end;

procedure vlua_pushvarrec(L: PLua_State; pv: PVarRec);
begin
case pv^.vtype of
  vtInteger    : lua_pushinteger( L, pv^.VInteger );
  vtBoolean    : lua_pushboolean( L, pv^.VBoolean );
  vtChar       : lua_pushansistring( L, pv^.VChar );
  vtWideChar   : lua_pushlstring( L, @(pv^.VWideChar), 2 );
  vtExtended   : lua_pushnumber( L, pv^.VExtended^ );
  vtString     : lua_pushansistring( L, pv^.VString^ );
  vtPChar      : lua_pushstring( L, pv^.VPChar );
  vtPWideChar  : lua_pushansistring( L, pv^.VPWideChar );
  vtAnsiString : lua_pushstring( L, pv^.VAnsiString );
  vtVariant    : vlua_pushvariant( L, pv^. VVariant^ );
  vtWideString : lua_pushstring( L, pv^.VWideString );
  vtInt64      : lua_pushnumber( L, pv^.VInt64^ );
  vtQWord      : lua_pushnumber( L, pv^.VQWord^ );
//  vtCurrency   : lua_pushnil( L );
//  vtInterface  : lua_pushnil( L );
//  vtPointer    : lua_pushnil( L );
//  vtObject     : lua_pushnil( L );
//  vtClass      : lua_pushnil( L );
else
  lua_pushnil( L );
end;
end;

function vlua_getvarrecfield ( L : PLua_State; idx : Integer; pv : PVarRec ) : Boolean;
var pc : PChar;
begin
  case pv^.vtype of
    vtinteger,
    vtint64,
    vtqword,
    vtboolean,
    vtchar       :
    begin
      if idx = 0 then Exit( False );
      vlua_pushvarrec( L, pv );
      lua_gettable( L, idx-1 );
      Exit( true );
    end;
    vtString     : pc := PChar( AnsiString(pv^.VString^) );
    vtPChar      : pc := pv^.VPChar;
    vtAnsiString : pc := PChar( AnsiString(pv^.VAnsiString) );
  else
    Exit(False);
  end;
  if idx = 0
    then lua_getglobal( L, pc )
    else lua_getfield( L, idx, pc );
  Exit( true );
end;

function vlua_tabletovararray(L: Plua_State; idx: Integer): Variant;
var cnt : Integer;
    va  : array of Variant;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil(L);

  cnt := 0;

  while lua_next(L, idx) <> 0 do
  begin
    SetLength(va, cnt+1);
    va[cnt] := vlua_tovariant(l, -1);
    lua_pop(L, 1);
    inc(cnt);
  end;

  if cnt > 0 then
  begin
    Result := VarArrayCreate([0,cnt-1], varvariant);
    while cnt > 0 do
    begin
      dec(cnt);
      Result[cnt] := va[cnt];
    end;
  end
  else
    result := VarArrayCreate([0,0], varvariant);
end;

function vlua_callfunction(L: PLua_State; name: AnsiString;
  const args: array of Variant; idx: Integer): Variant;
var nargs,  i :Integer;
    msg : AnsiString;
begin
  lua_pushansistring(L, name);
  if idx = 0
    then lua_rawget_global(L)
    else lua_rawget(L, idx);
  if not lua_isfunction( L, -1) then raise ELuaException.Create(Name+' not found!');
  NArgs := High(Args);
  for i:=0 to NArgs do
    vlua_pushvariant(l, args[i]);
  if lua_pcall(l, NArgs+1, 1, 0) <> 0 then
  begin
    msg := lua_tostring(l, -1);
    lua_pop(l, 1);
    raise Exception.Create(msg);
  end;
  vlua_callfunction := vlua_tovariant(L, -1);
  lua_pop(l, 1);
end;

function vlua_tobytearray(L: Plua_State; idx: Integer): TOpenByteArray;
var cnt : Word;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  cnt := 0;
  while lua_next(L, idx) <> 0 do
  begin
    SetLength(vlua_tobytearray, cnt+1);
    vlua_tobytearray[cnt] := Byte( lua_tointeger(l, -1) );
    lua_pop(L, 1);
    inc(cnt);
  end;
end;

function vlua_towordarray(L: Plua_State; idx: Integer): TOpenWordArray;
var cnt : Word;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  cnt := 0;
  while lua_next(L, idx) <> 0 do
  begin
    SetLength(vlua_towordarray, cnt+1);
    vlua_towordarray[cnt] := Word( lua_tointeger(l, -1) );
    lua_pop(L, 1);
    inc(cnt);
  end;
end;

function vlua_todwordarray(L: Plua_State; idx: Integer): TOpenDWordArray;
var cnt : Word;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  cnt := 0;
  while lua_next(L, idx) <> 0 do
  begin
    SetLength(vlua_todwordarray, cnt+1);
    vlua_todwordarray[cnt] := DWord( lua_tointeger(l, -1) );
    lua_pop(L, 1);
    inc(cnt);
  end;
end;

function vlua_tointegerarray(L: Plua_State; idx: Integer): TOpenIntegerArray;
var cnt : Word;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  cnt := 0;
  while lua_next(L, idx) <> 0 do
  begin
    SetLength(vlua_tointegerarray, cnt+1);
    vlua_tointegerarray[cnt] := lua_tointeger(l, -1);
    lua_pop(L, 1);
    inc(cnt);
  end;
end;

function vlua_tostringarray(L: Plua_State; idx: Integer): TOpenStringArray;
var cnt : Word;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  cnt := 0;
  while lua_next(L, idx) <> 0 do
  begin
    SetLength(vlua_tostringarray, cnt+1);
    vlua_tostringarray[cnt] := lua_tostring(l, -1);
    lua_pop(L, 1);
    inc(cnt);
  end;
end;

function vlua_tofloatarray(L: Plua_State; idx: Integer): TOpenFloatArray;
var cnt : Word;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  cnt := 0;
  while lua_next(L, idx) <> 0 do
  begin
    SetLength(vlua_tofloatarray, cnt+1);
    vlua_tofloatarray[cnt] := lua_tonumber(l, -1);
    lua_pop(L, 1);
    inc(cnt);
  end;
end;

function vlua_testudata(L : Plua_State; ud : Integer; const tname : PChar) : Pointer;
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
      end
    end;
  Exit( nil );
end;

procedure vlua_deepcopy(L: Plua_State; index: Integer);
begin
  index := lua_absindex(L,index);
  lua_newtable(L);
  lua_pushnil(L);

  while lua_next( L, index ) <> 0 do
  begin
    if lua_istable( L, -1 ) then
    begin
      vlua_deepcopy( L, -1 );
      lua_insert( L, -2 );
      lua_pop( L, 1 );
    end;
    lua_pushvalue(L, -2);
    lua_insert(L, -2);
    lua_settable(L, -4);
  end;
end;

procedure vlua_shallowcopy(L: Plua_State; index : Integer);
begin
  index := lua_absindex(L,index);
  lua_newtable(L);
  lua_pushnil(L);
  while lua_next(L, index) <> 0 do
  begin
    lua_pushvalue(L, -2);
    lua_insert(L, -2);
    lua_settable(L, -4);
  end
end;

procedure vlua_shallowmerge(L: Plua_State; index : Integer);
begin
  index := lua_absindex(L,index);
  lua_pushnil(L);
  while lua_next(L, index) <> 0 do
  begin
    lua_pushvalue(L, -2);
    lua_insert(L, -2);
    lua_rawset(L, -4);
  end
end;

function aux_booleantobyte( b : boolean ) : byte;
begin
  if b then Exit( 1 ) else Exit( 0 );
end;

function aux_bytetoboolean( b : byte ) : boolean;
begin
  Exit( b <> 0 );
end;

procedure vlua_tostream(L: Plua_State; index : Integer; out_stream: TStream);
var lnumber : LUA_NUMBER;
begin
  index := lua_absindex(L,index);
  out_stream.WriteByte( lua_type(L, index) );
  case lua_type(L, index) of
    LUA_TBOOLEAN : out_stream.WriteByte( aux_booleantobyte( lua_toboolean(L, index) ) );
    LUA_TSTRING  : out_stream.WriteAnsiString( lua_tostring( L, index ) );
    LUA_TTABLE   : vlua_tabletostream( L, index, out_stream );
    LUA_TNUMBER  : begin lnumber := lua_tonumber( L, index ); out_stream.Write( lnumber, sizeof( LUA_NUMBER ) ); end;
    LUA_TNIL     : ;
    else raise ELuaException.Create('Trying to stream improper type : '+lua_typename( L, lua_type(L, -1) )+'!');
  end;
end;

procedure vlua_pushfromstream(L: Plua_State; in_stream: TStream);
var ltype   : byte;
    lnumber : LUA_NUMBER;
begin
  {$HINTS OFF}
  ltype := in_stream.ReadByte();
  case ltype of
    LUA_TBOOLEAN : lua_pushboolean( L, aux_bytetoboolean( in_stream.ReadByte() ) );
    LUA_TSTRING  : lua_pushansistring( L, in_stream.ReadAnsiString() );
    LUA_TNUMBER  : begin in_stream.Read( lnumber, sizeof( LUA_NUMBER ) ); lua_pushnumber( L, lnumber ); end;
    LUA_TTABLE   : begin lua_newtable( L ); vlua_tablefromstream( L, -1, in_stream ); end;
    LUA_TNIL     : lua_pushnil( L );
    else raise ELuaException.Create('Improper type in stream: '+lua_typename( L, ltype )+'!');
  end;
  {$HINTS ON}
end;

procedure vlua_tabletostream(L: Plua_State; index : Integer; out_stream: TStream);
begin
  index := lua_absindex(L,index);
  lua_pushnil(L);
  while lua_next(L, index) <> 0 do
  begin
    // key (index -2), 'value' (index -1)
    vlua_tostream( L, -2, out_stream );
    vlua_tostream( L, -1, out_stream );
    // remove value, keep key
    lua_pop(L, 1);
  end;
  // stream additional nil, so we know not to read further values
  out_stream.WriteByte( LUA_TNIL );
end;

procedure vlua_tablefromstream(L: Plua_State; index : Integer; in_stream: TStream);
begin
  index := lua_absindex(L,index);
  // push first key
  vlua_pushfromstream(L, in_stream);
  while not lua_isnil(L,-1) do
  begin
    vlua_pushfromstream(L, in_stream);
    lua_settable( L, -3 );
    vlua_pushfromstream(L, in_stream);
  end;
  // pop extra nil
  lua_pop(L,1);
end;

function vlua_getpath( L : Plua_State; const path : AnsiString; idx : Integer; max : Integer ) : Boolean;
var Piece : AnsiString;
    Count : Integer;
begin
  idx := lua_absindex( L, idx );
  Count := 0;
  if Pos('.',path) = 0 then
  begin
    if idx = 0
      then lua_getglobal( L, PChar(Path) )
      else lua_getfield( L, idx, PChar(Path) );
  end
  else
  repeat
    Piece := ExtractDelimited( Count, path, ['.'] );
    if Piece = '' then
      if Count = 0 then Exit(False)
                   else break;
    if Count = 0 then
    begin
      if idx = 0
        then lua_getglobal( L, PChar(Piece) )
        else lua_getfield( L, idx, PChar(Piece) );
    end
    else
      if lua_istable( L, -1 ) then
      begin
        lua_getfield( L, -1, PChar(Piece) );
        lua_insert( L, -2 );
        lua_pop( L, 1 );
      end
      else
      begin
        lua_pop( L, 1 );
        Exit(False);
      end;
    Inc(Count);
  until Count = Max;
  if lua_isnil( L, -1 ) then
  begin
    lua_pop( L, 1 );
    Exit( False );
  end;
  Exit( True );
end;

function vlua_getpath ( L : Plua_State; const path : array of const; idx : Integer; max : Integer ) : Boolean;
var i     : Integer;
begin
  idx := lua_absindex( L, idx );
  if Max = -1 then Max := High(Path);
  if Max < 0 then Exit( false );
  for i:=0 to Max do
  begin
    if i <> 0 then
    begin
      idx := -1;
      if not lua_istable( L, idx ) then
      begin
        lua_pop( L, 1 );
        Exit( false );
      end;
    end;
    if not vlua_getvarrecfield( L, idx, @(Path[i]) ) then
    begin
      lua_pop( L, 1 );
      Exit( false );
    end;
    if idx = -1 then
    begin
      lua_insert( L, -2 );
      lua_pop( L, 1 );
    end;
  end;
  if lua_isnil( L, -1 ) then
  begin
    lua_pop( L, 1 );
    Exit( False );
  end;
  Exit( True );
end;

procedure vlua_gettableorcreate( L: Plua_State; const table : AnsiString );
begin
  lua_getglobal( L, table );
  if lua_isnil( L, -1 ) then
  begin
    lua_pop( L, 1 );
    lua_createtable( L, 0, 0 );
    lua_pushvalue( L, -1 );
    lua_setglobal( L, table );
  end
end;

procedure vlua_register ( L : Plua_State; const funcname : AnsiString; func : lua_CFunction; idx : Integer ) ;
begin
  if idx <> 0 then idx := lua_absindex( L, idx );
  lua_pushansistring( L, funcname );
  lua_pushcfunction( L, func );
  if idx = 0
    then lua_rawset_global(L)
    else lua_rawset(L, idx);
end;

procedure vlua_register ( L : Plua_State; const lr : PluaL_Reg; idx : Integer ) ;
var count : integer;
begin
  if idx <> 0 then idx := lua_absindex( L, idx );
  count := 0;
  while lr[ count ].name <> nil do
  begin
    vlua_register( L, lr[ count ].name, lr[ count ].func, idx );
    Inc( count );
  end;
end;

procedure vlua_register ( L : Plua_State; const libname, funcname : AnsiString; func : lua_CFunction ) ;
begin
  lua_getglobal( L, libname );
  if lua_isnil( L, -1 ) then
  begin
    lua_pop( L, 1 );
    lua_createtable( L, 0, 0 );
    vlua_register( L, funcname, func, -1 );
    lua_setglobal( L, libname );
  end
  else
  begin
    if not lua_istable( L, -1 ) then
    begin
      lua_pop( L, 1 );
      raise ELuaException.Create('Register called with '+libname+' not being a table!');
    end;
    vlua_register( L, funcname, func, -1 );
    lua_pop( L, 1 );
  end;
end;


procedure vlua_register ( L : Plua_State; const libname : AnsiString; const lr : PluaL_Reg ) ;
begin
  lua_getglobal( L, libname );
  if lua_isnil( L, -1 ) then
  begin
    lua_pop( L, 1 );
    lua_createtable( L, 0, 0 );
    vlua_register( L, lr, -1 );
    lua_setglobal( L, libname );
  end
  else
  begin
    if not lua_istable( L, -1 ) then
    begin
      lua_pop( L, 1 );
      raise ELuaException.Create('Register called with '+libname+' not being a table!');
    end;
    vlua_register( L, lr, -1 );
    lua_pop( L, 1 );
  end;
end;

function vlua_loadstream ( L : Plua_State; Stream : TStream; Size : DWord; StreamName : AnsiString = ''  ) : Integer;
var Data : PByte;
begin
  if Size = 0 then Size := Stream.Size;
  if StreamName = '' then StreamName := Stream.ClassName;
  GetMem(Data,Size);
  Stream.ReadBuffer(Data^,Size);
  vlua_loadstream := luaL_loadbuffer( L, PChar(Data), Size, PChar(StreamName) );
  FreeMem(Data);
end;

procedure vlua_registerenumvalues ( L : Plua_State; idx : Integer; enuminfo : PTypeInfo; UpperCase : Boolean ) ;
var count, i : DWord;
    name     : AnsiString;
begin
  idx := lua_absindex( L, idx );
  count := GetEnumNameCount( enuminfo );
  if count > 0 then
  for i := 0 to Count-1 do
  begin
    name  := GetEnumName( enuminfo, i );
    if UpperCase then name := UpCase(name);
    lua_pushansistring( L, name );
    lua_pushinteger( L, GetEnumValue( enuminfo, name ) );
    lua_rawset( L, idx );
  end;
end;

procedure vlua_table_toset(L: Plua_State; idx: Integer);
var i : Integer;
begin
  idx := lua_absindex( L, idx );
  lua_newtable(L);
  i := 0;
  while ( true ) do
  begin
    Inc( i );
    lua_rawgeti(L, idx, i);
    if ( lua_isnil( L, -1 ) ) then
    begin
      lua_pop( L, 1 );
      break;
    end;
    lua_pushboolean( L, true );
    lua_rawset(L, -3);
  end;
end;

procedure vlua_table_tokeyset( L: Plua_State; index : Integer );
begin
  index := lua_absindex( L, index );
  lua_newtable(L);
  lua_pushnil(L);

  while lua_next( L, index ) <> 0 do
  begin
    lua_pushvalue(L, -2);
    lua_pushboolean(L, true);
    lua_settable(L, -5);
    lua_pop(L, 1);
  end;
end;

function vlua_tovec2f ( L : Plua_State; idx : Integer ) : TVec2f;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 1 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tonumber(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

function vlua_tovec3f ( L : Plua_State; idx : Integer ) : TVec3f;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 2 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tonumber(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

function vlua_tovec4f ( L : Plua_State; idx : Integer ) : TVec4f;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 3 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tonumber(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

function vlua_tovec2i ( L : Plua_State; idx : Integer ) : TVec2i;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 1 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tointeger(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

function vlua_tovec3i ( L : Plua_State; idx : Integer ) : TVec3i;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 2 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tointeger(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

function vlua_tovec4i ( L : Plua_State; idx : Integer ) : TVec4i;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 3 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tointeger(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;


function vlua_tovec2b ( L : Plua_State; idx : Integer ) : TVec2b;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 1 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tointeger(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

function vlua_tovec3b ( L : Plua_State; idx : Integer ) : TVec3b;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 2 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tointeger(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

function vlua_tovec4b ( L : Plua_State; idx : Integer ) : TVec4b;
var cnt : Word;
begin
  Result.Init;
  idx := lua_absindex( L, idx );
  lua_pushnil(L);
  for cnt := 0 to 3 do
  begin
    if lua_next(L, idx) = 0 then Exit;
    Result.Data[cnt] := lua_tointeger(l, -1);
    lua_pop( L, 1 );
  end;
  lua_pop( L, 1 );
end;

end.

