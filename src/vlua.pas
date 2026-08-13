{$INCLUDE valkyrie.inc}
unit vlua;
interface
uses Variants, vlualibrary, vnode,vutil,classes,vdf, vrandom;


type
  ELuaException         = vlualibrary.ELuaException;


type
  Plua_State    = vlualibrary.Plua_State;
  TLuaErrorFunc = procedure ( const ErrorString : Ansistring ) of object;

var
  LuaRNG : TRNG = nil;

{ TLua }

type TLua = class(TVObject)
  constructor Create( coverState : Plua_State = nil ); virtual;

  procedure LoadFile(const FileName : AnsiString);
  procedure StreamLoader(IST : TStream; StreamName : AnsiString;  Size : DWord);
  procedure StreamLoaderDestroy(IST : TStream; StreamName : AnsiString;  Size : DWord);
  procedure LoadStream( DF : TVDataFile; const StreamName : AnsiString); overload;
  procedure LoadStream( DF : TVDataFile; const DirName, FileName : AnsiString ); overload;

  procedure Register(const Name : AnsiString; Proc : lua_CFunction);
  procedure Register(const Key, Value : Variant);
  procedure Error(const ErrorString : Ansistring); virtual;
  destructor Destroy; override;

public
  LuaState    : Plua_State;
  Owner       : Boolean;
  FErrorFunc  : TLuaErrorFunc;

public
  property NativeState : Plua_state read LuaState;
end;

implementation
uses SysUtils, vluaext;

function lua_math_random(L: Plua_State): Integer; cdecl;
var iArgs : Byte;
    iArg1 : LongInt;
    iArg2 : LongInt;
begin
  iArgs := lua_gettop(L);
  case iArgs of
    0 : lua_pushnumber( L, LuaRNG.RDouble );
    1 : lua_pushnumber( L, LuaRNG.RLongInt( Round(lua_tonumber(L, 1)) ) + 1 );
    2 : begin
          iArg1 := Round(lua_tonumber(L, 1));
          iArg2 := Round(lua_tonumber(L, 2));
          if iArg2 >= iArg1 then
            lua_pushnumber( L, LuaRNG.RLongInt( iArg1, iArg2 ) )
          else
            lua_pushnumber( L, LuaRNG.RLongInt( iArg2, iArg1 ) )
        end;
    else Exit(0);
  end;
  Result := 1;
end;

function lua_math_randomseed(L: Plua_State): Integer; cdecl;
var iArgs : Byte;
begin
  iArgs := lua_gettop(L);
  case iArgs of
    0 : LuaRNG.Randomize;
    1 : LuaRNG.SetSeed( DWord( lua_tointeger(L, 1) ) );
  end;
  Exit(0);
end;

constructor TLua.Create( coverState : Plua_State = nil );
begin
  LoadLua;
  if coverState = nil then
  begin
    LuaState := lua_open;
    luaopen_base(LuaState);
    luaopen_string(LuaState);
    luaopen_table(LuaState);
    luaopen_math(LuaState);
    Owner := True;
  end
  else
  begin
    Owner := False;
    LuaState := coverState;
  end;

  FErrorFunc  := nil;
  if LuaRNG = nil then LuaRNG := VRNG;
  lua_getglobal( LuaState, 'math' );
  lua_pushstring( LuaState, 'random' );
  lua_pushcfunction(LuaState, @lua_math_random );
  lua_rawset(LuaState, -3);
  lua_getglobal( LuaState, 'math' );
  lua_pushstring( LuaState, 'randomseed' );
  lua_pushcfunction(LuaState, @lua_math_randomseed );
  lua_rawset(LuaState, -3);
  lua_pop(LuaState, 1);
end;

procedure TLua.LoadFile(const FileName : AnsiString);
begin
  if luaL_dofile(LuaState, PChar(FileName)) <> 0 then
    raise ELuaException.Create(lua_tostring(LuaState,-1));
end;

procedure TLua.LoadStream(DF: TVDataFile; const StreamName: AnsiString);
var Stream : TStream;
    Size   : Int64;
begin
  Stream := DF.GetFile(StreamName);
  Size   := DF.GetFileSize(StreamName);
  StreamLoaderDestroy(Stream,StreamName,Size);
end;

procedure TLua.LoadStream(DF: TVDataFile; const DirName, FileName: AnsiString);
var Stream : TStream;
    Size   : Int64;
begin
  Stream := DF.GetFile(FileName,DirName);
  Size   := DF.GetFileSize(FileName,DirName);
  StreamLoaderDestroy(Stream,FileName,Size);
end;

procedure TLua.StreamLoader(IST : TStream; StreamName : AnsiString;  Size : DWord);
var Buf  : PByte;
begin
  Log('Loading LUA stream -- "'+StreamName+'" ('+IntToStr(Size)+'b)');
  GetMem(Buf,Size);
  Log('Reading "'+StreamName+'" ('+IntToStr(IST.Position)+'-'+IntToStr(IST.Position+Size)+')');
  IST.ReadBuffer(Buf^,Size);
  if ( luaL_loadbuffer(LuaState,PChar(Buf),Size,PChar(StreamName)) <> 0 )
  or ( lua_pcall(LuaState, 0, 0, 0) <> 0 ) then
  begin
    Error(StreamName+': '+lua_tostring(LuaState,-1));
    lua_pop(LuaState,1);
  end;

  FreeMem(Buf);
  Log('Loaded "'+StreamName+'" ('+IntToStr(Size)+'b)');
end;

procedure TLua.StreamLoaderDestroy(IST: TStream; StreamName: AnsiString; Size: DWord);
var Buf  : PByte;
begin
  Log('Loading LUA stream -- "'+StreamName+'" ('+IntToStr(Size)+'b)');
  GetMem(Buf,Size);
  Log('Reading "'+StreamName+'" ('+IntToStr(IST.Position)+'-'+IntToStr(IST.Position+Size)+')');
  IST.ReadBuffer(Buf^,Size);
  FreeAndNil(ISt);
  if ( luaL_loadbuffer(LuaState,PChar(Buf),Size,PChar(StreamName)) <> 0 )
  or ( lua_pcall(LuaState, 0, 0, 0) <> 0 ) then
  begin
    Error(StreamName+': '+lua_tostring(LuaState,-1));
    lua_pop(LuaState,1);
  end;

  FreeMem(Buf);
  Log('Loaded "'+StreamName+'" ('+IntToStr(Size)+'b)');
end;


procedure TLua.Register(const Name : AnsiString; Proc : lua_CFunction);
begin
  lua_register(LuaState, Name, Proc);
end;

procedure TLua.Register(const Key, Value: Variant);
begin
  vlua_pushvariant( LuaState, key );
  vlua_pushvariant( LuaState, value );
  lua_rawset_global( LuaState );
end;

procedure TLua.Error(const ErrorString: Ansistring);
begin
  if Assigned( FErrorFunc ) then
    FErrorFunc( ErrorString )
  else
    Log('LuaError: '+ErrorString);
end;

destructor TLua.Destroy;
begin
  if Owner then
  begin
    lua_close(LuaState);
    Log('Lua closed.');
  end;
  inherited Destroy;
end;

initialization
  LuaRNG := nil;

end.

