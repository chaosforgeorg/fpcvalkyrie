{$INCLUDE valkyrie.inc}
unit vlua;
interface
uses variants, classes,
     vlualibrary, vnode, vutil, vdf, vrandom;

type ELuaException = vlualibrary.ELuaException;
     Plua_State    = vlualibrary.Plua_State;

var  LuaRNG : TRNG = nil;

function vlua_rng_random( L : Plua_State; aRNG : TRNG ) : Integer;

{ TLua }

type TLua = class(TVObject)
  constructor Create( aCoverState : Plua_State = nil ); virtual;

  procedure LoadFile( const aFileName : AnsiString );
  procedure StreamLoader( aIST : TStream; aStreamName : AnsiString; aSize : DWord );
  procedure StreamLoaderDestroy( aIST : TStream; aStreamName : AnsiString; aSize : DWord );
  procedure LoadStream( aDF : TVDataFile; const aStreamName : AnsiString ); overload;
  procedure LoadStream( aDF : TVDataFile; const aDirName, aFileName : AnsiString ); overload;

  procedure Register( const aName : AnsiString; aProc : lua_CFunction );
  procedure Register( const aKey, aValue : Variant );
  procedure Error( const aErrorString : Ansistring ); virtual;
  destructor Destroy; override;

private
  FLuaState  : Plua_State;
  FOwner     : Boolean;
  FErrorFunc : TLuaErrorFunc;

public
  property NativeState : Plua_state    read FLuaState;
  property ErrorFunc   : TLuaErrorFunc read FErrorFunc write FErrorFunc;
end;

implementation
uses SysUtils, vluaext;

function lua_math_random(L: Plua_State): Integer; cdecl;
begin
  Exit( vlua_rng_random( L, LuaRNG ) );
end;

function vlua_rng_random( L : Plua_State; aRNG : TRNG ) : Integer;
var iArgs : Byte;
    iArg1 : LongInt;
    iArg2 : LongInt;
begin
  iArgs := lua_gettop(L);
  case iArgs of
    0 : lua_pushnumber( L, aRNG.RDouble );
    1 : lua_pushnumber( L, aRNG.RLongInt( Round(lua_tonumber(L, 1)) ) + 1 );
    2 : begin
          iArg1 := Round(lua_tonumber(L, 1));
          iArg2 := Round(lua_tonumber(L, 2));
          if iArg2 >= iArg1 then
            lua_pushnumber( L, aRNG.RLongInt( iArg1, iArg2 ) )
          else
            lua_pushnumber( L, aRNG.RLongInt( iArg2, iArg1 ) )
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

constructor TLua.Create( aCoverState : Plua_State = nil );
begin
  LoadLua;
  if aCoverState = nil then
  begin
    FLuaState := lua_open;
    luaopen_base(FLuaState);
    luaopen_string(FLuaState);
    luaopen_table(FLuaState);
    luaopen_math(FLuaState);
    FOwner := True;
  end
  else
  begin
    FOwner := False;
    FLuaState := aCoverState;
  end;

  FErrorFunc  := nil;
  if LuaRNG = nil then LuaRNG := VRNG;
  lua_getglobal( FLuaState, 'math' );
  lua_pushstring( FLuaState, 'random' );
  lua_pushcfunction(FLuaState, @lua_math_random );
  lua_rawset(FLuaState, -3);
  lua_getglobal( FLuaState, 'math' );
  lua_pushstring( FLuaState, 'randomseed' );
  lua_pushcfunction(FLuaState, @lua_math_randomseed );
  lua_rawset(FLuaState, -3);
  lua_pop(FLuaState, 1);
end;

procedure TLua.LoadFile( const aFileName : AnsiString );
begin
  if luaL_dofile(FLuaState, PChar(aFileName)) <> 0 then
    raise ELuaException.Create(lua_tostring(FLuaState,-1));
end;

procedure TLua.LoadStream( aDF: TVDataFile; const aStreamName: AnsiString );
var iStream : TStream;
    iSize   : Int64;
begin
  iStream := aDF.GetFile(aStreamName);
  iSize   := aDF.GetFileSize(aStreamName);
  StreamLoaderDestroy(iStream,aStreamName,iSize);
end;

procedure TLua.LoadStream( aDF: TVDataFile; const aDirName, aFileName: AnsiString );
var iStream : TStream;
    iSize   : Int64;
begin
  iStream := aDF.GetFile(aFileName,aDirName);
  iSize   := aDF.GetFileSize(aFileName,aDirName);
  StreamLoaderDestroy(iStream,aFileName,iSize);
end;

procedure TLua.StreamLoader( aIST : TStream; aStreamName : AnsiString; aSize : DWord);
var iBuf  : PByte;
begin
  Log('Loading LUA stream -- "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
  GetMem(iBuf,aSize);
  Log('Reading "'+aStreamName+'" ('+IntToStr(aIST.Position)+'-'+IntToStr(aIST.Position+aSize)+')');
  aIST.ReadBuffer(iBuf^,aSize);
  if ( luaL_loadbuffer(FLuaState,PChar(iBuf),aSize,PChar(aStreamName)) <> 0 )
  or ( lua_pcall(FLuaState, 0, 0, 0) <> 0 ) then
  begin
    Error(aStreamName+': '+lua_tostring(FLuaState,-1));
    lua_pop(FLuaState,1);
  end;

  FreeMem(iBuf);
  Log('Loaded "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
end;

procedure TLua.StreamLoaderDestroy( aIST: TStream; aStreamName: AnsiString; aSize: DWord );
var iBuf : PByte;
begin
  Log('Loading LUA stream -- "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
  GetMem(iBuf,aSize);
  Log('Reading "'+aStreamName+'" ('+IntToStr(aIST.Position)+'-'+IntToStr(aIST.Position+aSize)+')');
  aIST.ReadBuffer(iBuf^,aSize);
  FreeAndNil(aIST);
  if ( luaL_loadbuffer(FLuaState,PChar(iBuf),aSize,PChar(aStreamName)) <> 0 )
  or ( lua_pcall(FLuaState, 0, 0, 0) <> 0 ) then
  begin
    Error(aStreamName+': '+lua_tostring(FLuaState,-1));
    lua_pop(FLuaState,1);
  end;

  FreeMem(iBuf);
  Log('Loaded "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
end;


procedure TLua.Register( const aName : AnsiString; aProc : lua_CFunction);
begin
  lua_register(FLuaState, aName, aProc);
end;

procedure TLua.Register( const aKey, aValue: Variant );
begin
  vlua_pushvariant( FLuaState, aKey );
  vlua_pushvariant( FLuaState, aValue );
  lua_rawset_global( FLuaState );
end;

procedure TLua.Error( const aErrorString: Ansistring );
begin
  if Assigned( FErrorFunc ) then
    FErrorFunc( aErrorString )
  else
    Log('LuaError: '+aErrorString);
end;

destructor TLua.Destroy;
begin
  if FOwner then
  begin
    lua_close(FLuaState);
    Log('Lua closed.');
  end;
  inherited Destroy;
end;

initialization
  LuaRNG := nil;

end.

