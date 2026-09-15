{$INCLUDE valkyrie.inc}
unit vluastate;
interface
uses variants, classes, vlualibrary, vobject, vutil, vdf, vrandom;

type ELuaException = vlualibrary.ELuaException;
     Plua_State    = vlualibrary.Plua_State;

function vlua_rng_random( L : Plua_State; aRNG : TRNG ) : Integer;

{ TLuaState }

type TLuaState = class(TVObject)
  constructor Create( aCoverState : PLua_State = nil ); virtual;

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
  FState  : Plua_State;
  FOwner     : Boolean;
  FErrorFunc : TLuaErrorFunc;

public
  property Owner       : Boolean      read FOwner;
  property NativeState : Plua_state    read FState;
  property ErrorFunc   : TLuaErrorFunc read FErrorFunc write FErrorFunc;
end;

implementation
uses sysutils, vluaext, vlua;

function lua_math_random( L : PLua_State ) : Integer; cdecl;
var iRNG : TRNG;
begin
  iRNG := TLuaContext.RequireRNG( L );
  Exit( vlua_rng_random( L, iRNG ) );
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

function lua_math_randomseed( L : PLua_State ) : Integer; cdecl;
var iRNG  : TRNG;
    iArgs : Byte;
begin
  iRNG := TLuaContext.RequireRNG( L );
  iArgs := lua_gettop(L);
  case iArgs of
    0 : iRNG.Randomize;
    1 : iRNG.SetSeed( DWord( lua_tointeger(L, 1) ) );
  end;
  Exit(0);
end;

{$PUSH}
{$Q-}
{$R-}
function lua_math_mix_seed( L : Plua_State ) : Integer; cdecl;
var iValue : DWord;
begin
  iValue := DWord( luaL_checkinteger( L, 1 ) ) xor
    ( DWord( luaL_checkinteger( L, 2 ) ) * DWord( $9E3779B9 ) );
  iValue := ( iValue xor ( iValue shr 16 ) ) * DWord( $85EBCA6B );
  iValue := ( iValue xor ( iValue shr 13 ) ) * DWord( $C2B2AE35 );
  iValue := iValue xor ( iValue shr 16 );
  lua_pushnumber( L, ( iValue mod DWord( 1000000000 ) ) + 1 );
  Result := 1;
end;
{$POP}

constructor TLuaState.Create( aCoverState : PLua_State = nil );
begin
  LoadLua;
  if aCoverState = nil then
  begin
    FState := lua_open;
    luaopen_base(FState);
    luaopen_string(FState);
    luaopen_table(FState);
    luaopen_math(FState);
    FOwner := True;
  end
  else
  begin
    FOwner := False;
    FState := aCoverState;
  end;

  FErrorFunc  := nil;
  lua_getglobal( FState, 'math' );
  lua_pushstring( FState, 'random' );
  lua_pushcfunction( FState, @lua_math_random );
  lua_rawset(FState, -3);
  lua_pushstring( FState, 'mix_seed' );
  lua_pushcfunction(FState, @lua_math_mix_seed );
  lua_rawset(FState, -3);
  lua_pushstring( FState, 'randomseed' );
  lua_pushcfunction( FState, @lua_math_randomseed );
  lua_rawset(FState, -3);
  lua_pop(FState, 1);
end;

procedure TLuaState.LoadFile( const aFileName : AnsiString );
begin
  if luaL_dofile(FState, PChar(aFileName)) <> 0 then
    raise ELuaException.Create(lua_tostring(FState,-1));
end;

procedure TLuaState.LoadStream( aDF: TVDataFile; const aStreamName: AnsiString );
var iStream : TStream;
    iSize   : Int64;
begin
  iStream := aDF.GetFile(aStreamName);
  iSize   := aDF.GetFileSize(aStreamName);
  StreamLoaderDestroy(iStream,aStreamName,iSize);
end;

procedure TLuaState.LoadStream( aDF: TVDataFile; const aDirName, aFileName: AnsiString );
var iStream : TStream;
    iSize   : Int64;
begin
  iStream := aDF.GetFile(aFileName,aDirName);
  iSize   := aDF.GetFileSize(aFileName,aDirName);
  StreamLoaderDestroy(iStream,aFileName,iSize);
end;

procedure TLuaState.StreamLoader( aIST : TStream; aStreamName : AnsiString; aSize : DWord);
var iBuf  : PByte;
begin
  Log('Loading LUA stream -- "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
  GetMem(iBuf,aSize);
  Log('Reading "'+aStreamName+'" ('+IntToStr(aIST.Position)+'-'+IntToStr(aIST.Position+aSize)+')');
  aIST.ReadBuffer(iBuf^,aSize);
  if ( luaL_loadbuffer(FState,PChar(iBuf),aSize,PChar(aStreamName)) <> 0 )
  or ( lua_pcall(FState, 0, 0, 0) <> 0 ) then
  begin
    Error(aStreamName+': '+lua_tostring(FState,-1));
    lua_pop(FState,1);
  end;

  FreeMem(iBuf);
  Log('Loaded "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
end;

procedure TLuaState.StreamLoaderDestroy( aIST: TStream; aStreamName: AnsiString; aSize: DWord );
var iBuf : PByte;
begin
  Log('Loading LUA stream -- "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
  GetMem(iBuf,aSize);
  Log('Reading "'+aStreamName+'" ('+IntToStr(aIST.Position)+'-'+IntToStr(aIST.Position+aSize)+')');
  aIST.ReadBuffer(iBuf^,aSize);
  FreeAndNil(aIST);
  if ( luaL_loadbuffer(FState,PChar(iBuf),aSize,PChar(aStreamName)) <> 0 )
  or ( lua_pcall(FState, 0, 0, 0) <> 0 ) then
  begin
    Error(aStreamName+': '+lua_tostring(FState,-1));
    lua_pop(FState,1);
  end;

  FreeMem(iBuf);
  Log('Loaded "'+aStreamName+'" ('+IntToStr(aSize)+'b)');
end;


procedure TLuaState.Register( const aName : AnsiString; aProc : lua_CFunction);
begin
  lua_register(FState, aName, aProc);
end;

procedure TLuaState.Register( const aKey, aValue: Variant );
begin
  vlua_pushvariant( FState, aKey );
  vlua_pushvariant( FState, aValue );
  lua_rawset_global( FState );
end;

procedure TLuaState.Error( const aErrorString: Ansistring );
begin
  if Assigned( FErrorFunc ) then
    FErrorFunc( aErrorString )
  else
    Log('LuaError: '+aErrorString);
end;

destructor TLuaState.Destroy;
begin
  if FOwner then
  begin
    lua_close(FState);
    Log('Lua closed.');
  end;
  inherited Destroy;
end;

end.
