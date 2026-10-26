{$INCLUDE valkyrie.inc}
unit vluastore;
interface

uses vlua, vstoreinterface;

procedure RegisterStoreLuaAPI( aLua : TLua; aStore : TStoreInterface );

implementation

uses vdebug, vlualibrary, vluastack;

function lua_store_set_achievement( L : PLua_State ) : Integer; cdecl;
var iState : TLuaStack;
    iID    : Ansistring;
    iStore : TStoreInterface;
begin
  iState.Init( L );
  iStore := TStoreInterface( lua_touserdata( L, lua_upvalueindex( 1 ) ) );
  iID := iState.ToString( 1 );
  if iStore.SetAchievement( iID ) then
    Log( 'lua: store.set_achievement(' + iID + ') succeeded!' );
  Result := 0;
end;

function lua_store_inc_stat( L : PLua_State ) : Integer; cdecl;
var iState : TLuaStack;
    iID    : Ansistring;
    iStore : TStoreInterface;
begin
  iState.Init( L );
  iStore := TStoreInterface( lua_touserdata( L, lua_upvalueindex( 1 ) ) );
  iID := iState.ToString( 1 );
  if iStore.IncStat( iID ) then
    Log( 'lua: store.inc_stat(' + iID + ') succeeded!' );
  Result := 0;
end;

function lua_store_mark_stat( L : PLua_State ) : Integer; cdecl;
var iState : TLuaStack;
    iID    : Ansistring;
    iStore : TStoreInterface;
begin
  iState.Init( L );
  iStore := TStoreInterface( lua_touserdata( L, lua_upvalueindex( 1 ) ) );
  iID := iState.ToString( 1 );
  if iStore.MarkStat( iID ) then
    Log( 'lua: store.mark_stat(' + iID + ') succeeded!' );
  Result := 0;
end;

const lua_store_lib : array[0..3] of luaL_Reg = (
  ( name : 'set_achievement'; func : @lua_store_set_achievement ),
  ( name : 'inc_stat';        func : @lua_store_inc_stat ),
  ( name : 'mark_stat';       func : @lua_store_mark_stat ),
  ( name : nil;              func : nil )
);

procedure RegisterStoreLuaAPI( aLua : TLua; aStore : TStoreInterface );
begin
  // The store is borrowed and must outlive the Lua state.
  lua_pushlightuserdata( aLua.Raw, aStore );
  aLua.Register( 'store', lua_store_lib, 1 );
end;

end.
