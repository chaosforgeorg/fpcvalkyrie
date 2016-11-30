{$INCLUDE valkyrie.inc}
unit vluatype;
interface
uses Classes, SysUtils, vlualibrary;

type ILuaType = interface['vluatype.iluatype']
  procedure Push( L : PLua_state );
end;

type TLuaType = class( ILuaType )
  procedure Push( L : PLua_state ); virtual; abstract;
end;

type ILuaReferencedObject = interface['vluastate.luareferencedobject']
    function GetLuaIndex      : Integer;
    function GetID            : AnsiString;
    function GetProtoTable    : AnsiString;
    function HasVolatileHooks : Boolean;
end;

procedure vlua_pushobject( L : Plua_State; aObject : TObject );
procedure vlua_pushanyobject( L : Plua_State; aObject : TObject );
procedure vlua_pusharray( L : Plua_State; const Args: array of const);
function vlua_isobject( L: Plua_State; Index : Integer ) : Boolean;
function vlua_toobject( L : Plua_State; Index : Integer ) : TObject;

implementation

uses vluaext;

procedure vlua_pusharray( L : Plua_State; const Args: array of const);
var NArgs, i : LongInt;
begin
  NArgs := High(Args);
  if NArgs >= 0 then
  for i:=0 to NArgs do
  case Args[i].vtype of
    vtObject     :
    try
      vlua_pushobject( L, Args[i].vObject );
    except
      lua_pop( L, i+1 );
      raise
    end
    else
      vlua_pushvarrec( L, @Args[i].vtype);
  end;
end;


procedure vlua_pushobject ( L : Plua_State; aObject : TObject ) ;
begin
  if aObject = nil then
     lua_pushnil(L)
  else
  if Supports( aObject, ILuaType ) then
  begin
    (aObject as ILuaType).Push( L );
  end
  else
  if Supports( aObject, ILuaReferencedObject ) then
  begin
    lua_rawgeti( L, LUA_REGISTRYINDEX, (aObject as ILuaReferencedObject).GetLuaIndex );
    if not lua_istable( L, -1 ) then
    begin
      lua_pop( L, 1 );
      raise ELuaException.Create( 'Lua reference parameter not found!' );
    end;
  end
  else ELuaException.Create( 'Unknown class type passed to lua!' );
end;

procedure vlua_pushanyobject ( L : Plua_State; aObject : TObject ) ;
begin
  if aObject = nil then
    lua_pushnil( L )
  else
  if Supports( aObject, ILuaType ) then
    (aObject as ILuaType).Push( L )
  else
  if Supports( aObject, ILuaReferencedObject ) then
    lua_rawgeti( L, LUA_REGISTRYINDEX, (aObject as ILuaReferencedObject).GetLuaIndex )
  else
    lua_pushnil( L );
end;

function vlua_isobject ( L : Plua_State; Index : Integer ) : Boolean;
begin
  if not lua_istable( L , Index ) then Exit( False );
  Index := lua_absindex( L, Index );
  lua_pushstring( L, '__ptr' );
  lua_rawget( L, Index );
  vlua_isobject := lua_isuserdata( L, -1 );
  lua_pop( L, 1 );
end;

function vlua_toobject ( L : Plua_State; Index : Integer ) : TObject;
begin
  if not lua_istable( L , Index ) then Exit( nil );
  Index := lua_absindex( L, Index );
  lua_pushstring( L, '__ptr' );
  lua_rawget( L, Index );
  if not lua_isuserdata( L, -1 ) then
  begin
    lua_pop( L, 1 );
    Exit( nil );
  end;
  vlua_toobject := TObject( lua_touserdata( L, -1 ) );
  lua_pop( L, 1 );
end;

end.

