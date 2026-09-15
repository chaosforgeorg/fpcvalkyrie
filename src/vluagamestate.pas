{$INCLUDE valkyrie.inc}
// @abstract(LuaGameState class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
unit vluagamestate;
interface

uses Classes, SysUtils, vluastate, vrltools, vutil, vluaentitynode, vluasystem;

type TLuaGameState = object( TLuaState )
public
  function ToPosition( aIndex : Integer ) : TCoord2D;
  function ToNode( aIndex : Integer ) : TLuaEntityNode;
  function ToID( aLuaSystem : TLuaSystem; aIndex : Integer ) : DWord;
  function ToCellSet( aLuaSystem : TLuaSystem; aIndex : Integer ) : TFlags;
end;

implementation

uses vlualibrary;

{ TLuaGameState }

function TLuaGameState.ToPosition ( aIndex : Integer ) : TCoord2D;
var iObject : TObject;
begin
  if IsCoord( aIndex ) then Exit( ToCoord( aIndex ) );
  iObject := ToObject( aIndex );
  if iObject is TLuaEntityNode then Exit( TLuaEntityNode(iObject).Position );
  Error( 'Position expected at index '+IntToStr(aIndex)+'!' );
end;

function TLuaGameState.ToNode( aIndex : Integer ) : TLuaEntityNode;
var iObject : TObject;
begin
  iObject := ToObject( aIndex );
  if iObject is TLuaEntityNode then Exit( TLuaEntityNode(iObject) );
  Error( 'Node expected at index '+IntToStr(aIndex)+'!' );
end;

function TLuaGameState.ToID( aLuaSystem : TLuaSystem; aIndex : Integer ) : DWord;
var iValue : Integer;
begin
  if isNumber( aIndex ) then Exit( ToInteger( aIndex ) );
  if isString( aIndex ) then
  begin
    iValue := aLuaSystem.Defines.Get( ToString( aIndex ), -1 );
    if iValue >= 0 then Exit( DWord( iValue ) );
    Error('Unknown ID ("'+ToString( aIndex )+'") at index '+ToString( aIndex ) +'!');
  end;
  Error('ID expected at index '+IntToStr( aIndex ) +'!');
end;

function TLuaGameState.ToCellSet( aLuaSystem : TLuaSystem; aIndex : Integer ) : TFlags;
begin
  ToCellSet := [];

  case lua_type( FState, aIndex ) of
    LUA_TTABLE :
    begin
      lua_pushnil( FState );
      while lua_next( FState, aIndex ) <> 0 do
      begin
        Include( ToCellSet, ToID( aLuaSystem, -1 ) );
        lua_pop( FState, 1 );
      end;
    end;
    LUA_TSTRING : Include( ToCellSet, ToID( aLuaSystem, aIndex ) );
    LUA_TNUMBER : Include( ToCellSet, lua_tointeger( FState, aIndex ) );
  end;
end;
end.

