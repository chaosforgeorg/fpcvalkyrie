{$INCLUDE valkyrie.inc}
// @abstract(LuaGameState class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
unit vluagamestack;
interface

uses classes, sysutils, vluastack, vrltools, vutil, vluaentitynode, vlua, viotypes;

type TLuaGameStack = object( TLuaStack )
public
  function ToPosition( aIndex : Integer ) : TCoord2D; overload;
  function ToPosition( aIndex : Integer; aDefault : TCoord2D ) : TCoord2D; overload;
  function ToNode( aIndex : Integer ) : TLuaEntityNode;
  function ToID( aLua : TLua; aIndex : Integer ) : Integer;
  function ToIOColor( aIndex : Integer ) : TIOColor;
  function ToCellSet( aLua : TLua; aIndex : Integer ) : TFlags;
end;

implementation

uses vlualibrary, vvector;

{ TLuaGameStack }

function TLuaGameStack.ToPosition( aIndex : Integer ) : TCoord2D;
var iObject : TObject;
begin
  if IsCoord( aIndex ) then Exit( ToCoord( aIndex ) );
  iObject := ToObject( aIndex );
  if iObject is TLuaEntityNode then Exit( TLuaEntityNode(iObject).Position );
  Error( 'Position expected at index '+IntToStr(aIndex)+'!' );
end;

function TLuaGameStack.ToPosition( aIndex : Integer; aDefault : TCoord2D ) : TCoord2D;
begin
  if IsCoord( aIndex ) then Exit( ToCoord( aIndex ) );
  if IsObject( aIndex ) then Exit( ToNode( aIndex ).Position );
  Exit( aDefault );
end;

function TLuaGameStack.ToNode( aIndex : Integer ) : TLuaEntityNode;
var iObject : TObject;
begin
  iObject := ToObject( aIndex );
  if iObject is TLuaEntityNode then Exit( TLuaEntityNode(iObject) );
  Error( 'Node expected at index '+IntToStr(aIndex)+'!' );
end;

function TLuaGameStack.ToID( aLua : TLua; aIndex : Integer ) : Integer;
var iValue : Integer;
begin
  if isNumber( aIndex ) then Exit( ToInteger( aIndex ) );
  if isString( aIndex ) then
  begin
    iValue := aLua.Defines.Get( ToString( aIndex ), -1 );
    if iValue >= 0 then Exit( iValue );
    Error('Unknown ID ("'+ToString( aIndex )+'") at index '+IntToStr( aIndex ) +'!');
  end;
  Error('ID expected at index '+IntToStr( aIndex ) +'!');
end;

function TLuaGameStack.ToIOColor( aIndex : Integer ) : TIOColor;
var iC4b : TVec4b;
begin
  Result := 0;
  if IsNumber( aIndex )
    then Exit( ToInteger( aIndex ) )
    else if IsTable( aIndex ) then
    begin
      iC4b := ToVec4b( aIndex );
      Exit( IOColor( iC4b.X, iC4b.Y, iC4b.Z, iC4b.W ) );
    end;
end;

function TLuaGameStack.ToCellSet( aLua : TLua; aIndex : Integer ) : TFlags;
begin
  ToCellSet := [];

  case lua_type( FState, aIndex ) of
    LUA_TTABLE :
    begin
      lua_pushnil( FState );
      while lua_next( FState, aIndex ) <> 0 do
      begin
        Include( ToCellSet, ToID( aLua, -1 ) );
        lua_pop( FState, 1 );
      end;
    end;
    LUA_TSTRING : Include( ToCellSet, ToID( aLua, aIndex ) );
    LUA_TNUMBER : Include( ToCellSet, lua_tointeger( FState, aIndex ) );
  end;
end;
end.
