{$INCLUDE valkyrie.inc}
// @abstract(LuaGameState class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @cvs($Author: chaos-dev $)
//
//  @html <div class="license">
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or (at your
//  option) any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
//  for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this library; if not, write to the Free Software Foundation,
//  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//  @html </div>
unit vluagamestate;
interface

uses Classes, SysUtils, vluastate, vrltools, vutil;

type TLuaGameState = object( TLuaState )
  function ToPosition( aIndex : Integer ) : TCoord2D;
  function ToID( aIndex : Integer ) : DWord;
  function ToCellSet( aIndex : Integer ) : TFlags;
end;

implementation

uses vlualibrary, vluasystem, vluaentitynode;

{ TLuaGameState }

function TLuaGameState.ToPosition ( aIndex : Integer ) : TCoord2D;
var iObject : TObject;
begin
  if IsCoord( aIndex ) then Exit( ToCoord( aIndex ) );
  iObject := ToObject( aIndex );
  if iObject is TLuaEntityNode then Exit( TLuaEntityNode(iObject).Position );
  Error( 'Position expected at index '+IntToStr(aIndex)+'!' );
end;

function TLuaGameState.ToID ( aIndex : Integer ) : DWord;
var iValue : Integer;
begin
  if isNumber( aIndex ) then Exit( ToInteger( aIndex ) );
  if isString( aIndex ) then
  begin
    iValue := LuaSystem.Defines.Get( ToString( aIndex ), -1 );
    if iValue >= 0 then Exit( DWord( iValue ) );
    Error('Unknown ID ("'+ToString( aIndex )+'") at index '+ToString( aIndex ) +'!');
  end;
  Error('ID expected at index '+IntToStr( aIndex ) +'!');
end;

function TLuaGameState.ToCellSet ( aIndex : Integer ) : TFlags;
begin
  ToCellSet := [];

  case lua_type( FState, aIndex ) of
    LUA_TTABLE :
    begin
      lua_pushnil( FState );
      while lua_next( FState, aIndex ) <> 0 do
      begin
        Include( ToCellSet, ToID( -1 ) );
        lua_pop( FState, 1 );
      end;
    end;
    LUA_TSTRING : Include( ToCellSet, ToID( aIndex ) );
    LUA_TNUMBER : Include( ToCellSet, lua_tointeger( FState, aIndex ) );
  end;
end;
end.

