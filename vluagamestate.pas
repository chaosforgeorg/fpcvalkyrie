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

uses Classes, SysUtils, vluastate, vrltools;

type TLuaGameState = object( TLuaState )
  function ToPosition( Index : Integer ) : TCoord2D;
  function ToID( Index : Integer ) : DWord;
end;

implementation

uses vluasystem, vluaentitynode;

{ TLuaGameState }

function TLuaGameState.ToPosition ( Index : Integer ) : TCoord2D;
begin
  if IsObject( Index ) then
    with ToObject( Index ) as TLuaEntityNode do
      Exit( Position );
  if IsCoord( Index ) then Exit( ToCoord( Index ) );
  Error('Position expected!');
end;

function TLuaGameState.ToID ( Index : Integer ) : DWord;
begin
  if isNumber( Index ) then Exit( ToInteger( Index ) );
  if isString( Index ) then Exit( LuaSystem.Defines[ ToString( Index ) ] );
  Error('ID expected!');
end;

end.

