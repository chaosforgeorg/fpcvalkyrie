{$INCLUDE valkyrie.inc}
// @abstract(LuaEntityNode class for Valkyrie)
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
//
// TODO - unwrap TLuaState calls for efficiency

unit vluaentitynode;
interface
uses Classes, vnode, vutil, vrltools, vluastate, viotypes;

const ENTITY_BEING    = 1;
      ENTITY_ITEM     = 2;

type
TLuaEntityNode = class;

TLuaEntityNodeEnumerator = specialize TGNodeEnumerator< TLuaEntityNode >;

TLuaEntityNode = class( TNode )
  // Standard constructor, zeroes all fields.
  constructor Create( const aID : AnsiString ); reintroduce;
  // Stream constructor, reads UID, and ID from stream, should be overriden.
  constructor CreateFromStream( Stream : TStream ); override;
  // Write Node to stream (UID and ID) should be overriden.
  procedure WriteToStream( Stream : TStream ); override;
  // Position change - calls TLuaMapNode.Displace
  procedure Displace( const aWhere : TCoord2D ); virtual;
  // Returns true if the thing is in the vision field of the player - calls TLuaMapNode.isVisible
  function isVisible : boolean;
  // Return enumerator
  function GetEnumerator : TNodeEnumerator;
  // Function for getting custom properties in Lua.
  // Should push given property to the passed state
  function GetProperty( L : PLua_State; const aPropertyName : AnsiString ) : Integer; override;
  // Register API -- WARNING - registers TableName metatable!
  class procedure RegisterLuaAPI( const TableName : AnsiString );
protected
  // Position
  FPosition  : TCoord2D;
  // Entity ID (use 1 for NPC, 2 for Item, 3+ for custom)
  FEntityID  : Byte;
  // ASCII Gylph
  FGylph     : TIOGylph;
public
  // Property for position
  property Position  : TCoord2D read FPosition write FPosition;
  //
  property EntityID  : Byte     read FEntityID;
  //
  property Gylph     : TIOGylph read FGylph    write FGylph;
published
  property id       : AnsiString read FID;
  property uid      : TUID       read FUID;
  property name     : AnsiString read FName        write FName;
  property picture  : Char       read FGylph.ASCII write FGylph.ASCII;
  property color    : DWord      read FGylph.Color write FGylph.Color;
  property x        : Integer    read FPosition.x;
  property y        : Integer    read FPosition.y;
end;

function lua_entity_node_in_range_closure(L: Plua_State): Integer; cdecl;

implementation
uses vluasystem, vluamapnode, vluatools, vluatype, vlualibrary;

{ TLuaEntityNode }

constructor TLuaEntityNode.Create ( const aID : AnsiString ) ;
begin
  inherited Create( aID, True );
  FPosition.Create(0,0);
  FEntityID := 0;
  FGylph.Color := 7;
  FGylph.ASCII := '?';
end;

constructor TLuaEntityNode.CreateFromStream ( Stream : TStream ) ;
begin
  inherited CreateFromStream ( Stream ) ;
  FEntityID := Stream.ReadByte;
  Stream.Read( FPosition, SizeOf(FPosition) );
  Stream.Read( FGylph,    SizeOf(FGylph) );
end;

procedure TLuaEntityNode.WriteToStream ( Stream : TStream ) ;
begin
  inherited WriteToStream ( Stream ) ;
  Stream.WriteByte( FEntityID );
  Stream.Write( FPosition, SizeOf(FPosition) );
  Stream.Write( FGylph,    SizeOf(FGylph) );
end;

procedure TLuaEntityNode.Displace ( const aWhere : TCoord2D ) ;
begin
  if Parent <> nil then
    TLuaMapNode(Parent).Displace( Self, aWhere );
end;

function TLuaEntityNode.isVisible : boolean;
begin
  if Parent <> nil
    then Exit( TLuaMapNode(Parent).isVisible( FPosition ) )
    else Exit( False );
end;

function TLuaEntityNode.GetEnumerator : TNodeEnumerator;
begin
  GetEnumerator.Create( Self );
end;

function TLuaEntityNode.GetProperty ( L : PLua_State;
  const aPropertyName : AnsiString ) : Integer;
var iState : TLuaState;
begin
  iState.Init(L);
  if aPropertyName = 'position' then begin iState.PushCoord( FPosition ); Exit( 1 ); end;
  if aPropertyName = 'parent'   then begin iState.Push( Parent as TNode ); Exit( 1 ); end;
  Exit( 0 );
end;

function lua_entity_node_distance_to(L: Plua_State) : Integer; cdecl;
var State : TLuaState;
    GNode : TLuaEntityNode;
begin
  State.Init(L);
  GNode := State.ToObject(1) as TLuaEntityNode;
  if State.IsCoord(2) then
    State.Push( Distance( GNode.FPosition, State.ToCoord(2) ) )
  else
    State.Push( Distance( GNode.FPosition, (State.ToObject(2) as TLuaEntityNode).FPosition ) );
  Result := 1;
end;

function lua_entity_node_displace(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    GNode : TLuaEntityNode;
begin
  State.Init(L);
  GNode := State.ToObject(1) as TLuaEntityNode;
  GNode.Displace( State.ToCoord( 2 ) );
  Result := 0;
end;

function lua_entity_node_position(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    GNode : TLuaEntityNode;
begin
  State.Init(L);
  GNode := State.ToObject(1) as TLuaEntityNode;
  State.PushCoord( GNode.FPosition );
  Result := 1;
end;

function lua_entity_node_in_range_closure(L: Plua_State): Integer; cdecl;
var Coord    : TCoord2D;
    This     : TNode;
    Range    : Word;
    Filter   : AnsiString;
    FilterID : DWord;
    Next     : TNode;
    Current  : TLuaEntityNode;
    Parent   : TNode;
begin
  Parent  := TObject( lua_touserdata( L, lua_upvalueindex(1) ) ) as TNode;
  This    := TObject( lua_touserdata( L, lua_upvalueindex(2) ) ) as TNode;
  Coord   := vlua_tocoord( L, lua_upvalueindex(3) );
  Range   := lua_tointeger( L, lua_upvalueindex(4) );
  Filter  := '';
  FilterID:= 0;
  case lua_type( L, lua_upvalueindex(5) ) of
    LUA_TNUMBER : FilterID := lua_tointeger( L, lua_upvalueindex(5) );
    LUA_TSTRING : Filter   := lua_tostring( L, lua_upvalueindex(5) );
  end;
  Next   := TObject( lua_touserdata( L, lua_upvalueindex(6) ) ) as TNode;

  repeat
    Current := Next as TLuaEntityNode;
    if Next <> nil then Next := Next.Next;
    if Next = Parent.Child then Next := nil;
  until (Current = nil) or ( ((Filter   = '') or (Current.GetProtoName = Filter)) and
                             ((FilterID = 0)  or (Current.EntityID = FilterID)) and
                             ( Distance( Current.FPosition, Coord ) <= Range )  and
                             (This <> Current) );

  lua_pushlightuserdata( L, Next );
  lua_replace( L, lua_upvalueindex(6) );

  vlua_pushobject( L, Current );
  Exit(1);
end;

function lua_entity_node_siblings_in_range(L: Plua_State): Integer; cdecl;
var State   : TLuaState;
    LuaNode : TLuaEntityNode;
begin
  State.Init(L);
  LuaNode := State.ToObject(1) as TLuaEntityNode;
  lua_pushlightuserdata( L, LuaNode.Parent );
  lua_pushlightuserdata( L, LuaNode );
  vlua_pushcoord( L, LuaNode.FPosition );
  lua_pushvalue( L, 2 );
  if (lua_type( L, 3 ) = LUA_TSTRING) or (lua_type( L, 3 ) = LUA_TNUMBER)
    then lua_pushvalue( L, 3 )
    else lua_pushinteger( L, 0 );
  lua_pushlightuserdata( L, LuaNode.Child );
  lua_pushcclosure( L, @lua_entity_node_in_range_closure, 6 );
  Exit( 1 );
end;

function lua_entity_node_is_visible(L: Plua_State) : Integer; cdecl;
var State   : TLuaState;
    LuaNode : TLuaEntityNode;
begin
  State.Init(L);
  LuaNode := State.ToObject(1) as TLuaEntityNode;
  State.Push( LuaNode.isVisible );
  Result := 1;
end;

function lua_entity_node_is_being(L: Plua_State): Integer; cdecl;
var State   : TLuaState;
    LuaNode : TLuaEntityNode;
begin
  State.Init(L);
  LuaNode := State.ToObject(1) as TLuaEntityNode;
  State.Push( LuaNode.EntityID = ENTITY_BEING );
  Result := 1;
end;

function lua_entity_node_is_item(L: Plua_State): Integer; cdecl;
var State   : TLuaState;
    LuaNode : TLuaEntityNode;
begin
  State.Init(L);
  LuaNode := State.ToObject(1) as TLuaEntityNode;
  State.Push( LuaNode.EntityID = ENTITY_ITEM );
  Result := 1;
end;


const lua_entity_node_lib : array[0..6] of luaL_Reg = (
  ( name : 'distance_to';       func : @lua_entity_node_distance_to),
  ( name : 'displace';          func : @lua_entity_node_displace),
  ( name : 'siblings_in_range'; func : @lua_entity_node_siblings_in_range),
  ( name : 'is_visible';        func : @lua_entity_node_is_visible),
  ( name : 'is_being';          func : @lua_entity_node_is_being),
  ( name : 'is_item';           func : @lua_entity_node_is_item),
  ( name : nil;                 func : nil; )
);


class procedure TLuaEntityNode.RegisterLuaAPI ( const TableName : AnsiString );
begin
  LuaSystem.Register( TableName, lua_entity_node_lib );
end;

end.

