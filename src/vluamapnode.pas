{$INCLUDE valkyrie.inc}
// @abstract(LuaMapNode class for Valkyrie)
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

unit vluamapnode;
interface
uses SysUtils, Classes,
     vnode, vutil, vmaparea, vdungen, vvision, vrltools, vluaentitynode;

const vlfExplored     = 0;
      vlfVisible      = 1;
      vlfLighted      = 2;

const EF_NOBLOCK      = 0;
      EF_NOBEINGS     = 1;
      EF_NOITEMS      = 2;

{$PACKSET 2}
type TWordSet = set of 0..15;
type TMapCell = record
  Being : TLuaEntityNode;
  Item  : TLuaEntityNode;
  Light : TWordSet;
  Cell  : Byte;
  HP    : Byte;
end;
{$PACKSET DEFAULT}

type PMapCell = ^TMapCell;

type TLuaMapNode = class( TNode, IMapArea, ICellIDSource, IVisionQuery )
  // Create and setup
  constructor Create( const aID : AnsiString; aMaxX, aMaxY : DWord; aMaxVision : Byte );
  // Removes given bits from lightMap
  procedure ClearLightMapBits( aValue : TWordSet );
  // Fills the light map with given value
  procedure ClearLightMap( aValue : TWordSet = [] );
  // Fills the map with given value
  procedure ClearMap( aValue : Byte = 0 );
  // Clears the entities
  procedure ClearEntities;
  // Clears all values on the map
  procedure ClearAll( aMapValue : Byte = 0; aLightValue : TWordSet = []; aHPValue : Byte = 0 ); virtual;
  // Perform a FOV calculation
  procedure PrepareVision( const aSource : TCoord2D; aRange : Byte ); virtual;
  // Perform a Light calculation based on FOV
  procedure PrepareLight ( const aSource : TCoord2D; aVisionRange : Byte; aLightRange : Byte = 0 ); virtual;
  // Run the whole vision algorithm on defaults
  procedure RunVision( const aSource : TCoord2D; aVisionRange : Byte; aLightRange : Byte = 0 ); virtual;
  // Checks if cell is empty
  function isEmpty( const coord : TCoord2D; EmptyFlags : TFlags32 = []) : Boolean; virtual;
  // string to numeric ID conversion
  function IDtoCell( const aID : AnsiString ) : Byte; virtual;
  // numeric to string ID conversion
  function CellToID( const aCell : Byte ) : AnsiString; virtual;
  // function needed for the vvision interface
  function blocksVision( const Coord : TCoord2D ) : boolean; virtual; abstract;
  // Returns wether there is an unobstructed line of sight between the
  // points (ax,ay) and (bx,by).
  function isEyeContact( const a, b : TCoord2D ) : boolean;
  // Returns true if a coord is visible, false otherwise
  function isVisible( const aCoord : TCoord2D ) : boolean; virtual;
  // Returns true if cell is explored
  function isExplored( const aCoord : TCoord2D ) : boolean;
  // Returns true if a coord is in the map bounds (1..MAP_MAXX,1..MAP_MAXY),
  // returns false otherwise.
  function isProperCoord( const Coord : TCoord2D ) : boolean;
  // Checks if cell is blocked physically (map)
  function isPassable( const coord : TCoord2D ) : Boolean; virtual; abstract;
  // Procedure setting cell at given coord.
  procedure putCell( const aCoord : TCoord2D; const aValue : Byte ); virtual;
  // Function returning cell at given coord.
  function getCell( const aCoord : TCoord2D ) : Byte; virtual;
  // Procedure setting hp at given coord.
  procedure SetHitPoints( const aCoord : TCoord2D; const aValue : Byte );
  // Function returning hp at given coord.
  function GetHitPoints( const aCoord : TCoord2D ) : Byte;
  // Set Light flag
  procedure SetLightFlag( const aCoord : TCoord2D; aFlag : Byte; aValue : Boolean );
  // Get Light flag
  function GetLightFlag( const aCoord : TCoord2D; aFlag : Byte ) : Boolean;
  // Return Being at position aCoord
  function GetBeing( const aCoord : TCoord2D ) : TLuaEntityNode; virtual;
  // Return Item at position aCoord
  function GetItem( const aCoord : TCoord2D ) : TLuaEntityNode; virtual;
  // Drop something onto the map
  function Drop( aWhat : TLuaEntityNode; aPosition : TCoord2D; aEmptyFlags : TFlags32 = [] ) : TLuaEntityNode;
  // Change position of given thing, no error checking
  procedure Displace( aWhat : TLuaEntityNode; const aWhere : TCoord2D ); reintroduce;
  // Override of remove - automatic map clear
  procedure Remove( aNode : TNode ); override;
  // Free generator and structure
  destructor Destroy; override;
  // Stream constructor, reads UID, and ID from stream, should be overriden.
  constructor CreateFromStream( Stream : TStream ); override;
  // Write Node to stream (UID and ID) should be overriden.
  procedure WriteToStream( Stream : TStream ); override;
  // Register API
  class procedure RegisterLuaAPI( const aTableName : AnsiString );
protected
  // Abstract function for child creation from stream
  // Being = 1, Item = 2
  function EntityFromStream( aStream : TStream; aEntityID : Byte ) : TLuaEntityNode; virtual; abstract;
  // Set Being at position aCoord
  procedure SetBeing( const aCoord : TCoord2D; aBeing : TLuaEntityNode ); virtual;
  // Set Item at position aCoord
  procedure SetItem( const aCoord : TCoord2D; aItem : TLuaEntityNode ); virtual;
protected
  // Area holding the level boundaries.
  FArea      : TArea;
  // Level generator.
  FGenerator : TDungeonBuilder;
  // MapArea class
  FMapArea   : TMapArea;
  // MapArea class
  FVision    : TVision;
  // Max vision distance
  FMaxVision : Byte;
  // Cell map
  FCellMap   : PMapCell;
  // Cell array name
  FCellsName : AnsiString;
public
  // Property for Area
  property Area      : TArea           read FArea;
  // Property for Map Area
  property MapArea   : TMapArea        read FMapArea;
  // Property for Generator
  property Generator : TDungeonBuilder read FGenerator;
  // Property for Vison
  property Vision    : TVision         read FVision;
  // Property for Cell ID access.
  property Cell[ const aIndex : TCoord2D ] : Byte read getCell write putCell; default;
  // Property for Cell ID access.
  property HitPoints[ const aIndex : TCoord2D ] : Byte read getHitPoints write setHitPoints;
public
  property LightFlag[ const aCoord : TCoord2D; aFlag : Byte ] : Boolean read GetLightFlag write SetLightFlag;
end;

implementation

uses vluasystem, vmath, vluastate, vluagamestate, vluatools, vluatype, vlualibrary, math;

{ TLuaMapNode }

constructor TLuaMapNode.Create ( const aID : AnsiString; aMaxX, aMaxY : DWord; aMaxVision : Byte ) ;
begin
  inherited Create( aID, True );
  FArea.Create( NewCoord2D( 1, 1 ), NewCoord2D( aMaxX, aMaxY ) );
  FMapArea   := TMapArea.Create( FArea, Self );
  FGenerator := TDungeonBuilder.Create( aMaxX, aMaxY, Self, Self );
  FVision    := TIsaacVision.Create( Self, aMaxVision );
  FMaxVision := aMaxVision;
  FCellMap   := GetMem( aMaxX * aMaxY * SizeOf( TMapCell ) );
  FCellsName := 'cells';
  ClearAll;
end;

procedure TLuaMapNode.ClearLightMapBits ( aValue : TWordSet ) ;
var i : DWord;
begin
  for i := 0 to FArea.B.X * FArea.B.Y - 1 do
    FCellMap[ i ].Light := FCellMap[ i ].Light - aValue;
end;

procedure TLuaMapNode.ClearLightMap ( aValue : TWordSet ) ;
var i : DWord;
begin
  for i := 0 to FArea.B.X * FArea.B.Y - 1 do
    FCellMap[ i ].Light := aValue;
end;

procedure TLuaMapNode.ClearMap ( aValue : Byte ) ;
var i : DWord;
begin
  for i := 0 to FArea.B.X * FArea.B.Y - 1 do
    FCellMap[ i ].Cell := aValue;
end;

procedure TLuaMapNode.ClearEntities;
var i : DWord;
begin
  for i := 0 to FArea.B.X * FArea.B.Y - 1 do
  begin
    FCellMap[ i ].Being := nil;
    FCellMap[ i ].Item  := nil;
  end;
end;

procedure TLuaMapNode.ClearAll( aMapValue : Byte = 0; aLightValue : TWordSet = []; aHPValue : Byte = 0 );
var i : DWord;
begin
  ClearEntities;
  for i := 0 to FArea.B.X * FArea.B.Y - 1 do
  begin
    FCellMap[ i ].Cell  := aMapValue;
    FCellMap[ i ].Light := aLightValue;
    FCellMap[ i ].HP    := aHPValue;
  end;
end;

procedure TLuaMapNode.PrepareVision ( const aSource : TCoord2D; aRange : Byte ) ;
begin
  FVision.Run( aSource, aRange );
end;

procedure TLuaMapNode.PrepareLight ( const aSource : TCoord2D; aVisionRange : Byte; aLightRange : Byte = 0 ) ;
var ax,ay : Integer;
    index : DWord;
begin
  if aLightRange = 0 then aLightRange := aVisionRange;
  for ax := aSource.x-aVisionRange to aSource.x+aVisionRange do
   for ay := aSource.y-aVisionRange to aSource.y+aVisionRange do
    if isProperCoord( NewCoord2D(ax,ay) ) then
      if FVision.getLight( NewCoord2D(ax,ay) ) > 0 then
      begin
        index := ( ay - 1 ) * FArea.B.X + ( ax - 1 );
        Include( FCellMap[index].Light,vlfVisible );
        if Distance(ax,ay,aSource.x,aSource.y) <= aLightRange then
        begin
           Include( FCellMap[index].Light,vlfLighted );
           Include( FCellMap[index].Light,vlfExplored );
        end;
      end;
end;

procedure TLuaMapNode.RunVision ( const aSource : TCoord2D; aVisionRange : Byte; aLightRange : Byte = 0 ) ;
begin
  if aLightRange = 0 then aLightRange := aVisionRange;
  ClearLightMapBits( [ vlfVisible, vlfLighted ] );
  PrepareVision( aSource, aVisionRange );
  PrepareLight( aSource, aLightRange);
end;

function TLuaMapNode.isEmpty ( const coord : TCoord2D; EmptyFlags : TFlags32
  ) : Boolean;
begin
  if not isProperCoord( coord ) then Exit( False );
  if (EF_NOBLOCK  in EmptyFlags) and (not isPassable( coord ))  then Exit( False );
  if (EF_NOITEMS  in EmptyFlags) and (GetItem( coord ) <> nil)  then Exit( False );
  if (EF_NOBEINGS in EmptyFlags) and (GetBeing( coord ) <> nil) then Exit( False );
  Exit( True );
end;

function TLuaMapNode.IDtoCell ( const aID : AnsiString ) : Byte;
begin
  Exit( LuaSystem.Defines[ aID ] );
end;

function TLuaMapNode.CellToID( const aCell : Byte ) : AnsiString;
begin
  Exit( LuaSystem.Get([FCellsName,aCell,'id']) );
end;

function TLuaMapNode.isEyeContact( const a,b : TCoord2D ) : boolean;
var iEyeRay : TBresenhamRay;
    iCount  : byte;
    iDist   : Word;
begin
  if a = b then Exit( true );
  iDist := Max( Abs(a.x-b.x), Abs(a.y-b.y) );

  iEyeRay.Init( a, b );
  iCount := 0;
  repeat
    Inc( iCount );
    iEyeRay.Next;
    if iEyeRay.Done then Exit( True );
    if blocksVision( iEyeRay.GetC ) then Exit( False );
  until iCount > iDist+1;
  Exit( False );
end;

function TLuaMapNode.isVisible ( const aCoord : TCoord2D ) : boolean;
var iIndex : DWord;
begin
  iIndex := ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 );
  Result := [vlfVisible,vlfLighted] * FCellMap[iIndex].Light = [vlfVisible,vlfLighted];
end;

function TLuaMapNode.isExplored ( const aCoord : TCoord2D ) : boolean;
begin
  Result := vlfExplored in FCellMap[( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 )].Light;
end;

function TLuaMapNode.isProperCoord( const Coord : TCoord2D ) : boolean;
begin
  Exit( FArea.Contains( Coord ) );
end;

procedure TLuaMapNode.putCell ( const aCoord : TCoord2D; const aValue : Byte ) ;
begin
  FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].Cell := aValue;
end;

function TLuaMapNode.getCell ( const aCoord : TCoord2D ) : Byte;
begin
  Exit( FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].Cell );
end;

procedure TLuaMapNode.SetHitPoints( const aCoord : TCoord2D; const aValue : Byte );
begin
  FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].HP := aValue;
end;

function TLuaMapNode.GetHitPoints( const aCoord : TCoord2D ) : Byte;
begin
  Exit( FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].HP );
end;

procedure TLuaMapNode.SetLightFlag ( const aCoord : TCoord2D; aFlag : Byte; aValue : Boolean );
var iIndex : DWord;
begin
  iIndex := ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 );
  if aValue
    then Include( FCellMap[ iIndex ].Light, aFlag )
    else Exclude( FCellMap[ iIndex ].Light, aFlag );
end;

function TLuaMapNode.GetLightFlag ( const aCoord : TCoord2D; aFlag : Byte ) : Boolean;
begin
  Exit( aFlag in FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].Light );
end;

procedure TLuaMapNode.SetBeing ( const aCoord : TCoord2D; aBeing : TLuaEntityNode ) ;
begin
  FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].Being := aBeing;
end;

function TLuaMapNode.GetBeing ( const aCoord : TCoord2D ) : TLuaEntityNode;
begin
  Exit( FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].Being );
end;

procedure TLuaMapNode.SetItem ( const aCoord : TCoord2D; aItem : TLuaEntityNode ) ;
begin
  FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].Item := aItem;
end;

function TLuaMapNode.GetItem ( const aCoord : TCoord2D ) : TLuaEntityNode;
begin
  Exit( FCellMap[ ( aCoord.y - 1 ) * FArea.B.X + ( aCoord.x - 1 ) ].Item );
end;

function TLuaMapNode.Drop ( aWhat : TLuaEntityNode; aPosition : TCoord2D; aEmptyFlags : TFlags32 ) : TLuaEntityNode;
begin
  if aWhat = nil then Exit( nil );
  case aWhat.EntityID of
    ENTITY_BEING : if aEmptyFlags = [] then aEmptyFlags := [ EF_NOBLOCK, EF_NOBEINGS ];
    ENTITY_ITEM  : if aEmptyFlags = [] then aEmptyFlags := [ EF_NOBLOCK, EF_NOITEMS ];
    else Exit( nil );
  end;

  try
    aPosition := FMapArea.Drop( aPosition, aEmptyFlags );
    if aWhat.Parent <> Self then Add( aWhat );
    aWhat.Displace( aPosition );
    case aWhat.EntityID of
      ENTITY_BEING : SetBeing( aPosition, aWhat );
      ENTITY_ITEM  : SetItem( aPosition, aWhat );
    end;
  except on Exception do
    FreeAndNil( aWhat );
  end;
  Result := aWhat;
end;


procedure TLuaMapNode.Displace ( aWhat : TLuaEntityNode; const aWhere : TCoord2D ) ;
begin
  if aWhat = nil then Exit;
  if aWhat.EntityID = ENTITY_BEING  then
  begin
    if GetBeing(aWhat.Position) = aWhat then SetBeing( aWhat.Position, nil );
    aWhat.Position := aWhere;
    SetBeing( aWhat.Position, aWhat );
  end else
  if aWhat.EntityID = ENTITY_ITEM then
  begin
    if GetItem(aWhat.Position) = aWhat then SetItem( aWhat.Position, nil );
    aWhat.Position := aWhere;
    SetItem( aWhat.Position, aWhat );
  end;
end;

procedure TLuaMapNode.Remove ( aNode : TNode ) ;
begin
  inherited Remove ( aNode ) ;
  if aNode is TLuaEntityNode then
  with aNode as TLuaEntityNode do
  if isProperCoord(Position) then
  case EntityID of
    ENTITY_BEING : SetBeing( Position, nil );
    ENTITY_ITEM  : SetItem( Position, nil );
  end;
end;


destructor TLuaMapNode.Destroy;
begin
  FreeAndNil( FVision );
  FreeAndNil( FGenerator );
  inherited Destroy;
  FreeMem( FCellMap, FArea.B.X * FArea.B.Y * SizeOf( TMapCell ) );
end;

constructor TLuaMapNode.CreateFromStream ( Stream : TStream ) ;
var iEntityID : DWord;
    iEntity   : TLuaEntityNode;
begin
  inherited CreateFromStream ( Stream ) ;

  Stream.Read( FArea, SizeOf( FArea ) );
  FMaxVision := Stream.ReadByte;
  FCellsName := Stream.ReadAnsiString;
  FMapArea   := TMapArea.Create( FArea, Self );
  FGenerator := TDungeonBuilder.Create( FArea.B.X, FArea.B.Y, Self, Self );
  FVision    := TIsaacVision.Create( Self, FMaxVision );

  FCellMap := GetMem( FArea.B.X * FArea.B.Y * SizeOf( TMapCell ) );
  Stream.Read( FCellMap^, FArea.B.X * FArea.B.Y * SizeOf( TMapCell ) );

  ClearEntities;
  repeat
    iEntityID := Stream.ReadByte;
    if iEntityID <> 0 then
    begin
      iEntity := EntityFromStream( Stream, iEntityID );
      if iEntity = nil then Halt(0);
      if iEntityID = ENTITY_BEING then SetBeing( iEntity.Position, iEntity ) else
      if iEntityID = ENTITY_ITEM  then SetItem( iEntity.Position, iEntity );
      Add( iEntity );
    end;
  until iEntityID = 0;
end;

procedure TLuaMapNode.WriteToStream ( Stream : TStream ) ;
var iNode : TNode;
begin
  inherited WriteToStream ( Stream ) ;

  Stream.Write( FArea, SizeOf( FArea ) );
  Stream.WriteByte( FMaxVision );
  Stream.WriteAnsiString( FCellsName );

  Stream.Write( FCellMap^, FArea.B.X * FArea.B.Y * SizeOf( TMapCell ) );

  for iNode in Self do
    if iNode is TLuaEntityNode then
      with iNode as TLuaEntityNode do
      begin
        Stream.WriteByte( EntityID );
        WriteToStream( Stream );
      end;
  Stream.WriteByte( 0 );
end;

function lua_map_node_set_cell (L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  iNode.PutCell( iState.ToPosition( 2 ), iState.ToID( 3 ) );
  Result := 0;
end;

function lua_map_node_get_cell (L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  iState.Push( iNode.CellToID( iNode.GetCell( iState.ToPosition( 2 ) ) ) );
  Result := 1;
end;

function lua_map_node_set_hp (L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  iNode.SetHitpoints( iState.ToPosition( 2 ), iState.ToInteger( 3 ) );
  Result := 0;
end;

function lua_map_node_get_hp (L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  iState.Push( iNode.GetHitpoints( iState.ToPosition( 2 ) ) );
  Result := 1;
end;

function lua_map_node_raw_set_cell (L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  if iState.IsNumber( 2 )
    then iNode.PutCell( NewCoord2D( iState.ToInteger( 2 ), iState.ToInteger( 3 ) ), iState.ToInteger( 4 ) )
    else iNode.PutCell( iState.ToCoord( 2 ), iState.ToInteger( 3 ) );
  Result := 0;
end;

function lua_map_node_raw_get_cell (L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  if iState.IsNumber( 2 )
    then iState.Push( iNode.GetCell( NewCoord2D( iState.ToInteger( 2 ), iState.ToInteger( 3 ) ) ) )
    else iState.Push( iNode.GetCell( iState.ToCoord( 2 ) ) );
  Result := 1;
end;

function lua_map_node_eye_contact(L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  iState.Push( iNode.isEyeContact( iState.ToPosition( 2 ), iState.ToPosition( 3 ) ) );
  Result := 1;
end;

function lua_map_node_get_area(L: Plua_State): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
begin
  iState.Init(L);
  iNode := iState.ToObject( 1 ) as TLuaMapNode;
  iState.PushArea( iNode.FArea );
  Result := 1;
end;

function lua_map_node_is_empty( L: Plua_State ): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
    iCoord : TCoord2D;
begin
  iState.Init(L);
  iNode  := iState.ToObject( 1 ) as TLuaMapNode;
  iCoord := iState.ToCoord( 2 );
  iState.Push( iNode.isEmpty( iCoord, iState.ToFlags( 2 ) ) );
  Exit( 1 );
end;

function lua_map_node_is_visible( L: Plua_State ): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
    iCoord : TCoord2D;
begin
  iState.Init(L);
  iNode  := iState.ToObject( 1 ) as TLuaMapNode;
  iCoord := iState.ToCoord( 2 );
  iState.Push( iNode.isVisible( iCoord ) );
  Exit( 1 );
end;

function lua_map_node_is_empty_area( L: Plua_State ): Integer; cdecl;
var iState : TLuaGameState;
    iNode  : TLuaMapNode;
    iArea  : TArea;
    iFlags : TFlags32;
    iCoord : TCoord2D;
begin
  iState.Init(L);
  iNode  := iState.ToObject( 1 ) as TLuaMapNode;
  iArea  := iState.ToArea( 2 );
  iFlags := iState.ToFlags( 3 );
  for iCoord in iArea do
    if not iNode.isEmpty( iCoord, iFlags ) then
    begin
      iState.Push( False );
      Exit(1);
    end;
  iState.Push( True );
  Exit(1);
end;

// iterator
function lua_map_node_children_in_range(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  lua_pushlightuserdata( L, Node );
  lua_pushlightuserdata( L, nil );
  if vlua_iscoord( L, 2 )
    then lua_pushvalue( L, 2 )
    else vlua_pushcoord( L, (vlua_toobject( L, 2 ) as TLuaEntityNode).Position );
  lua_pushvalue( L, 3 );
  if (lua_type( L, 4 ) = LUA_TSTRING) or (lua_type( L, 4 ) = LUA_TNUMBER)
    then lua_pushvalue( L, 4 )
    else lua_pushinteger( L, 0 );
  lua_pushlightuserdata( L, Node.Child );
  lua_pushcclosure( L, @lua_entity_node_in_range_closure, 6 );
  Exit( 1 );
end;

function lua_map_node_get_light_flag(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iLevel : TLuaMapNode;
begin
  iState.Init(L);
  iLevel := iState.ToObject(1) as TLuaMapNode;
  if iState.IsNil(3) then
  begin
    lua_pushstring( L, '__coord' );
    lua_rawget( L, 1 );
    iState.Push( iLevel.LightFlag[ iState.ToCoord(-1), iState.ToInteger(2) ] )
  end
  else iState.Push( iLevel.LightFlag[ iState.ToCoord(2), iState.ToInteger(3) ] );
  Result := 1;
end;

function lua_map_node_set_light_flag(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iCoord : TCoord2D;
    iArea  : TArea;
    iLevel : TLuaMapNode;
    iFlag  : Integer;
    iValue : Boolean;
begin
  iState.Init(L);
  iLevel := iState.ToObject(1) as TLuaMapNode;
  if iState.IsNil(4) then
  begin
    lua_pushstring( L, '__coord' );
    lua_rawget( L, 1 );
    if lua_isnil( L, -1 ) then
    begin
      lua_pushstring( L, '__area' );
      lua_rawget( L, 1 );
      iArea  := vlua_toarea( L, lua_absindex(L,-1) );
      iFlag  := iState.ToInteger(2);
      iValue := iState.ToBoolean(3);
      for iCoord in iArea do
        iLevel.LightFlag[ iCoord, iFlag ] := iValue;
      Exit(0);
    end;
    iLevel.LightFlag[ iState.ToCoord(-1), iState.ToInteger(2) ] := iState.ToBoolean(3)
  end
  else
  begin
    if iState.IsCoord(2) then
      iLevel.LightFlag[ iState.ToCoord(2), iState.ToInteger(3) ] := iState.ToBoolean(4)
    else
    begin
      iFlag  := iState.ToInteger(3);
      iValue := iState.ToBoolean(4);
      for iCoord in iArea do
        iLevel.LightFlag[ iCoord, iFlag ] := iValue;
    end;
  end;
  Result := 0;
end;

function lua_map_node_light_index(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iLevel : TLuaMapNode;
begin
  iState.Init(L);
  iLevel := iState.ToObject(1) as TLuaMapNode;
  lua_settop( L, 2 );
  lua_createtable( L, 0, 2 );
    lua_pushstring( L, '__ptr');
    lua_pushlightuserdata( L, iLevel );
    lua_rawset( L, -3 );

    if vlua_iscoord( L, 2 ) then
      lua_pushstring( L, '__coord')
    else if vlua_isarea( L, 2 ) then
      lua_pushstring( L, '__area')
    else luaL_argerror( L, 2, 'Coord or area expected' );
    lua_pushvalue( L, 2 );
    lua_rawset( L, -3 );

    lua_createtable( L, 0, 2 );
      lua_pushcfunction( L, @lua_map_node_get_light_flag );
      lua_setfield( L, -2, '__index' );
      lua_pushcfunction( L, @lua_map_node_set_light_flag );
      lua_setfield( L, -2, '__newindex' );
    lua_setmetatable( L, -2 );

  Result := 1;
end;

function lua_map_node_light_newindex(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iLevel : TLuaMapNode;
    iCoord : TCoord2D;
    iFlag  : Byte;
    iValue : Boolean;
begin
  iState.Init(L);
  iLevel := iState.ToObject(1) as TLuaMapNode;
  iFlag  := iState.ToInteger(2);
  iValue := iState.ToBoolean(3);
  for iCoord in iLevel.Area do
    iLevel.LightFlag[ iCoord, iFlag ] := iValue;
  Result := 0;
end;

function lua_map_node_get_being(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iLevel : TLuaMapNode;
begin
  iState.Init( L );
  iLevel := iState.ToObject(1) as TLuaMapNode;
  iState.Push( iLevel.getBeing( iState.ToCoord(2) ) );
  Result := 1;
end;

function lua_map_node_get_item(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iLevel : TLuaMapNode;
begin
  iState.Init( L );
  iLevel := iState.ToObject(1) as TLuaMapNode;
  iState.Push( iLevel.getItem( iState.ToCoord(2) ) );
  Result := 1;
end;

function lua_map_node_drop(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iLevel : TLuaMapNode;
begin
  iState.Init( L );
  iLevel := iState.ToObject(1) as TLuaMapNode;
  iState.Push( iLevel.Drop( iState.ToObject(2) as TLuaEntityNode, iState.ToCoord(3), iState.ToFlags(4) ) );
  Result := 1;
end;


const lua_map_node_lib : array[0..17] of luaL_Reg = (
  ( name : 'get_area';          func : @lua_map_node_get_area),
  ( name : 'set_hp';            func : @lua_map_node_set_hp),
  ( name : 'get_hp';            func : @lua_map_node_get_hp),
  ( name : 'set_cell';          func : @lua_map_node_set_cell),
  ( name : 'get_cell';          func : @lua_map_node_get_cell),
  ( name : 'raw_set_cell';      func : @lua_map_node_raw_set_cell),
  ( name : 'raw_get_cell';      func : @lua_map_node_raw_get_cell),
  ( name : 'eye_contact';       func : @lua_map_node_eye_contact),
  ( name : 'is_visible';        func : @lua_map_node_is_visible),
  ( name : 'is_empty';          func : @lua_map_node_is_empty),
  ( name : 'is_empty_area';     func : @lua_map_node_is_empty_area),
  ( name : 'children_in_range'; func : @lua_map_node_children_in_range),
  ( name : 'get_light_flag'  ;  func : @lua_map_node_get_light_flag),
  ( name : 'set_light_flag'  ;  func : @lua_map_node_set_light_flag),
  ( name : 'drop';              func : @lua_map_node_drop),
  ( name : 'get_being';         func : @lua_map_node_get_being),
  ( name : 'get_item';          func : @lua_map_node_get_item),
  ( name : nil;                 func : nil; )
);

class procedure TLuaMapNode.RegisterLuaAPI ( const aTableName : AnsiString ) ;
begin
  LuaSystem.Register( aTableName, lua_map_node_lib );
  LuaSystem.RegisterMetaTable( aTableName, 'light', @lua_map_node_light_index, @lua_map_node_light_newindex );
  LuaSystem.RegisterMetaTable( aTableName, 'map',   @lua_map_node_get_cell,    @lua_map_node_set_cell );
  LuaSystem.RegisterMetaTable( aTableName, 'hp',    @lua_map_node_get_hp,      @lua_map_node_set_hp );
end;

end.

