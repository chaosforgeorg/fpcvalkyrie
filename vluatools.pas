{$INCLUDE valkyrie.inc}
// @abstract(Lua tools for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(April 29, 2011)
//
// THIS UNIT IS EXPERIMENTAL!
//
// TODO : set an OPTIONAL area clamping mechanism

unit vluatools;
interface
uses Classes, SysUtils, vlualibrary, vutil, vrltools, vluatype, vvector;

procedure RegisterTableAuxFunctions( L: Plua_State );
procedure RegisterMathAuxFunctions( L: Plua_State );
procedure RegisterUIDClass( L: Plua_State; const Name : AnsiString = 'uids' );
procedure RegisterWeightTableClass( L: Plua_State; const Name : AnsiString = 'weight_table' );
procedure RegisterStringListClass( L: Plua_State; const Name : AnsiString = 'string_list' );
procedure RegisterKillsClass( L: Plua_State; aKills : TKillTable; const aName : AnsiString = 'kills' );
procedure RegisterStatisticsClass( L: Plua_State; aStatistics : TStatistics; const aName : AnsiString = 'stats' );
procedure RegisterCoordClass( L: Plua_State );
procedure RegisterAreaClass( L: Plua_State );
procedure RegisterAreaFull( L: Plua_State; Area : TArea );
procedure RegisterPointClass( L: Plua_State );
procedure RegisterRectClass( L: Plua_State );
procedure RegisterVec2dClass( L: Plua_State );

// WARNING - use absolute indices!
function vlua_iscoord( L: Plua_State; Index : Integer ) : Boolean;
function vlua_tocoord( L: Plua_State; Index : Integer ) : TCoord2D;
function vlua_topcoord( L: Plua_State; Index : Integer ) : PCoord2D;
procedure vlua_pushcoord( L: Plua_State; const Coord : TCoord2D );
function vlua_isarea( L: Plua_State; Index : Integer ) : Boolean;
function vlua_toarea( L: Plua_State; Index : Integer ) : TArea;
function vlua_toparea( L: Plua_State; Index : Integer ) : PArea;
procedure vlua_pusharea( L: Plua_State; const Area : TArea );
function vlua_ispoint( L: Plua_State; Index : Integer ) : Boolean;
function vlua_topoint( L: Plua_State; Index : Integer ) : TPoint;
function vlua_toppoint( L: Plua_State; Index : Integer ) : PPoint;
procedure vlua_pushpoint( L: Plua_State; const Point : TPoint );
function vlua_isrect( L: Plua_State; Index : Integer ) : Boolean;
function vlua_torect( L: Plua_State; Index : Integer ) : TRectangle;
function vlua_toprect( L: Plua_State; Index : Integer ) : PRectangle;
procedure vlua_pushrect( L: Plua_State; const Rect : TRectangle );
function vlua_isvec2d( L: Plua_State; Index : Integer ) : Boolean;
function vlua_tovec2d( L: Plua_State; Index : Integer ) : TVec2d;
function vlua_topvec2d( L: Plua_State; Index : Integer ) : PVec2d;
procedure vlua_pushvec2d( L: Plua_State; const Vec2d : TVec2d );

function vlua_toflags( L : Plua_State; Index : Integer ): TFlags;
function vlua_toflags_array( L : Plua_State; Index : Integer ): TFlags;
function vlua_toflags_set( L : Plua_State; Index : Integer ): TFlags;
procedure vlua_pushflags_array( L : Plua_State; const Flags : TFlags );
procedure vlua_pushflags_set( L : Plua_State; const Flags : TFlags );

function vlua_tochar( L : Plua_State; Index : Integer ) : Char;
function vlua_ischar( L : Plua_State; Index : Integer ) : Boolean;
procedure vlua_pushchar( L : Plua_State; aChar : Char );

procedure vlua_push( L : Plua_State; const Args: array of const );

function LuaCoord( const aCoord : TCoord2D ) : TLuaType;
function LuaCoord( aX,aY : Integer ) : TLuaType;
function LuaArea( const aArea : TArea ) : TLuaType;
function LuaPoint( const aPoint : TPoint ) : TLuaType;
function LuaPoint( aX,aY : Integer ) : TLuaType;
function LuaRect( const aRect : TRectangle ) : TLuaType;
function LuaVec2d( aX,aY : Single ) : TLuaType;
function LuaVec2d( const aCoord : TCoord2D ) : TLuaType;
function LuaVec2d( const aVec2d : TVec2d ) : TLuaType;

implementation

uses vluastate, vluaext, vuid;

function vlua_toflags( L : Plua_State; Index : Integer ): TFlags;
begin
  Index := lua_absindex( L, Index );
  vlua_toflags := [];
  if lua_istable( L, Index ) then
  begin
    lua_pushnil( L );
    while (lua_next( L, Index ) <> 0) do
    begin
      if lua_type( L, -1 ) = LUA_TBOOLEAN
        then Include( vlua_toflags, lua_tointeger( L ,-2 ) )
        else Include( vlua_toflags, lua_tointeger( L ,-1 ) );
      lua_pop( L, 1 );
    end;
  end;
end;

function vlua_toflags_array( L : Plua_State; Index : Integer ): TFlags;
begin
  Index := lua_absindex( L, Index );
  vlua_toflags_array := [];
  if lua_istable( L, Index ) then
  begin
    lua_pushnil( L );
    while (lua_next( L, Index ) <> 0) do
    begin
       Include( vlua_toflags_array, lua_tointeger( L ,-1 ) );
       lua_pop( L, 1 );
    end;
  end;
end;

function vlua_toflags_set( L : Plua_State; Index : Integer ): TFlags;
begin
  Index := lua_absindex( L, Index );
  vlua_toflags_set := [];
  if lua_istable( L, Index ) then
  begin
    lua_pushnil( L );
    while (lua_next( L, Index ) <> 0) do
    begin
       Include( vlua_toflags_set, lua_tointeger( L ,-2 ) );
       lua_pop( L, 1 );
    end;
  end;
end;

procedure vlua_pushflags_array( L : Plua_State; const Flags : TFlags );
var Size, Flag : Byte;
begin
  Size := 0;
  for Flag in Flags do
    Inc( Size );
  lua_createtable( L, Size, 0 );
  Size := 0;
  for Flag in Flags do
  begin
    Inc( Size );
    lua_pushinteger( L, Flag );
    lua_rawseti( L, -2, Size );
  end;
end;

procedure vlua_pushflags_set( L : Plua_State; const Flags : TFlags );
var Size, Flag : Byte;
begin
  Size := 0;
  for Flag in Flags do
    Inc( Size );
  lua_createtable( L, 0, Size );
  Size := 0;
  for Flag in Flags do
  begin
    lua_pushboolean( L, true );
    lua_rawseti( L, -2, Flag );
  end;
end;

const VALKYRIE_COORD = 'valkyrie.coord';
      VALKYRIE_AREA  = 'valkyrie.area';
      VALKYRIE_POINT = 'valkyrie.point';
      VALKYRIE_RECT  = 'valkyrie.rect';
      VALKYRIE_VEC2D = 'valkyrie.vec2d';
      VALKYRIE_VEC3D = 'valkyrie.vec3d';

type TLuaCoord = class( TLuaType )
  constructor Create( const aCoord : TCoord2D );
  constructor Create( aX,aY : Integer );
  procedure Push( L : PLua_state ); override;
private
  FCoord : TCoord2D;
end;

constructor TLuaCoord.Create( const aCoord : TCoord2D );
begin
  FCoord := aCoord
end;

constructor TLuaCoord.Create( aX,aY : Integer );
begin
  FCoord.x := aX;
  FCoord.y := aY;
end;

procedure TLuaCoord.Push( L : PLua_state );
begin
  vlua_pushcoord( L, FCoord );
  Free;
end;

type TLuaArea = class( TLuaType )
  constructor Create( const aArea : TArea );
  procedure Push( L : PLua_state ); override;
private
  FArea : TArea;
end;

constructor TLuaArea.Create( const aArea : TArea );
begin
  FArea := aArea;
end;

procedure TLuaArea.Push( L : PLua_state );
begin
  vlua_pusharea( L, FArea );
  Free;
end;

type TLuaPoint = class( TLuaType )
  constructor Create( const aPoint : TPoint );
  constructor Create( aX,aY : Integer );
  procedure Push( L : PLua_state ); override;
private
  FPoint : TPoint;
end;

constructor TLuaPoint.Create( const aPoint : TPoint );
begin
  FPoint := aPoint
end;

constructor TLuaPoint.Create( aX,aY : Integer );
begin
  FPoint.x := aX;
  FPoint.y := aY;
end;

procedure TLuaPoint.Push( L : PLua_state );
begin
  vlua_pushpoint( L, FPoint );
  Free;
end;

type TLuaRect = class( TLuaType )
  constructor Create( const aRect : TRectangle );
  procedure Push( L : PLua_state ); override;
private
  FRect : TRectangle;
end;

constructor TLuaRect.Create( const aRect : TRectangle );
begin
  FRect := aRect;
end;

procedure TLuaRect.Push( L : PLua_state );
begin
  vlua_pushrect( L, FRect );
  Free;
end;

type TLuaVec2d = class( TLuaType )
  constructor Create( const aVec2d : TVec2d );
  constructor Create( const aCoord : TCoord2D );
  constructor Create( aX,aY : Single );
  procedure Push( L : PLua_state ); override;
private
  FVec2d : TVec2d;
end;

constructor TLuaVec2d.Create( const aVec2d : TVec2d );
begin
  FVec2d := aVec2d;
end;

constructor TLuaVec2d.Create( const aCoord : TCoord2D );
begin
  FVec2d.x := aCoord.x;
  FVec2d.y := aCoord.y;
end;

constructor TLuaVec2d.Create( aX,aY : Single );
begin
  FVec2d.x := aX;
  FVec2d.y := aY;
end;

procedure TLuaVec2d.Push( L : PLua_state );
begin
  vlua_pushvec2d( L, FVec2d );
  Free;
end;

function vlua_tochar ( L : Plua_State; Index : Integer ) : Char;
begin
  if (lua_type( L, Index ) <> LUA_TSTRING) or
     (lua_objlen( L, Index ) < 1) then Exit( ' ' );
  Exit( lua_tostring( L, Index )[1] );
end;

function vlua_ischar ( L : Plua_State; Index : Integer ) : Boolean;
begin
  Exit( (lua_type( L, Index ) = LUA_TSTRING) and (lua_objlen( L, Index ) >= 1) )
end;

procedure vlua_pushchar ( L : Plua_State; aChar : Char ) ;
begin
  lua_pushlstring( L, @aChar, 1 );
end;

procedure vlua_push ( L : Plua_State; const Args : array of const ) ;
var NArgs, i : LongInt;
begin
  NArgs := High(Args);
  if NArgs >= 0 then
  for i:=0 to NArgs do
    if Args[i].vtype = vtObject
       then vlua_pushobject( L, Args[i].VObject )
       else vlua_pushvarrec( L, @Args[i].vtype);
end;

function LuaCoord( const aCoord : TCoord2D ) : TLuaType;
begin
  Exit( TLuaCoord.Create( aCoord ) );
end;

function LuaCoord( aX,aY : Integer ) : TLuaType;
begin
  Exit( TLuaCoord.Create( aX,aY ) );
end;

function LuaArea( const aArea : TArea ) : TLuaType;
begin
  Exit( TLuaArea.Create( aArea ) );
end;

function LuaPoint( const aPoint : TPoint ) : TLuaType;
begin
  Exit( TLuaPoint.Create( aPoint ) );
end;

function LuaPoint( aX,aY : Integer ) : TLuaType;
begin
  Exit( TLuaPoint.Create( aX,aY ) );
end;

function LuaRect( const aRect : TRectangle ) : TLuaType;
begin
  Exit( TLuaRect.Create( aRect ) );
end;

function LuaVec2d( aX,aY : Single ) : TLuaType;
begin
  Exit( TLuaVec2d.Create( aX, aY ) );
end;

function LuaVec2d( const aCoord : TCoord2D ) : TLuaType;
begin
  Exit( TLuaVec2d.Create( aCoord ) );
end;

function LuaVec2d( const aVec2d : TVec2d ) : TLuaType;
begin
  Exit( TLuaVec2d.Create( aVec2d ) );
end;

// -------- Helper functions ------------------------------------------ //

function lua_tointeger_def( L: Plua_State; Index : Integer; DValue : Integer ) : Integer;
begin
  if lua_type( L, Index ) = LUA_TNUMBER
    then Exit( lua_tointeger( L, Index ) )
    else Exit( DValue );
end;

function lua_tonumber_def( L: Plua_State; Index : Integer; DValue : lua_Number ) : lua_Number;
begin
  if lua_type( L, Index ) = LUA_TNUMBER
    then Exit( lua_tonumber( L, Index ) )
    else Exit( DValue );
end;

function vlua_iscoord( L: Plua_State; Index : Integer ) : Boolean;
begin
  Exit( luaL_testudata( L, Index, VALKYRIE_COORD ) <> nil );
end;

function vlua_tocoord( L: Plua_State; Index : Integer ) : TCoord2D;
var CoordPtr : PCoord2D;
begin
  CoordPtr := luaL_checkudata( L, Index, VALKYRIE_COORD );
  Exit( CoordPtr^ );
end;

function vlua_topcoord( L: Plua_State; Index : Integer ) : PCoord2D;
var CoordPtr : PCoord2D;
begin
  CoordPtr := luaL_checkudata( L, Index, VALKYRIE_COORD );
  Exit( CoordPtr );
end;

procedure vlua_pushcoord( L: Plua_State; const Coord : TCoord2D );
var CoordPtr : PCoord2D;
begin
  CoordPtr  := PCoord2D(lua_newuserdata(L, SizeOf(TCoord2D)));
  CoordPtr^ := Coord;

  luaL_getmetatable( L, VALKYRIE_COORD );
  lua_setmetatable( L, -2 );
end;

function vlua_isarea( L: Plua_State; Index : Integer ) : Boolean;
begin
  Exit( luaL_testudata( L, Index, VALKYRIE_AREA ) <> nil );
end;

function vlua_toarea( L: Plua_State; Index : Integer ) : TArea;
var AreaPtr : PArea;
begin
  AreaPtr := luaL_checkudata( L, Index, VALKYRIE_AREA );
  Exit( AreaPtr^ );
end;

function vlua_toparea( L: Plua_State; Index : Integer ) : PArea;
var AreaPtr : PArea;
begin
  AreaPtr := luaL_checkudata( L, Index, VALKYRIE_AREA );
  Exit( AreaPtr );
end;

procedure vlua_pusharea( L: Plua_State; const Area : TArea );
var AreaPtr : PArea;
begin
  AreaPtr  := PArea(lua_newuserdata(L, SizeOf(TArea)));
  AreaPtr^ := Area;

  luaL_getmetatable( L, VALKYRIE_AREA );
  lua_setmetatable( L, -2 );
end;

function vlua_ispoint(L: Plua_State; Index: Integer): Boolean;
begin
  Exit( luaL_testudata( L, Index, VALKYRIE_POINT ) <> nil );
end;

function vlua_topoint(L: Plua_State; Index: Integer): TPoint;
var PointPtr : PPoint;
begin
  PointPtr := luaL_checkudata( L, Index, VALKYRIE_POINT );
  Exit( PointPtr^ );
end;

function vlua_toppoint(L: Plua_State; Index: Integer): PPoint;
var PointPtr : PPoint;
begin
  PointPtr := luaL_checkudata( L, Index, VALKYRIE_POINT );
  Exit( PointPtr );
end;

procedure vlua_pushpoint(L: Plua_State; const Point: TPoint);
var PointPtr : PPoint;
begin
  PointPtr  := PPoint(lua_newuserdata(L, SizeOf(TPoint)));
  PointPtr^ := Point;

  luaL_getmetatable( L, VALKYRIE_POINT );
  lua_setmetatable( L, -2 );
end;

function vlua_isrect( L: Plua_State; Index : Integer ) : Boolean;
begin
  Exit( luaL_testudata( L, Index, VALKYRIE_RECT ) <> nil );
end;

function vlua_torect( L: Plua_State; Index : Integer ) : TRectangle;
var RectPtr : PRectangle;
begin
  RectPtr := luaL_checkudata( L, Index, VALKYRIE_RECT );
  Exit( RectPtr^ );
end;

function vlua_toprect( L: Plua_State; Index : Integer ) : PRectangle;
var RectPtr : PRectangle;
begin
  RectPtr := luaL_checkudata( L, Index, VALKYRIE_RECT );
  Exit( RectPtr );
end;

procedure vlua_pushrect( L: Plua_State; const Rect : TRectangle );
var RectPtr : PRectangle;
begin
  RectPtr  := PRectangle(lua_newuserdata(L, SizeOf(TRectangle)));
  RectPtr^ := Rect;

  luaL_getmetatable( L, VALKYRIE_RECT );
  lua_setmetatable( L, -2 );
end;

function vlua_isvec2d(L: Plua_State; Index: Integer): Boolean;
begin
  Exit( luaL_testudata( L, Index, VALKYRIE_VEC2D ) <> nil );
end;

function vlua_tovec2d(L: Plua_State; Index: Integer): TVec2d;
var VecPtr : PVec2d;
begin
  VecPtr := luaL_checkudata( L, Index, VALKYRIE_VEC2D );
  Exit( VecPtr^ );
end;

function vlua_topvec2d(L: Plua_State; Index: Integer): PVec2d;
var VecPtr : PVec2d;
begin
  VecPtr := luaL_checkudata( L, Index, VALKYRIE_VEC2D );
  Exit( VecPtr );
end;

procedure vlua_pushvec2d(L: Plua_State; const Vec2d: TVec2d);
var VecPtr : PVec2d;
begin
  VecPtr  := PVec2d(lua_newuserdata(L, SizeOf(TVec2d)));
  VecPtr^ := Vec2d;

  luaL_getmetatable( L, VALKYRIE_VEC2D );
  lua_setmetatable( L, -2 );
end;


// -------- Coord functions ------------------------------------------- //

function lua_coord_new( L: Plua_State): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord.Create( lua_tointeger_def(L,1,0), lua_tointeger_def(L,2,0) );
  vlua_pushcoord( L, Coord );
  Exit(1);
end;

function lua_coord_unm( L: Plua_State): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord := vlua_tocoord( L, 1 );
  vlua_pushcoord( L, NewCoord2D( -Coord.x, -Coord.y ) );
  Exit(1);
end;

function lua_coord_add( L: Plua_State): Integer; cdecl;
begin
  vlua_pushcoord( L, vlua_tocoord( L, 1 ) + vlua_tocoord( L, 2 ) );
  Exit(1);
end;

function lua_coord_sub( L: Plua_State): Integer; cdecl;
begin
  vlua_pushcoord( L, vlua_tocoord( L, 1 ) - vlua_tocoord( L, 2 ) );
  Exit(1);
end;

function lua_coord_mul( L: Plua_State): Integer; cdecl;
begin
  if vlua_iscoord( L, 1 ) then
    if vlua_iscoord( L, 2 ) then
      vlua_pushcoord( L, vlua_tocoord( L, 1 ) * vlua_tocoord( L, 2 ) )
    else
      vlua_pushcoord( L, vlua_tocoord( L, 1 ) * lua_tointeger( L, 2 ) )
  else
    vlua_pushcoord( L, vlua_tocoord( L, 2 ) * lua_tointeger( L, 1 ) );
  Exit(1);
end;

function lua_coord_eq( L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean( L, vlua_tocoord( L, 1 ) = vlua_tocoord( L, 2 ) );
  Exit(1);
end;

function lua_coord_get( L: Plua_State): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord := vlua_tocoord( L, 1 );
  lua_pushinteger( L, Coord.x );
  lua_pushinteger( L, Coord.y );
  Exit(2);
end;

function lua_coord_tostring( L: Plua_State): Integer; cdecl;
begin
  lua_pushansistring( L, vlua_tocoord( L, 1 ).ToString );
  Exit(1);
end;

function lua_coord_abs( L: Plua_State): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord := vlua_tocoord( L, 1 );
  vlua_pushcoord( L, NewCoord2D( Abs(Coord.x), Abs(Coord.y) ) );
  Exit(1);
end;

function lua_coord_sign( L: Plua_State): Integer; cdecl;
begin
  vlua_pushcoord( L, vlua_tocoord( L, 1 ).Sign );
  Exit(1);
end;

function lua_coord_clone( L: Plua_State): Integer; cdecl;
begin
  vlua_pushcoord( L, vlua_tocoord( L, 1 ) );
  Exit(1);
end;

function lua_coord_distance( L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger( L, Distance( vlua_tocoord( L, 1 ), vlua_tocoord( L, 2 ) ) );
  Exit(1);
end;

function lua_coord_real_distance( L: Plua_State): Integer; cdecl;
begin
  lua_pushnumber( L, RealDistance( vlua_tocoord( L, 1 ), vlua_tocoord( L, 2 ) ) );
  Exit(1);
end;

function lua_coord_random( L: Plua_State): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord.Random( vlua_tocoord( L, 1 ), vlua_tocoord( L, 2 ) );
  vlua_pushcoord( L, Coord );
  Exit(1);
end;

function lua_coord_random_shift( L: Plua_State ): Integer; cdecl;
var PCoord : PCoord2D;
begin
  PCoord := vlua_topcoord( L, 1 );
  PCoord^.RandomShift( lua_tointeger_def( L, 2, 1 ) );
  Exit(0);
end;

function lua_coord_random_shifted( L: Plua_State ): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord := vlua_tocoord( L, 1 );
  Coord.RandomShift( lua_tointeger_def( L, 2, 1 ) );
  vlua_pushcoord( L, Coord );
  Exit(1);
end;

function lua_coord_cross_coords_closure( L: Plua_State): Integer; cdecl;
var c   : PCoord2D;
    idx : Byte;
begin
  c    := vlua_topcoord( L, lua_upvalueindex(1) );
  idx  := lua_tointeger( L, lua_upvalueindex(2) );
  Inc(Idx);
  case idx of
    1 : c^.x -= 1;
    2 : c^.x += 2;
    3 : begin c^.x -= 1; c^.y -= 1; end;
    4 : c^.y += 2;
  end;
  lua_pushinteger( L, Idx );
  lua_replace( L, lua_upvalueindex(2) );
  if Idx > 4
    then lua_pushnil( L )
    else lua_pushvalue( L, lua_upvalueindex(1) );
  Exit(1);
end;

function lua_coord_cross_coords( L: Plua_State): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord := vlua_tocoord( L, 1 );
  vlua_pushcoord( L, Coord );
  lua_pushinteger( L, 0 );
  lua_pushcclosure(L, @lua_coord_cross_coords_closure, 2);
  Exit(1);
end;

function lua_coord_around_coords_closure( L: Plua_State): Integer; cdecl;
var c   : PCoord2D;
    idx : Byte;
begin
  c    := vlua_topcoord( L, lua_upvalueindex(1) );
  idx  := lua_tointeger( L, lua_upvalueindex(2) );
  Inc(Idx);
  case idx of
    1 : begin c^.x -= 1; c^.y -= 1; end;
    2 : c^.x += 1;
    3 : c^.x += 1;
    4 : c^.y += 1;
    5 : c^.y += 1;
    6 : c^.x -= 1;
    7 : c^.x -= 1;
    8 : c^.y -= 1;
  end;
  lua_pushinteger( L, Idx );
  lua_replace( L, lua_upvalueindex(2) );
  if Idx > 8
    then lua_pushnil( L )
    else lua_pushvalue( L, lua_upvalueindex(1) );
  Exit(1);
end;

function lua_coord_around_coords( L: Plua_State): Integer; cdecl;
var Coord : TCoord2D;
begin
  Coord := vlua_tocoord( L, 1 );
  vlua_pushcoord( L, Coord );
  lua_pushinteger( L, 0 );
  lua_pushcclosure(L, @lua_coord_around_coords_closure, 2);
  Exit(1);
end;


function lua_coord_index( L: Plua_State ): Integer; cdecl;
var PCoord : PCoord2D;
    Index  : AnsiString;
begin
  PCoord := vlua_topcoord( L, 1 );
  Index  := lua_tostring( L, 2 );
       if Index = 'x' then lua_pushinteger( L, PCoord^.x )
  else if Index = 'y' then lua_pushinteger( L, PCoord^.y )
  else
    begin
      lua_getglobal( L, 'coord' );
      lua_pushvalue( L, -2 );
      lua_rawget( L, -2 );
    end;
  Exit(1);
end;

function lua_coord_newindex( L: Plua_State ): Integer; cdecl;
var PCoord : PCoord2D;
    Index  : AnsiString;
    Value  : Integer;
begin
  PCoord := vlua_topcoord( L, 1 );
  Index  := lua_tostring( L, 2 );
  Value  := lua_tointeger_def( L, 3, 0 );
       if Index = 'x' then PCoord^.x := Value
  else if Index = 'y' then PCoord^.y := Value;
  Exit(0);
end;

// -------- Area functions -------------------------------------------- //

function lua_area_new( L: Plua_State): Integer; cdecl;
var Area : TArea;
begin
  if vlua_iscoord( L, 1 ) then
    if vlua_iscoord( L, 2 ) then
      begin
        Area.Create( vlua_tocoord(L,1), vlua_tocoord(L,2) );
        vlua_pusharea( L, Area );
        Exit(1);
      end;
  Area.Create(
    NewCoord2D( lua_tointeger_def(L,1,0), lua_tointeger_def(L,2,0) ),
    NewCoord2D( lua_tointeger_def(L,3,0), lua_tointeger_def(L,4,0) )
  );
  vlua_pusharea( L, Area );
  Exit(1);
end;

function lua_area_eq( L: Plua_State): Integer; cdecl;
var lhs, rhs : TArea;
begin
  lhs := vlua_toarea( L, 1 );
  rhs := vlua_toarea( L, 2 );
  lua_pushboolean( L, (lhs.a = rhs.a) and (lhs.b = rhs.b) );
  Exit(1);
end;

function lua_area_get( L: Plua_State): Integer; cdecl;
var Area : TArea;
begin
  Area := vlua_toarea( L, 1 );
  vlua_pushcoord( L, Area.a );
  vlua_pushcoord( L, Area.b );
  Exit(2);
end;

function lua_area_clone( L: Plua_State): Integer; cdecl;
begin
  vlua_pusharea( L, vlua_toarea( L, 1 ) );
  Exit(1);
end;

function lua_area_tostring( L: Plua_State): Integer; cdecl;
begin
  lua_pushansistring( L, vlua_toarea( L, 1 ).ToString );
  Exit(1);
end;

function lua_area_coords_closure( L: Plua_State): Integer; cdecl;
var Area : PArea;
    c    : PCoord2D;
begin
  Area := vlua_toparea( L, lua_upvalueindex(1) );
  c    := vlua_topcoord( L, lua_upvalueindex(2) );

  c^.x := c^.x + 1;
  if c^.x > Area^.b.x then
  begin
    c^.x := Area^.a.x;
    c^.y := c^.y + 1;
    if c^.y > Area^.b.y then begin lua_pushnil( L ); Exit(1); end
  end;

  vlua_pushcoord( L, c^ );
  Exit(1);
end;

function lua_area_coords( L: Plua_State): Integer; cdecl;
var Area : PArea;
    A    : TCoord2D;
begin
  Area := vlua_toparea( L, 1 );
  A := Area^.A;
  A.X := A.X - 1;
  vlua_pushcoord( L, A );
  lua_pushcclosure(L, @lua_area_coords_closure, 2);
  Exit(1);
end;

function lua_area_edges_closure( L: Plua_State): Integer; cdecl;
var Area : PArea;
    c    : PCoord2D;
begin
  Area := vlua_toparea( L, lua_upvalueindex(1) );
  c    := vlua_topcoord( L, lua_upvalueindex(2) );

  c^.x := c^.x + 1;
  if c^.x > Area^.b.x then
  begin
    c^.x := Area^.a.x;
    c^.y := c^.y + 1;
    if c^.y > Area^.b.y then begin lua_pushnil( L ); Exit(1); end
  end;
  if (c^.y <> Area^.a.y) and (c^.y <> Area^.b.y) and (c^.x = Area^.a.x + 1) then c^.x := Area^.b.x;

  vlua_pushcoord( L, c^ );
  Exit(1);
end;

function lua_area_edges( L: Plua_State): Integer; cdecl;
var Area : PArea;
    A    : TCoord2D;
begin
  Area := vlua_toparea( L, 1 );
  A := Area^.A;
  A.X := A.X - 1;
  vlua_pushcoord( L, A );
  lua_pushcclosure(L, @lua_area_edges_closure, 2);
  Exit(1);
end;

function lua_area_corners_closure( L: Plua_State): Integer; cdecl;
var Index : Integer;
begin
  Index := lua_tointeger( L, lua_upvalueindex(2) ) + 1;
  lua_pushinteger( L, Index );
  lua_replace( L, lua_upvalueindex(2) ); // update
  lua_rawgeti( L, lua_upvalueindex(1), Index ); // get value
  Exit(1);
end;

function lua_area_corners( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );

  lua_createtable(L, 4, 0);
  vlua_pushcoord( L, Area^.A );
  lua_rawseti( L, -2, 1 );
  vlua_pushcoord( L, Area^.TopRight );
  lua_rawseti( L, -2, 2 );
  vlua_pushcoord( L, Area^.BottomLeft );
  lua_rawseti( L, -2, 3 );
  vlua_pushcoord( L, Area^.B );
  lua_rawseti( L, -2, 4 );

  lua_pushnumber(L, 0);

  lua_pushcclosure(L, @lua_area_corners_closure, 2);
  Exit(1);
end;

function lua_area_random_coord( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  vlua_pushcoord( L, Area^.RandomCoord() );
  Exit(1);
end;

function lua_area_random_edge_coord( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  vlua_pushcoord( L, Area^.RandomEdgeCoord() );
  Exit(1);
end;

function lua_area_random_inner_edge_coord( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  vlua_pushcoord( L, Area^.RandomInnerEdgeCoord() );
  Exit(1);
end;

function lua_area_shrink( L: Plua_State): Integer; cdecl;
var Area   : PArea;
    Amount : Integer;
begin
  Area   := vlua_toparea( L, 1 );
  Amount := lua_tointeger_def( L, 2, 1 );
  Area^.Shrink( Amount );
  Exit(0);
end;

function lua_area_shrinked( L: Plua_State): Integer; cdecl;
var Area : PArea;
    Amount : Integer;
begin
  Area   := vlua_toparea( L, 1 );
  Amount := lua_tointeger_def( L, 2, 1 );
  vlua_pusharea( L, Area^.Shrinked( Amount ) );
  Exit(1);
end;

function lua_area_expand( L: Plua_State): Integer; cdecl;
var Area   : PArea;
    Amount : Integer;
begin
  Area   := vlua_toparea( L, 1 );
  Amount := lua_tointeger_def( L, 2, 1 );
  Area^.Expand( Amount );
  Exit(0);
end;

function lua_area_expanded( L: Plua_State): Integer; cdecl;
var Area : PArea;
    Amount : Integer;
begin
  Area   := vlua_toparea( L, 1 );
  Amount := lua_tointeger_def( L, 2, 1 );
  vlua_pusharea( L, Area^.Expanded( Amount ) );
  Exit(1);
end;

function lua_area_clamp( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  vlua_toparea( L, 2 )^.Clamp( Area^ );
  Exit(0);
end;

function lua_area_clamped( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  vlua_pusharea( L, Area^.Clamped( vlua_toparea( L, 2 )^ ) );
  Exit(1);
end;

function lua_area_fix( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  if Area^.a.x > Area^.b.x then Area^.a.x := Area^.b.x;
  if Area^.a.y > Area^.b.y then Area^.a.y := Area^.b.y;
  Exit(0);
end;

function lua_area_proper( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  lua_pushboolean( L, ( Area^.a.x <= Area^.b.x ) and ( Area^.a.y <= Area^.b.y ) );
  Exit(1);
end;

function lua_area_dim( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  vlua_pushcoord( L, Area^.b - Area^.a + UnitCoord2D );
  Exit(1);
end;

function lua_area_size( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
  lua_pushinteger( L, Area^.EnclosedArea );
  Exit(1);
end;

function lua_area_around( L: Plua_State): Integer; cdecl;
var Where  : TCoord2D;
    Amount : Integer;
begin
  Where  := vlua_tocoord( L, 1 );
  Amount := lua_tointeger_def( L, 2, 1 );
  vlua_pusharea( L, NewArea( Where, Amount ) );

  // TODO: CLAMP?

  Exit(1);
end;

function lua_area_clamp_coord( L: Plua_State): Integer; cdecl;
var Area : PArea;
    PC   : PCoord2D;
begin
  Area := vlua_toparea( L, 1 );
  PC   := vlua_topcoord( L, 2 );
  Area^.Clamp( PC^ );
  Exit(0);
end;

function lua_area_contains( L: Plua_State): Integer; cdecl;
var Area : PArea;
begin
  Area := vlua_toparea( L, 1 );
       if vlua_iscoord( L, 2 ) then
         lua_pushboolean( L, Area^.Contains( vlua_tocoord( L, 2 ) ) )
  else if vlua_isarea( L, 2 ) then
         lua_pushboolean( L, Area^.Contains( vlua_toarea( L, 2 ) ) )
  else
         luaL_argerror( L, 2, 'area or coord expected' );
  Exit(1);
end;

function lua_area_random_subarea( L: Plua_State): Integer; cdecl;
var Area : PArea;
    Dim  : PCoord2D;
begin
  Area := vlua_toparea( L, 1 );
  Dim  := vlua_topcoord( L, 2 );
  vlua_pusharea( L, Area^.RandomSubArea( Dim^ ) );
  Exit(1);
end;

function lua_area_is_edge( L: Plua_State): Integer; cdecl;
var Area : PArea;
    C    : PCoord2D;
begin
  Area := vlua_toparea( L, 1 );
  C    := vlua_topcoord( L, 2 );
  lua_pushboolean( L, Area^.isEdge( c^ ) );
  Exit(1);
end;

function lua_area_index( L: Plua_State ): Integer; cdecl;
var Area  : PArea;
    Index : AnsiString;
begin
  Area  := vlua_toparea( L, 1 );
  Index := lua_tostring( L, 2 );
       if Index = 'a' then vlua_pushcoord( L, Area^.a )
  else if Index = 'b' then vlua_pushcoord( L, Area^.b )
  else
    begin
      lua_getglobal( L, 'area' );
      lua_pushvalue( L, -2 );
      lua_rawget( L, -2 );
    end;
  Exit(1);
end;

function lua_area_newindex( L: Plua_State ): Integer; cdecl;
var Area  : PArea;
    Index : AnsiString;
    Value : TCoord2D;
begin
  Area  := vlua_toparea( L, 1 );
  Index := lua_tostring( L, 2 );
  Value := vlua_tocoord( L, 3 );
       if Index = 'a' then Area^.a := Value
  else if Index = 'b' then Area^.b := Value;
  Exit(0);
end;

// -------- Point functions ------------------------------------------- //

function lua_point_new( L: Plua_State): Integer; cdecl;
var Point : TPoint;
begin
  Point.Init( lua_tointeger_def(L,1,0), lua_tointeger_def(L,2,0) );
  vlua_pushpoint( L, Point );
  Exit(1);
end;

function lua_point_unm( L: Plua_State): Integer; cdecl;
var Point : TPoint;
begin
  Point := vlua_topoint( L, 1 );
  vlua_pushpoint( L, vutil.Point( -Point.x, -Point.y ) );
  Exit(1);
end;

function lua_point_add( L: Plua_State): Integer; cdecl;
begin
  vlua_pushpoint( L, vlua_topoint( L, 1 ) + vlua_topoint( L, 2 ) );
  Exit(1);
end;

function lua_point_sub( L: Plua_State): Integer; cdecl;
begin
  vlua_pushpoint( L, vlua_topoint( L, 1 ) - vlua_topoint( L, 2 ) );
  Exit(1);
end;

function lua_point_mul( L: Plua_State): Integer; cdecl;
begin
  if vlua_ispoint( L, 1 ) then
    vlua_pushpoint( L, vlua_topoint( L, 1 ) * lua_tointeger( L, 2 ) )
  else
    vlua_pushpoint( L, vlua_topoint( L, 2 ) * lua_tointeger( L, 1 ) );
  Exit(1);
end;

function lua_point_eq( L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean( L, vlua_topoint( L, 1 ) = vlua_topoint( L, 2 ) );
  Exit(1);
end;

function lua_point_get( L: Plua_State): Integer; cdecl;
var Point : TPoint;
begin
  Point := vlua_topoint( L, 1 );
  lua_pushinteger( L, Point.x );
  lua_pushinteger( L, Point.y );
  Exit(2);
end;

function lua_point_tostring( L: Plua_State): Integer; cdecl;
begin
  lua_pushansistring( L, vlua_topoint( L, 1 ).ToString );
  Exit(1);
end;

function lua_point_clone( L: Plua_State): Integer; cdecl;
begin
  vlua_pushpoint( L, vlua_topoint( L, 1 ) );
  Exit(1);
end;

function lua_point_index( L: Plua_State ): Integer; cdecl;
var PPoint : vutil.PPoint;
    Index  : AnsiString;
begin
  PPoint := vlua_toppoint( L, 1 );
  Index  := lua_tostring( L, 2 );
       if Index = 'x' then lua_pushinteger( L, PPoint^.x )
  else if Index = 'y' then lua_pushinteger( L, PPoint^.y )
  else
    begin
      lua_getglobal( L, 'point' );
      lua_pushvalue( L, -2 );
      lua_rawget( L, -2 );
    end;
  Exit(1);
end;

function lua_point_newindex( L: Plua_State ): Integer; cdecl;
var PPoint : vutil.PPoint;
    Index  : AnsiString;
    Value  : Integer;
begin
  PPoint := vlua_toppoint( L, 1 );
  Index  := lua_tostring( L, 2 );
  Value  := lua_tointeger_def( L, 3, 0 );
       if Index = 'x' then PPoint^.x := Value
  else if Index = 'y' then PPoint^.y := Value;
  Exit(0);
end;

// -------- Rect functions -------------------------------------------- //

function lua_rect_new( L: Plua_State): Integer; cdecl;
var Rect : TRectangle;
begin
  if vlua_ispoint( L, 1 ) then
  begin
    if vlua_ispoint( L, 2 )
      then Rect.Init( vlua_topoint(L,1), vlua_topoint(L,2) )
      else Rect.Init( vlua_topoint(L,1),
             lua_tointeger_def(L,2,0), lua_tointeger_def(L,3,0) );
  end
  else
    Rect.Init(
      lua_tointeger_def(L,1,0), lua_tointeger_def(L,2,0),
      lua_tointeger_def(L,3,0), lua_tointeger_def(L,4,0)
    );
  vlua_pushrect( L, Rect );
  Exit(1);
end;

function lua_rect_eq( L: Plua_State): Integer; cdecl;
var lhs, rhs : TRectangle;
begin
  lhs := vlua_torect( L, 1 );
  rhs := vlua_torect( L, 2 );
  lua_pushboolean( L, (lhs.pos = rhs.pos) and (lhs.dim = rhs.dim) );
  Exit(1);
end;

function lua_rect_get( L: Plua_State): Integer; cdecl;
var Rect : TRectangle;
begin
  Rect := vlua_torect( L, 1 );
  vlua_pushpoint( L, Rect.Pos );
  vlua_pushpoint( L, Rect.Dim );
  Exit(2);
end;

function lua_rect_clone( L: Plua_State): Integer; cdecl;
begin
  vlua_pushrect( L, vlua_torect( L, 1 ) );
  Exit(1);
end;

function lua_rect_tostring( L: Plua_State): Integer; cdecl;
begin
  lua_pushansistring( L, vlua_torect( L, 1 ).ToString );
  Exit(1);
end;

function lua_rect_shrink( L: Plua_State): Integer; cdecl;
var Rect   : PRectangle;
begin
  Rect   := vlua_toprect( L, 1 );
  if lua_isnumber( L, 3 )
    then Rect^.Shrink( lua_tointeger( L, 2 ), lua_tointeger( L, 3 ) )
    else Rect^.Shrink( lua_tointeger_def( L, 2, 1 ) );
  Exit(0);
end;

function lua_rect_shrinked( L: Plua_State): Integer; cdecl;
var Rect   : PRectangle;
begin
  Rect   := vlua_toprect( L, 1 );
  if lua_isnumber( L, 3 )
    then vlua_pushrect( L, Rect^.Shrinked( lua_tointeger( L, 2 ), lua_tointeger( L, 3 ) ) )
    else vlua_pushrect( L, Rect^.Shrinked( lua_tointeger_def( L, 2, 1 ) ) );
  Exit(1);
end;

function lua_rect_expand( L: Plua_State): Integer; cdecl;
var Rect   : PRectangle;
begin
  Rect   := vlua_toprect( L, 1 );
  if lua_isnumber( L, 3 )
    then Rect^.Expand( lua_tointeger( L, 2 ), lua_tointeger( L, 3 ) )
    else Rect^.Expand( lua_tointeger_def( L, 2, 1 ) );
  Exit(0);
end;

function lua_rect_expanded( L: Plua_State): Integer; cdecl;
var Rect   : PRectangle;
begin
  Rect   := vlua_toprect( L, 1 );
  if lua_isnumber( L, 3 )
    then vlua_pushrect( L, Rect^.Expanded( lua_tointeger( L, 2 ), lua_tointeger( L, 3 ) ) )
    else vlua_pushrect( L, Rect^.Expanded( lua_tointeger_def( L, 2, 1 ) ) );
  Exit(1);
end;

function lua_rect_contains( L: Plua_State): Integer; cdecl;
var Rect   : PRectangle;
begin
  Rect   := vlua_toprect( L, 1 );
  if vlua_ispoint( L, 2 ) then
    lua_pushboolean( L, vlua_topoint( L, 2 ) in Rect^ )
  else
    luaL_argerror( L, 2, 'point expected' );
  Exit(1);
end;

function lua_rect_index( L: Plua_State ): Integer; cdecl;
var Rect  : PRectangle;
    Index : AnsiString;
begin
  Rect  := vlua_toprect( L, 1 );
  Index := lua_tostring( L, 2 );
       if Index = 'dim'   then vlua_pushpoint( L, Rect^.Dim )
  else if Index = 'pos'   then vlua_pushpoint( L, Rect^.Pos )
  else if Index = 'pos2'  then vlua_pushpoint( L, Rect^.Pos2 )
  else if Index = 'center'then vlua_pushpoint( L, Rect^.GetCenter )
  else if Index = 'x'     then lua_pushinteger( L, Rect^.X )
  else if Index = 'y'     then lua_pushinteger( L, Rect^.Y )
  else if Index = 'w'     then lua_pushinteger( L, Rect^.W )
  else if Index = 'h'     then lua_pushinteger( L, Rect^.H )
  else if Index = 'x2'    then lua_pushinteger( L, Rect^.X2 )
  else if Index = 'y2'    then lua_pushinteger( L, Rect^.Y2 )
  else
    begin
      lua_getglobal( L, 'rect' );
      lua_pushvalue( L, -2 );
      lua_rawget( L, -2 );
    end;
  Exit(1);
end;

function lua_rect_newindex( L: Plua_State ): Integer; cdecl;
var Rect  : PRectangle;
    Index : AnsiString;
begin
  Rect  := vlua_toprect( L, 1 );
  Index := lua_tostring( L, 2 );
       if Index = 'pos'   then Rect^.Pos   := vlua_topoint( L, 3 )
  else if Index = 'dim'   then Rect^.Dim   := vlua_topoint( L, 3 )
  else if Index = 'x'     then Rect^.X     := lua_tointeger( L, 3 )
  else if Index = 'y'     then Rect^.Y     := lua_tointeger( L, 3 )
  else if Index = 'w'     then Rect^.W     := lua_tointeger( L, 3 )
  else if Index = 'h'     then Rect^.H     := lua_tointeger( L, 3 );
  Exit(0);
end;

function lua_rect_add( L: Plua_State): Integer; cdecl;
begin
  vlua_pushrect( L, vlua_torect( L, 1 ) + vlua_topoint( L, 2 ) );
  Exit(1);
end;

function lua_rect_sub( L: Plua_State): Integer; cdecl;
begin
  vlua_pushrect( L, vlua_torect( L, 1 ) - vlua_topoint( L, 2 ) );
  Exit(1);
end;

// -------- Vec2d functions ------------------------------------------- //

function NewVec2d( x, y : Double ): TVec2d;
begin
  Result.Init(x,y);
end;

function lua_vec2d_new( L: Plua_State): Integer; cdecl;
var Vec2d : TVec2d;
    Coord : TCoord2d;
    Point : TPoint;
begin
  if vlua_iscoord( L, 1 ) then
  begin
    Coord := vlua_tocoord(L,1);
    Vec2d.Init(Coord.x, Coord.y);
  end
  else if vlua_ispoint( L, 1 ) then
  begin
    Point := vlua_topoint(L, 1);
    Vec2d.Init(Point.x, Point.y);
  end
  else
    Vec2d.Init( lua_tonumber_def(L,1,0), lua_tonumber_def(L,2,0) );
  vlua_pushvec2d( L, Vec2d );
  Exit(1);
end;

function lua_vec2d_unm( L: Plua_State): Integer; cdecl;
var Vec2d : TVec2d;
begin
  Vec2d := vlua_tovec2d( L, 1 );
  vlua_pushvec2d( L, NewVec2D( -Vec2d.x, -Vec2d.y ) );
  Exit(1);
end;

function lua_vec2d_add( L: Plua_State): Integer; cdecl;
begin
  vlua_pushvec2d( L, vlua_tovec2d( L, 1 ) + vlua_tovec2d( L, 2 ) );
  Exit(1);
end;

function lua_vec2d_sub( L: Plua_State): Integer; cdecl;
begin
  vlua_pushvec2d( L, vlua_tovec2d( L, 1 ) - vlua_tovec2d( L, 2 ) );
  Exit(1);
end;

function lua_vec2d_mul( L: Plua_State): Integer; cdecl;
begin
  if vlua_isvec2d( L, 1 ) then
    if vlua_isvec2d( L, 2 ) then
      vlua_pushvec2d( L, vlua_tovec2d( L, 1 ) * vlua_tovec2d( L, 2 ) )
    else
      vlua_pushvec2d( L, vlua_tovec2d( L, 1 ).Scaled( lua_tonumber( L, 2 ) ) )
  else
    vlua_pushvec2d( L, vlua_tovec2d( L, 2 ).Scaled( lua_tonumber( L, 1 ) ) );
  Exit(1);
end;

function lua_vec2d_eq( L: Plua_State): Integer; cdecl;
var a, b : TVec2d;
begin
  a := vlua_tovec2d( L, 1 );
  b := vlua_tovec2d( L, 2 );
  lua_pushboolean( L, (a.x = b.x) and (a.y = b.y) );
  Exit(1);
end;

function lua_vec2d_normalize( L: Plua_State): Integer; cdecl;
var Vec2d : PVec2d;
    len : Single;
begin
  Vec2d := vlua_topvec2d( L, 1 );
  len := Vec2d^.Length();
  Vec2d^.X := Vec2d^.X / len;
  Vec2d^.Y := Vec2d^.Y / len;
  Exit(0);
end;

function lua_vec2d_normalized( L: Plua_State): Integer; cdecl;
var Vec2d : TVec2d;
    len : Single;
begin
  Vec2d := vlua_tovec2d( L, 1 );
  len := Vec2d.Length();
  Vec2d.X := Vec2d.X / len;
  Vec2d.Y := Vec2d.Y / len;
  vlua_pushvec2d( L, Vec2d );
  Exit(1);
end;

function lua_vec2d_tostring( L: Plua_State): Integer; cdecl;
var Vec2d : TVec2d;
begin
  Vec2d := vlua_tovec2d( L, 1 );
  lua_pushansistring( L, FloatToStr(Vec2d.x) + ',' + FloatToStr(Vec2d.y) );
  Exit(1);
end;

function lua_vec2d_index( L: Plua_State ): Integer; cdecl;
var VecPtr : PVec2d;
    Index  : AnsiString;
begin
  VecPtr := vlua_topvec2d( L, 1 );
  Index  := lua_tostring( L, 2 );
       if Index = 'x' then lua_pushnumber( L, VecPtr^.x )
  else if Index = 'y' then lua_pushnumber( L, VecPtr^.y )
  else if Index = 'length' then lua_pushnumber( L, VecPtr^.Length() )
  else if Index = 'length_squared' then lua_pushnumber( L, VecPtr^.LengthSq() )
  else
    begin
      lua_getglobal( L, 'vec2d' );
      lua_pushvalue( L, -2 );
      lua_rawget( L, -2 );
    end;
  Exit(1);
end;

function lua_vec2d_newindex( L: Plua_State ): Integer; cdecl;
var VecPtr : PVec2d;
    Index  : AnsiString;
    Value  : Single;
begin
  VecPtr := vlua_topvec2d( L, 1 );
  Index  := lua_tostring( L, 2 );
  Value  := lua_tonumber_def( L, 3, 0 );
       if Index = 'x' then VecPtr^.x := Value
  else if Index = 'y' then VecPtr^.y := Value;
  Exit(0);
end;

// -------- Table functions ------------------------------------------- //

function lua_table_copy( L: Plua_State ): Integer; cdecl;
begin
  luaL_checkany( L, 1 );
  if lua_type( L, -1 ) <> LUA_TTABLE then Exit( 1 );
  lua_settop( L, 1 );
  lua_newtable(L);
  lua_pushnil(L);
  while ( lua_next( L, 1 ) <> 0 ) do
  begin
    lua_pushvalue(L, -2);
    lua_insert(L, -2);
    lua_settable(L, -4);
  end;
  Exit( 1 );
end;

function lua_table_icopy( L: Plua_State ): Integer; cdecl;
var i : Integer;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  if lua_type( L, -1 ) <> LUA_TTABLE then Exit( 1 );
  lua_settop( L, 1 );
  lua_newtable(L);
  i := 0;
  while ( true ) do
  begin
    Inc( i );
    lua_rawgeti(L, 1, i);
    if ( lua_isnil( L, -1 ) ) then
    begin
      lua_pop( L, 1 );
      break;
    end;
    lua_rawseti(L, 2, i);
  end;
  Exit( 1 );
end;

function lua_table_merge( L: Plua_State ): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  luaL_checktype( L, 2, LUA_TTABLE );
  lua_settop( L, 2 );
  lua_pushnil(L);
  while ( lua_next( L, 2 ) <> 0 ) do
  begin
    lua_pushvalue(L, -2);
    lua_insert(L, -2);
    lua_settable(L, 1);
  end;
  Exit( 0 );
end;

function lua_table_imerge( L: Plua_State ): Integer; cdecl;
var i : Integer;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  luaL_checktype( L, 2, LUA_TTABLE );
  lua_settop( L, 2 );
  i := 0;
  while ( true ) do
  begin
    Inc( i );
    lua_rawgeti(L, 2, i);
    if ( lua_isnil( L, -1 ) ) then
    begin
      lua_pop( L, 1 );
      break;
    end;
    lua_rawseti(L, 1, i);
  end;
  Exit( 0 );
end;

function lua_table_reversed( L: Plua_State ): Integer; cdecl;
var i, len : Integer;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_settop( L, 1 );
  len := lua_objlen(L,1);
  i   := len;
  lua_createtable(L,len,0);
  while ( i <> 0 ) do
  begin
    lua_rawgeti(L, 1, i);
    lua_rawseti(L, 2, len-i+1);
    Dec( i );
  end;
  Exit( 1 );
end;

function lua_table_toset( L: Plua_State ): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_settop( L, 1 );
  vlua_table_toset( L, 1 );
  Exit( 1 );
end;

function lua_table_tokeyset( L: Plua_State ): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_settop( L, 1 );
  vlua_table_tokeyset( L, 1 );
  Exit( 1 );
end;

function lua_table_random_pick( L: Plua_State ): Integer; cdecl;
var i : Integer;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  i := lua_objlen( L, 1 );
  if i = 0 then Exit( 0 );
  lua_rawgeti( L, 1, Random( i ) + 1 );
  Exit( 1 );
end;

function lua_table_transform( L: Plua_State ): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  luaL_checktype( L, 2, LUA_TFUNCTION );
  lua_settop( L, 2 );
  lua_pushnil(L);
  while ( lua_next( L, 1 ) <> 0 ) do
  begin
    // Key 3, Value 4
    lua_pushvalue(L, 3); // 3Key, 4Value, 5Key
    lua_insert(L,-2);    // 3Key, 4Key, 5Value
    lua_pushvalue(L, 2); // 3Key, 4Key, 5Value, 6Func
    lua_insert(L,-2);    // 3Key, 4Key, 5Func, 6Value
    lua_call(L, 1, 1);   // 3Key, 4Key, 5Result
    lua_rawset(L, 1);    // 3Key
  end;
  Exit( 0 );
end;

function lua_table_shuffle( L: Plua_State ): Integer; cdecl;
var n,k : Integer;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_settop( L, 1 );
  n := lua_objlen( L, 1 );

	while n >= 2 do
  begin
		k := Random(n)+1;
    if k <> n then
    begin
      lua_rawgeti( L, 1, n );
      lua_rawgeti( L, 1, k );
      lua_rawseti( L, 1, n );
      lua_rawseti( L, 1, k );
    end;
    Dec(n);
	end;

  Exit( 1 );
end;



// -------- Math functions -------------------------------------------- //

function lua_math_clamp( L: Plua_State ): Integer; cdecl;
var v,vmin,vmax : Double;
begin
  v    := luaL_checknumber(L, 1);
  vmin := luaL_optnumber(L,2,0);
  vmax := luaL_optnumber(L,3,1);
  if vmin > vmax then luaL_argerror( L, 2, 'min is larger than max!');
  if v < vmin then
    lua_pushvalue( L, 2 )
  else if v > vmax then
    lua_pushvalue( L, 3 )
  else
    lua_pushvalue( L, 1 );
  Result := 1;
end;

function lua_math_dice( L: Plua_State ): Integer; cdecl;
var dice, sides, count, res : LongInt;
begin
  dice  := luaL_checkint(L, 1);
  sides := luaL_checkint(L, 2);
  res   := 0;
  if (dice > 0) and (sides > 0) then
  for count := 1 to dice do
    res += Random( sides ) + 1;
  lua_pushnumber( L, res );
  Result := 1;
end;

// -------- UID functions --------------------------------------------- //

function lua_uid_count( L: Plua_State ): Integer; cdecl;
begin
  lua_pushnumber( L, UIDs.Size );
  Result := 1;
end;

function lua_uid_get( L: Plua_State ): Integer; cdecl;
begin
  vlua_pushanyobject( L, UIDs.Get( lua_tointeger( L, 1 ) ) );
  Result := 1;
end;

function lua_uid_exists( L: Plua_State ): Integer; cdecl;
begin
  lua_pushboolean( L, UIDs.Get( lua_tointeger( L, 1 ) ) <> nil );
  Result := 1;
end;

// -------- Weight table functions ------------------------------------ //

procedure lua_weight_table_raw_insert( L: Plua_State; IndexTable, IndexElement, Weight : Integer );
var iIndex  : Integer;
begin
  if Weight <= 0 then Exit;
  IndexTable   := lua_absindex( L, IndexTable );
  IndexElement := lua_absindex( L, IndexElement );

  lua_getfield( L, IndexTable, '_elements' );
  iIndex := lua_objlen( L, -1 ) + 1;
  lua_pushvalue( L, IndexElement );
  lua_rawseti( L, -2, iIndex );
  lua_pop( L, 1 );

  lua_getfield( L, IndexTable, '_weights' );
  lua_pushinteger( L, Weight );
  lua_rawseti( L, -2, iIndex );
  lua_pop( L, 1 );

  lua_getfield( L, IndexTable, '_weight' );
  lua_pushinteger( L, Weight + lua_tointeger( L, -1 ) );
  lua_setfield( L, IndexTable, '_weight' );
  lua_pop( L, 1 );
end;

function lua_weight_table_impl_add( L: Plua_State ): Integer; cdecl;
var iWeight : Integer;
begin
  iWeight := 1;
  lua_settop( L, 3 );
  luaL_checktype( L, 1, LUA_TTABLE );
  if lua_type( L, 2 ) = LUA_TNUMBER then
    lua_replace( L, 2 );
  if lua_isnoneornil( L, 3 ) then
  begin
    if lua_istable( L, 2 ) then
    begin
      lua_pushliteral( L, 'weight' );
      lua_rawget( L, 2 );
      if lua_type( L, -1 ) = LUA_TNUMBER then
        iWeight := lua_tointeger( L, -1 );
      lua_pop( L, 1 );
    end;
  end
  else
  begin
    luaL_checktype( L, 3, LUA_TNUMBER );
    iWeight := lua_tointeger( L, 3 );
  end;
  if lua_isnoneornil( L, 2 ) then
    luaL_argerror( L, 2, 'element expected' );

  lua_weight_table_raw_insert( L, 1, 2, iWeight );
  Result := 0;
end;

function lua_weight_table_impl_size( L: Plua_State ): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_getfield( L, 1, '_elements' );
  lua_pushinteger( L, lua_objlen( L, -1 ) );
  Result := 1;
end;

function lua_weight_table_impl_roll( L: Plua_State ): Integer; cdecl;
var iRoll  : Integer;
    iIndex : Integer;
    iCount : Integer;
    iSize  : Integer;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_settop( L, 1 );
  iIndex := 1;
  iCount := 0;

  lua_getfield( L, 1, '_elements' );
  iSize := lua_objlen( L, -1 );
  if iSize = 0 then Exit( 0 );
  lua_getfield( L, 1, '_weight' );
  iRoll := Random( lua_tointeger( L, -1 ) );

  lua_settop( L, 1 );

  lua_getfield( L, 1, '_weights' ); // Index #2
  while iIndex < iSize do
  begin
    lua_rawgeti( L, 2, iIndex );
    iCount += lua_tointeger( L, -1 );
    lua_pop( L, 1 );
    if iCount > iRoll then break;
    Inc( iIndex );
  end;

  lua_getfield( L, 1, '_elements' );
  lua_rawgeti( L, -1, iIndex );
  Result := 1;
end;

function lua_weight_table_new( L: Plua_State ): Integer; cdecl;
var iLength : Integer;
    iWeight : Integer;
begin
  iLength := 0;
  if not lua_isnoneornil( L, 1 ) then
  begin
     luaL_checktype( L, 1, LUA_TTABLE );
     iLength := lua_objlen( L, 1 );
  end;
  lua_settop( L, 1 );

  lua_createtable( L, 0, 6 );
    lua_pushnumber( L, 0 );
    lua_setfield( L, 2, '_weight' );
    lua_createtable( L, iLength, 0 );
    lua_setfield( L, 2, '_weights' );
    lua_createtable( L, iLength, 0 );
    lua_setfield( L, 2, '_elements' );

    lua_pushcfunction( L, @lua_weight_table_impl_add );
    lua_setfield( L, 2, 'add' );
    lua_pushcfunction( L, @lua_weight_table_impl_size );
    lua_setfield( L, 2, 'size' );
    lua_pushcfunction( L, @lua_weight_table_impl_roll );
    lua_setfield( L, 2, 'roll' );

    if not lua_isnoneornil( L, 1 ) then
    begin
      lua_pushnil(L);
      while lua_next( L, 1 ) <> 0 do
      begin
        iWeight := 1;
        if lua_type( L, -2 ) = LUA_TNUMBER then
        begin
          if lua_istable( L, -1 ) then
          begin
            lua_pushliteral( L, 'weight' );
            lua_rawget( L, -2 );
            if lua_type( L, -1 ) = LUA_TNUMBER then iWeight := lua_tointeger( L, -1 );
            lua_pop( L, 1 );
          end;
          lua_weight_table_raw_insert( L, 2, -1, iWeight );
        end
        else
        begin
          if lua_type( L, -1 ) = LUA_TNUMBER then iWeight := lua_tointeger( L, -1 );
          lua_weight_table_raw_insert( L, 2, -2, iWeight );
        end;
        lua_pop( L, 1 );
      end;
    end;

  Result := 1;
end;

// -------- String list functions ------------------------------------ //

function lua_string_list_impl_add( L: Plua_State ): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  luaL_checktype( L, 2, LUA_TSTRING );
  lua_rawseti( L, 1, lua_objlen( L, 1 ) + 1 );
  Result := 0;
end;

function lua_string_list_impl_size( L: Plua_State ): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_pushinteger( L, lua_objlen( L, 1 ) );
  Result := 1;
end;

function lua_string_list_new( L: Plua_State ): Integer; cdecl;
begin
  lua_settop( L, 1 );
  if lua_isnoneornil( L, 1 )
    then lua_createtable( L, 0, 3 )
    else luaL_checktype( L, 1, LUA_TTABLE );
  lua_pushcfunction( L, @lua_string_list_impl_add );
  lua_setfield( L, 1, 'add' );
  lua_pushcfunction( L, @lua_string_list_impl_size );
  lua_setfield( L, 1, 'size' );
  Result := 1;
end;

// -------- Kills table functions ------------------------------------ //

var GKills : TKillTable;

function lua_kills_get( L: Plua_State ): Integer; cdecl;
begin
  if lua_type( L, 2 ) = LUA_TSTRING
    then lua_pushinteger( L, GKills.Get( lua_tostring( L, 1 ), lua_tostring( L, 2 ) ) )
    else lua_pushinteger( L, GKills.Get( lua_tostring( L, 1 ) ) );
  Result := 1;
end;

function lua_kills_get_type( L: Plua_State ): Integer; cdecl;
begin
  lua_pushinteger( L, GKills.GetType( lua_tostring( L, 1 ) ) );
  Result := 1;
end;

function lua_kills_index( L: Plua_State ): Integer; cdecl;
var PPoint : vutil.PPoint;
    Index  : AnsiString;
begin
  Index  := lua_tostring( L, 2 );
       if Index = 'count'                   then lua_pushinteger( L, GKills.Count )
  else if Index = 'max_count'               then lua_pushinteger( L, GKills.MaxCount )
  else if Index = 'this_turn'               then lua_pushinteger( L, GKills.ThisTurn )
  else if Index = 'best_turn'               then lua_pushinteger( L, GKills.BestTurn )
  else if Index = 'cur_sequence'            then lua_pushinteger( L, GKills.CurrentSequence )
  else if Index = 'best_sequence'           then lua_pushinteger( L, GKills.BestSequence )
  else if Index = 'best_sequence_length'    then lua_pushinteger( L, GKills.BestSequenceLength )
  else if Index = 'no_damage_sequence'      then lua_pushinteger( L, GKills.NoDamageSequence )
  else if Index = 'best_no_damage_sequence' then lua_pushinteger( L, GKills.BestNoDamageSequence )
  else luaL_error( L,'Unknown kills table field: %s', PChar(Index) );
  Exit(1);
end;

// -------- Stats table functions ------------------------------------ //

var GStatistics : TStatistics;

function lua_stats_get( L: Plua_State ): Integer; cdecl;
begin
  lua_pushinteger( L, GStatistics.GetValue( lua_tostring( L, 1 ) ) );
  Result := 1;
end;

function lua_stats_set( L: Plua_State ): Integer; cdecl;
begin
  GStatistics.SetValue( lua_tostring( L, 1 ), lua_tointeger( L, 2 ) );
  Result := 0;
end;

function lua_stats_inc( L: Plua_State ): Integer; cdecl;
begin
  GStatistics.Inc( lua_tostring( L, 1 ), luaL_optint( L, 2, 1 ) );
  Result := 0;
end;

function lua_stats_max( L: Plua_State ): Integer; cdecl;
begin
  GStatistics.Max( lua_tostring( L, 1 ), lua_tointeger( L, 2 ) );
  Result := 0;
end;

function lua_stats_index( L: Plua_State ): Integer; cdecl;
begin
  lua_pushinteger( L, GStatistics.GetValue( lua_tostring( L, 2 ) ) );
  Result := 1;
end;

function lua_stats_newindex( L: Plua_State ): Integer; cdecl;
begin
  GStatistics.SetValue( lua_tostring( L, 2 ), lua_tointeger( L, 3 ) );
  Result := 0;
end;

// -------- Registration tables and functions ------------------------- //

const coordlib_f : array[0..13] of luaL_Reg = (
  ( name : 'new';            func : @lua_coord_new; ),
  ( name : 'get';            func : @lua_coord_get; ),
  ( name : 'abs';            func : @lua_coord_abs; ),
  ( name : 'sign';           func : @lua_coord_sign; ),
  ( name : 'clone';          func : @lua_coord_clone; ),
  ( name : 'distance';       func : @lua_coord_distance; ),
  ( name : 'real_distance';  func : @lua_coord_real_distance; ),
  ( name : 'random';         func : @lua_coord_random; ),
  ( name : 'random_shift';   func : @lua_coord_random_shift; ),
  ( name : 'random_shifted'; func : @lua_coord_random_shifted; ),
  ( name : 'tostring';       func : @lua_coord_tostring; ),
  ( name : 'cross_coords';   func : @lua_coord_cross_coords; ),
  ( name : 'around_coords';  func : @lua_coord_around_coords; ),
  ( name : nil;              func : nil; )
  );

const coordlib_m : array[0..8] of luaL_Reg = (
  ( name : '__add';      func : @lua_coord_add; ),
  ( name : '__sub';      func : @lua_coord_sub; ),
  ( name : '__unm';      func : @lua_coord_unm; ),
  ( name : '__mul';      func : @lua_coord_mul; ),
  ( name : '__eq';       func : @lua_coord_eq; ),
  ( name : '__index';    func : @lua_coord_index; ),
  ( name : '__newindex'; func : @lua_coord_newindex; ),
  ( name : '__tostring'; func : @lua_coord_tostring; ),
  ( name : nil;          func : nil; )
  );

const arealib_f : array[0..25] of luaL_Reg = (
  ( name : 'new';            func : @lua_area_new; ),
  ( name : 'get';            func : @lua_area_get; ),
  ( name : 'clone';          func : @lua_area_clone; ),
  ( name : 'tostring';       func : @lua_area_tostring; ),
  ( name : 'shrink';         func : @lua_area_shrink; ),
  ( name : 'shrinked';       func : @lua_area_shrinked; ),
  ( name : 'expand';         func : @lua_area_expand; ),
  ( name : 'expanded';       func : @lua_area_expanded; ),
  ( name : 'clamp';          func : @lua_area_clamp; ),
  ( name : 'clamped';        func : @lua_area_clamped; ),
  ( name : 'fix';            func : @lua_area_fix; ),
  ( name : 'proper';         func : @lua_area_proper; ),
  ( name : 'dim';            func : @lua_area_dim; ),
  ( name : 'size';           func : @lua_area_size; ),
  ( name : 'around';         func : @lua_area_around; ),
  ( name : 'clamp_coord';    func : @lua_area_clamp_coord; ),
  ( name : 'contains';       func : @lua_area_contains; ),
  ( name : 'is_edge';        func : @lua_area_is_edge; ),

  ( name : 'coords';         func : @lua_area_coords; ),
  ( name : 'edges';          func : @lua_area_edges; ),
  ( name : 'corners';        func : @lua_area_corners; ),

  ( name : 'random_subarea';          func : @lua_area_random_subarea; ),
  ( name : 'random_coord';            func : @lua_area_random_coord; ),
  ( name : 'random_edge_coord';       func : @lua_area_random_edge_coord; ),
  ( name : 'random_inner_edge_coord'; func : @lua_area_random_inner_edge_coord; ),

  ( name : nil;              func : nil; )
  );

const arealib_m : array[0..5] of luaL_Reg = (
  ( name : '__eq';       func : @lua_area_eq; ),
  ( name : '__index';    func : @lua_area_index; ),
  ( name : '__newindex'; func : @lua_area_newindex; ),
  ( name : '__tostring'; func : @lua_area_tostring; ),
  ( name : '__call';     func : @lua_area_coords; ),
  ( name : nil;          func : nil; )
  );

const pointlib_f : array[0..4] of luaL_Reg = (
  ( name : 'new';            func : @lua_point_new; ),
  ( name : 'get';            func : @lua_point_get; ),
  ( name : 'clone';          func : @lua_point_clone; ),
  ( name : 'tostring';       func : @lua_point_tostring; ),
  ( name : nil;              func : nil; )
  );

const pointlib_m : array[0..9] of luaL_Reg = (
  ( name : '__add';      func : @lua_point_add; ),
  ( name : '__sub';      func : @lua_point_sub; ),
  ( name : '__unm';      func : @lua_point_unm; ),
  ( name : '__mul';      func : @lua_point_mul; ),
  ( name : '__eq';       func : @lua_point_eq; ),
  ( name : '__call';     func : @lua_point_new; ),
  ( name : '__index';    func : @lua_point_index; ),
  ( name : '__newindex'; func : @lua_point_newindex; ),
  ( name : '__tostring'; func : @lua_point_tostring; ),
  ( name : nil;          func : nil; )
  );

const rectlib_f : array[0..9] of luaL_Reg = (
  ( name : 'new';            func : @lua_rect_new; ),
  ( name : 'get';            func : @lua_rect_get; ),
  ( name : 'clone';          func : @lua_rect_clone; ),
  ( name : 'tostring';       func : @lua_rect_tostring; ),
  ( name : 'shrink';         func : @lua_rect_shrink; ),
  ( name : 'shrinked';       func : @lua_rect_shrinked; ),
  ( name : 'expand';         func : @lua_rect_expand; ),
  ( name : 'expanded';       func : @lua_rect_expanded; ),
  ( name : 'contains';       func : @lua_rect_contains; ),
  ( name : nil;              func : nil; )
  );

const rectlib_m : array[0..7] of luaL_Reg = (
  ( name : '__eq';       func : @lua_rect_eq; ),
  ( name : '__index';    func : @lua_rect_index; ),
  ( name : '__newindex'; func : @lua_rect_newindex; ),
  ( name : '__tostring'; func : @lua_rect_tostring; ),
  ( name : '__add';      func : @lua_rect_add; ),
  ( name : '__call';     func : @lua_rect_new; ),
  ( name : '__sub';      func : @lua_rect_sub; ),
  ( name : nil;          func : nil; )
  );

const vec2dlib_f : array[0..4] of luaL_Reg = (
  ( name : 'new';            func : @lua_vec2d_new; ),
  ( name : 'normalize';      func : @lua_vec2d_normalize; ),
  ( name : 'normalized';     func : @lua_vec2d_normalized; ),
  ( name : 'tostring';       func : @lua_vec2d_tostring; ),
  ( name : nil;              func : nil; )
  );

const vec2dlib_m : array[0..8] of luaL_Reg = (
  ( name : '__add';      func : @lua_vec2d_add; ),
  ( name : '__sub';      func : @lua_vec2d_sub; ),
  ( name : '__unm';      func : @lua_vec2d_unm; ),
  ( name : '__mul';      func : @lua_vec2d_mul; ),
  ( name : '__eq';       func : @lua_vec2d_eq; ),
  ( name : '__index';    func : @lua_vec2d_index; ),
  ( name : '__newindex'; func : @lua_vec2d_newindex; ),
  ( name : '__tostring'; func : @lua_vec2d_tostring; ),
  ( name : nil;          func : nil; )
  );

const tableauxlib_f : array[0..10] of luaL_Reg = (
  ( name : 'copy';        func : @lua_table_copy ),
  ( name : 'icopy';       func : @lua_table_icopy ),
  ( name : 'merge';       func : @lua_table_merge ),
  ( name : 'imerge';      func : @lua_table_imerge ),
  ( name : 'reversed';    func : @lua_table_reversed ),
  ( name : 'toset';       func : @lua_table_toset ),
  ( name : 'tokeyset';    func : @lua_table_tokeyset ),
  ( name : 'random_pick'; func : @lua_table_random_pick ),
  ( name : 'transform';   func : @lua_table_transform ),
  ( name : 'shuffle';     func : @lua_table_shuffle ),
  ( name : nil;           func : nil; )
  );

const mathauxlib_f : array[0..2] of luaL_Reg = (
  ( name : 'clamp';     func : @lua_math_clamp ),
  ( name : 'dice';      func : @lua_math_dice ),
  ( name : nil;         func : nil; )
  );

const uidlib_f : array[0..3] of luaL_Reg = (
  ( name : 'count';     func : @lua_uid_count ),
  ( name : 'get';       func : @lua_uid_get ),
  ( name : 'exists';    func : @lua_uid_exists ),
  ( name : nil;         func : nil; )
  );

const weight_tablelib_f : array[0..1] of luaL_Reg = (
  ( name : 'new';       func : @lua_weight_table_new ),
  ( name : nil;         func : nil; )
  );

const string_listlib_f : array[0..1] of luaL_Reg = (
  ( name : 'new';       func : @lua_string_list_new ),
  ( name : nil;         func : nil; )
  );

const killslib_f : array[0..2] of luaL_Reg = (
  ( name : 'get';       func : @lua_kills_get ),
  ( name : 'get_type';  func : @lua_kills_get_type ),
  ( name : nil;         func : nil; )
  );

const killslib_m : array[0..1] of luaL_Reg = (
  ( name : '__index';    func : @lua_kills_index ),
//  ( name : '__call';     func : @lua_kills_call ),
  ( name : nil;          func : nil; )
  );

const statslib_f : array[0..4] of luaL_Reg = (
  ( name : 'get';    func : @lua_stats_get ),
  ( name : 'set';    func : @lua_stats_set ),
  ( name : 'inc';    func : @lua_stats_inc ),
  ( name : 'max';    func : @lua_stats_max ),
  ( name : nil;      func : nil; )
  );

const statslib_m : array[0..2] of luaL_Reg = (
  ( name : '__index';    func : @lua_stats_index ),
  ( name : '__newindex'; func : @lua_stats_newindex ),
  ( name : nil;          func : nil; )
  );

procedure RegisterTableAuxFunctions(L: Plua_State);
begin
  luaL_register( L, 'table', tableauxlib_f );
  lua_pop( L, 1 );
end;

procedure RegisterMathAuxFunctions ( L : Plua_State ) ;
begin
  luaL_register( L, 'math', mathauxlib_f );
  lua_pop( L, 1 );
end;

procedure RegisterUIDClass( L: Plua_State; const Name : AnsiString = 'uids' );
begin
  luaL_register( L, PChar(Name), uidlib_f );
  lua_pop( L, 1 );
end;

procedure RegisterWeightTableClass(L: Plua_State; const Name: AnsiString);
begin
  luaL_register( L, PChar(Name), weight_tablelib_f );
  lua_pop( L, 1 );
end;

procedure RegisterStringListClass(L: Plua_State; const Name: AnsiString);
begin
  luaL_register( L, PChar(Name), string_listlib_f );
  lua_pop( L, 1 );
end;

procedure RegisterCoordClass( L: Plua_State );
begin
  luaL_newmetatable( L, VALKYRIE_COORD );
  luaL_register( L, nil, coordlib_m );
  luaL_register( L, 'coord', coordlib_f );

  vlua_pushcoord( L, ZeroCoord2D );
  lua_setfield( L, -2, 'ZERO' );

  vlua_pushcoord( L, UnitCoord2D );
  lua_setfield( L, -2, 'UNIT' );
  lua_pop( L, 2 );
end;

procedure RegisterAreaClass( L: Plua_State );
begin
  luaL_newmetatable( L, VALKYRIE_AREA );
  luaL_register( L, nil, arealib_m );
  luaL_register( L, 'area', arealib_f );
  lua_pop( L, 2 );
end;

procedure RegisterAreaFull(L: Plua_State; Area: TArea);
begin
  lua_getglobal( L, 'area' );
  vlua_pusharea( L, Area );
  lua_setfield( L, -2, 'FULL' );
  vlua_pusharea( L, Area.Shrinked() );
  lua_setfield( L, -2, 'FULL_SHRINKED' );
  lua_pop( L, 1 );
end;

procedure RegisterPointClass(L: Plua_State);
begin
  luaL_newmetatable( L, VALKYRIE_POINT );
  luaL_register( L, nil, pointlib_m );
  luaL_register( L, 'point', pointlib_f );

  vlua_pushpoint( L, PointZero );
  lua_setfield( L, -2, 'ZERO' );

  vlua_pushpoint( L, PointUnit );
  lua_setfield( L, -2, 'UNIT' );
  lua_pop( L, 2 );
end;

procedure RegisterRectClass(L: Plua_State);
begin
  luaL_newmetatable( L, VALKYRIE_RECT );
  luaL_register( L, nil, rectlib_m );
  luaL_register( L, 'rect', rectlib_f );
  lua_pop( L, 2 );
end;

procedure RegisterVec2dClass( L: Plua_State );
begin
  luaL_newmetatable( L, VALKYRIE_VEC2D );
  luaL_register( L, nil, vec2dlib_m );
  luaL_register( L, 'vec2d', vec2dlib_f );

  vlua_pushvec2d( L, TVec2d.Create() );
  lua_setfield( L, -2, 'ZERO' );
  vlua_pushvec2d( L, TVec2d.Create(1) );
  lua_setfield( L, -2, 'X_AXIS' );
  vlua_pushvec2d( L, TVec2d.Create(0,1) );
  lua_setfield( L, -2, 'Y_AXIS' );

  lua_pop( L, 2 );
end;

procedure RegisterKillsClass(L: Plua_State; aKills: TKillTable; const aName : AnsiString = 'kills' );
begin
  GKills := aKills;
  lua_pushansistring( L, aName );
  lua_createtable( L, 0, 2 );
  luaL_register( L, nil, killslib_f );
  lua_createtable( L, 0, 2 );
  luaL_register( L, nil, killslib_m );
  lua_setmetatable( L, -2 );
  lua_rawset_global( L );
end;

procedure RegisterStatisticsClass(L: Plua_State; aStatistics: TStatistics; const aName: AnsiString);
begin
  GStatistics := aStatistics;
  lua_pushansistring( L, aName );
  lua_createtable( L, 0, 2 );
  luaL_register( L, nil, statslib_f );
  lua_createtable( L, 0, 2 );
  luaL_register( L, nil, statslib_m );
  lua_setmetatable( L, -2 );
  lua_rawset_global( L );
end;


end.

