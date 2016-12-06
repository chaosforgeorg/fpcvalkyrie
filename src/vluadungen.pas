unit vluadungen;

{$mode objfpc}
interface

uses Classes, SysUtils, vlualibrary, vrltools, vdungen;

procedure RegisterDungenClass( L : Plua_State; ObjectName : AnsiString = '' );
procedure RegisterDungen( DunGen : TDungeonBuilder );

implementation

uses vutil, vmaparea, vluatools, vluaext, strutils;

var
  Gen : TDungeonBuilder;

const
  VALKYRIE_DUNGEN      = 'valkyrie.dungen';
  VALKYRIE_DUNGEN_TILE = 'valkyrie.dungen.tile';

function lua_tointeger_def( L : Plua_State; Index : Integer; DValue : Integer ) : Integer;
begin
  if lua_type( L, Index ) = LUA_TNUMBER then
    Exit( lua_tointeger( L, Index ) )
  else
    Exit( DValue );
end;

function lua_toflags32( L : Plua_State; Index : Integer ) : TFlags32;
begin
  lua_toflags32 := [];
  if lua_istable( L, Index ) then
  begin
    lua_pushnil( L );
    while lua_next( L, Index ) <> 0 do
    begin
      if lua_isnumber( L, -1 ) then
        Include( lua_toflags32, lua_tointeger( L, -1 ) );
      lua_pop( L, 1 );
    end;
  end;
end;

// not used in set_cell on purpose!
function lua_tocell( L : Plua_State; Index : Integer ) : Byte;
begin
  if lua_type( L, Index ) = LUA_TSTRING then
    Exit( Gen.IDtoCell( lua_tostring( L, Index ) ) )
  else
    Exit( lua_tointeger( L, Index ) );
end;

function lua_tocellarray( L : Plua_State; idx : Integer ) : TOpenByteArray;
var
  cnt : Word;
begin
  idx := lua_absindex( L, idx );
  lua_pushnil( L );
  cnt := 0;
  while lua_next( L, idx ) <> 0 do
  begin
    SetLength( lua_tocellarray, cnt + 1 );
    lua_tocellarray[cnt] := lua_tocell( l, -1 );
    lua_pop( L, 1 );
    Inc( cnt );
  end;
end;

function lua_tocellset( L : Plua_State; Index : Integer ) : TFlags;
begin
  lua_tocellset := [];

  case lua_type( L, Index ) of
    LUA_TTABLE :
    begin
      lua_pushnil( L );
      while lua_next( L, Index ) <> 0 do
      begin
        if lua_type( L, -1 ) = LUA_TSTRING then
          Include( lua_tocellset, Gen.IDtoCell( lua_tostring( L, -1 ) ) )
        else
          Include( lua_tocellset, lua_tointeger( L, -1 ) );
        lua_pop( L, 1 );
      end;
    end;
    LUA_TSTRING : Include( lua_tocellset, Gen.IDtoCell( lua_tostring( L, Index ) ) );
    LUA_TNUMBER : Include( lua_tocellset, lua_tointeger( L, Index ) );
  end;
end;


function lua_dungen_get_cell_id( L : Plua_State ) : Integer; cdecl;
var
  Coord : PCoord2D;
begin
  Coord := vlua_topcoord( L, 1 );
  lua_pushansistring( L, Gen.CellToID( Gen.GetCell( Coord^ ) ) );
  Exit( 1 );
end;

function lua_dungen_get_cell( L : Plua_State ) : Integer; cdecl;
var
  Coord : PCoord2D;
begin
  Coord := vlua_topcoord( L, 1 );
  lua_pushinteger( L, Gen.GetCell( Coord^ ) );
  Exit( 1 );
end;

function lua_dungen_set_cell( L : Plua_State ) : Integer; cdecl;
var
  Coord : PCoord2D;
  LType : Integer;
begin
  Coord := vlua_topcoord( L, 1 );
  LType := lua_type( L, 2 );

  if LType = LUA_TSTRING then
    Gen.PutCell( Coord^, Gen.IDtoCell( lua_tostring( L, 2 ) ) )
  else
    Gen.PutCell( Coord^, lua_tointeger( L, 2 ) );

  Exit( 0 );
end;

function lua_dungen_fast_get_cell( L : Plua_State ) : Integer; cdecl;
begin
  lua_pushinteger( L, Gen.GetCell( NewCoord2D( lua_tointeger( L, 1 ), lua_tointeger( L, 2 ) ) ) );
  Exit( 1 );
end;

function lua_dungen_fast_set_cell( L : Plua_State ) : Integer; cdecl;
begin
  Gen.PutCell( NewCoord2D( lua_tointeger( L, 1 ), lua_tointeger( L, 2 ) ), lua_tointeger( L, 3 ) );
  Exit( 0 );
end;


function lua_dungen_is_empty( L : Plua_State ) : Integer; cdecl;
var
  Coord : PCoord2D;
begin
  Coord := vlua_topcoord( L, 1 );
  lua_pushboolean( L, Gen.isEmpty( Coord^, lua_toflags32( L, 2 ) ) );
  Exit( 1 );
end;

function lua_dungen_is_empty_area( L : Plua_State ) : Integer; cdecl;
var
  Area :  TArea;
  Flags : TFlags32;
  Coord : TCoord2D;
begin
  Area := vlua_toarea( L, 1 );
  Flags := lua_toflags32( L, 2 );
  for Coord in Area do
    if not Gen.isEmpty( Coord, Flags ) then
    begin
      lua_pushboolean( L, False );
      Exit( 1 );
    end;
  lua_pushboolean( L, True );
  Exit( 1 );
end;

function lua_dungen_fill( L : Plua_State ) : Integer; cdecl;
var
  Fill : Byte;
begin
  Fill := lua_tocell( L, 1 );
  if vlua_isarea( L, 2 ) then
    Gen.Fill( vlua_toarea( L, 2 ), Fill )
  else
    Gen.Fill( Fill );
  Exit( 0 );
end;

function lua_dungen_fill_pattern( L : Plua_State ) : Integer; cdecl;
var
  Area :  TArea;
  Horiz : Boolean;
begin
  Area := vlua_toarea( L, 1 );
  Horiz := lua_toboolean( L, 2 );
  if lua_type( L, 4 ) = LUA_TTABLE then
    Gen.Fill( Area, lua_tocellarray( L, 3 ), lua_tocellarray( L, 4 ), Horiz )
  else
    Gen.Fill( Area, lua_tocellarray( L, 3 ), Horiz );
  Exit( 0 );
end;


function lua_dungen_fill_edges( L : Plua_State ) : Integer; cdecl;
begin
  Gen.FillEdges( lua_tocell( L, 1 ) );
  Exit( 0 );
end;

function lua_dungen_transmute( L : Plua_State ) : Integer; cdecl;
var
  From : TFlags;
  Too :  Byte;
begin
  From := lua_tocellset( L, 1 );
  Too := lua_tocell( L, 2 );
  if vlua_isarea( L, 3 ) then
    Gen.Transmute( vlua_toarea( L, 3 ), From, Too )
  else
    Gen.Transmute( From, Too );
  Exit( 0 );
end;

function lua_dungen_around( L : Plua_State ) : Integer; cdecl;
begin
  lua_pushinteger( L, Gen.Around( vlua_tocoord( L, 1 ), lua_tocellset( L, 2 ), lua_tointeger_def( L, 3, 1 ) ) );
  Exit( 1 );
end;

function lua_dungen_cross_around( L : Plua_State ) : Integer; cdecl;
begin
  lua_pushinteger( L, Gen.CrossAround( vlua_tocoord( L, 1 ), lua_tocellset( L, 2 ) ) );
  Exit( 1 );
end;

function lua_dungen_random_square( L : Plua_State ) : Integer; cdecl;
var
  Res : Boolean;
begin
  Res := Gen.RandomCellSquare( lua_tocellset( L, 1 ) );
  if Res then
    vlua_pushcoord( L, Gen.FoundCell )
  else
    lua_pushnil( L );
  Exit( 1 );
end;


function lua_dungen_random_coord( L : Plua_State ) : Integer; cdecl;
var
  T1 :      Integer;
  CellSet : TFlags;
begin
  T1 := lua_type( L, 1 );

  if T1 <= LUA_TNIL then
  begin
    vlua_pushcoord( L, Gen.RanCoord );
    Exit( 1 );
  end;
  if T1 = LUA_TUSERDATA then
  begin
    vlua_pushcoord( L, vlua_toparea( L, 1 )^.RandomCoord );
    Exit( 1 );
  end;
  CellSet := lua_tocellset( L, 1 );

  try
    if lua_type( L, 2 ) = LUA_TUSERDATA then
      vlua_pushcoord( L, Gen.RanCoord( CellSet, vlua_toparea( L, 2 )^ ) )
    else
      vlua_pushcoord( L, Gen.RanCoord( CellSet ) );
    Exit( 1 );
  except
    on EPlacementException do
  end;
  Exit( 0 );
end;

function lua_dungen_random_empty_coord( L : Plua_State ) : Integer; cdecl;
var
  T1 :      Integer;
  CellSet : TFlags;
  Flags :   TFlags32;
begin
  Flags := lua_toflags32( L, 1 );
  try
    T1 := lua_type( L, 2 );

    if T1 <= LUA_TNIL then
    begin
      vlua_pushcoord( L, Gen.EmptyRanCoord( Flags ) );
      Exit( 1 );
    end;
    if T1 = LUA_TUSERDATA then
    begin
      vlua_pushcoord( L, Gen.EmptyRanCoord( Flags, vlua_toparea( L, 2 )^ ) );
      Exit( 1 );
    end;
    CellSet := lua_tocellset( L, 2 );

    if lua_type( L, 3 ) = LUA_TUSERDATA then
      vlua_pushcoord( L, Gen.EmptyRanCoord( CellSet, Flags, vlua_toparea( L, 3 )^ ) )
    else
      vlua_pushcoord( L, Gen.EmptyRanCoord( CellSet, Flags ) );
    Exit( 1 );
  except
    on EPlacementException do
  end;
  Exit( 0 );
end;

function lua_dungen_drop_coord( L : Plua_State ) : Integer; cdecl;
var
  Coord : PCoord2D;
begin
  Coord := vlua_topcoord( L, 1 );
  try
    vlua_pushcoord( L, Gen.Drop( Coord^, lua_toflags32( L, 2 ) ) );
    Exit( 1 );
  except
    on EPlacementException do
  end;
  Exit( 0 );
end;

function lua_dungen_find_coord( L : Plua_State ) : Integer; cdecl;
begin
  try
    if lua_type( L, 2 ) = LUA_TUSERDATA then
      vlua_pushcoord( L, Gen.FindCell( lua_tocellset( L, 1 ), vlua_toparea( L, 2 )^ ) )
    else
      vlua_pushcoord( L, Gen.FindCell( lua_tocellset( L, 1 ) ) );
    Exit( 1 );
  except
    on EPlacementException do
  end;
  Exit( 0 );
end;

function lua_dungen_find_empty_coord( L : Plua_State ) : Integer; cdecl;
begin
  try
    if lua_type( L, 3 ) = LUA_TUSERDATA then
      vlua_pushcoord( L, Gen.FindCell( lua_tocellset( L, 1 ), lua_toflags32( L, 2 ), vlua_toparea( L, 3 )^ ) )
    else
      vlua_pushcoord( L, Gen.FindCell( lua_tocellset( L, 1 ), lua_toflags32( L, 2 ) ) );
    Exit( 1 );
  except
    on EPlacementException do
  end;
  Exit( 0 );
end;

function lua_dungen_find_random_coord( L : Plua_State ) : Integer; cdecl;
begin
  try
    if lua_type( L, 2 ) = LUA_TUSERDATA then
      vlua_pushcoord( L, Gen.FindRanCoord( lua_tocellset( L, 1 ), vlua_toparea( L, 2 )^ ) )
    else
      vlua_pushcoord( L, Gen.FindRanCoord( lua_tocellset( L, 1 ) ) );
    Exit( 1 );
  except
    on EPlacementException do
  end;
  Exit( 0 );
end;

function lua_dungen_find_random_empty_coord( L : Plua_State ) : Integer; cdecl;
begin
  try
    if lua_type( L, 3 ) = LUA_TUSERDATA then
      vlua_pushcoord( L, Gen.FindEmptyRanCoord( lua_tocellset( L, 1 ), lua_toflags32( L, 2 ), vlua_toparea( L, 3 )^ ) )
    else
      vlua_pushcoord( L, Gen.FindEmptyRanCoord( lua_tocellset( L, 1 ), lua_toflags32( L, 2 ) ) );
    Exit( 1 );
  except
    on EPlacementException do
  end;
  Exit( 0 );
end;


function lua_dungen_plot_line( L : Plua_State ) : Integer; cdecl;
var
  Coord :   TCoord2D;
  Horiz :   Boolean;
  Cell :    Byte;
  CellSet : TFlags;
begin
  Coord := vlua_tocoord( L, 1 );
  Horiz := lua_toboolean( L, 2 );
  Cell := lua_tocell( L, 3 );
  CellSet := lua_tocellset( L, 4 );
  Gen.PlotLine( Coord, Horiz, Cell, 0, CellSet, 0 );
  Exit( 0 );
end;

function lua_dungen_get_endpoints( L : Plua_State ) : Integer; cdecl;
var
  Coord :   TCoord2D;
  Where :   TCoord2D;
  Step :    TCoord2D;
  Horiz :   Boolean;
  CellSet : TFlags;
  Cell :    Byte;
begin
  Where := vlua_tocoord( L, 1 );
  Horiz := lua_toboolean( L, 2 );
  CellSet := lua_tocellset( L, 3 );

  Coord := Where;
  if Horiz then
    Step := NewCoord2D( +1, 0 )
  else
    Step := NewCoord2D( 0, +1 );
  while True do
  begin
    Coord += Step;
    cell := Gen.GetCell( Coord );
    if not ( cell in CellSet ) then
    begin
      lua_pushinteger( L, cell );
      Break;
    end;
  end;
  Coord := Where;
  while True do
  begin
    Coord -= Step;
    cell := Gen.GetCell( Coord );
    if not ( cell in CellSet ) then
    begin
      lua_pushinteger( L, cell );
      Break;
    end;
  end;
  Exit( 2 );
end;

function lua_dungen_scan( L : Plua_State ) : Integer; cdecl;
var
  Area :   TArea;
  Ignore : TCellSet;
  Bound :  Boolean;
begin
  Area := vlua_toarea( L, 1 );
  Ignore := lua_tocellset( L, 2 );
  Bound := lua_toboolean( L, 3 );
  lua_pushinteger( L, Gen.Scan( Area, Ignore, Bound ) );
  Exit( 1 );
end;

function lua_dungen_each_closure( L : Plua_State ) : Integer; cdecl;
var
  Area : PArea;
  c :    PCoord2D;
  cell : Byte;
begin
  Area := vlua_toparea( L, lua_upvalueindex( 1 ) );
  c := vlua_topcoord( L, lua_upvalueindex( 2 ) );
  cell := lua_tointeger( L, lua_upvalueindex( 3 ) );

  repeat
    c^.x := c^.x + 1;
    if c^.x > Area^.b.x then
    begin
      c^.x := Area^.a.x;
      c^.y := c^.y + 1;
      if c^.y > Area^.b.y then
      begin
        lua_pushnil( L );
        Exit( 1 );
      end;
    end;
  until Gen.GetCell( c^ ) = cell;
  vlua_pushcoord( L, c^ );
  Exit( 1 );
end;

function lua_dungen_each( L : Plua_State ) : Integer; cdecl;
var
  Area : PArea;
  A :    TCoord2D;
  Cell : Byte;
begin
  Cell := lua_tocell( L, 1 );
  if vlua_isarea( L, 2 ) then
  begin
    Area := vlua_toparea( L, 2 );
    A := Area^.A;
  end
  else
  begin
    vlua_pusharea( L, Gen.Area );
    A := Gen.Area.A;
  end;
  A.X := A.X - 1;
  vlua_pushcoord( L, A );
  lua_pushinteger( L, Cell );
  lua_pushcclosure( L, @lua_dungen_each_closure, 3 );
  Exit( 1 );
end;

function lua_dungen_read_rooms( L : Plua_State ) : Integer; cdecl;
var
  Area :   TArea;
  Room :   TArea;
  Cell :   Byte;
  Count :  Word;
  c :      TCoord2D;
  rx, ry : TCoord2D;

  function RoomStart( ax, ay : Integer ) : Boolean;
  begin
    Exit( ( Gen.GetCell( NewCoord2D( ax + 1, ay ) ) = Cell ) and ( Gen.GetCell(
      NewCoord2D( ax, ay + 1 ) ) = Cell ) and ( Gen.GetCell( NewCoord2D( ax - 1, ay ) ) <> Cell ) and
      ( Gen.GetCell( NewCoord2D( ax, ay - 1 ) ) <> Cell ) );
  end;

begin
  Area := vlua_toarea( L, 1 );
  Cell := lua_tocell( L, 2 );
  lua_createtable( L, 0, 0 );
  Count := 1;

  c := Area.a;
  repeat
    if Gen.GetCell( c ) = Cell then
      if RoomStart( c.x, c.y ) then
      begin
        rx := c;
        ry := c;
        repeat
          Inc( rx.x );
        until Gen.GetCell( rx ) <> Cell;
        repeat
          Inc( ry.y );
        until Gen.GetCell( ry ) <> Cell;
        Room.A := c;
        Room.B := NewCoord2D( rx.x - 1, ry.y - 1 );
        vlua_pusharea( L, Room );
        lua_rawseti( L, -2, Count );
        Inc( Count );
      end;
    Inc( c.x );
    if c.x > Area.b.x then
    begin
      Inc( c.y );
      c.x := Area.a.x;
    end;
  until c.y > Area.b.y;
  Exit( 1 );
end;

function lua_dungen_run_drunkard_walk( L : Plua_State ) : Integer; cdecl;
var
  iSteps :  DWord;
  iCount :  DWord;
  iCoord :  TCoord2D;
  iArea :   TArea;
  iCell :   Byte;
  iIgnore : TCellSet;
  iBreak :  Boolean;
begin
  iArea := vlua_toarea( L, 1 );
  iCoord := vlua_tocoord( L, 2 );
  iCount := lua_tointeger( L, 3 );
  iCell := lua_tocell( L, 4 );
  iIgnore := [];
  iBreak := False;

  if lua_gettop( L ) > 4 then
    iIgnore := lua_tocellset( L, 5 );
  if lua_gettop( L ) > 5 then
    iBreak := lua_toboolean( L, 6 );

  if iCount = 0 then
    Exit( 0 );
  for iSteps := 1 to iCount do
  begin
    if not iArea.Contains( iCoord ) then
      if iBreak then
        Exit( 0 )
      else
        iArea.Clamp( iCoord );

    if not ( Gen.GetCell( iCoord ) in iIgnore ) then
      Gen.PutCell( iCoord, iCell );
    iCoord.RandomShift( 1 );
  end;
  Exit( 0 );
end;

// -------- Tile support ---------------------------------------------- //

type
  TTileRecord = record
    Data  : PByte;
    Ascii : PByte;
    SizeX : Word;
    Sizey : Word;
  end;

type
  PTileRecord = ^TTileRecord;

function vlua_istile( L : Plua_State; Index : Integer ) : Boolean;
begin
  Exit( luaL_testudata( L, Index, VALKYRIE_DUNGEN_TILE ) <> nil );
end;

function vlua_totile( L : Plua_State; Index : Integer ) : TTileRecord;
var
  TilePtr : PTileRecord;
begin
  TilePtr := luaL_checkudata( L, Index, VALKYRIE_DUNGEN_TILE );
  Exit( TilePtr^ );
end;

function vlua_toptile( L : Plua_State; Index : Integer ) : PTileRecord;
var
  TilePtr : PTileRecord;
begin
  TilePtr := luaL_checkudata( L, Index, VALKYRIE_DUNGEN_TILE );
  Exit( TilePtr );
end;

procedure vlua_pushtile( L : Plua_State; const Tile : TTileRecord );
var
  TilePtr : PTileRecord;
begin
  TilePtr := PTileRecord( lua_newuserdata( L, SizeOf( TTileRecord ) ) );
  TilePtr^ := Tile;

  luaL_getmetatable( L, VALKYRIE_DUNGEN_TILE );
  lua_setmetatable( L, -2 );
end;

function lua_dungen_tile_new( L : Plua_State ) : Integer; cdecl;
var
  Code        : AnsiString;
  Tile        : TTileRecord;
  Line, Row   : Word;
  Translation : array[0..255] of Byte;
  Gylph       : AnsiString;
  Ascii       : Boolean;
  Last        : Integer;
begin
  Ascii := lua_toboolean( L, 3 );
  lua_settop( L, 2 );
  Code := DelChars( DelChars( DelChars( TrimSet( lua_tostring( L, 1 ), [#1..#32] ), #13 ), #9 ), ' ' );
  Tile.SizeY := WordCount( Code, [#10] );
  Last := Pos( #10, Code ) - 1;
  if Last < 0
    then Tile.SizeX := Length( Code )
    else Tile.SizeX := Pos( #10, Code ) - 1;
  Tile.Data  := GetMem( Tile.SizeX * Tile.SizeY );
  if Ascii
    then Tile.Ascii := GetMem( Tile.SizeX * Tile.SizeY )
    else Tile.Ascii := nil;

  {$HINTS OFF}
  FillChar( Translation, 255, 0 );
  {$HINTS ON}

  // TODO: error reporting
  if lua_istable( L, 2 ) then
  begin
    lua_pushnil( L );
    while lua_next( L, 2 ) <> 0 do
    begin
      // uses 'key' (at index -2) and 'value' (at index -1) */
      if lua_isstring( L, -2 ) and ( lua_objlen( L, -2 ) = 1 ) then
        Translation[Ord( lua_tostring( L, -2 )[1] )] := Byte( lua_tocell( L, -1 ) );
      // removes 'value'; keeps 'key' for next iteration */
      lua_pop( L, 1 );
    end;
  end;

  for Line := 0 to Tile.SizeX - 1 do
    for Row := 0 to Tile.SizeY - 1 do
    begin
      Gylph := Code[Row * ( Tile.SizeX + 1 ) + Line + 1];
      // TODO: check for errors
      Tile.Data[Row * Tile.SizeX + Line] := Translation[Ord( Gylph[1] )];
      if Ascii then Tile.Ascii[Row * Tile.SizeX + Line] := Ord( Gylph[1] );
    end;

  vlua_pushtile( L, Tile );
  Exit( 1 );
end;

function lua_dungen_tile_clone( L : Plua_State ) : Integer; cdecl;
var
  NewTile : PTileRecord;
  OldTile : PTileRecord;
begin
  OldTile := vlua_toptile( L, 1 );
  NewTile := PTileRecord( lua_newuserdata( L, SizeOf( TTileRecord ) ) );
  NewTile^.SizeX := OldTile^.SizeX;
  NewTile^.SizeY := OldTile^.SizeY;
  NewTile^.Data := GetMem( NewTile^.SizeX * NewTile^.SizeY );
  if OldTile^.Ascii <> nil
    then NewTile^.Ascii := GetMem( NewTile^.SizeX * NewTile^.SizeY )
    else NewTile^.Ascii := nil;

  Move( OldTile^.Data^, NewTile^.Data^, NewTile^.SizeX * NewTile^.SizeY );
  if OldTile^.Ascii <> nil then
    Move( OldTile^.Ascii^, NewTile^.Ascii^, NewTile^.SizeX * NewTile^.SizeY );

  luaL_getmetatable( L, VALKYRIE_DUNGEN_TILE );
  lua_setmetatable( L, -2 );
  Result := 1;
end;

function lua_dungen_tile_place( L : Plua_State ) : Integer; cdecl;
var
  Tile :    PTileRecord;
  Coord :   TCoord2D;
  x, y, c : Word;
begin
  Coord := vlua_tocoord( L, 1 );
  Tile := vlua_toptile( L, 2 );

  for X := 0 to Tile^.SizeX - 1 do
    for Y := 0 to Tile^.SizeY - 1 do
    begin
      c := Tile^.Data[Y * Tile^.SizeX + X];
      if c <> 0 then
        Gen.PutCell( Coord + NewCoord2D( X, Y ), Tile^.Data[Y * Tile^.SizeX + X] );
    end;

  Exit( 0 );
end;

function lua_dungen_tile_flip_x( L : Plua_State ) : Integer; cdecl;
var Tile : PTileRecord;
 function FlipX( Source : PByte ) : PByte;
 var X, Y : Word;
     Data : PByte;
 begin
   if Source = nil then Exit( nil );
   Data := GetMem( Tile^.SizeX * Tile^.SizeY );
   for X := 0 to Tile^.SizeX - 1 do
     for Y := 0 to Tile^.SizeY - 1 do
       Data[Y * Tile^.SizeX + X] := Source[( Y + 1 ) * Tile^.SizeX - X - 1];
   FreeMem( Source, Tile^.SizeX * Tile^.SizeY );
   Exit( Data );
 end;
begin
  Tile := vlua_toptile( L, 1 );
  Tile^.Data  := FlipX( Tile^.Data );
  Tile^.Ascii := FlipX( Tile^.Ascii );
  Exit( 0 );
end;

function lua_dungen_tile_flip_y( L : Plua_State ) : Integer; cdecl;
var Tile : PTileRecord;
 function FlipY( Source : PByte ) : PByte;
 var X, Y : Word;
     Data : PByte;
 begin
   if Source = nil then Exit( nil );
   Data := GetMem( Tile^.SizeX * Tile^.SizeY );
   for X := 0 to Tile^.SizeX - 1 do
     for Y := 0 to Tile^.SizeY - 1 do
       Data[Y * Tile^.SizeX + X] := Source[( Tile^.SizeY - Y - 1 ) * Tile^.SizeX + X];
   FreeMem( Source, Tile^.SizeX * Tile^.SizeY );
   Exit( Data );
 end;
begin
  Tile := vlua_toptile( L, 1 );
  Tile^.Data  := FlipY( Tile^.Data );
  Tile^.Ascii := FlipY( Tile^.Ascii );
  Exit( 0 );
end;

function lua_dungen_tile_flip_xy( L : Plua_State ) : Integer; cdecl;
var Tile : PTileRecord;
 function FlipXY( Source : PByte ) : PByte;
 var X, Y : Word;
     Data : PByte;
 begin
   if Source = nil then Exit( nil );
   Data := GetMem( Tile^.SizeX * Tile^.SizeY );
   for X := 0 to Tile^.SizeX - 1 do
     for Y := 0 to Tile^.SizeY - 1 do
       Data[Y * Tile^.SizeX + X] := Source[( Tile^.SizeY - Y ) * Tile^.SizeX - X - 1];
   FreeMem( Source, Tile^.SizeX * Tile^.SizeY );
   Exit( Data );
 end;
begin
  Tile := vlua_toptile( L, 1 );
  Tile^.Data  := FlipXY( Tile^.Data );
  Tile^.Ascii := FlipXY( Tile^.Ascii );
  Exit( 0 );
end;

function lua_dungen_tile_flip_random( L : Plua_State ) : Integer; cdecl;
begin
  case Random( 4 ) of
    0 : ;
    1 : lua_dungen_tile_flip_x( L );
    2 : lua_dungen_tile_flip_y( L );
    3 : lua_dungen_tile_flip_xy( L );
  end;
  Exit( 0 );
end;

function lua_dungen_tile_get_size_coord( L : Plua_State ) : Integer; cdecl;
var
  Tile : PTileRecord;
begin
  Tile := vlua_toptile( L, 1 );
  vlua_pushcoord( L, NewCoord2D( Tile^.SizeX, Tile^.SizeY ) );
  Exit( 1 );
end;

function lua_dungen_tile_get_size_x( L : Plua_State ) : Integer; cdecl;
var
  Tile : PTileRecord;
begin
  Tile := vlua_toptile( L, 1 );
  lua_pushinteger( L, Tile^.SizeX );
  Exit( 1 );
end;

function lua_dungen_tile_get_size_y( L : Plua_State ) : Integer; cdecl;
var
  Tile : PTileRecord;
begin
  Tile := vlua_toptile( L, 1 );
  lua_pushinteger( L, Tile^.SizeX );
  Exit( 1 );
end;

function lua_dungen_tile_get_area( L : Plua_State ) : Integer; cdecl;
var
  Tile : PTileRecord;
begin
  Tile := vlua_toptile( L, 1 );
  vlua_pusharea( L, NewArea( NewCoord2D( 1, 1 ), NewCoord2D( Tile^.SizeX, Tile^.SizeY ) ) );
  Exit( 1 );
end;

function lua_dungen_tile_expand( L : Plua_State ) : Integer; cdecl;
var
  Tile :   PTileRecord;
  Data :   PByte;
  SizesX : array of Byte;
  SizesY : array of Byte;
  OrgX :   Word;
  OrgY :   Word;
  NewX :   Word;
  NewY :   Word;
  Line :   Word;
  Count :  Word;
  CY :     Word;

  procedure FillRow( OY, RY : Word );
  var
    x, px, c : Word;
  begin
    px := 0;
    for x := 0 to OrgX - 1 do
      for c := 0 to SizesX[x] - 1 do
      begin
        Data[px + RY * NewX] := Tile^.Data[x + OY * OrgX];
        Inc( px );
      end;
  end;

begin
  // TODO: lots of error checking
  Tile := vlua_toptile( L, 1 );
  Assert( Tile^.Ascii = nil );
  SizesX := vlua_tobytearray( L, 2 );
  if lua_istable( L, 3 ) then
    SizesY := vlua_tobytearray( L, 3 )
  else
    SizesY := SizesX;

  OrgX := Tile^.SizeX;
  OrgY := Tile^.SizeY;
  NewX := 0;
  NewY := 0;

  for Count := 0 to OrgX - 1 do
    NewX += SizesX[Count];
  for Count := 0 to OrgY - 1 do
    NewY += SizesY[Count];
  Data := GetMem( NewX * NewY );

  CY := 0;
  for Line := 0 to OrgY - 1 do
    for Count := 0 to SizesY[Line] - 1 do
    begin
      FillRow( Line, CY );
      Inc( CY );
    end;

  FreeMem( Tile^.Data, OrgX * OrgY );
  Tile^.SizeX := NewX;
  Tile^.SizeY := NewY;
  Tile^.Data := Data;
  Exit( 0 );
end;

function lua_dungen_tile_raw_get( L : Plua_State ) : Integer; cdecl;
var
  Tile :  PTileRecord;
  Coord : TCoord2D;
begin
  Tile := lua_touserdata( L, 1 );
  if lua_type( L, 2 ) = LUA_TNUMBER then
    lua_pushinteger( L, Tile^.Data[( lua_tointeger( L, 2 ) - 1 ) + ( lua_tointeger( L, 3 ) - 1 ) * Tile^.SizeX] )
  else
  begin
    Coord := vlua_tocoord( L, 2 );
    lua_pushinteger( L, Tile^.Data[( Coord.X - 1 ) + ( Coord.Y - 1 ) * Tile^.SizeX] );
  end;
  Exit( 1 );
end;

function lua_dungen_tile_raw_set( L : Plua_State ) : Integer; cdecl;
var
  Tile :  PTileRecord;
  Coord : TCoord2D;
begin
  Tile := lua_touserdata( L, 1 );
  if lua_type( L, 2 ) = LUA_TNUMBER then
    Tile^.Data[( lua_tointeger( L, 2 ) - 1 ) + ( lua_tointeger( L, 3 ) - 1 ) * Tile^.SizeX] := lua_tointeger( L, 4 )
  else
  begin
    Coord := vlua_tocoord( L, 2 );
    Tile^.Data[( Coord.X - 1 ) + ( Coord.Y - 1 ) * Tile^.SizeX] := lua_tointeger( L, 3 );
  end;
  Exit( 0 );
end;

function lua_dungen_tile_ascii_get( L : Plua_State ) : Integer; cdecl;
var
  Tile :  PTileRecord;
  Coord : TCoord2D;
begin
  Tile := lua_touserdata( L, 1 );
  if Tile^.Ascii = nil then Exit( 0 );
  if lua_type( L, 2 ) = LUA_TNUMBER then
    lua_pushinteger( L, Tile^.Ascii[( lua_tointeger( L, 2 ) - 1 ) + ( lua_tointeger( L, 3 ) - 1 ) * Tile^.SizeX] )
  else
  begin
    Coord := vlua_tocoord( L, 2 );
    lua_pushinteger( L, Tile^.Ascii[( Coord.X - 1 ) + ( Coord.Y - 1 ) * Tile^.SizeX] );
  end;
  Exit( 1 );
end;

function lua_dungen_tile_ascii_set( L : Plua_State ) : Integer; cdecl;
var
  Tile :  PTileRecord;
  Coord : TCoord2D;
begin
  Tile := lua_touserdata( L, 1 );
  if Tile^.Ascii = nil then Exit( 0 );
  if lua_type( L, 2 ) = LUA_TNUMBER then
    Tile^.Ascii[( lua_tointeger( L, 2 ) - 1 ) + ( lua_tointeger( L, 3 ) - 1 ) * Tile^.SizeX] := lua_tointeger( L, 4 )
  else
  begin
    Coord := vlua_tocoord( L, 2 );
    Tile^.Ascii[( Coord.X - 1 ) + ( Coord.Y - 1 ) * Tile^.SizeX] := lua_tointeger( L, 3 );
  end;
  Exit( 0 );
end;

function lua_dungen_tile_gc( L : Plua_State ) : Integer; cdecl;
var
  Tile : PTileRecord;
begin
  Tile := lua_touserdata( L, 1 );
  if Tile <> nil then
  begin
    FreeMem( Tile^.Data, Tile^.SizeX * Tile^.SizeY );
    if Tile^.Ascii <> nil then
      FreeMem( Tile^.Ascii, Tile^.SizeX * Tile^.SizeY );
  end;
  Exit( 0 );
end;

// -------- Registration tables and functions ------------------------- //

const
  dungenlib_f : array[0..29] of luaL_Reg = (
    ( Name : 'get_cell_id'; func : @lua_dungen_get_cell_id; ),
    ( Name : 'get_cell'; func : @lua_dungen_get_cell; ),
    ( Name : 'set_cell'; func : @lua_dungen_set_cell; ),
    ( Name : 'fast_get_cell'; func : @lua_dungen_fast_get_cell; ),
    ( Name : 'fast_set_cell'; func : @lua_dungen_fast_set_cell; ),
    ( Name : 'is_empty'; func : @lua_dungen_is_empty; ),
    ( Name : 'is_empty_area'; func : @lua_dungen_is_empty_area; ),
    ( Name : 'fill'; func : @lua_dungen_fill; ),
    ( Name : 'fill_pattern'; func : @lua_dungen_fill_pattern; ),
    ( Name : 'fill_edges'; func : @lua_dungen_fill_edges; ),
    ( Name : 'transmute'; func : @lua_dungen_transmute; ),
    ( Name : 'around'; func : @lua_dungen_around; ),
    ( Name : 'cross_around'; func : @lua_dungen_cross_around; ),
    ( Name : 'random_square'; func : @lua_dungen_random_square; ),
    ( Name : 'random_coord'; func : @lua_dungen_random_coord; ),
    ( Name : 'random_empty_coord'; func : @lua_dungen_random_empty_coord; ),
    ( Name : 'drop_coord'; func : @lua_dungen_drop_coord; ),
    ( Name : 'find_coord'; func : @lua_dungen_find_coord; ),
    ( Name : 'find_empty_coord'; func : @lua_dungen_find_empty_coord; ),
    ( Name : 'find_random_coord'; func : @lua_dungen_find_random_coord; ),
    ( Name : 'find_random_empty_coord'; func : @lua_dungen_find_random_empty_coord; ),
    ( Name : 'tile_new'; func : @lua_dungen_tile_new ),
    ( Name : 'tile_place'; func : @lua_dungen_tile_place; ),
    ( Name : 'plot_line'; func : @lua_dungen_plot_line; ),
    ( Name : 'get_endpoints'; func : @lua_dungen_get_endpoints; ),
    ( Name : 'scan'; func : @lua_dungen_scan; ),
    ( Name : 'each'; func : @lua_dungen_each; ),
    ( Name : 'read_rooms'; func : @lua_dungen_read_rooms; ),
    ( Name : 'run_drunkard_walk'; func : @lua_dungen_run_drunkard_walk; ),
    ( Name : nil; func : nil; )
    );

const
  dungentile_f : array[0..15] of luaL_Reg = (
    ( Name : 'clone'; func : @lua_dungen_tile_clone ),
    ( Name : 'flip_x'; func : @lua_dungen_tile_flip_x ),
    ( Name : 'flip_y'; func : @lua_dungen_tile_flip_y ),
    ( Name : 'flip_xy'; func : @lua_dungen_tile_flip_xy ),
    ( Name : 'flip_random'; func : @lua_dungen_tile_flip_random ),
    ( Name : 'get_size_coord'; func : @lua_dungen_tile_get_size_coord ),
    ( Name : 'get_size_x'; func : @lua_dungen_tile_get_size_x ),
    ( Name : 'get_size_y'; func : @lua_dungen_tile_get_size_y ),
    ( Name : 'get_area'; func : @lua_dungen_tile_get_area ),
    ( Name : 'expand'; func : @lua_dungen_tile_expand ),
    ( Name : 'raw_get'; func : @lua_dungen_tile_raw_get ),
    ( Name : 'raw_set'; func : @lua_dungen_tile_raw_set ),
    ( Name : 'get_ascii'; func : @lua_dungen_tile_ascii_get ),
    ( Name : 'set_ascii'; func : @lua_dungen_tile_ascii_set ),
    ( Name : '__gc'; func : @lua_dungen_tile_gc ),
    ( Name : nil; func : nil; ) );


procedure RegisterDungenClass( L : Plua_State; ObjectName : AnsiString = '' );
begin
  if ObjectName = '' then
    ObjectName := 'dungen';
  luaL_newmetatable( L, VALKYRIE_DUNGEN );
  luaL_register( L, PChar( ObjectName ), dungenlib_f );

  luaL_newmetatable( L, VALKYRIE_DUNGEN_TILE );
  lua_pushvalue( L, -1 );
  lua_setfield( L, -2, '__index' );
  luaL_register( L, nil, dungentile_f );
end;

procedure RegisterDungen( DunGen : TDungeonBuilder );
begin
  Gen := DunGen;
end;



end.
