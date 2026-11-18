unit vluadungen;

{$MODE OBJFPC}
interface

uses classes, sysutils, vlualibrary, vrltools, vluamapnode;

procedure RegisterDungenClass( L : Plua_State; ObjectName : AnsiString = '' );

implementation

uses strutils, vutil, vlua, vluatools, vluaext, vrandom;

const
  VALKYRIE_DUNGEN      = 'valkyrie.dungen';
  VALKYRIE_DUNGEN_TILE = 'valkyrie.dungen.tile';

function lua_dungen_plot_line( L : Plua_State ) : Integer; cdecl;
var iState      : TLuaMapStack;
    iPoint      : TCoord2D;
    iCoord      : TCoord2D;
    iStep       : TCoord2D;
    iHoriz      : Boolean;
    iArea       : TArea;
    iCell       : Byte;
    iBlockCells : TFlags;
begin
  iState.Init( L );
  iPoint      := iState.ToCoord( 2 );
  iHoriz      := iState.ToBoolean( 3 );
  iCell       := iState.ToCell( 4 );
  if iState.IsArea( 5 ) then
  begin
    iBlockCells := [ iCell ];
    iArea       := iState.ToOptionalArea( 5 );
  end
  else
  begin
    iBlockCells := iState.ToCellset( 5, [ iCell ] );
    iArea       := iState.ToOptionalArea( 6 );
  end;

  if iHoriz
    then iStep := NewCoord2D( +1,  0 )
    else iStep := NewCoord2D(  0, +1 );

  iCoord := iPoint;

  while iCoord.Horiz(iHoriz) < iArea.B.Horiz(iHoriz) do
  begin
    iCoord += iStep;
    if iState.Map.GetCell( iCoord ) in iBlockCells
      then Break
      else iState.Map.PutCell( iCoord, iCell );
  end;
  iCoord := iPoint;
  while iCoord.Horiz(iHoriz) > iArea.A.Horiz(iHoriz) do
  begin
    iCoord -= iStep;
    if iState.Map.GetCell( iCoord ) in iBlockCells
      then Break
      else iState.Map.PutCell( iCoord, iCell );
  end;

  iState.Map.PutCell( iPoint, iCell );
  Exit( 0 );
end;

// get_endpoints( map, where, horiz, open_cells, stop_cells [, limit_area ] )
// Walks from where in both directions ( left/up first ) over open_cells and
// returns the two end coords, or nil. At each step, in order: a stop cell
// ends the line on that cell; a cell outside limit_area ends the line on that
// cell; leaving the map or any other non-open cell fails. limit_area should
// lie inside the map.
function lua_dungen_get_endpoints( L : Plua_State ) : Integer; cdecl;
var iState    : TLuaMapStack;
    iWhere    : TCoord2D;
    iStep     : TCoord2D;
    iOpen     : TFlags;
    iStop     : TFlags;
    iHasLimit : Boolean;
    iLimit    : TArea;
    iEnd1     : TCoord2D;
    iEnd2     : TCoord2D;

  function ScanEnd( aStep : TCoord2D; out aEnd : TCoord2D ) : Boolean;
  var iCell : Byte;
  begin
    aEnd := iWhere;
    while True do
    begin
      aEnd += aStep;
      if not iState.Map.isProperCoord( aEnd ) then Exit( False );
      iCell := iState.Map.GetCell( aEnd );
      if iCell in iStop then Exit( True );
      if iHasLimit and ( not iLimit.Contains( aEnd ) ) then Exit( True );
      if not ( iCell in iOpen ) then Exit( False );
    end;
  end;

begin
  iState.Init( L );
  iWhere    := iState.ToCoord( 2 );
  iOpen     := iState.ToCellSet( 4 );
  iStop     := iState.ToCellSet( 5 );
  iHasLimit := iState.IsArea( 6 );
  if iHasLimit then iLimit := iState.ToOptionalArea( 6 );

  if iState.ToBoolean( 3 )
    then iStep := NewCoord2D( -1, 0 )
    else iStep := NewCoord2D( 0, -1 );

  if not ( ScanEnd( iStep, iEnd1 ) and ScanEnd( NewCoord2D( -iStep.x, -iStep.y ), iEnd2 ) ) then
  begin
    lua_pushnil( L );
    Exit( 1 );
  end;
  iState.PushCoord( iEnd1 );
  iState.PushCoord( iEnd2 );
  Exit( 2 );
end;

// fill_dead_ends( map, open_cells, wall_cells, fill_cell [, area [, max_passes ]] )
// An open cell in area with at least 3 of its 4 direct neighbours in
// wall_cells is a dead end. Each pass turns every cell that is a dead end at
// its start into fill_cell, removing one layer. Passes repeat until none is
// left, or for at most max_passes passes ( omitted or 0: no limit ). area
// defaults to the map without its edge; fill_cell must not be an open cell.
// Returns the number of filled cells.
function lua_dungen_fill_dead_ends( L : Plua_State ) : Integer; cdecl;
var iState      : TLuaMapStack;
    iOpen       : TFlags;
    iWalls      : TFlags;
    iFill       : Byte;
    iArea       : TArea;
    iMaxPasses  : Integer;
    iPass       : Integer;
    iFilled     : Integer;
    iLayer      : array of TCoord2D;
    iLayerCount : Integer;
    iNext       : array of TCoord2D;
    iNextCount  : Integer;
    iIndex      : Integer;
    iX, iY      : Integer;

  procedure AddIfDeadEnd( const aCoord : TCoord2D );
  begin
    if not iArea.Contains( aCoord ) then Exit;
    if not ( iState.Map.GetCell( aCoord ) in iOpen ) then Exit;
    if iState.Map.CellsCrossAround( aCoord, iWalls ) < 3 then Exit;
    if iNextCount = Length( iNext ) then SetLength( iNext, iNextCount * 2 + 16 );
    iNext[ iNextCount ] := aCoord;
    Inc( iNextCount );
  end;

begin
  iState.Init( L );
  iOpen  := iState.ToCellSet( 2 );
  iWalls := iState.ToCellSet( 3 );
  iFill  := iState.ToCell( 4 );
  if iState.IsArea( 5 )
    then iArea := iState.ToOptionalArea( 5 )
    else iArea := iState.Map.Area.Shrinked;
  iState.Map.Area.Clamp( iArea );
  iMaxPasses := 0;
  if lua_isnumber( L, 6 ) then iMaxPasses := lua_tointeger( L, 6 );
  if iFill in iOpen then
    Exit( luaL_error( L, 'fill_dead_ends: fill_cell must not be an open cell' ) );

  iNextCount := 0;
  for iY := iArea.A.Y to iArea.B.Y do
    for iX := iArea.A.X to iArea.B.X do
      AddIfDeadEnd( NewCoord2D( iX, iY ) );

  iFilled := 0;
  iPass   := 0;
  while ( iNextCount > 0 ) and ( ( iMaxPasses <= 0 ) or ( iPass < iMaxPasses ) ) do
  begin
    Inc( iPass );
    iLayer      := iNext;
    iLayerCount := iNextCount;
    iNext       := nil;
    iNextCount  := 0;

    // a cell can be listed twice; the second time it is no longer open
    for iIndex := 0 to iLayerCount - 1 do
      if iState.Map.GetCell( iLayer[ iIndex ] ) in iOpen then
      begin
        iState.Map.PutCell( iLayer[ iIndex ], iFill );
        Inc( iFilled );
      end;

    // only neighbours of the filled layer can have become dead ends
    for iIndex := 0 to iLayerCount - 1 do
      with iLayer[ iIndex ] do
      begin
        AddIfDeadEnd( NewCoord2D( X - 1, Y ) );
        AddIfDeadEnd( NewCoord2D( X + 1, Y ) );
        AddIfDeadEnd( NewCoord2D( X, Y - 1 ) );
        AddIfDeadEnd( NewCoord2D( X, Y + 1 ) );
      end;
  end;

  lua_pushinteger( L, iFilled );
  Exit( 1 );
end;

// transform_around( map, from_cells, to_cell, around_cells, min, max [, area ] )
// Each cell in area that is in from_cells and has between min and max
// ( inclusive ) of its 8 neighbours in around_cells becomes to_cell.
// Neighbours outside the map are not counted. Cells change in place, row by
// row, so later cells see earlier changes. area defaults to the map without
// its edge. Returns the number of changed cells.
function lua_dungen_transform_around( L : Plua_State ) : Integer; cdecl;
var iState   : TLuaMapStack;
    iFrom    : TFlags;
    iTo      : Byte;
    iAround  : TFlags;
    iMin     : Integer;
    iMax     : Integer;
    iArea    : TArea;
    iCount   : Integer;
    iChanged : Integer;
    iCoord   : TCoord2D;
    iX, iY   : Integer;
begin
  iState.Init( L );
  iFrom   := iState.ToCellSet( 2 );
  iTo     := iState.ToCell( 3 );
  iAround := iState.ToCellSet( 4 );
  iMin    := luaL_checkinteger( L, 5 );
  iMax    := luaL_checkinteger( L, 6 );
  if iState.IsArea( 7 )
    then iArea := iState.ToOptionalArea( 7 )
    else iArea := iState.Map.Area.Shrinked;
  iState.Map.Area.Clamp( iArea );

  iChanged := 0;
  for iY := iArea.A.Y to iArea.B.Y do
    for iX := iArea.A.X to iArea.B.X do
    begin
      iCoord := NewCoord2D( iX, iY );
      if not ( iState.Map.GetCell( iCoord ) in iFrom ) then Continue;
      iCount := iState.Map.CellsAround( iCoord, iAround );
      if ( iCount < iMin ) or ( iCount > iMax ) then Continue;
      iState.Map.PutCell( iCoord, iTo );
      Inc( iChanged );
    end;

  lua_pushinteger( L, iChanged );
  Exit( 1 );
end;

// random_door_spot( map, wall_cells, open_cells [, door_cells [, min_door_distance [, area [, edge_only ]]]] )
// A door spot is a wall cell in a straight wall: its left and right
// neighbours are wall cells and its upper and lower ones open cells, or the
// other way around. With door_cells, spots with a door cell within
// min_door_distance ( default 1: the 8 neighbours ) are skipped. area
// defaults to the map without its edge. With edge_only, only the edge cells
// of area without its corners are considered. Spots are counted in row order
// and one is picked uniformly with a single RNG draw; returns nil if none.
function lua_dungen_random_door_spot( L : Plua_State ) : Integer; cdecl;
var iState    : TLuaMapStack;
    iArea     : TArea;
    iWalls    : TFlags;
    iOpen     : TFlags;
    iDoors    : TFlags;
    iHasDoors : Boolean;
    iDistance : Integer;
    iEdgeOnly : Boolean;
    iCount    : Integer;
    iPick     : Integer;
    iX, iY    : Integer;

  // with edge_only, rows between the first and last only visit both ends
  function NextX( aX, aY : Integer ) : Integer;
  begin
    if iEdgeOnly and ( aY <> iArea.A.Y ) and ( aY <> iArea.B.Y ) and ( aX = iArea.A.X ) and ( iArea.B.X > iArea.A.X )
      then Exit( iArea.B.X )
      else Exit( aX + 1 );
  end;

  function InSet( aX, aY : Integer; const aSet : TFlags ) : Boolean;
  var iCoord : TCoord2D;
  begin
    iCoord := NewCoord2D( aX, aY );
    Exit( iState.Map.isProperCoord( iCoord ) and ( iState.Map.GetCell( iCoord ) in aSet ) );
  end;

  function IsSpot( aX, aY : Integer ) : Boolean;
  var iDX, iDY : Integer;
  begin
    if iEdgeOnly then
    begin
      if ( aX <> iArea.A.X ) and ( aX <> iArea.B.X ) and ( aY <> iArea.A.Y ) and ( aY <> iArea.B.Y ) then Exit( False );
      if ( ( aX = iArea.A.X ) or ( aX = iArea.B.X ) ) and ( ( aY = iArea.A.Y ) or ( aY = iArea.B.Y ) ) then Exit( False );
    end;
    if not InSet( aX, aY, iWalls ) then Exit( False );
    if not ( ( InSet( aX - 1, aY, iWalls ) and InSet( aX + 1, aY, iWalls ) and InSet( aX, aY - 1, iOpen ) and InSet( aX, aY + 1, iOpen ) )
          or ( InSet( aX, aY - 1, iWalls ) and InSet( aX, aY + 1, iWalls ) and InSet( aX - 1, aY, iOpen ) and InSet( aX + 1, aY, iOpen ) ) ) then
      Exit( False );
    if iHasDoors then
      for iDY := -iDistance to iDistance do
        for iDX := -iDistance to iDistance do
          if ( ( iDX <> 0 ) or ( iDY <> 0 ) ) and InSet( aX + iDX, aY + iDY, iDoors ) then
            Exit( False );
    Exit( True );
  end;

begin
  iState.Init( L );
  iWalls    := iState.ToCellSet( 2 );
  iOpen     := iState.ToCellSet( 3 );
  iHasDoors := not lua_isnoneornil( L, 4 );
  if iHasDoors then iDoors := iState.ToCellSet( 4 );
  iDistance := 1;
  if lua_isnumber( L, 5 ) then iDistance := lua_tointeger( L, 5 );
  if iState.IsArea( 6 )
    then iArea := iState.ToOptionalArea( 6 )
    else iArea := iState.Map.Area.Shrinked;
  iState.Map.Area.Clamp( iArea );
  iEdgeOnly := lua_toboolean( L, 7 );

  iCount := 0;
  for iY := iArea.A.Y to iArea.B.Y do
  begin
    iX := iArea.A.X;
    while iX <= iArea.B.X do
    begin
      if IsSpot( iX, iY ) then Inc( iCount );
      iX := NextX( iX, iY );
    end;
  end;

  if iCount > 0 then
  begin
    iPick := TLuaContext.RequireRNG( L ).RLongInt( iCount );
    for iY := iArea.A.Y to iArea.B.Y do
    begin
      iX := iArea.A.X;
      while iX <= iArea.B.X do
      begin
        if IsSpot( iX, iY ) then
        begin
          if iPick = 0 then
          begin
            iState.PushCoord( NewCoord2D( iX, iY ) );
            Exit( 1 );
          end;
          Dec( iPick );
        end;
        iX := NextX( iX, iY );
      end;
    end;
  end;
  lua_pushnil( L );
  Exit( 1 );
end;

// read_rooms( map, room_cells, area [, min_size ] )
// Returns the rooms found in area, in row order of their start cells, each
// as an area that includes its frame. A start is a cell of area not in
// room_cells whose lower right neighbour is in room_cells and whose right and
// lower neighbours are not. The frame's right column is the first column whose
// cell on the row below the start is not in room_cells; its bottom row is the
// first row whose cell on the column right of the start is not in
// room_cells. A room is dropped when a cell of the start's row or column up to
// the frame corner is in room_cells, when the right column lies past area or
// the bottom row on or past area's last row, or when it is smaller than
// min_size ( omitted: no minimum ). Cells inside the frame are not checked.
// Coordinates outside the map read the nearest map cell. Same definition as
// nv's read_rooms.
function lua_dungen_read_rooms( L : Plua_State ) : Integer; cdecl;
var iState : TLuaMapStack;
    iRooms : TFlags;
    iArea  : TArea;
    iMin   : TCoord2D;
    iEnd   : TCoord2D;
    iCount : Integer;
    iX, iY : Integer;

  function InRoom( aX, aY : Integer ) : Boolean;
  begin
    Exit( iState.Map.GetCell( iState.Map.Area.Clamped( NewCoord2D( aX, aY ) ) ) in iRooms );
  end;

  function IsStart( aX, aY : Integer ) : Boolean;
  begin
    Exit( ( not InRoom( aX, aY ) ) and InRoom( aX + 1, aY + 1 ) and
          ( not InRoom( aX, aY + 1 ) ) and ( not InRoom( aX + 1, aY ) ) );
  end;

  function ReadRoom( aX, aY : Integer; out aEnd : TCoord2D ) : Boolean;
  begin
    aEnd := NewCoord2D( aX, aY );
    repeat
      if aEnd.X >= iArea.B.X then Exit( False );
      Inc( aEnd.X );
      if InRoom( aEnd.X, aY ) then Exit( False );
    until not InRoom( aEnd.X, aY + 1 );
    repeat
      Inc( aEnd.Y );
      if aEnd.Y >= iArea.B.Y then Exit( False );
      if InRoom( aX, aEnd.Y ) then Exit( False );
    until not InRoom( aX + 1, aEnd.Y );
    Exit( ( aEnd.X - aX + 1 >= iMin.X ) and ( aEnd.Y - aY + 1 >= iMin.Y ) );
  end;

begin
  iState.Init( L );
  iRooms := iState.ToCellSet( 2 );
  iArea  := iState.ToArea( 3 );
  if iState.IsCoord( 4 )
    then iMin := iState.ToCoord( 4 )
    else iMin := NewCoord2D( 0, 0 );

  lua_createtable( L, 0, 0 );
  iCount := 0;
  for iY := iArea.A.Y to iArea.B.Y do
    for iX := iArea.A.X to iArea.B.X do
      if IsStart( iX, iY ) and ReadRoom( iX, iY, iEnd ) then
      begin
        Inc( iCount );
        iState.PushArea( NewArea( NewCoord2D( iX, iY ), iEnd ) );
        lua_rawseti( L, -2, iCount );
      end;
  Exit( 1 );
end;

function lua_dungen_run_drunkard_walk( L : PLua_State ) : Integer; cdecl;
var iState  : TLuaMapStack;
    iRNG    : TRNG;
    iSteps  : DWord;
    iCount  : DWord;
    iCoord  : TCoord2D;
    iArea   : TArea;
    iCell   : Byte;
    iIgnore : TCellSet;
    iBreak  : Boolean;
begin
  iState.Init( L );
  iArea   := iState.ToArea( 2 );
  iCoord  := iState.ToCoord( 3 );
  iCount  := iState.ToInteger( 4 );
  iCell   := iState.ToCell( 5 );
  iIgnore := [];
  iBreak  := False;

  if lua_gettop( L ) > 5 then
    iIgnore := iState.ToCellSet( 6 );
  if lua_gettop( L ) > 6 then
    iBreak := iState.ToBoolean( 7 );

  if iCount = 0 then
    Exit( 0 );
  iRNG := iState.Map.GameRNG;
  for iSteps := 1 to iCount do
  begin
    if not iArea.Contains( iCoord ) then
      if iBreak then
        Exit( 0 )
      else
        iArea.Clamp( iCoord );

    if not ( iState.Map.GetCell( iCoord ) in iIgnore ) then
      iState.Map.PutCell( iCoord, iCell );
    iCoord.RandomShift( iRNG, 1 );
  end;
  Exit( 0 );
end;

// Area argument of the cellular functions: defaults to the map without its
// edge, and is clamped to it.
function CellularArea( var aState : TLuaMapStack; aIndex : Integer ) : TArea;
var iInner : TArea;
begin
  iInner := aState.Map.Area.Shrinked( 1 );
  if not aState.IsArea( aIndex ) then Exit( iInner );
  Result := aState.ToArea( aIndex );
  iInner.Clamp( Result );
end;

// cellular_init( map, full_cell, empty_cell, chance [, area ] )
// Sets each cell of area to full_cell with probability chance, otherwise to
// empty_cell; one RNG draw per cell, in row order. area defaults to the map
// without its edge and is clamped to it. Same definition as nv's
// cellular_init.
function lua_dungen_cellular_init( L : PLua_State ) : Integer; cdecl;
var iState  : TLuaMapStack;
    iRNG    : TRNG;
    iCoord  : TCoord2D;
    iArea   : TArea;
    iFull   : Byte;
    iEmpty  : Byte;
    iChance : Single;
begin
  iState.Init( L );
  iRNG := iState.Map.GameRNG;
  iFull   := iState.ToCell( 2 );
  iEmpty  := iState.ToCell( 3 );
  iChance := iState.ToFloat( 4 );
  iArea   := CellularArea( iState, 5 );

  for iCoord in iArea do
    if iRNG.RDouble < iChance
      then iState.Map.putCell( iCoord, iFull )
      else iState.Map.putCell( iCoord, iEmpty );

  Exit( 0 );
end;

// cellular_random( map, full_cell, empty_cell, neighbours, count [, area [, strict ]] )
// count times, picks a random cell of area and sets it to full_cell if more
// than neighbours cells of its 3x3 block ( itself included ) are full_cell,
// otherwise to empty_cell. With strict, a pick that is neither full_cell nor
// empty_cell is drawn again and not counted. area defaults to the map without
// its edge and is clamped to it. Same definition as nv's cellular_random.
function lua_dungen_cellular_random( L : PLua_State ) : Integer; cdecl;
var iState  : TLuaMapStack;
    iRNG    : TRNG;
    iCoord  : TCoord2D;
    iC      : TCoord2D;
    iArea   : TArea;
    iFull   : Byte;
    iEmpty  : Byte;
    iCell   : Byte;
    iNeigh  : Integer;
    iCount  : Integer;
    i, iCnt : Integer;
    iStrict : Boolean;
begin
  iState.Init( L );
  iRNG := iState.Map.GameRNG;
  iFull   := iState.ToCell( 2 );
  iEmpty  := iState.ToCell( 3 );
  iNeigh  := iState.ToInteger( 4 );
  iCount  := iState.ToInteger( 5 );
  iArea   := CellularArea( iState, 6 );
  iStrict := iState.ToBoolean( 7, False );
  i := 0;
  while i < iCount do
  begin
    iCoord := iArea.RandomCoord( iRNG );
    if iStrict then
    begin
      iCell := iState.Map.getCell( iCoord );
      if ( iCell <> iFull ) and ( iCell <> iEmpty ) then Continue;
    end;
    Inc( i );
    // area lies inside the edge, so the block is inside the map
    iCnt := 0;
    for iC in NewArea( iCoord, 1 ) do
      if iState.Map.GetCell( iC ) = iFull then
      begin
        Inc( iCnt );
        if iCnt > iNeigh then
          Break;
      end;

    if iCnt > iNeigh
      then iState.Map.putCell( iCoord, iFull )
      else iState.Map.putCell( iCoord, iEmpty );
  end;
  Exit( 0 );
end;

// cellular_clear( map, cell, neighbours [, area ] )
// Sets each cell of area that is not cell to cell if at least neighbours of
// its 4 orthogonal neighbours are cell; in place, in row order. area defaults
// to the map without its edge and is clamped to it. Same definition as nv's
// cellular_clear.
function lua_dungen_cellular_clear( L : Plua_State ) : Integer; cdecl;
var iState : TLuaMapStack;
    iCoord : TCoord2D;
    iArea  : TArea;
    iCell  : Byte;
    iNeigh : Integer;
begin
  iState.Init( L );
  iCell   := iState.ToCell( 2 );
  iNeigh  := iState.ToInteger( 3 );
  iArea   := CellularArea( iState, 4 );

  for iCoord in iArea do
    if iState.Map.getCell( iCoord ) <> iCell then
      if iState.Map.CellsCrossAround( iCoord, [ iCell ] ) >= iNeigh then
         iState.Map.putCell( iCoord, iCell );
  Exit( 0 );
end;

const
  FloodStepX : array[0..7] of Integer = ( 1, -1, 0,  0, 1, -1,  1, -1 );
  FloodStepY : array[0..7] of Integer = ( 0,  0, 1, -1, 1,  1, -1, -1 );

// flood_fill( map, start, from_cells, to_cell [, area [, diagonal ]] )
// Changes every cell of area in from_cells that is connected to start through
// cells of area in from_cells into to_cell, and returns their count, start
// included. Cells connect through their 4 orthogonal neighbours, or through
// all 8 with diagonal. Returns 0 without changes when start is outside area or
// not in from_cells. area defaults to the map and is clamped to it. to_cell in
// from_cells is an error. Same definition as nv's flood_fill.
function lua_dungen_flood_fill( L : Plua_State ) : Integer; cdecl;
var iState : TLuaMapStack;
    iStart : TCoord2D;
    iCur   : TCoord2D;
    iC     : TCoord2D;
    iFrom  : TFlags;
    iTo    : Byte;
    iArea  : TArea;
    iSteps : Integer;
    iStep  : Integer;
    iQueue : array of TCoord2D;
    iHead  : Integer;
    iTail  : Integer;
begin
  iState.Init( L );
  iStart := iState.ToCoord( 2 );
  iFrom  := iState.ToCellSet( 3 );
  iTo    := iState.ToCell( 4 );
  iArea  := iState.ToOptionalArea( 5 );
  if iState.ToBoolean( 6, False )
    then iSteps := 8
    else iSteps := 4;
  if iTo in iFrom then iState.Error( 'flood_fill : to_cell is in from_cells!' );
  iState.Map.Area.Clamp( iArea );

  if ( not iArea.Contains( iStart ) ) or ( not ( iState.Map.GetCell( iStart ) in iFrom ) ) then
  begin
    iState.Push( 0 );
    Exit( 1 );
  end;

  // a cell is queued once: changing it to to_cell takes it out of from_cells
  Initialize( iQueue );
  SetLength( iQueue, ( iArea.Width + 1 ) * ( iArea.Height + 1 ) + 1 );
  iState.Map.PutCell( iStart, iTo );
  iQueue[ 0 ] := iStart;
  iHead := 0;
  iTail := 1;
  while iHead < iTail do
  begin
    iCur := iQueue[ iHead ];
    Inc( iHead );
    for iStep := 0 to iSteps - 1 do
    begin
      iC := NewCoord2D( iCur.X + FloodStepX[ iStep ], iCur.Y + FloodStepY[ iStep ] );
      if iArea.Contains( iC ) and ( iState.Map.GetCell( iC ) in iFrom ) then
      begin
        iState.Map.PutCell( iC, iTo );
        iQueue[ iTail ] := iC;
        Inc( iTail );
      end;
    end;
  end;
  iState.Push( iTail );
  Exit( 1 );
end;

// fill_unreachable( map, start, passable_cells, fill_cell [, area [, diagonal [, max_count ]]] )
// Changes every cell of area in passable_cells that is not connected to start
// ( connected as in flood_fill ) into fill_cell, and returns their count.
// Returns nil without changes when start is outside area or not in
// passable_cells, or when more than max_count ( omitted: no limit ) cells
// would change. area defaults to the map and is clamped to it. Same definition
// as nv's fill_unreachable.
function lua_dungen_fill_unreachable( L : Plua_State ) : Integer; cdecl;
var iState    : TLuaMapStack;
    iStart    : TCoord2D;
    iCur      : TCoord2D;
    iC        : TCoord2D;
    iPassable : TFlags;
    iFill     : Byte;
    iArea     : TArea;
    iSteps    : Integer;
    iStep     : Integer;
    iMax      : Integer;
    iCount    : Integer;
    iQueue    : array of TCoord2D;
    iHead     : Integer;
    iTail     : Integer;
begin
  iState.Init( L );
  iStart    := iState.ToCoord( 2 );
  iPassable := iState.ToCellSet( 3 );
  iFill     := iState.ToCell( 4 );
  iArea     := iState.ToOptionalArea( 5 );
  if iState.ToBoolean( 6, False )
    then iSteps := 8
    else iSteps := 4;
  iMax      := iState.ToInteger( 7, -1 );
  iState.Map.Area.Clamp( iArea );

  if ( not iArea.Contains( iStart ) ) or ( not ( iState.Map.GetCell( iStart ) in iPassable ) ) then
  begin
    lua_pushnil( L );
    Exit( 1 );
  end;

  // reached cells are marked with vlfFloodFill
  iState.Map.ClearLightMapBits( [vlfFloodFill] );
  Initialize( iQueue );
  SetLength( iQueue, ( iArea.Width + 1 ) * ( iArea.Height + 1 ) + 1 );
  iState.Map.LightFlag[ iStart, vlfFloodFill ] := True;
  iQueue[ 0 ] := iStart;
  iHead := 0;
  iTail := 1;
  while iHead < iTail do
  begin
    iCur := iQueue[ iHead ];
    Inc( iHead );
    for iStep := 0 to iSteps - 1 do
    begin
      iC := NewCoord2D( iCur.X + FloodStepX[ iStep ], iCur.Y + FloodStepY[ iStep ] );
      if iArea.Contains( iC ) and ( not iState.Map.LightFlag[ iC, vlfFloodFill ] )
         and ( iState.Map.GetCell( iC ) in iPassable ) then
      begin
        iState.Map.LightFlag[ iC, vlfFloodFill ] := True;
        iQueue[ iTail ] := iC;
        Inc( iTail );
      end;
    end;
  end;

  iCount := 0;
  for iC in iArea do
    if ( not iState.Map.LightFlag[ iC, vlfFloodFill ] ) and ( iState.Map.GetCell( iC ) in iPassable ) then
      Inc( iCount );

  if ( iMax >= 0 ) and ( iCount > iMax ) then
  begin
    iState.Map.ClearLightMapBits( [vlfFloodFill] );
    lua_pushnil( L );
    Exit( 1 );
  end;

  if iCount > 0 then
    for iC in iArea do
      if ( not iState.Map.LightFlag[ iC, vlfFloodFill ] ) and ( iState.Map.GetCell( iC ) in iPassable ) then
        iState.Map.PutCell( iC, iFill );
  iState.Map.ClearLightMapBits( [vlfFloodFill] );
  iState.Push( iCount );
  Exit( 1 );
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
var iState       : TLuaMapStack;
    iCode        : AnsiString;
    iTile        : TTileRecord;
    iLine, iRow  : Word;
    iTranslation : array[0..255] of Byte;
    iGylph       : AnsiString;
    iAscii       : Boolean;
    iLast        : Integer;
begin
  iState.Init( L );
  iCode  := DelChars( DelChars( DelChars( TrimSet( iState.ToString( 2 ), [#1..#32] ), #13 ), #9 ), ' ' );
  iAscii := iState.ToBoolean( 4, False );
  lua_settop( L, 3 );
  iTile.SizeY := WordCount( iCode, [#10] );
  iLast := Pos( #10, iCode ) - 1;
  if iLast < 0
    then iTile.SizeX := Length( iCode )
    else iTile.SizeX := Pos( #10, iCode ) - 1;
  iTile.Data  := GetMem( iTile.SizeX * iTile.SizeY );
  if iAscii
    then iTile.Ascii := GetMem( iTile.SizeX * iTile.SizeY )
    else iTile.Ascii := nil;

  {$HINTS OFF}
  FillChar( iTranslation, 255, 0 );
  {$HINTS ON}

  // TODO: error reporting
  if lua_istable( L, 3 ) then
  begin
    lua_pushnil( L );
    while lua_next( L, 3 ) <> 0 do
    begin
      // uses 'key' (at index -2) and 'value' (at index -1) */
      if lua_isstring( L, -2 ) and ( lua_objlen( L, -2 ) = 1 ) then
        iTranslation[Ord( lua_tostring( L, -2 )[1] )] := Byte( iState.ToCell( -1 ) );
      // removes 'value'; keeps 'key' for next iteration */
      lua_pop( L, 1 );
    end;
  end;

  for iLine := 0 to iTile.SizeX - 1 do
    for iRow := 0 to iTile.SizeY - 1 do
    begin
      iGylph := iCode[iRow * ( iTile.SizeX + 1 ) + iLine + 1];
      // TODO: check for errors
      iTile.Data[iRow * iTile.SizeX + iLine] := iTranslation[Ord( iGylph[1] )];
      if iAscii then iTile.Ascii[iRow * iTile.SizeX + iLine] := Ord( iGylph[1] );
    end;

  vlua_pushtile( L, iTile );
  Exit( 1 );
end;

function lua_dungen_tile_place( L : Plua_State ) : Integer; cdecl;
var iState : TLuaMapStack;
    iTile  : PTileRecord;
    iCoord : TCoord2D;
    iX, iY : Word;
    iCell  : Byte;
begin
  iState.Init( L );
  iCoord := iState.ToCoord( 2 );
  iTile  := vlua_toptile( L, 3 );

  for iX := 0 to iTile^.SizeX - 1 do
    for iY := 0 to iTile^.SizeY - 1 do
    begin
      iCell := iTile^.Data[iY * iTile^.SizeX + iX];
      if iCell <> 0 then
        iState.Map.PutCell( iCoord + NewCoord2D( iX, iY ), iCell );
    end;

  Exit( 0 );
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

function lua_dungen_tile_flip_random( L : PLua_State ) : Integer; cdecl;
var iRNG : TRNG;
begin
  iRNG := TLuaContext.RequireRNG( L );
  case iRNG.RLongInt( 4 ) of
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

function lua_dungen_tile_get( L : Plua_State ) : Integer; cdecl;
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

function lua_dungen_tile_set( L : Plua_State ) : Integer; cdecl;
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
  dungenlib_f : array[0..14] of luaL_Reg = (
    ( Name : 'tile_new'; func : @lua_dungen_tile_new ),
    ( Name : 'tile_place'; func : @lua_dungen_tile_place; ),
    ( Name : 'plot_line'; func : @lua_dungen_plot_line; ),
    ( Name : 'get_endpoints'; func : @lua_dungen_get_endpoints; ),
    ( Name : 'fill_dead_ends'; func : @lua_dungen_fill_dead_ends; ),
    ( Name : 'transform_around'; func : @lua_dungen_transform_around; ),
    ( Name : 'random_door_spot'; func : @lua_dungen_random_door_spot; ),
    ( Name : 'read_rooms'; func : @lua_dungen_read_rooms; ),
    ( Name : 'run_drunkard_walk'; func : @lua_dungen_run_drunkard_walk; ),
    ( Name : 'cellular_init'; func : @lua_dungen_cellular_init; ),
    ( Name : 'cellular_random'; func : @lua_dungen_cellular_random; ),
    ( Name : 'cellular_clear'; func : @lua_dungen_cellular_clear; ),
    ( Name : 'flood_fill'; func : @lua_dungen_flood_fill; ),
    ( Name : 'fill_unreachable'; func : @lua_dungen_fill_unreachable; ),
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
    ( Name : 'get'; func : @lua_dungen_tile_get ),
    ( Name : 'set'; func : @lua_dungen_tile_set ),
    ( Name : 'get_ascii'; func : @lua_dungen_tile_ascii_get ),
    ( Name : 'set_ascii'; func : @lua_dungen_tile_ascii_set ),
    ( Name : '__gc'; func : @lua_dungen_tile_gc ),
    ( Name : nil; func : nil; ) );

procedure RegisterDungenClass( L : Plua_State; ObjectName : AnsiString = '' );
begin
  if ObjectName = '' then
    ObjectName := 'dungen';
  vlua_newmetatable( L, VALKYRIE_DUNGEN );
  luaL_register( L, PChar( ObjectName ), dungenlib_f );

  vlua_newmetatable( L, VALKYRIE_DUNGEN_TILE );
  lua_pushvalue( L, -1 );
  lua_setfield( L, -2, '__index' );
  luaL_register( L, nil, dungentile_f );
end;

end.
