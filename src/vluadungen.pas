unit vluadungen;

{$mode objfpc}
interface

uses Classes, SysUtils, vlualibrary, vrltools, vluamapnode;

procedure RegisterDungenClass( L : Plua_State; ObjectName : AnsiString = '' );

implementation

uses vutil, vluatools, vluaext, strutils;

const
  VALKYRIE_DUNGEN      = 'valkyrie.dungen';
  VALKYRIE_DUNGEN_TILE = 'valkyrie.dungen.tile';

function lua_dungen_plot_line( L : Plua_State ) : Integer; cdecl;
var iState      : TLuaMapState;
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
  iBlockCells := iState.ToCellset( 5 );
  iArea       := iState.Map.Area;

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

function lua_dungen_get_endpoints( L : Plua_State ) : Integer; cdecl;
var iState   : TLuaMapState;
    iCoord   : TCoord2D;
    iWhere   : TCoord2D;
    iStep    : TCoord2D;
    iHoriz   : Boolean;
    iCellSet : TFlags;
    iCell    : Byte;
begin
  iState.Init( L );
  iWhere   := iState.ToCoord( 2 );
  iHoriz   := iState.ToBoolean( 3 );
  iCellSet := iState.ToCellSet( 4 );

  iCoord := iWhere;
  if iHoriz
    then iStep := NewCoord2D( +1, 0 )
    else iStep := NewCoord2D( 0, +1 );
  while True do
  begin
    iCoord += iStep;
    iCell := iState.Map.GetCell( iCoord );
    if not ( iCell in iCellSet ) then
    begin
      lua_pushinteger( L, iCell );
      Break;
    end;
  end;
  iCoord := iWhere;
  while True do
  begin
    iCoord -= iStep;
    iCell := iState.Map.GetCell( iCoord );
    if not ( iCell in iCellSet ) then
    begin
      lua_pushinteger( L, iCell );
      Break;
    end;
  end;
  Exit( 2 );
end;

function lua_dungen_read_rooms( L : Plua_State ) : Integer; cdecl;
var iState   : TLuaMapState;
    iMap     : TLuaMapNode;
    iArea    : TArea;
    iCell    : Byte;
    iCount   : Word;
    iC       : TCoord2D;
    iRx, iRy : TCoord2D;

  function RoomStart( aX, aY : Integer ) : Boolean;
  begin
    Exit( ( iMap.GetCell( NewCoord2D( aX + 1, aY ) ) = iCell ) and ( iMap.GetCell(
      NewCoord2D( aX, aY + 1 ) ) = iCell ) and ( iMap.GetCell( NewCoord2D( aX - 1, aY ) ) <> iCell ) and
      ( iMap.GetCell( NewCoord2D( aX, aY - 1 ) ) <> iCell ) );
  end;

begin
  iState.Init( L );
  iArea  := iState.ToArea( 2 );
  iCell  := iState.ToCell( 3 );
  iCount := 1;
  iMap   := iState.Map;
  iC     := iArea.a;
  lua_createtable( L, 0, 0 );
  repeat
    if iMap.GetCell( iC ) = iCell then
      if RoomStart( iC.x, iC.y ) then
      begin
        iRx := iC;
        iRy := iC;
        repeat Inc( iRx.x ); until iMap.GetCell( iRx ) <> iCell;
        repeat Inc( iRy.y ); until iMap.GetCell( iRy ) <> iCell;
        vlua_pusharea( L, NewArea( iC, NewCoord2D( iRx.x - 1, iRy.y - 1 ) ) );
        lua_rawseti( L, -2, iCount );
        Inc( iCount );
      end;
    Inc( iC.x );
    if iC.x > iArea.b.x then
    begin
      Inc( iC.y );
      iC.x := iArea.a.x;
    end;
  until iC.y > iArea.b.y;
  Exit( 1 );
end;

function lua_dungen_run_drunkard_walk( L : Plua_State ) : Integer; cdecl;
var iState  : TLuaMapState;
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
  for iSteps := 1 to iCount do
  begin
    if not iArea.Contains( iCoord ) then
      if iBreak then
        Exit( 0 )
      else
        iArea.Clamp( iCoord );

    if not ( iState.Map.GetCell( iCoord ) in iIgnore ) then
      iState.Map.PutCell( iCoord, iCell );
    iCoord.RandomShift( 1 );
  end;
  Exit( 0 );
end;

function lua_dungen_cellular_init( L : Plua_State ) : Integer; cdecl;
var iState  : TLuaMapState;
    iCoord  : TCoord2D;
    iArea   : TArea;
    iFull   : Byte;
    iEmpty  : Byte;
    iChance : Single;
begin
  iState.Init( L );
  iFull   := iState.ToCell( 2 );
  iEmpty  := iState.ToCell( 3 );
  iChance := iState.ToFloat( 4 );
  iArea   := iState.ToOptionalArea( 5 );
  if iArea = iState.Map.Area
    then iArea := iArea.Shrinked( 1 )
    else iState.Map.Area.Clamp( iArea );

  for iCoord in iArea do
    if Random < iChance
      then iState.Map.putCell( iCoord, iFull )
      else iState.Map.putCell( iCoord, iEmpty );

  Exit( 0 );
end;

function lua_dungen_cellular_random( L : Plua_State ) : Integer; cdecl;
var iState  : TLuaMapState;
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
  iFull   := iState.ToCell( 2 );
  iEmpty  := iState.ToCell( 3 );
  iNeigh  := iState.ToInteger( 4 );
  iCount  := iState.ToInteger( 5 );
  iArea   := iState.ToOptionalArea( 6 );
  iStrict := iState.ToBoolean( 7, False );
  iState.Map.Area.Clamp( iArea );
  i := 0;
  repeat
    Inc( i );
    iCoord := iArea.RandomCoord;
    if iStrict then
    begin
      iCell := iState.Map.getCell( iCoord );
      if ( iCell <> iFull ) and ( iCell <> iEmpty ) then
      begin
        Dec( i );
        Continue;
      end;
    end;
    iCnt := 0;
    for iC in NewArea( iCoord, 1 ).Clamped(iArea) do
      if iState.Map.GetCell( iC ) = iFull then
      begin
        Inc( iCnt );
        if iCnt > iNeigh then
          Break;
      end;

    if iCnt > iNeigh
      then iState.Map.putCell( iCoord, iFull )
      else iState.Map.putCell( iCoord, iEmpty );
  until i >= iCount;
  Exit( 0 );
end;

function lua_dungen_cellular_clear( L : Plua_State ) : Integer; cdecl;
var iState : TLuaMapState;
    iCoord : TCoord2D;
    iArea  : TArea;
    iCell  : Byte;
    iNeigh : Integer;
begin
  iState.Init( L );
  iCell   := iState.ToCell( 2 );
  iNeigh  := iState.ToInteger( 3 );
  iArea   := iState.ToOptionalArea( 4 );

  iState.Map.Area.Clamp( iArea );
  for iCoord in iArea do
    if iState.Map.getCell( iCoord ) <> iCell then
      if iState.Map.CellsCrossAround( iCoord, [ iCell ] ) >= iNeigh then
         iState.Map.putCell( iCoord, iCell );
  Exit( 0 );
end;

function lua_dungen_flood_fill( L : Plua_State ) : Integer; cdecl;
var iState : TLuaMapState;
    iStart : TCoord2D;
    iCur   : TCoord2D;
    iC     : TCoord2D;
    iFrom  : TCellSet;
    iTo    : Byte;
    iArea  : TArea;
    iAround: TArea;

    iQueue : array of TCoord2D;
    iHead  : Integer;
    iTail  : Integer;
    iCount : Integer;

    iFFrom : Boolean;
    iFTo   : Boolean;
begin
  iState.Init( L );
  iStart  := iState.ToCoord( 2 );
  iFTo    := iState.IsFunction( 3 );
  if not iFTo   then iTo := iState.ToCell( 3 );
  iFFrom  := iState.IsFunction( 4 );
  if not iFFrom then iFrom   := iState.ToCellSet( 4 );
  iArea   := iState.ToOptionalArea( 5 );
  iState.Map.Area.Clamp( iArea );

  if not iState.Map.isProperCoord( iStart ) then Exit( 0 );
  if not iArea.Contains( iStart )           then Exit( 0 );

  iState.Map.ClearLightMapBits( [vlfFlood] );
  Initialize( iQueue );
  SetLength( iQueue, ( iArea.Width + 1 ) * ( iArea.Height + 1 ) + 1 );
  iHead  := 0;
  iTail  := 0;
  iCount := 0;

  iState.Map.LightFlag[ iStart, vlfFlood ] := True;
  if iFTo
    then iState.CallFunction( 3, [LuaCoord( iStart )] )
    else iState.Map.putCell( iStart, iTo );

  iQueue[ iTail ] := iStart;
  Inc( iTail );
  while iHead < iTail do
  begin
    iCur := iQueue[iHead];
    Inc( iHead );
    iAround := NewArea( iCur, 1 ).Clamped( iArea );
    for iC in iAround do
      if iC <> iCur then
        if not iState.Map.LightFlag[ iC, vlfFlood ] then
        begin
          iState.Map.LightFlag[ iC, vlfFlood ] := True;
          if ( ( not iFFrom ) and ( iState.Map.getCell( iC ) in iFrom ) ) or
             ( iFFrom and Boolean( iState.CallFunction( 4, [LuaCoord( iC )] ) ) ) then
          begin
            if iFTo
              then iState.CallFunction( 3, [LuaCoord( iC )] )
              else iState.Map.putCell( iC, iTo );
            iQueue[iTail] := iC;
            Inc( iTail );
            Inc( iCount );
          end;
        end;
  end;
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
var iState       : TLuaMapState;
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
var iState : TLuaMapState;
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
  dungenlib_f : array[0..10] of luaL_Reg = (
    ( Name : 'tile_new'; func : @lua_dungen_tile_new ),
    ( Name : 'tile_place'; func : @lua_dungen_tile_place; ),
    ( Name : 'plot_line'; func : @lua_dungen_plot_line; ),
    ( Name : 'get_endpoints'; func : @lua_dungen_get_endpoints; ),
    ( Name : 'read_rooms'; func : @lua_dungen_read_rooms; ),
    ( Name : 'run_drunkard_walk'; func : @lua_dungen_run_drunkard_walk; ),
    ( Name : 'cellular_init'; func : @lua_dungen_cellular_init; ),
    ( Name : 'cellular_random'; func : @lua_dungen_cellular_random; ),
    ( Name : 'cellular_clear'; func : @lua_dungen_cellular_clear; ),
    ( Name : 'flood_fill'; func : @lua_dungen_flood_fill; ),
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
