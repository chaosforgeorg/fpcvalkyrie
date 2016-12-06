{$INCLUDE valkyrie.inc}
// @abstract(Dungeon Generator for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 9, 2005)
//
// THIS UNIT IS EXPERIMENTAL!
//
// Implements various dungeon generation routines. The Map coordination
// is handled by overloading the TDungeonBuilder class, and it's
// three abstract functions : PutCell, GetCell, and PutRoom (optionaly).
//
// In order to prevent Translations of the generated dungeons it is
// advised to set the tile ID's beforehand using SetWallTile,
// SetFloorTile and SetDoorTile.
//
// TODO: Make MazeDungeon respect zone!

unit vdungen;
interface
uses vutil, vnode, vmath, vrltools, vmaparea;

// Default TileID's for the generator.
const DUNGEN_FLOOR  = 201;
      DUNGEN_FLOOR2 = 202;
      DUNGEN_FLOOR3 = 203;
      DUNGEN_WALL   = 204;
      DUNGEN_WALL2  = 205;
      DUNGEN_WALL3  = 206;
      DUNGEN_WATER  = 207;
      DUNGEN_DOOR   = 208;

      DUNGEN_TILE_IGNORE  = 230;
      DUNGEN_TILE_HOTSPOT = 231;
      DUNGEN_TILE_HOTWALL = 232;

      // GetRCell out of bounds error code.
      DUNGEN_ERROR  = 255;
      
      // Horizontal constant for PlotLine and DividedDungeon.
      DUNGEN_HORIZONTAL = True;
      // Vertical constant for PlotLine and DividedDungeon.
      DUNGEN_VERTICAL   = False;

      // Out of bounds code for Scan.
      DUNGEN_SCANINVALID = -1;

      // Prepared Cellsets.
      DUNGEN_FLOORS     = [DUNGEN_FLOOR,DUNGEN_FLOOR2,DUNGEN_FLOOR3];
      DUNGEN_WALLS      = [DUNGEN_WALL,DUNGEN_WALL2,DUNGEN_WALL3];
      DUNGEN_PASSABLE   = DUNGEN_FLOORS+[DUNGEN_WATER];
      DUNGEN_INPASSABLE = DUNGEN_WALLS+[DUNGEN_DOOR];
      
      MAXZONES = 50;
      
// Set of Cells.
type TCellSet = set of Byte;

const StandardTranslation : TPrintableCharToByte = (
  {!}0,
  {"}0,
  {#}DUNGEN_WALL,
 {_$}DUNGEN_WALL3,
  {%}DUNGEN_WALL2,
  {&}DUNGEN_TILE_HOTSPOT,
  {'}0,
  {(}0,
  {)}0,
  {*}DUNGEN_TILE_HOTSPOT,
  {+}DUNGEN_DOOR,
  {,}DUNGEN_FLOOR2,
  {-}0,
  {.}DUNGEN_FLOOR,
  {/}0,
  {0-9}0,0,0,0,0,0,0,0,0,0,
  {:}0,
  {;}DUNGEN_FLOOR3,
  {<}0,
  {=}DUNGEN_WATER,
  {>}0,
  {?}0,
  {@}0,
  {A-Z}0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  {[}0,
  {\}0,
  {]}0,
  {^}0,
  {_}0,
  {`}DUNGEN_TILE_IGNORE,
  {a-z}0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  (*{*)0,
  {|}0,
  (*}*)0,
  {~}0
);


type

{ TDungeonTile }

TDungeonTile = class(TVObject)
    Area : TArea;
    Map  : array of array of Byte;
    constructor Create( SizeX, SizeY : Byte );
    constructor Create( const TileCode : Ansistring; const Translation : TPrintableCharToByte );
    procedure Decode( const TileCode : Ansistring; const Translation : TPrintableCharToByte );
    procedure Resize( SX, SY : Byte );
    procedure Expand( Size : Byte );
    procedure Expand( const SizeX, SizeY : array of Byte );
    function SizeX : Byte;
    function SizeY : Byte;
  end;

type ICellIDSource = interface['valkyrie.icellidsource']
  function IDtoCell( const aID : AnsiString ) : Byte;
  function CellToID( const aCell : Byte ) : AnsiString;
end;


// The Dungeon Builder class. Should be overriden.
type

{ TDungeonBuilder }

TDungeonBuilder = class(TMapArea, IMapArea, ICellIDSource)
  // Initialization (takes the size of the level as arguments).
  constructor Create(const sx,sy : Word; aTarget : IMapArea = nil; aCellIDSource : ICellIDSource = nil);

  // May be overloaded if a system of rooms is implemented.
  procedure PutRoom(const aArea : TArea); virtual;

  // Sets WallCell
  procedure SetWallCell(const newWallCell : Byte);
  // Sets FloorCell
  procedure SetFloorCell(const newFloorCell : Byte);
  // Sets DoorCell
  procedure SetDoorCell(const newDoorCell : Byte);

  procedure FillCircle(const Where : TCoord2D; radius : Word; const CellID : Byte);
  procedure RestoreWalls(const CellID : Byte);
  function RandomCellSquare(const Cell : TCellSet) : boolean;
  function RandomCellNear(const Where : TCoord2D; const Range : Byte; const Cell : TCellSet) : boolean;
  function Scan(aArea : TArea; const Ignore : TCellSet; IgnoreBoundaries : Boolean = False) : LongInt;
  procedure RandomCircles(const Hits, Radius, CellID : byte);
  procedure DrunkardWalk(const aStart : TCoord2D; const Steps : DWord; const CellID : Byte; const Ignore : TCellSet = []; const BreakOnEdge : Boolean = False);
  procedure DrunkardWalk(const Amount, Steps : DWord; const CellID : Byte; const Ignore : TCellSet = []; const BreakOnEdge : Boolean = False);
  procedure DrunkardWalk(const Amount, Steps : DWord; const CellID : Byte; const Edge1Cells, Edge2Cells : TCellSet; const Ignore : TCellSet = []; const BreakOnEdge : Boolean = False);
  procedure PlotLine(const Where : TCoord2D;  const Horizontal : Boolean; const CellID : Byte; const PointCellID : Byte = 0; const BlockCells : TCellSet = []; const SubstituteCell : Byte = 0);
  function GetEndpoints( const Where : TCoord2D; const Horizontal : Boolean; Ignore : TCellSet ) : TCellSet;
  
  procedure CityDungeon(const Rooms,  MaxRSizeX, MaxRSizeY : Word; GoodFloors : TCellSet = []; UsedFloorCell : Byte = 0; AroundCell : Byte = 0; Doors : Byte = 1);
  procedure DividedDungeon(const Divisions : Byte; const FirstDivision : Boolean; const AdditionalDoors : Byte);
  procedure ClassicDungeon(const RecDepth : DWord; const DoorChance : byte);
  procedure MazeDungeon( aFloorTile, aWallTile : Byte; aGranularity : Byte; aTries : Word; aMinLength, aMaxLength : Byte );

  procedure FromString( const MapCode : AnsiString; const Translation : TPrintableCharToByte ); overload;
  procedure FromString( const aWhere : TCoord2D; const MapCode : AnsiString; const Translation : TPrintableCharToByte ); overload;
  procedure Place( Tile : TDungeonTile; const Pos : TCoord2D );
  procedure pushZone(const aArea : TArea);
  procedure pushZone(const A,B : TCoord2D);
  function IDtoCell( const aID : AnsiString ) : Byte; virtual;
  function CellToID( const aCell : Byte ) : AnsiString; virtual;
  function popZone : boolean;
  procedure clearZones;

  public
  // The dimensions of the currently generated part of the level.
  // All the functions work only in this area.
  // Area      : TArea;
  // The current cell coordinates. Modified by Find* and Random* functions.
  FoundCell : TCoord2D;
  // Standard Cell ID's.
  WallCell, FloorCell, DoorCell : Byte;
  
  ZoneArray    : array[0..MAXZONES] of TArea;
  Zones        : Word;
  CellIDSource : ICellIDSource;
end;



implementation

uses sysutils, strutils;

{$HINTS OFF}
procedure TDungeonBuilder.PutRoom(const aArea : TArea);
begin

end;
{$HINTS ON}



procedure TDungeonBuilder.RandomCircles(const Hits, Radius, CellID : byte);
var HitCount : byte;
begin
  for HitCount := 1 to Hits do
    FillCircle( Area.RandomCoord, Radius, CellID );
end;

procedure TDungeonBuilder.FillCircle(const Where : TCoord2D; radius : Word; const CellID : Byte);
var Coord : TCoord2D;
begin
  for Coord in NewArea( Where, Radius ).Clamped( Area ) do
    if Distance( Coord, Where ) <= Radius then
      PutCell( Coord, CellID );
end;

procedure TDungeonBuilder.DrunkardWalk( const aStart: TCoord2D; const Steps: DWord; const CellID: Byte;
  const Ignore: TCellSet; const BreakOnEdge: Boolean );
var iSteps : DWord;
    iCoord : TCoord2D;
begin
  iCoord := aStart;
  if Steps = 0 then Exit;
  for iSteps := 1 to Steps do
  begin
    if not Area.Shrinked.Contains( iCoord ) then
      if BreakOnEdge then Exit
      else
        Area.Clamp( iCoord );

    if not ( GetCell( iCoord ) in Ignore ) then PutCell( iCoord, CellID );

    iCoord.RandomShift(1);
  end;
end;

procedure TDungeonBuilder.DrunkardWalk(const Amount, Steps : DWord; const CellID : Byte; const Ignore : TCellSet; const BreakOnEdge : Boolean);
var iCount : DWord;
begin
  if Amount = 0 then Exit;
  for iCount := 1 to Amount do
    DrunkardWalk( Area.RandomInnerCoord, Steps, CellID, Ignore, BreakOnEdge );
end;

procedure TDungeonBuilder.DrunkardWalk(const Amount, Steps: DWord; const CellID: Byte; const Edge1Cells,
  Edge2Cells: TCellSet; const Ignore: TCellSet; const BreakOnEdge: Boolean);
var iCount : DWord;
    iCoord : TCoord2D;
begin
  if Amount = 0 then Exit;
  for iCount := 1 to Amount do
  begin
    repeat
      iCoord := Area.RandomInnerCoord;
    until ( CrossAround( iCoord, Edge1Cells ) > 0 ) and
          ( CrossAround( iCoord, Edge2Cells ) > 0 );
    DrunkardWalk( iCoord, Steps, CellID, Ignore, BreakOnEdge );
  end;
end;

procedure TDungeonBuilder.PlotLine(const Where : TCoord2D; const Horizontal : Boolean;
  const CellID : Byte; const PointCellID : Byte; const BlockCells : TCellSet;
  const SubstituteCell : Byte = 0);
var Coord : TCoord2D;
    Step  : TCoord2D;
begin
  if Horizontal
    then Step := NewCoord2D( +1,  0 )
    else Step := NewCoord2D(  0, +1 );

  Coord := Where;

  while Coord.Horiz(Horizontal) < Area.B.Horiz(Horizontal) do
  begin
    Coord += Step;
    if GetCell( Coord ) in BlockCells then
      if SubstituteCell = 0 then Break
                            else PutCell( Coord, SubstituteCell )
    else PutCell( Coord, CellID );
  end;
  Coord := Where;
  while Coord.Horiz(Horizontal) > Area.A.Horiz(Horizontal) do
  begin
    Coord -= Step;
    if GetCell( Coord ) in BlockCells then
      if SubstituteCell = 0 then Break
                            else PutCell( Coord, SubstituteCell )
    else PutCell( Coord, CellID );
  end;

  if PointCellID = 0 then
    if (GetCell(Where) in BlockCells) and (SubstituteCell <> 0) then PutCell(Where,SubstituteCell)
    else PutCell( Where, CellID )
  else PutCell( Where, PointCellID );
end;

function TDungeonBuilder.GetEndpoints(const Where: TCoord2D;
  const Horizontal: Boolean; Ignore: TCellSet): TCellSet;
var cell  : Byte;
    Coord : TCoord2D;
    Step  : TCoord2D;
begin
  GetEndpoints := [];
  Coord := Where;
  if Horizontal
    then Step := NewCoord2D( +1,  0 )
    else Step := NewCoord2D(  0, +1 );
  while true do
  begin
    Coord += Step;
    cell := GetCell( Coord );
    if not (cell in Ignore) then
    begin
      Include(GetEndpoints, cell );
      Break;
    end;
  end;
  Coord := Where;
  while true do
  begin
    Coord -= Step;
    cell := GetCell( Coord );
    if not (cell in Ignore) then
    begin
      Include(GetEndpoints, cell );
      Break;
    end;
  end;
end;

constructor TDungeonBuilder.Create(const sx,sy : Word; aTarget : IMapArea; aCellIDSource : ICellIDSource);
begin
  CellIDSource := aCellIDSource;
  FoundCell.Create( 1, 1 );
  WallCell  := DUNGEN_WALL;
  FloorCell := DUNGEN_FLOOR;
  DoorCell  := DUNGEN_DOOR;
  Zones := 0;
  with Area do
  begin
    A.Create(1,1);
    B.Create(sx,sy);
  end;
  inherited Create( Area, aTarget );
  ZoneArray[0] := Area;
end;

procedure TDungeonBuilder.ClassicDungeon(const RecDepth : DWord; const DoorChance : byte);
var v1        : DWord;
    Direction : TDirection;
    Coord     : TCoord2D;

  function CreateRandomRoom( Where : TCoord2D; Direction : TDirection ) : Boolean;
  var SizeX, SizeY : byte;
      epos : word;
      Room : TArea;
  begin
    CreateRandomRoom := False;
    if not Direction.isSquare then Exit;

    SizeX := Random(10)+3;
    SizeY := Random(5)+3;
    
    if Direction.x <> 0 then
    begin
      epos := Random(SizeY)+1;
      Room := NewArea( Where.ifIncY( -epos ), Where.ifIncY( SizeY+1-epos ) );
      if Direction.x > 0 then Room.B.X += SizeX+1;
      if Direction.x < 0 then Room.A.X -= SizeX+1;
    end
    else
    begin
      epos := Random(SizeX)+1;
      Room := NewArea( Where.ifIncX( -epos ), Where.ifIncX( SizeX+1-epos ) );
      if Direction.y > 0 then Room.B.Y += SizeY+1;
      if Direction.y < 0 then Room.A.Y -= SizeY+1;
    end;
    
    if Scan( Room, [WallCell] ) < 2 then
    begin
      Fill( Room.Shrinked, FloorCell );
      CreateRandomRoom := true;
    end;
  end;
  
  procedure RemoveRandomHall( Where : TCoord2D; Direction : TDirection );
  begin
    Direction.Reverse;
    while ( CrossAround( Where, [FloorCell] ) = 1 )
       and ( GetCell( Where ) = FloorCell) do
    begin
      PutCell( Where, WallCell );
      Where += Direction;
    end;
  end;
  
  function CreateRandomHall( var Where : TCoord2D; Direction : TDirection ) : boolean;
  var Size : byte;
      Corr : TArea;
  begin
    if not Direction.isSquare then Exit( False );
    Size := Random(10)+3;
    
    if Direction.x <> 0 then
    begin
      Corr := NewArea( Where.ifIncY(-1), Where.ifIncY( 1) );
      if Direction.x > 0 then Corr.B.X += Size+1;
      if Direction.x < 0 then Corr.A.X -= Size+1;
    end
    else
    begin
      Corr := NewArea( Where.ifIncX(-1), Where.ifIncX( 1) );
      if Direction.y > 0 then Corr.B.Y += Size+1;
      if Direction.y < 0 then Corr.A.Y -= Size+1;
    end;
    
    if Scan( Corr, [WallCell] ) < 3 then
    begin
      Corr := NewArea( Where, Where );
      if Direction.y > 0 then Corr.B.Y += Size;
      if Direction.y < 0 then Corr.A.Y -= Size;
      if Direction.x > 0 then Corr.B.X += Size;
      if Direction.x < 0 then Corr.A.X -= Size;
      Fill( Corr, FloorCell );
      Where.y += Direction.Y*Size;
      Where.x += Direction.X*Size;
      Exit( True );
    end;
    Exit( False );
  end;

  function ArcAround( Where : TCoord2D; Direction : TDirection ) : byte;
  var Coord : TCoord2D;
  begin
    ArcAround := 0;
    Coord := Where + Direction;
    if Direction.X <> 0 then
    begin
      if GetCell( Coord.ifIncX(-1) ) = FloorCell then Inc( ArcAround );
      if GetCell( Coord            ) = FloorCell then Inc( ArcAround );
      if GetCell( Coord.ifIncX( 1) ) = FloorCell then Inc( ArcAround );
    end;
    if Direction.Y <> 0 then
    begin
      if GetCell( Coord.ifIncY(-1) ) = FloorCell then Inc( ArcAround );
      if GetCell( Coord            ) = FloorCell then Inc( ArcAround );
      if GetCell( Coord.ifIncY( 1) ) = FloorCell then Inc( ArcAround );
    end;
  end;
  
  function ExitDirection( Where : TCoord2D ) : TDirection;
  var dx, dy : Integer;
      Found  : Boolean;
  begin
    dx := 0;
    dy := 0;
    Found := False;
    ExitDirection.Code := 0;
    if GetCell( Where.ifIncY( 1) ) = FloorCell then begin Found := not Found; if not Found then Exit; dy := 1; end;
    if GetCell( Where.ifIncY(-1) ) = FloorCell then begin Found := not Found; if not Found then Exit; dy := -1; end;
    if GetCell( Where.ifIncX( 1) ) = FloorCell then begin Found := not Found; if not Found then Exit; dx := 1; end;
    if GetCell( Where.ifIncX(-1) ) = FloorCell then begin Found := not Found; if not Found then Exit; dx := -1; end;
    ExitDirection.Create( dx, dy )
  end;
  procedure FindWall( var Place : TCoord2D; var Direction : TDirection );
  begin
    repeat
      Place := RanCoord([WallCell]);
      Direction := ExitDirection( Place );
    until (Direction.Code <> 0) and (Around( Place, [FloorCell] ) = ArcAround( Place, Direction ) );
    Direction.Reverse;
  end;
  
  function DoorPos( Where : TCoord2D ) : boolean;
  begin
    if (GetCell( Where.ifIncX(1) ) = WallCell) and (GetCell( Where.ifIncX(-1) ) = WallCell) then Exit(True);
    if (GetCell( Where.ifIncY(1) ) = WallCell) and (GetCell( Where.ifIncY(-1) ) = WallCell) then Exit(True);
    Exit(False);
  end;


begin
  Fill(WallCell);
  RestoreWalls(DUNGEN_ERROR);

  repeat
    Coord.x := Random(Area.B.X-30)+15;
    Coord.y := Random(Area.B.Y-14)+7;
    Direction.RandomSquare;
  until CreateRandomRoom( Coord, Direction );

  V1 := 0;
  try
  repeat
    Inc(V1);
    FindWall( Coord, Direction );
    if CreateRandomHall( Coord, Direction ) then
      if not CreateRandomRoom( Coord, Direction ) then
        RemoveRandomHall( Coord, Direction );
  until V1 = RecDepth;
  except
    on e : EPlacementException do;
  end;
  for Coord in Area do
    if GetCell( Coord ) = FloorCell then
      if Around( Coord, [DoorCell]) = 0 then
        if Around( Coord, [FloorCell]) >= 3 then
          if DoorPos( Coord ) then
            if Random(100) < DoorChance then
               PutCell( Coord, DoorCell );

{  Write('Disabling dead ends      <');
  V1 := 0;
  repeat
   Inc(V1);
   DeadEnd;
   if V1 mod 3 = 0 then Write('.');
  until V1 = 100;}
  RestoreWalls(WallCell);
end;

procedure TDungeonBuilder.MazeDungeon( aFloorTile, aWallTile : Byte; aGranularity : Byte; aTries : Word; aMinLength, aMaxLength : Byte );
var iCoord  : TCoord2D;
    iDiff   : TCoord2D;
    iCount  : DWord;
    iLength : DWord;
begin
  for iCount := 1 to aTries do
  begin
    iCoord.x := aGranularity * ( Random( ( Area.B.X - 1 ) div aGranularity ) + 1 ) + 1;
    iCoord.y := aGranularity * ( Random( ( Area.B.Y - 1 ) div aGranularity ) + 1 ) + 1;
    if ( GetCell( iCoord ) <> aFloorTile ) or
       ( CrossAround( iCoord, [ aFloorTile ] ) <> 4 ) then
         Continue;
    iDiff := ZeroCoord2D;

    iLength := aMinLength + aGranularity * DWord(Random((aMaxLength-aMinLength) div aGranularity + 1));

    case Random(2) of
      0 : iDiff.x := Random(2)*2-1;
      1 : iDiff.y := Random(2)*2-1;
    end;

    while ( GetCell( iCoord + iDiff ) = aFloorTile) and ( iLength > 0 ) do
    begin
      PutCell( iCoord, aWallTile );
      iCoord += iDiff;
      Dec( iLength );
    end;
      PutCell( iCoord, aWallTile );
  end;

end;

procedure TDungeonBuilder.FromString(const MapCode: AnsiString; const Translation: TPrintableCharToByte);
begin
  FromString(NewCoord2D(0,0), MapCode, Translation);
end;

procedure TDungeonBuilder.FromString(const aWhere: TCoord2D; const MapCode: AnsiString;
  const Translation: TPrintableCharToByte);
var Temp  : AnsiString;
    SX    : Byte;
    SY    : Byte;
    Line  : Byte;
    Row   : Byte;
begin
  Temp := DelChars(DelChars(DelChars(TrimSet(MapCode,[#1..#32]),#13),#9),' ');
  SY := WordCount(Temp,[#10]);
  SX := Pos(#10,Temp)-1;
  for Line := 1 to SY do
    for Row := 1 to SX do
      PutCell( NewCoord2D(Row,Line) + aWhere, Translation[Clamp(Ord(Temp[(Line-1)*(SX+1)+Row]),Low(TPrintableCharToByte),High(TPrintableCharToByte))] );
end;

procedure TDungeonBuilder.Place(Tile: TDungeonTile; const Pos: TCoord2D);
var SX    : Byte;
    SY    : Byte;
    S     : TCoord2D;
begin
  for SX := 0 to Tile.SizeX-1 do
    for SY := 0 to Tile.SizeY-1 do
    begin
      S.Create(SX,SY);
      PutCell( S + Pos, Tile.Map[SX][SY] );
    end;
end;

procedure TDungeonBuilder.pushZone( const aArea : TArea );
begin
  Inc(Zones);
  ZoneArray[Zones] := aArea;
end;

procedure TDungeonBuilder.pushZone(const A, B: TCoord2D);
begin
  pushZone( NewArea( A, B ) );
end;

function TDungeonBuilder.IDtoCell(const aID: AnsiString): Byte;
begin
  if CellIDSource = nil then Exit(0);
  Exit( CellIDSource.IDToCell( aID ) );
end;

function TDungeonBuilder.CellToID(const aCell: Byte): AnsiString;
begin
  if CellIDSource = nil then Exit('');
  Exit( CellIDSource.CellToID( aCell ) );
end;

function TDungeonBuilder.popZone : boolean;
begin
  if Zones = 0 then Exit(False);
  popZone := True;
  Area := ZoneArray[Zones];
  Dec(Zones);
end;

procedure TDungeonBuilder.clearZones;
begin
  Area := ZoneArray[0];
  Zones := 0;
end;

procedure TDungeonBuilder.SetWallCell(const newWallCell : Byte);
begin
  if newWallCell >= 200 then raise Exception.Create('Wall Cell can''t be assigned a value greater then 199!');
  WallCell  := newWallCell;
end;

procedure TDungeonBuilder.SetFloorCell(const newFloorCell : Byte);
begin
  if newFloorCell >= 200 then raise Exception.Create('Floor Cell can''t be assigned a value greater then 199!');
  FloorCell  := newFloorCell;
end;

procedure TDungeonBuilder.SetDoorCell(const newDoorCell : Byte);
begin
  if newDoorCell >= 200 then raise Exception.Create('Door Cell can''t be assigned a value greater then 199!');
  DoorCell  := newDoorCell;
end;

function TDungeonBuilder.Scan(aArea : TArea; const Ignore : TCellSet; IgnoreBoundaries : Boolean = False) : LongInt;
var Other : DWord;
    c     : TCoord2D;
begin
  if IgnoreBoundaries then
    Area.Clamp( aArea )
  else
    if ( not Area.Contains( aArea.A ) ) or ( not Area.Contains( aArea.B ) ) then
      Exit(DUNGEN_SCANINVALID);

  Other := 0;
  for c in aArea do
    if not (GetCell(c) in Ignore) then Inc(Other);

  Scan := Other;
end;

procedure TDungeonBuilder.RestoreWalls(const CellID : Byte);
begin
  FillEdges( CellID );
end;

function TDungeonBuilder.RandomCellSquare(const Cell : TCellSet) : boolean;
const Limit = 6000;
var   LimitCount : DWord;
begin
  RandomCellSquare := False;
  LimitCount := 0;
  repeat
    Inc(LimitCount);
    if (LimitCount > Limit) then Exit(False);
    try
      FoundCell := RanCoord(Cell);
    except
      Exit;
    end;
  until Around( FoundCell, Cell ) = 8;
  Exit(True);
end;

function TDungeonBuilder.RandomCellNear(const Where : TCoord2D; const Range : Byte; const Cell : TCellSet) : boolean;
const Limit = 6000;
var   LimitCount : DWord;
begin
  LimitCount := 0;
  repeat
    Inc(LimitCount);
    if (LimitCount > Limit) then Exit(False);
    FoundCell := Where.RandomShifted(Range);
  until Area.Contains(FoundCell) and ( GetCell(FoundCell) in Cell );
  Exit(true);
end;

procedure TDungeonBuilder.CityDungeon(const Rooms,  MaxRSizeX, MaxRSizeY : Word; GoodFloors : TCellSet = []; UsedFloorCell : Byte = 0; AroundCell : Byte = 0; Doors : Byte = 1);
var RoomCount : Word;
    DoorCount : Word;
    Room      : TArea;
    Coord     : TCoord2D;
    
begin
  if AroundCell = 0    then AroundCell    := FloorCell;
  if UsedFloorCell = 0 then UsedFloorCell := FloorCell;
  if GoodFloors = [] then
  begin
    Fill(FloorCell);
    GoodFloors := [FloorCell];
  end;
  
  for RoomCount := 1 to Rooms do
  begin
    Room := Area.Shrinked.RandomSubArea( NewByteRange(4,MaxRSizeX), NewByteRange(4,MaxRSizeY) );

    if Scan( Room, GoodFloors ) = 0 then
    begin
      PutRoom( Room );
      Fill( Room.Expanded(1) ,DUNGEN_FLOOR3);
      Fill( Room             ,WallCell);
      Fill( Room.Shrinked(1) ,DUNGEN_FLOOR2);
      
      DoorCount := Doors;
      while DoorCount > 0 do
      begin
        repeat
          Coord := Room.RandomInnerEdgeCoord;
        until GetCell( Coord ) = WallCell;
        PutCell( Coord, DoorCell );
        Dec(DoorCount);
      end;
    end;
  end;
  Transmute([DUNGEN_FLOOR2],UsedFloorCell);
  Transmute([DUNGEN_FLOOR3],AroundCell);
end;

procedure TDungeonBuilder.DividedDungeon(const Divisions : Byte; const FirstDivision : Boolean; const AdditionalDoors : Byte);
var Count : Word;
  procedure Spot;
  begin
    repeat FoundCell := RanCoord([FloorCell]) until (FoundCell.X mod 2 = 0) and (FoundCell.Y mod 2 = 0)
  end;
begin
  Fill(FloorCell);
  Spot;
  PlotLine(FoundCell,FirstDivision,WallCell,DoorCell,[WallCell,DoorCell]);
  Spot;
  PlotLine(FoundCell,not FirstDivision,WallCell,DoorCell,[WallCell,DoorCell]);
  try
    if Divisions > 2 then
    for Count := 1 to Divisions-2 do
    begin
      Spot;
      case random(2) of
        0 : PlotLine(FoundCell,DUNGEN_VERTICAL,WallCell,DoorCell,[WallCell,DoorCell]);
        1 : PlotLine(FoundCell,DUNGEN_HORIZONTAL,WallCell,DoorCell,[WallCell,DoorCell]);
      end;
    end;
  except on e: EPlacementException do;
  end;

  try
    for Count := 1 to AdditionalDoors do
    begin
      FoundCell := RanCoord([WallCell]);
      if Around( FoundCell,[DoorCell]) = 0 then
        if CrossAround( FoundCell, [WallCell] ) = 2 then
           PutCell(FoundCell,DoorCell);
    end;
  except on e: EPlacementException do;
  end;
end;


{ TDungeonTile }

constructor TDungeonTile.Create(SizeX, SizeY: Byte);
begin
  Resize(SizeX,SizeY);
end;

constructor TDungeonTile.Create(const TileCode: Ansistring; const Translation: TPrintableCharToByte);
begin
  Decode(TileCode,Translation);
end;

procedure TDungeonTile.Decode(const TileCode: Ansistring; const Translation: TPrintableCharToByte);
var Temp  : AnsiString;
    SX    : Byte;
    SY    : Byte;
    Line  : Byte;
    Row   : Byte;
begin
  Temp := DelChars(DelChars(DelChars(TrimSet(TileCode,[#1..#32]),#13),#9),' ');
  SY := WordCount(Temp,[#10]);
  SX := Pos(#10,Temp)-1;
  Resize(SX, SY);
  for Line := 0 to SY-1 do
    for Row := 0 to SX-1 do
      Map[Row][Line] := Translation[Clamp(Ord(Temp[Line*(SX+1)+Row+1]),Low(TPrintableCharToByte),High(TPrintableCharToByte))];
end;

procedure TDungeonTile.Resize(SX, SY: Byte);
begin
  SetLength(Map, SX, SY);
  Area.A.Create(0,0);
  Area.B.Create(SX-1,SY-1);
end;

procedure TDungeonTile.Expand(Size: Byte);
var NewMap : array of array of Byte;
    SX    : Word;
    SY,CY : Word;
    Line  : Word;
    Count : Word;
    procedure FillRow( OY, RY : Word );
    var x,px,c : Word;
    begin
      px := 0;
      for x := 0 to Area.B.X do
        if x mod 2 = 0 then
        begin
          NewMap[px][RY] := Map[x][OY];
          Inc(px);
        end
        else
        begin
          for c := 0 to Size - 1 do
          begin
            NewMap[px][RY] := Map[x][OY];
            Inc(px);
          end;
        end;
    end;
begin
  SX := Area.B.X div 2 * (Size+1) + 1;
  SY := Area.B.Y div 2 * (Size+1) + 1;
  SetLength( NewMap, SX, SY );
  CY := 0;
  for Line := 0 to Area.B.Y do
    if Line mod 2 = 0 then
    begin
      FillRow( Line, CY );
      Inc(CY);
    end
    else
      for Count := 0 to Size - 1 do
      begin
        FillRow( Line, CY );
        Inc(CY);
      end;
  Map := NewMap;
  Area.A.Create(0,0);
  Area.B.Create(SX-1,SY-1);
end;

procedure TDungeonTile.Expand(const SizeX, SizeY: array of Byte);
var NewMap : array of array of Byte;
    SX    : Word;
    SY,CY : Word;
    Line  : Word;
    Count : Word;
    procedure FillRow( OY, RY : Word );
    var x,px,c : Word;
    begin
      px := 0;
      for x := 0 to Area.B.X do
        for c := 0 to SizeX[x] - 1 do
        begin
          NewMap[px][RY] := Map[x][OY];
          Inc(px);
        end;
    end;
begin
  SX := 0;
  SY := 0;
  for Count := 0 to Area.B.X do SX += SizeX[Count];
  for Count := 0 to Area.B.Y do SY += SizeY[Count];
  SetLength( NewMap, SX, SY );
  CY := 0;
  for Line := 0 to Area.B.Y do
    for Count := 0 to SizeY[Line] - 1 do
    begin
      FillRow( Line, CY );
      Inc(CY);
    end;
  Map := NewMap;
  Area.A.Create(0,0);
  Area.B.Create(SX-1,SY-1);
end;

function TDungeonTile.SizeX: Byte;
begin
  Exit( Area.B.X + 1 );
end;

function TDungeonTile.SizeY: Byte;
begin
  Exit( Area.B.Y + 1 );
end;

end.

{CIIIIIIITTTTTTTTYYYYYYYYY***************************}
(*
procedure GenCity_AddSquare;
var x1,x2,y1,y2 : byte;
begin
  x1 := (((Random(MapSizeX)+1) div 12)+2)*4;
  y1 := (((Random(MapSizeY)+1) div 12)+1)*4;
  x2 := (((Random(MapSizeX)+1) div 12)+((MapSizeX div 12)*2)-2)*4;
  y2 := (((Random(MapSizeY)+1) div 12)+((MapSizeY div 12)*2)-1)*4;
  GenCreateRoom(x1,y1,x2,y2,CellWall);
  GenCreateRoom(x1+1,y1+1,x2-1,y2-1,CellFloor);
end;

procedure GenCity_DrawRoad(x1,y1,x2,y2 : byte; doortile : byte);
var tx,ty : byte;
    tw    : ShortInt;
begin
  for tx := x1 to x2 do
    for ty := y1 to y2 do
      begin
        if SMap(tx,ty) = 255 then Continue;
        if Level.Map[tx,ty].cell = CellWall  then Level.Map[tx,ty].cell := doorTile;
        if Level.Map[tx,ty].cell in [11,18]  then Level.Map[tx,ty].cell := 12;
        if Level.Map[tx,ty].cell = CellFloor then Level.Map[tx,ty].cell := 12;
      end;
end;

function LineYScan(LineY : byte; cntCells : CellSet) : byte;
var tx  : byte;
    cnt : byte;
begin
  cnt := 0;
  for tx := 1 to LSizeX do
    if Level.Map.c[tx,LineY] in cntCells then Inc(cnt);
  LineYScan := cnt;
end;

function LineXScan(LineX : byte; cntCells : CellSet) : byte;
var ty  : byte;
    cnt : byte;
begin
  cnt := 0;
  for ty := 1 to LSizeY do
    if Level.Map.c[LineX,ty] in cntCells then Inc(cnt);
  LineXScan := cnt;
end;

procedure GenCityWalls;
var ax,ay : byte;
begin
  GenFill(11);
  GenSprinkle(18,200);
  for ax := 1 to 3 do GenCity_AddSquare;
  for ax := 2 to LSizeX-1 do
    for ay := 2 to LSizeY-1 do
      if Level.Map.c[ax,ay] = CellWall then
        if GenCrossAround(ax,ay,CellFloors) >= 2 then
          if GenAround(ax,ay,[11,18]) = 0 then
             Level.Map.c[ax,ay] := CellFloor;


  {1. Doda† drogi wedˆug kt¢rych uˆo¾one jest miasto.              }
  {2. Zaznaczy† drogi = przy przeci©ciu z murami doda† bram© ;-)   }
  {TEMP}
  repeat
    ay := Random(LSizeY-20)+10;
  until (LineYScan(ay, [CellWall]) = 2) and (LineYScan(ay+1,[CellWall]) = 2);
  GenCity_DrawRoad(1,ay,LSizeX,ay+1,29);
  if k(1,2) = 1 then
  begin
    repeat
      ax := Random(LSizeX-20)+10;
    until (LineXScan(ax, [CellWall]) = 2) and (LineXScan(ax+1,[CellWall]) = 2);
    GenCity_DrawRoad(ax,1,ax+1,LSizeY,31);
  end;
  {/TEMP}
  {3. Doda† wie¾e na rogach!                                       }
  {TEMP}
  for ax := 2 to LSizeX-1 do
    for ay := 2 to LSizeY-1 do
      if Level.Map.c[ax,ay] = CellWall then
        if GenAround(ax,ay,CellFloors) = 1 then
          if GenAround(ax,ay,CellWalls) = 2 then
             GenCreateRoom(ax-1,ay-1,ax+1,ay+1,CellWall);
  {/TEMP}

  {4. teraz losowym algorytmem doda† budynki (scan musi by† czysty)}

  GenSprinkleTiles('towntile.dat',[CellFloor],1000);

  Ay := k(1,5)+5;
  for AX := 1 to AY do
  begin
    GenRandomTile(CellFloors+[12]);
  end;
end;

*)
