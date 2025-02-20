{$INCLUDE valkyrie.inc}
// @abstract(Map Area for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 5, 2009)
//
// THIS UNIT IS EXPERIMENTAL!

unit vmaparea;
interface
uses Classes, vnode, vgenerics, vutil, vrltools;

type EPlacementException = class(EException);


// Set of Cells.
type TCellSet = set of Byte;
     TMinCoordChoice = specialize TGMinimalChoice<TCoord2D>;
     TCoordArray     = specialize TGArray<TCoord2D>;




type

TMapArea = class;

IMapArea = interface['valkyrie.imaparea']
  function GetCell( const aWhere : TCoord2D ) : Byte;
  procedure PutCell( const aWhere : TCoord2D; const aWhat : Byte );
  function isEmpty( const coord : TCoord2D; EmptyFlags : TFlags32 = []) : Boolean;
end;


{ TMapArea }

TMapArea = class(TNode, IMapArea)
    Area        : TArea;
    Target      : IMapArea;
    constructor Create( aArea : TArea; aTarget : IMapArea = nil ); reintroduce;
    function RanCoord : TCoord2D;
    function RanCoord( const aSeekedCells : TCellSet ) : TCoord2D;
    function RanCoord( const aSeekedCells : TCellSet; const aArea : TArea ) : TCoord2D;
    function EmptyRanCoord( const EmptyFlags : TFlags32 ) : TCoord2D;
    function EmptyRanCoord( const EmptyFlags : TFlags32; const aArea : TArea ) : TCoord2D;
    function EmptyRanCoord( const aSeekedCells : TCellSet; const EmptyFlags : TFlags32 ) : TCoord2D;
    function EmptyRanCoord( const aSeekedCells : TCellSet; const EmptyFlags : TFlags32; const aArea : TArea ) : TCoord2D;
    function Drop( const aCoord : TCoord2D; const aEmptyFlags : TFlags32 ) : TCoord2D; // raises EPlacementException
    function Around(const Where : TCoord2D; const Cell : TCellSet; Range : Byte = 1 ) : byte;
    function CrossAround(const Coord : TCoord2D; const Cell : TCellSet) : byte;
    function TryFindCell( out aResult : TCoord2D; const aSeekedCells : TCellSet ) : Boolean;
    function FindCell( const SeekedCells : TCellSet ) : TCoord2D;  // raises EPlacementException
    function FindCell( const SeekedCells : TCellSet; const aArea : TArea ) : TCoord2D;  // raises EPlacementException
    function FindCell( const SeekedCells : TCellSet; const EmptyFlags : TFlags32 ) : TCoord2D;  // raises EPlacementException
    function FindCell( const SeekedCells : TCellSet; const EmptyFlags : TFlags32; const aArea : TArea ) : TCoord2D;  // raises EPlacementException
    function Width  : Word;
    function Height : Word;
    function Size : DWord;
    function properCoord( aWhere : TCoord2D ) : boolean;
    function GetCell( const aWhere : TCoord2D ) : Byte; virtual;
    procedure PutCell( const aWhere : TCoord2D; const aWhat : Byte ); virtual;
    function isEmpty( const coord : TCoord2D; EmptyFlags : TFlags32 = []) : Boolean; virtual;
    constructor CreateFromStream( ISt : TStream ); override;
    procedure WriteToStream( OSt : TStream ); override;
  end;

operator enumerator (ma : TMapArea) : TAreaEnumerator;

implementation

uses sysutils;

{ TMapArea }

operator enumerator (ma : TMapArea) : TAreaEnumerator;
begin
  Result.Create( ma.Area );
end;

constructor TMapArea.Create ( aArea : TArea; aTarget : IMapArea ) ;
begin
  inherited Create;
  Area := aArea;
  Target := aTarget;
end;

function TMapArea.RanCoord: TCoord2D;
begin
  RanCoord := Area.RandomCoord;
end;

function TMapArea.RanCoord(const aSeekedCells: TCellSet): TCoord2D;
begin
  Exit( RanCoord( aSeekedCells, Area ) );
end;

function TMapArea.RanCoord(const aSeekedCells: TCellSet; const aArea: TArea
  ): TCoord2D;
const LIM = 5000;
var   cn     : word;
      iCoord : TCoord2D;
begin
  cn := 0;
repeat
  if cn = LIM then raise EPlacementException.Create('TMapArea.RanCoord failed!');
  iCoord := aArea.RandomCoord;
  Inc(cn);
until GetCell( iCoord ) in aSeekedCells;
  Exit( iCoord );
end;

function TMapArea.EmptyRanCoord(const EmptyFlags: TFlags32): TCoord2D;
begin
  Exit( EmptyRanCoord( EmptyFlags, Area ) );
end;

function TMapArea.EmptyRanCoord(const EmptyFlags: TFlags32; const aArea : TArea ): TCoord2D;
const LIM = 5000;
var   cn     : word;
      iCoord : TCoord2D;
begin
  cn := 0;
repeat
  if cn = LIM then raise EPlacementException.Create('TMapArea.RanCoord failed!');
  iCoord := aArea.RandomCoord;
  Inc(cn);
until isEmpty(iCoord, EmptyFlags);
  Exit( iCoord );
end;

function TMapArea.EmptyRanCoord(const aSeekedCells: TCellSet; const EmptyFlags: TFlags32): TCoord2D;
begin
  Exit( EmptyRanCoord( aSeekedCells, EmptyFlags, Area ) );
end;

function TMapArea.EmptyRanCoord(const aSeekedCells: TCellSet; const EmptyFlags: TFlags32; const aArea : TArea ): TCoord2D;
const LIM = 10000;
var   cn     : word;
      iCoord : TCoord2D;
begin
  cn := 0;
repeat
  if cn = LIM then raise EPlacementException.Create('');
  iCoord := aArea.RandomCoord;
  Inc(cn);
until (GetCell( iCoord ) in aSeekedCells) and ( isEmpty(iCoord, EmptyFlags) );
  Exit( iCoord );
end;

function TMapArea.Drop(const aCoord: TCoord2D; const aEmptyFlags: TFlags32): TCoord2D;
var s     : TCoord2D;
    iList : TMinCoordChoice;
begin
  if isEmpty( aCoord, aEmptyFlags ) then
    Exit( aCoord );

  iList := TMinCoordChoice.Create;

  for s in NewArea( aCoord, 1 ) do
    if properCoord( s ) then
      if isEmpty( s, aEmptyFlags ) then
        iList.Add( s, Distance( aCoord, s ) );

  if iList.IsEmpty then
  for s in NewArea( aCoord, 5 ) do
    if properCoord(s) then
      if isEmpty( s, aEmptyFlags ) then
        iList.Add( s, Distance( aCoord, s ) );

  if iList.IsEmpty then raise EPlacementException.CreateFmt('TMapArea.Drop(%d,%d) failed!',[aCoord.x, aCoord.y]);

  Drop := iList.Return;
  FreeAndNil( iList );
  Exit( Drop );
end;

function TMapArea.Around(const Where : TCoord2D; const Cell : TCellSet; Range : Byte = 1) : byte;
var c : TCoord2D;
begin
  Around := 0;
  for c in NewArea( Where, Range ).Clamped( Area ) do
    if not ( c = Where )
      then if GetCell( c ) in Cell
        then Inc(Around);
end;

function TMapArea.CrossAround(const Coord : TCoord2D; const Cell : TCellSet) : byte;
begin
  CrossAround := 0;
  if ( Coord.x < Area.B.X ) and ( GetCell( Coord.ifIncX( 1) ) in Cell ) then Inc( CrossAround );
  if ( Coord.y < Area.B.Y ) and ( GetCell( Coord.ifIncY( 1) ) in Cell ) then Inc( CrossAround );
  if ( Coord.x > Area.A.X ) and ( GetCell( Coord.ifIncX(-1) ) in Cell ) then Inc( CrossAround );
  if ( Coord.y > Area.A.Y ) and ( GetCell( Coord.ifIncY(-1) ) in Cell ) then Inc( CrossAround );
end;

function TMapArea.TryFindCell ( out aResult : TCoord2D; const aSeekedCells : TCellSet ) : Boolean;
var c : TCoord2D;
begin
  for c in Area do
    if GetCell( c ) in aSeekedCells then
    begin
      aResult := c;
      Exit( True );
    end;
  Exit( False );
end;


function TMapArea.FindCell( const SeekedCells: TCellSet ): TCoord2D;
begin
  Exit( FindCell( SeekedCells, Area ) );
end;

function TMapArea.FindCell( const SeekedCells: TCellSet; const EmptyFlags: TFlags32 ): TCoord2D;
begin
  Exit( FindCell( SeekedCells, EmptyFlags, Area ) );
end;

function TMapArea.FindCell( const SeekedCells: TCellSet; const aArea : TArea ): TCoord2D;
var c : TCoord2D;
begin
  for c in aArea do
    if GetCell( c ) in SeekedCells then Exit( c );
  raise EPlacementException.Create('TMapArea.FindCell failed!');
end;

function TMapArea.FindCell( const SeekedCells: TCellSet; const EmptyFlags: TFlags32; const aArea : TArea ): TCoord2D;
var c : TCoord2D;
begin
  for c in aArea do
    if (GetCell( c ) in SeekedCells) and isEmpty(c,EmptyFlags) then
      Exit( c );
  raise EPlacementException.Create('TMapArea.FindCell failed!');
end;

function TMapArea.Width: Word;
begin
  Exit( Area.Width );
end;

function TMapArea.Height: Word;
begin
  Exit( Area.Height );
end;

function TMapArea.Size: DWord;
begin
  Exit( Area.Area );
end;

function TMapArea.properCoord(aWhere: TCoord2D): boolean;
begin
  Exit( Area.Contains( aWhere ) );
end;

function TMapArea.GetCell ( const aWhere : TCoord2D ) : Byte;
begin
  Exit( Target.GetCell ( aWhere ) );
end;

procedure TMapArea.PutCell ( const aWhere : TCoord2D; const aWhat : Byte  ) ;
begin
  Target.PutCell ( aWhere, aWhat ) ;
end;

function TMapArea.isEmpty ( const coord : TCoord2D;
  EmptyFlags : TFlags32 ) : Boolean;
begin
  Exit( Target.isEmpty ( coord, EmptyFlags ) );
end;

constructor TMapArea.CreateFromStream ( ISt : TStream ) ;
begin
  inherited CreateFromStream ( ISt ) ;
  ISt.Read( Area, SizeOf(TArea) );
end;

procedure TMapArea.WriteToStream ( OSt : TStream ) ;
begin
  inherited WriteToStream ( OSt ) ;
  OSt.Write( Area, SizeOf(TArea) );
end;

end.

