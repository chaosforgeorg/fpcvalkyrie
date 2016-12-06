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

{ TCellFilterEnumerator }

TCellFilterEnumerator = object
private
  FMapArea    : TMapArea;
  FFilter     : TCellSet;
  FEnumerator : TAreaEnumerator;
public
  constructor Create( MapArea : TMapArea; const Filter : TCellSet );
  function MoveNext : Boolean;
  function GetCurrent : TCoord2D;
  function GetEnumerator : TCellFilterEnumerator;
public
  property Current : TCoord2D read GetCurrent;
end;

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
    function FindRanCoord( const aSeekedCells : TCellSet ) : TCoord2D;
    function FindRanCoord( const aSeekedCells : TCellSet; const aArea : TArea ) : TCoord2D;
    function FindEmptyRanCoord( const EmptyFlags : TFlags32 ) : TCoord2D;
    function FindEmptyRanCoord( const EmptyFlags : TFlags32; const aArea : TArea ) : TCoord2D;
    function FindEmptyRanCoord( const aSeekedCells : TCellSet; const EmptyFlags : TFlags32 ) : TCoord2D;
    function FindEmptyRanCoord( const aSeekedCells : TCellSet; const EmptyFlags : TFlags32; const aArea : TArea ) : TCoord2D;
    procedure ForAllCells( aWhatArea : TArea; aWhat : TCellMethod ); overload;
    procedure ForAllCells( aWhat : TCellMethod ); overload;
    function Drop( const aCoord : TCoord2D; const aEmptyFlags : TFlags32 ) : TCoord2D; // raises EPlacementException
    procedure Fill( const WhatArea : TArea; const CellID : Byte ); overload;
    procedure Fill( CellID : byte ); overload;
    procedure Fill( CellID : byte; Chance : byte ); overload;
    procedure Fill( const aArea : TArea; const Pattern : array of byte; Horiz : Boolean; StartOver : Boolean = True ); overload;
    procedure Fill( const aArea : TArea; const PatternA, PatternB : array of byte; Horiz : Boolean); overload;
    procedure FillEdges( const CellID : byte );
    procedure Sprinkle(const CellID : Byte; const Amount : DWord);
    procedure Transmute( const aWhatArea : TArea; const aFrom : TCellSet; aTo : Byte ); overload;
    procedure Transmute( const aFrom : TCellSet; aTo : Byte ); overload;
    procedure Transmute( const aWhatArea : TArea; const aFrom : TCellSet; const Translation : TCellTranslateMethod ); overload;
    procedure Transmute( const aFrom : TCellSet; const Translation : TCellTranslateMethod ); overload;
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
    function FindCells( aCellSet : TCellSet ) : TCellFilterEnumerator;
    function FindCells( aWhat : Byte ) : TCellFilterEnumerator;
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

function TMapArea.FindRanCoord(const aSeekedCells: TCellSet): TCoord2D;
begin
  Exit( FindRanCoord( aSeekedCells, Area ) );
end;

function TMapArea.FindRanCoord(const aSeekedCells: TCellSet; const aArea: TArea
  ): TCoord2D;
var c  : TCoord2D;
begin
  with TCoordArray.Create do
  try
    for c in aArea do
      if GetCell( c ) in aSeekedCells then
        Push( c );
    if IsEmpty then raise EPlacementException.Create('TMapArea.FindRanCoord failed!');
    Result := Items[ Random( Size ) ];
  finally
    Free
  end;
end;

function TMapArea.FindEmptyRanCoord(const EmptyFlags: TFlags32): TCoord2D;
begin
  Exit( FindEmptyRanCoord( EmptyFlags, Area ) );
end;

function TMapArea.FindEmptyRanCoord(const EmptyFlags: TFlags32;
  const aArea: TArea): TCoord2D;
var c  : TCoord2D;
begin
  with TCoordArray.Create do
  try
    for c in aArea do
      if Self.isEmpty( c, EmptyFlags ) then
        Push( c );
    if IsEmpty then raise EPlacementException.Create('TMapArea.FindEmptyRanCoord failed!');
    Result := Items[ Random( Size ) ];
  finally
    Free
  end;
end;

function TMapArea.FindEmptyRanCoord(const aSeekedCells: TCellSet;
  const EmptyFlags: TFlags32): TCoord2D;
begin
  Exit( FindEmptyRanCoord( aSeekedCells, EmptyFlags, Area ) );
end;

function TMapArea.FindEmptyRanCoord(const aSeekedCells: TCellSet;
  const EmptyFlags: TFlags32; const aArea: TArea): TCoord2D;
var c  : TCoord2D;
begin
  with TCoordArray.Create do
  try
    for c in aArea do
      if (GetCell( c ) in aSeekedCells) and (Self.isEmpty( c, EmptyFlags )) then
        Push( c );
    if IsEmpty then raise EPlacementException.Create('TMapArea.FindEmptyRanCoord failed!');
    Result := Items[ Random( Size ) ];
  finally
    Free
  end;
end;

procedure TMapArea.ForAllCells( aWhatArea: TArea; aWhat: TCellMethod );
begin
  aWhatArea.ForAllCells( aWhat );
end;

procedure TMapArea.ForAllCells( aWhat: TCellMethod );
begin
  Area.ForAllCells( aWhat );
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

  if iList.IsEmpty then raise EPlacementException.Create('TMapArea.Drop(@1,@2) failed!',[aCoord.x, aCoord.y]);

  Drop := iList.Return;
  FreeAndNil( iList );
  Exit( Drop );
end;

procedure TMapArea.Fill(const WhatArea: TArea; const CellID: Byte);
var c : TCoord2D;
begin
  for c in WhatArea do
    PutCell( c, CellID );
end;

procedure TMapArea.Fill(CellID: byte);
begin
  Fill( Area, CellID );
end;

procedure TMapArea.Fill(CellID: byte; Chance: byte);
var c : TCoord2D;
begin
  for c in Area do
    if Random(100) < Chance then
      PutCell( c, CellID );
end;

procedure TMapArea.Fill(const aArea: TArea; const Pattern: array of byte; Horiz: Boolean; StartOver: Boolean);
var Coord : TCoord2D;
    Count : DWord;
    ASize : DWord;
  function NextCell : Byte;
  begin
    if StartOver and (Coord.Horiz(Horiz) = aArea.A.Horiz(Horiz)) then Count := 0;
    NextCell := Count mod (ASize+1);
    Inc(Count);
  end;
begin
  Coord.Create(0,0);
  Count := 0;
  ASize := High(Pattern);
  while aArea.NextCoord(Coord,Horiz) do PutCell(Coord, Pattern[NextCell]);
end;

procedure TMapArea.Fill(const aArea: TArea; const PatternA, PatternB: array of byte; Horiz: Boolean);
var Coord : TCoord2D;
    Count : DWord;
    SizeA : DWord;
    SizeB : DWord;
    Flip  : Boolean;
  function NextCell : Byte;
  begin
    if Coord.Horiz(Horiz) = aArea.A.Horiz(Horiz) then
    begin
      Count := 0;
      Flip := not Flip;
    end;
    if Flip
      then NextCell := PatternB[Count mod (SizeB+1)]
      else NextCell := PatternA[Count mod (SizeA+1)];
    Inc(Count);
  end;
begin
  Flip := True;
  Coord.Create(0,0);
  Count := 0;
  SizeA := High(PatternA);
  SizeB := High(PatternB);
  while aArea.NextCoord(Coord,Horiz) do PutCell(Coord, NextCell);
end;

procedure TMapArea.FillEdges(const CellID: byte);
var cx,cy : Word;
begin
  for cx := Area.A.X to Area.B.X do
  begin
    PutCell(NewCoord2D(cx,Area.A.Y),CellID);
    PutCell(NewCoord2D(cx,Area.B.Y),CellID);
  end;
  for cy := Area.A.Y to Area.B.Y do
  begin
    PutCell(NewCoord2D(Area.A.X,cy),CellID);
    PutCell(NewCoord2D(Area.B.X,cy),CellID);
  end;
end;

procedure TMapArea.Sprinkle(const CellID : Byte; const Amount : DWord);
var Count : word;
begin
  for Count := 1 to Amount do
    PutCell(Area.RandomCoord,CellID);
end;

procedure TMapArea.Transmute(const aWhatArea : TArea; const aFrom: TCellSet; aTo: Byte);
var c : TCoord2D;
begin
  for c in aWhatArea do
    if GetCell( c ) in aFrom then
      putCell( c, aTo );
end;

procedure TMapArea.Transmute(const aFrom: TCellSet; aTo: Byte);
begin
  Transmute( Area, aFrom, aTo );
end;

procedure TMapArea.Transmute(const aWhatArea: TArea; const aFrom: TCellSet; const Translation: TCellTranslateMethod);
var c : TCoord2D;
begin
  for c in aWhatArea do
    if GetCell(c) in aFrom then
      PutCell(c,Translation(c));
end;

procedure TMapArea.Transmute( const aFrom: TCellSet; const Translation: TCellTranslateMethod);
begin
  Transmute( Area, aFrom, Translation );
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

function TMapArea.FindCells(aCellSet: TCellSet): TCellFilterEnumerator;
begin
  FindCells.Create(Self,aCellSet);
end;

function TMapArea.FindCells(aWhat: Byte): TCellFilterEnumerator;
begin
  FindCells.Create(Self,[aWhat]);
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

{ TCellFilterEnumerator }

constructor TCellFilterEnumerator.Create(MapArea: TMapArea;
  const Filter: TCellSet);
begin
  FMapArea := MapArea;
  FFilter  := Filter;
  FEnumerator.Create(MapArea.Area);
end;

function TCellFilterEnumerator.MoveNext: Boolean;
begin
  while FEnumerator.MoveNext do
    if FMapArea.GetCell(FEnumerator.Current) in FFilter then Exit(True);
  Exit(False);
end;

function TCellFilterEnumerator.GetCurrent: TCoord2D;
begin
  Exit( FEnumerator.Current );
end;

function TCellFilterEnumerator.GetEnumerator: TCellFilterEnumerator;
begin
  Exit( Self );
end;

end.

