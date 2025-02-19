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

// Set of Cells.
type TCellSet = set of Byte;

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
  function IDtoCell( const aID : AnsiString ) : Byte; virtual;
  function CellToID( const aCell : Byte ) : AnsiString; virtual;
public
  FCellIDSource : ICellIDSource;
end;

implementation

uses sysutils, strutils;

constructor TDungeonBuilder.Create(const sx,sy : Word; aTarget : IMapArea; aCellIDSource : ICellIDSource);
begin
  FCellIDSource := aCellIDSource;
  with Area do
  begin
    A.Create(1,1);
    B.Create(sx,sy);
  end;
  inherited Create( Area, aTarget );
end;

function TDungeonBuilder.IDtoCell(const aID: AnsiString): Byte;
begin
  if FCellIDSource = nil then Exit(0);
  Exit( FCellIDSource.IDToCell( aID ) );
end;

function TDungeonBuilder.CellToID(const aCell: Byte): AnsiString;
begin
  if FCellIDSource = nil then Exit('');
  Exit( FCellIDSource.CellToID( aCell ) );
end;

end.

