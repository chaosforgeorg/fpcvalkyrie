{$INCLUDE valkyrie.inc}
// @abstract(Presented copy of a cell map for Valkyrie)
//
// A turn-based game resolves a whole action at once, while its presentation
// plays out over time. TGVisualMap keeps the per-cell state a renderer shows
// and converges on the game's state by itself: each Update compares every
// cell with its source and starts a fade where they differ, unless the cell
// is held. Holds are the only timing input. Once holds and fades have ended
// the map equals its source, so a change made without a hold is shown at
// once rather than lost.
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
unit vvisualmap;
interface
uses vrltools;

// vlShown is the state a fade starts from; vlTarget the state it ends in.
// Outside a fade both hold the same state.
type TVisualLayer = ( vlShown, vlTarget );

// T is a packed record of plain values; the default Equal compares its bytes.
type generic TGVisualMap<T> = class
public type
  TVisualCell = record
    Shown     : T;
    Target    : T;
    HoldUntil : QWord;
    FadeStart : QWord;
    FadeTime  : DWord;
  end;
  PVisualCell = ^TVisualCell;
public
  constructor Create( const aArea : TArea );
  // Advances presentation time by aTime ms and follows the source.
  procedure Update( aTime : DWord ); virtual;
  // Hides changes of aCoord and its neighbours for aDelay ms. Neighbours
  // are included because their appearance is derived from the cell.
  procedure Hold( const aCoord : TCoord2D; aDelay : DWord );
  // Ends every hold and fade: afterwards the map equals its source.
  procedure Synchronize; virtual;
  function Contains( const aCoord : TCoord2D ) : Boolean; inline;
  function State( const aCoord : TCoord2D; aLayer : TVisualLayer ) : T; inline;
  function Fading( const aCoord : TCoord2D ) : Boolean;
  // Progress in [0,1] of aCoord's own fade; 1 when it is not fading.
  function Progress( const aCoord : TCoord2D ) : Single;
  // Whether aCoord or a neighbour is fading. A tile drawn from neighbour
  // states shows both layers; aProgress is its own fade, else the most
  // advanced neighbouring one.
  function NeighbourhoodFade( const aCoord : TCoord2D; out aProgress : Single ) : Boolean;
  destructor Destroy; override;
protected
  // The state the source has now; aTarget is the state currently shown.
  function Sample( const aCoord : TCoord2D; const aTarget : T ) : T; virtual; abstract;
  // Duration of the fade between two states in ms; 0 switches at once.
  function FadeTime( const aCoord : TCoord2D; const aFrom, aTo : T ) : DWord; virtual; abstract;
  function Equal( const aA, aB : T ) : Boolean; virtual;
  function Cell( const aCoord : TCoord2D ) : PVisualCell; inline;
private
  FArea    : TArea;
  FWidth   : Integer;
  FCells   : array of TVisualCell;
  FFades   : Integer;
  FPending : Integer;
  function CellProgress( const aCell : TVisualCell ) : Single; inline;
protected
  FTime    : QWord;
public
  property Area : TArea read FArea;
  property Time : QWord read FTime;
  // Something is still to be shown: a fade runs or a held change waits.
  // Valid since the last Update.
  function Busy : Boolean;
end;

implementation

uses math;

constructor TGVisualMap.Create( const aArea : TArea );
begin
  inherited Create;
  FArea  := aArea;
  FWidth := aArea.B.X - aArea.A.X + 1;
  SetLength( FCells, FWidth * ( aArea.B.Y - aArea.A.Y + 1 ) );
  FillChar( FCells[0], Length( FCells ) * SizeOf( TVisualCell ), 0 );
  FTime    := 0;
  FFades   := 0;
  FPending := 0;
end;

destructor TGVisualMap.Destroy;
begin
  FCells := nil;
  inherited Destroy;
end;

function TGVisualMap.Contains( const aCoord : TCoord2D ) : Boolean; inline;
begin
  Exit( FArea.Contains( aCoord ) );
end;

function TGVisualMap.Cell( const aCoord : TCoord2D ) : PVisualCell; inline;
begin
  Exit( @FCells[ ( aCoord.Y - FArea.A.Y ) * FWidth + ( aCoord.X - FArea.A.X ) ] );
end;

function TGVisualMap.State( const aCoord : TCoord2D; aLayer : TVisualLayer ) : T; inline;
begin
  if aLayer = vlShown
    then Exit( Cell( aCoord )^.Shown )
    else Exit( Cell( aCoord )^.Target );
end;

function TGVisualMap.Equal( const aA, aB : T ) : Boolean;
begin
  Exit( CompareByte( aA, aB, SizeOf( T ) ) = 0 );
end;

function TGVisualMap.CellProgress( const aCell : TVisualCell ) : Single; inline;
begin
  if aCell.FadeTime = 0 then Exit( 1.0 );
  Exit( Min( ( FTime - aCell.FadeStart ) / aCell.FadeTime, 1.0 ) );
end;

procedure TGVisualMap.Update( aTime : DWord );
var iCoord  : TCoord2D;
    iCell   : PVisualCell;
    iSample : T;
begin
  FTime    += aTime;
  FFades   := 0;
  FPending := 0;
  for iCoord in FArea do
  begin
    iCell := Cell( iCoord );
    if ( iCell^.FadeTime > 0 ) and ( FTime >= iCell^.FadeStart + iCell^.FadeTime ) then
    begin
      iCell^.Shown    := iCell^.Target;
      iCell^.FadeTime := 0;
    end;
    iSample := Sample( iCoord, iCell^.Target );
    if not Equal( iSample, iCell^.Target ) then
    begin
      if FTime < iCell^.HoldUntil then
        Inc( FPending )
      else
      begin
        // A fade still running ends here; its target starts the next one.
        iCell^.Shown     := iCell^.Target;
        iCell^.Target    := iSample;
        iCell^.FadeStart := FTime;
        iCell^.FadeTime  := FadeTime( iCoord, iCell^.Shown, iCell^.Target );
        if iCell^.FadeTime = 0 then iCell^.Shown := iCell^.Target;
      end;
    end;
    if iCell^.FadeTime > 0 then Inc( FFades );
  end;
end;

procedure TGVisualMap.Hold( const aCoord : TCoord2D; aDelay : DWord );
var iCoord : TCoord2D;
    iCell  : PVisualCell;
begin
  if aDelay = 0 then Exit;
  for iCoord in NewArea( aCoord, 1 ).Clamped( FArea ) do
  begin
    iCell := Cell( iCoord );
    iCell^.HoldUntil := Max( iCell^.HoldUntil, FTime + aDelay );
  end;
end;

procedure TGVisualMap.Synchronize;
var iCoord : TCoord2D;
    iCell  : PVisualCell;
begin
  for iCoord in FArea do
  begin
    iCell := Cell( iCoord );
    iCell^.Target    := Sample( iCoord, iCell^.Target );
    iCell^.Shown     := iCell^.Target;
    iCell^.HoldUntil := 0;
    iCell^.FadeTime  := 0;
  end;
  FFades   := 0;
  FPending := 0;
end;

function TGVisualMap.Fading( const aCoord : TCoord2D ) : Boolean;
begin
  Exit( Cell( aCoord )^.FadeTime > 0 );
end;

function TGVisualMap.Progress( const aCoord : TCoord2D ) : Single;
begin
  Exit( CellProgress( Cell( aCoord )^ ) );
end;

function TGVisualMap.NeighbourhoodFade( const aCoord : TCoord2D; out aProgress : Single ) : Boolean;
var iCoord : TCoord2D;
    iCell  : PVisualCell;
begin
  aProgress := 1.0;
  if FFades = 0 then Exit( False );
  iCell := Cell( aCoord );
  if iCell^.FadeTime > 0 then
  begin
    aProgress := CellProgress( iCell^ );
    Exit( True );
  end;
  Result := False;
  aProgress := 0.0;
  for iCoord in NewArea( aCoord, 1 ).Clamped( FArea ) do
  begin
    iCell := Cell( iCoord );
    if iCell^.FadeTime > 0 then
    begin
      aProgress := Max( aProgress, CellProgress( iCell^ ) );
      Result := True;
    end;
  end;
  if not Result then aProgress := 1.0;
end;

function TGVisualMap.Busy : Boolean;
begin
  Exit( ( FFades > 0 ) or ( FPending > 0 ) );
end;

end.
