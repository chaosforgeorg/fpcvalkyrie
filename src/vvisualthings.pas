{$INCLUDE valkyrie.inc}
// @abstract(Presented copies of map things for Valkyrie)
//
// The thing counterpart of TGVisualMap. A visual thing follows its world
// thing, identified by UID, unless clips own it or a hold delays it. It
// outlives its world thing until both have ended, so a thing removed by an
// action that is still being presented stays drawn until it is shown to go.
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
unit vvisualthings;
interface
uses vutil, vvector, vrltools;

type

{ TVisualThing }

TVisualThing = class
  constructor Create( aUID : TUID );
protected
  FUID       : TUID;
  FCoord     : TCoord2D;
  FHoldUntil : QWord;
  FClips     : Word;
  FShown     : Boolean;
  FPresent   : Boolean;
  function GetOwned : Boolean;
public
  // Position in cell units; the cell at Coord spans Coord-1 to Coord.
  // Followed from the world while free, written by owning clips otherwise.
  Position : TVec2f;
  property UID     : TUID     read FUID;
  // The cell the thing is presented in.
  property Coord   : TCoord2D read FCoord;
  // Whether the thing has appeared; a thing added under a hold has not yet.
  property Shown   : Boolean  read FShown;
  // Whether its world thing was reported in the last synchronization.
  property Present : Boolean  read FPresent;
  // Clips own the thing's presentation while any of them exists.
  property Owned   : Boolean  read GetOwned;
end;

TVisualThingArray = array of TVisualThing;

TVisualThings = class
  constructor Create;
  destructor Destroy; override;
  // Removes every visual thing; no clip may still own one.
  procedure Clear;
  function Find( aUID : TUID ) : TVisualThing;
  // A clip takes ownership until it calls Release. A missing thing is
  // created at aCoord, shown.
  function Acquire( aUID : TUID; const aCoord : TCoord2D ) : TVisualThing;
  procedure Release( aThing : TVisualThing );
  // Keeps the thing as it is shown until aUntil. A thing not yet presented
  // is created hidden and appears when the hold ends.
  procedure Hold( aUID : TUID; aUntil : QWord );
  // One synchronization reports every world thing between BeginSync and
  // EndSync. Report returns its visual thing and whether it follows the
  // world now; the caller then copies the world state it presents.
  procedure BeginSync( aTime : QWord );
  function Report( aUID : TUID; const aCoord : TCoord2D; out aFollows : Boolean ) : TVisualThing;
  // Removes things whose world thing was not reported, once no clip owns
  // and no hold keeps them.
  procedure EndSync;
  // Something still differs from the world: a clip or hold keeps a thing.
  function Busy : Boolean;
protected
  function CreateThing( aUID : TUID ) : TVisualThing; virtual;
private
  FThings    : TVisualThingArray;
  FCount     : Integer;
  FTime      : QWord;
  FClips     : Integer;
  FHoldUntil : QWord;
  function Search( aUID : TUID; out aIndex : Integer ) : Boolean;
  function Insert( aIndex : Integer; aUID : TUID ) : TVisualThing;
  procedure Delete( aIndex : Integer );
public
  property Count : Integer read FCount;
  // Things in UID order, Things[0..Count-1].
  property Things : TVisualThingArray read FThings;
  property Time : QWord read FTime;
end;

implementation

uses sysutils;

{ TVisualThing }

constructor TVisualThing.Create( aUID : TUID );
begin
  inherited Create;
  FUID       := aUID;
  FCoord     := ZeroCoord2D;
  FHoldUntil := 0;
  FClips     := 0;
  FShown     := False;
  FPresent   := False;
  Position   := Vec2f( 0, 0 );
end;

function TVisualThing.GetOwned : Boolean;
begin
  Exit( FClips > 0 );
end;

{ TVisualThings }

constructor TVisualThings.Create;
begin
  inherited Create;
  FThings := nil;
  FCount     := 0;
  FTime      := 0;
  FClips     := 0;
  FHoldUntil := 0;
end;

destructor TVisualThings.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TVisualThings.Clear;
var i : Integer;
begin
  for i := 0 to FCount - 1 do
  begin
    Assert( FThings[i].FClips = 0, 'Visual thing cleared while a clip owns it' );
    FreeAndNil( FThings[i] );
  end;
  FCount     := 0;
  FHoldUntil := 0;
end;

function TVisualThings.CreateThing( aUID : TUID ) : TVisualThing;
begin
  Exit( TVisualThing.Create( aUID ) );
end;

function TVisualThings.Search( aUID : TUID; out aIndex : Integer ) : Boolean;
var iLow, iHigh, iMid : Integer;
begin
  iLow  := 0;
  iHigh := FCount - 1;
  while iLow <= iHigh do
  begin
    iMid := ( iLow + iHigh ) div 2;
    if FThings[iMid].FUID = aUID then
    begin
      aIndex := iMid;
      Exit( True );
    end;
    if FThings[iMid].FUID < aUID
      then iLow  := iMid + 1
      else iHigh := iMid - 1;
  end;
  aIndex := iLow;
  Exit( False );
end;

function TVisualThings.Insert( aIndex : Integer; aUID : TUID ) : TVisualThing;
begin
  if FCount = Length( FThings ) then
    SetLength( FThings, 2 * FCount + 16 );
  if aIndex < FCount then
    Move( FThings[aIndex], FThings[aIndex + 1], ( FCount - aIndex ) * SizeOf( TVisualThing ) );
  Result := CreateThing( aUID );
  FThings[aIndex] := Result;
  Inc( FCount );
end;

procedure TVisualThings.Delete( aIndex : Integer );
begin
  FThings[aIndex].Free;
  Dec( FCount );
  if aIndex < FCount then
    Move( FThings[aIndex + 1], FThings[aIndex], ( FCount - aIndex ) * SizeOf( TVisualThing ) );
  FThings[FCount] := nil;
end;

function TVisualThings.Find( aUID : TUID ) : TVisualThing;
var iIndex : Integer;
begin
  if Search( aUID, iIndex ) then Exit( FThings[iIndex] );
  Exit( nil );
end;

function TVisualThings.Acquire( aUID : TUID; const aCoord : TCoord2D ) : TVisualThing;
var iIndex : Integer;
begin
  if Search( aUID, iIndex ) then
    Result := FThings[iIndex]
  else
  begin
    Result := Insert( iIndex, aUID );
    Result.FCoord   := aCoord;
    Result.Position := Vec2f( aCoord.X - 1, aCoord.Y - 1 );
  end;
  // A clip shows the thing even when a hold has not yet revealed it.
  Result.FShown := True;
  Inc( Result.FClips );
  Inc( FClips );
end;

procedure TVisualThings.Release( aThing : TVisualThing );
begin
  Assert( aThing.FClips > 0, 'Visual thing released more often than acquired' );
  Dec( aThing.FClips );
  Dec( FClips );
end;

procedure TVisualThings.Hold( aUID : TUID; aUntil : QWord );
var iIndex : Integer;
    iThing : TVisualThing;
begin
  if aUntil <= FTime then Exit;
  if Search( aUID, iIndex )
    then iThing := FThings[iIndex]
    else iThing := Insert( iIndex, aUID );
  if iThing.FHoldUntil < aUntil then iThing.FHoldUntil := aUntil;
  if FHoldUntil < aUntil then FHoldUntil := aUntil;
end;

procedure TVisualThings.BeginSync( aTime : QWord );
var i : Integer;
begin
  FTime := aTime;
  for i := 0 to FCount - 1 do
    FThings[i].FPresent := False;
end;

function TVisualThings.Report( aUID : TUID; const aCoord : TCoord2D; out aFollows : Boolean ) : TVisualThing;
var iIndex : Integer;
begin
  if Search( aUID, iIndex )
    then Result := FThings[iIndex]
    else Result := Insert( iIndex, aUID );
  Result.FPresent := True;
  aFollows := ( Result.FClips = 0 ) and ( FTime >= Result.FHoldUntil );
  if not aFollows then Exit;
  Result.FShown   := True;
  Result.FCoord   := aCoord;
  Result.Position := Vec2f( aCoord.X - 1, aCoord.Y - 1 );
end;

procedure TVisualThings.EndSync;
var i : Integer;
begin
  for i := FCount - 1 downto 0 do
    with FThings[i] do
      if ( not FPresent ) and ( FClips = 0 ) and ( FTime >= FHoldUntil ) then
        Delete( i );
end;

function TVisualThings.Busy : Boolean;
begin
  Exit( ( FClips > 0 ) or ( FTime < FHoldUntil ) );
end;

end.
