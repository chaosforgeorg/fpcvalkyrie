{$INCLUDE valkyrie.inc}
// @abstract(Vision Toolkit for Valkyrie)
// @author(Stefan O'Rear <stefanor@cox.net>)
// @created(Jan 12, 2008)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-10-14 19:45:46 +0200 (Tue, 14 Oct 2008) $)
//
// This implements the line of sight algorithm for Valkyrie-based games;
// determining what the player can see, et cetera.
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

unit vvision2;
interface

uses Classes, SysUtils, vrltools;

// Your map class should implement this; Valkyrie needs to be know where light
// can travel!  Requests may be made arbitrarily far away from the starting
// point, so, uh, do bounds checking and return a sane value for distant
// points.
//
// TODO: This should be more than just a boolean if we want to handle unusual
// lighting effects, like Crawl's fog.  Or mirrors, or semitransparent walls
// that recolor things behind them, or...

type IVisionQuery = interface
  function blocksVision( const Coord : TCoord2D ) : boolean;
end;

// This abstract class encapsulates a single line of sight algorithm; in a
// sense, it controls the geometrical properties of your roguelike's space.
// As such, it controls both field of view (from here, what can I see?),
// line of sight (Can I see that from here?), and targetting paths (If I
// shoot from here to there, what could get in the way?).

type TVision = class
  // Every TVision instance is associated with a map, which can be changed at
  // any time.  TODO: It might be worth requiring notification of map changes.
  constructor Create( newMap : IVisionQuery );
  procedure ChangeSource( newMap : IVisionQuery );

  // These functions handle field of view.  Declare the field of view origin,
  // then you can make many LOS checks with very high efficiency.  Radius may
  // be -1 for a FOV limited only by walls (this is not supported by all
  // algorithms).
  //
  // Light values represent the remaining distance; they are equal to the LOS
  // radius when adjacent to the souce, and decrease to 1 at LOS edge.
  procedure Run( Coord : TCoord2D; Radius : LongInt = -1 ); virtual; abstract;
  function isVisible( Coord : TCoord2D ) : boolean; virtual; abstract;
  function getLight( Coord : TCoord2D ) : Byte;

  // This performs a single LOS check, with arbitrary coordinates.  Again,
  // MaxDist may be -1 for no limit if the algorithm supports it.  If you
  // want to make many LOS checks with the same source, use Run and isVisible
  // instead.
  function isLoS( Src : TCoord2D; Dst : TCoord2D; MaxDist : LongInt = -1 ) : boolean; virtual; abstract;
  function getLoS( Src : TCoord2D; Dst : TCoord2D; MaxDist : LongInt = -1 ) : Byte;

  // This calculates a path, as if for a projectile, shot from Src and aimed
  // at Aim.  If Range is unspecified, the projectile will continue until it
  // hits a wall (which is included in the path), otherwise the path will have
  // length at most Range.  The actor may be considered to look around before
  // shooting, so if it has finite sight range less than the distance to Aim,
  // SightRange should be specified.  Spread is an angle in degrees, useful
  // for cone attacks.  If Spread is NOT specified, the path is ordered;
  // earlier (blocking) points will be guaranteed to come first.  The source
  // will NOT be included in the path, the target and beyond may be.
  function shotPath( Src : TCoord2D; Aim : TCoord2D; Range : LongInt = -1; Vision : LongInt = -1; Spread : LongInt = 0) : Array of TCoord2D; virtual; abstract;
  protected
  Map : IVisionQuery;
  CenterLight : Byte;
  Center : TCoord2D;
end;

// This object implements vision based on the geometric premise that all opaque
// squares contain maximal inscribed diamonds, and a square is visible iff a
// line can be drawn from the center of the source to any point in the
// destination.  Targetting uses the center of the visible portion of the aim
// tile; if the aim tile is entirely hidden, the targetting source may side
// step in the minor axis and will fire a straight ray.  This vision supports
// unlimited ranges.
type TStefanVision = class (TVision)
  constructor Create( newMap : IVisionQuery );
  procedure Run( Coord : TCoord2D; Radius : LongInt = -1 ); virtual; override;
  function isVisible( Coord : TCoord2D ) : boolean; virtual; override;
  function isLoS( Src : TCoord2D; Dst : TCoord2D; MaxDist : LongInt = -1 ) : boolean; virtual; override;
  function shotPath( Src : TCoord2D; Aim : TCoord2D; Range : LongInt = -1; Vision : LongInt = -1; Spread : LongInt = 0) : Array of TCoord2D; override;

  protected
  FFOV : TSparseSet;

  procedure ShadowCast( Src : TCoord2D; MajorDir, MinorDir : TDirection;
    Slope1N, Slope1D, Slope2N, Slope2D : Integer; MajorDist : Integer;
    MaxRadius : Integer; OnIlluminate : procedure(Src, Cell : TCoord2D;
      Slope1N, Slope1D, Slope2N, Slope2D : Integer) of object);
end;

// This calculates a straight ray, ignoring walls.  It's useful for special
// effects, but remember that it doesn't agree with any of our FOVs so you
// should never use it for vision or targeting!  Excludes source, includes
// (and stops at) Aim.
function BresenhamRay( Src : TCoord2D; Aim : TCoord2D; Range : LongInt = -1) : Array of TCoord2D;

implementation

constructor TVision.Create(newMap: IVisionQuery);
begin
  Map := newMap;
end;

procedure TVision.ChangeSource(newMap: IVisionQuery);
begin
  Map := newMap;
end;

function TVision.getLight(Coord : TCoord2D) : Byte;
begin
  if (not isVisible(Coord)) then Result := 0;
  else Result := max(1, CenterLight - min(128, Trunc(hypot(Coord.X - Center.X, Coord.Y - Center.Y))));
end;

function TVision.getLoS( Src : TCoord2D; Dst : TCoord2D; MaxDist : LoongInt = -1) : Byte;
begin
  if (not isLoS(Src, Dst, MaxDist)) then Result := 0;
  else Result := max(1, MaxDist - min(127, Trunc(hypot(Src.X - Dst.X, Src.Y - Dst.Y))));
end;


function BresenhamRay( Src : TCoord2D; Aim : TCoord2D; Range : LongInt = -1) : Array of TCoord2D;
var
  Point  : TCoord2D;
  Change : Integer;

  PMinor, PMajor       : ^Integer;
  FromMinor, FromMajor : Integer;
  ToMinor, ToMajor     : Integer;

  Len, Offs : Integer;
begin
  Point := Src;

  // What I'd really like to do is store the *field itself* in a variable.  You
  // can do that in C, C++, and Perl, but not here...
  if Abs(Src.X - Aim.X) >= Abs(Src.Y - Aim.Y) then
  begin
    PMajor    := @Point.X;
    PMinor    := @Point.Y;
    FromMajor := Src.X;
    FromMinor := Src.Y;
    ToMajor   := Dst.X;
    ToMinor   := Dst.Y;
  end
  else
  begin
    PMajor    := @Point.Y;
    PMinor    := @Point.X;
    FromMajor := Src.Y;
    FromMinor := Src.X;
    ToMajor   := Dst.Y;
    ToMinor   := Dst.X;
  end;

  Len  := Abs(ToMajor - FromMajor);
  Offs := 0;

  SetLength(BresenhamRay, Len);

  Change := Len;

  while PMajor^ <> ToMajor do
  begin
    Change := Change + Abs(ToMinor - FromMinor);

    if Change > 2*Len then
    begin
      Change := Change - 2*Len;

      if ToMinor > FromMinor then PMinor^ := PMinor^ + 1;
                             else PMinor^ := PMinor^ - 1;

    end;

    if ToMajor > FromMajor then PMajor^ := PMajor^ + 1;
                           else PMajor^ := PMajor^ - 1;

    BresenhamRay[Offs] := Point;
    Offs := Offs + 1;
  end;
end;

f

// Modified      : $Date: 2008-10-14 19:45:46 +0200 (Tue, 14 Oct 2008) $
// Last revision : $Revision: 227 $
// Last author   : $Author: chaos-dev $
// Last commit   : $Log$
// Head URL      : $HeadURL: https://libvalkyrie.svn.sourceforge.net/svnroot/libvalkyrie/fp/src/vvision.pas $
