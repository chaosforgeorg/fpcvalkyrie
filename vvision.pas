{$INCLUDE valkyrie.inc}
// @abstract(Roguelike Toolkit for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(Jan 12, 2008)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-10-14 19:45:46 +0200 (Tue, 14 Oct 2008) $)
//
// Gathers some useful functions for roguelike development in Valkyrie.
// As for the current state it's experimental.
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

unit vvision;
interface

uses Classes, SysUtils, vrltools;

type IVisionQuery = interface
  function blocksVision( const Coord : TCoord2D ) : boolean;
end;


type TVision = class
  constructor Create( newMap : IVisionQuery );
  procedure ChangeSource( newMap : IVisionQuery ); virtual;
  procedure Run( Coord : TCoord2D; Radius : LongInt ); virtual; abstract;
  function isVisible( Coord : TCoord2D ) : boolean;
  function getLight( Coord : TCoord2D ) : Byte; virtual; abstract;
//  function isLoS(sX,sY,tX,tY : LongInt) : boolean; virtual; abstract;
  protected
  Map : IVisionQuery;
end;

type TIsaacVision = class (TVision)
  constructor Create( newMap : IVisionQuery; newMaxRadius : DWord );
  procedure Run( Coord : TCoord2D; Radius : LongInt ); override;
  function getLight( Coord : TCoord2D ) : Byte; override;
//  function isLoS(sX,sY,tX,tY : LongInt) : boolean; override;
  protected
  procedure Clear(Value : Byte = 0);
  procedure setLight( Coord : TCoord2D; Value : LongInt);
  protected
  Source    : TCoord2D;
  Light     : array of array of Word;
  MaxRadius : LongInt;
end;

type TBresenhamRay = object
  Done   : Boolean;
  cnt    : Word;
  Xsign  : Integer;
  Ysign  : Integer;
  public
  procedure Init(nx1,ny1,nx2,ny2 : integer);
  procedure Init(c1,c2 : TCoord2D);
  procedure Next;
  function GetX : Integer;
  function GetY : Integer;
  function GetC : TCoord2D;
  private
  Orto   : Boolean;
  dx,dy  : Integer;
  bx,by  : Integer;
  tx,ty  : Integer;
  p      : Integer;
  c1,c2  : Integer;
end;

type

{ TVisionRay }

TVisionRay = object
  Done   : Boolean;
  Xsign  : Integer;
  Ysign  : Integer;
  Map    : IVisionQuery;
  cnt    : DWord;
  coord  : TCoord2D;
  prev   : TCoord2D;
  public
  procedure Init(newMap : IVisionQuery; nx1,ny1,nx2,ny2 : integer; precision : Single = 0.6);
  procedure Init(newMap : IVisionQuery; c1,c2 : TCoord2D; precision : Single = 0.6);
  procedure Next;
  function GetPrev : TCoord2D;
  function GetX : Integer;
  function GetY : Integer;
  function GetC : TCoord2D;
  function GetSource : TCoord2D;
  function GetTarget : TCoord2D;
  private
  dcnt    : DWord;
  fdx,fdy : Single;
  fx,fy   : Single;
  tx,ty   : Integer;
  sx,sy   : Integer;
end;

implementation
uses vmath,math;

constructor TVision.Create(newMap: IVisionQuery);
begin
  Map := newMap;
end;

procedure TVision.ChangeSource(newMap: IVisionQuery);
begin
  Map := newMap;
end;

function TVision.isVisible( Coord : TCoord2D ): boolean;
begin
  Exit( getLight( Coord ) > 0 );
end;

{ TIsaacVision }

constructor TIsaacVision.Create(newMap: IVisionQuery; newMaxRadius: DWord);
var Count : DWord;
begin
  inherited Create(newMap);
  MaxRadius := newMaxRadius;
  SetLength(Light,maxRadius*2+4);
  for Count := 0 to maxRadius*2+3 do
    SetLength(Light[Count],maxRadius*2+4);
end;

function TIsaacVision.getLight( Coord : TCoord2D ): byte;
var Translated : TCoord2D;
begin
  Translated.X := Coord.X - source.X + maxRadius + 1;
  Translated.Y := Coord.Y - source.Y + maxRadius + 1;
  if (Translated.x < 0) or (Translated.y < 0) or
     (Translated.x > maxRadius*2+3) or (Translated.y > maxRadius*2+3) then Exit(0);
  Exit( Light[Translated.x,Translated.y] );
end;

procedure TIsaacVision.setLight( Coord : TCoord2D; Value : LongInt );
var tX,tY : LongInt;
begin
  tX := Coord.X - source.X + maxRadius + 1;
  tY := Coord.Y - source.Y + maxRadius + 1;
  Light[tx,ty] := Max(Value,0);
end;

procedure TIsaacVision.Clear(Value : Byte = 0);
var x,y : LongInt;
begin
  for x := 0 to maxRadius*2+3 do
    for y := 0 to maxRadius*2+3 do
      Light[x,y] := Value;
end;


// Special thanks for this procedure goes to Isaac Kuo. This beamcasting
// algorithm is a ported to FreePascal modified version of his algorithm
// posted on http://www.roguelikedevelopment.org

procedure TIsaacVision.Run( Coord : TCoord2D; Radius : LongInt );
var t : TCoord2D;
    mini, maxi, cor, u, v : LongInt;
    quad, slope, l : Byte;
const quads : array[1..4] of array[1..2] of ShortInt =
      ((1,1),(-1,-1),(-1,+1),(+1,-1));
      odir : array[1..4] of array[1..2] of ShortInt =
      ((1,0),(-1,0),(0,1),(0,-1));
      RayNumber          = 32; // It's effective to keep this number a power of 2.
      RayWidthCorrection = 8; // Must be smaller then RayNumber/2 ;-)

begin
  if Radius > maxRadius then Radius := maxRadius;

  Source := Coord;

  Clear;

  // Set 0,0 to be visible even if the player is
  // standing on something opaque
  setLight(Coord,Radius);
  
  l := 0;

  // Check the orthogonal directions
  for quad := 1 to 4 do
    for l := 1 to Radius do
    begin
      t := coord.ifInc( l * odir[quad,1], l * odir[quad,2] );
      setLight( t , Radius - l + 1 );
      if Map.blocksVision( t ) then break;
    end;

  // Loop through the quadrants
  for quad := 1 to 4 do
  // Now loop on the diagonal directions
  for slope := 1 to RayNumber-1 do
  begin
    // initialize the v coordinate and set the beam size
    // to maximum--mini and maxi store the beam\'s current
    // top and bottom positions.
    // As long as mini<maxi, the beam has some width.
    // When mini=maxi, the beam is a thin line.
    // When mini>maxi, the beam has been blocked.

    v := slope; u := 0;
    mini := RayWidthCorrection; maxi := RayNumber-RayWidthCorrection;
    repeat
      Inc(u);
      t.y:= v div RayNumber;
      t.x:= u - t.y;  //Do the transform
      
      cor:= RayNumber-(v mod RayNumber);         //calculate the position of block corner within beam
      
      if mini < cor then begin //beam is low enough to hit (x,y) block
        if Map.blocksVision( coord.ifInc( quads[quad][1]*t.x,quads[quad][2]*t.y) ) then mini := cor; //beam was partially blocked
        l := Distance(t.x,t.y,0,0);
        if l > Radius then Break;
        Light[quads[quad][1]*t.x+maxRadius+1,quads[quad][2]*t.y+maxRadius+1] := Radius-l+1;
      end;
      if maxi > cor then begin //beam is high enough to hit (x-1,y+1) block
        if Map.blocksVision( coord.ifInc( quads[quad][1]*(t.x-1), quads[quad][2]*(t.y+1) ) ) then maxi := cor; //beam was partially blocked
        l := Distance(t.x-1,t.y+1,0,0);
        if l > Radius then Break;
        Light[quads[quad][1]*(t.x-1)+maxRadius+1,quads[quad][2]*(t.y+1)+maxRadius+1] := Radius-l+1;
      end;
      v := v + slope;  //increment the beam\'s v coordinate
    until (mini > maxi);
  end;
end;


procedure TBresenhamRay.Init(nx1,ny1,nx2,ny2 : integer);
begin
  Done := False;
  cnt := 0;
  bx := nx1;
  by := ny1;
  tx := nx2;
  ty := ny2;
  
  dx := tx - bx;
  dy := ty - by;

  Orto := (dx*dy = 0);
  xsign := Sgn(dx);
  ysign := Sgn(dy);

  dx := Abs(dx);
  dy := Abs(dy);
  if (dx < dy) then
  begin
    p  := 2*dx - dy;
    c1 := 2*dx;
    c2 := 2*dx-2*dy;
  end
  else
  begin
    p  := 2*dy - dx;
    c1 := 2*dy;
    c2 := 2*dy-2*dx;
  end;
end;

procedure TBresenhamRay.Init(c1,c2 : TCoord2D);
begin
  Init(c1.x,c1.y,c2.x,c2.y);
end;

procedure TBresenhamRay.Next;
begin
  Inc(cnt);
  if Orto then
  begin
    if (dy = 0) then bx += xsign
                else by += ysign;
  end
  else
    if (dx < dy) then
    begin
      by += ysign;
      if (p < 0) then p += c1 else
      begin
        p += c2;
        bx += xsign;
      end;
    end
    else
    begin
      bx += xsign;
      if (p < 0) then p += c1 else
      begin
        p += c2;
        by += ysign;
      end;
    end;


  if (bx = tx) and (by = ty) then Done := True;

end;

function TBresenhamRay.GetX: Integer;
begin
  Exit(bx);
end;

function TBresenhamRay.GetY : Integer;
begin
  Exit(by);
end;

function TBresenhamRay.GetC: TCoord2D;
begin
  Exit(NewCoord2D(bx,by));
end;


{ TVision2Ray }

procedure TVisionRay.Init(newMap: IVisionQuery; nx1, ny1, nx2, ny2 : integer; precision : Single = 0.6);
var dx,dy   : Integer;
    shx,shy : Float;
    shift   : Float;
    YSigned : Boolean;
begin
  Prev.Create(nx1, ny1);
  Coord.Create(nx1, ny1);
  Done   := false;
  Map    := newMap;
  cnt    := 0;
  
  sx := nx1;
  sy := ny1;
  tx := nx2;
  ty := ny2;


  dx := nx2-nx1;
  dy := ny2-ny1;
  xsign := sgn(dx);
  ysign := sgn(dy);
  fx := nx1+0.5;
  fy := ny1+0.5;

  if xsign*ysign = 0 then
  begin
    fdx := xsign;
    fdy := ysign;
    dcnt := Abs(dy+dx);
    Exit;
  end;
  YSigned := Abs(dx) < Abs(dy);
  {$PUSH}
  {$HINTS OFF}
  if YSigned then
  begin
    fdy := ysign;
    fdx := xsign*abs(dx/dy);
    dcnt := Abs(dy);
  end
  else
  begin
    fdx := xsign;
    fdy := ysign*abs(dy/dx);
    dcnt := Abs(dx);
  end;
  {$POP} {restore $HINTS}

  shx := 0;
  shy := 0;

  repeat
    Inc(cnt);
    if cnt = dcnt then Break;
    fx += fdx;
    fy += fdy;
    if not (Map.blocksVision( NewCoord2D( Round(fx-0.5),(Round(fy-0.5))))) then Continue;
    shift := 0;
    if YSigned then
    begin
      if Map.blocksVision( NewCoord2D( Round(fx-0.4+precision),(Round(fy-0.5)))) then shift := shift-precision;
      if Map.blocksVision( NewCoord2D( Round(fx-0.6-precision),(Round(fy-0.5)))) then shift := shift+precision;
      if shift <> 0 then
      begin
        shx := shift;
        Break;
      end
    end else
    begin
      if Map.blocksVision( NewCoord2D( Round(fx-0.5),(Round(fy-0.4+precision)))) then shift := shift-precision;
      if Map.blocksVision( NewCoord2D( Round(fx-0.5),(Round(fy-0.6-precision)))) then shift := shift+precision;
    end;
    if shift <> 0 then
    begin
      shy := shift;
      Break;
    end
  until cnt >= dcnt;

  cnt := 0;
  fx := nx1+0.5+shx;
  fy := ny1+0.5+shy;
end;

procedure TVisionRay.Init(newMap : IVisionQuery; c1,c2 : TCoord2D; precision : Single = 0.6);
begin
  Init( newMap, c1.x, c1.y, c2.x, c2.y, precision );
end;

procedure TVisionRay.Next;
begin
  Prev := Coord;
  Inc(cnt);
  fx += fdx;
  fy += fdy;
  if cnt = dcnt then
  begin
    Coord.Create(tx,ty);
    Done := True;
  end
  else
  begin
    Coord.Create(Round(fx-0.5),Round(fy-0.5));
    if xsign < 0 then Coord.X := Min(Coord.X,sx) else Coord.X := Max(Coord.X,sx);
    if ysign < 0 then Coord.Y := Min(Coord.Y,sy) else Coord.Y := Max(Coord.Y,sy);
  end;
end;

function TVisionRay.GetPrev: TCoord2D;
begin
  Exit( Prev );
end;

function TVisionRay.GetX: Integer;
begin
  Exit( Coord.X );
end;

function TVisionRay.GetY: Integer;
begin
  Exit( Coord.Y );
end;

function TVisionRay.GetC: TCoord2D;
begin
  Exit( Coord );
end;

function TVisionRay.GetSource: TCoord2D;
begin
  GetSource.x := sx;
  GetSource.y := sy;
end;

function TVisionRay.GetTarget: TCoord2D;
begin
  GetTarget.x := tx;
  GetTarget.y := ty;
end;


end.


// Modified      : $Date: 2008-10-14 19:45:46 +0200 (Tue, 14 Oct 2008) $
// Last revision : $Revision: 227 $
// Last author   : $Author: chaos-dev $
// Last commit   : $Log$
// Head URL      : $HeadURL: https://libvalkyrie.svn.sourceforge.net/svnroot/libvalkyrie/fp/src/vvision.pas $
