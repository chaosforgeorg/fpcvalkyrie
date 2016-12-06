{$INCLUDE valkyrie.inc}
// @abstract(Mathematical routines for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(Oct 14, 2006)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-01-14 22:16:41 +0100 (Mon, 14 Jan 2008) $)
//
// Various mathematical constructs.
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

unit vmath;
interface

function TrueModulo( value : LongInt; modval : LongInt ) : DWord;
function Distance(x1,y1,x2,y2 : Integer): DWord;  {$IFDEF VINLINE} inline; {$ENDIF} overload;
function TriDistance(x1,y1,x2,y2 : Integer): DWord;  {$IFDEF VINLINE} inline; {$ENDIF}
function RealDistance(x1,y1,x2,y2 : Double): Double; overload;

function Minf(x,y : Single) : Single;  {$IFDEF VINLINE} inline; {$ENDIF}
function Min(x,y : LongInt) : LongInt;  {$IFDEF VINLINE} inline; {$ENDIF}
function Min(x,y,z : LongInt) : LongInt;  {$IFDEF VINLINE} inline; {$ENDIF}
function Max(x,y : LongInt) : LongInt;  {$IFDEF VINLINE} inline; {$ENDIF}
function Max(x,y,z : LongInt) : LongInt;  {$IFDEF VINLINE} inline; {$ENDIF}
function Sgn(x : LongInt) : ShortInt;  {$IFDEF VINLINE} inline; {$ENDIF}

function Clampf(x,min,max : Single) : Single;  {$IFDEF VINLINE} inline; {$ENDIF}
function Clamp(x,min,max : LongInt) : LongInt;  {$IFDEF VINLINE} inline; {$ENDIF}
function Lerp(min,max : LongInt; Value : Real) : LongInt; overload; {$IFDEF VINLINE} inline; {$ENDIF}
function S3Interpolate(min,max : LongInt; Value : Real) : LongInt;  {$IFDEF VINLINE} inline; {$ENDIF}
function S5Interpolate(min,max : LongInt; Value : Real) : LongInt;  {$IFDEF VINLINE} inline; {$ENDIF}

function UpToPowerOf2( X : DWord ) : DWord;
function IsPowerOf2( X : DWord ) : Boolean;

implementation

function Minf ( x, y : Single ) : Single; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  if x > y then exit(y) else exit(x);
end;

function Min(x,y : LongInt) : LongInt; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  if x > y then exit(y) else exit(x);
end;

function Min(x,y,z : LongInt) : LongInt; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  if x > y then exit(Min(y,z)) else exit(Min(x,z));
end;

function Max(x,y : LongInt) : LongInt; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  if x > y then exit(x) else exit(y);
end;

function Max(x,y,z : LongInt) : LongInt; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  if x > y then  exit(Max(x,z)) else exit(Max(y,z));
end;

function Sgn(x : LongInt) : ShortInt; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  if x > 0 then exit(1)
           else if x < 0 then exit(-1)
                         else exit(0);
end;

function Clampf( x, min, max : Single ) : Single;
begin
  if x < min then Exit(min)
  else if x > max then Exit(max)
       else Exit(x)
end;

function Clamp(x, min, max: LongInt): LongInt; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  if x < min then Exit(min)
  else if x > max then Exit(max)
       else Exit(x)
end;

function Lerp(min, max: LongInt; Value: Real): LongInt; inline;
begin
  Exit(min + Round((max - min)*Value));
end;

function S3Interpolate(min, max: LongInt; Value: Real): LongInt; inline;
begin
  Value := Value * Value * (3.0 - 2.0 * Value);
  Exit(Lerp(min,max,Value));
end;

function S5Interpolate(min, max: LongInt; Value: Real): LongInt; inline;
var a3,a4,a5 : Real;
begin
  a3 := Value * Value * Value;
  a4 := a3 * Value;
  a5 := a4 * Value;
  Value := (6.0 * a5) - (15.0 * a4) + (10.0 * a3);
  Exit(Lerp(min,max,Value));
end;

function UpToPowerOf2( X : DWord ) : DWord;
begin
  UpToPowerOf2 := 1;
  while UpToPowerOf2 < X do UpToPowerOf2 *= 2;
end;

function IsPowerOf2( X : DWord ) : Boolean;
begin
  Exit( (x and (x - 1)) = 0 );
end;

function TrueModulo ( value : LongInt; modval : LongInt ) : DWord;
begin
  if value < 0
    then Exit( (modval - ((-value) mod modval)) mod modval )
    else Exit( value mod modval );
end;

function Distance(x1,y1,x2,y2 : Integer): DWord; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  Distance := Round(Min(Abs(x2-x1),Abs(y2-y1)) div 2) + Max(Abs(x2-x1),Abs(y2-y1));
end;


function RealDistance(x1,y1,x2,y2 : Double): Double; 
begin
  RealDistance := Sqrt(Sqr(Abs(x2-x1)) + Sqr(Abs(y2-y1)));
end;

function TriDistance(x1,y1,x2,y2 : Integer): DWord; {$IFDEF VINLINE} inline; {$ENDIF}
begin
  TriDistance := Round(Sqrt(Sqr(Abs(x2-x1)) + Sqr(Abs(y2-y1))));
end;

end.


// Modified      : $Date: 2008-01-14 22:16:41 +0100 (Mon, 14 Jan 2008) $
// Last revision : $Revision: 110 $
// Last author   : $Author: chaos-dev $
// Last commit   : $Log$
// Head URL      : $HeadURL: https://libvalkyrie.svn.sourceforge.net/svnroot/libvalkyrie/fp/src/vmath.pas $



