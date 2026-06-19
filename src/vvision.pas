{$INCLUDE valkyrie.inc}
// @abstract(Vision/LoS for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(Jan 12, 2008)
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
  function blocksVision( const aCoord : TCoord2D ) : boolean;
end;

const CIsaacRayNumber          = 32;  // It's effective to keep this number a power of 2.
      CIsaacRayWidthCorrection = 8;   // Must be smaller then CIsaacRayNumber/2 ;-)
      CIsaacRayMaxPath         = 128;

type TIsaacRayPath = array[0..CIsaacRayMaxPath] of TCoord2D;

type TVision = class
  constructor Create( aMap : IVisionQuery );
  procedure ChangeSource( aMap : IVisionQuery ); virtual;
  procedure Run( aCoord : TCoord2D; aRadius : LongInt ); virtual; abstract;
  function isVisible( aCoord : TCoord2D ) : boolean;
  function getLight( aCoord : TCoord2D ) : Byte; virtual; abstract;
protected
  FMap : IVisionQuery;
end;

type TIsaacVision = class (TVision)
  constructor Create( aMap : IVisionQuery; aMaxRadius : DWord );
  procedure Run( aCoord : TCoord2D; aRadius : LongInt ); override;
  function getLight( aCoord : TCoord2D ) : Byte; override;
protected
  procedure Clear( aValue : Byte = 0 );
  procedure setLight( aCoord : TCoord2D; aValue : LongInt );
protected
  FSource    : TCoord2D;
  FLight     : array of array of Word;
  FMaxRadius : LongInt;
end;

type TBresenhamRay = object
  procedure Init( aX1, aY1, aX2, aY2 : integer );
  procedure Init( aCoord1, aCoord2 : TCoord2D );
  procedure Next;
private
  FDone     : Boolean;
  FCnt      : Integer;
  FXSign    : Integer;
  FYSign    : Integer;
  FOrto     : Boolean;
  FDX, FDY  : Integer;
  FBX, FBY  : Integer;
  FTX, FTY  : Integer;
  FP        : Integer;
  FC1, FC2  : Integer;
  FCoord    : TCoord2D;
public 
  property Steps    : Integer      read FCnt;
  property Current  : TCoord2D     read FCoord;
  property Done     : Boolean      read FDone;
end;

type

{ TIsaacRay }

TIsaacRay = object
public
  procedure Init( aMap : IVisionQuery; aX1, aY1, aX2, aY2 : integer; aRange : Word = 0; aVisionRange : Word = 0 );
  procedure Init( aMap : IVisionQuery; aCoord1, aCoord2 : TCoord2D; aRange : Word = 0; aVisionRange : Word = 0 );
  procedure Next;
private
  function AddPathStep( const aCoord : TCoord2D ) : Boolean;
  function AppendBresenhamPath( const aFrom, aTarget : TCoord2D ) : Boolean;
  function BuildDirectPathTo( const aTarget : TCoord2D ) : Boolean;
  function BuildDirectPrefixTo( const aTarget : TCoord2D ) : Boolean;
  function IsIsaacLit( const aCoord : TCoord2D ) : Boolean;
  function RayExtensionTarget : TCoord2D;
private
  FDone       : Boolean;
  FMap        : IVisionQuery;
  FCnt        : Integer;
  FCoord      : TCoord2D;
  FPrev       : TCoord2D;
  FTarget     : TCoord2D;
  FSource     : TCoord2D;
  FRange      : Integer;
  FVisionRange: Integer;
  FBallisticFallback : Boolean;
  FPath       : TIsaacRayPath;
  FPathIndex  : Integer;
  FPathLast   : Integer;
public
  property Map      : IVisionQuery read FMap;
  property Steps    : Integer      read FCnt;
  property Previous : TCoord2D     read FPrev;
  property Current  : TCoord2D     read FCoord;
  property Source   : TCoord2D     read FSource;
  property Target   : TCoord2D     read FTarget;
  property Done     : Boolean      read FDone;
end;

type

{ TVisionRay }

TVisionRay = object
public
  procedure Init( aMap : IVisionQuery; aX1, aY1, aX2, aY2 : integer; aPrecision : Single = 0.6 );
  procedure Init( aMap : IVisionQuery; aCoord1, aCoord2 : TCoord2D; aPrecision : Single = 0.6 );
  procedure Next;
private
  FDone     : Boolean;
  FXSign    : Integer;
  FYSign    : Integer;
  FMap      : IVisionQuery;
  FCnt      : Integer;
  FCoord    : TCoord2D;
  FPrev     : TCoord2D;
  FTarget   : TCoord2D;
  FSource   : TCoord2D;
  FDCnt     : Integer;
  FDX, FDY  : Single;
  FX, FY    : Single;
public 
  property Map      : IVisionQuery read FMap;
  property Steps    : Integer      read FCnt;
  property Previous : TCoord2D     read FPrev;
  property Current  : TCoord2D     read FCoord;
  property Source   : TCoord2D     read FSource;
  property Target   : TCoord2D     read FTarget;
  property Done     : Boolean      read FDone;
end;

implementation
uses vmath,math;

const CIsaacQuads : array[1..4] of array[1..2] of ShortInt =
      ((1,1),(-1,-1),(-1,+1),(+1,-1));
      CIsaacODir : array[1..4] of array[1..2] of ShortInt =
      ((1,0),(-1,0),(0,1),(0,-1));

constructor TVision.Create( aMap : IVisionQuery );
begin
  FMap := aMap;
end;

procedure TVision.ChangeSource( aMap : IVisionQuery );
begin
  FMap := aMap;
end;

function TVision.isVisible( aCoord : TCoord2D ) : boolean;
begin
  Exit( getLight( aCoord ) > 0 );
end;

{ TIsaacVision }

constructor TIsaacVision.Create( aMap : IVisionQuery; aMaxRadius : DWord );
var iCount : DWord;
begin
  inherited Create( aMap );
  FMaxRadius := aMaxRadius;
  SetLength( FLight, FMaxRadius*2+4 );
  for iCount := 0 to FMaxRadius*2+3 do
    SetLength( FLight[iCount], FMaxRadius*2+4 );
end;

function TIsaacVision.getLight( aCoord : TCoord2D ) : byte;
var iTranslated : TCoord2D;
begin
  iTranslated.X := aCoord.X - FSource.X + FMaxRadius + 1;
  iTranslated.Y := aCoord.Y - FSource.Y + FMaxRadius + 1;
  if (iTranslated.x < 0) or (iTranslated.y < 0) or
     (iTranslated.x > FMaxRadius*2+3) or (iTranslated.y > FMaxRadius*2+3) then Exit( 0 );
  Exit( FLight[iTranslated.x,iTranslated.y] );
end;

procedure TIsaacVision.setLight( aCoord : TCoord2D; aValue : LongInt );
var iX, iY : LongInt;
begin
  iX := aCoord.X - FSource.X + FMaxRadius + 1;
  iY := aCoord.Y - FSource.Y + FMaxRadius + 1;
  FLight[iX,iY] := Max( aValue, 0 );
end;

procedure TIsaacVision.Clear( aValue : Byte = 0 );
var iX, iY : LongInt;
begin
  for iX := 0 to FMaxRadius*2+3 do
    for iY := 0 to FMaxRadius*2+3 do
      FLight[iX,iY] := aValue;
end;


// Special thanks for this procedure goes to Isaac Kuo. This beamcasting
// algorithm is a ported to FreePascal modified version of his algorithm
// posted on http://www.roguelikedevelopment.org
procedure TIsaacVision.Run( aCoord : TCoord2D; aRadius : LongInt );
var iTarget : TCoord2D;
    iMini, iMaxi, iCor, iU, iV : LongInt;
    iQuad, iSlope, iLight : Byte;
begin
  if aRadius > FMaxRadius then aRadius := FMaxRadius;

  FSource := aCoord;

  Clear;

  // Set 0,0 to be visible even if the player is
  // standing on something opaque
  setLight( aCoord, aRadius );
  
  iLight := 0;

  // Check the orthogonal directions
  for iQuad := 1 to 4 do
    for iLight := 1 to aRadius do
    begin
      iTarget := aCoord.ifInc( iLight * CIsaacODir[iQuad,1], iLight * CIsaacODir[iQuad,2] );
      setLight( iTarget, aRadius - iLight + 1 );
      if FMap.blocksVision( iTarget ) then break;
    end;

  // Loop through the quadrants
  for iQuad := 1 to 4 do
  // Now loop on the diagonal directions
  for iSlope := 1 to CIsaacRayNumber-1 do
  begin
    // initialize the v coordinate and set the beam size
    // to maximum--mini and maxi store the beam\'s current
    // top and bottom positions.
    // As long as mini<maxi, the beam has some width.
    // When mini=maxi, the beam is a thin line.
    // When mini>maxi, the beam has been blocked.

    iV := iSlope; iU := 0;
    iMini := CIsaacRayWidthCorrection; iMaxi := CIsaacRayNumber-CIsaacRayWidthCorrection;
    repeat
      Inc( iU );
      iTarget.y:= iV div CIsaacRayNumber;
      iTarget.x:= iU - iTarget.y;  //Do the transform
      
      iCor:= CIsaacRayNumber-(iV mod CIsaacRayNumber);         //calculate the position of block corner within beam
      
      if iMini < iCor then begin //beam is low enough to hit (x,y) block
        if FMap.blocksVision( aCoord.ifInc( CIsaacQuads[iQuad][1]*iTarget.x, CIsaacQuads[iQuad][2]*iTarget.y ) ) then iMini := iCor; //beam was partially blocked
        iLight := Distance( iTarget.x, iTarget.y, 0, 0 );
        if iLight > aRadius then Break;
        FLight[CIsaacQuads[iQuad][1]*iTarget.x+FMaxRadius+1,CIsaacQuads[iQuad][2]*iTarget.y+FMaxRadius+1] := aRadius-iLight+1;
      end;
      if iMaxi > iCor then begin //beam is high enough to hit (x-1,y+1) block
        if FMap.blocksVision( aCoord.ifInc( CIsaacQuads[iQuad][1]*(iTarget.x-1), CIsaacQuads[iQuad][2]*(iTarget.y+1) ) ) then iMaxi := iCor; //beam was partially blocked
        iLight := Distance( iTarget.x-1, iTarget.y+1, 0, 0 );
        if iLight > aRadius then Break;
        FLight[CIsaacQuads[iQuad][1]*(iTarget.x-1)+FMaxRadius+1,CIsaacQuads[iQuad][2]*(iTarget.y+1)+FMaxRadius+1] := aRadius-iLight+1;
      end;
      iV := iV + iSlope;  //increment the beam\'s v coordinate
    until (iMini > iMaxi);
  end;
end;

{ TIsaacRay }

procedure TIsaacRay.Init( aMap : IVisionQuery; aX1, aY1, aX2, aY2 : integer; aRange : Word = 0; aVisionRange : Word = 0 );
var iEndTarget : TCoord2D;
begin
  FMap := aMap;
  FSource.Create( aX1, aY1 );
  FTarget.Create( aX2, aY2 );
  FPrev := FSource;
  FCoord := FSource;
  FCnt := 0;
  FPathIndex := 0;
  FPathLast := 0;
  FPath[0] := FSource;
  FBallisticFallback := aRange <> 0;
  if aRange = 0
    then FRange := Distance( FSource, FTarget )
    else FRange := aRange;
  if aVisionRange = 0
    then FVisionRange := FRange
    else FVisionRange := aVisionRange;

  if FSource = FTarget then
  begin
    FDone := True;
    Exit;
  end;

  if IsIsaacLit( FTarget ) then
  begin
    if not BuildDirectPathTo( FTarget ) then
      BuildDirectPrefixTo( FTarget );
  end
  else
    BuildDirectPrefixTo( FTarget );

  iEndTarget := RayExtensionTarget;
  if FBallisticFallback and (FPathLast > 0) and (FPath[FPathLast] = FTarget) and (FPath[FPathLast] <> iEndTarget) then
    AppendBresenhamPath( FPath[FPathLast], iEndTarget );

  FDone := FPathLast = 0;
end;

procedure TIsaacRay.Init( aMap : IVisionQuery; aCoord1, aCoord2 : TCoord2D; aRange : Word = 0; aVisionRange : Word = 0 );
begin
  Init( aMap, aCoord1.x, aCoord1.y, aCoord2.x, aCoord2.y, aRange, aVisionRange );
end;

function TIsaacRay.AddPathStep( const aCoord : TCoord2D ) : Boolean;
begin
  if FPathLast >= CIsaacRayMaxPath then Exit( False );
  Inc( FPathLast );
  FPath[ FPathLast ] := aCoord;
  Exit( True );
end;

function TIsaacRay.AppendBresenhamPath( const aFrom, aTarget : TCoord2D ) : Boolean;
var iRay : TBresenhamRay;
begin
  if aFrom = aTarget then Exit( True );
  iRay.Init( aFrom, aTarget );
  repeat
    iRay.Next;
    if not AddPathStep( iRay.Current ) then Exit( False );
  until iRay.Done;
  Exit( True );
end;

function TIsaacRay.RayExtensionTarget : TCoord2D;
var iDiff : TCoord2D;
    iDist : Integer;
begin
  iDiff := FTarget - FSource;
  iDist := Distance( FSource, FTarget );
  if (iDist = 0) or (FRange <= iDist) then Exit( FTarget );
  Result.Create(
    FSource.X + Round( iDiff.X * FRange / iDist ),
    FSource.Y + Round( iDiff.Y * FRange / iDist )
  );
end;

function TIsaacRay.IsIsaacLit( const aCoord : TCoord2D ) : Boolean;
var iDX, iDY     : Integer;
    iLocalX      : Integer;
    iLocalY      : Integer;
    iQuad        : Byte;
    iSlope       : Byte;
    iMini, iMaxi : LongInt;
    iCor, iU, iV : LongInt;
    iTarget      : TCoord2D;
    iCheck       : TCoord2D;
    iLight       : DWord;
begin
  if aCoord = FSource then Exit( True );
  if Distance( FSource, aCoord ) > FVisionRange then Exit( False );

  iDX := aCoord.X - FSource.X;
  iDY := aCoord.Y - FSource.Y;

  if iDX * iDY = 0 then
  begin
    if iDX > 0 then iQuad := 1
    else if iDX < 0 then iQuad := 2
    else if iDY > 0 then iQuad := 3
    else iQuad := 4;
    for iLight := 1 to FVisionRange do
    begin
      iCheck := FSource.ifInc( iLight * CIsaacODir[iQuad,1], iLight * CIsaacODir[iQuad,2] );
      if iCheck = aCoord then Exit( True );
      if FMap.blocksVision( iCheck ) then Break;
    end;
    Exit( False );
  end;

  if (iDX > 0) and (iDY > 0) then iQuad := 1
  else if (iDX < 0) and (iDY < 0) then iQuad := 2
  else if (iDX < 0) and (iDY > 0) then iQuad := 3
  else iQuad := 4;

  iLocalX := Abs( iDX );
  iLocalY := Abs( iDY );

  for iSlope := 1 to CIsaacRayNumber-1 do
  begin
    iV := iSlope; iU := 0;
    iMini := CIsaacRayWidthCorrection;
    iMaxi := CIsaacRayNumber-CIsaacRayWidthCorrection;
    repeat
      Inc( iU );
      iTarget.y := iV div CIsaacRayNumber;
      iTarget.x := iU - iTarget.y;
      iCor := CIsaacRayNumber-(iV mod CIsaacRayNumber);

      if iMini < iCor then
      begin
        iCheck := FSource.ifInc( CIsaacQuads[iQuad][1]*iTarget.x, CIsaacQuads[iQuad][2]*iTarget.y );
        if FMap.blocksVision( iCheck ) then iMini := iCor;
        iLight := Distance( iTarget.x, iTarget.y, 0, 0 );
        if iLight > DWord(FVisionRange) then Break;
        if (iTarget.x = iLocalX) and (iTarget.y = iLocalY) then Exit( True );
      end;
      if iMaxi > iCor then
      begin
        iCheck := FSource.ifInc( CIsaacQuads[iQuad][1]*(iTarget.x-1), CIsaacQuads[iQuad][2]*(iTarget.y+1) );
        if FMap.blocksVision( iCheck ) then iMaxi := iCor;
        iLight := Distance( iTarget.x-1, iTarget.y+1, 0, 0 );
        if iLight > DWord(FVisionRange) then Break;
        if (iTarget.x-1 = iLocalX) and (iTarget.y+1 = iLocalY) then Exit( True );
      end;
      iV := iV + iSlope;
    until (iMini > iMaxi);
  end;
  Exit( False );
end;

function TIsaacRay.BuildDirectPathTo( const aTarget : TCoord2D ) : Boolean;
var iDiff       : TCoord2D;
    iSign       : TCoord2D;
    iAbsX       : Integer;
    iAbsY       : Integer;
    iLocalX     : Integer;
    iLocalY     : Integer;
    iStepX      : Integer;
    iStepY      : Integer;
    iNextX      : Integer;
    iNextY      : Integer;
    iBestX      : Integer;
    iBestY      : Integer;
    iScore      : Int64;
    iBestScore  : Int64;
    iCandidate  : TCoord2D;
begin
  FPathLast := 0;
  FPath[0] := FSource;

  iDiff := aTarget - FSource;
  iSign := iDiff.Sign;
  iAbsX := Abs( iDiff.X );
  iAbsY := Abs( iDiff.Y );

  if iAbsX * iAbsY = 0 then
  begin
    if not AppendBresenhamPath( FSource, aTarget ) then
    begin
      FPathLast := 0;
      Exit( False );
    end;
    Exit( FPathLast > 0 );
  end;

  iLocalX := 0;
  iLocalY := 0;
  repeat
  begin
    iBestX := -1;
    iBestY := -1;
    iBestScore := High( Int64 );
    for iStepX := 0 to 1 do
      for iStepY := 0 to 1 do
      begin
        if iStepX + iStepY = 0 then Continue;
        iNextX := iLocalX + iStepX;
        iNextY := iLocalY + iStepY;
        if (iNextX > iAbsX) or (iNextY > iAbsY) then Continue;
        iCandidate := FSource.ifInc( iSign.X * iNextX, iSign.Y * iNextY );
        if iCandidate <> aTarget then
        begin
          if FMap.blocksVision( iCandidate ) then Continue;
          if not IsIsaacLit( iCandidate ) then Continue;
        end;
        iScore := Sqr( Int64( iAbsY ) * iNextX - Int64( iAbsX ) * iNextY ) * 16 + 2 - iStepX - iStepY;
        if iScore < iBestScore then
        begin
          iBestScore := iScore;
          iBestX := iNextX;
          iBestY := iNextY;
        end;
      end;
    if iBestX < 0 then
    begin
      FPathLast := 0;
      Exit( False );
    end;
    iLocalX := iBestX;
    iLocalY := iBestY;
    iCandidate := FSource.ifInc( iSign.X * iLocalX, iSign.Y * iLocalY );
    if not AddPathStep( iCandidate ) then
    begin
      FPathLast := 0;
      Exit( False );
    end;
  end
  until (iLocalX = iAbsX) and (iLocalY = iAbsY);

  Exit( FPathLast > 0 );
end;

function TIsaacRay.BuildDirectPrefixTo( const aTarget : TCoord2D ) : Boolean;
var iCurrent  : TCoord2D;
    iImpact   : TCoord2D;
    iDiff     : TCoord2D;
    iSign     : TCoord2D;
    iAbsX     : Integer;
    iAbsY     : Integer;
    iStepX    : Integer;
    iStepY    : Integer;
    iCrossX   : Int64;
    iCrossY   : Int64;
    iContinue : Boolean;
    iBlocks   : Boolean;
begin
  FPathLast := 0;
  FPath[0] := FSource;
  iContinue := False;
  iCurrent := FSource;
  iDiff    := aTarget - FSource;
  iSign    := iDiff.Sign;
  iAbsX    := Abs( iDiff.X );
  iAbsY    := Abs( iDiff.Y );
  iStepX   := 0;
  iStepY   := 0;
  iImpact  := aTarget;
  repeat
    if iAbsX = 0 then
    begin
      iCurrent.Y += iSign.Y;
      Inc( iStepY );
    end
    else if iAbsY = 0 then
    begin
      iCurrent.X += iSign.X;
      Inc( iStepX );
    end
    else
    begin
      iCrossX := Int64( 2 * iStepX + 1 ) * iAbsY;
      iCrossY := Int64( 2 * iStepY + 1 ) * iAbsX;
      if iCrossX < iCrossY then
      begin
        iCurrent.X += iSign.X;
        Inc( iStepX );
      end
      else if iCrossY < iCrossX then
      begin
        iCurrent.Y += iSign.Y;
        Inc( iStepY );
      end
      else
      begin
        iCurrent.X += iSign.X;
        iCurrent.Y += iSign.Y;
        Inc( iStepX );
        Inc( iStepY );
      end;
    end;
    iBlocks := FMap.blocksVision( iCurrent );
    if (not IsIsaacLit( iCurrent )) and (not iBlocks) then
    begin
      iImpact := iCurrent;
      iContinue := True;
      Break;
    end;
    if iBlocks then
    begin
      iImpact := iCurrent;
      iContinue := True;
      Break;
    end;
  until iCurrent = aTarget;
  if not AppendBresenhamPath( FSource, iImpact ) then
  begin
    FPathLast := 0;
    Exit( False );
  end;
  if FBallisticFallback and iContinue and (iImpact <> aTarget) then
    if not AppendBresenhamPath( iImpact, aTarget ) then
    begin
      FPathLast := 0;
      Exit( False );
    end;
  Exit( FPathLast > 0 );
end;

procedure TIsaacRay.Next;
begin
  if FDone then Exit;
  FPrev := FCoord;
  Inc( FPathIndex );
  FCoord := FPath[ FPathIndex ];
  FCnt := FPathIndex;
  if FPathIndex >= FPathLast then FDone := True;
end;


procedure TBresenhamRay.Init( aX1, aY1, aX2, aY2 : integer );
begin
  FDone := False;
  FCnt := 0;
  FBX := aX1;
  FBY := aY1;
  FTX := aX2;
  FTY := aY2;
  
  FDX := FTX - FBX;
  FDY := FTY - FBY;

  FOrto := (FDX*FDY = 0);
  FXSign := Sgn( FDX );
  FYSign := Sgn( FDY );

  FDX := Abs( FDX );
  FDY := Abs( FDY );
  if (FDX < FDY) then
  begin
    FP  := 2*FDX - FDY;
    FC1 := 2*FDX;
    FC2 := 2*FDX-2*FDY;
  end
  else
  begin
    FP  := 2*FDY - FDX;
    FC1 := 2*FDY;
    FC2 := 2*FDY-2*FDX;
  end;
  FCoord.Create( FBX, FBY );
end;

procedure TBresenhamRay.Init( aCoord1, aCoord2 : TCoord2D );
begin
  Init( aCoord1.x, aCoord1.y, aCoord2.x, aCoord2.y );
end;

procedure TBresenhamRay.Next;
begin
  Inc( FCnt );
  if FOrto then
  begin
    if (FDY = 0) then FBX += FXSign
                 else FBY += FYSign;
  end
  else
    if (FDX < FDY) then
    begin
      FBY += FYSign;
      if (FP < 0) then FP += FC1 else
      begin
        FP += FC2;
        FBX += FXSign;
      end;
    end
    else
    begin
      FBX += FXSign;
      if (FP < 0) then FP += FC1 else
      begin
        FP += FC2;
        FBY += FYSign;
      end;
    end;


  if (FBX = FTX) and (FBY = FTY) then FDone := True;
  FCoord.Create( FBX, FBY );
end;

{ TVision2Ray }

procedure TVisionRay.Init( aMap : IVisionQuery; aX1, aY1, aX2, aY2 : integer; aPrecision : Single = 0.6 );
var iDX, iDY     : Integer;
    iShiftX, iShiftY : Float;
    iShift       : Float;
    iYSigned     : Boolean;
begin
  FPrev.Create( aX1, aY1 );
  FCoord.Create( aX1, aY1 );
  FDone := false;
  FMap  := aMap;
  FCnt  := 0;
  
  FSource.Create( aX1, aY1 );
  FTarget.Create( aX2, aY2 );

  iDX := aX2-aX1;
  iDY := aY2-aY1;
  FXSign := sgn( iDX );
  FYSign := sgn( iDY );
  FX := aX1+0.5;
  FY := aY1+0.5;

  if FXSign*FYSign = 0 then
  begin
    FDX := FXSign;
    FDY := FYSign;
    FDCnt := Abs( iDY+iDX );
    Exit;
  end;
  iYSigned := Abs( iDX ) < Abs( iDY );
  {$PUSH}
  {$HINTS OFF}
  if iYSigned then
  begin
    FDY := FYSign;
    FDX := FXSign*abs( iDX/iDY );
    FDCnt := Abs( iDY );
  end
  else
  begin
    FDX := FXSign;
    FDY := FYSign*abs( iDY/iDX );
    FDCnt := Abs( iDX );
  end;
  {$POP} {restore $HINTS}

  iShiftX := 0;
  iShiftY := 0;

  repeat
    Inc( FCnt );
    if FCnt = FDCnt then Break;
    FX += FDX;
    FY += FDY;
    if not (FMap.blocksVision( NewCoord2D( Round( FX-0.5 ), (Round( FY-0.5 )) ) )) then Continue;
    iShift := 0;
    if iYSigned then
    begin
      if FMap.blocksVision( NewCoord2D( Round( FX-0.4+aPrecision ), (Round( FY-0.5 )) ) ) then iShift := iShift-aPrecision;
      if FMap.blocksVision( NewCoord2D( Round( FX-0.6-aPrecision ), (Round( FY-0.5 )) ) ) then iShift := iShift+aPrecision;
      if iShift <> 0 then
      begin
        iShiftX := iShift;
        Break;
      end
    end else
    begin
      if FMap.blocksVision( NewCoord2D( Round( FX-0.5 ), (Round( FY-0.4+aPrecision )) ) ) then iShift := iShift-aPrecision;
      if FMap.blocksVision( NewCoord2D( Round( FX-0.5 ), (Round( FY-0.6-aPrecision )) ) ) then iShift := iShift+aPrecision;
    end;
    if iShift <> 0 then
    begin
      iShiftY := iShift;
      Break;
    end
  until FCnt >= FDCnt;

  FCnt := 0;
  FX := aX1+0.5+iShiftX;
  FY := aY1+0.5+iShiftY;
end;

procedure TVisionRay.Init( aMap : IVisionQuery; aCoord1, aCoord2 : TCoord2D; aPrecision : Single = 0.6 );
begin
  Init( aMap, aCoord1.x, aCoord1.y, aCoord2.x, aCoord2.y, aPrecision );
end;

procedure TVisionRay.Next;
begin
  FPrev := FCoord;
  Inc( FCnt );
  FX += FDX;
  FY += FDY;
  if FCnt = FDCnt then
  begin
    FCoord := FTarget;
    FDone := True;
  end
  else
  begin
    FCoord.Create( Round( FX-0.5 ), Round( FY-0.5 ) );
    if FXSign < 0 then FCoord.X := Min( FCoord.X, FSource.X ) else FCoord.X := Max( FCoord.X, FSource.X );
    if FYSign < 0 then FCoord.Y := Min( FCoord.Y, FSource.Y ) else FCoord.Y := Max( FCoord.Y, FSource.Y );
  end;
end;

end.
