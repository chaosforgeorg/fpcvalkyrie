{$INCLUDE valkyrie.inc}
// @abstract(Animation interface for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
//
// Each singleton must derive from this class. The purpose
// of @link(TSystem) is to provide a general interface
// for debugging and calling.
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
unit vanimation;
interface
uses SysUtils, Classes, vutil, vgenerics;

type TAnimation = class
  constructor Create( aDuration : DWord; aDelay : DWord; aUID : TUID );
  procedure OnUpdate( aTime : DWord ); virtual;
  procedure Draw;
  procedure OnStart; virtual;
  procedure OnDraw; virtual;
  function IsExpired : Boolean;
  function Started : Boolean;
protected
  FDelay    : DWord;
  FTime     : DWord;
  FDuration : DWord;
  FUID      : TUID;
  FBlocking : Boolean;
  // Limit catch-up steps at the motion's midpoint and end.
  FPreserveMotionSamples : Boolean;
public
  property Expired  : Boolean read IsExpired;
  property Duration : DWord   read FDuration;
  property Time     : DWord   read FTime;
  property UID      : TUID    read FUID;
  property Delay    : DWord   read FDelay write FDelay;
  property Blocking : Boolean read FBlocking;
end;

type TAnimationArray = specialize TGObjectArray< TAnimation >;

type TAnimations = class
  constructor Create;
  function AddAnimation( aAnimation : TAnimation ) : DWord;
  procedure Update( aTime : DWord );
  procedure Draw;
  procedure Clear;
  procedure RequestCatchUp;
  // Restore normal playback speed without changing animation progress.
  procedure ResetPlaybackSpeed;
  function Finished : Boolean;
  function BlockingFinished : Boolean;
  destructor Destroy; override;
private
  function UIDDuration( aUID : TUID ) : DWord;
  function BlockingDuration : DWord;
private
  FAnimations   : TAnimationArray;
  FPlaybackRate : Double;
  FTargetRate   : Double;
  FTimeFraction : Double;
public
  property Animations : TAnimationArray read FAnimations;
end;

implementation

uses math;

const CATCH_UP_OVERLAP_MS   = 16;
      CATCH_UP_BACKLOG_MS   = 100.0;
      CATCH_UP_MAX_RATE     = 3.0;
      CATCH_UP_RISE_MS      = 80.0;

{ TAnimation }

constructor TAnimation.Create ( aDuration : DWord; aDelay : DWord; aUID : TUID ) ;
begin
  FDelay    := aDelay;
  FTime     := 0;
  FDuration := aDuration;
  FUID      := aUID;
  FBlocking := True;
  FPreserveMotionSamples := False;
end;

procedure TAnimation.OnUpdate ( aTime : DWord ) ;
begin
  if FTime > 0
    then FTime += aTime
    else
    begin
      if FDelay >= aTime then
      begin
        FDelay -= aTime;
        Exit;
      end;
      aTime -= FDelay;
      FTime += aTime;
      FDelay := 0;
      OnStart;
    end;
end;

procedure TAnimation.OnDraw;
begin
  // noop
end;

procedure TAnimation.OnStart;
begin
  // no-op
end;

procedure TAnimation.Draw;
begin
  if FTime > 0 then OnDraw;
end;

function TAnimation.IsExpired: Boolean;
begin
  Exit( FTime > FDuration );
end;

function TAnimation.Started : Boolean;
begin
  Exit( FTime > 0 );
end;

constructor TAnimations.Create;
begin
  FAnimations := TAnimationArray.Create;
  ResetPlaybackSpeed;
end;

procedure TAnimations.ResetPlaybackSpeed;
begin
  FPlaybackRate := 1.0;
  FTargetRate   := 1.0;
  FTimeFraction := 0.0;
end;

function TAnimations.BlockingDuration : DWord;
var iAnim : TAnimation;
begin
  Result := 0;
  for iAnim in FAnimations do
    if iAnim.Blocking then
      Result := Max( Result, iAnim.FDelay + iAnim.FDuration - Min( iAnim.FTime, iAnim.FDuration ) );
end;

procedure TAnimations.RequestCatchUp;
var iRemaining : DWord;
begin
  iRemaining := BlockingDuration;
  if iRemaining <= CATCH_UP_OVERLAP_MS then Exit;
  // Keep the ramp across consecutive actions until blocking animations finish.
  FTargetRate := Max( FTargetRate, Min( CATCH_UP_MAX_RATE, 1.0 + iRemaining / CATCH_UP_BACKLOG_MS ) );
end;

function TAnimations.AddAnimation( aAnimation: TAnimation ) : DWord;
begin
  if aAnimation.FDuration = 0 then
  begin
    FreeAndNil( aAnimation );
    Exit(0);
  end;
  if aAnimation.FUID <> 0 then aAnimation.FDelay := aAnimation.FDelay + UIDDuration( aAnimation.FUID );
  FAnimations.Push( aAnimation );
  Result := aAnimation.FDelay;
end;

procedure TAnimations.Update( aTime : DWord );
var iCount   : DWord;
    iAnim    : TAnimation;
    iTime    : DWord;
    iLimit   : DWord;
    iSample  : DWord;
    iDecay   : Double;
    iAdvance : Double;
begin
  if aTime = 0 then aTime := 1;
  iTime := aTime;
  if ( FTargetRate <> 1.0 ) or ( FPlaybackRate <> 1.0 ) then
  begin
    iDecay := Exp( -Double( aTime ) / CATCH_UP_RISE_MS );
    // Integrate the rate ramp, retaining fractional milliseconds across frames.
    iAdvance := FTargetRate * aTime + ( FPlaybackRate - FTargetRate ) * CATCH_UP_RISE_MS * ( 1.0 - iDecay ) + FTimeFraction;
    FPlaybackRate := FTargetRate + ( FPlaybackRate - FTargetRate ) * iDecay;
    iTime := Trunc( iAdvance );
    FTimeFraction := iAdvance - iTime;

    iLimit := iTime;
    for iAnim in FAnimations do
      if iAnim.FPreserveMotionSamples and ( iAnim.FTime < iAnim.FDuration ) then
      begin
        iSample := iAnim.FDuration div 2;
        if iAnim.FTime >= iSample then iSample := iAnim.FDuration;
        iLimit := Min( iLimit, iAnim.FDelay + iSample - iAnim.FTime );
      end;
    // Never slow baseline playback, even after a long frame. Discard excess
    // catch-up time instead of carrying it into a later frame as a large jump.
    iTime := Max( aTime, iLimit );
  end;
  if FAnimations.Size > 0 then
  begin
    for iAnim in FAnimations do
      iAnim.OnUpdate( iTime );
    iCount := 0;
    repeat
      if FAnimations[iCount].Expired
        then FAnimations.Delete( iCount )
        else Inc( iCount );
    until iCount >= FAnimations.Size;
  end;
  if ( FPlaybackRate <> 1.0 ) and BlockingFinished then ResetPlaybackSpeed;
end;

procedure TAnimations.Draw;
var iAnim  : TAnimation;
begin
  for iAnim in FAnimations do iAnim.Draw;
end;

procedure TAnimations.Clear;
begin
  FAnimations.Clear;
  ResetPlaybackSpeed;
end;

function TAnimations.Finished: Boolean;
begin
  Exit( FAnimations.Size = 0 );
end;

function TAnimations.BlockingFinished : Boolean;
var iAnim : TAnimation;
begin
  for iAnim in FAnimations do
    if iAnim.Blocking then
      Exit( False );
  Exit( True );
end;

destructor TAnimations.Destroy;
begin
  FreeAndNil( FAnimations );
  inherited Destroy;
end;

function TAnimations.UIDDuration(aUID: TUID): DWord;
var iCount     : DWord;
    iRemaining : DWord;
begin
  if FAnimations.Size = 0 then Exit( 0 );
  UIDDuration := 0;
  for iCount := FAnimations.Size-1 downto 0 do
    with FAnimations[ iCount ] do
      if FUID = aUID then
      begin
        iRemaining := 0;
        if FDuration > FTime then iRemaining := FDuration - FTime;
        UIDDuration := Max( UIDDuration, FDelay + iRemaining );
      end;
end;


end.

