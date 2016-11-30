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
protected
  FDelay    : DWord;
  FTime     : DWord;
  FDuration : DWord;
  FUID      : TUID;
public
  property Expired  : Boolean read IsExpired;
  property Duration : DWord   read FDuration;
  property Time     : DWord   read FTime;
  property UID      : TUID    read FUID;
  property Delay    : DWord   read FDelay write FDelay;
end;

type TAnimationArray = specialize TGObjectArray< TAnimation >;

type TAnimations = class
  constructor Create;
  function AddAnimation( aAnimation : TAnimation ) : DWord;
  procedure Update( aTime : DWord );
  procedure Draw;
  procedure Clear;
  function Finished : Boolean;
  destructor Destroy; override;
private
  function UIDDuration( aUID : TUID ) : DWord;
private
  FAnimations : TAnimationArray;
end;

implementation

uses math;

{ TAnimation }

constructor TAnimation.Create ( aDuration : DWord; aDelay : DWord; aUID : TUID ) ;
begin
  FDelay    := aDelay;
  FTime     := 0;
  FDuration := aDuration;
  FUID      := aUID;
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
  Exit( FTime >= FDuration );
end;

constructor TAnimations.Create;
begin
  FAnimations := TAnimationArray.Create;
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
var iCount : DWord;
begin
  if aTime = 0 then aTime := 1;
  if FAnimations.Size > 0 then
  begin
    iCount := 0;
    repeat
      if FAnimations[iCount].Expired then
      begin
        FAnimations.Delete( iCount )
      end
      else
      begin
        FAnimations[iCount].OnUpdate( aTime );
        Inc(iCount);
      end;
    until iCount >= FAnimations.Size;
  end;
end;

procedure TAnimations.Draw;
var iAnim  : TAnimation;
begin
  for iAnim in FAnimations do iAnim.Draw;
end;

procedure TAnimations.Clear;
begin
  FAnimations.Clear;
end;

function TAnimations.Finished: Boolean;
begin
  Exit( FAnimations.Size = 0 );
end;

destructor TAnimations.Destroy;
begin
  FreeAndNil( FAnimations );
  inherited Destroy;
end;

function TAnimations.UIDDuration(aUID: TUID): DWord;
var iCount : DWord;
begin
  if FAnimations.Size = 0 then Exit( 0 );
  UIDDuration := 0;
  if FAnimations.Size > 0 then
  for iCount := FAnimations.Size-1 downto 0 do
    with FAnimations[ iCount ] do
      if FUID = aUID then
        UIDDuration := Max( UIDDuration, FDelay + ( FDuration - FTime ) );
end;


end.

