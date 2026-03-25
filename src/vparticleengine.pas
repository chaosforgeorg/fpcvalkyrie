unit vparticleengine;
{$include valkyrie.inc}
interface
uses SysUtils, Math, vvector, vcolor, vrltools, vgltypes, vspriteengine;

const PARTICLE_MAX_DEFAULT = 16384;

type
  TParticleSubID = 0..3;  // Quadrant: 0=TL, 1=TR, 2=BL, 3=BR

  TParticleFlag = (
    PF_ACTIVE, PF_DEAD, PF_NOLOOP, PF_ROTATE,
    PF_BOUNCE, PF_DECAL_ON_GROUND, PF_FADE_ALPHA, PF_COSPLAY,
    PF_RAND_OFFSET
  );
  TParticleFlags = set of TParticleFlag;

  TParticle = packed record
    Position       : TVec3f;
    Velocity       : TVec3f;
    Acceleration   : TVec3f;
    ColorStart     : TColor;
    ColorEnd       : TColor;
    SpriteID       : DWord;
    SubID          : Byte;
    AnimFrames     : Byte;
    AnimFrameTime  : Single;
    AnimTimeOffset : Single;
    Rotation       : Single;
    RotationSpeed  : Single;
    Life           : Single;
    LifeMax        : Single;
    Scale          : Single;
    DecalSprite    : DWord;
    Flags          : TParticleFlags;
    EmitterIndex   : Integer;
  end;

  TEmitterShape = (
    ES_POINT,
    ES_SPHERE,
    ES_BASE_RING,
    ES_BASE_ELLIPSE
  );

  TEmitterFlag = (
    EF_LOOPING, EF_WORLD_SPACE, EF_ATTACHED
  );
  TEmitterFlags = set of TEmitterFlag;

  PParticleEmitterData = ^TParticleEmitterData;
  TParticleEmitterData = packed record
    Flags           : TEmitterFlags;
    Shape           : TEmitterShape;
    ShapeParams     : TVec3f;
    PositionOffset  : TVec3f;
    Direction       : TVec3f;
    SpreadAngle     : Single;
    SpeedRange      : TFloatRange;
    AccelRange      : TVec3fRange;
    LifetimeRange   : TFloatRange;
    ScaleRange      : TFloatRange;
    RotationRange   : TFloatRange;
    RotSpeedRange   : TFloatRange;
    ColorStartRange : TColorRange;
    ColorEndRange   : TColorRange;
    SpriteID        : DWord;
    SubID           : Byte;
    AnimFrames      : Byte;
    AnimFrameTime   : Single;
    DecalSprite     : DWord;
    ParticleFlags   : TParticleFlags;
    Rate            : Single;
    Duration        : Single;
    RepeatDelay     : TFloatRange;
    MaxParticles    : Word;
    BurstCount      : Word;
  end;

  TParticleEmitter = packed record
    Used          : Boolean;
    Active        : Boolean;
    HasBurst      : Boolean;
    Position      : TVec3f;
    Direction     : TVec3f;
    Data          : PParticleEmitterData;
    TimeAlive     : Single;
    TimeSinceEmit : Single;
    ActiveCount   : Word;
    PoolIndex     : Integer;
  end;

  TParticleDecalCallback = procedure( const aPosition : TVec3f; aDecalSprite : DWord );

{ TParticleEngine }

  TParticleEngine = class
    constructor Create( aMaxParticles : Integer = PARTICLE_MAX_DEFAULT );
    destructor Destroy; override;
    procedure Clear;
    procedure ClearParticles;
    procedure Update( aDeltaSec : Single );
    procedure Render( aSpriteEngine : TSpriteEngine );
    function  EmitStart( aData : PParticleEmitterData; const aPos : TVec3f ) : Integer;
    procedure EmitStop( aIndex : Integer );
    procedure EmitKill( aIndex : Integer );
    procedure EmitSetPosition( aIndex : Integer; const aPos : TVec3f );
    procedure EmitSetDirection( aIndex : Integer; const aDir : TVec3f );
    procedure EmitSetSpriteRotation( aIndex : Integer; aDegrees : Single );
    function  IsEmitterUsed( aIndex : Integer ) : Boolean;
    function  SpawnParticle( const aParticle : TParticle ) : Integer;
    procedure SpawnBurst( aData : PParticleEmitterData; const aPos, aDir : TVec3f; aCount : Word );
  private
    FParticles     : array of TParticle;
    FEmitters      : array of TParticleEmitter;
    FParticleCount : Integer;
    FEmitterCount  : Integer;
    FMaxParticles  : Integer;
    FMaxEmitters   : Integer;
    FDecalCallback : TParticleDecalCallback;
    function  AllocParticle : Integer;
    function  AllocEmitter : Integer;
    procedure UpdateParticle( aIndex : Integer; aDeltaSec : Single );
    procedure EmitFromEmitter( aIndex : Integer; aDeltaSec : Single );
    procedure SpawnFromEmitter( aIndex : Integer );
    procedure KillParticle( aIndex : Integer );
    procedure RenderParticle( aIndex : Integer; aSpriteEngine : TSpriteEngine );
    function  RandomDirection( const aDir : TVec3f; aSpreadAngle : Single ) : TVec3f;
    function  RandomPointInShape( const aPos : TVec3f; aData : PParticleEmitterData ) : TVec3f;
  public
    property DecalCallback : TParticleDecalCallback read FDecalCallback write FDecalCallback;
    property ParticleCount : Integer read FParticleCount;
    property EmitterCount : Integer read FEmitterCount;
  end;

implementation

{ TParticleEngine }

constructor TParticleEngine.Create( aMaxParticles : Integer );
begin
  inherited Create;
  FDecalCallback := nil;
  FMaxParticles  := aMaxParticles;
  FMaxEmitters   := 128;
  SetLength( FParticles, FMaxParticles );
  SetLength( FEmitters, FMaxEmitters );
  Clear;
end;

destructor TParticleEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TParticleEngine.Clear;
var i : Integer;
begin
  FParticleCount := 0;
  FEmitterCount  := 0;
  for i := 0 to FMaxEmitters - 1 do
    FEmitters[i].Used := False;
end;

procedure TParticleEngine.ClearParticles;
var i : Integer;
begin
  FParticleCount := 0;
  for i := 0 to FMaxEmitters - 1 do
    if FEmitters[i].Used then
    begin
      FEmitters[i].ActiveCount  := 0;
      FEmitters[i].TimeSinceEmit := 0;
    end;
end;

function TParticleEngine.AllocParticle : Integer;
begin
  if FParticleCount >= FMaxParticles then Exit( -1 );
  Result := FParticleCount;
  Inc( FParticleCount );
end;

function TParticleEngine.AllocEmitter : Integer;
var i, iOldMax : Integer;
begin
  for i := 0 to FMaxEmitters - 1 do
    if not FEmitters[i].Used then
    begin
      Inc( FEmitterCount );
      Exit( i );
    end;
  // Grow emitter array — indexes stay valid
  iOldMax := FMaxEmitters;
  FMaxEmitters := FMaxEmitters * 2;
  SetLength( FEmitters, FMaxEmitters );
  for i := iOldMax to FMaxEmitters - 1 do
    FEmitters[i].Used := False;
  Inc( FEmitterCount );
  Result := iOldMax;
end;

function TParticleEngine.RandomDirection( const aDir : TVec3f; aSpreadAngle : Single ) : TVec3f;
var iAngle : Double;
begin
  if aSpreadAngle <= 0 then
    Exit( aDir );
  if ( Abs( aDir.X ) < 0.001 ) and ( Abs( aDir.Y ) < 0.001 ) then
    iAngle := Random * 2.0 * PI
  else
    iAngle := ArcTan2( aDir.Y, aDir.X ) + ( Random * 2.0 - 1.0 ) * aSpreadAngle * PI / 180.0;
  Result.Init( Cos( iAngle ), Sin( iAngle ), 0 );
end;

function TParticleEngine.RandomPointInShape( const aPos : TVec3f; aData : PParticleEmitterData ) : TVec3f;
var iAngle, iRadius : Single;
    iBase : TVec3f;
begin
  iBase := aPos + aData^.PositionOffset;
  case aData^.Shape of
    ES_POINT:
      Result := iBase;
    ES_SPHERE:
    begin
      iAngle := Random * 2 * PI;
      iRadius := aData^.ShapeParams.X;
      Result.X := iBase.X + Cos( iAngle ) * iRadius * ( 0.5 + Random * 0.5 );
      Result.Y := iBase.Y + Sin( iAngle ) * iRadius * ( 0.5 + Random * 0.5 );
      Result.Z := iBase.Z + ( Random - 0.5 ) * iRadius;
    end;
    ES_BASE_RING:
    begin
      iAngle := Random * 2 * PI;
      Result.X := iBase.X + Cos( iAngle ) * aData^.ShapeParams.X;
      Result.Y := iBase.Y + Sin( iAngle ) * aData^.ShapeParams.Y;
      // Z varies for depth: bottom of ring is positive (behind), top is negative (in front)
      Result.Z := Sin( iAngle ) * aData^.ShapeParams.Z;
    end;
    ES_BASE_ELLIPSE:
    begin
      iAngle := Random * 2 * PI;
      iRadius := Sqrt( Random );  // uniform disc distribution
      Result.X := iBase.X + Cos( iAngle ) * aData^.ShapeParams.X * iRadius;
      Result.Y := iBase.Y + Sin( iAngle ) * aData^.ShapeParams.Y * iRadius;
      Result.Z := Sin( iAngle ) * aData^.ShapeParams.Z * iRadius;
    end;
  end;
end;

procedure TParticleEngine.SpawnFromEmitter( aIndex : Integer );
var iP    : Integer;
    iE    : ^TParticleEmitter;
    iD    : PParticleEmitterData;
    iSpeed: Single;
    iDir  : TVec3f;
begin
  iE := @FEmitters[aIndex];
  iD := iE^.Data;
  if iE^.ActiveCount >= iD^.MaxParticles then Exit;

  iP := AllocParticle;
  if iP < 0 then Exit;

  with FParticles[iP] do
  begin
    Position      := RandomPointInShape( iE^.Position, iD );
    iSpeed        := iD^.SpeedRange.Random;
    iDir          := RandomDirection( iE^.Direction, iD^.SpreadAngle );
    Velocity.X    := iDir.X * iSpeed;
    Velocity.Y    := iDir.Y * iSpeed;
    Velocity.Z    := iDir.Z * iSpeed;
    Acceleration  := iD^.AccelRange.Random;
    Life          := iD^.LifetimeRange.Random;
    LifeMax       := Life;
    Scale         := iD^.ScaleRange.Random;
    Rotation      := iD^.RotationRange.Random;
    RotationSpeed := iD^.RotSpeedRange.Random;
    ColorStart    := iD^.ColorStartRange.Random;
    ColorEnd      := iD^.ColorEndRange.Random;
    SpriteID      := iD^.SpriteID;
    SubID         := iD^.SubID;
    AnimFrames    := iD^.AnimFrames;
    AnimFrameTime := iD^.AnimFrameTime;
    if ( AnimFrames > 1 ) and ( PF_RAND_OFFSET in iD^.ParticleFlags ) then
      AnimTimeOffset := Random * AnimFrameTime * AnimFrames
    else
      AnimTimeOffset := 0;
    DecalSprite   := iD^.DecalSprite;
    Flags         := iD^.ParticleFlags + [PF_ACTIVE];
    EmitterIndex  := aIndex;
  end;

  Inc( iE^.ActiveCount );
end;

procedure TParticleEngine.EmitFromEmitter( aIndex : Integer; aDeltaSec : Single );
var iE         : ^TParticleEmitter;
    iD         : PParticleEmitterData;
    iToSpawn   : Integer;
    i          : Integer;
begin
  iE := @FEmitters[aIndex];
  if not iE^.Used then Exit;
  iD := iE^.Data;

  // Inactive emitters just wait for particles to die
  if not iE^.Active then
  begin
    if iE^.ActiveCount = 0 then
    begin
      iE^.Used := False;
      Dec( FEmitterCount );
    end;
    Exit;
  end;

  iE^.TimeAlive += aDeltaSec;

  // Check duration
  if ( iD^.Duration > 0 ) and ( iE^.TimeAlive >= iD^.Duration ) then
  begin
    if EF_LOOPING in iD^.Flags then
    begin
      iE^.TimeAlive := 0;
      iE^.HasBurst  := False;
    end
    else
    begin
      iE^.Active := False;
      Exit;
    end;
  end;

  if iD^.BurstCount > 0 then
  begin
    if not iE^.HasBurst then
    begin
      iE^.HasBurst := True;
      for i := 1 to iD^.BurstCount do
        SpawnFromEmitter( aIndex );
      // Non-looping burst: deactivate immediately, auto-drain frees slot
      if not ( EF_LOOPING in iD^.Flags ) then
        iE^.Active := False;
    end;
    Exit;
  end;

  // Continuous mode
  iE^.TimeSinceEmit += aDeltaSec;
  if iD^.Rate > 0 then
  begin
    iToSpawn := Floor( iE^.TimeSinceEmit * iD^.Rate );
    if iToSpawn > 0 then
    begin
      iE^.TimeSinceEmit -= iToSpawn / iD^.Rate;
      for i := 1 to iToSpawn do
        SpawnFromEmitter( aIndex );
    end;
  end;
end;

procedure TParticleEngine.KillParticle( aIndex : Integer );
var iEI : Integer;
begin
  iEI := FParticles[aIndex].EmitterIndex;
  if ( iEI >= 0 ) and ( iEI < FMaxEmitters ) then
    if FEmitters[iEI].Used then
    begin
      if FEmitters[iEI].ActiveCount > 0 then
        Dec( FEmitters[iEI].ActiveCount );
      // Auto-free inactive emitters when last particle dies
      if ( not FEmitters[iEI].Active ) and ( FEmitters[iEI].ActiveCount = 0 ) then
      begin
        FEmitters[iEI].Used := False;
        Dec( FEmitterCount );
      end;
    end;
  Dec( FParticleCount );
  if aIndex < FParticleCount then
    FParticles[aIndex] := FParticles[FParticleCount];
end;

procedure TParticleEngine.UpdateParticle( aIndex : Integer; aDeltaSec : Single );
var iP     : ^TParticle;
    iFrame : Integer;
begin
  iP := @FParticles[aIndex];

  iP^.Life -= aDeltaSec;
  if iP^.Life <= 0 then
  begin
    Include( iP^.Flags, PF_DEAD );
    Exit;
  end;

  // Euler integration
  iP^.Velocity := iP^.Velocity + iP^.Acceleration.Scaled( aDeltaSec );
  iP^.Position := iP^.Position + iP^.Velocity.Scaled( aDeltaSec );

  // Ground collision (Z <= 0 means ground hit)
  if iP^.Position.Z < 0 then
  begin
    if PF_BOUNCE in iP^.Flags then
    begin
      iP^.Position.Z := -iP^.Position.Z * 0.3;
      iP^.Velocity.Z := -iP^.Velocity.Z * 0.3;
    end
    else
    begin
      iP^.Position.Z := 0;
      if ( PF_DECAL_ON_GROUND in iP^.Flags ) and ( iP^.DecalSprite <> 0 ) then
        if Assigned( FDecalCallback ) then
          FDecalCallback( iP^.Position, iP^.DecalSprite );
      Include( iP^.Flags, PF_DEAD );
      Exit;
    end;
  end;

  // Rotation
  if PF_ROTATE in iP^.Flags then
    iP^.Rotation += iP^.RotationSpeed * aDeltaSec;

  // Animation frame advance
  if iP^.AnimFrames > 1 then
  begin
    iFrame := Floor( ( iP^.LifeMax - iP^.Life + iP^.AnimTimeOffset ) / Max( iP^.AnimFrameTime, 0.001 ) );
    if PF_NOLOOP in iP^.Flags then
      iP^.SubID := Min( Byte( iFrame ), iP^.AnimFrames - 1 )
    else
      iP^.SubID := Byte( iFrame ) mod iP^.AnimFrames;
  end;
end;

procedure TParticleEngine.Update( aDeltaSec : Single );
var i : Integer;
begin
  // Update emitters
  for i := 0 to FMaxEmitters - 1 do
    if FEmitters[i].Used then
      EmitFromEmitter( i, aDeltaSec );

  // Forward update pass: mark dead particles
  for i := 0 to FParticleCount - 1 do
    UpdateParticle( i, aDeltaSec );

  // Forward sweep: remove dead particles (swap-with-last, don't advance on kill)
  i := 0;
  while i < FParticleCount do
  begin
    if PF_DEAD in FParticles[i].Flags then
      KillParticle( i )
    else
      Inc( i );
  end;
end;

procedure TParticleEngine.RenderParticle( aIndex : Integer; aSpriteEngine : TSpriteEngine );
var iP         : ^TParticle;
    iT         : Single;
    iColor     : TColor;
    iLayerIdx  : DWord;
    iSpriteID  : DWord;
    iLayer     : TSpriteDataSet;
    iSubCol    : Integer;
    iSubRow    : Integer;
    iTexPos    : TVec2f;
    iTexA      : TVec2f;
    iTexB      : TVec2f;
    iScreenPos : TVec2i;
    iHalf      : Integer;
    iZ         : Integer;
    iCosColor  : TColor;
    iCoord     : TGLRawQCoord;
    iTex       : TGLRawQTexCoord;
    iSin, iCos : Single;
    function Rotated( pX, pY : Single ) : TVec2i;
    begin
      Rotated.X := Round( pX * iCos - pY * iSin + iScreenPos.X );
      Rotated.Y := Round( pY * iCos + pX * iSin + iScreenPos.Y );
    end;
begin
  iP := @FParticles[aIndex];

  // Lifetime ratio for lerp
  if iP^.LifeMax > 0 then
    iT := 1.0 - ( iP^.Life / iP^.LifeMax )
  else
    iT := 0;

  // Interpolate color
  iColor := ColorLerp( iP^.ColorStart, iP^.ColorEnd, iT );

  // Alpha fade
  if PF_FADE_ALPHA in iP^.Flags then
    iColor.A := Round( iColor.A * ( iP^.Life / iP^.LifeMax ) );

  // Resolve sprite layer
  iLayerIdx := iP^.SpriteID div 100000;
  iSpriteID := iP^.SpriteID mod 100000;
  if Integer(iLayerIdx) >= aSpriteEngine.Layers.Size then Exit;
  iLayer := aSpriteEngine.Layers[iLayerIdx];

  // Sub-sprite tex coords: half-sprite within the full sprite cell
  iSubCol := iP^.SubID mod 2;
  iSubRow := iP^.SubID div 2;
  iTexPos := TVec2f.CreateModDiv( iSpriteID - 1, iLayer.RowSize );
  iTexA.X := ( iTexPos.X + iSubCol * 0.5 ) * iLayer.TexUnit.X;
  iTexA.Y := ( iTexPos.Y + iSubRow * 0.5 ) * iLayer.TexUnit.Y;
  iTexB.X := ( iTexPos.X + ( iSubCol + 1 ) * 0.5 ) * iLayer.TexUnit.X;
  iTexB.Y := ( iTexPos.Y + ( iSubRow + 1 ) * 0.5 ) * iLayer.TexUnit.Y;

  // Screen position from base coords (Z subtracts from Y for fake-3D rise)
  iScreenPos.X := Round( iP^.Position.X * aSpriteEngine.Scale );
  iScreenPos.Y := Round( ( iP^.Position.Y - iP^.Position.Z ) * aSpriteEngine.Scale );
  iHalf := Round( aSpriteEngine.Grid.X * iP^.Scale * 0.25 );

  // Z-order: Y-row sorting + depth from Z
  iZ := Round( iP^.Position.Y / 32 ) * 10 + 4000 + Round( iP^.Position.Z * 0.1 );

  // Color channels: cosplay sprites use additive cos_color, others use vertex color tinting
  if PF_COSPLAY in iP^.Flags then
  begin
    iCosColor := iColor;
    iColor    := NewColor( 255, 255, 255, iColor.A );
  end
  else
    iCosColor := ColorZero;

  // Build quad via raw Push API
  iTex.Init( iTexA, iTexB );

  if PF_ROTATE in iP^.Flags then
  begin
    Math.SinCos( iP^.Rotation * PI / 180, iSin, iCos );
    iCoord.Data[0] := Rotated( -iHalf, -iHalf );
    iCoord.Data[1] := Rotated( -iHalf, +iHalf );
    iCoord.Data[2] := Rotated( +iHalf, +iHalf );
    iCoord.Data[3] := Rotated( +iHalf, -iHalf );
  end
  else
  begin
    iCoord.Data[0].Init( iScreenPos.X - iHalf, iScreenPos.Y - iHalf );
    iCoord.Data[1].Init( iScreenPos.X - iHalf, iScreenPos.Y + iHalf );
    iCoord.Data[2].Init( iScreenPos.X + iHalf, iScreenPos.Y + iHalf );
    iCoord.Data[3].Init( iScreenPos.X + iHalf, iScreenPos.Y - iHalf );
  end;

  iLayer.Push( @iCoord, @iTex, iColor, iCosColor, ColorZero, iCosColor, iZ );
end;

procedure TParticleEngine.Render( aSpriteEngine : TSpriteEngine );
var i : Integer;
begin
  for i := 0 to FParticleCount - 1 do
    RenderParticle( i, aSpriteEngine );
end;

function TParticleEngine.EmitStart( aData : PParticleEmitterData; const aPos : TVec3f ) : Integer;
var iIdx : Integer;
begin
  iIdx := AllocEmitter;
  if iIdx < 0 then Exit( -1 );
  FEmitters[iIdx].Used          := True;
  FEmitters[iIdx].Active        := True;
  FEmitters[iIdx].Position      := aPos;
  FEmitters[iIdx].Direction     := aData^.Direction;
  FEmitters[iIdx].Data          := aData;
  FEmitters[iIdx].TimeAlive     := 0;
  FEmitters[iIdx].TimeSinceEmit := 0;
  FEmitters[iIdx].HasBurst      := False;
  FEmitters[iIdx].ActiveCount   := 0;
  FEmitters[iIdx].PoolIndex     := iIdx;
  Result := iIdx;
end;

procedure TParticleEngine.EmitStop( aIndex : Integer );
begin
  if ( aIndex < 0 ) or ( aIndex >= FMaxEmitters ) then Exit;
  // Stop spawning but let existing particles live
  FEmitters[aIndex].Active := False;
end;

procedure TParticleEngine.EmitKill( aIndex : Integer );
var i : Integer;
begin
  if ( aIndex < 0 ) or ( aIndex >= FMaxEmitters ) then Exit;
  // Kill all particles belonging to this emitter
  i := 0;
  while i < FParticleCount do
  begin
    if FParticles[i].EmitterIndex = aIndex then
      KillParticle( i )
    else
      Inc( i );
  end;
  FEmitters[aIndex].Used := False;
  FEmitters[aIndex].Active := False;
  FEmitters[aIndex].ActiveCount := 0;
  if FEmitterCount > 0 then Dec( FEmitterCount );
end;

procedure TParticleEngine.EmitSetPosition( aIndex : Integer; const aPos : TVec3f );
begin
  if ( aIndex < 0 ) or ( aIndex >= FMaxEmitters ) then Exit;
  FEmitters[aIndex].Position := aPos;
end;

procedure TParticleEngine.EmitSetDirection( aIndex : Integer; const aDir : TVec3f );
begin
  if ( aIndex < 0 ) or ( aIndex >= FMaxEmitters ) then Exit;
  FEmitters[aIndex].Direction := aDir;
end;

procedure TParticleEngine.EmitSetSpriteRotation( aIndex : Integer; aDegrees : Single );
var i : Integer;
begin
  if ( aIndex < 0 ) or ( aIndex >= FMaxEmitters ) then Exit;
  for i := 0 to FParticleCount - 1 do
    if FParticles[i].EmitterIndex = aIndex then
      FParticles[i].Rotation := aDegrees;
end;

function TParticleEngine.IsEmitterUsed( aIndex : Integer ) : Boolean;
begin
  if ( aIndex < 0 ) or ( aIndex >= FMaxEmitters ) then Exit( False );
  Result := FEmitters[aIndex].Used;
end;

function TParticleEngine.SpawnParticle( const aParticle : TParticle ) : Integer;
var iIdx : Integer;
begin
  iIdx := AllocParticle;
  if iIdx < 0 then Exit( -1 );
  FParticles[iIdx] := aParticle;
  Include( FParticles[iIdx].Flags, PF_ACTIVE );
  FParticles[iIdx].EmitterIndex := -1;
  Result := iIdx;
end;

procedure TParticleEngine.SpawnBurst( aData : PParticleEmitterData; const aPos, aDir : TVec3f; aCount : Word );
var i    : Integer;
    iIdx : Integer;
    iP   : ^TParticle;
    iSpeed : Single;
    iDir   : TVec3f;
begin
  for i := 1 to aCount do
  begin
    iIdx := AllocParticle;
    if iIdx < 0 then Exit;
    iP := @FParticles[iIdx];
    iP^.Position      := RandomPointInShape( aPos, aData );
    iSpeed            := aData^.SpeedRange.Random;
    iDir              := RandomDirection( aDir, aData^.SpreadAngle );
    iP^.Velocity.X    := iDir.X * iSpeed;
    iP^.Velocity.Y    := iDir.Y * iSpeed;
    iP^.Velocity.Z    := iDir.Z * iSpeed;
    iP^.Acceleration  := aData^.AccelRange.Random;
    iP^.Life          := aData^.LifetimeRange.Random;
    iP^.LifeMax       := iP^.Life;
    iP^.Scale         := aData^.ScaleRange.Random;
    iP^.Rotation      := aData^.RotationRange.Random;
    iP^.RotationSpeed := aData^.RotSpeedRange.Random;
    iP^.ColorStart    := aData^.ColorStartRange.Random;
    iP^.ColorEnd      := aData^.ColorEndRange.Random;
    iP^.SpriteID      := aData^.SpriteID;
    iP^.SubID         := aData^.SubID;
    iP^.AnimFrames    := aData^.AnimFrames;
    iP^.AnimFrameTime := aData^.AnimFrameTime;
    if ( aData^.AnimFrames > 1 ) and ( PF_RAND_OFFSET in aData^.ParticleFlags ) then
      iP^.AnimTimeOffset := Random * aData^.AnimFrameTime * aData^.AnimFrames
    else
      iP^.AnimTimeOffset := 0;
    iP^.DecalSprite   := aData^.DecalSprite;
    iP^.Flags         := aData^.ParticleFlags + [PF_ACTIVE];
    iP^.EmitterIndex  := -1;
  end;
end;

end.
