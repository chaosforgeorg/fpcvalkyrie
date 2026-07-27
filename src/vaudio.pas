{$INCLUDE valkyrie.inc}
// @abstract(Sound system for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
//
// Backend-neutral 2D audio service
//
// Design restrictions
// - only 2d sound (positional)
// - no movement effects
// - one listener only
// - one active music track only

unit vaudio;

interface

uses Classes, SysUtils, vsystem, vvector;

type
  EAudioException = class(Exception);

  TAudioAssetHandle    = Integer;
  TAudioInstanceHandle = Integer;

  TAudioUsage    = (auSound, auMusic);
  TAudioLoadMode = (almDefault, almPreload, almStream);

  TAudio = class(TSystem)
  private type
    TAsset = record
      Data   : Pointer;
      Loaded : Boolean;
      Stream : Boolean;
    end;
    TInstance = record
      Data       : Pointer;
      Asset      : TAudioAssetHandle;
      Playing    : Boolean;
      Music      : Boolean;
      Ambient    : Boolean;
      Loop       : Boolean;
      Position   : TVec2f;
      Radius     : Single;
      Volume     : Integer;
      FadeFrom   : Single;
      FadeTo     : Single;
      FadeTotal  : DWord;
      FadePassed : DWord;
      StopOnFade : Boolean;
    end;
  private
    FAssets       : array of TAsset;
    FInstances    : array of TInstance;
    FListener     : TVec2f;
    FMusicVolume  : Integer;
    FSoundVolume  : Integer;
    FMusic        : TAudioInstanceHandle;

    function CheckAsset( aAsset : TAudioAssetHandle ) : Integer;
    function CheckInstance( aInstance : TAudioInstanceHandle ) : Integer;
    function AllocateInstance : Integer;
    function CategoryGain( aMusic : Boolean ) : Single;
    function InstanceGain( aIndex : Integer ) : Single;
    procedure ApplyGain( aIndex : Integer );
    procedure ClearInstance( aIndex : Integer );
  protected
    function LoadBackend( const aFileName : AnsiString; aStream : Boolean ) : Pointer; virtual; abstract;
    function LoadBackendStream( aStream : TStream; aSize : DWord;
      const aNameHint : AnsiString; aStreamed : Boolean ) : Pointer; virtual; abstract;
    procedure FreeBackendAsset( aData : Pointer ); virtual; abstract;
    function StartBackend( aAssetData : Pointer; aStream, aLoop : Boolean;
      aMusic : Boolean ) : Pointer; virtual; abstract;
    procedure StopBackend( aInstanceData : Pointer ); virtual; abstract;
    function BackendPlaying( aInstanceData : Pointer ) : Boolean; virtual; abstract;
    procedure SetBackendGain( aInstanceData : Pointer; aGain : Single ); virtual; abstract;
    procedure BackendUpdate; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Load( const aFileName : AnsiString; aUsage : TAudioUsage;
      aMode : TAudioLoadMode = almDefault ) : TAudioAssetHandle; overload;
    function Load( aStream : TStream; aSize : DWord; const aNameHint : AnsiString;
      aUsage : TAudioUsage; aMode : TAudioLoadMode = almDefault ) : TAudioAssetHandle; overload;
    procedure Unload( aAsset : TAudioAssetHandle );
    function IsLoaded( aAsset : TAudioAssetHandle ) : Boolean;

    function Play( aAsset : TAudioAssetHandle; aVolumePercent : Integer = 100;
      aLoop : Boolean = False; aFadeInMS : DWord = 0 ) : TAudioInstanceHandle;
    function PlayMusic( aAsset : TAudioAssetHandle; aLoop : Boolean = True;
      aFadeInMS : DWord = 0 ) : TAudioInstanceHandle;
    procedure Stop( aInstance : TAudioInstanceHandle; aFadeOutMS : DWord = 0 );
    procedure StopMusic( aFadeOutMS : DWord = 0 );
    procedure StopAllSounds( aFadeOutMS : DWord = 0 );
    function IsPlaying( aInstance : TAudioInstanceHandle ) : Boolean;
    procedure SetInstanceVolume( aInstance : TAudioInstanceHandle; aPercent : Integer );

    procedure SetListenerPosition( const aPosition : TVec2f );
    function StartAmbient( aAsset : TAudioAssetHandle; const aPosition : TVec2f;
      aRadius : Single; aVolumePercent : Integer = 100;
      aFadeInMS : DWord = 0 ) : TAudioInstanceHandle;
    procedure SetAmbientPosition( aInstance : TAudioInstanceHandle;
      const aPosition : TVec2f );
    procedure SetAmbientRadius( aInstance : TAudioInstanceHandle; aRadius : Single );
    procedure StopAmbient( aInstance : TAudioInstanceHandle; aFadeOutMS : DWord = 0 );

    procedure SetMusicVolumePercent( aPercent : Integer );
    function GetMusicVolumePercent : Integer;
    procedure SetSoundVolumePercent( aPercent : Integer );
    function GetSoundVolumePercent : Integer;
    procedure Update( aElapsedMS : DWord ); virtual;
    procedure Reset; virtual;
  end;

const Audio : TAudio = nil;

implementation

function ClampPercent( aPercent : Integer ) : Integer; inline;
begin
  if aPercent < 0 then Exit( 0 );
  if aPercent > 100 then Exit( 100 );
  Result := aPercent;
end;

constructor TAudio.Create;
begin
  inherited Create;
  FListener := Vec2f;
  FMusicVolume := 100;
  FSoundVolume := 100;
  FMusic := 0;
end;

destructor TAudio.Destroy;
begin
  Reset;
  inherited Destroy;
end;

function TAudio.CheckAsset( aAsset : TAudioAssetHandle ) : Integer;
begin
  Result := aAsset - 1;
  if ( Result < 0 ) or ( Result >= Length(FAssets) ) or not FAssets[Result].Loaded then
    raise EAudioException.Create( 'Invalid audio asset handle '+IntToStr(aAsset) );
end;

function TAudio.CheckInstance( aInstance : TAudioInstanceHandle ) : Integer;
begin
  Result := aInstance - 1;
  if ( Result < 0 ) or ( Result >= Length(FInstances) ) or not FInstances[Result].Playing then
    raise EAudioException.Create( 'Invalid audio instance handle '+IntToStr(aInstance) );
end;

function TAudio.AllocateInstance : Integer;
begin
  for Result := 0 to High(FInstances) do
    if not FInstances[Result].Playing then Exit;
  Result := Length(FInstances);
  SetLength( FInstances, Result + 1 );
end;

function TAudio.CategoryGain( aMusic : Boolean ) : Single;
var iPercent : Single;
begin
  if aMusic 
    then iPercent := FMusicVolume
    else iPercent := FSoundVolume;
  iPercent := iPercent / 100.0;
  Result := iPercent * iPercent;
end;

function TAudio.InstanceGain( aIndex : Integer ) : Single;
var iInstance : ^TInstance;
    iDistance : Single;
begin
  iInstance := @FInstances[aIndex];
  Result := CategoryGain( iInstance^.Music ) * Sqr( iInstance^.Volume / 100.0 );
  if iInstance^.Ambient then
  begin
    if iInstance^.Radius <= 0.0 then Exit( 0.0 );
    iDistance := iInstance^.Position.Distance( FListener );
    Result := Result * ( 1.0 - iDistance / iInstance^.Radius );
    if Result < 0.0 then Result := 0.0;
  end;
  if iInstance^.FadeTotal > 0 then
    Result := Result * ( iInstance^.FadeFrom +
      ( iInstance^.FadeTo - iInstance^.FadeFrom ) *
      ( iInstance^.FadePassed / iInstance^.FadeTotal ) );
end;

procedure TAudio.ApplyGain( aIndex : Integer );
begin
  SetBackendGain( FInstances[aIndex].Data, InstanceGain(aIndex) );
end;

procedure TAudio.ClearInstance( aIndex : Integer );
begin
  if FMusic = aIndex + 1 then FMusic := 0;
  FillChar( FInstances[aIndex], SizeOf(TInstance), 0 );
end;

function TAudio.Load( const aFileName : AnsiString; aUsage : TAudioUsage;
  aMode : TAudioLoadMode ) : TAudioAssetHandle;
var iStream : Boolean;
begin
  iStream := ( aMode = almStream ) or ((aMode = almDefault) and (aUsage = auMusic));
  Result := Length(FAssets) + 1;
  SetLength( FAssets, Result );
  FAssets[Result-1].Data := LoadBackend( aFileName, iStream );
  if FAssets[Result-1].Data = nil then
  begin
    SetLength( FAssets, Result-1 );
    raise EAudioException.Create( 'Unable to load audio "'+aFileName+'"' );
  end;
  FAssets[Result-1].Loaded := True;
  FAssets[Result-1].Stream := iStream;
end;

function TAudio.Load( aStream : TStream; aSize : DWord; const aNameHint : AnsiString;
  aUsage : TAudioUsage; aMode : TAudioLoadMode ) : TAudioAssetHandle;
var iStream : Boolean;
begin
  iStream := ( aMode = almStream ) or ((aMode = almDefault) and (aUsage = auMusic));
  Result := Length(FAssets) + 1;
  SetLength( FAssets, Result );
  FAssets[Result-1].Data := LoadBackendStream( aStream, aSize, aNameHint, iStream );
  if FAssets[Result-1].Data = nil then
  begin
    SetLength( FAssets, Result-1 );
    raise EAudioException.Create( 'Unable to load audio stream "'+aNameHint+'"' );
  end;
  FAssets[Result-1].Loaded := True;
  FAssets[Result-1].Stream := iStream;
end;

procedure TAudio.Unload( aAsset : TAudioAssetHandle );
var iIndex, i : Integer;
begin
  iIndex := CheckAsset( aAsset );
  for i := 0 to High(FInstances) do
    if FInstances[i].Playing and (FInstances[i].Asset = aAsset) then Stop( i + 1 );
  FreeBackendAsset( FAssets[iIndex].Data );
  FillChar( FAssets[iIndex], SizeOf(TAsset), 0 );
end;

function TAudio.IsLoaded( aAsset : TAudioAssetHandle ) : Boolean;
var iIndex : Integer;
begin
  iIndex := aAsset - 1;
  Result := (iIndex >= 0) and (iIndex < Length(FAssets)) and FAssets[iIndex].Loaded;
end;

function TAudio.Play( aAsset : TAudioAssetHandle; aVolumePercent : Integer;
  aLoop : Boolean; aFadeInMS : DWord ) : TAudioInstanceHandle;
var iAsset, iInstance : Integer;
begin
  iAsset := CheckAsset( aAsset );
  iInstance := AllocateInstance;
  FillChar( FInstances[iInstance], SizeOf(TInstance), 0 );
  with FInstances[iInstance] do
  begin
    Asset := aAsset;
    Playing := True;
    Loop := aLoop;
    Volume := ClampPercent( aVolumePercent );
    Data := StartBackend( FAssets[iAsset].Data, FAssets[iAsset].Stream, aLoop, False );
    if Data = nil then
    begin
      ClearInstance( iInstance );
      Exit( 0 );
    end;
    FadeTo := 1.0;
    if aFadeInMS > 0 then
    begin
      FadeTotal := aFadeInMS;
      FadeFrom := 0.0;
    end
    else
      FadeFrom := 1.0;
  end;
  ApplyGain( iInstance );
  Result := iInstance + 1;
end;

function TAudio.PlayMusic( aAsset : TAudioAssetHandle; aLoop : Boolean;
  aFadeInMS : DWord ) : TAudioInstanceHandle;
var iAsset, iInstance : Integer;
begin
  if FMusic <> 0 then Stop( FMusic );
  iAsset := CheckAsset( aAsset );
  iInstance := AllocateInstance;
  FillChar( FInstances[iInstance], SizeOf(TInstance), 0 );
  with FInstances[iInstance] do
  begin
    Asset   := aAsset;
    Playing := True;
    Music   := True;
    Loop    := aLoop;
    Volume  := 100;
    Data    := StartBackend( FAssets[iAsset].Data, FAssets[iAsset].Stream, aLoop, True );
    if Data = nil then
    begin
      ClearInstance( iInstance );
      Exit( 0 );
    end;
    FadeTo := 1.0;
    if aFadeInMS > 0 then
    begin
      FadeTotal := aFadeInMS;
      FadeFrom := 0.0;
    end
    else
      FadeFrom := 1.0;
  end;
  FMusic := iInstance + 1;
  ApplyGain( iInstance );
  Result := FMusic;
end;

procedure TAudio.Stop( aInstance : TAudioInstanceHandle; aFadeOutMS : DWord );
var iIndex : Integer;
begin
  iIndex := CheckInstance( aInstance );
  if aFadeOutMS = 0 then
  begin
    StopBackend( FInstances[iIndex].Data );
    ClearInstance( iIndex );
  end
  else with FInstances[iIndex] do
  begin
    FadeFrom   := 1.0;
    FadeTo     := 0.0;
    FadeTotal  := aFadeOutMS;
    FadePassed := 0;
    StopOnFade := True;
  end;
end;

procedure TAudio.StopMusic( aFadeOutMS : DWord );
begin
  if FMusic <> 0 then Stop( FMusic, aFadeOutMS );
end;

procedure TAudio.StopAllSounds( aFadeOutMS : DWord );
var i : Integer;
begin
  for i := 0 to High(FInstances) do
    if FInstances[i].Playing and not FInstances[i].Music then
      Stop( i+1, aFadeOutMS );
end;

function TAudio.IsPlaying( aInstance : TAudioInstanceHandle ) : Boolean;
var iIndex : Integer;
begin
  iIndex := aInstance - 1;
  Result := (iIndex >= 0) and (iIndex < Length(FInstances)) and FInstances[iIndex].Playing;
end;

procedure TAudio.SetInstanceVolume( aInstance : TAudioInstanceHandle; aPercent : Integer );
var iIndex : Integer;
begin
  iIndex := CheckInstance( aInstance );
  FInstances[iIndex].Volume := ClampPercent(aPercent);
  ApplyGain( iIndex );
end;

procedure TAudio.SetListenerPosition( const aPosition : TVec2f );
var i : Integer;
begin
  FListener := aPosition;
  for i := 0 to High(FInstances) do
    if FInstances[i].Playing and FInstances[i].Ambient then ApplyGain(i);
end;

function TAudio.StartAmbient( aAsset : TAudioAssetHandle; const aPosition : TVec2f;
  aRadius : Single; aVolumePercent : Integer; aFadeInMS : DWord ) : TAudioInstanceHandle;
var iIndex : Integer;
begin
  if aRadius <= 0.0 then raise EAudioException.Create('Ambient radius must be positive');
  Result := Play( aAsset, aVolumePercent, True, aFadeInMS );
  if Result = 0 then Exit;
  iIndex := Result - 1;
  FInstances[iIndex].Ambient  := True;
  FInstances[iIndex].Position := aPosition;
  FInstances[iIndex].Radius   := aRadius;
  ApplyGain( iIndex );
end;

procedure TAudio.SetAmbientPosition( aInstance : TAudioInstanceHandle; const aPosition : TVec2f );
var iIndex : Integer;
begin
  iIndex := CheckInstance(aInstance);
  if not FInstances[iIndex].Ambient then raise EAudioException.Create('Audio instance is not ambient');
  FInstances[iIndex].Position := aPosition;
  ApplyGain(iIndex);
end;

procedure TAudio.SetAmbientRadius( aInstance : TAudioInstanceHandle; aRadius : Single );
var iIndex : Integer;
begin
  if aRadius <= 0.0 then raise EAudioException.Create('Ambient radius must be positive');
  iIndex := CheckInstance(aInstance);
  if not FInstances[iIndex].Ambient then raise EAudioException.Create('Audio instance is not ambient');
  FInstances[iIndex].Radius := aRadius;
  ApplyGain(iIndex);
end;

procedure TAudio.StopAmbient( aInstance : TAudioInstanceHandle; aFadeOutMS : DWord );
begin
  Stop( aInstance, aFadeOutMS );
end;

procedure TAudio.SetMusicVolumePercent( aPercent : Integer );
var i : Integer;
begin
  FMusicVolume := ClampPercent(aPercent);
  for i := 0 to High(FInstances) do
    if FInstances[i].Playing and FInstances[i].Music then ApplyGain(i);
end;

function TAudio.GetMusicVolumePercent : Integer;
begin
  Result := FMusicVolume;
end;

procedure TAudio.SetSoundVolumePercent( aPercent : Integer );
var i : Integer;
begin
  FSoundVolume := ClampPercent(aPercent);
  for i := 0 to High(FInstances) do
    if FInstances[i].Playing and not FInstances[i].Music then ApplyGain(i);
end;

function TAudio.GetSoundVolumePercent : Integer;
begin
  Result := FSoundVolume;
end;

procedure TAudio.Update( aElapsedMS : DWord );
var i : Integer;
begin
  BackendUpdate;
  for i := 0 to High(FInstances) do if FInstances[i].Playing then
  begin
    with FInstances[i] do if FadeTotal > 0 then
    begin
      if FadePassed + aElapsedMS >= FadeTotal then FadePassed := FadeTotal else Inc(FadePassed, aElapsedMS);
      ApplyGain(i);
      if (FadePassed = FadeTotal) and StopOnFade then
      begin
        StopBackend(Data);
        ClearInstance(i);
        Continue;
      end;
      if FadePassed = FadeTotal then FadeTotal := 0;
    end;
    if FInstances[i].Playing and not BackendPlaying(FInstances[i].Data) then
    begin
      StopBackend(FInstances[i].Data);
      ClearInstance(i);
    end;
  end;
end;

procedure TAudio.Reset;
var i : Integer;
begin
  for i := 0 to High(FInstances) do
    if FInstances[i].Playing then
    begin
      StopBackend( FInstances[i].Data );
      ClearInstance( i );
    end;

  for i := 0 to High(FAssets) do
    if FAssets[i].Loaded then
    begin
      FreeBackendAsset( FAssets[i].Data );
      FillChar( FAssets[i], SizeOf(TAsset), 0 );
    end;

  SetLength( FInstances, 0 );
  SetLength( FAssets, 0 );
  FMusic := 0;
end;

procedure TAudio.BackendUpdate;
begin
end;

end.
