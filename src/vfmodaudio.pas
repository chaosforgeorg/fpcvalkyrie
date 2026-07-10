{$INCLUDE valkyrie.inc}
// @abstract(FMOD Sound system for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
//
// Implements an FMOD sound system for Valkyrie
unit vfmodaudio;

interface

uses Classes, SysUtils, vaudio, vfmodlibrary;

type TFMODAudio = class( TAudio )
  private type
    TFMODAsset = record
      Sound  : PFMOD_SOUND;
      Memory : Pointer;
      Size   : DWord;
    end;
    PFMODAsset = ^TFMODAsset;
  private
    FSystem : PFMOD_SYSTEM;
    FSoundGroup : PFMOD_CHANNELGROUP;
    FMusicGroup : PFMOD_CHANNELGROUP;
    procedure Check( aResult : FMOD_RESULT );
  protected
    function LoadBackend( const aFileName : AnsiString; aStream : Boolean ) : Pointer; override;
    function LoadBackendStream( aStream : TStream; aSize : DWord;
      const aNameHint : AnsiString; aStreamed : Boolean ) : Pointer; override;
    procedure FreeBackendAsset( aData : Pointer ); override;
    function StartBackend( aAssetData : Pointer; aStream, aLoop : Boolean;
      aMusic : Boolean ) : Pointer; override;
    procedure StopBackend( aInstanceData : Pointer ); override;
    function BackendPlaying( aInstanceData : Pointer ) : Boolean; override;
    procedure SetBackendGain( aInstanceData : Pointer; aGain : Single ); override;
    procedure BackendUpdate; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

procedure TFMODAudio.Check( aResult : FMOD_RESULT );
begin
  if aResult <> FMOD_OK then 
    raise EAudioException.Create( FMOD_ErrorString(aResult) );
end;

constructor TFMODAudio.Create;
begin
  inherited Create;
  if not LoadFMOD then
    raise EAudioException.Create('Unable to load FMOD');
  Check( FMOD_System_Create(@FSystem, FMOD_VERSION) );
  Check( FMOD_System_Init(FSystem, 64, FMOD_INIT_NORMAL, nil) );
  Check( FMOD_System_CreateChannelGroup(FSystem, 'sound', @FSoundGroup) );
  Check( FMOD_System_CreateChannelGroup(FSystem, 'music', @FMusicGroup) );
end;

destructor TFMODAudio.Destroy;
begin
  inherited Destroy;
  if FSoundGroup <> nil then FMOD_ChannelGroup_Release( FSoundGroup );
  if FMusicGroup <> nil then FMOD_ChannelGroup_Release( FMusicGroup );
  if FSystem <> nil then
  begin
    FMOD_System_Close( FSystem );
    FMOD_System_Release( FSystem );
  end;
end;

function TFMODAudio.LoadBackend( const aFileName : AnsiString; aStream : Boolean ) : Pointer;
var iAsset : PFMODAsset;
    iMode  : FMOD_MODE;
begin
  New( iAsset );
  FillChar( iAsset^, SizeOf(iAsset^), 0 );
  iMode := FMOD_2D;
  if aStream then iMode := iMode or FMOD_CREATESTREAM;
  if aStream then Check( FMOD_System_CreateStream(FSystem, PChar(aFileName), iMode, nil, @iAsset^.Sound) )
             else Check( FMOD_System_CreateSound(FSystem, PChar(aFileName), iMode, nil, @iAsset^.Sound) );
  Result := iAsset;
end;

function TFMODAudio.LoadBackendStream( aStream : TStream; aSize : DWord;
  const aNameHint : AnsiString; aStreamed : Boolean ) : Pointer;
var iAsset : PFMODAsset;
    iInfo  : FMOD_CREATESOUNDEXINFO;
    iMode  : FMOD_MODE;
begin
  New( iAsset );
  FillChar( iAsset^, SizeOf(iAsset^), 0 );
  GetMem( iAsset^.Memory, aSize );
  iAsset^.Size := aSize;
  aStream.Read( iAsset^.Memory^, aSize );
  FillChar( iInfo, SizeOf(iInfo), 0 );
  iInfo.cbsize := SizeOf(iInfo);
  iInfo.length := aSize;
  iMode := FMOD_2D or FMOD_OPENMEMORY;
  if aStreamed then iMode := iMode or FMOD_CREATESTREAM;
  if aStreamed then Check( FMOD_System_CreateStream(FSystem, PChar(iAsset^.Memory), iMode, @iInfo, @iAsset^.Sound) )
               else Check( FMOD_System_CreateSound(FSystem, PChar(iAsset^.Memory), iMode, @iInfo, @iAsset^.Sound) );
  Result := iAsset;
end;

procedure TFMODAudio.FreeBackendAsset( aData : Pointer );
var iAsset : PFMODAsset;
begin
  iAsset := PFMODAsset(aData);
  FMOD_Sound_Release(iAsset^.Sound);
  if iAsset^.Memory <> nil then FreeMem(iAsset^.Memory, iAsset^.Size);
  Dispose(iAsset);
end;

function TFMODAudio.StartBackend( aAssetData : Pointer; aStream, aLoop : Boolean;
  aMusic : Boolean ) : Pointer;
var iAsset   : PFMODAsset;
    iChannel : PFMOD_CHANNEL;
begin
  iAsset := PFMODAsset( aAssetData );
  iChannel := nil;
  if aMusic then Check(FMOD_System_PlaySound(FSystem, iAsset^.Sound, FMusicGroup, 1, @iChannel))
            else Check(FMOD_System_PlaySound(FSystem, iAsset^.Sound, FSoundGroup, 1, @iChannel));
  if aLoop then
  begin
    Check( FMOD_Channel_SetMode(iChannel, FMOD_LOOP_NORMAL) );
    Check( FMOD_Channel_SetLoopCount(iChannel, -1) );
  end;
  Check(FMOD_Channel_SetPaused(iChannel, 0));
  Result := iChannel;
end;

procedure TFMODAudio.StopBackend( aInstanceData : Pointer );
begin
  FMOD_Channel_Stop( PFMOD_CHANNEL(aInstanceData) );
end;

function TFMODAudio.BackendPlaying( aInstanceData : Pointer ) : Boolean;
var iPlaying : FMOD_BOOL;
begin
  iPlaying := 0;
  Result := (FMOD_Channel_IsPlaying(PFMOD_CHANNEL(aInstanceData), iPlaying) = FMOD_OK) and (iPlaying <> 0);
end;

procedure TFMODAudio.SetBackendGain( aInstanceData : Pointer; aGain : Single );
begin
  FMOD_Channel_SetVolume( PFMOD_CHANNEL(aInstanceData), aGain );
end;

procedure TFMODAudio.BackendUpdate;
begin
  Check( FMOD_System_Update(FSystem) );
end;

end.
