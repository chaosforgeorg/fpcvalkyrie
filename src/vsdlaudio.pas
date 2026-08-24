{$INCLUDE valkyrie.inc}
unit vsdlaudio;

interface

uses Classes, SysUtils, vaudio, vsdl3library, vsdl3mixerlibrary;

type
  TSDLAudio = class(TAudio)
  private type
    TSDLAsset = record
      Audio  : PMIX_Audio;
      IO     : PSDL_IOStream;
      Stream : Boolean;
    end;
    PSDLAsset = ^TSDLAsset;
  private
    FMixer : PMIX_Mixer;
  protected
    function LoadBackend( const aFileName : AnsiString; aStream : Boolean ) : Pointer; override;
    function LoadBackendStream( aStream : TStream; aSize : DWord;
      const aNameHint : AnsiString; aStreamed : Boolean ) : Pointer; override;
    procedure FreeBackendAsset( aData : Pointer ); override;
    function StartBackend( aAssetData : Pointer; aStream, aLoop : Boolean;
      aMusic : Boolean; aGain : Single ) : Pointer; override;
    procedure StopBackend( aInstanceData : Pointer ); override;
    function BackendPlaying( aInstanceData : Pointer ) : Boolean; override;
    procedure SetBackendGain( aInstanceData : Pointer; aGain : Single ); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

constructor TSDLAudio.Create;
begin
  inherited Create;
  if not LoadSDL3Mixer then
    raise EAudioException.Create('Unable to load SDL3_mixer');
  if not SDL_Init(SDL_INIT_AUDIO) then
    raise EAudioException.Create(SDL_GetError());
  if not MIX_Init() then
    raise EAudioException.Create(SDL_GetError());
  FMixer := MIX_CreateMixerDevice($FFFFFFFF, nil);
  if FMixer = nil then 
    raise EAudioException.Create(SDL_GetError());
end;

destructor TSDLAudio.Destroy;
begin
  inherited Destroy;
  if FMixer <> nil then MIX_DestroyMixer(FMixer);
  if SDL3_mixer <> nil then MIX_Quit;
  SDL_Quit;
end;

function TSDLAudio.LoadBackend( const aFileName : AnsiString; aStream : Boolean ) : Pointer;
var iAsset : PSDLAsset;
begin
  New( iAsset );
  FillChar( iAsset^, SizeOf(iAsset^), 0 );
  iAsset^.Stream := aStream;
  if aStream then iAsset^.IO := SDL_IOFromFile( PChar(aFileName), 'rb' )
             else iAsset^.Audio := MIX_LoadAudio( FMixer, PChar(aFileName), True );
  if (iAsset^.IO = nil) and (iAsset^.Audio = nil) then
  begin
    Dispose( iAsset );
    Exit( nil );
  end;
  Result := iAsset;
end;

function TSDLAudio.LoadBackendStream( aStream : TStream; aSize : DWord;
  const aNameHint : AnsiString; aStreamed : Boolean ) : Pointer;
var iAsset : PSDLAsset;
    iIO    : PSDL_IOStream;
begin
  New( iAsset );
  FillChar( iAsset^, SizeOf(iAsset^), 0 );
  iAsset^.Stream := aStreamed;
  iIO := SDL_IOFromStream( aStream, aSize, False, True );
  if aStreamed then iAsset^.IO := SDL_IOCopyToOwningMemStream( iIO, True )
               else iAsset^.Audio := MIX_LoadAudio_IO( FMixer, iIO, True, True );
  if (iAsset^.IO = nil) and (iAsset^.Audio = nil) then
  begin
    Dispose( iAsset );
    Exit( nil );
  end;
  Result := iAsset;
end;

procedure TSDLAudio.FreeBackendAsset( aData : Pointer );
var iAsset : PSDLAsset;
begin
  iAsset := PSDLAsset(aData);
  if iAsset^.Stream then SDL_CloseIO(iAsset^.IO) else MIX_DestroyAudio(iAsset^.Audio);
  Dispose(iAsset);
end;

function TSDLAudio.StartBackend( aAssetData : Pointer; aStream, aLoop : Boolean;
  aMusic : Boolean; aGain : Single ) : Pointer;
var iAsset : PSDLAsset;
    iTrack : PMIX_Track;
    iProps : SDL_PropertiesID;
begin
  iAsset := PSDLAsset(aAssetData);
  iTrack := MIX_CreateTrack(FMixer);
  if iTrack = nil then Exit( nil );
  if iAsset^.Stream then MIX_SetTrackIOStream( iTrack, iAsset^.IO, False )
                    else MIX_SetTrackAudio( iTrack, iAsset^.Audio );
  MIX_SetTrackGain( iTrack, aGain );
  iProps := SDL_CreateProperties();
  if aLoop then SDL_SetNumberProperty(iProps, 'SDL_mixer.play.loops', -1);
  if not MIX_PlayTrack( iTrack, iProps ) then
  begin
    SDL_DestroyProperties( iProps );
    MIX_DestroyTrack( iTrack );
    Exit( nil );
  end;
  SDL_DestroyProperties(iProps);
  Result := iTrack;
end;

procedure TSDLAudio.StopBackend( aInstanceData : Pointer );
var iTrack : PMIX_Track;
begin
  iTrack := PMIX_Track(aInstanceData);
  MIX_StopTrack(iTrack, 0);
  MIX_DestroyTrack(iTrack);
end;

function TSDLAudio.BackendPlaying( aInstanceData : Pointer ) : Boolean;
begin
  Result := MIX_TrackPlaying( PMIX_Track(aInstanceData) );
end;

procedure TSDLAudio.SetBackendGain( aInstanceData : Pointer; aGain : Single );
begin
  MIX_SetTrackGain( PMIX_Track(aInstanceData), aGain );
end;

end.
