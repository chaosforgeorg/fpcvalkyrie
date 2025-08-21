{$INCLUDE valkyrie.inc}
// @abstract(SDL Sound system for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 03, 2009)
// @lastmod(May 03, 2009)
//
// Implements an SDL sound system for Valkyrie
//
// Default behaviour for music is stoping the previous song before
// playing a new one.
//
// TODO : Currently the sound system opens and closes SDL. Make a separate SDL
//   from SDLVideo system

unit vsdlsound;

interface

uses Classes, SysUtils, vsound, vgenerics, vsdl3mixerlibrary;

// The basic sound class, published as the singleton @link(Sound).
// Should be initialized and disposed via TSystems.
type

{ TSound }

{ TSDLSound }
TSDLTrackArray = specialize TGArray< PMIX_Track >;

TSDLSound = class(TSound)
       // Initializes the Sound system.
       constructor Create; override;
       // Deinitializes the Sound system.
       destructor Destroy; override;
     protected
       // Open audio device with given parameters
       function OpenDevice : Boolean;
       // Implementation of Music Loading
       function LoadMusic( const aFileName : AnsiString; Streamed : Boolean ) : Pointer; override;
       // Implementation of Sound Loading
       function LoadSound( const aFileName : AnsiString ) : Pointer; override;
       // Implementation of Music Loading
       function LoadMusicStream( Stream : TStream; Size : DWord; Streamed : Boolean ) : Pointer; override;
       // Implementation of Sound Loading
       function LoadSoundStream( Stream : TStream; Size : DWord ) : Pointer; override;
       // Implementation of Music Freeing
       procedure FreeMusic( aData : Pointer; const aType : Ansistring ); override;
       // Implementation of Sound Freeing
       procedure FreeSound( aData : Pointer ); override;
       // Implementation of get error
       function GetError( ) : AnsiString; override;
       // Implementation of play Sound
       procedure PlaySound( aData : Pointer; aVolume : Byte; aPan : Integer = -1 ); override;
       // Implementation of play Sound
       procedure PlayMusic( aData : Pointer; const aType : Ansistring; aRepeat : Boolean = True ); override;
       // Implementation of StopMusic
       procedure StopMusic( aData : Pointer; const aType : Ansistring ); override;
       // Implementation of StopSound
       procedure StopSound(); override;	   
       // Implementation of VolumeMusic
       procedure VolumeMusic( aData : Pointer; const aType : Ansistring; aVolume : Byte ); override;
     private
       function GetFreeSoundTrack : PMIX_Track;
       function IsStreamed( const aType : Ansistring ) : Boolean;
     private
       FMixer : PMIX_Mixer;
       FMusic : PMIX_Track;
       FSound : TSDLTrackArray;
     end;

implementation

uses vutil, vsdl3library;

function ByteVolumeToGain( aVol: Byte ): Single; inline;
begin
  // SDL2 volumes were 0..128; clamp defensively and convert to 0..1
  if aVol > 128 then aVol := 128;
  Result := aVol / 128.0;
end;


procedure Pan255ToLR( const aPan: Integer; out gains : MIX_StereoGains ); inline;
var P: Integer;
begin
  // aPan: 0 = full left, 255 = full right, -1 = no pan
  if aPan < 0 then
  begin
    gains.left := 1.0; gains.right := 1.0;
    Exit;
  end;
  if aPan < 0 then P := 0 else if aPan > 255 then P := 255 else P := aPan;
  gains.left  := (255 - P) / 255.0;
  gains.right := P / 255.0;
end;

{ TSDLSound }

constructor TSDLSound.Create;
var i      : Integer;
    iTrack : PMIX_Track;
begin
  inherited Create;
  FMixer := nil;
  LoadSDL3Mixer;
  Log( LOGINFO, 'Opening SDL_Audio...' );
  if not SDL_Init(SDL_INIT_AUDIO) then
    raise Exception.Create('Can''t open SDL_Audio!');
  if not OpenDevice then
    raise Exception.Create('Can''t open SDL_Mixer!');
  FSound := TSDLTrackArray.Create;
  FMusic := MIX_CreateTrack( FMixer );
  MIX_TagTrack( FMusic, 'music' );
  for i := 1 to 32 do
  begin
    iTrack := MIX_CreateTrack( FMixer );
    MIX_TagTrack( iTrack, 'sfx' );
    FSound.Push( iTrack );
  end;
end;

destructor TSDLSound.Destroy;
var i : Integer;
begin
   MIX_StopAllTracks( FMixer, 0 );
   MIX_SetTrackIOStream( FMusic, nil, false );

  inherited Destroy;
  if SDL3 <> nil then
  begin
    if FMixer <> nil then
    begin
      MIX_UnTagTrack( FMusic, 'music' );
      MIX_DestroyTrack( FMusic );
      FMusic := nil;
      if FSound.Size > 0 then
        for i := 0 to FSound.Size - 1 do
        begin
          MIX_UnTagTrack( FSound[i], 'sfx' );
          MIX_DestroyTrack( FSound[i] );
        end;
      FreeAndNil( FSound );
      MIX_DestroyMixer( FMixer );
      FMixer := nil;
    end;
    if SDL3_mixer <> nil then
      MIX_Quit();
    SDL_Quit();
  end;
end;

function TSDLSound.OpenDevice : Boolean;
begin
  Log( LOGINFO, 'Opening SDL_Mixer...' );
  if not MIX_Init() then
  begin
    Log( LOGERROR, 'Could not open SDL_Mixer! Error : %s', [SDL_GetError()] );
    Exit( False );
  end;
  FMixer := MIX_CreateMixerDevice( $FFFFFFFF, nil);
  if FMixer = nil then
  begin
    Log( LOGERROR, 'Could not open SDL_Mixer! Error : %s', [SDL_GetError()] );
    Exit( False );
  end;
  Log( LOGINFO, 'SDL_Mixer opened.' );
  Exit( True );
end;

function TSDLSound.LoadMusic(const aFileName: AnsiString; Streamed : Boolean): Pointer;
begin
  if Streamed
    then Exit( SDL_IOFromFile( PChar(aFileName), 'rb' ) )
    else Exit( MIX_LoadAudio( FMixer, PChar( aFileName ), True ) );
end;

function TSDLSound.LoadSound(const aFileName: AnsiString): Pointer;
begin
  Exit( MIX_LoadAudio( FMixer, PChar( aFileName ), True ) );
end;

function TSDLSound.LoadMusicStream(Stream: TStream; Size: DWord; Streamed : Boolean): Pointer;
var iStream : PSDL_IOStream;
begin
  iStream := SDL_IOFromStream( Stream, Size, False, True );
  if Streamed
    then Exit( SDL_IOCopyToOwningMemStream( iStream, True ) )
    else Exit( Mix_LoadAudio_IO( FMixer, iStream, False, True ) );
end;

function TSDLSound.LoadSoundStream(Stream: TStream; Size: DWord): Pointer;
var iStream : PSDL_IOStream;
begin
  iStream := SDL_IOFromStream( Stream, Size, False, True );
  Exit( Mix_LoadAudio_IO( FMixer, iStream, False, True ) );
end;

procedure TSDLSound.FreeMusic(aData: Pointer; const aType : Ansistring );
begin
  if IsStreamed( aType )
    then SDL_CloseIO( PSDL_IOStream(aData) )
    else MIX_DestroyAudio( PMIX_Audio(aData));
end;

procedure TSDLSound.FreeSound(aData: Pointer);
begin
  MIX_DestroyAudio( PMIX_Audio(aData));
end;

function TSDLSound.GetError(): AnsiString;
var iError : AnsiString;
begin
  iError := SDL_GetError();
  Exit( iError );
end;

procedure TSDLSound.PlaySound(aData: Pointer; aVolume: Byte; aPan: Integer);
var iTrack : PMIX_Track;
    iGains : MIX_StereoGains;
begin
  iTrack := GetFreeSoundTrack;
  if iTrack = nil then Exit;

  MIX_SetTrackAudio( iTrack, PMIX_Audio(aData) );
  MIX_SetTrackGain( iTrack, ByteVolumeToGain(aVolume) );

  Pan255ToLR( aPan, iGains );
  MIX_SetTrackStereo( iTrack, @iGains );
  MIX_PlayTrack( iTrack, 0 );
end;

procedure TSDLSound.PlayMusic(aData: Pointer; const aType : Ansistring; aRepeat: Boolean);
var iLoopCount : Integer;
    iProps     : SDL_PropertiesID;
begin
  MIX_SetTagGain( FMixer, 'music', ByteVolumeToGain(MusicVolume) );
  if aRepeat
    then iLoopCount := -1
    else iLoopCount := 0;

  iProps := SDL_CreateProperties();
  SDL_SetNumberProperty( iProps, 'SDL_mixer.play.loops', iLoopCount );
  if IsStreamed( aType )
    then MIX_SetTrackIOStream( FMusic, PSDL_IOStream(aData), False )
    else MIX_SetTrackAudio( FMusic, PMIX_Audio(aData) );
  MIX_PlayTrack( FMusic, iProps );
  SDL_DestroyProperties( iProps );
end;

procedure TSDLSound.StopMusic(aData: Pointer; const aType : Ansistring );
begin
  MIX_StopTrack( FMusic, 0 );
end;

procedure TSDLSound.StopSound();
begin
  MIX_StopTag( FMixer, 'sfx', 4 );
end;

procedure TSDLSound.VolumeMusic(aData: Pointer; const aType : Ansistring; aVolume: Byte );
begin
  MIX_SetTagGain( FMixer, 'music', ByteVolumeToGain(aVolume) );
end;

function TSDLSound.GetFreeSoundTrack : PMIX_Track;
var i : Integer;
begin
  if FSound.Size > 0 then
    for i := 0 to FSound.Size - 1 do
      if not MIX_TrackPlaying(FSound[i]) then
        Exit( FSound[i] );
  Exit( FSound[ Random( FSound.Size ) ] );
end;

function TSDLSound.IsStreamed( const aType : Ansistring ) : Boolean;
begin
  Exit( ( aType = '.mp3' ) or ( aType = '.ogg' ) or ( aType = '.wav' ) );
end;

end.
