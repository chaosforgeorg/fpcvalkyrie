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

uses Classes, SysUtils, vsound, vsdl2mixerlibrary;

// The basic sound class, published as the singleton @link(Sound).
// Should be initialized and disposed via TSystems.
type

{ TSound }

{ TSDLSound }

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
       procedure FreeMusic( aData : Pointer; const aType : String ); override;
       // Implementation of Sound Freeing
       procedure FreeSound( aData : Pointer ); override;
       // Implementation of get error
       function GetError( ) : AnsiString; override;
       // Implementation of play Sound
       procedure PlaySound( aData : Pointer; aVolume : Byte; aPan : Integer = -1 ); override;
       // Implementation of play Sound
       procedure PlayMusic( aData : Pointer; const aType : string; aRepeat : Boolean = True ); override;
       // Implementation of StopMusic
       procedure StopMusic( aData : Pointer; const aType : string ); override;
       // Implementation of StopSound
       procedure StopSound(); override;	   
       // Implementation of VolumeMusic
       procedure VolumeMusic( aData : Pointer; const aType : string; aVolume : Byte ); override;
     end;

implementation

uses vutil, vsdl2library;

{ TSDLSound }

constructor TSDLSound.Create;
begin
  inherited Create;
  LoadSDL2Mixer;
  Log( LOGINFO, 'Opening SDL_Audio...' );
  if SDL_Init(SDL_INIT_AUDIO) < 0 then
    raise Exception.Create('Can''t open SDL_Audio!');
  if not OpenDevice then
    raise Exception.Create('Can''t open SDL_Mixer!');
  Mix_VolumeMusic( GetMusicVolume );
end;

destructor TSDLSound.Destroy;
begin
  inherited Destroy;
  if SDL2 <> nil then
  begin
    if SDL2_mixer <> nil then
      Mix_CloseAudio();
    SDL_Quit();
  end;
end;

function TSDLSound.OpenDevice : Boolean;
var iResult : Integer;
begin
  Log( LOGINFO, 'Opening SDL_Mixer...' );
  iResult := Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048);
  if iResult < 0 then
  begin
    Log( LOGERROR, 'Could not open SDL_Mixer! Error : %s', [SDL_GetError()] );
    Exit( False );
  end;
  Log( LOGINFO, 'SDL_Mixer opened.' );
  Exit( True );
end;

function TSDLSound.LoadMusic(const aFileName: AnsiString; Streamed : Boolean): Pointer;
begin
  Exit( Mix_LoadMUS( PChar( aFileName ) ) )
end;

function TSDLSound.LoadSound(const aFileName: AnsiString): Pointer;
begin
  Exit( Mix_LoadWAV( PChar( aFileName ) ) );
end;

function TSDLSound.LoadMusicStream(Stream: TStream; Size: DWord; Streamed : Boolean): Pointer;
var Data : Pointer;
begin
  if Streamed then
  begin
    Data := GetCacheMem( Size );
    Stream.Read( Data^, Size );
    Exit( Mix_LoadMUS_RW( SDL_RWFromMem( Data, Size ), 0 ) );
  end
  else
    Exit( Mix_LoadMUS_RW( SDL_RWopsFromStream( Stream, Size ), 0 ) );
end;

function TSDLSound.LoadSoundStream(Stream: TStream; Size: DWord): Pointer;
begin
  Exit( Mix_LoadWAV_RW( SDL_RWopsFromStream( Stream, Size ), 0 ) );
end;

procedure TSDLSound.FreeMusic(aData: Pointer; const aType : String );
begin
  Mix_FreeMusic(PMix_Music(aData));
end;

procedure TSDLSound.FreeSound(aData: Pointer);
begin
  Mix_FreeChunk(PMix_Chunk(aData));
end;

function TSDLSound.GetError(): AnsiString;
var iError : AnsiString;
begin
  iError := SDL_GetError();
  Exit( iError );
end;

procedure TSDLSound.PlaySound(aData: Pointer; aVolume: Byte; aPan: Integer);
var iChannel : Integer;
begin
  iChannel := Mix_PlayChannel( -1, PMix_Chunk(aData), 0 );
  Mix_Volume( iChannel, aVolume );
  if aPan <> -1 then Mix_SetPanning( iChannel, 255-aPan, aPan );
end;

procedure TSDLSound.PlayMusic(aData: Pointer; const aType : string; aRepeat: Boolean);
begin
  Mix_VolumeMusic(MusicVolume);
  if aRepeat then
    Mix_PlayMusic( PMix_Music(aData), -1 )
  else
    Mix_PlayMusic( PMix_Music(aData), 1 );
end;

procedure TSDLSound.StopMusic(aData: Pointer; const aType : string );
begin
  Mix_HaltMusic();
end;

procedure TSDLSound.StopSound();
begin
  Mix_HaltChannel(-1);
end;

procedure TSDLSound.VolumeMusic(aData: Pointer; const aType : string; aVolume: Byte );
begin
  Mix_VolumeMusic(aVolume);
end;

end.
