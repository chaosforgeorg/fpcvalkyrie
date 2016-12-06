unit vsdlmixerlibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, Types, vlibrary, vsdllibrary;

const
{$IFDEF WINDOWS}
  SDLMixerDefaultPath = 'SDL_mixer.dll';
{$ELSE}
  {$IFDEF DARWIN}
  SDLMixerDefaultPath = 'SDL_mixer.framework/SDL_mixer';
  {$ELSE}
  SDLMixerDefaultPath = 'libSDL_mixer-1.2.so.0';
  {$ENDIF}
{$ENDIF}


const
  SDL_MIXER_MAJOR_VERSION = 1;
  SDL_MIXER_MINOR_VERSION = 2;
  SDL_MIXER_PATCHLEVEL    = 8;

{$include vsdlmixerconst.inc}
{$include vsdlmixertypes.inc}

{$DEFINE calldecl := cdecl}

var
  Mix_Linked_Version    : function : PSDL_version; calldecl;
  Mix_OpenAudio         : function ( frequency : integer; format : Uint16; channels : integer; chunksize : integer ) : integer; calldecl;
  Mix_AllocateChannels  : function ( numchannels : integer ) : integer; calldecl;
  Mix_QuerySpec         : function ( var frequency : integer; var format : Uint16; var channels : integer ) : integer; calldecl;
  Mix_LoadWAV_RW        : function ( src : PSDL_RWops; freesrc : integer ) : PMix_Chunk; calldecl;
  Mix_LoadMUS           : function ( const filename : PChar ) : PMix_Music; calldecl;
  Mix_LoadMUS_RW        : function ( src : PSDL_RWops ) : PMix_Music; calldecl;
  Mix_QuickLoad_WAV     : function ( mem : PUint8 ) : PMix_Chunk; calldecl;
  Mix_FreeChunk         : procedure ( chunk : PMix_Chunk ); calldecl;
  Mix_FreeMusic         : procedure ( music : PMix_Music ); calldecl;
  Mix_GetMusicType      : function ( music : PMix_Music ) : TMix_MusicType; calldecl;
  Mix_SetPostMix        : procedure ( mix_func : TMixFunction; arg : Pointer ); calldecl;
  Mix_HookMusic         : procedure ( mix_func : TMixFunction; arg : Pointer ); calldecl;
  Mix_HookMusicFinished : procedure ( music_finished : Pointer ); calldecl;
  Mix_GetMusicHookData  : function : Pointer; calldecl;
  Mix_ChannelFinished   : procedure ( channel_finished : TChannel_finished ); calldecl;

  Mix_RegisterEffect       : function ( chan : integer; f : TMix_EffectFunc; d : TMix_EffectDone; arg : Pointer ) : integer; calldecl;
  Mix_UnregisterEffect     : function ( channel : integer; f : TMix_EffectFunc ) : integer; calldecl;
  Mix_UnregisterAllEffects : function ( channel : integer ) : integer; calldecl;

  Mix_SetPanning           : function ( channel : integer; left : Uint8; right : Uint8  ) : integer; calldecl;
  Mix_SetPosition          : function ( channel :integer; angle : Sint16; distance : Uint8  ) : integer; calldecl;
  Mix_SetDistance          : function ( channel : integer; distance : Uint8 ) : integer; calldecl;
  Mix_SetReverseStereo     : function ( channel : integer; flip : integer ) : integer; calldecl;
  Mix_ReserveChannels      : function ( num : integer ) : integer; calldecl;

  Mix_GroupChannel      : function ( which : integer; tag : integer ) : integer; calldecl;
  Mix_GroupChannels     : function ( from : integer; to_ : integer; tag : integer ) : integer; calldecl;
  Mix_GroupAvailable    : function ( tag : integer ) : integer; calldecl;
  Mix_GroupCount        : function ( tag : integer ) : integer; calldecl;
  Mix_GroupOldest       : function ( tag : integer ) : integer; calldecl;
  Mix_GroupNewer        : function ( tag : integer ) : integer; calldecl;
  Mix_PlayChannelTimed  : function ( channel : integer; chunk : PMix_Chunk; loops : integer; ticks : integer ) : integer; calldecl;

  Mix_PlayMusic          : function ( music : PMix_Music; loops : integer ) : integer; calldecl;
  Mix_FadeInMusic        : function ( music : PMix_Music; loops : integer; ms : integer ) : integer; calldecl;
  Mix_FadeInChannelTimed : function ( channel : integer; chunk : PMix_Chunk; loops : integer; ms : integer; ticks : integer ) : integer; calldecl;

  Mix_Volume           : function ( channel : integer; volume : integer ) : integer; calldecl;
  Mix_VolumeChunk      : function ( chunk : PMix_Chunk; volume : integer ) : integer; calldecl;
  Mix_VolumeMusic      : function ( volume : integer ) : integer; calldecl;
  Mix_HaltChannel      : function ( channel : integer ) : integer; calldecl;
  Mix_HaltGroup        : function ( tag : integer ) : integer; calldecl;
  Mix_HaltMusic        : function : integer; calldecl;
  Mix_ExpireChannel    : function ( channel : integer; ticks : integer ) : integer; calldecl;
  Mix_FadeOutChannel   : function ( which : integer; ms : integer ) : integer; calldecl;
  Mix_FadeOutGroup     : function ( tag : integer; ms : integer ) : integer; calldecl;
  Mix_FadeOutMusic     : function ( ms : integer ) : integer; calldecl;
  Mix_FadingMusic      : function : TMix_Fading; calldecl;
  Mix_FadingChannel    : function ( which : integer ) : TMix_Fading; calldecl;
  Mix_Pause            : procedure ( channel : integer ); calldecl;
  Mix_Resume           : procedure ( channel : integer ); calldecl;
  Mix_Paused           : function ( channel : integer ) : integer; calldecl;
  Mix_PauseMusic       : procedure ; calldecl;
  Mix_ResumeMusic      : procedure ; calldecl;
  Mix_RewindMusic      : procedure ; calldecl;
  Mix_PausedMusic      : function : integer; calldecl;
  Mix_SetMusicPosition : function ( position : double ) : integer; calldecl;
  Mix_Playing          : function ( channel : integer ) : integer; calldecl;
  Mix_PlayingMusic     : function : integer; calldecl;
  Mix_SetMusicCMD      : function ( const command : PChar ) : integer; calldecl;
  Mix_SetSynchroValue  : function ( value : integer ) : integer; calldecl;
  Mix_GetChunk         : function ( channel : integer ) : PMix_Chunk; calldecl;
  Mix_CloseAudio       : procedure ; calldecl;

procedure SDL_MIXER_VERSION(var X: TSDL_Version);
function Mix_LoadWAV( filename : PChar ) : PMix_Chunk;
function Mix_PlayChannel( channel : integer; chunk : PMix_Chunk; loops : integer ) : integer;
function Mix_FadeInChannel( channel : integer; chunk : PMix_Chunk; loops : integer; ms : integer ) : integer;
procedure Mix_SetError( fmt : PChar );
function Mix_GetError : PChar;

var
  SDL_mixer : TLibrary = nil;

function LoadSDLMixer( const aPath : AnsiString = SDLMixerDefaultPath ) : Boolean;

implementation

uses Math;

function LoadSDLMixer ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL_mixer.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL_mixer : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL = nil then LoadSDL();
  if SDL = nil then Exit( False );
  if SDL_mixer <> nil then Exit( True );
  SDL_mixer := TLibrary.Load( aPath );
  if SDL_mixer = nil then Exit( False );

  {$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
  {$endif}

  Pointer(Mix_Linked_Version)     := GetSymbol('Mix_Linked_Version');
  Pointer(Mix_OpenAudio)          := GetSymbol('Mix_OpenAudio');
  Pointer(Mix_AllocateChannels)   := GetSymbol('Mix_AllocateChannels');
  Pointer(Mix_QuerySpec)          := GetSymbol('Mix_QuerySpec');
  Pointer(Mix_LoadWAV_RW)         := GetSymbol('Mix_LoadWAV_RW');
  Pointer(Mix_LoadMUS)            := GetSymbol('Mix_LoadMUS');
  Pointer(Mix_LoadMUS_RW)         := GetSymbol('Mix_LoadMUS_RW');
  Pointer(Mix_QuickLoad_WAV)      := GetSymbol('Mix_QuickLoad_WAV');
  Pointer(Mix_FreeChunk)          := GetSymbol('Mix_FreeChunk');
  Pointer(Mix_FreeMusic)          := GetSymbol('Mix_FreeMusic');
  Pointer(Mix_GetMusicType)       := GetSymbol('Mix_GetMusicType');
  Pointer(Mix_SetPostMix)         := GetSymbol('Mix_SetPostMix');
  Pointer(Mix_HookMusic)          := GetSymbol('Mix_HookMusic');
  Pointer(Mix_HookMusicFinished)  := GetSymbol('Mix_HookMusicFinished');
  Pointer(Mix_GetMusicHookData)   := GetSymbol('Mix_GetMusicHookData');
  Pointer(Mix_ChannelFinished)    := GetSymbol('Mix_ChannelFinished');

  Pointer(Mix_RegisterEffect)        := GetSymbol('Mix_RegisterEffect');
  Pointer(Mix_UnregisterEffect)      := GetSymbol('Mix_UnregisterEffect');
  Pointer(Mix_UnregisterAllEffects)  := GetSymbol('Mix_UnregisterAllEffects');

  Pointer(Mix_SetPanning)            := GetSymbol('Mix_SetPanning');
  Pointer(Mix_SetPosition)           := GetSymbol('Mix_SetPosition');
  Pointer(Mix_SetDistance)           := GetSymbol('Mix_SetDistance');
  Pointer(Mix_SetReverseStereo)      := GetSymbol('Mix_SetReverseStereo');
  Pointer(Mix_ReserveChannels)       := GetSymbol('Mix_ReserveChannels');

  Pointer(Mix_GroupChannel)       := GetSymbol('Mix_GroupChannel');
  Pointer(Mix_GroupChannels)      := GetSymbol('Mix_GroupChannels');
  Pointer(Mix_GroupAvailable)     := GetSymbol('Mix_GroupAvailable');
  Pointer(Mix_GroupCount)         := GetSymbol('Mix_GroupCount');
  Pointer(Mix_GroupOldest)        := GetSymbol('Mix_GroupOldest');
  Pointer(Mix_GroupNewer)         := GetSymbol('Mix_GroupNewer');
  Pointer(Mix_PlayChannelTimed)   := GetSymbol('Mix_PlayChannelTimed');

  Pointer(Mix_PlayMusic)           := GetSymbol('Mix_PlayMusic');
  Pointer(Mix_FadeInMusic)         := GetSymbol('Mix_FadeInMusic');
  Pointer(Mix_FadeInChannelTimed)  := GetSymbol('Mix_FadeInChannelTimed');

  Pointer(Mix_Volume)            := GetSymbol('Mix_Volume');
  Pointer(Mix_VolumeChunk)       := GetSymbol('Mix_VolumeChunk');
  Pointer(Mix_VolumeMusic)       := GetSymbol('Mix_VolumeMusic');
  Pointer(Mix_HaltChannel)       := GetSymbol('Mix_HaltChannel');
  Pointer(Mix_HaltGroup)         := GetSymbol('Mix_HaltGroup');
  Pointer(Mix_HaltMusic)         := GetSymbol('Mix_HaltMusic');
  Pointer(Mix_ExpireChannel)     := GetSymbol('Mix_ExpireChannel');
  Pointer(Mix_FadeOutChannel)    := GetSymbol('Mix_FadeOutChannel');
  Pointer(Mix_FadeOutGroup)      := GetSymbol('Mix_FadeOutGroup');
  Pointer(Mix_FadeOutMusic)      := GetSymbol('Mix_FadeOutMusic');
  Pointer(Mix_FadingMusic)       := GetSymbol('Mix_FadingMusic');
  Pointer(Mix_FadingChannel)     := GetSymbol('Mix_FadingChannel');
  Pointer(Mix_Pause)             := GetSymbol('Mix_Pause');
  Pointer(Mix_Resume)            := GetSymbol('Mix_Resume');
  Pointer(Mix_Paused)            := GetSymbol('Mix_Paused');
  Pointer(Mix_PauseMusic)        := GetSymbol('Mix_PauseMusic');
  Pointer(Mix_ResumeMusic)       := GetSymbol('Mix_ResumeMusic');
  Pointer(Mix_RewindMusic)       := GetSymbol('Mix_RewindMusic');
  Pointer(Mix_PausedMusic)       := GetSymbol('Mix_PausedMusic');
  Pointer(Mix_SetMusicPosition)  := GetSymbol('Mix_SetMusicPosition');
  Pointer(Mix_Playing)           := GetSymbol('Mix_Playing');
  Pointer(Mix_PlayingMusic)      := GetSymbol('Mix_PlayingMusic');
  Pointer(Mix_SetMusicCMD)       := GetSymbol('Mix_SetMusicCMD');
  Pointer(Mix_SetSynchroValue)   := GetSymbol('Mix_SetSynchroValue');
  Pointer(Mix_GetChunk)          := GetSymbol('Mix_GetChunk');
  Pointer(Mix_CloseAudio)        := GetSymbol('Mix_CloseAudio');

  Exit( True );
end;

procedure SDL_MIXER_VERSION( var X : TSDL_version );
begin
  X.major := SDL_MIXER_MAJOR_VERSION;
  X.minor := SDL_MIXER_MINOR_VERSION;
  X.patch := SDL_MIXER_PATCHLEVEL;
end;

function Mix_LoadWAV( filename : PChar ) : PMix_Chunk;
begin
  result := Mix_LoadWAV_RW( SDL_RWFromFile( filename, 'rb' ), 1 );
end;

function Mix_PlayChannel( channel : integer; chunk : PMix_Chunk; loops : integer ) : integer;
begin
  result := Mix_PlayChannelTimed( channel, chunk, loops, -1 );
end;

function Mix_FadeInChannel( channel : integer; chunk : PMix_Chunk; loops :
  integer; ms : integer ) : integer;
begin
  result := Mix_FadeInChannelTimed( channel, chunk, loops, ms, -1 );
end;

procedure Mix_SetError( fmt : PChar );
begin
  SDL_SetError( fmt );
end;

function Mix_GetError : PChar;
begin
  result := SDL_GetError();
end;

finalization
  if SDL_mixer <> nil then FreeAndNil( SDL_mixer );

end.

