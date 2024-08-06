unit vsdl2mixerlibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary, vsdl2library;

const
{$IFDEF WINDOWS}
  SDL2MixerDefaultPath = 'SDL2_mixer.dll';
{$ELSE}
  {$IFDEF DARWIN}
  SDL2MixerDefaultPath = 'SDL2_mixer.framework/SDL2_mixer';
  {$ELSE}
  SDL2MixerDefaultPath = 'libSDL2_mixer-2.0.so.0';
  {$ENDIF}
{$ENDIF}


const
  SDL2_MIXER_MAJOR_VERSION = 2;
  SDL2_MIXER_MINOR_VERSION = 7;
  SDL2_MIXER_PATCHLEVEL    = 1;

{$include vsdl2mixerconst.inc}
{$include vsdl2mixertypes.inc}

// Function variables
var
  Mix_Linked_Version    : function: PSDL_Version; cdecl;
  Mix_Init              : function(flags: Integer): Integer; cdecl;
  Mix_Quit              : procedure; cdecl;
  Mix_OpenAudio         : function(frequency: Integer; format: UInt16; channels: Integer; chunksize: Integer): Integer; cdecl;
  Mix_OpenAudioDevice   : function(frequency: Integer; format: UInt16; channels: Integer; chunksize: Integer; device: PChar; allowed_changes: Integer): Integer; cdecl;
// (2.8.0) Mix_PauseAudio        : procedure(pause_on: Integer); cdecl;
  Mix_QuerySpec         : function(frequency: PInteger; format: PUInt16; channels: PInteger): Integer; cdecl;
  Mix_AllocateChannels  : function(numchans: Integer): Integer; cdecl;
  Mix_LoadWAV_RW        : function(src: PSDL_RWops; freesrc: Integer): PMix_Chunk; cdecl;
  Mix_LoadWAV           : function(file_: PChar): PMix_Chunk; cdecl;
  Mix_LoadMUS           : function(file_: PChar): PMix_Music; cdecl;
  Mix_LoadMUS_RW        : function(src: PSDL_RWops; freesrc: Integer): PMix_Music; cdecl;
  Mix_LoadMUSType_RW    : function(src: PSDL_RWops; type_: Mix_MusicType; freesrc: Integer): PMix_Music; cdecl;
  Mix_QuickLoad_WAV     : function(mem: PUInt8): PMix_Chunk; cdecl;
  Mix_QuickLoad_RAW     : function(mem: PUInt8; len: UInt32): PMix_Chunk; cdecl;
  Mix_FreeChunk         : procedure(chunk: PMix_Chunk); cdecl;
  Mix_FreeMusic         : procedure(music: PMix_Music); cdecl;
  Mix_GetNumChunkDecoders: function: Integer; cdecl;
  Mix_GetChunkDecoder   : function(index: Integer): PChar; cdecl;
  Mix_HasChunkDecoder   : function(name: PChar): SDL_bool; cdecl;
  Mix_GetNumMusicDecoders: function: Integer; cdecl;
  Mix_GetMusicDecoder   : function(index: Integer): PChar; cdecl;
  Mix_HasMusicDecoder   : function(name: PChar): SDL_bool; cdecl;
  Mix_GetMusicType      : function(music: PMix_Music): Mix_MusicType; cdecl;
  Mix_GetMusicTitle     : function(music: PMix_Music): PChar; cdecl;
  Mix_GetMusicTitleTag  : function(music: PMix_Music): PChar; cdecl;
  Mix_GetMusicArtistTag : function(music: PMix_Music): PChar; cdecl;
  Mix_GetMusicAlbumTag  : function(music: PMix_Music): PChar; cdecl;
  Mix_GetMusicCopyrightTag: function(music: PMix_Music): PChar; cdecl;
  Mix_SetPostMix        : procedure(mix_func: Pointer; arg: Pointer); cdecl;
  Mix_HookMusic         : procedure(mix_func: Pointer; arg: Pointer); cdecl;
  Mix_HookMusicFinished : procedure(music_finished: Pointer); cdecl;
  Mix_GetMusicHookData  : function: Pointer; cdecl;
  Mix_ChannelFinished   : procedure(channel_finished: Pointer); cdecl;
  Mix_RegisterEffect       : function(chan: Integer; f: Mix_EffectFunc_t; d: Mix_EffectDone_t; arg: Pointer): Integer; cdecl;
  Mix_UnregisterEffect     : function(channel: Integer; f: Mix_EffectFunc_t): Integer; cdecl;
  Mix_UnregisterAllEffects : function(channel: Integer): Integer; cdecl;
  Mix_SetPanning           : function(channel: Integer; left: UInt8; right: UInt8): Integer; cdecl;
  Mix_SetPosition          : function(channel: Integer; angle: SInt16; distance: UInt8): Integer; cdecl;
  Mix_SetDistance          : function(channel: Integer; distance: UInt8): Integer; cdecl;
  Mix_SetReverseStereo     : function(channel: Integer; flip: Integer): Integer; cdecl;
  Mix_ReserveChannels      : function(num: Integer): Integer; cdecl;
  Mix_GroupChannel         : function(which: Integer; tag: Integer): Integer; cdecl;
  Mix_GroupChannels        : function(from: Integer; to_: Integer; tag: Integer): Integer; cdecl;
  Mix_GroupAvailable       : function(tag: Integer): Integer; cdecl;
  Mix_GroupCount           : function(tag: Integer): Integer; cdecl;
  Mix_GroupOldest          : function(tag: Integer): Integer; cdecl;
  Mix_GroupNewer           : function(tag: Integer): Integer; cdecl;
  Mix_PlayChannel          : function(channel: Integer; chunk: PMix_Chunk; loops: Integer): Integer; cdecl;
  Mix_PlayChannelTimed     : function(channel: Integer; chunk: PMix_Chunk; loops: Integer; ticks: Integer): Integer; cdecl;
  Mix_PlayMusic            : function(music: PMix_Music; loops: Integer): Integer; cdecl;
  Mix_FadeInMusic          : function(music: PMix_Music; loops: Integer; ms: Integer): Integer; cdecl;
  Mix_FadeInMusicPos       : function(music: PMix_Music; loops: Integer; ms: Integer; position: Double): Integer; cdecl;
  Mix_FadeInChannel        : function(channel: Integer; chunk: PMix_Chunk; loops: Integer; ms: Integer): Integer; cdecl;
  Mix_FadeInChannelTimed   : function(channel: Integer; chunk: PMix_Chunk; loops: Integer; ms: Integer; ticks: Integer): Integer; cdecl;
  Mix_Volume               : function(channel: Integer; volume: Integer): Integer; cdecl;
  Mix_VolumeChunk          : function(chunk: PMix_Chunk; volume: Integer): Integer; cdecl;
  Mix_VolumeMusic          : function(volume: Integer): Integer; cdecl;
  Mix_GetMusicVolume       : function(music: PMix_Music): Integer; cdecl;
  Mix_MasterVolume         : function(volume: Integer): Integer; cdecl;
  Mix_HaltChannel          : function(channel: Integer): Integer; cdecl;
  Mix_HaltGroup            : function(tag: Integer): Integer; cdecl;
  Mix_HaltMusic            : function: Integer; cdecl;
  Mix_ExpireChannel        : function(channel: Integer; ticks: Integer): Integer; cdecl;
  Mix_FadeOutChannel       : function(which: Integer; ms: Integer): Integer; cdecl;
  Mix_FadeOutGroup         : function(tag: Integer; ms: Integer): Integer; cdecl;
  Mix_FadeOutMusic         : function(ms: Integer): Integer; cdecl;
  Mix_FadingMusic          : function: Mix_Fading; cdecl;
  Mix_FadingChannel        : function(which: Integer): Mix_Fading; cdecl;
  Mix_Pause                : procedure(channel: Integer); cdecl;
  Mix_Resume               : procedure(channel: Integer); cdecl;
  Mix_Paused               : function(channel: Integer): Integer; cdecl;
  Mix_PauseMusic           : procedure; cdecl;
  Mix_ResumeMusic          : procedure; cdecl;
  Mix_RewindMusic          : procedure; cdecl;
  Mix_PausedMusic          : function: Integer; cdecl;
  Mix_ModMusicJumpToOrder  : function(order: Integer): Integer; cdecl;
// (2.8.0)   Mix_StartTrack           : function(music: PMix_Music; track: Integer): Integer; cdecl;
// (2.8.0)   Mix_GetNumTracks         : function(music: PMix_Music): Integer; cdecl;
  Mix_SetMusicPosition     : function(position: Double): Integer; cdecl;
  Mix_GetMusicPosition     : function(music: PMix_Music): Double; cdecl;
  Mix_MusicDuration        : function(music: PMix_Music): Double; cdecl;
  Mix_GetMusicLoopStartTime: function(music: PMix_Music): Double; cdecl;
  Mix_GetMusicLoopEndTime  : function(music: PMix_Music): Double; cdecl;
  Mix_GetMusicLoopLengthTime: function(music: PMix_Music): Double; cdecl;
  Mix_Playing              : function(channel: Integer): Integer; cdecl;
  Mix_PlayingMusic         : function: Integer; cdecl;
  Mix_SetMusicCMD          : function(command: PChar): Integer; cdecl;
  Mix_SetSynchroValue      : function(value: Integer): Integer; cdecl;
  Mix_GetSynchroValue      : function: Integer; cdecl;
  Mix_SetSoundFonts        : function(paths: PChar): Integer; cdecl;
  Mix_GetSoundFonts        : function: PChar; cdecl;
  Mix_EachSoundFont        : function(function_: Pointer; data: Pointer): Integer; cdecl;
  Mix_SetTimidityCfg       : function(path: PChar): Integer; cdecl;
  Mix_GetTimidityCfg       : function: PChar; cdecl;
  Mix_GetChunk             : function(channel: Integer): PMix_Chunk; cdecl;
  Mix_CloseAudio           : procedure; cdecl;


var
  SDL2_mixer : TLibrary = nil;

function LoadSDL2Mixer( const aPath : AnsiString = SDL2MixerDefaultPath ) : Boolean;

implementation

function LoadSDL2Mixer ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL2_mixer.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL2_mixer : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL2 = nil then LoadSDL2();
  if SDL2 = nil then Exit( False );
  if SDL2_mixer <> nil then Exit( True );
  SDL2_mixer := TLibrary.Load( aPath );
  if SDL2_mixer = nil then Exit( False );

//  {$if defined(cpui386) or defined(cpux86_64)}
//  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
//  {$endif}

  Pointer(Mix_Linked_Version) := GetSymbol('Mix_Linked_Version');
  Pointer(Mix_Init) := GetSymbol('Mix_Init');
  Pointer(Mix_Quit) := GetSymbol('Mix_Quit');

  Pointer(Mix_OpenAudio) := GetSymbol('Mix_OpenAudio');
  Pointer(Mix_OpenAudioDevice) := GetSymbol('Mix_OpenAudioDevice');
//  Pointer(Mix_PauseAudio) := GetSymbol('Mix_PauseAudio');
  Pointer(Mix_QuerySpec) := GetSymbol('Mix_QuerySpec');
  Pointer(Mix_AllocateChannels) := GetSymbol('Mix_AllocateChannels');
  Pointer(Mix_LoadWAV_RW) := GetSymbol('Mix_LoadWAV_RW');
  Pointer(Mix_LoadWAV) := GetSymbol('Mix_LoadWAV');
  Pointer(Mix_LoadMUS) := GetSymbol('Mix_LoadMUS');
  Pointer(Mix_LoadMUS_RW) := GetSymbol('Mix_LoadMUS_RW');
  Pointer(Mix_LoadMUSType_RW) := GetSymbol('Mix_LoadMUSType_RW');
  Pointer(Mix_QuickLoad_WAV) := GetSymbol('Mix_QuickLoad_WAV');
  Pointer(Mix_QuickLoad_RAW) := GetSymbol('Mix_QuickLoad_RAW');
  Pointer(Mix_FreeChunk) := GetSymbol('Mix_FreeChunk');
  Pointer(Mix_FreeMusic) := GetSymbol('Mix_FreeMusic');
  Pointer(Mix_GetNumChunkDecoders) := GetSymbol('Mix_GetNumChunkDecoders');
  Pointer(Mix_GetChunkDecoder) := GetSymbol('Mix_GetChunkDecoder');
  Pointer(Mix_HasChunkDecoder) := GetSymbol('Mix_HasChunkDecoder');
  Pointer(Mix_GetNumMusicDecoders) := GetSymbol('Mix_GetNumMusicDecoders');
  Pointer(Mix_GetMusicDecoder) := GetSymbol('Mix_GetMusicDecoder');
  Pointer(Mix_HasMusicDecoder) := GetSymbol('Mix_HasMusicDecoder');
  Pointer(Mix_GetMusicType) := GetSymbol('Mix_GetMusicType');
  Pointer(Mix_GetMusicTitle) := GetSymbol('Mix_GetMusicTitle');
  Pointer(Mix_GetMusicTitleTag) := GetSymbol('Mix_GetMusicTitleTag');
  Pointer(Mix_GetMusicArtistTag) := GetSymbol('Mix_GetMusicArtistTag');
  Pointer(Mix_GetMusicAlbumTag) := GetSymbol('Mix_GetMusicAlbumTag');
  Pointer(Mix_GetMusicCopyrightTag) := GetSymbol('Mix_GetMusicCopyrightTag');
  Pointer(Mix_SetPostMix) := GetSymbol('Mix_SetPostMix');
  Pointer(Mix_HookMusic) := GetSymbol('Mix_HookMusic');
  Pointer(Mix_HookMusicFinished) := GetSymbol('Mix_HookMusicFinished');
  Pointer(Mix_GetMusicHookData) := GetSymbol('Mix_GetMusicHookData');
  Pointer(Mix_ChannelFinished) := GetSymbol('Mix_ChannelFinished');
  Pointer(Mix_RegisterEffect) := GetSymbol('Mix_RegisterEffect');
  Pointer(Mix_UnregisterEffect) := GetSymbol('Mix_UnregisterEffect');
  Pointer(Mix_UnregisterAllEffects) := GetSymbol('Mix_UnregisterAllEffects');
  Pointer(Mix_SetPanning) := GetSymbol('Mix_SetPanning');
  Pointer(Mix_SetPosition) := GetSymbol('Mix_SetPosition');
  Pointer(Mix_SetDistance) := GetSymbol('Mix_SetDistance');
  Pointer(Mix_SetReverseStereo) := GetSymbol('Mix_SetReverseStereo');

  Pointer(Mix_ReserveChannels) := GetSymbol('Mix_ReserveChannels');
  Pointer(Mix_GroupChannel) := GetSymbol('Mix_GroupChannel');
  Pointer(Mix_GroupChannels) := GetSymbol('Mix_GroupChannels');
  Pointer(Mix_GroupAvailable) := GetSymbol('Mix_GroupAvailable');
  Pointer(Mix_GroupCount) := GetSymbol('Mix_GroupCount');
  Pointer(Mix_GroupOldest) := GetSymbol('Mix_GroupOldest');
  Pointer(Mix_GroupNewer) := GetSymbol('Mix_GroupNewer');
  Pointer(Mix_PlayChannel) := GetSymbol('Mix_PlayChannel');
  Pointer(Mix_PlayChannelTimed) := GetSymbol('Mix_PlayChannelTimed');
  Pointer(Mix_PlayMusic) := GetSymbol('Mix_PlayMusic');
  Pointer(Mix_FadeInMusic) := GetSymbol('Mix_FadeInMusic');
  Pointer(Mix_FadeInMusicPos) := GetSymbol('Mix_FadeInMusicPos');
  Pointer(Mix_FadeInChannel) := GetSymbol('Mix_FadeInChannel');
  Pointer(Mix_FadeInChannelTimed) := GetSymbol('Mix_FadeInChannelTimed');
  Pointer(Mix_Volume) := GetSymbol('Mix_Volume');
  Pointer(Mix_VolumeChunk) := GetSymbol('Mix_VolumeChunk');
  Pointer(Mix_VolumeMusic) := GetSymbol('Mix_VolumeMusic');
  Pointer(Mix_GetMusicVolume) := GetSymbol('Mix_GetMusicVolume');
  Pointer(Mix_MasterVolume) := GetSymbol('Mix_MasterVolume');
  Pointer(Mix_HaltChannel) := GetSymbol('Mix_HaltChannel');
  Pointer(Mix_HaltGroup) := GetSymbol('Mix_HaltGroup');
  Pointer(Mix_HaltMusic) := GetSymbol('Mix_HaltMusic');
  Pointer(Mix_ExpireChannel) := GetSymbol('Mix_ExpireChannel');
  Pointer(Mix_FadeOutChannel) := GetSymbol('Mix_FadeOutChannel');
  Pointer(Mix_FadeOutGroup) := GetSymbol('Mix_FadeOutGroup');
  Pointer(Mix_FadeOutMusic) := GetSymbol('Mix_FadeOutMusic');
  Pointer(Mix_FadingMusic) := GetSymbol('Mix_FadingMusic');
  Pointer(Mix_FadingChannel) := GetSymbol('Mix_FadingChannel');
  Pointer(Mix_Pause) := GetSymbol('Mix_Pause');
  Pointer(Mix_Resume) := GetSymbol('Mix_Resume');
  Pointer(Mix_Paused) := GetSymbol('Mix_Paused');
  Pointer(Mix_PauseMusic) := GetSymbol('Mix_PauseMusic');
  Pointer(Mix_ResumeMusic) := GetSymbol('Mix_ResumeMusic');
  Pointer(Mix_RewindMusic) := GetSymbol('Mix_RewindMusic');
  Pointer(Mix_PausedMusic) := GetSymbol('Mix_PausedMusic');
  Pointer(Mix_ModMusicJumpToOrder) := GetSymbol('Mix_ModMusicJumpToOrder');
// (2.8.0)  Pointer(Mix_StartTrack) := GetSymbol('Mix_StartTrack');
// (2.8.0)  Pointer(Mix_GetNumTracks) := GetSymbol('Mix_GetNumTracks');
  Pointer(Mix_SetMusicPosition) := GetSymbol('Mix_SetMusicPosition');
  Pointer(Mix_GetMusicPosition) := GetSymbol('Mix_GetMusicPosition');
  Pointer(Mix_MusicDuration) := GetSymbol('Mix_MusicDuration');
  Pointer(Mix_GetMusicLoopStartTime) := GetSymbol('Mix_GetMusicLoopStartTime');
  Pointer(Mix_GetMusicLoopEndTime) := GetSymbol('Mix_GetMusicLoopEndTime');
  Pointer(Mix_GetMusicLoopLengthTime) := GetSymbol('Mix_GetMusicLoopLengthTime');
  Pointer(Mix_Playing) := GetSymbol('Mix_Playing');
  Pointer(Mix_PlayingMusic) := GetSymbol('Mix_PlayingMusic');
  Pointer(Mix_SetMusicCMD) := GetSymbol('Mix_SetMusicCMD');
  Pointer(Mix_SetSynchroValue) := GetSymbol('Mix_SetSynchroValue');
  Pointer(Mix_GetSynchroValue) := GetSymbol('Mix_GetSynchroValue');
  Pointer(Mix_SetSoundFonts) := GetSymbol('Mix_SetSoundFonts');
  Pointer(Mix_GetSoundFonts) := GetSymbol('Mix_GetSoundFonts');
  Pointer(Mix_EachSoundFont) := GetSymbol('Mix_EachSoundFont');
  Pointer(Mix_SetTimidityCfg) := GetSymbol('Mix_SetTimidityCfg');
  Pointer(Mix_GetTimidityCfg) := GetSymbol('Mix_GetTimidityCfg');
  Pointer(Mix_GetChunk) := GetSymbol('Mix_GetChunk');
  Pointer(Mix_CloseAudio) := GetSymbol('Mix_CloseAudio');

  Exit( True );
end;

finalization
  if SDL2_mixer <> nil then FreeAndNil( SDL2_mixer );

end.

