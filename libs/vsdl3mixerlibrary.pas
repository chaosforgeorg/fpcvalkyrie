{$include ../src/valkyrie.inc}
unit vsdl3mixerlibrary;
{$PACKRECORDS C}
{$MINENUMSIZE 4}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary, vsdl3library,
{$IFDEF WINDOWS}
  Windows;
{$ENDIF}
{$IFDEF UNIX}
  pthreads,
  baseunix,
  {$IFDEF DARWIN}
     unix;
  {$ELSE}
     unix, x, xlib;
  {$ENDIF}
{$ENDIF}

const
{$IFDEF WINDOWS}
  SDL3MixerDefaultPath = 'SDL3_mixer.dll';
{$ELSE}
  {$IFDEF DARWIN}
    SDL3MixerDefaultPath = 'SDL3_mixer.framework/SDL3_mixer';
    {$linklib SDLmain}
    {$linkframework Cocoa}
    {$linkframework SDL}
    {$PASCALMAINNAME SDL_main}
  {$ELSE}
  SDL3MixerDefaultPath = 'libSDL3_mixer-3.0.so.0';
  {$ENDIF}
{$ENDIF}

// missing from vsdl3
type
  PSDL_AudioSpec    = Pointer;
  PSDL_AudioStream  = Pointer;
  SDL_AudioDeviceID = Uint32;


const

  SDL_MIXER_MAJOR_VERSION = 3;
  SDL_MIXER_MINOR_VERSION = 1;
  SDL_MIXER_PATCHLEVEL    = 0;



type
  PMIX_Mixer        = Pointer;
  PMIX_Audio        = Pointer;
  PMIX_Track        = Pointer;
  PMIX_Group        = Pointer;
  PMIX_AudioDecoder = Pointer;

  MIX_StereoGains = record
    left : Single;
    right: Single;
  end;

  MIX_Point3D = record
    x: Single;
    y: Single;
    z: Single;
  end;

  PMIX_StereoGains = ^MIX_StereoGains;
  PMIX_Point3D     = ^MIX_Point3D;


  MIX_TrackStoppedCallback = procedure(userdata: Pointer; track: PMIX_Track); cdecl;
  MIX_TrackMixCallback     = procedure(userdata: Pointer; track: PMIX_Track; const spec: PSDL_AudioSpec; pcm: PSingle; samples: Integer); cdecl;
  MIX_GroupMixCallback     = procedure(userdata: Pointer; group: PMIX_Group; const spec: PSDL_AudioSpec; pcm: PSingle; samples: Integer); cdecl;
  MIX_PostMixCallback      = procedure(userdata: Pointer; mixer: PMIX_Mixer; const spec: PSDL_AudioSpec; pcm: PSingle; samples: Integer); cdecl;

var
  MIX_Version                  : function: Integer; cdecl;

  MIX_Init                     : function: Boolean; cdecl;
  MIX_Quit                     : procedure; cdecl;

  MIX_GetNumAudioDecoders      : function: Integer; cdecl;
  MIX_GetAudioDecoder          : function(index: Integer): PAnsiChar; cdecl;

  MIX_CreateMixerDevice        : function(devid: SDL_AudioDeviceID; const spec: PSDL_AudioSpec): PMIX_Mixer; cdecl;
  MIX_CreateMixer              : function(const spec: PSDL_AudioSpec): PMIX_Mixer; cdecl;
  MIX_DestroyMixer             : procedure(mixer: PMIX_Mixer); cdecl;

  MIX_GetMixerProperties       : function(mixer: PMIX_Mixer): SDL_PropertiesID; cdecl;
  MIX_GetMixerFormat           : function(mixer: PMIX_Mixer; spec: PSDL_AudioSpec): Boolean; cdecl;

  MIX_LoadAudio_IO             : function(mixer: PMIX_Mixer; io: PSDL_IOStream; predecode, closeio: Boolean): PMIX_Audio; cdecl;
  MIX_LoadAudio                : function(mixer: PMIX_Mixer; const path: PAnsiChar; predecode: Boolean): PMIX_Audio; cdecl;
  MIX_LoadAudioWithProperties  : function(props: SDL_PropertiesID): PMIX_Audio; cdecl;

  MIX_LoadRawAudio_IO          : function(mixer: PMIX_Mixer; io: PSDL_IOStream; const spec: PSDL_AudioSpec; closeio: Boolean): PMIX_Audio; cdecl;
  MIX_LoadRawAudio             : function(mixer: PMIX_Mixer; const data: Pointer; datalen: NativeUInt; const spec: PSDL_AudioSpec): PMIX_Audio; cdecl;
  MIX_LoadRawAudioNoCopy       : function(mixer: PMIX_Mixer; const data: Pointer; datalen: NativeUInt; const spec: PSDL_AudioSpec; free_when_done: Boolean): PMIX_Audio; cdecl;

  MIX_CreateSineWaveAudio      : function(mixer: PMIX_Mixer; hz: Integer; amplitude: Single): PMIX_Audio; cdecl;

  MIX_GetAudioProperties       : function(audio: PMIX_Audio): SDL_PropertiesID; cdecl;
  MIX_GetAudioDuration         : function(audio: PMIX_Audio): Int64; cdecl;
  MIX_GetAudioFormat           : function(audio: PMIX_Audio; spec: PSDL_AudioSpec): Boolean; cdecl;
  MIX_DestroyAudio             : procedure(audio: PMIX_Audio); cdecl;

  MIX_CreateTrack              : function(mixer: PMIX_Mixer): PMIX_Track; cdecl;
  MIX_DestroyTrack             : procedure(track: PMIX_Track); cdecl;
  MIX_GetTrackProperties       : function(track: PMIX_Track): SDL_PropertiesID; cdecl;
  MIX_GetTrackMixer            : function(track: PMIX_Track): PMIX_Mixer; cdecl;

  MIX_SetTrackAudio            : function(track: PMIX_Track; audio: PMIX_Audio): Boolean; cdecl;
  MIX_SetTrackAudioStream      : function(track: PMIX_Track; stream: PSDL_AudioStream): Boolean; cdecl;
  MIX_SetTrackIOStream         : function(track: PMIX_Track; io: PSDL_IOStream; closeio: Boolean): Boolean; cdecl;

  MIX_TagTrack                 : function(track: PMIX_Track; const tag: PAnsiChar): Boolean; cdecl;
  MIX_UntagTrack               : procedure(track: PMIX_Track; const tag: PAnsiChar); cdecl;

  MIX_SetTrackPlaybackPosition : function(track: PMIX_Track; frames: Int64): Boolean; cdecl;
  MIX_GetTrackPlaybackPosition : function(track: PMIX_Track): Int64; cdecl;
  MIX_TrackLooping             : function(track: PMIX_Track): Boolean; cdecl;
  MIX_GetTrackAudio            : function(track: PMIX_Track): PMIX_Audio; cdecl;
  MIX_GetTrackAudioStream      : function(track: PMIX_Track): PSDL_AudioStream; cdecl;
  MIX_GetTrackRemaining        : function(track: PMIX_Track): Int64; cdecl;

  MIX_TrackMSToFrames          : function(track: PMIX_Track; ms: Int64): Int64; cdecl;
  MIX_TrackFramesToMS          : function(track: PMIX_Track; frames: Int64): Int64; cdecl;
  MIX_AudioMSToFrames          : function(audio: PMIX_Audio; ms: Int64): Int64; cdecl;
  MIX_AudioFramesToMS          : function(audio: PMIX_Audio; frames: Int64): Int64; cdecl;
  MIX_MSToFrames               : function(sample_rate: Integer; ms: Int64): Int64; cdecl;
  MIX_FramesToMS               : function(sample_rate: Integer; frames: Int64): Int64; cdecl;

  MIX_PlayTrack                : function(track: PMIX_Track; options: SDL_PropertiesID): Boolean; cdecl;
  MIX_PlayTag                  : function(mixer: PMIX_Mixer; const tag: PAnsiChar; options: SDL_PropertiesID): Boolean; cdecl;
  MIX_PlayAudio                : function(mixer: PMIX_Mixer; audio: PMIX_Audio): Boolean; cdecl;

  MIX_StopTrack                : function(track: PMIX_Track; fade_out_frames: Int64): Boolean; cdecl;
  MIX_StopAllTracks            : function(mixer: PMIX_Mixer; fade_out_ms: Int64): Boolean; cdecl;
  MIX_StopTag                  : function(mixer: PMIX_Mixer; const tag: PAnsiChar; fade_out_ms: Int64): Boolean; cdecl;

  MIX_PauseTrack               : function(track: PMIX_Track): Boolean; cdecl;
  MIX_PauseAllTracks           : function(mixer: PMIX_Mixer): Boolean; cdecl;
  MIX_PauseTag                 : function(mixer: PMIX_Mixer; const tag: PAnsiChar): Boolean; cdecl;

  MIX_ResumeTrack              : function(track: PMIX_Track): Boolean; cdecl;
  MIX_ResumeAllTracks          : function(mixer: PMIX_Mixer): Boolean; cdecl;
  MIX_ResumeTag                : function(mixer: PMIX_Mixer; const tag: PAnsiChar): Boolean; cdecl;

  MIX_TrackPlaying             : function(track: PMIX_Track): Boolean; cdecl;
  MIX_TrackPaused              : function(track: PMIX_Track): Boolean; cdecl;

  MIX_SetMasterGain            : function(mixer: PMIX_Mixer; gain: Single): Boolean; cdecl;
  MIX_GetMasterGain            : function(mixer: PMIX_Mixer): Single; cdecl;

  MIX_SetTrackGain             : function(track: PMIX_Track; gain: Single): Boolean; cdecl;
  MIX_GetTrackGain             : function(track: PMIX_Track): Single; cdecl;
  MIX_SetTagGain               : function(mixer: PMIX_Mixer; const tag: PAnsiChar; gain: Single): Boolean; cdecl;

  MIX_SetTrackFrequencyRatio   : function(track: PMIX_Track; ratio: Single): Boolean; cdecl;
  MIX_GetTrackFrequencyRatio   : function(track: PMIX_Track): Single; cdecl;

  MIX_SetTrackOutputChannelMap : function(track: PMIX_Track; const chmap: PInteger; count: Integer): Boolean; cdecl;

  MIX_SetTrackStereo           : function(track: PMIX_Track; const gains: PMIX_StereoGains): Boolean; cdecl;

  MIX_SetTrack3DPosition       : function(track: PMIX_Track; const position: PMIX_Point3D): Boolean; cdecl;
  MIX_GetTrack3DPosition       : function(track: PMIX_Track; position: PMIX_Point3D): Boolean; cdecl;

  MIX_CreateGroup              : function(mixer: PMIX_Mixer): PMIX_Group; cdecl;
  MIX_DestroyGroup             : procedure(group: PMIX_Group); cdecl;
  MIX_GetGroupProperties       : function(group: PMIX_Group): SDL_PropertiesID; cdecl;
  MIX_GetGroupMixer            : function(group: PMIX_Group): PMIX_Mixer; cdecl;
  MIX_SetTrackGroup            : function(track: PMIX_Track; group: PMIX_Group): Boolean; cdecl;

  MIX_SetTrackStoppedCallback  : function(track: PMIX_Track; cb: MIX_TrackStoppedCallback; userdata: Pointer): Boolean; cdecl;
  MIX_SetTrackRawCallback      : function(track: PMIX_Track; cb: MIX_TrackMixCallback; userdata: Pointer): Boolean; cdecl;
  MIX_SetTrackCookedCallback   : function(track: PMIX_Track; cb: MIX_TrackMixCallback; userdata: Pointer): Boolean; cdecl;

  MIX_SetGroupPostMixCallback  : function(group: PMIX_Group; cb: MIX_GroupMixCallback; userdata: Pointer): Boolean; cdecl;
  MIX_SetPostMixCallback       : function(mixer: PMIX_Mixer; cb: MIX_PostMixCallback; userdata: Pointer): Boolean; cdecl;

  MIX_Generate                 : function(mixer: PMIX_Mixer; buffer: Pointer; buflen: Integer): Boolean; cdecl;

  MIX_CreateAudioDecoder       : function(const path: PAnsiChar; props: SDL_PropertiesID): PMIX_AudioDecoder; cdecl;
  MIX_CreateAudioDecoder_IO    : function(io: PSDL_IOStream; closeio: Boolean; props: SDL_PropertiesID): PMIX_AudioDecoder; cdecl;
  MIX_DestroyAudioDecoder      : procedure(audiodecoder: PMIX_AudioDecoder); cdecl;
  MIX_GetAudioDecoderProperties: function(audiodecoder: PMIX_AudioDecoder): SDL_PropertiesID; cdecl;
  MIX_GetAudioDecoderFormat    : function(audiodecoder: PMIX_AudioDecoder; spec: PSDL_AudioSpec): Boolean; cdecl;
  MIX_DecodeAudio              : function(audiodecoder: PMIX_AudioDecoder; buffer: Pointer; buflen: Integer; const spec: PSDL_AudioSpec): Integer; cdecl;


var
  SDL3_mixer : TLibrary = nil;

function LoadSDL3Mixer( const aPath : AnsiString = SDL3MixerDefaultPath ) : Boolean;

implementation

function LoadSDL3Mixer( const aPath : AnsiString = SDL3MixerDefaultPath ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL3_mixer.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL3_mixer : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL3_mixer <> nil then Exit( True );
  SDL3_mixer := TLibrary.Load( aPath );
  if SDL3_mixer = nil then Exit( False );

  Pointer(MIX_Version)                  := GetSymbol('MIX_Version');
  Pointer(MIX_Init)                     := GetSymbol('MIX_Init');
  Pointer(MIX_Quit)                     := GetSymbol('MIX_Quit');
  Pointer(MIX_GetNumAudioDecoders)      := GetSymbol('MIX_GetNumAudioDecoders');
  Pointer(MIX_GetAudioDecoder)          := GetSymbol('MIX_GetAudioDecoder');
  Pointer(MIX_CreateMixerDevice)        := GetSymbol('MIX_CreateMixerDevice');
  Pointer(MIX_CreateMixer)              := GetSymbol('MIX_CreateMixer');
  Pointer(MIX_DestroyMixer)             := GetSymbol('MIX_DestroyMixer');
  Pointer(MIX_GetMixerProperties)       := GetSymbol('MIX_GetMixerProperties');
  Pointer(MIX_GetMixerFormat)           := GetSymbol('MIX_GetMixerFormat');
  Pointer(MIX_LoadAudio_IO)             := GetSymbol('MIX_LoadAudio_IO');
  Pointer(MIX_LoadAudio)                := GetSymbol('MIX_LoadAudio');
  Pointer(MIX_LoadAudioWithProperties)  := GetSymbol('MIX_LoadAudioWithProperties');
  Pointer(MIX_LoadRawAudio_IO)          := GetSymbol('MIX_LoadRawAudio_IO');
  Pointer(MIX_LoadRawAudio)             := GetSymbol('MIX_LoadRawAudio');
  Pointer(MIX_LoadRawAudioNoCopy)       := GetSymbol('MIX_LoadRawAudioNoCopy');
  Pointer(MIX_CreateSineWaveAudio)      := GetSymbol('MIX_CreateSineWaveAudio');
  Pointer(MIX_GetAudioProperties)       := GetSymbol('MIX_GetAudioProperties');
  Pointer(MIX_GetAudioDuration)         := GetSymbol('MIX_GetAudioDuration');
  Pointer(MIX_GetAudioFormat)           := GetSymbol('MIX_GetAudioFormat');
  Pointer(MIX_DestroyAudio)             := GetSymbol('MIX_DestroyAudio');
  Pointer(MIX_CreateTrack)              := GetSymbol('MIX_CreateTrack');
  Pointer(MIX_DestroyTrack)             := GetSymbol('MIX_DestroyTrack');
  Pointer(MIX_GetTrackProperties)       := GetSymbol('MIX_GetTrackProperties');
  Pointer(MIX_GetTrackMixer)            := GetSymbol('MIX_GetTrackMixer');
  Pointer(MIX_SetTrackAudio)            := GetSymbol('MIX_SetTrackAudio');
  Pointer(MIX_SetTrackAudioStream)      := GetSymbol('MIX_SetTrackAudioStream');
  Pointer(MIX_SetTrackIOStream)         := GetSymbol('MIX_SetTrackIOStream');
  Pointer(MIX_TagTrack)                 := GetSymbol('MIX_TagTrack');
  Pointer(MIX_UntagTrack)               := GetSymbol('MIX_UntagTrack');
  Pointer(MIX_SetTrackPlaybackPosition) := GetSymbol('MIX_SetTrackPlaybackPosition');
  Pointer(MIX_GetTrackPlaybackPosition) := GetSymbol('MIX_GetTrackPlaybackPosition');
  Pointer(MIX_TrackLooping)             := GetSymbol('MIX_TrackLooping');
  Pointer(MIX_GetTrackAudio)            := GetSymbol('MIX_GetTrackAudio');
  Pointer(MIX_GetTrackAudioStream)      := GetSymbol('MIX_GetTrackAudioStream');
  Pointer(MIX_GetTrackRemaining)        := GetSymbol('MIX_GetTrackRemaining');
  Pointer(MIX_TrackMSToFrames)          := GetSymbol('MIX_TrackMSToFrames');
  Pointer(MIX_TrackFramesToMS)          := GetSymbol('MIX_TrackFramesToMS');
  Pointer(MIX_AudioMSToFrames)          := GetSymbol('MIX_AudioMSToFrames');
  Pointer(MIX_AudioFramesToMS)          := GetSymbol('MIX_AudioFramesToMS');
  Pointer(MIX_MSToFrames)               := GetSymbol('MIX_MSToFrames');
  Pointer(MIX_FramesToMS)               := GetSymbol('MIX_FramesToMS');
  Pointer(MIX_PlayTrack)                := GetSymbol('MIX_PlayTrack');
  Pointer(MIX_PlayTag)                  := GetSymbol('MIX_PlayTag');
  Pointer(MIX_PlayAudio)                := GetSymbol('MIX_PlayAudio');
  Pointer(MIX_StopTrack)                := GetSymbol('MIX_StopTrack');
  Pointer(MIX_StopAllTracks)            := GetSymbol('MIX_StopAllTracks');
  Pointer(MIX_StopTag)                  := GetSymbol('MIX_StopTag');
  Pointer(MIX_PauseTrack)               := GetSymbol('MIX_PauseTrack');
  Pointer(MIX_PauseAllTracks)           := GetSymbol('MIX_PauseAllTracks');
  Pointer(MIX_PauseTag)                 := GetSymbol('MIX_PauseTag');
  Pointer(MIX_ResumeTrack)              := GetSymbol('MIX_ResumeTrack');
  Pointer(MIX_ResumeAllTracks)          := GetSymbol('MIX_ResumeAllTracks');
  Pointer(MIX_ResumeTag)                := GetSymbol('MIX_ResumeTag');
  Pointer(MIX_TrackPlaying)             := GetSymbol('MIX_TrackPlaying');
  Pointer(MIX_TrackPaused)              := GetSymbol('MIX_TrackPaused');
  Pointer(MIX_SetMasterGain)            := GetSymbol('MIX_SetMasterGain');
  Pointer(MIX_GetMasterGain)            := GetSymbol('MIX_GetMasterGain');
  Pointer(MIX_SetTrackGain)             := GetSymbol('MIX_SetTrackGain');
  Pointer(MIX_GetTrackGain)             := GetSymbol('MIX_GetTrackGain');
  Pointer(MIX_SetTagGain)               := GetSymbol('MIX_SetTagGain');
  Pointer(MIX_SetTrackFrequencyRatio)   := GetSymbol('MIX_SetTrackFrequencyRatio');
  Pointer(MIX_GetTrackFrequencyRatio)   := GetSymbol('MIX_GetTrackFrequencyRatio');
  Pointer(MIX_SetTrackOutputChannelMap) := GetSymbol('MIX_SetTrackOutputChannelMap');
  Pointer(MIX_SetTrackStereo)           := GetSymbol('MIX_SetTrackStereo');
  Pointer(MIX_SetTrack3DPosition)       := GetSymbol('MIX_SetTrack3DPosition');
  Pointer(MIX_GetTrack3DPosition)       := GetSymbol('MIX_GetTrack3DPosition');
  Pointer(MIX_CreateGroup)              := GetSymbol('MIX_CreateGroup');
  Pointer(MIX_DestroyGroup)             := GetSymbol('MIX_DestroyGroup');
  Pointer(MIX_GetGroupProperties)       := GetSymbol('MIX_GetGroupProperties');
  Pointer(MIX_GetGroupMixer)            := GetSymbol('MIX_GetGroupMixer');
  Pointer(MIX_SetTrackGroup)            := GetSymbol('MIX_SetTrackGroup');
  Pointer(MIX_SetTrackStoppedCallback)  := GetSymbol('MIX_SetTrackStoppedCallback');
  Pointer(MIX_SetTrackRawCallback)      := GetSymbol('MIX_SetTrackRawCallback');
  Pointer(MIX_SetTrackCookedCallback)   := GetSymbol('MIX_SetTrackCookedCallback');
  Pointer(MIX_SetGroupPostMixCallback)  := GetSymbol('MIX_SetGroupPostMixCallback');
  Pointer(MIX_SetPostMixCallback)       := GetSymbol('MIX_SetPostMixCallback');
  Pointer(MIX_Generate)                 := GetSymbol('MIX_Generate');
  Pointer(MIX_CreateAudioDecoder)       := GetSymbol('MIX_CreateAudioDecoder');
  Pointer(MIX_CreateAudioDecoder_IO)    := GetSymbol('MIX_CreateAudioDecoder_IO');
  Pointer(MIX_DestroyAudioDecoder)      := GetSymbol('MIX_DestroyAudioDecoder');
  Pointer(MIX_GetAudioDecoderProperties):= GetSymbol('MIX_GetAudioDecoderProperties');
  Pointer(MIX_GetAudioDecoderFormat)    := GetSymbol('MIX_GetAudioDecoderFormat');
  Pointer(MIX_DecodeAudio)              := GetSymbol('MIX_DecodeAudio');

  Exit( True );
end;

end.
