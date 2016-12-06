type
  PSingle = ^Single;
  THandle = Cardinal;

type
  PFSoundSample = Pointer;
  PFSoundStream = Pointer;
  PFSoundDSPUnit = Pointer;
  PFMusicModule = Pointer;
  PFSyncPoint = Pointer;

  PFSoundVector = ^TFSoundVector;
  TFSoundVector = record
    x: Single;
    y: Single;
    z: Single;
  end;

  TFSoundStreamCallback   = function (Stream: PFSoundStream; Buff: Pointer; Length, Param: Integer): ByteBool; stdcall;
  TFSoundDSPCallback      = function (OriginalBuffer: Pointer; NewBuffer: Pointer; Length, Param: Integer): Pointer; stdcall;
  TFMusicCallback         = procedure (Module: PFMusicModule; Param: Byte); stdcall;
  TFSoundOpenCallback     = function (Name: PChar): Cardinal; stdcall;
  TFSoundCloseCallback    = procedure (Handle: Cardinal); stdcall;
  TFSoundReadCallback     = function (Buffer: Pointer; Size: Cardinal; Handle: Cardinal): Cardinal; stdcall;
  TFSoundSeekCallback     = procedure (Handle: Cardinal; Pos: Cardinal; Mode: Byte); stdcall;
  TFSoundTellCallback     = function (Handle: Cardinal): Cardinal; stdcall;
  TFSoundAllocCallback    = function(Size: Cardinal): Pointer; stdcall;
  TFSoundReallocCallback  = function(Ptr: Pointer; Size: Cardinal): Pointer; stdcall;
  TFSoundFreeCallback     = procedure(Ptr: Pointer); stdcall;
  TFMetaDataCallback      = function(Name: PChar; Value: PChar; userdata: Integer): ByteBool; stdcall;

type
  TFModErrors = (
    FMOD_ERR_NONE,
    FMOD_ERR_BUSY,
    FMOD_ERR_UNINITIALIZED,
    FMOD_ERR_INIT,
    FMOD_ERR_ALLOCATED,
    FMOD_ERR_PLAY,
    FMOD_ERR_OUTPUT_FORMAT,
    FMOD_ERR_COOPERATIVELEVEL,
    FMOD_ERR_CREATEBUFFER,
    FMOD_ERR_FILE_NOTFOUND,
    FMOD_ERR_FILE_FORMAT,
    FMOD_ERR_FILE_BAD,
    FMOD_ERR_MEMORY,
    FMOD_ERR_VERSION,
    FMOD_ERR_INVALID_PARAM,
    FMOD_ERR_NO_EAX,
    FMOD_ERR_CHANNEL_ALLOC,
    FMOD_ERR_RECORD,
    FMOD_ERR_MEDIAPLAYER,
    FMOD_ERR_CDDEVICE
  );

type
  TFSoundOutputTypes = (
    FSOUND_OUTPUT_NOSOUND,
    FSOUND_OUTPUT_WINMM,
    FSOUND_OUTPUT_DSOUND,
    FSOUND_OUTPUT_A3D,

    FSOUND_OUTPUT_OSS,
    FSOUND_OUTPUT_ESD,
    FSOUND_OUTPUT_ALSA,

    FSOUND_OUTPUT_ASIO,
    FSOUND_OUTPUT_XBOX,
    FSOUND_OUTPUT_PS2,
    FSOUND_OUTPUT_MAC,
    FSOUND_OUTPUT_GC,

    FSOUND_OUTPUT_NOSOUND_NONREALTIME
  );

type
  TFSoundMixerTypes = (
    FSOUND_MIXER_AUTODETECT,
    FSOUND_MIXER_BLENDMODE,
    FSOUND_MIXER_MMXP5,
    FSOUND_MIXER_MMXP6,

    FSOUND_MIXER_QUALITY_AUTODETECT,
    FSOUND_MIXER_QUALITY_FPU,
    FSOUND_MIXER_QUALITY_MMXP5,
    FSOUND_MIXER_QUALITY_MMXP6,

    FSOUND_MIXER_MONO,
    FSOUND_MIXER_QUALITY_MONO,

    FSOUND_MIXER_MAX
  );

type
  TFMusicTypes = (
    FMUSIC_TYPE_NONE,
    FMUSIC_TYPE_MOD,
    FMUSIC_TYPE_S3M,
    FMUSIC_TYPE_XM,
    FMUSIC_TYPE_IT,
    FMUSIC_TYPE_MIDI,
    FMUSIC_TYPE_FSB
  );

type
  TFSoundReverbProperties = record
    Environment: Cardinal;
    EnvSize: Single;
    EnvDiffusion: Single;
    Room: Integer;
    RoomHF: Integer;
    RoomLF: Integer;
    DecayTime: Single;
    DecayHFRatio: Single;
    DecayLFRatio: Single;
    Reflections: Integer;
    ReflectionsDelay: Single;
    ReflectionsPan: array [0..2] of Single;
    Reverb: Integer;
    ReverbDelay: Single;
    ReverbPan: array [0..2] of Single;
    EchoTime: Single;
    EchoDepth: Single;
    ModulationTime: Single;
    ModulationDepth: Single;
    AirAbsorptionHF: Single;
    HFReference: Single;
    LFReference: Single;
    RoomRolloffFactor: Single;
    Diffusion: Single;
    Density: Single;
    Flags: Cardinal;
  end;

type
  TFSoundReverbChannelProperties = record
    Direct: Integer;
    DirectHF: Integer;
    Room: Integer;
    RoomHF: Integer;
    Obstruction: Integer;
    ObstructionLFRatio: Single;
    Occlusion: Integer;
    OcclusionLFRatio: Single;
    OcclusionRoomRatio: Single;
    OcclusionDirectRatio: Single;
    Exclusion: Integer;
    ExclusionLFRatio: Single;
    OutsideVolumeHF: Integer;
    DopplerFactor: Single;
    RolloffFactor: Single;
    RoomRolloffFactor: Single;
    AirAbsorptionFactor: Single;
    Flags: Integer;
  end;

type
  TFSoundFXModes = (
    FSOUND_FX_CHORUS,
    FSOUND_FX_COMPRESSOR,
    FSOUND_FX_DISTORTION,
    FSOUND_FX_ECHO,
    FSOUND_FX_FLANGER,
    FSOUND_FX_GARGLE,
    FSOUND_FX_I3DL2REVERB,
    FSOUND_FX_PARAMEQ,
    FSOUND_FX_WAVES_REVERB,
    FSOUND_FX_MAX
  );

type
  TFSoundSpeakerModes =
  (
    FSOUND_SPEAKERMODE_DOLBYDIGITAL,
    FSOUND_SPEAKERMODE_HEADPHONES,
    FSOUND_SPEAKERMODE_MONO,
    FSOUND_SPEAKERMODE_QUAD,
    FSOUND_SPEAKERMODE_STEREO,
    FSOUND_SPEAKERMODE_SURROUND,
    FSOUND_SPEAKERMODE_DTS
  );
  FSOUND_SPEAKERMODES = TFSoundSpeakerModes;

type
  TFSoundStreamNetStatus =
  (
    FSOUND_STREAM_NET_NOTCONNECTED,
    FSOUND_STREAM_NET_CONNECTING,
    FSOUND_STREAM_NET_BUFFERING,
    FSOUND_STREAM_NET_READY,
    FSOUND_STREAM_NET_ERROR
  );

type
  TFSoundTagFieldType =
  (
    FSOUND_TAGFIELD_VORBISCOMMENT,
    FSOUND_TAGFIELD_ID3V1,
    FSOUND_TAGFIELD_ID3V2,
    FSOUND_TAGFIELD_SHOUTCAST,
    FSOUND_TAGFIELD_ICECAST,
    FSOUND_TAGFIELD_ASF
  );

type
  TFSoundTOCTag = record
    Name: array [0..3] of Char;
    NumTracks: Integer;
    Min: array [0..99] of Integer;
    Sec: array [0..99] of Integer;
    Frame: array [0..99] of Integer;
  end;

