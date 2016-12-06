type
  PMusicCMD = ^TMusicCMD;
  TMusicCMD = record
    filename : array[ 0..PATH_MAX - 1 ] of char;
    cmd : array[ 0..PATH_MAX - 1 ] of char;
    pid : TSYS_ThreadHandle;
  end;

  PWAVStream = ^TWAVStream;
  TWAVStream = record
    wavefp : Pointer;
    start : longint;
    stop : longint;
    cvt : TSDL_AudioCVT;
  end;

  PMidiEvent = ^TMidiEvent;
  TMidiEvent = record
    time : Longint;
    channel : uint8;
    type_ : uint8;
    a : uint8;
    b : uint8;
  end;

  PMidiSong = ^TMidiSong;
  TMidiSong = record
    samples : Longint;
    events : PMidiEvent;
  end;

  POGG_Music = ^TOGG_Music;
  TOGG_Music = record
    playing : integer;
    volume : integer;
    //vf: OggVorbis_File;
    section : integer;
    cvt : TSDL_AudioCVT;
    len_available : integer;
    snd_available : PUint8;
  end;

  PMODULE = ^TMODULE;
  TMODULE = record end;

  PUNIMOD = ^TUNIMOD;
  TUNIMOD = TMODULE;

  TErrorEnum = (
    MMERR_OPENING_FILE,
    MMERR_OUT_OF_MEMORY,
    MMERR_DYNAMIC_LINKING,
    MMERR_SAMPLE_TOO_BIG,
    MMERR_OUT_OF_HANDLES,
    MMERR_UNKNOWN_WAVE_TYPE,
    MMERR_LOADING_PATTERN,
    MMERR_LOADING_TRACK,
    MMERR_LOADING_HEADER,
    MMERR_LOADING_SAMPLEINFO,
    MMERR_NOT_A_MODULE,
    MMERR_NOT_A_STREAM,
    MMERR_MED_SYNTHSAMPLES,
    MMERR_ITPACK_INVALID_DATA,
    MMERR_DETECTING_DEVICE,
    MMERR_INVALID_DEVICE,
    MMERR_INITIALIZING_MIXER,
    MMERR_OPENING_AUDIO,
    MMERR_8BIT_ONLY,
    MMERR_16BIT_ONLY,
    MMERR_STEREO_ONLY,
    MMERR_ULAW,
    MMERR_NON_BLOCK,
    MMERR_AF_AUDIO_PORT,
    MMERR_AIX_CONFIG_INIT,
    MMERR_AIX_CONFIG_CONTROL,
    MMERR_AIX_CONFIG_START,
    MMERR_GUS_SETTINGS,
    MMERR_GUS_RESET,
    MMERR_GUS_TIMER,
    MMERR_HP_SETSAMPLESIZE,
    MMERR_HP_SETSPEED,
    MMERR_HP_CHANNELS,
    MMERR_HP_AUDIO_OUTPUT,
    MMERR_HP_AUDIO_DESC,
    MMERR_HP_BUFFERSIZE,
    MMERR_OSS_SETFRAGMENT,
    MMERR_OSS_SETSAMPLESIZE,
    MMERR_OSS_SETSTEREO,
    MMERR_OSS_SETSPEED,
    MMERR_SGI_SPEED,
    MMERR_SGI_16BIT,
    MMERR_SGI_8BIT,
    MMERR_SGI_STEREO,
    MMERR_SGI_MONO,
    MMERR_SUN_INIT,
    MMERR_OS2_MIXSETUP,
    MMERR_OS2_SEMAPHORE,
    MMERR_OS2_TIMER,
    MMERR_OS2_THREAD,
    MMERR_DS_PRIORITY,
    MMERR_DS_BUFFER,
    MMERR_DS_FORMAT,
    MMERR_DS_NOTIFY,
    MMERR_DS_EVENT,
    MMERR_DS_THREAD,
    MMERR_DS_UPDATE,
    MMERR_WINMM_HANDLE,
    MMERR_WINMM_ALLOCATED,
    MMERR_WINMM_DEVICEID,
    MMERR_WINMM_FORMAT,
    MMERR_WINMM_UNKNOWN,
    MMERR_MAC_SPEED,
    MMERR_MAC_START,
    MMERR_MAX
 );

  PMix_Chunk = ^TMix_Chunk;
  TMix_Chunk = record
    allocated : integer;
    abuf : PUint8;
    alen : Uint32;
    volume : Uint8;
  end;
  Mix_Chunk = TMix_Chunk;

  TMix_Fading = (
    MIX_NO_FADING,
    MIX_FADING_OUT,
    MIX_FADING_IN
    );
  Mix_Fading = TMix_Fading;

  TMix_MusicType = (
    MUS_NONE,
    MUS_CMD,
    MUS_WAV,
    MUS_MOD,
    MUS_MID,
    MUS_OGG,
    MUS_MP3
  );
  Mix_MusicType = TMix_MusicType;

  TMusicUnion = record
    case Byte of
      0 : ( cmd : PMusicCMD );
      1 : ( wave : PWAVStream );
      2 : ( module : PUNIMOD );
      3 : ( midi : TMidiSong );
      4 : ( ogg : POGG_music );
  end;

  P_Mix_Music = ^T_Mix_Music;
  T_Mix_Music = record
    type_ : TMix_MusicType;
    data : TMusicUnion;
    fading : TMix_Fading;
    fade_volume : integer;
    fade_step : integer;
    fade_steps : integer;
    error : integer;
  end;

  PMix_Music = ^TMix_Music;
  TMix_Music = T_Mix_Music;

  TMixFunction = function( udata : Pointer; stream : PUint8; len : integer ) : Pointer; cdecl;

  TChannel_finished = procedure( channel: Integer ); cdecl;
  TMix_EffectFunc = function( chan : integer; stream : Pointer; len : integer; udata : Pointer ) : Pointer; cdecl;
  TMix_EffectDone = function( chan : integer; udata : Pointer ) : Pointer; cdecl;

