type
  FMOD_BOOL = LongInt;

  PFMOD_SYSTEM = ^FMOD_SYSTEM;
  FMOD_SYSTEM = record
  end;
  PPFMOD_SYSTEM = ^PFMOD_SYSTEM;

  PFMOD_SOUND = ^FMOD_SOUND;
  FMOD_SOUND = record
  end;
  PPFMOD_SOUND = ^PFMOD_SOUND;

  PFMOD_CHANNELCONTROL = ^FMOD_CHANNELCONTROL;
  FMOD_CHANNELCONTROL = record
  end;

  PFMOD_CHANNEL = ^FMOD_CHANNEL;
  FMOD_CHANNEL = record
  end;
  PPFMOD_CHANNEL = ^PFMOD_CHANNEL;

  PFMOD_CHANNELGROUP = ^FMOD_CHANNELGROUP;
  FMOD_CHANNELGROUP = record
  end;
  PPFMOD_CHANNELGROUP = ^PFMOD_CHANNELGROUP;

  PFMOD_SOUNDGROUP = ^FMOD_SOUNDGROUP;
  FMOD_SOUNDGROUP = record
  end;
  PPFMOD_SOUNDGROUP = ^PFMOD_SOUNDGROUP;

  PFMOD_REVERB3D = ^FMOD_REVERB3D;
  FMOD_REVERB3D = record
  end;

  PFMOD_DSP = ^FMOD_DSP;
  FMOD_DSP = record
  end;

  PFMOD_DSPCONNECTION = ^FMOD_DSPCONNECTION;
  FMOD_DSPCONNECTION = record
  end;

  PFMOD_POLYGON = ^FMOD_POLYGON;
  FMOD_POLYGON = record
  end;

  PFMOD_GEOMETRY = ^FMOD_GEOMETRY;
  FMOD_GEOMETRY = record
  end;

  PFMOD_SYNCPOINT = ^FMOD_SYNCPOINT;
  FMOD_SYNCPOINT = record
  end;

  PFMOD_ASYNCREADINFO = ^FMOD_ASYNCREADINFO;
  FMOD_ASYNCREADINFO = record
  end;

type
  FMOD_GUID = record
    Data1: LongWord;
    Data2: Word;
    Data3: Word;
    Data4: array[0..7] of Byte;
  end;
  PFMOD_GUID = ^FMOD_GUID;

type
  FMOD_VECTOR = record
    x: Single;
    y: Single;
    z: Single;
  end;
  PFMOD_VECTOR = ^FMOD_VECTOR;

  FMOD_DEBUG_CALLBACK = function(flags: FMOD_DEBUG_FLAGS; const file_: PChar; line: Integer; const func: PChar; const message: PChar): FMOD_RESULT; cdecl;
  FMOD_SYSTEM_CALLBACK = function(system: PFMOD_SYSTEM; type_: FMOD_SYSTEM_CALLBACK_TYPE; commanddata1: Pointer; commanddata2: Pointer; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_CHANNELCONTROL_CALLBACK = function(channelcontrol: PFMOD_CHANNELCONTROL; controltype: FMOD_CHANNELCONTROL_TYPE; callbacktype: FMOD_CHANNELCONTROL_CALLBACK_TYPE; commanddata1: Pointer; commanddata2: Pointer): FMOD_RESULT; cdecl;
  FMOD_DSP_CALLBACK = function(dsp: PFMOD_DSP; type_: FMOD_DSP_CALLBACK_TYPE; data: Pointer): FMOD_RESULT; cdecl;
  FMOD_SOUND_NONBLOCK_CALLBACK = function(sound: PFMOD_SOUND; result: FMOD_RESULT): FMOD_RESULT; cdecl;
  FMOD_SOUND_PCMREAD_CALLBACK = function(sound: PFMOD_SOUND; data: Pointer; datalen: Cardinal): FMOD_RESULT; cdecl;
  FMOD_SOUND_PCMSETPOS_CALLBACK = function(sound: PFMOD_SOUND; subsound: Integer; position: Cardinal; postype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_FILE_OPEN_CALLBACK = function(const name: PChar; filesize: PCardinal; handle: PPointer; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_FILE_CLOSE_CALLBACK = function(handle: Pointer; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_FILE_READ_CALLBACK = function(handle: Pointer; buffer: Pointer; sizebytes: Cardinal; bytesread: PCardinal; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_FILE_SEEK_CALLBACK = function(handle: Pointer; pos: Cardinal; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_FILE_ASYNCREAD_CALLBACK = function(info: PFMOD_ASYNCREADINFO; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_FILE_ASYNCCANCEL_CALLBACK = function(info: PFMOD_ASYNCREADINFO; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_FILE_ASYNCDONE_FUNC = procedure(info: PFMOD_ASYNCREADINFO; result: FMOD_RESULT); cdecl;
  FMOD_MEMORY_ALLOC_CALLBACK = function(size: Cardinal; type_: FMOD_MEMORY_TYPE; const sourcestr: PChar): Pointer; cdecl;
  FMOD_MEMORY_REALLOC_CALLBACK = function(ptr: Pointer; size: Cardinal; type_: FMOD_MEMORY_TYPE; const sourcestr: PChar): Pointer; cdecl;
  FMOD_MEMORY_FREE_CALLBACK = procedure(ptr: Pointer; type_: FMOD_MEMORY_TYPE; const sourcestr: PChar); cdecl;
  FMOD_3D_ROLLOFF_CALLBACK = function(channelcontrol: PFMOD_CHANNELCONTROL; distance: Single): Single; cdecl;

  FMOD_CREATESOUNDEXINFO = record
    cbsize: Integer;
    length: Cardinal;
    fileoffset: Cardinal;
    numchannels: Integer;
    defaultfrequency: Integer;
    format: FMOD_SOUND_FORMAT;
    decodebuffersize: Cardinal;
    initialsubsound: Integer;
    numsubsounds: Integer;
    inclusionlist: PInteger;
    inclusionlistnum: Integer;
    pcmreadcallback: FMOD_SOUND_PCMREAD_CALLBACK;
    pcmsetposcallback: FMOD_SOUND_PCMSETPOS_CALLBACK;
    nonblockcallback: FMOD_SOUND_NONBLOCK_CALLBACK;
    dlsname: PChar;
    encryptionkey: PChar;
    maxpolyphony: Integer;
    userdata: Pointer;
    suggestedsoundtype: FMOD_SOUND_TYPE;
    fileuseropen: FMOD_FILE_OPEN_CALLBACK;
    fileusercFMOD_CREATESOUNDEXINFOlose: FMOD_FILE_CLOSE_CALLBACK;
    fileuserread: FMOD_FILE_READ_CALLBACK;
    fileuserseek: FMOD_FILE_SEEK_CALLBACK;
    fileuserasyncread: FMOD_FILE_ASYNCREAD_CALLBACK;
    fileuserasynccancel: FMOD_FILE_ASYNCCANCEL_CALLBACK;
    fileuserdata: Pointer;
    filebuffersize: Integer;
    channelorder: FMOD_CHANNELORDER;
    initialsoundgroup: PFMOD_SOUNDGROUP;
    initialseekposition: Cardinal;
    initialseekpostype: FMOD_TIMEUNIT;
    ignoresetfilesystem: Integer;
    audioqueuepolicy: Cardinal;
    minmidigranularity: Cardinal;
    nonblockthreadid: Integer;
    fsbguid: PFMOD_GUID;
  end;
  PFMOD_CREATESOUNDEXINFO = ^FMOD_CREATESOUNDEXINFO;

type
  PFMOD_SPEAKER        = ^FMOD_SPEAKER;
  PFMOD_ADVANCEDSETTINGS = ^FMOD_ADVANCEDSETTINGS;
  FMOD_ADVANCEDSETTINGS = record
    cbSize: Integer;
    maxMPEGCodecs: Integer;
    maxADPCMCodecs: Integer;
    maxXMACodecs: Integer;
    maxVorbisCodecs: Integer;
    maxAT9Codecs: Integer;
    maxFADPCMCodecs: Integer;
    maxPCMCodecs: Integer;
    ASIONumChannels: Integer;
    ASIOChannelList: ^PChar;
    ASIOSpeakerList: PFMOD_SPEAKER;
    vol0virtualvol: Single;
    defaultDecodeBufferSize: LongWord;
    profilePort: Word;
    geometryMaxFadeTime: LongWord;
    distanceFilterCenterFreq: Single;
    reverb3Dinstance: Integer;
    DSPBufferPoolSize: Integer;
    resamplerMethod: FMOD_DSP_RESAMPLER;
    randomSeed: LongWord;
    maxConvolutionThreads: Integer;
    maxOpusCodecs: Integer;
    maxSpatialObjects: Integer;
  end;

  PFMOD_SPEAKERMODE      = ^FMOD_SPEAKERMODE;

type
  PFMOD_CPU_USAGE = ^FMOD_CPU_USAGE;
  FMOD_CPU_USAGE = record
    dsp: Single;
    stream: Single;
    geometry: Single;
    update: Single;
    convolution1: Single;
    convolution2: Single;
  end;

 type
  PFMOD_TAG = ^FMOD_TAG;
  FMOD_TAG = record
    type_: FMOD_TAGTYPE;
    datatype: FMOD_TAGDATATYPE;
    name: PChar;
    data: Pointer;
    datalen: LongWord;
    updated: FMOD_BOOL;
  end;


