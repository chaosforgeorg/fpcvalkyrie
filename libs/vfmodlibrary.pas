unit vfmodlibrary;
{$include ../src/valkyrie.inc}
{$MACRO ON}
{$PACKRECORDS C}
{$Z4}
interface
uses Classes, SysUtils, vlibrary;

const
{$IFDEF WINDOWS}
{$IFDEF WIN64}
  FMODDefaultPath = 'fmod64.dll';
{$ELSE}
  FMODDefaultPath = 'fmod.dll';
{$ENDIF}
{$ELSE}
  {$IFDEF DARWIN}
  FMODDefaultPath = 'libfmod.dylib';
  {$ELSE}
  FMODDefaultPath = 'libfmod.so';
  {$ENDIF}
{$ENDIF}

const
  FMOD_VERSION: Single = 3.74;

{$include vfmodconst.inc}
{$include vfmodtypes.inc}

{$IFDEF UNIX}
  {$DEFINE calldecl := cdecl}
{$ELSE}
  {$DEFINE calldecl := stdcall}
{$ENDIF}

var
// Initialization / Global functions
  FSOUND_SetOutput : function (OutputType: TFSoundOutputTypes): ByteBool; calldecl;
  FSOUND_SetDriver : function (Driver: Integer): ByteBool; calldecl;
  FSOUND_SetMixer : function (Mixer: TFSoundMixerTypes): ByteBool; calldecl;
  FSOUND_SetBufferSize : function (LenMs: Integer): ByteBool; calldecl;
  FSOUND_SetHWND : function (Hwnd: THandle): ByteBool; calldecl;
  FSOUND_SetMinHardwareChannels : function (Min: Integer): ByteBool; calldecl;
  FSOUND_SetMaxHardwareChannels : function (Max: Integer): ByteBool; calldecl;
  FSOUND_SetMemorySystem : function (Pool: Pointer; PoolLen: Integer; UserAlloc: TFSoundAllocCallback; UserRealloc: TFSoundReallocCallback; UserFree: TFSoundFreeCallback): ByteBool; calldecl;

// Main initialization / closedown functions.
  FSOUND_Init : function (MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; calldecl;
  FSOUND_Close : procedure ; calldecl;

// Runtime system level functions
  FSOUND_Update : procedure ; calldecl;
  FSOUND_SetSpeakerMode : procedure (SpeakerMode: Cardinal); calldecl;
  FSOUND_SetSFXMasterVolume : procedure (Volume: Integer); calldecl;
  FSOUND_SetPanSeperation : procedure (PanSep: Single); calldecl;
  FSOUND_File_SetCallbacks : procedure (OpenCallback: TFSoundOpenCallback; CloseCallback: TFSoundCloseCallback; ReadCallback: TFSoundReadCallback; SeekCallback: TFSoundSeekCallback; TellCallback: TFSoundTellCallback); calldecl;

// System information functions
  FSOUND_GetError : function : TFModErrors; calldecl;
  FSOUND_GetVersion : function : Single; calldecl;
  FSOUND_GetOutput : function : TFSoundOutputTypes; calldecl;
  FSOUND_GetOutputHandle : function : Pointer; calldecl;
  FSOUND_GetDriver : function : Integer; calldecl;
  FSOUND_GetMixer : function : TFSoundMixerTypes; calldecl;
  FSOUND_GetNumDrivers : function : Integer; calldecl;
  FSOUND_GetDriverName : function (Id: Integer): PChar; calldecl;
  FSOUND_GetDriverCaps : function (Id: Integer; var Caps: Cardinal): ByteBool; calldecl;
  FSOUND_GetOutputRate : function : Integer; calldecl;
  FSOUND_GetMaxChannels : function : Integer; calldecl;
  FSOUND_GetMaxSamples : function : Integer; calldecl;
  FSOUND_GetSFXMasterVolume : function : Integer; calldecl;
  FSOUND_GetNumHWChannels : function (var num2d: Integer; var num3d: Integer; var total: Integer): ByteBool; calldecl;
  FSOUND_GetChannelsPlaying : function : Integer; calldecl;
  FSOUND_GetCPUUsage : function : Single; calldecl;
  FSOUND_GetMemoryStats : procedure (var CurrentAlloced: Cardinal; var MaxAlloced: Cardinal); calldecl;

// Sample management / load functions.
  FSOUND_Sample_Load : function (Index: Integer; const NameOrData: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundSample; calldecl;
  FSOUND_Sample_Alloc : function (Index: Integer; Length: Integer; Mode: Cardinal; DefFreq: Integer; DefVol: Integer; DefPan: Integer; DefPri: Integer): PFSoundSample; calldecl;
  FSOUND_Sample_Free : procedure (Sptr: PFSoundSample); calldecl;
  FSOUND_Sample_Upload : function (Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; calldecl;
  FSOUND_Sample_Lock : function (Sptr: PFSoundSample; Offset: Integer; Length: Integer; var Ptr1: Pointer; var Ptr2: Pointer; var Len1: Cardinal; var Len2: Cardinal): ByteBool; calldecl;
  FSOUND_Sample_Unlock : function (Sptr: PFSoundSample; Ptr1: Pointer; Ptr2: Pointer; Len1: Cardinal; Len2: Cardinal): ByteBool; calldecl;

// Sample control functions
  FSOUND_Sample_SetMode : function (Sptr: PFSoundSample; Mode: Cardinal): ByteBool; calldecl;
  FSOUND_Sample_SetLoopPoints : function (Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; calldecl;
  FSOUND_Sample_SetDefaults : function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; calldecl;
  FSOUND_Sample_SetDefaultsEx : function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri, VarFreq, VarVol, VarPan: Integer): ByteBool; calldecl;
  FSOUND_Sample_SetMinMaxDistance : function (Sptr: PFSoundSample; Min, Max: Single): ByteBool; calldecl;
  FSOUND_Sample_SetMaxPlaybacks : function (Sptr: PFSoundSample; Max: Integer): ByteBool; calldecl;

// Sample information functions
  FSOUND_Sample_Get : function (SampNo: Integer): PFSoundSample; calldecl;
  FSOUND_Sample_GetName : function (Sptr: PFSoundSample): PCHAR; calldecl;
  FSOUND_Sample_GetLength : function (Sptr: PFSoundSample): Cardinal; calldecl;
  FSOUND_Sample_GetLoopPoints : function (Sptr: PFSoundSample; var LoopStart: Integer; var LoopEnd: Integer): ByteBool; calldecl;
  FSOUND_Sample_GetDefaults : function (Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer): ByteBool; calldecl;
  FSOUND_Sample_GetDefaultsEx : function (Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer;var VarFreq: Integer; var VarVol: Integer; var VarPan: Integer): ByteBool; calldecl;
  FSOUND_Sample_GetMode : function (Sptr: PFSoundSample): Cardinal; calldecl;
  FSOUND_Sample_GetMinMaxDistance : function (Sptr: PFSoundSample; var Min: Single; var Max: Single): ByteBool; calldecl;

// Channel control functions.
  FSOUND_PlaySound : function (Channel: Integer; Sptr: PFSoundSample): Integer; calldecl;
  FSOUND_PlaySoundEx : function (Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; calldecl;
  FSOUND_StopSound : function (Channel: Integer): ByteBool; calldecl;

// Functions to control playback of a channel.
  FSOUND_SetFrequency : function (Channel: Integer; Freq: Integer): ByteBool; calldecl;
  FSOUND_SetVolume : function (Channel: Integer; Vol: Integer): ByteBool; calldecl;
  FSOUND_SetVolumeAbsolute : function (Channel: Integer; Vol: Integer): ByteBool; calldecl;
  FSOUND_SetPan : function (Channel: Integer; Pan: Integer): ByteBool; calldecl;
  FSOUND_SetSurround : function (Channel: Integer; Surround: ByteBool): ByteBool; calldecl;
  FSOUND_SetMute : function (Channel: Integer; Mute: ByteBool): ByteBool; calldecl;
  FSOUND_SetPriority : function (Channel: Integer; Priority: Integer): ByteBool; calldecl;
  FSOUND_SetReserved : function (Channel: Integer; Reserved: ByteBool): ByteBool; calldecl;
  FSOUND_SetPaused : function (Channel: Integer; Paused: ByteBool): ByteBool; calldecl;
  FSOUND_SetLoopMode : function (Channel: Integer; LoopMode: Cardinal): ByteBool; calldecl;
  FSOUND_SetCurrentPosition : function (Channel: Integer; Offset: Cardinal): ByteBool; calldecl;
  FSOUND_3D_SetAttributes : function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; calldecl;
  FSOUND_3D_SetMinMaxDistance : function (Channel: Integer; Min: Single; Max: Single): ByteBool; calldecl;

// Channel information functions
  FSOUND_IsPlaying : function (Channel: Integer): ByteBool; calldecl;
  FSOUND_GetFrequency : function (Channel: Integer): Integer; calldecl;
  FSOUND_GetVolume : function (Channel: Integer): Integer; calldecl;
  FSOUND_GetAmplitude : function (Channel: Integer): Integer; calldecl;
  FSOUND_GetPan : function (Channel: Integer): Integer; calldecl;
  FSOUND_GetSurround : function (Channel: Integer): ByteBool; calldecl;
  FSOUND_GetMute : function (Channel: Integer): ByteBool; calldecl;
  FSOUND_GetPriority : function (Channel: Integer): Integer; calldecl;
  FSOUND_GetReserved : function (Channel: Integer): ByteBool; calldecl;
  FSOUND_GetPaused : function (Channel: Integer): ByteBool; calldecl;
  FSOUND_GetLoopMode : function (Channel: Integer): Cardinal; calldecl;
  FSOUND_GetCurrentPosition : function (Channel: Integer): Cardinal; calldecl;
  FSOUND_GetCurrentSample : function (Channel: Integer): PFSoundSample; calldecl;
  FSOUND_GetCurrentLevels : function (Channel: Integer; l, r: PSingle): ByteBool; calldecl;
  FSOUND_GetNumSubChannels : function (Channel: Integer): Integer; calldecl;
  FSOUND_GetSubChannel : function (Channel: Integer; SubChannel: Integer): Integer; calldecl;
  FSOUND_3D_GetAttributes : function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; calldecl;
  FSOUND_3D_GetMinMaxDistance : function (Channel: Integer; var Min: Single; var Max: Single): ByteBool; calldecl;

// 3D sound functions.
  FSOUND_3D_Listener_SetCurrent : procedure (current: Integer); calldecl;
  FSOUND_3D_Listener_SetAttributes : procedure (Pos: PFSoundVector; Vel: PFSoundVector; fx: Single; fy: Single; fz: Single; tx: Single; ty: Single; tz: Single); calldecl;
  FSOUND_3D_Listener_GetAttributes : procedure (Pos: PFSoundVector; Vel: PFSoundVector; fx: PSingle; fy: PSingle; fz: PSingle; tx: PSingle; ty: PSingle; tz: PSingle); calldecl;
  FSOUND_3D_SetDopplerFactor : procedure (Scale: Single); calldecl;
  FSOUND_3D_SetDistanceFactor : procedure (Scale: Single); calldecl;
  FSOUND_3D_SetRolloffFactor : procedure (Scale: Single); calldecl;

// FX functions.
  FSOUND_FX_Enable : function (Channel: Integer; Fx: TFSoundFXModes): Integer; calldecl;
  FSOUND_FX_Disable : function (Channel: Integer): ByteBool; calldecl;
  FSOUND_FX_SetChorus : function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; calldecl;
  FSOUND_FX_SetCompressor : function (FXId: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; calldecl;
  FSOUND_FX_SetDistortion : function (FXId: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; calldecl;
  FSOUND_FX_SetEcho : function (FXId: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; calldecl;
  FSOUND_FX_SetFlanger : function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; calldecl;
  FSOUND_FX_SetGargle : function (FXId, RateHz, WaveShape: Integer): ByteBool; calldecl;
  FSOUND_FX_SetI3DL2Reverb : function (FXId, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; calldecl;
  FSOUND_FX_SetParamEQ : function (FXId: Integer; Center, Bandwidth, Gain: Single): ByteBool; calldecl;
  FSOUND_FX_SetWavesReverb : function (FXId: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; calldecl;

// File Streaming functions.
  FSOUND_Stream_SetBufferSize : function (Ms: Integer): ByteBool; calldecl;
  FSOUND_Stream_Open : function (const name_or_data: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundStream; calldecl;
  FSOUND_Stream_Create : function (Callback: TFSoundStreamCallback; Length: Integer; Mode: Cardinal; SampleRate: Integer; UserData: Integer): PFSoundStream; calldecl;
  FSOUND_Stream_Close : function (Stream: PFSoundStream): ByteBool; calldecl;

  FSOUND_Stream_Play : function (Channel: Integer; Stream: PFSoundStream): Integer; calldecl;
  FSOUND_Stream_PlayEx : function (Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; calldecl;
  FSOUND_Stream_Stop : function (Stream: PFSoundStream): ByteBool; calldecl;

  FSOUND_Stream_SetPosition : function (Stream: PFSoundStream; Position: Cardinal): ByteBool; calldecl;
  FSOUND_Stream_GetPosition : function (Stream: PFSoundStream): Cardinal; calldecl;
  FSOUND_Stream_SetTime : function (Stream: PFSoundStream; Ms: Integer): ByteBool; calldecl;
  FSOUND_Stream_GetTime : function (Stream: PFSoundStream): Integer; calldecl;
  FSOUND_Stream_GetLength : function (Stream: PFSoundStream): Integer; calldecl;
  FSOUND_Stream_GetLengthMs : function (Stream: PFSoundStream): Integer; calldecl;

  FSOUND_Stream_SetMode : function (Stream: PFSoundStream; mode: Integer): ByteBool; calldecl;
  FSOUND_Stream_GetMode : function (Stream: PFSoundStream): Integer; calldecl;
  FSOUND_Stream_SetLoopPoints : function (Stream: PFSoundStream; LoopStartPCM, LoopEndPCM: Integer): ByteBool; calldecl;
  FSOUND_Stream_SetLoopCount : function (Stream: PFSoundStream; Count: Integer): ByteBool; calldecl;
  FSOUND_Stream_GetOpenState : function (Stream: PFSoundStream): Integer; calldecl;
  FSOUND_Stream_GetSample : function (Stream: PFSoundStream): PFSoundSample; calldecl;
  FSOUND_Stream_CreateDSP : function (Stream: PFSoundStream; Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; calldecl;

  FSOUND_Stream_SetEndCallback : function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; calldecl;
  FSOUND_Stream_SetSyncCallback : function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; calldecl;

  FSOUND_Stream_AddSyncPoint : function (Stream: PFSoundStream; PCMOffset: Cardinal; Name: PChar): PFSyncPoint; calldecl;
  FSOUND_Stream_DeleteSyncPoint : function (Point: PFSyncPoint): ByteBool; calldecl;
  FSOUND_Stream_GetNumSyncPoints : function (Stream: PFSoundStream): Integer; calldecl;
  FSOUND_Stream_GetSyncPoint : function (Stream: PFSoundStream; Index: Integer): PFSyncPoint; calldecl;
  FSOUND_Stream_GetSyncPointInfo : function (Point: PFSyncPoint; var PCMOffset: Cardinal): PCHAR; calldecl;

  FSOUND_Stream_SetSubStream : function (Stream: PFSoundStream; Index: Integer): ByteBool; calldecl;
  FSOUND_Stream_GetNumSubStreams : function (Stream: PFSoundStream): Integer; calldecl;
  FSOUND_Stream_SetSubStreamSentence : function (Stream: PFSoundStream; var SentenceList: Cardinal; NumItems: Integer): ByteBool; calldecl;

  FSOUND_Stream_GetNumTagFields : function (Stream: PFSoundStream; var Num: Integer): ByteBool; calldecl;
  FSOUND_Stream_GetTagField : function (Stream: PFSoundStream; Num: Integer; var TagType: TFSoundTagFieldType; var Name: PChar; var Value: Pointer; var Length: Integer): ByteBool; calldecl;
  FSOUND_Stream_FindTagField : function (Stream: PFSoundStream; TagType: TFSoundTagFieldType; Name: PChar; var Value: Pointer; var Length: Integer): ByteBool; calldecl;

// Internet streaming functions
  FSOUND_Stream_Net_SetProxy : function (Proxy: PChar): ByteBool; calldecl;
  FSOUND_Stream_Net_GetLastServerStatus : function (): PChar; calldecl;
  FSOUND_Stream_Net_SetBufferProperties : function (BufferSize: Integer; PreBuffer_Percent: Integer; ReBuffer_Percent:  Integer): ByteBool; calldecl;
  FSOUND_Stream_Net_GetBufferProperties : function (var Buffersize: Integer; var PreBuffer_Percent: Integer;  var ReBuffer_Percent: Integer): ByteBool; calldecl;
  FSOUND_Stream_Net_SetMetadataCallback : function (Stream: PFSoundStream; Callback: TFMetaDataCallback; UserData: Integer): ByteBool; calldecl;
  FSOUND_Stream_Net_GetStatus : function (Stream: PFSoundStream; var Status: TFSoundStreamNetStatus; var BufferPercentUsed: Integer; var BitRate: Integer; var Flags: Cardinal): ByteBool; calldecl;

// CD audio functions.
  FSOUND_CD_Play : function (Drive: Byte; Track: Integer): ByteBool; calldecl;
  FSOUND_CD_SetPlayMode : procedure (Drive: Byte; Mode: Integer); calldecl;
  FSOUND_CD_Stop : function (Drive: Byte): ByteBool; calldecl;
  FSOUND_CD_SetPaused : function (Drive: Byte; Paused: ByteBool): ByteBool; calldecl;
  FSOUND_CD_SetVolume : function (Drive: Byte; Volume: Integer): ByteBool; calldecl;
  FSOUND_CD_SetTrackTime : function (Drive: Byte; ms: Integer): ByteBool; calldecl;
  FSOUND_CD_OpenTray : function (Drive: Byte; Open: Byte): ByteBool; calldecl;

  FSOUND_CD_GetPaused : function (Drive: Byte): ByteBool; calldecl;
  FSOUND_CD_GetTrack : function (Drive: Byte): Integer; calldecl;
  FSOUND_CD_GetNumTracks : function (Drive: Byte): Integer; calldecl;
  FSOUND_CD_GetVolume : function (Drive: Byte): Integer; calldecl;
  FSOUND_CD_GetTrackLength : function (Drive: Byte; Track: Integer): Integer; calldecl;
  FSOUND_CD_GetTrackTime : function (Drive: Byte): Integer; calldecl;

// DSP functions.
  FSOUND_DSP_Create : function (Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; calldecl;
  FSOUND_DSP_Free : procedure (DSPUnit: PFSoundDSPUnit); calldecl;
  FSOUND_DSP_SetPriority : procedure (DSPUnit: PFSoundDSPUnit; Priority: Integer); calldecl;
  FSOUND_DSP_GetPriority : function (DSPUnit: PFSoundDSPUnit): Integer; calldecl;
  FSOUND_DSP_SetActive : procedure (DSPUnit: PFSoundDSPUnit; Active: ByteBool); calldecl;
  FSOUND_DSP_GetActive : function (DSPUnit: PFSoundDSPUnit): ByteBool; calldecl;

  FSOUND_DSP_GetClearUnit : function : PFSoundDSPUnit; calldecl;
  FSOUND_DSP_GetSFXUnit : function : PFSoundDSPUnit; calldecl;
  FSOUND_DSP_GetMusicUnit : function : PFSoundDSPUnit; calldecl;
  FSOUND_DSP_GetFFTUnit : function : PFSoundDSPUnit; calldecl;
  FSOUND_DSP_GetClipAndCopyUnit : function : PFSoundDSPUnit; calldecl;

  FSOUND_DSP_MixBuffers : function (DestBuffer: Pointer; SrcBuffer: Pointer; Len: Integer; Freq: Integer; Vol: Integer; Pan: Integer; Mode: Cardinal): ByteBool; calldecl;
  FSOUND_DSP_ClearMixBuffer : procedure ; calldecl;
  FSOUND_DSP_GetBufferLength : function : Integer; calldecl;
  FSOUND_DSP_GetBufferLengthTotal : function : Integer; calldecl;
  FSOUND_DSP_GetSpectrum : function : PSingle; calldecl;

// Reverb functions. (eax2/3 reverb)  (NOT SUPPORTED IN UNIX/CE)
  FSOUND_Reverb_SetProperties : function (var Prop: TFSoundReverbProperties): ByteBool; calldecl;
  FSOUND_Reverb_GetProperties : function (var Prop: TFSoundReverbProperties): ByteBool; calldecl;
  FSOUND_Reverb_SetChannelProperties : function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; calldecl;
  FSOUND_Reverb_GetChannelProperties : function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; calldecl;

// Recording functions  (NOT SUPPORTED IN UNIX/MAC) }
  FSOUND_Record_SetDriver : function (OutputType: Integer): ByteBool; calldecl;
  FSOUND_Record_GetNumDrivers : function : Integer; calldecl;
  FSOUND_Record_GetDriverName : function (Id: Integer): PChar; calldecl;
  FSOUND_Record_GetDriver : function : Integer; calldecl;

// Recording functionality. Only one recording session will work at a time.
  FSOUND_Record_StartSample : function (Sptr: PFSoundSample; Loop: ByteBool): ByteBool; calldecl;
  FSOUND_Record_Stop : function : ByteBool; calldecl;
  FSOUND_Record_GetPosition : function : Integer; calldecl;

// FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK)
  FMUSIC_LoadSong : function (const Name: PChar): PFMusicModule; calldecl;
  FMUSIC_LoadSongEx : function (Name_Or_Data: Pointer; Offset: Integer; Length: Integer; Mode: Cardinal; var SampleList: Integer; SampleListNum: Integer): PFMusicModule; calldecl;
  FMUSIC_GetOpenState : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_FreeSong : function (Module: PFMusicModule): ByteBool; calldecl;
  FMUSIC_PlaySong : function (Module: PFMusicModule): ByteBool; calldecl;
  FMUSIC_StopSong : function (Module: PFMusicModule): ByteBool; calldecl;
  FMUSIC_StopAllSongs : procedure ; calldecl;

  FMUSIC_SetZxxCallback : function (Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; calldecl;
  FMUSIC_SetRowCallback : function (Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; calldecl;
  FMUSIC_SetOrderCallback : function (Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; calldecl;
  FMUSIC_SetInstCallback : function (Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; calldecl;

  FMUSIC_SetSample : function (Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; calldecl;
  FMUSIC_SetUserData : function (Module: PFMusicModule; userdata: Integer): ByteBool; calldecl;
  FMUSIC_OptimizeChannels : function (Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; calldecl;

// Runtime song functions.
  FMUSIC_SetReverb : function (Reverb: ByteBool): ByteBool; calldecl;
  FMUSIC_SetLooping : function (Module: PFMusicModule; Looping: ByteBool): ByteBool; calldecl;
  FMUSIC_SetOrder : function (Module: PFMusicModule; Order: Integer): ByteBool; calldecl;
  FMUSIC_SetPaused : function (Module: PFMusicModule; Pause: ByteBool): ByteBool; calldecl;
  FMUSIC_SetMasterVolume : function (Module: PFMusicModule; Volume: Integer): ByteBool; calldecl;
  FMUSIC_SetMasterSpeed : function (Module: PFMusicModule; speed: Single): ByteBool; calldecl;
  FMUSIC_SetPanSeperation : function (Module: PFMusicModule; PanSep: Single): ByteBool; calldecl;

// Static song information functions.
  FMUSIC_GetName : function (Module: PFMusicModule): PCHAR; calldecl;
  FMUSIC_GetType : function (Module: PFMusicModule): TFMusicTypes; calldecl;
  FMUSIC_GetNumOrders : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetNumPatterns : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetNumInstruments : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetNumSamples : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetNumChannels : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetSample : function (Module: PFMusicModule; SampNo: Integer): PFSoundSample; calldecl;
  FMUSIC_GetPatternLength : function (Module: PFMusicModule; OrderNo: Integer): Integer; calldecl;

// Runtime song information.
  FMUSIC_IsFinished : function (Module: PFMusicModule): ByteBool; calldecl;
  FMUSIC_IsPlaying : function (Module: PFMusicModule): ByteBool; calldecl;
  FMUSIC_GetMasterVolume : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetGlobalVolume : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetOrder : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetPattern : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetSpeed : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetBPM : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetRow : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetPaused : function (Module: PFMusicModule): ByteBool; calldecl;
  FMUSIC_GetTime : function (Module: PFMusicModule): Integer; calldecl;
  FMUSIC_GetRealChannel : function (Module: PFMusicModule; modchannel: Integer): Integer; calldecl;
  FMUSIC_GetUserData : function (Module: PFMusicModule): Integer; calldecl;

function FMOD_ErrorString( ErrorCode: TFModErrors ): AnsiString;

var
  FMOD : TLibrary = nil;

function LoadFMOD( const aPath : AnsiString = FMODDefaultPath ) : Boolean;

implementation

var
  Saved8087CW: Word;

const
  Default8087CW = $1332;

function LoadFMOD ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := FMOD.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'FMOD : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if FMOD <> nil then Exit( True );

  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all fpu exceptions }

  FMOD := TLibrary.Load( aPath );
  if FMOD = nil then Exit( False );


{$IFDEF WIN64}
  Pointer(FSOUND_SetOutput) := GetSymbol('FSOUND_SetOutput');
  Pointer(FSOUND_SetDriver) := GetSymbol('FSOUND_SetDriver');
  Pointer(FSOUND_SetMixer) := GetSymbol('FSOUND_SetMixer');
  Pointer(FSOUND_SetBufferSize) := GetSymbol('FSOUND_SetBufferSize');
  Pointer(FSOUND_SetHWND) := GetSymbol('FSOUND_SetHWND');
  Pointer(FSOUND_SetMinHardwareChannels) := GetSymbol('FSOUND_SetMinHardwareChannels');
  Pointer(FSOUND_SetMaxHardwareChannels) := GetSymbol('FSOUND_SetMaxHardwareChannels');
  Pointer(FSOUND_SetMemorySystem) := GetSymbol('FSOUND_SetMemorySystem');
  Pointer(FSOUND_Init) := GetSymbol('FSOUND_Init');
  Pointer(FSOUND_Close) := GetSymbol('FSOUND_Close');
  Pointer(FSOUND_Update) := GetSymbol('FSOUND_Update');
  Pointer(FSOUND_SetSpeakerMode) := GetSymbol('FSOUND_SetSpeakerMode');
  Pointer(FSOUND_SetSFXMasterVolume) := GetSymbol('FSOUND_SetSFXMasterVolume');
  Pointer(FSOUND_SetPanSeperation) := GetSymbol('FSOUND_SetPanSeperation');
  Pointer(FSOUND_GetError) := GetSymbol('FSOUND_GetError');
  Pointer(FSOUND_GetVersion) := GetSymbol('FSOUND_GetVersion');
  Pointer(FSOUND_GetOutput) := GetSymbol('FSOUND_GetOutput');
  Pointer(FSOUND_GetOutputHandle) := GetSymbol('FSOUND_GetOutputHandle');
  Pointer(FSOUND_GetDriver) := GetSymbol('FSOUND_GetDriver');
  Pointer(FSOUND_GetMixer) := GetSymbol('FSOUND_GetMixer');
  Pointer(FSOUND_GetNumDrivers) := GetSymbol('FSOUND_GetNumDrivers');
  Pointer(FSOUND_GetDriverName) := GetSymbol('FSOUND_GetDriverName');
  Pointer(FSOUND_GetDriverCaps) := GetSymbol('FSOUND_GetDriverCaps');
  Pointer(FSOUND_GetOutputRate) := GetSymbol('FSOUND_GetOutputRate');
  Pointer(FSOUND_GetMaxChannels) := GetSymbol('FSOUND_GetMaxChannels');
  Pointer(FSOUND_GetMaxSamples) := GetSymbol('FSOUND_GetMaxSamples');
  Pointer(FSOUND_GetSFXMasterVolume) := GetSymbol('FSOUND_GetSFXMasterVolume');
  Pointer(FSOUND_GetNumHWChannels) := GetSymbol('FSOUND_GetNumHWChannels');
  Pointer(FSOUND_GetChannelsPlaying) := GetSymbol('FSOUND_GetChannelsPlaying');
  Pointer(FSOUND_GetCPUUsage) := GetSymbol('FSOUND_GetCPUUsage');
  Pointer(FSOUND_GetMemoryStats) := GetSymbol('FSOUND_GetMemoryStats');
  Pointer(FSOUND_Sample_Load) := GetSymbol('FSOUND_Sample_Load');
  Pointer(FSOUND_Sample_Alloc) := GetSymbol('FSOUND_Sample_Alloc');
  Pointer(FSOUND_Sample_Free) := GetSymbol('FSOUND_Sample_Free');
  Pointer(FSOUND_Sample_Upload) := GetSymbol('FSOUND_Sample_Upload');
  Pointer(FSOUND_Sample_Lock) := GetSymbol('FSOUND_Sample_Lock');
  Pointer(FSOUND_Sample_Unlock) := GetSymbol('FSOUND_Sample_Unlock');
  Pointer(FSOUND_Sample_SetMode) := GetSymbol('FSOUND_Sample_SetMode');
  Pointer(FSOUND_Sample_SetLoopPoints) := GetSymbol('FSOUND_Sample_SetLoopPoints');
  Pointer(FSOUND_Sample_SetDefaults) := GetSymbol('FSOUND_Sample_SetDefaults');
  Pointer(FSOUND_Sample_SetDefaultsEx) := GetSymbol('FSOUND_Sample_SetDefaultsEx');
  Pointer(FSOUND_Sample_SetMinMaxDistance) := GetSymbol('FSOUND_Sample_SetMinMaxDistance');
  Pointer(FSOUND_Sample_SetMaxPlaybacks) := GetSymbol('FSOUND_Sample_SetMaxPlaybacks');
  Pointer(FSOUND_Sample_Get) := GetSymbol('FSOUND_Sample_Get');
  Pointer(FSOUND_Sample_GetName) := GetSymbol('FSOUND_Sample_GetName');
  Pointer(FSOUND_Sample_GetLength) := GetSymbol('FSOUND_Sample_GetLength');
  Pointer(FSOUND_Sample_GetLoopPoints) := GetSymbol('FSOUND_Sample_GetLoopPoints');
  Pointer(FSOUND_Sample_GetDefaults) := GetSymbol('FSOUND_Sample_GetDefaults');
  Pointer(FSOUND_Sample_GetDefaultsEx) := GetSymbol('FSOUND_Sample_GetDefaultsEx');
  Pointer(FSOUND_Sample_GetMode) := GetSymbol('FSOUND_Sample_GetMode');
  Pointer(FSOUND_Sample_GetMinMaxDistance) := GetSymbol('FSOUND_Sample_GetMinMaxDistance');
  Pointer(FSOUND_PlaySound) := GetSymbol('FSOUND_PlaySound');
  Pointer(FSOUND_PlaySoundEx) := GetSymbol('FSOUND_PlaySoundEx');
  Pointer(FSOUND_StopSound) := GetSymbol('FSOUND_StopSound');
  Pointer(FSOUND_SetFrequency) := GetSymbol('FSOUND_SetFrequency');
  Pointer(FSOUND_SetVolume) := GetSymbol('FSOUND_SetVolume');
  Pointer(FSOUND_SetVolumeAbsolute) := GetSymbol('FSOUND_SetVolumeAbsolute');
  Pointer(FSOUND_SetPan) := GetSymbol('FSOUND_SetPan');
  Pointer(FSOUND_SetSurround) := GetSymbol('FSOUND_SetSurround');
  Pointer(FSOUND_SetMute) := GetSymbol('FSOUND_SetMute');
  Pointer(FSOUND_SetPriority) := GetSymbol('FSOUND_SetPriority');
  Pointer(FSOUND_SetReserved) := GetSymbol('FSOUND_SetReserved');
  Pointer(FSOUND_SetPaused) := GetSymbol('FSOUND_SetPaused');
  Pointer(FSOUND_SetLoopMode) := GetSymbol('FSOUND_SetLoopMode');
  Pointer(FSOUND_SetCurrentPosition) := GetSymbol('FSOUND_SetCurrentPosition');
  Pointer(FSOUND_3D_SetAttributes) := GetSymbol('FSOUND_3D_SetAttributes');
  Pointer(FSOUND_3D_SetMinMaxDistance) := GetSymbol('FSOUND_3D_SetMinMaxDistance');
  Pointer(FSOUND_IsPlaying) := GetSymbol('FSOUND_IsPlaying');
  Pointer(FSOUND_GetFrequency) := GetSymbol('FSOUND_GetFrequency');
  Pointer(FSOUND_GetVolume) := GetSymbol('FSOUND_GetVolume');
  Pointer(FSOUND_GetAmplitude) := GetSymbol('FSOUND_GetAmplitude');
  Pointer(FSOUND_GetPan) := GetSymbol('FSOUND_GetPan');
  Pointer(FSOUND_GetSurround) := GetSymbol('FSOUND_GetSurround');
  Pointer(FSOUND_GetMute) := GetSymbol('FSOUND_GetMute');
  Pointer(FSOUND_GetPriority) := GetSymbol('FSOUND_GetPriority');
  Pointer(FSOUND_GetReserved) := GetSymbol('FSOUND_GetReserved');
  Pointer(FSOUND_GetPaused) := GetSymbol('FSOUND_GetPaused');
  Pointer(FSOUND_GetLoopMode) := GetSymbol('FSOUND_GetLoopMode');
  Pointer(FSOUND_GetCurrentPosition) := GetSymbol('FSOUND_GetCurrentPosition');
  Pointer(FSOUND_GetCurrentSample) := GetSymbol('FSOUND_GetCurrentSample');
  Pointer(FSOUND_GetCurrentLevels) := GetSymbol('FSOUND_GetCurrentLevels');
  Pointer(FSOUND_GetNumSubChannels) := GetSymbol('FSOUND_GetNumSubChannels');
  Pointer(FSOUND_GetSubChannel) := GetSymbol('FSOUND_GetSubChannel');
  Pointer(FSOUND_3D_GetAttributes) := GetSymbol('FSOUND_3D_GetAttributes');
  Pointer(FSOUND_3D_GetMinMaxDistance) := GetSymbol('FSOUND_3D_GetMinMaxDistance');
  Pointer(FSOUND_3D_Listener_SetCurrent) := GetSymbol('FSOUND_3D_Listener_SetCurrent');
  Pointer(FSOUND_3D_Listener_SetAttributes) := GetSymbol('FSOUND_3D_Listener_SetAttributes');
  Pointer(FSOUND_3D_Listener_GetAttributes) := GetSymbol('FSOUND_3D_Listener_GetAttributes');
  Pointer(FSOUND_3D_SetDopplerFactor) := GetSymbol('FSOUND_3D_SetDopplerFactor');
  Pointer(FSOUND_3D_SetDistanceFactor) := GetSymbol('FSOUND_3D_SetDistanceFactor');
  Pointer(FSOUND_3D_SetRolloffFactor) := GetSymbol('FSOUND_3D_SetRolloffFactor');
  Pointer(FSOUND_FX_Enable) := GetSymbol('FSOUND_FX_Enable');
  Pointer(FSOUND_FX_Disable) := GetSymbol('FSOUND_FX_Disable');
  Pointer(FSOUND_FX_SetChorus) := GetSymbol('FSOUND_FX_SetChorus');
  Pointer(FSOUND_FX_SetCompressor) := GetSymbol('FSOUND_FX_SetCompressor');
  Pointer(FSOUND_FX_SetDistortion) := GetSymbol('FSOUND_FX_SetDistortion');
  Pointer(FSOUND_FX_SetEcho) := GetSymbol('FSOUND_FX_SetEcho');
  Pointer(FSOUND_FX_SetFlanger) := GetSymbol('FSOUND_FX_SetFlanger');
  Pointer(FSOUND_FX_SetGargle) := GetSymbol('FSOUND_FX_SetGargle');
  Pointer(FSOUND_FX_SetI3DL2Reverb) := GetSymbol('FSOUND_FX_SetI3DL2Reverb');
  Pointer(FSOUND_FX_SetParamEQ) := GetSymbol('FSOUND_FX_SetParamEQ');
  Pointer(FSOUND_FX_SetWavesReverb) := GetSymbol('FSOUND_FX_SetWavesReverb');
  Pointer(FSOUND_Stream_Open) := GetSymbol('FSOUND_Stream_Open');
  Pointer(FSOUND_Stream_Create) := GetSymbol('FSOUND_Stream_Create');
  Pointer(FSOUND_Stream_Play) := GetSymbol('FSOUND_Stream_Play');
  Pointer(FSOUND_Stream_PlayEx) := GetSymbol('FSOUND_Stream_PlayEx');
  Pointer(FSOUND_Stream_Stop) := GetSymbol('FSOUND_Stream_Stop');
  Pointer(FSOUND_Stream_Close) := GetSymbol('FSOUND_Stream_Close');
  Pointer(FSOUND_Stream_SetEndCallback) := GetSymbol('FSOUND_Stream_SetEndCallback');
  Pointer(FSOUND_Stream_SetSyncCallback) := GetSymbol('FSOUND_Stream_SetSyncCallback');
  Pointer(FSOUND_Stream_GetSample) := GetSymbol('FSOUND_Stream_GetSample');
  Pointer(FSOUND_Stream_CreateDSP) := GetSymbol('FSOUND_Stream_CreateDSP');
  Pointer(FSOUND_Stream_SetBufferSize) := GetSymbol('FSOUND_Stream_SetBufferSize');
  Pointer(FSOUND_Stream_SetPosition) := GetSymbol('FSOUND_Stream_SetPosition');
  Pointer(FSOUND_Stream_GetPosition) := GetSymbol('FSOUND_Stream_GetPosition');
  Pointer(FSOUND_Stream_SetTime) := GetSymbol('FSOUND_Stream_SetTime');
  Pointer(FSOUND_Stream_GetTime) := GetSymbol('FSOUND_Stream_GetTime');
  Pointer(FSOUND_Stream_GetLength) := GetSymbol('FSOUND_Stream_GetLength');
  Pointer(FSOUND_Stream_GetLengthMs) := GetSymbol('FSOUND_Stream_GetLengthMs');
  Pointer(FSOUND_Stream_SetMode) := GetSymbol('FSOUND_Stream_SetMode');
  Pointer(FSOUND_Stream_GetMode) := GetSymbol('FSOUND_Stream_GetMode');
  Pointer(FSOUND_Stream_SetLoopPoints) := GetSymbol('FSOUND_Stream_SetLoopPoints');
  Pointer(FSOUND_Stream_SetLoopCount) := GetSymbol('FSOUND_Stream_SetLoopCount');
  Pointer(FSOUND_Stream_AddSyncPoint) := GetSymbol('FSOUND_Stream_AddSyncPoint');
  Pointer(FSOUND_Stream_DeleteSyncPoint) := GetSymbol('FSOUND_Stream_DeleteSyncPoint');
  Pointer(FSOUND_Stream_GetNumSyncPoints) := GetSymbol('FSOUND_Stream_GetNumSyncPoints');
  Pointer(FSOUND_Stream_GetSyncPoint) := GetSymbol('FSOUND_Stream_GetSyncPoint');
  Pointer(FSOUND_Stream_GetSyncPointInfo) := GetSymbol('FSOUND_Stream_GetSyncPointInfo');
  Pointer(FSOUND_Stream_GetOpenState) := GetSymbol('FSOUND_Stream_GetOpenState');
  Pointer(FSOUND_Stream_SetSubStream) := GetSymbol('FSOUND_Stream_SetSubStream');
  Pointer(FSOUND_Stream_GetNumSubStreams) := GetSymbol('FSOUND_Stream_GetNumSubStreams');
  Pointer(FSOUND_Stream_SetSubStreamSentence) := GetSymbol('FSOUND_Stream_SetSubStreamSentence');
  Pointer(FSOUND_Stream_GetNumTagFields) := GetSymbol('FSOUND_Stream_GetNumTagFields');
  Pointer(FSOUND_Stream_GetTagField) := GetSymbol('FSOUND_Stream_GetTagField');
  Pointer(FSOUND_Stream_FindTagField) := GetSymbol('FSOUND_Stream_FindTagField');
  Pointer(FSOUND_Stream_Net_SetProxy) := GetSymbol('FSOUND_Stream_Net_SetProxy');
  Pointer(FSOUND_Stream_Net_GetLastServerStatus) := GetSymbol('FSOUND_Stream_Net_GetLastServerStatus');
  Pointer(FSOUND_Stream_Net_SetBufferProperties) := GetSymbol('FSOUND_Stream_Net_SetBufferProperties');
  Pointer(FSOUND_Stream_Net_GetBufferProperties) := GetSymbol('FSOUND_Stream_Net_GetBufferProperties');
  Pointer(FSOUND_Stream_Net_SetMetadataCallback) := GetSymbol('FSOUND_Stream_Net_SetMetadataCallback');
  Pointer(FSOUND_Stream_Net_GetStatus) := GetSymbol('FSOUND_Stream_Net_GetStatus');
  Pointer(FSOUND_CD_Play) := GetSymbol('FSOUND_CD_Play');
  Pointer(FSOUND_CD_SetPlayMode) := GetSymbol('FSOUND_CD_SetPlayMode');
  Pointer(FSOUND_CD_Stop) := GetSymbol('FSOUND_CD_Stop');
  Pointer(FSOUND_CD_SetPaused) := GetSymbol('FSOUND_CD_SetPaused');
  Pointer(FSOUND_CD_SetVolume) := GetSymbol('FSOUND_CD_SetVolume');
  Pointer(FSOUND_CD_SetTrackTime) := GetSymbol('FSOUND_CD_SetTrackTime');
  Pointer(FSOUND_CD_OpenTray) := GetSymbol('FSOUND_CD_OpenTray');
  Pointer(FSOUND_CD_GetPaused) := GetSymbol('FSOUND_CD_GetPaused');
  Pointer(FSOUND_CD_GetTrack) := GetSymbol('FSOUND_CD_GetTrack');
  Pointer(FSOUND_CD_GetNumTracks) := GetSymbol('FSOUND_CD_GetNumTracks');
  Pointer(FSOUND_CD_GetVolume) := GetSymbol('FSOUND_CD_GetVolume');
  Pointer(FSOUND_CD_GetTrackLength) := GetSymbol('FSOUND_CD_GetTrackLength');
  Pointer(FSOUND_CD_GetTrackTime) := GetSymbol('FSOUND_CD_GetTrackTime');
  Pointer(FSOUND_DSP_Create) := GetSymbol('FSOUND_DSP_Create');
  Pointer(FSOUND_DSP_Free) := GetSymbol('FSOUND_DSP_Free');
  Pointer(FSOUND_DSP_SetPriority) := GetSymbol('FSOUND_DSP_SetPriority');
  Pointer(FSOUND_DSP_GetPriority) := GetSymbol('FSOUND_DSP_GetPriority');
  Pointer(FSOUND_DSP_SetActive) := GetSymbol('FSOUND_DSP_SetActive');
  Pointer(FSOUND_DSP_GetActive) := GetSymbol('FSOUND_DSP_GetActive');
  Pointer(FSOUND_DSP_GetClearUnit) := GetSymbol('FSOUND_DSP_GetClearUnit');
  Pointer(FSOUND_DSP_GetSFXUnit) := GetSymbol('FSOUND_DSP_GetSFXUnit');
  Pointer(FSOUND_DSP_GetMusicUnit) := GetSymbol('FSOUND_DSP_GetMusicUnit');
  Pointer(FSOUND_DSP_GetClipAndCopyUnit) := GetSymbol('FSOUND_DSP_GetClipAndCopyUnit');
  Pointer(FSOUND_DSP_GetFFTUnit) := GetSymbol('FSOUND_DSP_GetFFTUnit');
  Pointer(FSOUND_DSP_MixBuffers) := GetSymbol('FSOUND_DSP_MixBuffers');
  Pointer(FSOUND_DSP_ClearMixBuffer) := GetSymbol('FSOUND_DSP_ClearMixBuffer');
  Pointer(FSOUND_DSP_GetBufferLength) := GetSymbol('FSOUND_DSP_GetBufferLength');
  Pointer(FSOUND_DSP_GetBufferLengthTotal) := GetSymbol('FSOUND_DSP_GetBufferLengthTotal');
  Pointer(FSOUND_DSP_GetSpectrum) := GetSymbol('FSOUND_DSP_GetSpectrum');
  Pointer(FSOUND_Reverb_SetProperties) := GetSymbol('FSOUND_Reverb_SetProperties');
  Pointer(FSOUND_Reverb_GetProperties) := GetSymbol('FSOUND_Reverb_GetProperties');
  Pointer(FSOUND_Reverb_SetChannelProperties) := GetSymbol('FSOUND_Reverb_SetChannelProperties');
  Pointer(FSOUND_Reverb_GetChannelProperties) := GetSymbol('FSOUND_Reverb_GetChannelProperties');
  Pointer(FSOUND_Record_SetDriver) := GetSymbol('FSOUND_Record_SetDriver');
  Pointer(FSOUND_Record_GetNumDrivers) := GetSymbol('FSOUND_Record_GetNumDrivers');
  Pointer(FSOUND_Record_GetDriverName) := GetSymbol('FSOUND_Record_GetDriverName');
  Pointer(FSOUND_Record_GetDriver) := GetSymbol('FSOUND_Record_GetDriver');
  Pointer(FSOUND_Record_StartSample) := GetSymbol('FSOUND_Record_StartSample');
  Pointer(FSOUND_Record_Stop) := GetSymbol('FSOUND_Record_Stop');
  Pointer(FSOUND_Record_GetPosition) := GetSymbol('FSOUND_Record_GetPosition');
  Pointer(FSOUND_File_SetCallbacks) := GetSymbol('FSOUND_File_SetCallbacks');
  Pointer(FMUSIC_LoadSong) := GetSymbol('FMUSIC_LoadSong');
  Pointer(FMUSIC_LoadSongEx) := GetSymbol('FMUSIC_LoadSongEx');
  Pointer(FMUSIC_GetOpenState) := GetSymbol('FMUSIC_GetOpenState');
  Pointer(FMUSIC_FreeSong) := GetSymbol('FMUSIC_FreeSong');
  Pointer(FMUSIC_PlaySong) := GetSymbol('FMUSIC_PlaySong');
  Pointer(FMUSIC_StopSong) := GetSymbol('FMUSIC_StopSong');
  Pointer(FMUSIC_StopAllSongs) := GetSymbol('FMUSIC_StopAllSongs');
  Pointer(FMUSIC_SetZxxCallback) := GetSymbol('FMUSIC_SetZxxCallback');
  Pointer(FMUSIC_SetRowCallback) := GetSymbol('FMUSIC_SetRowCallback');
  Pointer(FMUSIC_SetOrderCallback) := GetSymbol('FMUSIC_SetOrderCallback');
  Pointer(FMUSIC_SetInstCallback) := GetSymbol('FMUSIC_SetInstCallback');
  Pointer(FMUSIC_SetSample) := GetSymbol('FMUSIC_SetSample');
  Pointer(FMUSIC_SetUserData) := GetSymbol('FMUSIC_SetUserData');
  Pointer(FMUSIC_OptimizeChannels) := GetSymbol('FMUSIC_OptimizeChannels');
  Pointer(FMUSIC_SetReverb) := GetSymbol('FMUSIC_SetReverb');
  Pointer(FMUSIC_SetLooping) := GetSymbol('FMUSIC_SetLooping');
  Pointer(FMUSIC_SetOrder) := GetSymbol('FMUSIC_SetOrder');
  Pointer(FMUSIC_SetPaused) := GetSymbol('FMUSIC_SetPaused');
  Pointer(FMUSIC_SetMasterVolume) := GetSymbol('FMUSIC_SetMasterVolume');
  Pointer(FMUSIC_SetMasterSpeed) := GetSymbol('FMUSIC_SetMasterSpeed');
  Pointer(FMUSIC_SetPanSeperation) := GetSymbol('FMUSIC_SetPanSeperation');
  Pointer(FMUSIC_GetName) := GetSymbol('FMUSIC_GetName');
  Pointer(FMUSIC_GetType) := GetSymbol('FMUSIC_GetType');
  Pointer(FMUSIC_GetNumOrders) := GetSymbol('FMUSIC_GetNumOrders');
  Pointer(FMUSIC_GetNumPatterns) := GetSymbol('FMUSIC_GetNumPatterns');
  Pointer(FMUSIC_GetNumInstruments) := GetSymbol('FMUSIC_GetNumInstruments');
  Pointer(FMUSIC_GetNumSamples) := GetSymbol('FMUSIC_GetNumSamples');
  Pointer(FMUSIC_GetNumChannels) := GetSymbol('FMUSIC_GetNumChannels');
  Pointer(FMUSIC_GetSample) := GetSymbol('FMUSIC_GetSample');
  Pointer(FMUSIC_GetPatternLength) := GetSymbol('FMUSIC_GetPatternLength');
  Pointer(FMUSIC_IsFinished) := GetSymbol('FMUSIC_IsFinished');
  Pointer(FMUSIC_IsPlaying) := GetSymbol('FMUSIC_IsPlaying');
  Pointer(FMUSIC_GetMasterVolume) := GetSymbol('FMUSIC_GetMasterVolume');
  Pointer(FMUSIC_GetGlobalVolume) := GetSymbol('FMUSIC_GetGlobalVolume');
  Pointer(FMUSIC_GetOrder) := GetSymbol('FMUSIC_GetOrder');
  Pointer(FMUSIC_GetPattern) := GetSymbol('FMUSIC_GetPattern');
  Pointer(FMUSIC_GetSpeed) := GetSymbol('FMUSIC_GetSpeed');
  Pointer(FMUSIC_GetBPM) := GetSymbol('FMUSIC_GetBPM');
  Pointer(FMUSIC_GetRow) := GetSymbol('FMUSIC_GetRow');
  Pointer(FMUSIC_GetPaused) := GetSymbol('FMUSIC_GetPaused');
  Pointer(FMUSIC_GetTime) := GetSymbol('FMUSIC_GetTime');
  Pointer(FMUSIC_GetRealChannel) := GetSymbol('FMUSIC_GetRealChannel');
  Pointer(FMUSIC_GetUserData) := GetSymbol('FMUSIC_GetUserData');
{$ELSE}
  Pointer(FSOUND_SetOutput) := GetSymbol('_FSOUND_SetOutput@4');
  Pointer(FSOUND_SetDriver) := GetSymbol('_FSOUND_SetDriver@4');
  Pointer(FSOUND_SetMixer) := GetSymbol('_FSOUND_SetMixer@4');
  Pointer(FSOUND_SetBufferSize) := GetSymbol('_FSOUND_SetBufferSize@4');
  Pointer(FSOUND_SetHWND) := GetSymbol('_FSOUND_SetHWND@4');
  Pointer(FSOUND_SetMinHardwareChannels) := GetSymbol('_FSOUND_SetMinHardwareChannels@4');
  Pointer(FSOUND_SetMaxHardwareChannels) := GetSymbol('_FSOUND_SetMaxHardwareChannels@4');
  Pointer(FSOUND_SetMemorySystem) := GetSymbol('_FSOUND_SetMemorySystem@20');
  Pointer(FSOUND_Init) := GetSymbol('_FSOUND_Init@12');
  Pointer(FSOUND_Close) := GetSymbol('_FSOUND_Close@0');
  Pointer(FSOUND_Update) := GetSymbol('_FSOUND_Update@0');
  Pointer(FSOUND_SetSpeakerMode) := GetSymbol('_FSOUND_SetSpeakerMode@4');
  Pointer(FSOUND_SetSFXMasterVolume) := GetSymbol('_FSOUND_SetSFXMasterVolume@4');
  Pointer(FSOUND_SetPanSeperation) := GetSymbol('_FSOUND_SetPanSeperation@4');
  Pointer(FSOUND_GetError) := GetSymbol('_FSOUND_GetError@0');
  Pointer(FSOUND_GetVersion) := GetSymbol('_FSOUND_GetVersion@0');
  Pointer(FSOUND_GetOutput) := GetSymbol('_FSOUND_GetOutput@0');
  Pointer(FSOUND_GetOutputHandle) := GetSymbol('_FSOUND_GetOutputHandle@0');
  Pointer(FSOUND_GetDriver) := GetSymbol('_FSOUND_GetDriver@0');
  Pointer(FSOUND_GetMixer) := GetSymbol('_FSOUND_GetMixer@0');
  Pointer(FSOUND_GetNumDrivers) := GetSymbol('_FSOUND_GetNumDrivers@0');
  Pointer(FSOUND_GetDriverName) := GetSymbol('_FSOUND_GetDriverName@4');
  Pointer(FSOUND_GetDriverCaps) := GetSymbol('_FSOUND_GetDriverCaps@8');
  Pointer(FSOUND_GetOutputRate) := GetSymbol('_FSOUND_GetOutputRate@0');
  Pointer(FSOUND_GetMaxChannels) := GetSymbol('_FSOUND_GetMaxChannels@0');
  Pointer(FSOUND_GetMaxSamples) := GetSymbol('_FSOUND_GetMaxSamples@0');
  Pointer(FSOUND_GetSFXMasterVolume) := GetSymbol('_FSOUND_GetSFXMasterVolume@0');
  Pointer(FSOUND_GetNumHWChannels) := GetSymbol('_FSOUND_GetNumHWChannels@12');
  Pointer(FSOUND_GetChannelsPlaying) := GetSymbol('_FSOUND_GetChannelsPlaying@0');
  Pointer(FSOUND_GetCPUUsage) := GetSymbol('_FSOUND_GetCPUUsage@0');
  Pointer(FSOUND_GetMemoryStats) := GetSymbol('_FSOUND_GetMemoryStats@8');
  Pointer(FSOUND_Sample_Load) := GetSymbol('_FSOUND_Sample_Load@20');
  Pointer(FSOUND_Sample_Alloc) := GetSymbol('_FSOUND_Sample_Alloc@28');
  Pointer(FSOUND_Sample_Free) := GetSymbol('_FSOUND_Sample_Free@4');
  Pointer(FSOUND_Sample_Upload) := GetSymbol('_FSOUND_Sample_Upload@12');
  Pointer(FSOUND_Sample_Lock) := GetSymbol('_FSOUND_Sample_Lock@28');
  Pointer(FSOUND_Sample_Unlock) := GetSymbol('_FSOUND_Sample_Unlock@20');
  Pointer(FSOUND_Sample_SetMode) := GetSymbol('_FSOUND_Sample_SetMode@8');
  Pointer(FSOUND_Sample_SetLoopPoints) := GetSymbol('_FSOUND_Sample_SetLoopPoints@12');
  Pointer(FSOUND_Sample_SetDefaults) := GetSymbol('_FSOUND_Sample_SetDefaults@20');
  Pointer(FSOUND_Sample_SetDefaultsEx) := GetSymbol('_FSOUND_Sample_SetDefaultsEx@32');
  Pointer(FSOUND_Sample_SetMinMaxDistance) := GetSymbol('_FSOUND_Sample_SetMinMaxDistance@12');
  Pointer(FSOUND_Sample_SetMaxPlaybacks) := GetSymbol('_FSOUND_Sample_SetMaxPlaybacks@8');
  Pointer(FSOUND_Sample_Get) := GetSymbol('_FSOUND_Sample_Get@4');
  Pointer(FSOUND_Sample_GetName) := GetSymbol('_FSOUND_Sample_GetName@4');
  Pointer(FSOUND_Sample_GetLength) := GetSymbol('_FSOUND_Sample_GetLength@4');
  Pointer(FSOUND_Sample_GetLoopPoints) := GetSymbol('_FSOUND_Sample_GetLoopPoints@12');
  Pointer(FSOUND_Sample_GetDefaults) := GetSymbol('_FSOUND_Sample_GetDefaults@20');
  Pointer(FSOUND_Sample_GetDefaultsEx) := GetSymbol('_FSOUND_Sample_GetDefaultsEx@32');
  Pointer(FSOUND_Sample_GetMode) := GetSymbol('_FSOUND_Sample_GetMode@4');
  Pointer(FSOUND_Sample_GetMinMaxDistance) := GetSymbol('_FSOUND_Sample_GetMinMaxDistance@12');
  Pointer(FSOUND_PlaySound) := GetSymbol('_FSOUND_PlaySound@8');
  Pointer(FSOUND_PlaySoundEx) := GetSymbol('_FSOUND_PlaySoundEx@16');
  Pointer(FSOUND_StopSound) := GetSymbol('_FSOUND_StopSound@4');
  Pointer(FSOUND_SetFrequency) := GetSymbol('_FSOUND_SetFrequency@8');
  Pointer(FSOUND_SetVolume) := GetSymbol('_FSOUND_SetVolume@8');
  Pointer(FSOUND_SetVolumeAbsolute) := GetSymbol('_FSOUND_SetVolumeAbsolute@8');
  Pointer(FSOUND_SetPan) := GetSymbol('_FSOUND_SetPan@8');
  Pointer(FSOUND_SetSurround) := GetSymbol('_FSOUND_SetSurround@8');
  Pointer(FSOUND_SetMute) := GetSymbol('_FSOUND_SetMute@8');
  Pointer(FSOUND_SetPriority) := GetSymbol('_FSOUND_SetPriority@8');
  Pointer(FSOUND_SetReserved) := GetSymbol('_FSOUND_SetReserved@8');
  Pointer(FSOUND_SetPaused) := GetSymbol('_FSOUND_SetPaused@8');
  Pointer(FSOUND_SetLoopMode) := GetSymbol('_FSOUND_SetLoopMode@8');
  Pointer(FSOUND_SetCurrentPosition) := GetSymbol('_FSOUND_SetCurrentPosition@8');
  Pointer(FSOUND_3D_SetAttributes) := GetSymbol('_FSOUND_3D_SetAttributes@12');
  Pointer(FSOUND_3D_SetMinMaxDistance) := GetSymbol('_FSOUND_3D_SetMinMaxDistance@12');
  Pointer(FSOUND_IsPlaying) := GetSymbol('_FSOUND_IsPlaying@4');
  Pointer(FSOUND_GetFrequency) := GetSymbol('_FSOUND_GetFrequency@4');
  Pointer(FSOUND_GetVolume) := GetSymbol('_FSOUND_GetVolume@4');
  Pointer(FSOUND_GetAmplitude) := GetSymbol('_FSOUND_GetAmplitude@4');
  Pointer(FSOUND_GetPan) := GetSymbol('_FSOUND_GetPan@4');
  Pointer(FSOUND_GetSurround) := GetSymbol('_FSOUND_GetSurround@4');
  Pointer(FSOUND_GetMute) := GetSymbol('_FSOUND_GetMute@4');
  Pointer(FSOUND_GetPriority) := GetSymbol('_FSOUND_GetPriority@4');
  Pointer(FSOUND_GetReserved) := GetSymbol('_FSOUND_GetReserved@4');
  Pointer(FSOUND_GetPaused) := GetSymbol('_FSOUND_GetPaused@4');
  Pointer(FSOUND_GetLoopMode) := GetSymbol('_FSOUND_GetLoopMode@4');
  Pointer(FSOUND_GetCurrentPosition) := GetSymbol('_FSOUND_GetCurrentPosition@4');
  Pointer(FSOUND_GetCurrentSample) := GetSymbol('_FSOUND_GetCurrentSample@4');
  Pointer(FSOUND_GetCurrentLevels) := GetSymbol('_FSOUND_GetCurrentLevels@12');
  Pointer(FSOUND_GetNumSubChannels) := GetSymbol('_FSOUND_GetNumSubChannels@4');
  Pointer(FSOUND_GetSubChannel) := GetSymbol('_FSOUND_GetSubChannel@8');
  Pointer(FSOUND_3D_GetAttributes) := GetSymbol('_FSOUND_3D_GetAttributes@12');
  Pointer(FSOUND_3D_GetMinMaxDistance) := GetSymbol('_FSOUND_3D_GetMinMaxDistance@12');
  Pointer(FSOUND_3D_Listener_SetCurrent) := GetSymbol('_FSOUND_3D_Listener_SetCurrent@8');
  Pointer(FSOUND_3D_Listener_SetAttributes) := GetSymbol('_FSOUND_3D_Listener_SetAttributes@32');
  Pointer(FSOUND_3D_Listener_GetAttributes) := GetSymbol('_FSOUND_3D_Listener_GetAttributes@32');
  Pointer(FSOUND_3D_SetDopplerFactor) := GetSymbol('_FSOUND_3D_SetDopplerFactor@4');
  Pointer(FSOUND_3D_SetDistanceFactor) := GetSymbol('_FSOUND_3D_SetDistanceFactor@4');
  Pointer(FSOUND_3D_SetRolloffFactor) := GetSymbol('_FSOUND_3D_SetRolloffFactor@4');
  Pointer(FSOUND_FX_Enable) := GetSymbol('_FSOUND_FX_Enable@8');
  Pointer(FSOUND_FX_Disable) := GetSymbol('_FSOUND_FX_Disable@4');
  Pointer(FSOUND_FX_SetChorus) := GetSymbol('_FSOUND_FX_SetChorus@32');
  Pointer(FSOUND_FX_SetCompressor) := GetSymbol('_FSOUND_FX_SetCompressor@28');
  Pointer(FSOUND_FX_SetDistortion) := GetSymbol('_FSOUND_FX_SetDistortion@24');
  Pointer(FSOUND_FX_SetEcho) := GetSymbol('_FSOUND_FX_SetEcho@24');
  Pointer(FSOUND_FX_SetFlanger) := GetSymbol('_FSOUND_FX_SetFlanger@32');
  Pointer(FSOUND_FX_SetGargle) := GetSymbol('_FSOUND_FX_SetGargle@12');
  Pointer(FSOUND_FX_SetI3DL2Reverb) := GetSymbol('_FSOUND_FX_SetI3DL2Reverb@52');
  Pointer(FSOUND_FX_SetParamEQ) := GetSymbol('_FSOUND_FX_SetParamEQ@16');
  Pointer(FSOUND_FX_SetWavesReverb) := GetSymbol('_FSOUND_FX_SetWavesReverb@20');
  Pointer(FSOUND_Stream_Open) := GetSymbol('_FSOUND_Stream_Open@16');
  Pointer(FSOUND_Stream_Create) := GetSymbol('_FSOUND_Stream_Create@20');
  Pointer(FSOUND_Stream_Play) := GetSymbol('_FSOUND_Stream_Play@8');
  Pointer(FSOUND_Stream_PlayEx) := GetSymbol('_FSOUND_Stream_PlayEx@16');
  Pointer(FSOUND_Stream_Stop) := GetSymbol('_FSOUND_Stream_Stop@4');
  Pointer(FSOUND_Stream_Close) := GetSymbol('_FSOUND_Stream_Close@4');
  Pointer(FSOUND_Stream_SetEndCallback) := GetSymbol('_FSOUND_Stream_SetEndCallback@12');
  Pointer(FSOUND_Stream_SetSyncCallback) := GetSymbol('_FSOUND_Stream_SetSyncCallback@12');
  Pointer(FSOUND_Stream_GetSample) := GetSymbol('_FSOUND_Stream_GetSample@4');
  Pointer(FSOUND_Stream_CreateDSP) := GetSymbol('_FSOUND_Stream_CreateDSP@16');
  Pointer(FSOUND_Stream_SetBufferSize) := GetSymbol('_FSOUND_Stream_SetBufferSize@4');
  Pointer(FSOUND_Stream_SetPosition) := GetSymbol('_FSOUND_Stream_SetPosition@8');
  Pointer(FSOUND_Stream_GetPosition) := GetSymbol('_FSOUND_Stream_GetPosition@4');
  Pointer(FSOUND_Stream_SetTime) := GetSymbol('_FSOUND_Stream_SetTime@8');
  Pointer(FSOUND_Stream_GetTime) := GetSymbol('_FSOUND_Stream_GetTime@4');
  Pointer(FSOUND_Stream_GetLength) := GetSymbol('_FSOUND_Stream_GetLength@4');
  Pointer(FSOUND_Stream_GetLengthMs) := GetSymbol('_FSOUND_Stream_GetLengthMs@4');
  Pointer(FSOUND_Stream_SetMode) := GetSymbol('_FSOUND_Stream_SetMode@8');
  Pointer(FSOUND_Stream_GetMode) := GetSymbol('_FSOUND_Stream_GetMode@4');
  Pointer(FSOUND_Stream_SetLoopPoints) := GetSymbol('_FSOUND_Stream_SetLoopPoints@12');
  Pointer(FSOUND_Stream_SetLoopCount) := GetSymbol('_FSOUND_Stream_SetLoopCount@8');
  Pointer(FSOUND_Stream_AddSyncPoint) := GetSymbol('_FSOUND_Stream_AddSyncPoint@12');
  Pointer(FSOUND_Stream_DeleteSyncPoint) := GetSymbol('_FSOUND_Stream_DeleteSyncPoint@4');
  Pointer(FSOUND_Stream_GetNumSyncPoints) := GetSymbol('_FSOUND_Stream_GetNumSyncPoints@4');
  Pointer(FSOUND_Stream_GetSyncPoint) := GetSymbol('_FSOUND_Stream_GetSyncPoint@8');
  Pointer(FSOUND_Stream_GetSyncPointInfo) := GetSymbol('_FSOUND_Stream_GetSyncPointInfo@8');
  Pointer(FSOUND_Stream_GetOpenState) := GetSymbol('_FSOUND_Stream_GetOpenState@4');
  Pointer(FSOUND_Stream_SetSubStream) := GetSymbol('_FSOUND_Stream_SetSubStream@8');
  Pointer(FSOUND_Stream_GetNumSubStreams) := GetSymbol('_FSOUND_Stream_GetNumSubStreams@4');
  Pointer(FSOUND_Stream_SetSubStreamSentence) := GetSymbol('_FSOUND_Stream_SetSubStreamSentence@12');
  Pointer(FSOUND_Stream_GetNumTagFields) := GetSymbol('_FSOUND_Stream_GetNumTagFields@8');
  Pointer(FSOUND_Stream_GetTagField) := GetSymbol('_FSOUND_Stream_GetTagField@24');
  Pointer(FSOUND_Stream_FindTagField) := GetSymbol('_FSOUND_Stream_FindTagField@20');
  Pointer(FSOUND_Stream_Net_SetProxy) := GetSymbol('_FSOUND_Stream_Net_SetProxy@4');
  Pointer(FSOUND_Stream_Net_GetLastServerStatus) := GetSymbol('_FSOUND_Stream_Net_GetLastServerStatus@0');
  Pointer(FSOUND_Stream_Net_SetBufferProperties) := GetSymbol('_FSOUND_Stream_Net_SetBufferProperties@12');
  Pointer(FSOUND_Stream_Net_GetBufferProperties) := GetSymbol('_FSOUND_Stream_Net_GetBufferProperties@12');
  Pointer(FSOUND_Stream_Net_SetMetadataCallback) := GetSymbol('_FSOUND_Stream_Net_SetMetadataCallback@12');
  Pointer(FSOUND_Stream_Net_GetStatus) := GetSymbol('_FSOUND_Stream_Net_GetStatus@20');
  Pointer(FSOUND_CD_Play) := GetSymbol('_FSOUND_CD_Play@8');
  Pointer(FSOUND_CD_SetPlayMode) := GetSymbol('_FSOUND_CD_SetPlayMode@8');
  Pointer(FSOUND_CD_Stop) := GetSymbol('_FSOUND_CD_Stop@4');
  Pointer(FSOUND_CD_SetPaused) := GetSymbol('_FSOUND_CD_SetPaused@8');
  Pointer(FSOUND_CD_SetVolume) := GetSymbol('_FSOUND_CD_SetVolume@8');
  Pointer(FSOUND_CD_SetTrackTime) := GetSymbol('_FSOUND_CD_SetTrackTime@8');
  Pointer(FSOUND_CD_OpenTray) := GetSymbol('_FSOUND_CD_OpenTray@8');
  Pointer(FSOUND_CD_GetPaused) := GetSymbol('_FSOUND_CD_GetPaused@4');
  Pointer(FSOUND_CD_GetTrack) := GetSymbol('_FSOUND_CD_GetTrack@4');
  Pointer(FSOUND_CD_GetNumTracks) := GetSymbol('_FSOUND_CD_GetNumTracks@4');
  Pointer(FSOUND_CD_GetVolume) := GetSymbol('_FSOUND_CD_GetVolume@4');
  Pointer(FSOUND_CD_GetTrackLength) := GetSymbol('_FSOUND_CD_GetTrackLength@8');
  Pointer(FSOUND_CD_GetTrackTime) := GetSymbol('_FSOUND_CD_GetTrackTime@4');
  Pointer(FSOUND_DSP_Create) := GetSymbol('_FSOUND_DSP_Create@12');
  Pointer(FSOUND_DSP_Free) := GetSymbol('_FSOUND_DSP_Free@4');
  Pointer(FSOUND_DSP_SetPriority) := GetSymbol('_FSOUND_DSP_SetPriority@8');
  Pointer(FSOUND_DSP_GetPriority) := GetSymbol('_FSOUND_DSP_GetPriority@4');
  Pointer(FSOUND_DSP_SetActive) := GetSymbol('_FSOUND_DSP_SetActive@8');
  Pointer(FSOUND_DSP_GetActive) := GetSymbol('_FSOUND_DSP_GetActive@4');
  Pointer(FSOUND_DSP_GetClearUnit) := GetSymbol('_FSOUND_DSP_GetClearUnit@0');
  Pointer(FSOUND_DSP_GetSFXUnit) := GetSymbol('_FSOUND_DSP_GetSFXUnit@0');
  Pointer(FSOUND_DSP_GetMusicUnit) := GetSymbol('_FSOUND_DSP_GetMusicUnit@0');
  Pointer(FSOUND_DSP_GetClipAndCopyUnit) := GetSymbol('_FSOUND_DSP_GetClipAndCopyUnit@0');
  Pointer(FSOUND_DSP_GetFFTUnit) := GetSymbol('_FSOUND_DSP_GetFFTUnit@0');
  Pointer(FSOUND_DSP_MixBuffers) := GetSymbol('_FSOUND_DSP_MixBuffers@28');
  Pointer(FSOUND_DSP_ClearMixBuffer) := GetSymbol('_FSOUND_DSP_ClearMixBuffer@0');
  Pointer(FSOUND_DSP_GetBufferLength) := GetSymbol('_FSOUND_DSP_GetBufferLength@0');
  Pointer(FSOUND_DSP_GetBufferLengthTotal) := GetSymbol('_FSOUND_DSP_GetBufferLengthTotal@0');
  Pointer(FSOUND_DSP_GetSpectrum) := GetSymbol('_FSOUND_DSP_GetSpectrum@0');
  Pointer(FSOUND_Reverb_SetProperties) := GetSymbol('_FSOUND_Reverb_SetProperties@4');
  Pointer(FSOUND_Reverb_GetProperties) := GetSymbol('_FSOUND_Reverb_GetProperties@4');
  Pointer(FSOUND_Reverb_SetChannelProperties) := GetSymbol('_FSOUND_Reverb_SetChannelProperties@8');
  Pointer(FSOUND_Reverb_GetChannelProperties) := GetSymbol('_FSOUND_Reverb_GetChannelProperties@8');
  Pointer(FSOUND_Record_SetDriver) := GetSymbol('_FSOUND_Record_SetDriver@4');
  Pointer(FSOUND_Record_GetNumDrivers) := GetSymbol('_FSOUND_Record_GetNumDrivers@0');
  Pointer(FSOUND_Record_GetDriverName) := GetSymbol('_FSOUND_Record_GetDriverName@4');
  Pointer(FSOUND_Record_GetDriver) := GetSymbol('_FSOUND_Record_GetDriver@0');
  Pointer(FSOUND_Record_StartSample) := GetSymbol('_FSOUND_Record_StartSample@8');
  Pointer(FSOUND_Record_Stop) := GetSymbol('_FSOUND_Record_Stop@0');
  Pointer(FSOUND_Record_GetPosition) := GetSymbol('_FSOUND_Record_GetPosition@0');
  Pointer(FSOUND_File_SetCallbacks) := GetSymbol('_FSOUND_File_SetCallbacks@20');
  Pointer(FMUSIC_LoadSong) := GetSymbol('_FMUSIC_LoadSong@4');
  Pointer(FMUSIC_LoadSongEx) := GetSymbol('_FMUSIC_LoadSongEx@24');
  Pointer(FMUSIC_GetOpenState) := GetSymbol('_FMUSIC_GetOpenState@4');
  Pointer(FMUSIC_FreeSong) := GetSymbol('_FMUSIC_FreeSong@4');
  Pointer(FMUSIC_PlaySong) := GetSymbol('_FMUSIC_PlaySong@4');
  Pointer(FMUSIC_StopSong) := GetSymbol('_FMUSIC_StopSong@4');
  Pointer(FMUSIC_StopAllSongs) := GetSymbol('_FMUSIC_StopAllSongs@0');
  Pointer(FMUSIC_SetZxxCallback) := GetSymbol('_FMUSIC_SetZxxCallback@8');
  Pointer(FMUSIC_SetRowCallback) := GetSymbol('_FMUSIC_SetRowCallback@12');
  Pointer(FMUSIC_SetOrderCallback) := GetSymbol('_FMUSIC_SetOrderCallback@12');
  Pointer(FMUSIC_SetInstCallback) := GetSymbol('_FMUSIC_SetInstCallback@12');
  Pointer(FMUSIC_SetSample) := GetSymbol('_FMUSIC_SetSample@12');
  Pointer(FMUSIC_SetUserData) := GetSymbol('_FMUSIC_SetUserData@8');
  Pointer(FMUSIC_OptimizeChannels) := GetSymbol('_FMUSIC_OptimizeChannels@12');
  Pointer(FMUSIC_SetReverb) := GetSymbol('_FMUSIC_SetReverb@4');
  Pointer(FMUSIC_SetLooping) := GetSymbol('_FMUSIC_SetLooping@8');
  Pointer(FMUSIC_SetOrder) := GetSymbol('_FMUSIC_SetOrder@8');
  Pointer(FMUSIC_SetPaused) := GetSymbol('_FMUSIC_SetPaused@8');
  Pointer(FMUSIC_SetMasterVolume) := GetSymbol('_FMUSIC_SetMasterVolume@8');
  Pointer(FMUSIC_SetMasterSpeed) := GetSymbol('_FMUSIC_SetMasterSpeed@8');
  Pointer(FMUSIC_SetPanSeperation) := GetSymbol('_FMUSIC_SetPanSeperation@8');
  Pointer(FMUSIC_GetName) := GetSymbol('_FMUSIC_GetName@4');
  Pointer(FMUSIC_GetType) := GetSymbol('_FMUSIC_GetType@4');
  Pointer(FMUSIC_GetNumOrders) := GetSymbol('_FMUSIC_GetNumOrders@4');
  Pointer(FMUSIC_GetNumPatterns) := GetSymbol('_FMUSIC_GetNumPatterns@4');
  Pointer(FMUSIC_GetNumInstruments) := GetSymbol('_FMUSIC_GetNumInstruments@4');
  Pointer(FMUSIC_GetNumSamples) := GetSymbol('_FMUSIC_GetNumSamples@4');
  Pointer(FMUSIC_GetNumChannels) := GetSymbol('_FMUSIC_GetNumChannels@4');
  Pointer(FMUSIC_GetSample) := GetSymbol('_FMUSIC_GetSample@8');
  Pointer(FMUSIC_GetPatternLength) := GetSymbol('_FMUSIC_GetPatternLength@8');
  Pointer(FMUSIC_IsFinished) := GetSymbol('_FMUSIC_IsFinished@4');
  Pointer(FMUSIC_IsPlaying) := GetSymbol('_FMUSIC_IsPlaying@4');
  Pointer(FMUSIC_GetMasterVolume) := GetSymbol('_FMUSIC_GetMasterVolume@4');
  Pointer(FMUSIC_GetGlobalVolume) := GetSymbol('_FMUSIC_GetGlobalVolume@4');
  Pointer(FMUSIC_GetOrder) := GetSymbol('_FMUSIC_GetOrder@4');
  Pointer(FMUSIC_GetPattern) := GetSymbol('_FMUSIC_GetPattern@4');
  Pointer(FMUSIC_GetSpeed) := GetSymbol('_FMUSIC_GetSpeed@4');
  Pointer(FMUSIC_GetBPM) := GetSymbol('_FMUSIC_GetBPM@4');
  Pointer(FMUSIC_GetRow) := GetSymbol('_FMUSIC_GetRow@4');
  Pointer(FMUSIC_GetPaused) := GetSymbol('_FMUSIC_GetPaused@4');
  Pointer(FMUSIC_GetTime) := GetSymbol('_FMUSIC_GetTime@4');
  Pointer(FMUSIC_GetRealChannel) := GetSymbol('_FMUSIC_GetRealChannel@8');
  Pointer(FMUSIC_GetUserData) := GetSymbol('_FMUSIC_GetUserData@4');
  {$ENDIF}
  Exit( True );
end;

function FMOD_ErrorString(ErrorCode: TFModErrors): AnsiString;
begin
  case ErrorCode of
    FMOD_ERR_NONE:              Result := 'No errors';
    FMOD_ERR_BUSY:              Result := 'Cannot call this command after FSOUND_Init.  Call FSOUND_Close first';
    FMOD_ERR_UNINITIALIZED:     Result := 'This command failed because FSOUND_Init was not called';
    FMOD_ERR_PLAY:              Result := 'Playing the sound failed';
    FMOD_ERR_INIT:              Result := 'Error initializing output device';
    FMOD_ERR_ALLOCATED:         Result := 'The output device is already in use and cannot be reused';
    FMOD_ERR_OUTPUT_FORMAT:     Result := 'Soundcard does not support the features needed for this soundsystem (16bit stereo output)';
    FMOD_ERR_COOPERATIVELEVEL:  Result := 'Error setting cooperative level for hardware';
    FMOD_ERR_CREATEBUFFER:      Result := 'Error creating hardware sound buffer';
    FMOD_ERR_FILE_NOTFOUND:     Result := 'File not found';
    FMOD_ERR_FILE_FORMAT:       Result := 'Unknown file format';
    FMOD_ERR_FILE_BAD:          Result := 'Error loading file';
    FMOD_ERR_MEMORY:            Result := 'Not enough memory or resources';
    FMOD_ERR_VERSION:           Result := 'The version number of this file format is not supported';
    FMOD_ERR_INVALID_PARAM:     Result := 'An invalid parameter was passed to this function';
    FMOD_ERR_NO_EAX:            Result := 'Tried to use an EAX command on a non EAX enabled channel or output';
    FMOD_ERR_CHANNEL_ALLOC:     Result := 'Failed to allocate a new channel';
    FMOD_ERR_RECORD:            Result := 'Recording is not supported on this machine';
    FMOD_ERR_MEDIAPLAYER:       Result := 'Required Mediaplayer codec is not installed';
  else
    Result := 'Unknown error';
  end;
end;

finalization
  if FMOD <> nil then
  begin
    FreeAndNil( FMOD );
    Set8087CW(Saved8087CW);
  end;

end.

