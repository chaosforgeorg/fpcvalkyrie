unit vfmodlibrary;
{$include ../src/valkyrie.inc}
{$MACRO ON}
{$PACKRECORDS C}
{$Z4}
interface
uses Classes, SysUtils, vlibrary;

const
{$IFDEF WINDOWS}
  FMODDefaultPath = 'fmod64.dll';
{$ELSE}
  {$IFDEF DARWIN}
  FMODDefaultPath = 'libfmod.dylib';
  {$ELSE}
  FMODDefaultPath = 'libfmod.so';
  {$ENDIF}
{$ENDIF}

// Version 2.2.23
const FMOD_VERSION = $00020223;

{$include vfmodconst.inc}
{$include vfmodtypes.inc}

{$IFDEF UNIX}
  {$DEFINE calldecl := cdecl}
{$ELSE}
  {$DEFINE calldecl := stdcall}
{$ENDIF}

var
// Initialization / Global functions
  FMOD_System_Create: function(system: PPFMOD_SYSTEM; headerversion: LongWord): FMOD_RESULT; cdecl;
  FMOD_System_Release: function(system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;

  FMOD_System_SetOutput: function(system: PFMOD_SYSTEM; output: FMOD_OUTPUTTYPE): FMOD_RESULT; cdecl;
  FMOD_System_GetOutput: function(system: PFMOD_SYSTEM; var output: FMOD_OUTPUTTYPE): FMOD_RESULT; cdecl;
  FMOD_System_GetNumDrivers: function(system: PFMOD_SYSTEM; var numdrivers: Integer): FMOD_RESULT; cdecl;
  FMOD_System_GetDriverInfo: function(system: PFMOD_SYSTEM; id: Integer; name: PChar; namelen: Integer; guid: PFMOD_GUID; systemrate: PInteger; speakermode: PFMOD_SPEAKERMODE; speakermodechannels: PInteger): FMOD_RESULT; cdecl;
  FMOD_System_SetDriver: function(system: PFMOD_SYSTEM; driver: Integer): FMOD_RESULT; cdecl;
  FMOD_System_GetDriver: function(system: PFMOD_SYSTEM; var driver: Integer): FMOD_RESULT; cdecl;
  FMOD_System_SetSoftwareChannels: function(system: PFMOD_SYSTEM; numsoftwarechannels: Integer): FMOD_RESULT; cdecl;
  FMOD_System_GetSoftwareChannels: function(system: PFMOD_SYSTEM; var numsoftwarechannels: Integer): FMOD_RESULT; cdecl;
  FMOD_System_SetSoftwareFormat: function(system: PFMOD_SYSTEM; samplerate: Integer; speakermode: FMOD_SPEAKERMODE; numrawspeakers: Integer): FMOD_RESULT; cdecl;
  FMOD_System_GetSoftwareFormat: function(system: PFMOD_SYSTEM; var samplerate: Integer; var speakermode: FMOD_SPEAKERMODE; var numrawspeakers: Integer): FMOD_RESULT; cdecl;
  FMOD_System_SetDSPBufferSize: function(system: PFMOD_SYSTEM; bufferlength: LongWord; numbuffers: Integer): FMOD_RESULT; cdecl;
  FMOD_System_GetDSPBufferSize: function(system: PFMOD_SYSTEM; var bufferlength: LongWord; var numbuffers: Integer): FMOD_RESULT; cdecl;
  FMOD_System_SetFileSystem: function(system: PFMOD_SYSTEM; useropen: FMOD_FILE_OPEN_CALLBACK; userclose: FMOD_FILE_CLOSE_CALLBACK; userread: FMOD_FILE_READ_CALLBACK; userseek: FMOD_FILE_SEEK_CALLBACK; userasyncread: FMOD_FILE_ASYNCREAD_CALLBACK; userasynccancel: FMOD_FILE_ASYNCCANCEL_CALLBACK; blockalign: Integer): FMOD_RESULT; cdecl;
  FMOD_System_AttachFileSystem: function(system: PFMOD_SYSTEM; useropen: FMOD_FILE_OPEN_CALLBACK; userclose: FMOD_FILE_CLOSE_CALLBACK; userread: FMOD_FILE_READ_CALLBACK; userseek: FMOD_FILE_SEEK_CALLBACK): FMOD_RESULT; cdecl;
  FMOD_System_SetAdvancedSettings: function(system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): FMOD_RESULT; cdecl;
  FMOD_System_GetAdvancedSettings: function(system: PFMOD_SYSTEM; settings: PFMOD_ADVANCEDSETTINGS): FMOD_RESULT; cdecl;
  FMOD_System_SetCallback: function(system: PFMOD_SYSTEM; callback: FMOD_SYSTEM_CALLBACK; callbackmask: FMOD_SYSTEM_CALLBACK_TYPE): FMOD_RESULT; cdecl;

  FMOD_System_Init: function(system: PFMOD_SYSTEM; maxchannels: Integer; flags: FMOD_INITFLAGS; extradriverdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_System_Close: function(system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;

  FMOD_System_Update: function(system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;
  FMOD_System_SetSpeakerPosition: function(system: PFMOD_SYSTEM; speaker: FMOD_SPEAKER; x: Single; y: Single; active: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_System_GetSpeakerPosition: function(system: PFMOD_SYSTEM; speaker: FMOD_SPEAKER; var x: Single; var y: Single; var active: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_System_SetStreamBufferSize: function(system: PFMOD_SYSTEM; filebuffersize: LongWord; filebuffersizetype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_System_GetStreamBufferSize: function(system: PFMOD_SYSTEM; var filebuffersize: LongWord; var filebuffersizetype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_System_Set3DSettings: function(system: PFMOD_SYSTEM; dopplerscale: Single; distancefactor: Single; rolloffscale: Single): FMOD_RESULT; cdecl;
  FMOD_System_Get3DSettings: function(system: PFMOD_SYSTEM; var dopplerscale: Single; var distancefactor: Single; var rolloffscale: Single): FMOD_RESULT; cdecl;
  FMOD_System_Set3DNumListeners: function(system: PFMOD_SYSTEM; numlisteners: Integer): FMOD_RESULT; cdecl;
  FMOD_System_Get3DNumListeners: function(system: PFMOD_SYSTEM; var numlisteners: Integer): FMOD_RESULT; cdecl;
  FMOD_System_Set3DListenerAttributes: function(system: PFMOD_SYSTEM; listener: Integer; const pos: PFMOD_VECTOR; const vel: PFMOD_VECTOR; const forward: PFMOD_VECTOR; const up: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_System_Get3DListenerAttributes: function(system: PFMOD_SYSTEM; listener: Integer; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR; forward: PFMOD_VECTOR; up: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_System_Set3DRolloffCallback: function(system: PFMOD_SYSTEM; callback: FMOD_3D_ROLLOFF_CALLBACK): FMOD_RESULT; cdecl;
  FMOD_System_MixerSuspend: function(system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;
  FMOD_System_MixerResume: function(system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;
  FMOD_System_GetDefaultMixMatrix: function(system: PFMOD_SYSTEM; sourcespeakermode: FMOD_SPEAKERMODE; targetspeakermode: FMOD_SPEAKERMODE; matrix: PSingle; matrixhop: Integer): FMOD_RESULT; cdecl;
  FMOD_System_GetSpeakerModeChannels: function(system: PFMOD_SYSTEM; mode: FMOD_SPEAKERMODE; var channels: Integer): FMOD_RESULT; cdecl;

  FMOD_System_GetVersion: function(system: PFMOD_SYSTEM; var version: LongWord): FMOD_RESULT; cdecl;
  FMOD_System_GetOutputHandle: function(system: PFMOD_SYSTEM; var handle: Pointer): FMOD_RESULT; cdecl;
  FMOD_System_GetChannelsPlaying: function(system: PFMOD_SYSTEM; var channels: Integer; var realchannels: Integer): FMOD_RESULT; cdecl;
  FMOD_System_GetCPUUsage: function(system: PFMOD_SYSTEM; usage: PFMOD_CPU_USAGE): FMOD_RESULT; cdecl;
  FMOD_System_GetFileUsage: function(system: PFMOD_SYSTEM; var sampleBytesRead: Int64; var streamBytesRead: Int64; var otherBytesRead: Int64): FMOD_RESULT; cdecl;

  FMOD_System_CreateSound: function(system: PFMOD_SYSTEM; const name_or_data: PChar; mode: FMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): FMOD_RESULT; cdecl;
  FMOD_System_CreateStream: function(system: PFMOD_SYSTEM; const name_or_data: PChar; mode: FMOD_MODE; exinfo: PFMOD_CREATESOUNDEXINFO; sound: PPFMOD_SOUND): FMOD_RESULT; cdecl;
  FMOD_System_CreateChannelGroup: function(system: PFMOD_SYSTEM; const name: PChar; channelgroup: PPFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;
  FMOD_System_CreateSoundGroup: function(system: PFMOD_SYSTEM; const name: PChar; soundgroup: PPFMOD_SOUNDGROUP): FMOD_RESULT; cdecl;
  FMOD_System_CreateReverb3D: function(system: PFMOD_SYSTEM; var reverb: PFMOD_REVERB3D): FMOD_RESULT; cdecl;
  FMOD_System_PlaySound: function(system: PFMOD_SYSTEM; sound: PFMOD_SOUND; channelgroup: PFMOD_CHANNELGROUP; paused: FMOD_BOOL; channel: PPFMOD_CHANNEL): FMOD_RESULT; cdecl;
  FMOD_System_PlayDSP: function(system: PFMOD_SYSTEM; dsp: PFMOD_DSP; channelgroup: PFMOD_CHANNELGROUP; paused: FMOD_BOOL; channel: PPFMOD_CHANNEL): FMOD_RESULT; cdecl;
  FMOD_System_GetChannel: function(system: PFMOD_SYSTEM; channelid: Integer; var channel: PFMOD_CHANNEL): FMOD_RESULT; cdecl;
  FMOD_System_GetMasterChannelGroup: function(system: PFMOD_SYSTEM; var channelgroup: PFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;
  FMOD_System_GetMasterSoundGroup: function(system: PFMOD_SYSTEM; var soundgroup: PFMOD_SOUNDGROUP): FMOD_RESULT; cdecl;

  FMOD_Sound_Release: function(sound: PFMOD_SOUND): FMOD_RESULT; cdecl;
  FMOD_Sound_GetSystemObject: function(sound: PFMOD_SOUND; var system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;

  FMOD_Sound_Lock: function(sound: PFMOD_SOUND; offset: LongWord; length: LongWord; var ptr1: Pointer; var ptr2: Pointer; var len1: LongWord; var len2: LongWord): FMOD_RESULT; cdecl;
  FMOD_Sound_Unlock: function(sound: PFMOD_SOUND; ptr1: Pointer; ptr2: Pointer; len1: LongWord; len2: LongWord): FMOD_RESULT; cdecl;
  FMOD_Sound_SetDefaults: function(sound: PFMOD_SOUND; frequency: Single; priority: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetDefaults: function(sound: PFMOD_SOUND; var frequency: Single; var priority: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_Set3DMinMaxDistance: function(sound: PFMOD_SOUND; min: Single; max: Single): FMOD_RESULT; cdecl;
  FMOD_Sound_Get3DMinMaxDistance: function(sound: PFMOD_SOUND; var min: Single; var max: Single): FMOD_RESULT; cdecl;
  FMOD_Sound_Set3DConeSettings: function(sound: PFMOD_SOUND; insideconeangle: Single; outsideconeangle: Single; outsidevolume: Single): FMOD_RESULT; cdecl;
  FMOD_Sound_Get3DConeSettings: function(sound: PFMOD_SOUND; var insideconeangle: Single; var outsideconeangle: Single; var outsidevolume: Single): FMOD_RESULT; cdecl;
  FMOD_Sound_Set3DCustomRolloff: function(sound: PFMOD_SOUND; points: PFMOD_VECTOR; numpoints: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_Get3DCustomRolloff: function(sound: PFMOD_SOUND; var points: PFMOD_VECTOR; var numpoints: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetSubSound: function(sound: PFMOD_SOUND; index: Integer; var subsound: PFMOD_SOUND): FMOD_RESULT; cdecl;
  FMOD_Sound_GetSubSoundParent: function(sound: PFMOD_SOUND; var parentsound: PFMOD_SOUND): FMOD_RESULT; cdecl;
  FMOD_Sound_GetName: function(sound: PFMOD_SOUND; name: PChar; namelen: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetLength: function(sound: PFMOD_SOUND; var length: LongWord; lengthtype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_Sound_GetFormat: function(sound: PFMOD_SOUND; var type_: FMOD_SOUND_TYPE; var format: FMOD_SOUND_FORMAT; var channels: Integer; var bits: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetNumSubSounds: function(sound: PFMOD_SOUND; var numsubsounds: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetNumTags: function(sound: PFMOD_SOUND; var numtags: Integer; var numtagsupdated: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetTag: function(sound: PFMOD_SOUND; const name: PChar; index: Integer; var tag: FMOD_TAG): FMOD_RESULT; cdecl;
  FMOD_Sound_GetOpenState: function(sound: PFMOD_SOUND; var openstate: FMOD_OPENSTATE; var percentbuffered: LongWord; var starving: FMOD_BOOL; var diskbusy: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Sound_ReadData: function(sound: PFMOD_SOUND; buffer: Pointer; length: LongWord; var read: LongWord): FMOD_RESULT; cdecl;
  FMOD_Sound_SeekData: function(sound: PFMOD_SOUND; pcm: LongWord): FMOD_RESULT; cdecl;

  FMOD_Sound_SetSoundGroup: function(sound: PFMOD_SOUND; soundgroup: PFMOD_SOUNDGROUP): FMOD_RESULT; cdecl;
  FMOD_Sound_GetSoundGroup: function(sound: PFMOD_SOUND; var soundgroup: PFMOD_SOUNDGROUP): FMOD_RESULT; cdecl;

  FMOD_Sound_GetNumSyncPoints: function(sound: PFMOD_SOUND; var numsyncpoints: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetSyncPoint: function(sound: PFMOD_SOUND; index: Integer; var point: PFMOD_SYNCPOINT): FMOD_RESULT; cdecl;
  FMOD_Sound_GetSyncPointInfo: function(sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT; name: PChar; namelen: Integer; var offset: LongWord; offsettype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_Sound_AddSyncPoint: function(sound: PFMOD_SOUND; offset: LongWord; offsettype: FMOD_TIMEUNIT; const name: PChar; var point: PFMOD_SYNCPOINT): FMOD_RESULT; cdecl;
  FMOD_Sound_DeleteSyncPoint: function(sound: PFMOD_SOUND; point: PFMOD_SYNCPOINT): FMOD_RESULT; cdecl;

  FMOD_Sound_SetMode: function(sound: PFMOD_SOUND; mode: FMOD_MODE): FMOD_RESULT; cdecl;
  FMOD_Sound_GetMode: function(sound: PFMOD_SOUND; var mode: FMOD_MODE): FMOD_RESULT; cdecl;
  FMOD_Sound_SetLoopCount: function(sound: PFMOD_SOUND; loopcount: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetLoopCount: function(sound: PFMOD_SOUND; var loopcount: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_SetLoopPoints: function(sound: PFMOD_SOUND; loopstart: LongWord; loopstarttype: FMOD_TIMEUNIT; loopend: LongWord; loopendtype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_Sound_GetLoopPoints: function(sound: PFMOD_SOUND; var loopstart: LongWord; loopstarttype: FMOD_TIMEUNIT; var loopend: LongWord; loopendtype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;

  FMOD_Sound_GetMusicNumChannels: function(sound: PFMOD_SOUND; var numchannels: Integer): FMOD_RESULT; cdecl;
  FMOD_Sound_SetMusicChannelVolume: function(sound: PFMOD_SOUND; channel: Integer; volume: Single): FMOD_RESULT; cdecl;
  FMOD_Sound_GetMusicChannelVolume: function(sound: PFMOD_SOUND; channel: Integer; var volume: Single): FMOD_RESULT; cdecl;
  FMOD_Sound_SetMusicSpeed: function(sound: PFMOD_SOUND; speed: Single): FMOD_RESULT; cdecl;
  FMOD_Sound_GetMusicSpeed: function(sound: PFMOD_SOUND; var speed: Single): FMOD_RESULT; cdecl;

  FMOD_Sound_SetUserData: function(sound: PFMOD_SOUND; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_Sound_GetUserData: function(sound: PFMOD_SOUND; var userdata: Pointer): FMOD_RESULT; cdecl;

var
  FMOD_Channel_GetSystemObject: function(channel: PFMOD_CHANNEL; var system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;

  FMOD_Channel_Stop: function(channel: PFMOD_CHANNEL): FMOD_RESULT; cdecl;
  FMOD_Channel_SetPaused: function(channel: PFMOD_CHANNEL; paused: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_GetPaused: function(channel: PFMOD_CHANNEL; var paused: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_SetVolume: function(channel: PFMOD_CHANNEL; volume: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_GetVolume: function(channel: PFMOD_CHANNEL; var volume: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetVolumeRamp: function(channel: PFMOD_CHANNEL; ramp: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_GetVolumeRamp: function(channel: PFMOD_CHANNEL; var ramp: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_GetAudibility: function(channel: PFMOD_CHANNEL; var audibility: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetPitch: function(channel: PFMOD_CHANNEL; pitch: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_GetPitch: function(channel: PFMOD_CHANNEL; var pitch: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetMute: function(channel: PFMOD_CHANNEL; mute: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_GetMute: function(channel: PFMOD_CHANNEL; var mute: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_SetReverbProperties: function(channel: PFMOD_CHANNEL; instance: Integer; wet: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_GetReverbProperties: function(channel: PFMOD_CHANNEL; instance: Integer; var wet: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetLowPassGain: function(channel: PFMOD_CHANNEL; gain: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_GetLowPassGain: function(channel: PFMOD_CHANNEL; var gain: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetMode: function(channel: PFMOD_CHANNEL; mode: FMOD_MODE): FMOD_RESULT; cdecl;
  FMOD_Channel_GetMode: function(channel: PFMOD_CHANNEL; var mode: FMOD_MODE): FMOD_RESULT; cdecl;
  FMOD_Channel_SetCallback: function(channel: PFMOD_CHANNEL; callback: FMOD_CHANNELCONTROL_CALLBACK): FMOD_RESULT; cdecl;
  FMOD_Channel_IsPlaying: function(channel: PFMOD_CHANNEL; var isplaying: FMOD_BOOL): FMOD_RESULT; cdecl;

  FMOD_Channel_SetPan: function(channel: PFMOD_CHANNEL; pan: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetMixLevelsOutput: function(channel: PFMOD_CHANNEL; frontleft: Single; frontright: Single; center: Single; lfe: Single; surroundleft: Single; surroundright: Single; backleft: Single; backright: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetMixLevelsInput: function(channel: PFMOD_CHANNEL; levels: PSingle; numlevels: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_SetMixMatrix: function(channel: PFMOD_CHANNEL; matrix: PSingle; outchannels: Integer; inchannels: Integer; inchannel_hop: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_GetMixMatrix: function(channel: PFMOD_CHANNEL; matrix: PSingle; var outchannels: Integer; var inchannels: Integer; inchannel_hop: Integer): FMOD_RESULT; cdecl;

  FMOD_Channel_GetDSPClock: function(channel: PFMOD_CHANNEL; var dspclock: UInt64; var parentclock: UInt64): FMOD_RESULT; cdecl;
  FMOD_Channel_SetDelay: function(channel: PFMOD_CHANNEL; dspclock_start: UInt64; dspclock_end: UInt64; stopchannels: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_GetDelay: function(channel: PFMOD_CHANNEL; var dspclock_start: UInt64; var dspclock_end: UInt64; var stopchannels: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_AddFadePoint: function(channel: PFMOD_CHANNEL; dspclock: UInt64; volume: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetFadePointRamp: function(channel: PFMOD_CHANNEL; dspclock: UInt64; volume: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_RemoveFadePoints: function(channel: PFMOD_CHANNEL; dspclock_start: UInt64; dspclock_end: UInt64): FMOD_RESULT; cdecl;
  FMOD_Channel_GetFadePoints: function(channel: PFMOD_CHANNEL; var numpoints: UInt32; point_dspclock: PUInt64; point_volume: PSingle): FMOD_RESULT; cdecl;

  FMOD_Channel_GetDSP: function(channel: PFMOD_CHANNEL; index: Integer; var dsp: PFMOD_DSP): FMOD_RESULT; cdecl;
  FMOD_Channel_AddDSP: function(channel: PFMOD_CHANNEL; index: Integer; dsp: PFMOD_DSP): FMOD_RESULT; cdecl;
  FMOD_Channel_RemoveDSP: function(channel: PFMOD_CHANNEL; dsp: PFMOD_DSP): FMOD_RESULT; cdecl;
  FMOD_Channel_GetNumDSPs: function(channel: PFMOD_CHANNEL; var numdsps: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_SetDSPIndex: function(channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; index: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_GetDSPIndex: function(channel: PFMOD_CHANNEL; dsp: PFMOD_DSP; var index: Integer): FMOD_RESULT; cdecl;

  FMOD_Channel_Set3DAttributes: function(channel: PFMOD_CHANNEL; const pos: PFMOD_VECTOR; const vel: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DAttributes: function(channel: PFMOD_CHANNEL; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DMinMaxDistance: function(channel: PFMOD_CHANNEL; mindistance: Single; maxdistance: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DMinMaxDistance: function(channel: PFMOD_CHANNEL; var mindistance: Single; var maxdistance: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DConeSettings: function(channel: PFMOD_CHANNEL; insideconeangle: Single; outsideconeangle: Single; outsidevolume: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DConeSettings: function(channel: PFMOD_CHANNEL; var insideconeangle: Single; var outsideconeangle: Single; var outsidevolume: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DConeOrientation: function(channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DConeOrientation: function(channel: PFMOD_CHANNEL; orientation: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DCustomRolloff: function(channel: PFMOD_CHANNEL; points: PFMOD_VECTOR; numpoints: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DCustomRolloff: function(channel: PFMOD_CHANNEL; var points: PFMOD_VECTOR; var numpoints: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DOcclusion: function(channel: PFMOD_CHANNEL; directocclusion: Single; reverbocclusion: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DOcclusion: function(channel: PFMOD_CHANNEL; var directocclusion: Single; var reverbocclusion: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DSpread: function(channel: PFMOD_CHANNEL; angle: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DSpread: function(channel: PFMOD_CHANNEL; var angle: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DLevel: function(channel: PFMOD_CHANNEL; level: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DLevel: function(channel: PFMOD_CHANNEL; var level: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DDopplerLevel: function(channel: PFMOD_CHANNEL; level: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DDopplerLevel: function(channel: PFMOD_CHANNEL; var level: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Set3DDistanceFilter: function(channel: PFMOD_CHANNEL; custom: FMOD_BOOL; customLevel: Single; centerFreq: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_Get3DDistanceFilter: function(channel: PFMOD_CHANNEL; var custom: FMOD_BOOL; var customLevel: Single; var centerFreq: Single): FMOD_RESULT; cdecl;

  FMOD_Channel_SetUserData: function(channel: PFMOD_CHANNEL; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_Channel_GetUserData: function(channel: PFMOD_CHANNEL; var userdata: Pointer): FMOD_RESULT; cdecl;

  FMOD_Channel_SetFrequency: function(channel: PFMOD_CHANNEL; frequency: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_GetFrequency: function(channel: PFMOD_CHANNEL; var frequency: Single): FMOD_RESULT; cdecl;
  FMOD_Channel_SetPriority: function(channel: PFMOD_CHANNEL; priority: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_GetPriority: function(channel: PFMOD_CHANNEL; var priority: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_SetPosition: function(channel: PFMOD_CHANNEL; position: LongWord; postype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_Channel_GetPosition: function(channel: PFMOD_CHANNEL; var position: LongWord; postype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_Channel_SetChannelGroup: function(channel: PFMOD_CHANNEL; channelgroup: PFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;
  FMOD_Channel_GetChannelGroup: function(channel: PFMOD_CHANNEL; var channelgroup: PFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;
  FMOD_Channel_SetLoopCount: function(channel: PFMOD_CHANNEL; loopcount: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_GetLoopCount: function(channel: PFMOD_CHANNEL; var loopcount: Integer): FMOD_RESULT; cdecl;
  FMOD_Channel_SetLoopPoints: function(channel: PFMOD_CHANNEL; loopstart: LongWord; loopstarttype: FMOD_TIMEUNIT; loopend: LongWord; loopendtype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;
  FMOD_Channel_GetLoopPoints: function(channel: PFMOD_CHANNEL; var loopstart: LongWord; loopstarttype: FMOD_TIMEUNIT; var loopend: LongWord; loopendtype: FMOD_TIMEUNIT): FMOD_RESULT; cdecl;

  FMOD_Channel_IsVirtual: function(channel: PFMOD_CHANNEL; var isvirtual: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_Channel_GetCurrentSound: function(channel: PFMOD_CHANNEL; var sound: PFMOD_SOUND): FMOD_RESULT; cdecl;
  FMOD_Channel_GetIndex: function(channel: PFMOD_CHANNEL; var index: Integer): FMOD_RESULT; cdecl;

var
  FMOD_ChannelGroup_GetSystemObject: function(channelgroup: PFMOD_CHANNELGROUP; var system: PFMOD_SYSTEM): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_Stop: function(channelgroup: PFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetPaused: function(channelgroup: PFMOD_CHANNELGROUP; paused: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetPaused: function(channelgroup: PFMOD_CHANNELGROUP; var paused: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetVolume: function(channelgroup: PFMOD_CHANNELGROUP; volume: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetVolume: function(channelgroup: PFMOD_CHANNELGROUP; var volume: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetVolumeRamp: function(channelgroup: PFMOD_CHANNELGROUP; ramp: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetVolumeRamp: function(channelgroup: PFMOD_CHANNELGROUP; var ramp: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetAudibility: function(channelgroup: PFMOD_CHANNELGROUP; var audibility: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetPitch: function(channelgroup: PFMOD_CHANNELGROUP; pitch: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetPitch: function(channelgroup: PFMOD_CHANNELGROUP; var pitch: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetMute: function(channelgroup: PFMOD_CHANNELGROUP; mute: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetMute: function(channelgroup: PFMOD_CHANNELGROUP; var mute: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetReverbProperties: function(channelgroup: PFMOD_CHANNELGROUP; instance: Integer; wet: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetReverbProperties: function(channelgroup: PFMOD_CHANNELGROUP; instance: Integer; var wet: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetLowPassGain: function(channelgroup: PFMOD_CHANNELGROUP; gain: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetLowPassGain: function(channelgroup: PFMOD_CHANNELGROUP; var gain: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetMode: function(channelgroup: PFMOD_CHANNELGROUP; mode: FMOD_MODE): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetMode: function(channelgroup: PFMOD_CHANNELGROUP; var mode: FMOD_MODE): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetCallback: function(channelgroup: PFMOD_CHANNELGROUP; callback: FMOD_CHANNELCONTROL_CALLBACK): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_IsPlaying: function(channelgroup: PFMOD_CHANNELGROUP; var isplaying: FMOD_BOOL): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_SetPan: function(channelgroup: PFMOD_CHANNELGROUP; pan: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetMixLevelsOutput: function(channelgroup: PFMOD_CHANNELGROUP; frontleft: Single; frontright: Single; center: Single; lfe: Single; surroundleft: Single; surroundright: Single; backleft: Single; backright: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetMixLevelsInput: function(channelgroup: PFMOD_CHANNELGROUP; levels: PSingle; numlevels: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetMixMatrix: function(channelgroup: PFMOD_CHANNELGROUP; matrix: PSingle; outchannels: Integer; inchannels: Integer; inchannel_hop: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetMixMatrix: function(channelgroup: PFMOD_CHANNELGROUP; matrix: PSingle; var outchannels: Integer; var inchannels: Integer; inchannel_hop: Integer): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_GetDSPClock: function(channelgroup: PFMOD_CHANNELGROUP; var dspclock: UInt64; var parentclock: UInt64): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetDelay: function(channelgroup: PFMOD_CHANNELGROUP; dspclock_start: UInt64; dspclock_end: UInt64; stopchannels: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetDelay: function(channelgroup: PFMOD_CHANNELGROUP; var dspclock_start: UInt64; var dspclock_end: UInt64; var stopchannels: FMOD_BOOL): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_AddFadePoint: function(channelgroup: PFMOD_CHANNELGROUP; dspclock: UInt64; volume: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetFadePointRamp: function(channelgroup: PFMOD_CHANNELGROUP; dspclock: UInt64; volume: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_RemoveFadePoints: function(channelgroup: PFMOD_CHANNELGROUP; dspclock_start: UInt64; dspclock_end: UInt64): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetFadePoints: function(channelgroup: PFMOD_CHANNELGROUP; var numpoints: UInt32; point_dspclock: PUInt64; point_volume: PSingle): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_GetDSP: function(channelgroup: PFMOD_CHANNELGROUP; index: Integer; var dsp: PFMOD_DSP): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_AddDSP: function(channelgroup: PFMOD_CHANNELGROUP; index: Integer; dsp: PFMOD_DSP): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_RemoveDSP: function(channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetNumDSPs: function(channelgroup: PFMOD_CHANNELGROUP; var numdsps: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_SetDSPIndex: function(channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; index: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetDSPIndex: function(channelgroup: PFMOD_CHANNELGROUP; dsp: PFMOD_DSP; var index: Integer): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_Set3DAttributes: function(channelgroup: PFMOD_CHANNELGROUP; const pos: PFMOD_VECTOR; const vel: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DAttributes: function(channelgroup: PFMOD_CHANNELGROUP; pos: PFMOD_VECTOR; vel: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DMinMaxDistance: function(channelgroup: PFMOD_CHANNELGROUP; mindistance: Single; maxdistance: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DMinMaxDistance: function(channelgroup: PFMOD_CHANNELGROUP; var mindistance: Single; var maxdistance: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DConeSettings: function(channelgroup: PFMOD_CHANNELGROUP; insideconeangle: Single; outsideconeangle: Single; outsidevolume: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DConeSettings: function(channelgroup: PFMOD_CHANNELGROUP; var insideconeangle: Single; var outsideconeangle: Single; var outsidevolume: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DConeOrientation: function(channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DConeOrientation: function(channelgroup: PFMOD_CHANNELGROUP; orientation: PFMOD_VECTOR): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DCustomRolloff: function(channelgroup: PFMOD_CHANNELGROUP; points: PFMOD_VECTOR; numpoints: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DCustomRolloff: function(channelgroup: PFMOD_CHANNELGROUP; var points: PFMOD_VECTOR; var numpoints: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DOcclusion: function(channelgroup: PFMOD_CHANNELGROUP; directocclusion: Single; reverbocclusion: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DOcclusion: function(channelgroup: PFMOD_CHANNELGROUP; var directocclusion: Single; var reverbocclusion: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DSpread: function(channelgroup: PFMOD_CHANNELGROUP; angle: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DSpread: function(channelgroup: PFMOD_CHANNELGROUP; var angle: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DLevel: function(channelgroup: PFMOD_CHANNELGROUP; level: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DLevel: function(channelgroup: PFMOD_CHANNELGROUP; var level: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DDopplerLevel: function(channelgroup: PFMOD_CHANNELGROUP; level: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DDopplerLevel: function(channelgroup: PFMOD_CHANNELGROUP; var level: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Set3DDistanceFilter: function(channelgroup: PFMOD_CHANNELGROUP; custom: FMOD_BOOL; customLevel: Single; centerFreq: Single): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_Get3DDistanceFilter: function(channelgroup: PFMOD_CHANNELGROUP; var custom: FMOD_BOOL; var customLevel: Single; var centerFreq: Single): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_SetUserData: function(channelgroup: PFMOD_CHANNELGROUP; userdata: Pointer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetUserData: function(channelgroup: PFMOD_CHANNELGROUP; var userdata: Pointer): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_Release: function(channelgroup: PFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_AddGroup: function(channelgroup: PFMOD_CHANNELGROUP; group: PFMOD_CHANNELGROUP; propagatedspclock: FMOD_BOOL; var connection: PFMOD_DSPCONNECTION): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetNumGroups: function(channelgroup: PFMOD_CHANNELGROUP; var numgroups: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetGroup: function(channelgroup: PFMOD_CHANNELGROUP; index: Integer; var group: PFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetParentGroup: function(channelgroup: PFMOD_CHANNELGROUP; var group: PFMOD_CHANNELGROUP): FMOD_RESULT; cdecl;

  FMOD_ChannelGroup_GetName: function(channelgroup: PFMOD_CHANNELGROUP; name: PChar; namelen: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetNumChannels: function(channelgroup: PFMOD_CHANNELGROUP; var numchannels: Integer): FMOD_RESULT; cdecl;
  FMOD_ChannelGroup_GetChannel: function(channelgroup: PFMOD_CHANNELGROUP; index: Integer; var channel: PFMOD_CHANNEL): FMOD_RESULT; cdecl;

function FMOD_ErrorString( ErrCode: FMOD_RESULT ): AnsiString;

var
  FMOD : TLibrary = nil;

function LoadFMOD( const aPath : AnsiString = FMODDefaultPath ) : Boolean;

implementation

var
  Saved8087CW: Word;

const
  Default8087CW = $1332;

function LoadFMOD( const aPath : AnsiString ) : Boolean;
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
  Pointer(FMOD_System_Create)              := GetSymbol('FMOD_System_Create');
  Pointer(FMOD_System_Release)             := GetSymbol('FMOD_System_Release');
  Pointer(FMOD_System_SetOutput)           := GetSymbol('FMOD_System_SetOutput');
  Pointer(FMOD_System_GetOutput)           := GetSymbol('FMOD_System_GetOutput');
  Pointer(FMOD_System_GetNumDrivers)       := GetSymbol('FMOD_System_GetNumDrivers');
  Pointer(FMOD_System_GetDriverInfo)       := GetSymbol('FMOD_System_GetDriverInfo');
  Pointer(FMOD_System_SetDriver)           := GetSymbol('FMOD_System_SetDriver');
  Pointer(FMOD_System_GetDriver)           := GetSymbol('FMOD_System_GetDriver');
  Pointer(FMOD_System_SetSoftwareChannels) := GetSymbol('FMOD_System_SetSoftwareChannels');
  Pointer(FMOD_System_GetSoftwareChannels) := GetSymbol('FMOD_System_GetSoftwareChannels');
  Pointer(FMOD_System_SetSoftwareFormat)   := GetSymbol('FMOD_System_SetSoftwareFormat');
  Pointer(FMOD_System_GetSoftwareFormat)   := GetSymbol('FMOD_System_GetSoftwareFormat');
  Pointer(FMOD_System_SetDSPBufferSize)    := GetSymbol('FMOD_System_SetDSPBufferSize');
  Pointer(FMOD_System_GetDSPBufferSize)    := GetSymbol('FMOD_System_GetDSPBufferSize');
  Pointer(FMOD_System_SetFileSystem)       := GetSymbol('FMOD_System_SetFileSystem');
  Pointer(FMOD_System_AttachFileSystem)    := GetSymbol('FMOD_System_AttachFileSystem');
  Pointer(FMOD_System_SetAdvancedSettings) := GetSymbol('FMOD_System_SetAdvancedSettings');
  Pointer(FMOD_System_GetAdvancedSettings) := GetSymbol('FMOD_System_GetAdvancedSettings');
  Pointer(FMOD_System_SetCallback)         := GetSymbol('FMOD_System_SetCallback');

  Pointer(FMOD_System_Init)                      := GetSymbol('FMOD_System_Init');
  Pointer(FMOD_System_Close)                     := GetSymbol('FMOD_System_Close');

  Pointer(FMOD_System_Update)                    := GetSymbol('FMOD_System_Update');
  Pointer(FMOD_System_SetSpeakerPosition)        := GetSymbol('FMOD_System_SetSpeakerPosition');
  Pointer(FMOD_System_GetSpeakerPosition)        := GetSymbol('FMOD_System_GetSpeakerPosition');
  Pointer(FMOD_System_SetStreamBufferSize)       := GetSymbol('FMOD_System_SetStreamBufferSize');
  Pointer(FMOD_System_GetStreamBufferSize)       := GetSymbol('FMOD_System_GetStreamBufferSize');
  Pointer(FMOD_System_Set3DSettings)             := GetSymbol('FMOD_System_Set3DSettings');
  Pointer(FMOD_System_Get3DSettings)             := GetSymbol('FMOD_System_Get3DSettings');
  Pointer(FMOD_System_Set3DNumListeners)         := GetSymbol('FMOD_System_Set3DNumListeners');
  Pointer(FMOD_System_Get3DNumListeners)         := GetSymbol('FMOD_System_Get3DNumListeners');
  Pointer(FMOD_System_Set3DListenerAttributes)   := GetSymbol('FMOD_System_Set3DListenerAttributes');
  Pointer(FMOD_System_Get3DListenerAttributes)   := GetSymbol('FMOD_System_Get3DListenerAttributes');
  Pointer(FMOD_System_Set3DRolloffCallback)      := GetSymbol('FMOD_System_Set3DRolloffCallback');
  Pointer(FMOD_System_MixerSuspend)              := GetSymbol('FMOD_System_MixerSuspend');
  Pointer(FMOD_System_MixerResume)               := GetSymbol('FMOD_System_MixerResume');
  Pointer(FMOD_System_GetDefaultMixMatrix)       := GetSymbol('FMOD_System_GetDefaultMixMatrix');
  Pointer(FMOD_System_GetSpeakerModeChannels)    := GetSymbol('FMOD_System_GetSpeakerModeChannels');

  Pointer(FMOD_System_GetVersion)                := GetSymbol('FMOD_System_GetVersion');
  Pointer(FMOD_System_GetOutputHandle)           := GetSymbol('FMOD_System_GetOutputHandle');
  Pointer(FMOD_System_GetChannelsPlaying)        := GetSymbol('FMOD_System_GetChannelsPlaying');
  Pointer(FMOD_System_GetCPUUsage)               := GetSymbol('FMOD_System_GetCPUUsage');
  Pointer(FMOD_System_GetFileUsage)              := GetSymbol('FMOD_System_GetFileUsage');

  Pointer(FMOD_System_CreateSound)               := GetSymbol('FMOD_System_CreateSound');
  Pointer(FMOD_System_CreateStream)              := GetSymbol('FMOD_System_CreateStream');
  Pointer(FMOD_System_CreateChannelGroup)        := GetSymbol('FMOD_System_CreateChannelGroup');
  Pointer(FMOD_System_CreateSoundGroup)          := GetSymbol('FMOD_System_CreateSoundGroup');
  Pointer(FMOD_System_CreateReverb3D)            := GetSymbol('FMOD_System_CreateReverb3D');
  Pointer(FMOD_System_PlaySound)                 := GetSymbol('FMOD_System_PlaySound');
  Pointer(FMOD_System_PlayDSP)                   := GetSymbol('FMOD_System_PlayDSP');
  Pointer(FMOD_System_GetChannel)                := GetSymbol('FMOD_System_GetChannel');
  Pointer(FMOD_System_GetMasterChannelGroup)     := GetSymbol('FMOD_System_GetMasterChannelGroup');
  Pointer(FMOD_System_GetMasterSoundGroup)       := GetSymbol('FMOD_System_GetMasterSoundGroup');

    Pointer(FMOD_Sound_Release)                    := GetSymbol('FMOD_Sound_Release');
  Pointer(FMOD_Sound_GetSystemObject)            := GetSymbol('FMOD_Sound_GetSystemObject');

  Pointer(FMOD_Sound_Lock)                       := GetSymbol('FMOD_Sound_Lock');
  Pointer(FMOD_Sound_Unlock)                     := GetSymbol('FMOD_Sound_Unlock');
  Pointer(FMOD_Sound_SetDefaults)                := GetSymbol('FMOD_Sound_SetDefaults');
  Pointer(FMOD_Sound_GetDefaults)                := GetSymbol('FMOD_Sound_GetDefaults');
  Pointer(FMOD_Sound_Set3DMinMaxDistance)        := GetSymbol('FMOD_Sound_Set3DMinMaxDistance');
  Pointer(FMOD_Sound_Get3DMinMaxDistance)        := GetSymbol('FMOD_Sound_Get3DMinMaxDistance');
  Pointer(FMOD_Sound_Set3DConeSettings)          := GetSymbol('FMOD_Sound_Set3DConeSettings');
  Pointer(FMOD_Sound_Get3DConeSettings)          := GetSymbol('FMOD_Sound_Get3DConeSettings');
  Pointer(FMOD_Sound_Set3DCustomRolloff)         := GetSymbol('FMOD_Sound_Set3DCustomRolloff');
  Pointer(FMOD_Sound_Get3DCustomRolloff)         := GetSymbol('FMOD_Sound_Get3DCustomRolloff');
  Pointer(FMOD_Sound_GetSubSound)                := GetSymbol('FMOD_Sound_GetSubSound');
  Pointer(FMOD_Sound_GetSubSoundParent)          := GetSymbol('FMOD_Sound_GetSubSoundParent');
  Pointer(FMOD_Sound_GetName)                    := GetSymbol('FMOD_Sound_GetName');
  Pointer(FMOD_Sound_GetLength)                  := GetSymbol('FMOD_Sound_GetLength');
  Pointer(FMOD_Sound_GetFormat)                  := GetSymbol('FMOD_Sound_GetFormat');
  Pointer(FMOD_Sound_GetNumSubSounds)            := GetSymbol('FMOD_Sound_GetNumSubSounds');
  Pointer(FMOD_Sound_GetNumTags)                 := GetSymbol('FMOD_Sound_GetNumTags');
  Pointer(FMOD_Sound_GetTag)                     := GetSymbol('FMOD_Sound_GetTag');
  Pointer(FMOD_Sound_GetOpenState)               := GetSymbol('FMOD_Sound_GetOpenState');
  Pointer(FMOD_Sound_ReadData)                   := GetSymbol('FMOD_Sound_ReadData');
  Pointer(FMOD_Sound_SeekData)                   := GetSymbol('FMOD_Sound_SeekData');

  Pointer(FMOD_Sound_SetSoundGroup)              := GetSymbol('FMOD_Sound_SetSoundGroup');
  Pointer(FMOD_Sound_GetSoundGroup)              := GetSymbol('FMOD_Sound_GetSoundGroup');

  Pointer(FMOD_Sound_GetNumSyncPoints)           := GetSymbol('FMOD_Sound_GetNumSyncPoints');
  Pointer(FMOD_Sound_GetSyncPoint)               := GetSymbol('FMOD_Sound_GetSyncPoint');
  Pointer(FMOD_Sound_GetSyncPointInfo)           := GetSymbol('FMOD_Sound_GetSyncPointInfo');
  Pointer(FMOD_Sound_AddSyncPoint)               := GetSymbol('FMOD_Sound_AddSyncPoint');
  Pointer(FMOD_Sound_DeleteSyncPoint)            := GetSymbol('FMOD_Sound_DeleteSyncPoint');

  Pointer(FMOD_Sound_SetMode)                    := GetSymbol('FMOD_Sound_SetMode');
  Pointer(FMOD_Sound_GetMode)                    := GetSymbol('FMOD_Sound_GetMode');
  Pointer(FMOD_Sound_SetLoopCount)               := GetSymbol('FMOD_Sound_SetLoopCount');
  Pointer(FMOD_Sound_GetLoopCount)               := GetSymbol('FMOD_Sound_GetLoopCount');
  Pointer(FMOD_Sound_SetLoopPoints)              := GetSymbol('FMOD_Sound_SetLoopPoints');
  Pointer(FMOD_Sound_GetLoopPoints)              := GetSymbol('FMOD_Sound_GetLoopPoints');

  Pointer(FMOD_Sound_GetMusicNumChannels)        := GetSymbol('FMOD_Sound_GetMusicNumChannels');
  Pointer(FMOD_Sound_SetMusicChannelVolume)      := GetSymbol('FMOD_Sound_SetMusicChannelVolume');
  Pointer(FMOD_Sound_GetMusicChannelVolume)      := GetSymbol('FMOD_Sound_GetMusicChannelVolume');
  Pointer(FMOD_Sound_SetMusicSpeed)              := GetSymbol('FMOD_Sound_SetMusicSpeed');
  Pointer(FMOD_Sound_GetMusicSpeed)              := GetSymbol('FMOD_Sound_GetMusicSpeed');

  Pointer(FMOD_Sound_SetUserData)                := GetSymbol('FMOD_Sound_SetUserData');
  Pointer(FMOD_Sound_GetUserData)                := GetSymbol('FMOD_Sound_GetUserData');

  Pointer(FMOD_Channel_GetSystemObject)          := GetSymbol('FMOD_Channel_GetSystemObject');

  Pointer(FMOD_Channel_Stop)                     := GetSymbol('FMOD_Channel_Stop');
  Pointer(FMOD_Channel_SetPaused)                := GetSymbol('FMOD_Channel_SetPaused');
  Pointer(FMOD_Channel_GetPaused)                := GetSymbol('FMOD_Channel_GetPaused');
  Pointer(FMOD_Channel_SetVolume)                := GetSymbol('FMOD_Channel_SetVolume');
  Pointer(FMOD_Channel_GetVolume)                := GetSymbol('FMOD_Channel_GetVolume');
  Pointer(FMOD_Channel_SetVolumeRamp)            := GetSymbol('FMOD_Channel_SetVolumeRamp');
  Pointer(FMOD_Channel_GetVolumeRamp)            := GetSymbol('FMOD_Channel_GetVolumeRamp');
  Pointer(FMOD_Channel_GetAudibility)            := GetSymbol('FMOD_Channel_GetAudibility');
  Pointer(FMOD_Channel_SetPitch)                 := GetSymbol('FMOD_Channel_SetPitch');
  Pointer(FMOD_Channel_GetPitch)                 := GetSymbol('FMOD_Channel_GetPitch');
  Pointer(FMOD_Channel_SetMute)                  := GetSymbol('FMOD_Channel_SetMute');
  Pointer(FMOD_Channel_GetMute)                  := GetSymbol('FMOD_Channel_GetMute');
  Pointer(FMOD_Channel_SetReverbProperties)      := GetSymbol('FMOD_Channel_SetReverbProperties');
  Pointer(FMOD_Channel_GetReverbProperties)      := GetSymbol('FMOD_Channel_GetReverbProperties');
  Pointer(FMOD_Channel_SetLowPassGain)           := GetSymbol('FMOD_Channel_SetLowPassGain');
  Pointer(FMOD_Channel_GetLowPassGain)           := GetSymbol('FMOD_Channel_GetLowPassGain');
  Pointer(FMOD_Channel_SetMode)                  := GetSymbol('FMOD_Channel_SetMode');
  Pointer(FMOD_Channel_GetMode)                  := GetSymbol('FMOD_Channel_GetMode');
  Pointer(FMOD_Channel_SetCallback)              := GetSymbol('FMOD_Channel_SetCallback');
  Pointer(FMOD_Channel_IsPlaying)                := GetSymbol('FMOD_Channel_IsPlaying');

  Pointer(FMOD_Channel_SetPan)                   := GetSymbol('FMOD_Channel_SetPan');
  Pointer(FMOD_Channel_SetMixLevelsOutput)       := GetSymbol('FMOD_Channel_SetMixLevelsOutput');
  Pointer(FMOD_Channel_SetMixLevelsInput)        := GetSymbol('FMOD_Channel_SetMixLevelsInput');
  Pointer(FMOD_Channel_SetMixMatrix)             := GetSymbol('FMOD_Channel_SetMixMatrix');
  Pointer(FMOD_Channel_GetMixMatrix)             := GetSymbol('FMOD_Channel_GetMixMatrix');

  Pointer(FMOD_Channel_GetDSPClock)              := GetSymbol('FMOD_Channel_GetDSPClock');
  Pointer(FMOD_Channel_SetDelay)                 := GetSymbol('FMOD_Channel_SetDelay');
  Pointer(FMOD_Channel_GetDelay)                 := GetSymbol('FMOD_Channel_GetDelay');
  Pointer(FMOD_Channel_AddFadePoint)             := GetSymbol('FMOD_Channel_AddFadePoint');
  Pointer(FMOD_Channel_SetFadePointRamp)         := GetSymbol('FMOD_Channel_SetFadePointRamp');
  Pointer(FMOD_Channel_RemoveFadePoints)         := GetSymbol('FMOD_Channel_RemoveFadePoints');
  Pointer(FMOD_Channel_GetFadePoints)            := GetSymbol('FMOD_Channel_GetFadePoints');

  Pointer(FMOD_Channel_GetDSP)                   := GetSymbol('FMOD_Channel_GetDSP');
  Pointer(FMOD_Channel_AddDSP)                   := GetSymbol('FMOD_Channel_AddDSP');
  Pointer(FMOD_Channel_RemoveDSP)                := GetSymbol('FMOD_Channel_RemoveDSP');
  Pointer(FMOD_Channel_GetNumDSPs)               := GetSymbol('FMOD_Channel_GetNumDSPs');
  Pointer(FMOD_Channel_SetDSPIndex)              := GetSymbol('FMOD_Channel_SetDSPIndex');
  Pointer(FMOD_Channel_GetDSPIndex)              := GetSymbol('FMOD_Channel_GetDSPIndex');

  Pointer(FMOD_Channel_Set3DAttributes)          := GetSymbol('FMOD_Channel_Set3DAttributes');
  Pointer(FMOD_Channel_Get3DAttributes)          := GetSymbol('FMOD_Channel_Get3DAttributes');
  Pointer(FMOD_Channel_Set3DMinMaxDistance)      := GetSymbol('FMOD_Channel_Set3DMinMaxDistance');
  Pointer(FMOD_Channel_Get3DMinMaxDistance)      := GetSymbol('FMOD_Channel_Get3DMinMaxDistance');
  Pointer(FMOD_Channel_Set3DConeSettings)        := GetSymbol('FMOD_Channel_Set3DConeSettings');
  Pointer(FMOD_Channel_Get3DConeSettings)        := GetSymbol('FMOD_Channel_Get3DConeSettings');
  Pointer(FMOD_Channel_Set3DConeOrientation)     := GetSymbol('FMOD_Channel_Set3DConeOrientation');
  Pointer(FMOD_Channel_Get3DConeOrientation)     := GetSymbol('FMOD_Channel_Get3DConeOrientation');
  Pointer(FMOD_Channel_Set3DCustomRolloff)       := GetSymbol('FMOD_Channel_Set3DCustomRolloff');
  Pointer(FMOD_Channel_Get3DCustomRolloff)       := GetSymbol('FMOD_Channel_Get3DCustomRolloff');
  Pointer(FMOD_Channel_Set3DOcclusion)           := GetSymbol('FMOD_Channel_Set3DOcclusion');
  Pointer(FMOD_Channel_Get3DOcclusion)           := GetSymbol('FMOD_Channel_Get3DOcclusion');
  Pointer(FMOD_Channel_Set3DSpread)              := GetSymbol('FMOD_Channel_Set3DSpread');
  Pointer(FMOD_Channel_Get3DSpread)              := GetSymbol('FMOD_Channel_Get3DSpread');
  Pointer(FMOD_Channel_Set3DLevel)               := GetSymbol('FMOD_Channel_Set3DLevel');
  Pointer(FMOD_Channel_Get3DLevel)               := GetSymbol('FMOD_Channel_Get3DLevel');
  Pointer(FMOD_Channel_Set3DDopplerLevel)        := GetSymbol('FMOD_Channel_Set3DDopplerLevel');
  Pointer(FMOD_Channel_Get3DDopplerLevel)        := GetSymbol('FMOD_Channel_Get3DDopplerLevel');
  Pointer(FMOD_Channel_Set3DDistanceFilter)      := GetSymbol('FMOD_Channel_Set3DDistanceFilter');
  Pointer(FMOD_Channel_Get3DDistanceFilter)      := GetSymbol('FMOD_Channel_Get3DDistanceFilter');

  Pointer(FMOD_Channel_SetUserData)              := GetSymbol('FMOD_Channel_SetUserData');
  Pointer(FMOD_Channel_GetUserData)              := GetSymbol('FMOD_Channel_GetUserData');

  Pointer(FMOD_Channel_SetFrequency)             := GetSymbol('FMOD_Channel_SetFrequency');
  Pointer(FMOD_Channel_GetFrequency)             := GetSymbol('FMOD_Channel_GetFrequency');
  Pointer(FMOD_Channel_SetPriority)              := GetSymbol('FMOD_Channel_SetPriority');
  Pointer(FMOD_Channel_GetPriority)              := GetSymbol('FMOD_Channel_GetPriority');
  Pointer(FMOD_Channel_SetPosition)              := GetSymbol('FMOD_Channel_SetPosition');
  Pointer(FMOD_Channel_GetPosition)              := GetSymbol('FMOD_Channel_GetPosition');
  Pointer(FMOD_Channel_SetChannelGroup)          := GetSymbol('FMOD_Channel_SetChannelGroup');
  Pointer(FMOD_Channel_GetChannelGroup)          := GetSymbol('FMOD_Channel_GetChannelGroup');
  Pointer(FMOD_Channel_SetLoopCount)             := GetSymbol('FMOD_Channel_SetLoopCount');
  Pointer(FMOD_Channel_GetLoopCount)             := GetSymbol('FMOD_Channel_GetLoopCount');
  Pointer(FMOD_Channel_SetLoopPoints)            := GetSymbol('FMOD_Channel_SetLoopPoints');
  Pointer(FMOD_Channel_GetLoopPoints)            := GetSymbol('FMOD_Channel_GetLoopPoints');

  Pointer(FMOD_Channel_IsVirtual)                := GetSymbol('FMOD_Channel_IsVirtual');
  Pointer(FMOD_Channel_GetCurrentSound)          := GetSymbol('FMOD_Channel_GetCurrentSound');
  Pointer(FMOD_Channel_GetIndex)                 := GetSymbol('FMOD_Channel_GetIndex');

  Pointer(FMOD_ChannelGroup_GetSystemObject)     := GetSymbol('FMOD_ChannelGroup_GetSystemObject');

  Pointer(FMOD_ChannelGroup_Stop)                := GetSymbol('FMOD_ChannelGroup_Stop');
  Pointer(FMOD_ChannelGroup_SetPaused)           := GetSymbol('FMOD_ChannelGroup_SetPaused');
  Pointer(FMOD_ChannelGroup_GetPaused)           := GetSymbol('FMOD_ChannelGroup_GetPaused');
  Pointer(FMOD_ChannelGroup_SetVolume)           := GetSymbol('FMOD_ChannelGroup_SetVolume');
  Pointer(FMOD_ChannelGroup_GetVolume)           := GetSymbol('FMOD_ChannelGroup_GetVolume');
  Pointer(FMOD_ChannelGroup_SetVolumeRamp)       := GetSymbol('FMOD_ChannelGroup_SetVolumeRamp');
  Pointer(FMOD_ChannelGroup_GetVolumeRamp)       := GetSymbol('FMOD_ChannelGroup_GetVolumeRamp');
  Pointer(FMOD_ChannelGroup_GetAudibility)       := GetSymbol('FMOD_ChannelGroup_GetAudibility');
  Pointer(FMOD_ChannelGroup_SetPitch)            := GetSymbol('FMOD_ChannelGroup_SetPitch');
  Pointer(FMOD_ChannelGroup_GetPitch)            := GetSymbol('FMOD_ChannelGroup_GetPitch');
  Pointer(FMOD_ChannelGroup_SetMute)             := GetSymbol('FMOD_ChannelGroup_SetMute');
  Pointer(FMOD_ChannelGroup_GetMute)             := GetSymbol('FMOD_ChannelGroup_GetMute');
  Pointer(FMOD_ChannelGroup_SetReverbProperties) := GetSymbol('FMOD_ChannelGroup_SetReverbProperties');
  Pointer(FMOD_ChannelGroup_GetReverbProperties) := GetSymbol('FMOD_ChannelGroup_GetReverbProperties');
  Pointer(FMOD_ChannelGroup_SetLowPassGain)      := GetSymbol('FMOD_ChannelGroup_SetLowPassGain');
  Pointer(FMOD_ChannelGroup_GetLowPassGain)      := GetSymbol('FMOD_ChannelGroup_GetLowPassGain');
  Pointer(FMOD_ChannelGroup_SetMode)             := GetSymbol('FMOD_ChannelGroup_SetMode');
  Pointer(FMOD_ChannelGroup_GetMode)             := GetSymbol('FMOD_ChannelGroup_GetMode');
  Pointer(FMOD_ChannelGroup_SetCallback)         := GetSymbol('FMOD_ChannelGroup_SetCallback');
  Pointer(FMOD_ChannelGroup_IsPlaying)           := GetSymbol('FMOD_ChannelGroup_IsPlaying');

  Pointer(FMOD_ChannelGroup_SetPan)              := GetSymbol('FMOD_ChannelGroup_SetPan');
  Pointer(FMOD_ChannelGroup_SetMixLevelsOutput)  := GetSymbol('FMOD_ChannelGroup_SetMixLevelsOutput');
  Pointer(FMOD_ChannelGroup_SetMixLevelsInput)   := GetSymbol('FMOD_ChannelGroup_SetMixLevelsInput');
  Pointer(FMOD_ChannelGroup_SetMixMatrix)        := GetSymbol('FMOD_ChannelGroup_SetMixMatrix');
  Pointer(FMOD_ChannelGroup_GetMixMatrix)        := GetSymbol('FMOD_ChannelGroup_GetMixMatrix');

  Pointer(FMOD_ChannelGroup_GetDSPClock)         := GetSymbol('FMOD_ChannelGroup_GetDSPClock');
  Pointer(FMOD_ChannelGroup_SetDelay)            := GetSymbol('FMOD_ChannelGroup_SetDelay');
  Pointer(FMOD_ChannelGroup_GetDelay)            := GetSymbol('FMOD_ChannelGroup_GetDelay');
  Pointer(FMOD_ChannelGroup_AddFadePoint)        := GetSymbol('FMOD_ChannelGroup_AddFadePoint');
  Pointer(FMOD_ChannelGroup_SetFadePointRamp)    := GetSymbol('FMOD_ChannelGroup_SetFadePointRamp');
  Pointer(FMOD_ChannelGroup_RemoveFadePoints)    := GetSymbol('FMOD_ChannelGroup_RemoveFadePoints');
  Pointer(FMOD_ChannelGroup_GetFadePoints)       := GetSymbol('FMOD_ChannelGroup_GetFadePoints');

  Pointer(FMOD_ChannelGroup_GetDSP)              := GetSymbol('FMOD_ChannelGroup_GetDSP');
  Pointer(FMOD_ChannelGroup_AddDSP)              := GetSymbol('FMOD_ChannelGroup_AddDSP');
  Pointer(FMOD_ChannelGroup_RemoveDSP)           := GetSymbol('FMOD_ChannelGroup_RemoveDSP');
  Pointer(FMOD_ChannelGroup_GetNumDSPs)          := GetSymbol('FMOD_ChannelGroup_GetNumDSPs');
  Pointer(FMOD_ChannelGroup_SetDSPIndex)         := GetSymbol('FMOD_ChannelGroup_SetDSPIndex');
  Pointer(FMOD_ChannelGroup_GetDSPIndex)         := GetSymbol('FMOD_ChannelGroup_GetDSPIndex');

  Pointer(FMOD_ChannelGroup_Set3DAttributes)     := GetSymbol('FMOD_ChannelGroup_Set3DAttributes');
  Pointer(FMOD_ChannelGroup_Get3DAttributes)     := GetSymbol('FMOD_ChannelGroup_Get3DAttributes');
  Pointer(FMOD_ChannelGroup_Set3DMinMaxDistance) := GetSymbol('FMOD_ChannelGroup_Set3DMinMaxDistance');
  Pointer(FMOD_ChannelGroup_Get3DMinMaxDistance) := GetSymbol('FMOD_ChannelGroup_Get3DMinMaxDistance');
  Pointer(FMOD_ChannelGroup_Set3DConeSettings)   := GetSymbol('FMOD_ChannelGroup_Set3DConeSettings');
  Pointer(FMOD_ChannelGroup_Get3DConeSettings)   := GetSymbol('FMOD_ChannelGroup_Get3DConeSettings');
  Pointer(FMOD_ChannelGroup_Set3DConeOrientation):= GetSymbol('FMOD_ChannelGroup_Set3DConeOrientation');
  Pointer(FMOD_ChannelGroup_Get3DConeOrientation):= GetSymbol('FMOD_ChannelGroup_Get3DConeOrientation');
  Pointer(FMOD_ChannelGroup_Set3DCustomRolloff)  := GetSymbol('FMOD_ChannelGroup_Set3DCustomRolloff');
  Pointer(FMOD_ChannelGroup_Get3DCustomRolloff)  := GetSymbol('FMOD_ChannelGroup_Get3DCustomRolloff');
  Pointer(FMOD_ChannelGroup_Set3DOcclusion)      := GetSymbol('FMOD_ChannelGroup_Set3DOcclusion');
  Pointer(FMOD_ChannelGroup_Get3DOcclusion)      := GetSymbol('FMOD_ChannelGroup_Get3DOcclusion');
  Pointer(FMOD_ChannelGroup_Set3DSpread)         := GetSymbol('FMOD_ChannelGroup_Set3DSpread');
  Pointer(FMOD_ChannelGroup_Get3DSpread)         := GetSymbol('FMOD_ChannelGroup_Get3DSpread');
  Pointer(FMOD_ChannelGroup_Set3DLevel)          := GetSymbol('FMOD_ChannelGroup_Set3DLevel');
  Pointer(FMOD_ChannelGroup_Get3DLevel)          := GetSymbol('FMOD_ChannelGroup_Get3DLevel');
  Pointer(FMOD_ChannelGroup_Set3DDopplerLevel)   := GetSymbol('FMOD_ChannelGroup_Set3DDopplerLevel');
  Pointer(FMOD_ChannelGroup_Get3DDopplerLevel)   := GetSymbol('FMOD_ChannelGroup_Get3DDopplerLevel');
  Pointer(FMOD_ChannelGroup_Set3DDistanceFilter) := GetSymbol('FMOD_ChannelGroup_Set3DDistanceFilter');
  Pointer(FMOD_ChannelGroup_Get3DDistanceFilter) := GetSymbol('FMOD_ChannelGroup_Get3DDistanceFilter');

  Pointer(FMOD_ChannelGroup_SetUserData)         := GetSymbol('FMOD_ChannelGroup_SetUserData');
  Pointer(FMOD_ChannelGroup_GetUserData)         := GetSymbol('FMOD_ChannelGroup_GetUserData');

  Pointer(FMOD_ChannelGroup_Release)             := GetSymbol('FMOD_ChannelGroup_Release');

  Pointer(FMOD_ChannelGroup_AddGroup)            := GetSymbol('FMOD_ChannelGroup_AddGroup');
  Pointer(FMOD_ChannelGroup_GetNumGroups)        := GetSymbol('FMOD_ChannelGroup_GetNumGroups');
  Pointer(FMOD_ChannelGroup_GetGroup)            := GetSymbol('FMOD_ChannelGroup_GetGroup');
  Pointer(FMOD_ChannelGroup_GetParentGroup)      := GetSymbol('FMOD_ChannelGroup_GetParentGroup');

  Pointer(FMOD_ChannelGroup_GetName)             := GetSymbol('FMOD_ChannelGroup_GetName');
  Pointer(FMOD_ChannelGroup_GetNumChannels)      := GetSymbol('FMOD_ChannelGroup_GetNumChannels');
  Pointer(FMOD_ChannelGroup_GetChannel)          := GetSymbol('FMOD_ChannelGroup_GetChannel');

  Exit( True );
end;

function FMOD_ErrorString( ErrCode: FMOD_RESULT ): AnsiString;
begin
  case errcode of
    FMOD_OK:                            Result := 'No errors.';
    FMOD_ERR_BADCOMMAND:                Result := 'Tried to call a function on a data type that does not allow this type of functionality (ie calling Sound::lock on a streaming sound).';
    FMOD_ERR_CHANNEL_ALLOC:             Result := 'Error trying to allocate a channel.';
    FMOD_ERR_CHANNEL_STOLEN:            Result := 'The specified channel has been reused to play another sound.';
    FMOD_ERR_DMA:                       Result := 'DMA Failure.  See debug output for more information.';
    FMOD_ERR_DSP_CONNECTION:            Result := 'DSP connection error.  Connection possibly caused a cyclic dependency or connected dsps with incompatible buffer counts.';
    FMOD_ERR_DSP_DONTPROCESS:           Result := 'DSP return code from a DSP process query callback.  Tells mixer not to call the process callback and therefore not consume CPU.  Use this to optimize the DSP graph.';
    FMOD_ERR_DSP_FORMAT:                Result := 'DSP Format error.  A DSP unit may have attempted to connect to this network with the wrong format, or a matrix may have been set with the wrong size if the target unit has a specified channel map.';
    FMOD_ERR_DSP_INUSE:                 Result := 'DSP is already in the mixer''s DSP network. It must be removed before being reinserted or released.';
    FMOD_ERR_DSP_NOTFOUND:              Result := 'DSP connection error.  Couldn''t find the DSP unit specified.';
    FMOD_ERR_DSP_RESERVED:              Result := 'DSP operation error.  Cannot perform operation on this DSP as it is reserved by the system.';
    FMOD_ERR_DSP_SILENCE:               Result := 'DSP return code from a DSP process query callback.  Tells mixer silence would be produced from read, so go idle and not consume CPU.  Use this to optimize the DSP graph.';
    FMOD_ERR_DSP_TYPE:                  Result := 'DSP operation cannot be performed on a DSP of this type.';
    FMOD_ERR_FILE_BAD:                  Result := 'Error loading file.';
    FMOD_ERR_FILE_COULDNOTSEEK:         Result := 'Couldn''t perform seek operation.  This is a limitation of the medium (ie netstreams) or the file format.';
    FMOD_ERR_FILE_DISKEJECTED:          Result := 'Media was ejected while reading.';
    FMOD_ERR_FILE_EOF:                  Result := 'End of file unexpectedly reached while trying to read essential data (truncated?).';
    FMOD_ERR_FILE_ENDOFDATA:            Result := 'End of current chunk reached while trying to read data.';
    FMOD_ERR_FILE_NOTFOUND:             Result := 'File not found.';
    FMOD_ERR_FORMAT:                    Result := 'Unsupported file or audio format.';
    FMOD_ERR_HEADER_MISMATCH:           Result := 'There is a version mismatch between the FMOD header and either the FMOD Studio library or the FMOD Low Level library.';
    FMOD_ERR_HTTP:                      Result := 'A HTTP error occurred. This is a catch-all for HTTP errors not listed elsewhere.';
    FMOD_ERR_HTTP_ACCESS:               Result := 'The specified resource requires authentication or is forbidden.';
    FMOD_ERR_HTTP_PROXY_AUTH:           Result := 'Proxy authentication is required to access the specified resource.';
    FMOD_ERR_HTTP_SERVER_ERROR:         Result := 'A HTTP server error occurred.';
    FMOD_ERR_HTTP_TIMEOUT:              Result := 'The HTTP request timed out.';
    FMOD_ERR_INITIALIZATION:            Result := 'FMOD was not initialized correctly to support this function.';
    FMOD_ERR_INITIALIZED:               Result := 'Cannot call this command after System::init.';
    FMOD_ERR_INTERNAL:                  Result := 'An error occurred in the FMOD system. Use the logging version of FMOD for more information.';
    FMOD_ERR_INVALID_FLOAT:             Result := 'Value passed in was a NaN, Inf or denormalized float.';
    FMOD_ERR_INVALID_HANDLE:            Result := 'An invalid object handle was used.';
    FMOD_ERR_INVALID_PARAM:             Result := 'An invalid parameter was passed to this function.';
    FMOD_ERR_INVALID_POSITION:          Result := 'An invalid seek position was passed to this function.';
    FMOD_ERR_INVALID_SPEAKER:           Result := 'An invalid speaker was passed to this function based on the current speaker mode.';
    FMOD_ERR_INVALID_SYNCPOINT:         Result := 'The syncpoint did not come from this sound handle.';
    FMOD_ERR_INVALID_THREAD:            Result := 'Tried to call a function on a thread that is not supported.';
    FMOD_ERR_INVALID_VECTOR:            Result := 'The vectors passed in are not unit length, or perpendicular.';
    FMOD_ERR_MAXAUDIBLE:                Result := 'Reached maximum audible playback count for this sound''s soundgroup.';
    FMOD_ERR_MEMORY:                    Result := 'Not enough memory or resources.';
    FMOD_ERR_MEMORY_CANTPOINT:          Result := 'Can''t use FMOD_OPENMEMORY_POINT on non PCM source data, or non mp3/xma/adpcm data if FMOD_CREATECOMPRESSEDSAMPLE was used.';
    FMOD_ERR_NEEDS3D:                   Result := 'Tried to call a command on a 2d sound when the command was meant for 3d sound.';
    FMOD_ERR_NEEDSHARDWARE:             Result := 'Tried to use a feature that requires hardware support.';
    FMOD_ERR_NET_CONNECT:               Result := 'Couldn''t connect to the specified host.';
    FMOD_ERR_NET_SOCKET_ERROR:          Result := 'A socket error occurred.  This is a catch-all for socket-related errors not listed elsewhere.';
    FMOD_ERR_NET_URL:                   Result := 'The specified URL couldn''t be resolved.';
    FMOD_ERR_NET_WOULD_BLOCK:           Result := 'Operation on a non-blocking socket could not complete immediately.';
    FMOD_ERR_NOTREADY:                  Result := 'Operation could not be performed because specified sound/DSP connection is not ready.';
    FMOD_ERR_OUTPUT_ALLOCATED:          Result := 'Error initializing output device, but more specifically, the output device is already in use and cannot be reused.';
    FMOD_ERR_OUTPUT_CREATEBUFFER:       Result := 'Error creating hardware sound buffer.';
    FMOD_ERR_OUTPUT_DRIVERCALL:         Result := 'A call to a standard soundcard driver failed, which could possibly mean a bug in the driver or resources were missing or exhausted.';
    FMOD_ERR_OUTPUT_FORMAT:             Result := 'Soundcard does not support the specified format.';
    FMOD_ERR_OUTPUT_INIT:               Result := 'Error initializing output device.';
    FMOD_ERR_OUTPUT_NODRIVERS:          Result := 'The output device has no drivers installed.  If pre-init, FMOD_OUTPUT_NOSOUND is selected as the output mode.  If post-init, the function just fails.';
    FMOD_ERR_PLUGIN:                    Result := 'An unspecified error has been returned from a plugin.';
    FMOD_ERR_PLUGIN_MISSING:            Result := 'A requested output, dsp unit type or codec was not available.';
    FMOD_ERR_PLUGIN_RESOURCE:           Result := 'A resource that the plugin requires cannot be allocated or found. (ie the DLS file for MIDI playback)';
    FMOD_ERR_PLUGIN_VERSION:            Result := 'A plugin was built with an unsupported SDK version.';
    FMOD_ERR_RECORD:                    Result := 'An error occurred trying to initialize the recording device.';
    FMOD_ERR_REVERB_CHANNELGROUP:       Result := 'Reverb properties cannot be set on this channel because a parent channelgroup owns the reverb connection.';
    FMOD_ERR_REVERB_INSTANCE:           Result := 'Specified instance in FMOD_REVERB_PROPERTIES couldn''t be set. Most likely because it is an invalid instance number or the reverb doesn''t exist.';
    FMOD_ERR_SUBSOUNDS:                 Result := 'The error occurred because the sound referenced contains subsounds when it shouldn''t have, or it doesn''t contain subsounds when it should have.  The operation may also not be able to be performed on a parent sound.';
    FMOD_ERR_SUBSOUND_ALLOCATED:        Result := 'This subsound is already being used by another sound, you cannot have more than one parent to a sound.  Null out the other parent''s entry first.';
    FMOD_ERR_SUBSOUND_CANTMOVE:         Result := 'Shared subsounds cannot be replaced or moved from their parent stream, such as when the parent stream is an FSB file.';
    FMOD_ERR_TAGNOTFOUND:               Result := 'The specified tag could not be found or there are no tags.';
    FMOD_ERR_TOOMANYCHANNELS:           Result := 'The sound created exceeds the allowable input channel count.  This can be increased using the ''maxinputchannels'' parameter in System::setSoftwareFormat.';
    FMOD_ERR_TRUNCATED:                 Result := 'The retrieved string is too long to fit in the supplied buffer and has been truncated.';
    FMOD_ERR_UNIMPLEMENTED:             Result := 'Something in FMOD hasn''t been implemented when it should be. Contact support.';
    FMOD_ERR_UNINITIALIZED:             Result := 'This command failed because System::init or System::setDriver was not called.';
    FMOD_ERR_UNSUPPORTED:               Result := 'A command issued was not supported by this object.  Possibly a plugin without certain callbacks specified.';
    FMOD_ERR_VERSION:                   Result := 'The version number of this file format is not supported.';
    FMOD_ERR_EVENT_ALREADY_LOADED:      Result := 'The specified bank has already been loaded.';
    FMOD_ERR_EVENT_LIVEUPDATE_BUSY:     Result := 'The live update connection failed due to the game already being connected.';
    FMOD_ERR_EVENT_LIVEUPDATE_MISMATCH: Result := 'The live update connection failed due to the game data being out of sync with the tool.';
    FMOD_ERR_EVENT_LIVEUPDATE_TIMEOUT:  Result := 'The live update connection timed out.';
    FMOD_ERR_EVENT_NOTFOUND:            Result := 'The requested event, parameter, bus or vca could not be found.';
    FMOD_ERR_STUDIO_UNINITIALIZED:      Result := 'The Studio::System object is not yet initialized.';
    FMOD_ERR_STUDIO_NOT_LOADED:         Result := 'The specified resource is not loaded, so it can''t be unloaded.';
    FMOD_ERR_INVALID_STRING:            Result := 'An invalid string was passed to this function.';
    FMOD_ERR_ALREADY_LOCKED:            Result := 'The specified resource is already locked.';
    FMOD_ERR_NOT_LOCKED:                Result := 'The specified resource is not locked, so it can''t be unlocked.';
    FMOD_ERR_RECORD_DISCONNECTED:       Result := 'The specified recording driver has been disconnected.';
    FMOD_ERR_TOOMANYSAMPLES:            Result := 'The length provided exceeds the allowable limit.';
  else
    Result := 'Unknown error.';
  end;
end;

finalization
  if FMOD <> nil then
  begin
    FreeAndNil( FMOD );
    Set8087CW(Saved8087CW);
  end;

end.

