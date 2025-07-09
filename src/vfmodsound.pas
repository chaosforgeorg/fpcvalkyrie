{$INCLUDE valkyrie.inc}
// @abstract(FMOD Sound system for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(June 10, 2009)
//
// Implements an FMOD sound system for Valkyrie
//
// Default behaviour for music is stoping the previous song before
// playing a new one.

unit vfmodsound;

interface

uses Classes, SysUtils, vsound, vrltools;

// The basic sound class, published as the singleton @link(Sound).
// Should be initialized and disposed via TSystems.
type

{ TFMODSound }

TFMODSound = class(TSound)
       // Initializes the Sound system.
       constructor Create; override;
       // Update the sound system
       procedure Update; override;
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
       // Implementation of Number of Channels Playing
       function NumChannelsPlaying: Integer;
       // Implementation of get error
       function GetError( ) : AnsiString; override;
       // Implementation of play Sound 3D
       procedure PlaySound3D( aData : Pointer; aRelative : TCoord2D ); override;
       // Implementation of play Sound
       procedure PlaySound( aData : Pointer; aVolume : Byte; aPan : Integer = -1 ); override;
       // Implementation of play Sound
       procedure PlayMusic( aData : Pointer; const aType : string; aRepeat : Boolean = True ); override;
       // Implementation of StopMusic
       procedure StopMusic( aData : Pointer; const aType : string ); override;
       // Implementation of StopMusic
       procedure StopSound(); override;	   
       // Implementation of VolumeMusic
       procedure VolumeMusic( aData : Pointer; const aType : string; aVolume : Byte ); override;
     end;

implementation

uses math, vutil, vdebug, vfmodlibrary;

var GSystem      : PFMOD_SYSTEM;
    GLastError   : FMOD_RESULT;
    GGroupSounds : PFMOD_CHANNELGROUP;
    GGroupMusic  : PFMOD_CHANNELGROUP;

const SoundLibMaxConcurrentSounds = 128;  // Max number of sounds playing at once by the library
const MaxConcurrentSounds = SoundLibMaxConcurrentSounds - 1; //Our limit of sounds includes a reduction by one to reserve for music playback

procedure FMOD_CHECK( aResult : FMOD_RESULT );
begin
  GLastError := aResult;
  if aResult <> FMOD_OK then
    Log( LOGERROR, 'FMOD error : '+FMOD_ErrorString(aResult));
end;

{ TFMODSound }

constructor TFMODSound.Create;
begin
  inherited Create;
  LoadFMOD;

  if not OpenDevice then
    raise Exception.Create('FMODInit Failed -- '+GetError());
end;

destructor TFMODSound.Destroy;
begin
  inherited Destroy;

  if GGroupSounds <> nil then FMOD_ChannelGroup_Release( GGroupSounds );
  if GGroupMusic <> nil  then FMOD_ChannelGroup_Release( GGroupMusic );

  if GSystem <> nil then
  begin
    FMOD_System_Close(GSystem);
    FMOD_System_Release(GSystem);
  end;
end;

// Update the sound system
procedure TFMODSound.Update;
begin
  FMOD_System_Update( GSystem );
end;

function TFModSound.NumChannelsPlaying : Integer;
var Channels, RealChannels : Integer;
begin
  Channels := 0; RealChannels := 0;
  FMOD_System_GetChannelsPlaying( GSystem, Channels, RealChannels);
  Exit(Channels);
end;

function TFMODSound.OpenDevice : Boolean;
const CPos : FMOD_VECTOR = ( x : 0.0; y : 0.0; z : 0.0 );
      CVel : FMOD_VECTOR = ( x : 0.0; y : 0.0; z : 0.0 );
      CFWd : FMOD_VECTOR = ( x : 0.0; y : 0.0; z : 1.0 );
      CUp  : FMOD_VECTOR = ( x : 0.0; y : 1.0; z : 0.0 );
begin
  GGroupSounds := nil;
  GGroupMusic  := nil;
  Log( LOGINFO, 'Opening FMOD... ' );
  GLastError := FMOD_System_Create( @GSystem, FMOD_VERSION);
  if GLastError <> FMOD_OK then
  begin
    Log( LOGERROR, 'FMOD_System_Create failed, error : ' + GetError() );
    if GSystem <> nil then FMOD_System_Release(GSystem);
    Exit( False );
  end;
  GLastError := FMOD_System_Init( GSystem, 128, FMOD_INIT_NORMAL, nil);
  if GLastError <> FMOD_OK then
  begin
    Log( LOGERROR, 'FMOD_System_Init failed, error : ' + GetError() );
    FMOD_System_Close(GSystem);
    FMOD_System_Release(GSystem);
    Exit( False );
  end;
  Log( LOGINFO, 'FMOD Initialized.' );

  FMOD_CHECK( FMOD_System_CreateChannelGroup( GSystem, 'sound', @GGroupSounds ) );
  FMOD_CHECK( FMOD_System_CreateChannelGroup( GSystem, 'music', @GGroupMusic ) );
  FMOD_CHECK( FMOD_System_set3DListenerAttributes( GSystem, 0, @CPos, @CVel, @CFwd, @CUp ) );
  Exit( True );
end;

function TFMODSound.LoadMusic(const aFileName: AnsiString; Streamed : Boolean): Pointer;
var iStream : PFMOD_SOUND;
    iInfo   : FMOD_CREATESOUNDEXINFO;
begin
  iStream := nil;
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  FMOD_CHECK( FMOD_System_CreateStream( GSystem, PChar(aFileName), FMOD_2D or FMOD_CREATESTREAM or FMOD_LOOP_NORMAL, @iInfo, @iStream) );
  Exit( iStream );
end;

function TFMODSound.LoadSound(const aFileName: AnsiString): Pointer;
var iSound : PFMOD_SOUND;
    iInfo  : FMOD_CREATESOUNDEXINFO;
    iMode  : FMOD_MODE;
begin
  iMode := FMOD_DEFAULT;
  if FSurroundEnabled then
    iMode := FMOD_3D or FMOD_3D_WORLDRELATIVE or FMOD_3D_INVERSEROLLOFF;

  iSound := nil;
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  FMOD_CHECK( FMOD_System_CreateSound( GSystem, PChar(aFileName), iMode, @iInfo, @iSound ) );
  Exit( iSound );
end;

function TFMODSound.LoadMusicStream(Stream: TStream; Size : DWord; Streamed : Boolean ): Pointer;
var iStream : PFMOD_SOUND;
    iInfo  : FMOD_CREATESOUNDEXINFO;
    iData  : Pointer;
begin
  iData := GetMem( Size );
  Stream.Read( iData^, Size );
  iStream := nil;
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  iInfo.length := Size;
  FMOD_CHECK( FMOD_System_CreateStream( GSystem, PChar(iData), FMOD_2D or FMOD_CREATESTREAM or FMOD_LOOP_NORMAL or FMOD_OPENMEMORY, @iInfo, @iStream) );
  // TODO: this memory is never freed, but needs to be freed with the MusicStream!
  //FreeMem( iData, Size );
  Exit( iStream );
end;

function TFMODSound.LoadSoundStream(Stream: TStream; Size : DWord ): Pointer;
var iSound : PFMOD_SOUND;
    iInfo  : FMOD_CREATESOUNDEXINFO;
    iMode  : FMOD_MODE;
    iData  : Pointer;
begin
  iData := GetMem( Size );
  Stream.Read( iData^, Size );

  iMode := FMOD_DEFAULT or FMOD_OPENMEMORY;
  if FSurroundEnabled then
    iMode := FMOD_3D or FMOD_3D_WORLDRELATIVE or FMOD_3D_INVERSEROLLOFF or FMOD_OPENMEMORY;

  iSound := nil;
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  iInfo.length := Size;
  FMOD_CHECK( FMOD_System_CreateSound( GSystem, PChar( iData ), iMode, @iInfo, @iSound ) );
  FreeMem( iData, Size );
  Exit( iSound );
end;

procedure TFMODSound.FreeMusic( aData: Pointer; const aType : String );
begin
  FMOD_CHECK( FMOD_Sound_Release(PFMOD_SOUND(aData)) );
end;

procedure TFMODSound.FreeSound(aData: Pointer);
begin
  FMOD_CHECK( FMOD_Sound_Release(PFMOD_SOUND(aData)) );
end;

function TFMODSound.GetError(): AnsiString;
var iError : AnsiString;
begin
  iError := FMOD_ErrorString(GLastError);
  Exit( iError );
end;

procedure TFMODSound.PlaySound3D(aData: Pointer; aRelative: TCoord2D);
var iChannel   : PFMOD_CHANNEL;
    iPosition  : FMOD_VECTOR;
begin
  iPosition.x := aRelative.X * 0.2;
  iPosition.y := aRelative.Y * 0.2;
  iPosition.z := 0.0;
  if NumChannelsPlaying() = MaxConcurrentSounds then Exit;  //Do not allow more sounds than this or a channel will be stolen (e.g. music)
  FMOD_CHECK( FMOD_System_PlaySound( GSystem, PFMOD_SOUND(aData), GGroupSounds, 1, @iChannel ) );
  FMOD_CHECK( FMOD_Channel_SetVolume( iChannel, Single( Min( SoundVolume, 128 ) / 128.0 ) ) );
  FMOD_CHECK( FMOD_Channel_set3DAttributes( iChannel, @iPosition, nil ) );
  FMOD_CHECK( FMOD_Channel_SetPaused( iChannel, 0 ) );
end;

procedure TFMODSound.PlaySound(aData: Pointer; aVolume: Byte; aPan: Integer);
var iChannel : PFMOD_CHANNEL;
begin
  if NumChannelsPlaying() = MaxConcurrentSounds then Exit;  //Do not allow more sounds than this or a channel will be stolen (e.g. music)
  FMOD_CHECK( FMOD_System_PlaySound( GSystem, PFMOD_SOUND(aData), GGroupSounds, 1, @iChannel ) );
  FMOD_CHECK( FMOD_Channel_SetVolume( iChannel, ( Single(aVolume) / 255.0 ) ) );
  if aPan <> -1
    then FMOD_CHECK( FMOD_Channel_SetPan( iChannel, ( Single(aPan-128) / 128.0 ) ) )
    else FMOD_CHECK( FMOD_Channel_SetPan( iChannel, 0 ) );
  FMOD_CHECK( FMOD_Channel_SetPaused( iChannel, 0 ) );
end;

procedure TFMODSound.PlayMusic(aData: Pointer; const aType : string; aRepeat: Boolean);
begin
  FMOD_ChannelGroup_SetVolume( GGroupMusic, Single( Min( MusicVolume, 128 ) / 128.0 ) );
  if aRepeat
    then FMOD_Sound_SetLoopCount( PFMOD_SOUND(aData), -1 )
    else FMOD_Sound_SetLoopCount( PFMOD_SOUND(aData), 0 );
  FMOD_CHECK( FMOD_System_PlaySound( GSystem, PFMOD_SOUND(aData), GGroupMusic, 0, nil ) );
end;

procedure TFMODSound.StopMusic(aData: Pointer; const aType : string );
begin
  FMOD_CHECK( FMOD_ChannelGroup_Stop( GGroupMusic ) );
end;

procedure TFMODSound.StopSound();
begin
  FMOD_CHECK( FMOD_ChannelGroup_Stop( GGroupSounds ) );
end;

procedure TFMODSound.VolumeMusic(aData: Pointer; const aType : string; aVolume: Byte );
begin
  FMOD_ChannelGroup_SetVolume( GGroupMusic, Single( Min( aVolume, 128 ) / 128.0 ) );
end;

initialization

GSystem := nil;

end.
