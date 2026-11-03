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

uses Classes, SysUtils, vsound, vrltools, vgenerics, vfmodlibrary;

type TFMODSoundData = record
    Struct : Pointer;
    Data   : Pointer;
    Size   : Integer;
  end;
type TFMODSoundDataArray = specialize TGArray< TFMODSoundData >;

type TFMODSound = class(TSound)
       // Initializes the Sound system.
       constructor Create; override;
       // Update the sound system
       procedure Update; override;
       // Reset the sound system
       procedure Reset; override;
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
       procedure FreeMusic( aData : Pointer; const aType : Ansistring ); override;
       // Implementation of Sound Freeing
       procedure FreeSound( aData : Pointer ); override;
       // Implementation of get error
       function GetError( ) : AnsiString; override;
       // Implementation of play Sound 3D
       procedure PlaySound3D( aData : Pointer; aRelative : TCoord2D ); override;
       // Implementation of play Sound
       procedure PlaySound( aData : Pointer; aVolume : Byte; aPan : Integer = -1 ); override;
       // Implementation of play Sound
       procedure PlayMusic( aData : Pointer; const aType : Ansistring; aRepeat : Boolean = True ); override;
       // Implementation of StopMusic
       procedure StopMusic( aData : Pointer; const aType : Ansistring ); override;
       // Implementation of StopMusic
       procedure StopSound(); override;	   
       // Implementation of VolumeMusic
       procedure VolumeMusic( aData : Pointer; const aType : Ansistring; aVolume : Byte ); override;
     protected
       function CalculateMusicVolume( aVolume : Byte ) : Single;
       function PushSimpleData( aPointer : Pointer ) : Pointer;
       function DataToSound( aData : Pointer ) : Pointer; inline;
       procedure Check( aResult : FMOD_RESULT );
     protected
       FInternalDataArray   : TFMODSoundDataArray;
       FInternalMusicVolume : Single;
       FSystem              : PFMOD_SYSTEM;
       FLastError           : FMOD_RESULT;
       FGroupSounds         : PFMOD_CHANNELGROUP;
       FGroupMusic          : PFMOD_CHANNELGROUP;
     end;

implementation

uses math, vutil, vdebug;

{ TFMODSound }

constructor TFMODSound.Create;
begin
  inherited Create;
  LoadFMOD;
  FInternalDataArray := nil;

  if not OpenDevice then
    raise Exception.Create('FMODInit Failed -- '+GetError());

  FInternalDataArray := TFMODSoundDataArray.Create;
end;

destructor TFMODSound.Destroy;
begin
  inherited Destroy;

  FreeAndNil( FInternalDataArray );

  if FGroupSounds <> nil then FMOD_ChannelGroup_Release( FGroupSounds );
  if FGroupMusic <> nil  then FMOD_ChannelGroup_Release( FGroupMusic );

  if FSystem <> nil then
  begin
    FMOD_System_Close(FSystem);
    FMOD_System_Release(FSystem);
  end;
end;

// Update the sound system
procedure TFMODSound.Update;
begin
  FMOD_System_Update( FSystem );
end;

// Reset the sound system
procedure TFMODSound.Reset;
begin
  inherited Reset;
  FInternalDataArray.Clear;
end;

function TFMODSound.OpenDevice : Boolean;
const CPos : FMOD_VECTOR = ( x : 0.0; y : 0.0; z : 0.0 );
      CVel : FMOD_VECTOR = ( x : 0.0; y : 0.0; z : 0.0 );
      CFWd : FMOD_VECTOR = ( x : 0.0; y : 0.0; z : 1.0 );
      CUp  : FMOD_VECTOR = ( x : 0.0; y : 1.0; z : 0.0 );
begin
  FGroupSounds := nil;
  FGroupMusic  := nil;
  Log( LOGINFO, 'Opening FMOD... ' );
  FLastError := FMOD_System_Create( @FSystem, FMOD_VERSION);
  if FLastError <> FMOD_OK then
  begin
    Log( LOGERROR, 'FMOD_System_Create failed, error : ' + GetError() );
    if FSystem <> nil then FMOD_System_Release(FSystem);
    FSystem := nil;
    Exit( False );
  end;
  FLastError := FMOD_System_Init( FSystem, 128, FMOD_INIT_NORMAL, nil);
  if FLastError <> FMOD_OK then
  begin
    Log( LOGERROR, 'FMOD_System_Init failed, error : ' + GetError() );
    FMOD_System_Close(FSystem);
    FMOD_System_Release(FSystem);
    FSystem := nil;
    Exit( False );
  end;
  Log( LOGINFO, 'FMOD Initialized.' );

  Check( FMOD_System_CreateChannelGroup( FSystem, 'sound', @FGroupSounds ) );
  Check( FMOD_System_CreateChannelGroup( FSystem, 'music', @FGroupMusic ) );
  Check( FMOD_System_set3DListenerAttributes( FSystem, 0, @CPos, @CVel, @CFwd, @CUp ) );
  Exit( True );
end;

function TFMODSound.LoadMusic(const aFileName: AnsiString; Streamed : Boolean): Pointer;
var iStream : PFMOD_SOUND;
    iInfo   : FMOD_CREATESOUNDEXINFO;
begin
  iStream := nil;
  Initialize( iInfo );
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  Check( FMOD_System_CreateStream( FSystem, PChar(aFileName), FMOD_2D or FMOD_CREATESTREAM or FMOD_LOOP_NORMAL, @iInfo, @iStream) );
  Exit( PushSimpleData( iStream ) );
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
  Initialize( iInfo );
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  Check( FMOD_System_CreateSound( FSystem, PChar(aFileName), iMode, @iInfo, @iSound ) );
  Exit( PushSimpleData( iSound ) );
end;

function TFMODSound.LoadMusicStream(Stream: TStream; Size : DWord; Streamed : Boolean ): Pointer;
var iStream : PFMOD_SOUND;
    iInfo  : FMOD_CREATESOUNDEXINFO;
    iData  : Pointer;
    iStore : TFMODSoundData;
begin
  iData := GetMem( Size );
  Stream.Read( iData^, Size );
  iStream := nil;
  Initialize( iInfo );
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  iInfo.length := Size;
  Check( FMOD_System_CreateStream( FSystem, PChar(iData), FMOD_2D or FMOD_CREATESTREAM or FMOD_LOOP_NORMAL or FMOD_OPENMEMORY, @iInfo, @iStream) );
  iStore.Size   := Size;
  iStore.Data   := iData;
  iStore.Struct := iStream;
  FInternalDataArray.Push( iStore );
  Exit( Pointer( FInternalDataArray.Size ) );
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
  Initialize( iInfo );
  FillChar(iInfo, SizeOf(FMOD_CREATESOUNDEXINFO), 0);
  iInfo.cbsize := SizeOf(FMOD_CREATESOUNDEXINFO);
  iInfo.length := Size;
  Check( FMOD_System_CreateSound( FSystem, PChar( iData ), iMode, @iInfo, @iSound ) );
  FreeMem( iData, Size );
  Exit( PushSimpleData( iSound ) );
end;

procedure TFMODSound.FreeMusic( aData: Pointer; const aType : Ansistring );
begin
  FreeSound( aData );
end;

procedure TFMODSound.FreeSound(aData: Pointer);
var iIndex : Integer;
begin
  iIndex := PtrInt( aData );
  if ( iIndex <= 0 ) or ( iIndex > FInternalDataArray.Size ) then
    raise Exception.Create('Internal Data Array corrupted on FreeSound!');
  Dec( iIndex );
  with FInternalDataArray[ iIndex ] do
  begin
    Check( FMOD_Sound_Release(PFMOD_SOUND(Struct)) );
    if Data <> nil then
      FreeMem( Data, Size );
  end;
end;

function TFMODSound.GetError(): AnsiString;
var iError : AnsiString;
begin
  iError := FMOD_ErrorString(FLastError);
  Exit( iError );
end;

procedure TFMODSound.PlaySound3D(aData: Pointer; aRelative: TCoord2D);
var iChannel   : PFMOD_CHANNEL;
    iPosition  : FMOD_VECTOR;
    iSound     : PFMOD_SOUND;
begin
  iSound      := PFMOD_SOUND(DataToSound( aData ));
  iPosition.x := aRelative.X * 0.2;
  iPosition.y := aRelative.Y * 0.2;
  iPosition.z := 0.0;
  Check( FMOD_System_PlaySound( FSystem, iSound, FGroupSounds, 1, @iChannel ) );
  Check( FMOD_Channel_SetVolume( iChannel, Single( Min( SoundVolume, 128 ) / 100.0 ) ) );
  Check( FMOD_Channel_set3DAttributes( iChannel, @iPosition, nil ) );
  Check( FMOD_Channel_SetPaused( iChannel, 0 ) );
end;

procedure TFMODSound.PlaySound(aData: Pointer; aVolume: Byte; aPan: Integer);
var iChannel : PFMOD_CHANNEL;
    iSound   : PFMOD_SOUND;
begin
  iSound      := PFMOD_SOUND(DataToSound( aData ));
  Check( FMOD_System_PlaySound( FSystem, iSound, FGroupSounds, 1, @iChannel ) );
  Check( FMOD_Channel_SetVolume( iChannel, ( Single(aVolume) / 100.0 ) ) );
  if aPan <> -1
    then Check( FMOD_Channel_SetPan( iChannel, ( Single(aPan-128) / 128.0 ) ) )
    else Check( FMOD_Channel_SetPan( iChannel, 0 ) );
  Check( FMOD_Channel_SetPaused( iChannel, 0 ) );
end;

procedure TFMODSound.PlayMusic(aData: Pointer; const aType : Ansistring; aRepeat: Boolean);
var iSound : PFMOD_SOUND;
begin
  iSound   := PFMOD_SOUND(DataToSound( aData ));
  FMOD_ChannelGroup_SetVolume( FGroupMusic, CalculateMusicVolume( MusicVolume ) );
  if aRepeat
    then FMOD_Sound_SetLoopCount( iSound, -1 )
    else FMOD_Sound_SetLoopCount( iSound, 0 );
  Check( FMOD_System_PlaySound( FSystem, iSound, FGroupMusic, 0, nil ) );
end;

procedure TFMODSound.StopMusic(aData: Pointer; const aType : Ansistring );
begin
  Check( FMOD_ChannelGroup_Stop( FGroupMusic ) );
end;

procedure TFMODSound.StopSound();
begin
  Check( FMOD_ChannelGroup_Stop( FGroupSounds ) );
end;

procedure TFMODSound.VolumeMusic(aData: Pointer; const aType : Ansistring; aVolume: Byte );
begin
  FMOD_ChannelGroup_SetVolume( FGroupMusic, CalculateMusicVolume( aVolume ) );
end;

function TFMODSound.CalculateMusicVolume( aVolume : Byte ) : Single;
var iValue : Single;
begin
  iValue := aVolume / 100.0;
  Result := iValue * iValue;
end;

function TFMODSound.PushSimpleData( aPointer : Pointer ) : Pointer;
var iData : TFMODSoundData;
begin
  iData.Struct   := aPointer;
  iData.Data     := nil;
  iData.Size     := 0;
  FInternalDataArray.Push( iData );
  Exit( Pointer( FInternalDataArray.Size ) );
end;

function TFMODSound.DataToSound( aData : Pointer ) : Pointer; inline;
var iIndex : Integer;
begin
  iIndex := PtrInt( aData );
  if ( iIndex <= 0 ) or ( iIndex > FInternalDataArray.Size ) then
    raise Exception.Create('Internal Data Array corrupted!');
  Exit( FInternalDataArray[ iIndex - 1 ].struct );
end;

procedure TFMODSound.Check( aResult : FMOD_RESULT );
begin
  FLastError := aResult;
  if aResult <> FMOD_OK then
    Log( LOGERROR, 'FMOD error : '+FMOD_ErrorString(aResult));
end;

end.
