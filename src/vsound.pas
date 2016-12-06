{$INCLUDE valkyrie.inc}
// @abstract(Sound system for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 03, 2009)
//
// Implements an abstract sound system for Valkyrie
//
// Default behaviour for music is stoping the previous song before
// playing a new one.

unit vsound;

interface

uses Classes, SysUtils, vutil, vrltools, vsystem, vgenerics, vluaconfig;

type ESoundException  = class( EException );
     TPtrArray        = specialize TGArray<Pointer>;
     TExtArray        = specialize TGArray<AnsiString>;
     TDWordArray      = specialize TGArray<DWord>;
     TIntAssocArray   = specialize TGHashMap<LongInt>;

// The basic sound class, published as the singleton @link(Sound).
// Should be initialized and disposed via TSystems.
type

{ TSound }

TSound = class(TSystem)
       // Initializes the Sound system.
       constructor Create; override;
       // Initializes the Sound system.
       constructor Configure( aConfig : TLuaConfig ); virtual;
       // Adds a MIDI/MOD file
       procedure RegisterMusic(const FileName : Ansistring; mID : Ansistring);
       // Adds a WAV file
       procedure RegisterSample(const FileName : Ansistring; mID : Ansistring);
       // Adds a MIDI/MOD file
       procedure RegisterMusic(Stream : TStream; Size : DWord; mID : Ansistring; aMusicType : AnsiString = '.mid');
       // Adds a WAV file
       procedure RegisterSample(Stream : TStream; Size : DWord; mID : Ansistring);
       //
       function MusicExists(const mID : Ansistring) : boolean;
       //
       function SampleExists(const mID : Ansistring) : boolean;
       //
       function GetSampleID(const  mID: Ansistring ) : DWord;
       // Plays a MIDI/MOD song
       procedure PlayMusic(const mID : Ansistring);
       // Plays a MIDI/MOD song once
       procedure PlayMusicOnce(const mID : Ansistring);
       // Plays a sample once
       procedure PlaySample(mID : Word; Volume : Byte = 128; Pan : Integer = -1 );
       // Plays a sample once
       procedure PlaySample(const mID : Ansistring; Volume : Byte = 128; Pan : Integer = -1);
       // Plays a sample once
       procedure PlaySample(mID : Word; aWhere : TCoord2D );
       // Plays a sample once
       procedure PlaySample(const mID : Ansistring; aWhere : TCoord2D );
       // Stops all MIDI/MOD songs
       procedure Silence;
       // Deinitializes the Sound system.
       destructor Destroy; override;
       // Sets the volume of the music.
       procedure SetMusicVolume(Volume : Byte);
       // Sets the volume of the sound effects.
       procedure SetSoundVolume(Volume : Byte);
       // Gets the volume of the music.
       function GetMusicVolume : Byte;
       // Gets the volume of the sound effects.
       function GetSoundVolume : Byte;
       //
       procedure Reset;
       // Utility Alias for VDF
       procedure MusicStreamLoader(Stream : TStream; Name : Ansistring; Size : DWord);
       // Utility Alias for VDF
       procedure SampleStreamLoader(Stream : TStream; Name : Ansistring; Size : DWord);
       // Implementation of StopSound
       procedure StopSound(); virtual; abstract;
       // Registers Lua API
       class procedure RegisterLuaAPI( L : Pointer; const aTableName : AnsiString = 'audio' );
     protected
       // Alloctes memory and stores it in the cache
       function GetCacheMem( Size : DWord ) : Pointer;
       // Implementation of Music Loading
       function LoadMusic( const aFileName : AnsiString; Streamed : Boolean ) : Pointer; virtual; abstract;
       // Implementation of Sound Loading
       function LoadSound( const aFileName : AnsiString ) : Pointer; virtual; abstract;
       // Implementation of Music Loading
       function LoadMusicStream( Stream : TStream; Size : DWord; Streamed : Boolean ) : Pointer; virtual; abstract;
       // Implementation of Sound Loading
       function LoadSoundStream( Stream : TStream; Size : DWord ) : Pointer; virtual; abstract;
       // Implementation of Music Freeing
       procedure FreeMusic( aData : Pointer; const aType : String ); virtual; abstract;
       // Implementation of Sound Freeing
       procedure FreeSound( aData : Pointer ); virtual; abstract;
       // Implementation of get error
       function GetError( ) : AnsiString; virtual; abstract;
       // Implementation of play Sound
       procedure PlaySound( aData : Pointer; aVolume : Byte; aPan : Integer = -1 ); virtual; abstract;
       // Implementation of play Sound 3D
       procedure PlaySound3D( aData : Pointer; aRelative : TCoord2D ); virtual; abstract;
       // Implementation of play Sound
       procedure PlayMusic( aData : Pointer; const aType : string; aRepeat : Boolean = True ); virtual; abstract;
       // Implementation of StopMusic
       procedure StopMusic( aData : Pointer; const aType : string ); virtual; abstract;
       // Implementation of VolumeMusic
       procedure VolumeMusic( aData : Pointer; const aType : string; aVolume : Byte ); virtual; abstract;

     private
       function RawRegisterMusic( aData : Pointer; const aType : string ) : DWord;
       function RawRegisterSample( aData : Pointer ) : DWord;
     protected
       FMusicEnabled    : Boolean;
       FSoundEnabled    : Boolean;
       FSurroundEnabled : Boolean;
       FPosMinVolume    : Byte;
       FPosFadeDist     : Byte;
       FListener        : TCoord2D;
       MusicPlaying : Word;
       MusicArray   : TPtrArray;
       MusicType    : TExtArray;
       MusicNames   : TIntAssocArray;
       SampleArray  : TPtrArray;
       SampleNames  : TIntAssocArray;
       MusicVolume  : Byte;
       SoundVolume  : Byte;
       CacheData    : TPtrArray;
       CacheSize    : TDWordArray;
     public
       property MusicEnabled    : Boolean  read FMusicEnabled    write FMusicEnabled;
       property SoundEnabled    : Boolean  read FSoundEnabled    write FSoundEnabled;
       property SurroundEnabled : Boolean  read FSurroundEnabled write FSurroundEnabled;
       property PosMinVolume    : Byte     read FPosMinVolume    write FPosMinVolume;
       property PosFadeDist     : Byte     read FPosFadeDist     write FPosFadeDist;
       property Listener        : TCoord2D read FListener        write FListener;
     end;

const Sound : TSound = nil;

implementation

uses vlualibrary, vluaext, vluatools, vmath;

{ TSound }

constructor TSound.Create;
begin
  inherited Create;
  FMusicEnabled    := True;
  FSoundEnabled    := True;
  FSurroundEnabled := False;
  FPosMinVolume    := 30;
  FPosFadeDist     := 25;
  FListener        := NewCoord2D( 0, 0 );

  MusicArray   := TPtrArray.Create;
  SampleArray  := TPtrArray.Create;
  MusicNames   := TIntAssocArray.Create;
  SampleNames  := TIntAssocArray.Create;
  MusicType    := TExtArray.Create;
  MusicPlaying := 0;
  MusicVolume  := 100;
  SoundVolume  := 100;
  CacheData    := TPtrArray.Create;
  CacheSize    := TDWordArray.Create;
  if UpCase(Self.ClassName) = 'TSOUND' then ESoundException.Create( 'Plain TSound system initialized!' );
end;

constructor TSound.Configure(aConfig: TLuaConfig);
begin
  FMusicEnabled    := aConfig.Configure( 'audio.music_enabled', True );
  FSoundEnabled    := aConfig.Configure( 'audio.sound_enabled', True );
  MusicVolume      := aConfig.Configure( 'audio.music_volume', 100 );
  SoundVolume      := aConfig.Configure( 'audio.sound_volume', 100 );
  FSurroundEnabled := aConfig.Configure( 'audio.surround_enabled', False );
  FPosMinVolume    := aConfig.Configure( 'audio.pos_min_volume', 30 );
  FPosFadeDist     := aConfig.Configure( 'audio.pos_fade_distance', 25 );
end;

function TSound.RawRegisterMusic ( aData : Pointer; const aType : string ) : DWord;
begin
  MusicArray.Push( aData );
  MusicType.Push( aType );
  Exit( MusicArray.Size - 1 );
end;

function TSound.RawRegisterSample ( aData : Pointer ) : DWord;
begin
  SampleArray.Push( aData );
  Exit( SampleArray.Size - 1 );
end;

procedure TSound.RegisterMusic(const FileName: Ansistring; mID: Ansistring);
var iPtr : Pointer;
    iExt : AnsiString;
begin
  iExt := ExtractFileExt( FileName );
  {$IFDEF Darwin}if iExt = '.mid' then Exit;{$ENDIF}
  iPtr := LoadMusic( FileName, (iExt = '.ogg') or (iExt = '.mp3') or (iExt = '.wav'));
  if iPtr = nil then raise ESoundException.Create( 'RegisterMusic('+Filename+'): '+GetError());
  MusicNames[mID] := RawRegisterMusic( iPtr, iExt );
end;

procedure TSound.RegisterSample(const FileName: Ansistring; mID: Ansistring);
var iPtr : Pointer;
begin
  iPtr := LoadSound( FileName );
  if iPtr = nil then raise ESoundException.Create( 'RegisterSample('+ Filename + '): '+GetError());
  SampleNames[mID] := RawRegisterSample( iPtr );
end;

procedure TSound.RegisterMusic(Stream: TStream; Size : DWord; mID: Ansistring; aMusicType : AnsiString = '.mid');
var iPtr : Pointer;
begin
  {$IFDEF Darwin}if aMusicType = '.mid' then Exit;{$ENDIF}
  iPtr := LoadMusicStream( Stream, Size, (aMusicType = '.ogg') or (aMusicType = '.mp3') or (aMusicType = '.wav'));
  if iPtr = nil then raise ESoundException.Create( 'RegisterMusic(Stream): '+GetError());
  MusicNames[mID] := RawRegisterMusic( iPtr, aMusicType );
end;

procedure TSound.RegisterSample(Stream: TStream; Size : DWord; mID: Ansistring);
var iPtr : Pointer;
begin
  iPtr := LoadSoundStream( Stream, Size );
  if iPtr = nil then raise ESoundException.Create( 'RegisterSample(Stream): '+GetError());
  SampleNames[mID] := RawRegisterSample( iPtr );
end;

function TSound.MusicExists(const mID: Ansistring): boolean;
begin
  Exit(MusicNames.Exists(mID));
end;

function TSound.SampleExists(const mID: Ansistring): boolean;
begin
  Exit(SampleNames.Exists(mID));
end;

function TSound.GetSampleID(const  mID: Ansistring ) : DWord;
begin
  if SampleNames.Exists(mID) then
    Exit( SampleNames[mID]+1 )
  else
    Exit( 0 );
end;

procedure TSound.PlayMusic(const mID : Ansistring);
var iId : DWord;
begin
  if not FMusicEnabled then Exit;
  if MusicPlaying <> 0 then Silence;
  if not MusicNames.Exists(mID) then raise ESoundException.Create('Trying play non-existent Music ID#'+mID+'!');
  iID := MusicNames[mID];
  PlayMusic( MusicArray[iID], MusicType[iID] );
  MusicPlaying := iID;
end;

procedure TSound.PlayMusicOnce(const mID : Ansistring);
var iId : DWord;
begin
  if not FMusicEnabled then Exit;
  if MusicPlaying <> 0 then Silence;
  if not MusicNames.Exists(mID) then raise ESoundException.Create('Trying play non-existent Music ID#'+mID+'!');
  iID := MusicNames[mID];
  PlayMusic( MusicArray[iID], MusicType[iID], False );
  MusicPlaying := iID;
end;


procedure TSound.PlaySample( mID: Word; Volume : Byte = 128; Pan : Integer = -1 );
begin
  if not FSoundEnabled then Exit;
  Dec( mID );
  if mID >= SampleArray.Size then raise ESoundException.Create('Trying play non-existent Sample ID#'+IntToStr(mID)+'!');

  if Volume = 128 then
    Volume := SoundVolume
  else
    Volume := Round(Volume*(SoundVolume/128.0));

  PlaySound( SampleArray[mID], Volume, Pan );
end;

procedure TSound.PlaySample( const mID: Ansistring; Volume : Byte = 128; Pan : Integer = -1 );
begin
  if not FSoundEnabled then Exit;
  PlaySample( SampleNames[mID] + 1, Volume, Pan );
end;

procedure TSound.PlaySample(mID: Word; aWhere: TCoord2D);
var iDistance : Word;
    iFDist    : Single;
    iVolume   : Word;
    iPan      : Integer;
    iRelative : TCoord2D;
begin
  if not FSoundEnabled then Exit;
  Dec( mID );
  if mID >= SampleArray.Size then raise ESoundException.Create('Trying play non-existent Sample ID#'+IntToStr(mID)+'!');
  iRelative := aWhere - FListener;
  if FSurroundEnabled then
  begin
    PlaySound3D( SampleArray[mID], iRelative );
    Exit;
  end;
  iDistance := iRelative.Length;
  iVolume := Clamp( Lerp( 127, FPosMinVolume, iDistance / FPosFadeDist ), FPosMinVolume, 127 );
  iVolume := Clamp( Round(iVolume*(SoundVolume/128.0)), 0, 255 );
  iPan := Clamp(iRelative.x * 15,-128,127)+128;
  PlaySound( SampleArray[mID], iVolume, iPan );
end;

procedure TSound.PlaySample(const mID: Ansistring; aWhere : TCoord2D);
begin
  if not FSoundEnabled then Exit;
  PlaySample( SampleNames[mID] + 1, aWhere );
end;

procedure TSound.Silence;
begin
  if MusicPlaying <> 0 then
    StopMusic( MusicArray[MusicPlaying], MusicType[MusicPlaying] );
  MusicPlaying := 0;
end;

destructor TSound.Destroy;
var iCount : Word;
begin
  if MusicArray.Size > 0 then
   for iCount := 0 to MusicArray.Size-1 do
     FreeMusic( MusicArray[iCount], MusicType[iCount] );
  if SampleArray.Size > 0 then
   for iCount := 0 to SampleArray.Size-1 do
     FreeSound( SampleArray[iCount] );
  if CacheData.Size > 0 then
  for iCount := 0 to CacheData.Size-1 do
    FreeMem( CacheData[iCount], CacheSize[iCount] );
  FreeAndNil( MusicArray );
  FreeAndNil( SampleArray );
  FreeAndNil( MusicNames );
  FreeAndNil( SampleNames );
  FreeAndNil( MusicType );
  inherited Destroy;
end;

procedure TSound.SetMusicVolume(Volume: Byte);
begin
  MusicVolume := Volume;
  if MusicPlaying <> 0 then
    VolumeMusic( MusicArray[MusicPlaying], MusicType[MusicPlaying], Volume );
end;

procedure TSound.SetSoundVolume(Volume: Byte);
begin
  SoundVolume := Volume;
end;

function TSound.GetMusicVolume: Byte;
begin
  Exit(MusicVolume);
end;

function TSound.GetSoundVolume: Byte;
begin
  Exit(SoundVolume);
end;

procedure TSound.Reset;
var iCount : DWord;
begin
  if MusicArray.Size > 0 then
  for iCount := 0 to MusicArray.Size-1 do
    FreeMusic( MusicArray[iCount], MusicType[iCount] );
  if SampleArray.Size > 0 then
  for iCount := 0 to SampleArray.Size-1 do
    FreeSound( SampleArray[iCount] );
  if not CacheData.isEmpty then
  for iCount := 0 to CacheData.Size-1 do
    FreeMem( CacheData[iCount], CacheSize[iCount] );

  MusicArray.Reset;
  SampleArray.Reset;
  MusicNames.Clear;
  SampleNames.Clear;
  MusicType.Reset;
  CacheData.Reset;
  CacheSize.Reset;
  MusicPlaying := 0;
  MusicVolume  := 100;
  SoundVolume  := 100;
end;

procedure TSound.MusicStreamLoader(Stream: TStream; Name: Ansistring;
  Size: DWord);
var Ext : AnsiString;
begin
  Ext := ExtractFileExt(Name);
  RegisterMusic( Stream, Size, LeftStr( Name, Length(Name) - Length(Ext) ), Ext );
end;

procedure TSound.SampleStreamLoader(Stream: TStream; Name: Ansistring;
  Size: DWord);
var Ext : AnsiString;
begin
  Ext := ExtractFileExt(Name);
  RegisterSample( Stream, Size, LeftStr( Name, Length(Name) - Length(Ext) ) );
end;

function TSound.GetCacheMem(Size: DWord): Pointer;
begin
  GetCacheMem := GetMem(Size);
  if GetCacheMem = nil then Exit;
  CacheData.Push( GetCacheMem );
  CacheSize.Push( Size );
end;

function lua_audio_register_sound(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  Sound.RegisterSample( lua_tostring( L, 1 ), lua_tostring( L, 2 ) );
  Result := 0;
end;

function lua_audio_register_music(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  Sound.RegisterMusic( lua_tostring( L, 1 ), lua_tostring( L, 2 ) );
  Result := 0;
end;

function lua_audio_sound_exists(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  lua_pushboolean( L, Sound.SampleExists( lua_tostring( L, 1 ) ) );
  Result := 1;
end;

function lua_audio_music_exists(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  lua_pushboolean( L, Sound.MusicExists( lua_tostring( L, 1 ) ) );
  Result := 1;
end;

function lua_audio_set_music_volume(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  Sound.SetMusicVolume( lua_tointeger( L, 1 ) );
  Result := 0;
end;

function lua_audio_get_music_volume(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  lua_pushnumber( L, Sound.GetMusicVolume );
  Result := 1;
end;

function lua_audio_set_sound_volume(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  Sound.SetSoundVolume( lua_tointeger( L, 1 ) );
  Result := 0;
end;

function lua_audio_get_sound_volume(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  lua_pushnumber( L, Sound.GetSoundVolume );
  Result := 1;
end;

function lua_audio_silence(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  Sound.Silence;
  Result := 0;
end;

function lua_audio_stop_sound(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  Sound.StopSound;
  Result := 0;
end;

function lua_audio_get_sound_id(L: Plua_State): Integer; cdecl;
begin
  if Sound = nil then Exit(0);
  lua_pushnumber( L, Sound.GetSampleID( lua_tostring( L, 1 ) ) );
  Result := 1;
end;

function lua_audio_play_music(L: Plua_State): Integer; cdecl;
var iMusicID : AnsiString;
begin
  if Sound = nil then Exit(0);
  iMusicID := lua_tostring( L, 1 );
  if iMusicID = '' then
  begin
    Sound.Silence;
    Exit(0);
  end;
  if not Sound.MusicExists( iMusicID ) then
  begin
    Sound.Log( LOGWARN, 'Music entry "%s" does not exist!', [iMusicID] );
    Exit( 0 );
  end;
  Sound.PlayMusic( iMusicID );
  Result := 0;
end;

function lua_audio_play_music_once(L: Plua_State): Integer; cdecl;
var iMusicID : AnsiString;
begin
  if Sound = nil then Exit(0);
  iMusicID := lua_tostring( L, 1 );
  if iMusicID = '' then
  begin
    Sound.Silence;
    Exit(0);
  end;
  if not Sound.MusicExists( iMusicID ) then
  begin
    Sound.Log( LOGWARN, 'Music entry "%s" does not exist!', [iMusicID] );
    Exit( 0 );
  end;
  Sound.PlayMusicOnce( iMusicID );
  Result := 0;
end;

function lua_audio_play_sound(L: Plua_State): Integer; cdecl;
var iVolume, iPan : Integer;
    iSoundNID     : Word;
    iSoundID      : AnsiString;
begin
  if Sound = nil then Exit(0);
  if not Sound.SoundEnabled then Exit(0);
  if lua_isnumber( L, 1 )
    then iSoundNID := lua_tointeger( L, 1 )
    else
    begin
      iSoundID := lua_tostring( L, 1 );
      if not Sound.SampleExists( iSoundID ) then
      begin
        Sound.Log( LOGWARN, 'Sound entry "%s" does not exist!', [iSoundID] );
        Exit( 0 );
      end;
      iSoundNID := Sound.SampleNames[iSoundID] + 1;
    end;
  if iSoundNID > Sound.SampleArray.Size then
  begin
    Sound.Log( LOGWARN, 'Trying play non-existent Sample ID#%d!', [iSoundNID] );
    Exit( 0 );
  end;
  if vlua_iscoord( L, 2 ) then
  begin
    Sound.PlaySample( iSoundID, vlua_tocoord( L, 2 ) );
  end
  else
  begin
    iVolume := 128;
    iPan    := -1;
    if lua_isnumber( L, 2 ) then iVolume := lua_tointeger( L, 2 );
    if lua_isnumber( L, 3 ) then iPan    := lua_tointeger( L, 3 );
    Sound.PlaySample( iSoundID, iVolume, iPan );
  end;
  Result := 0;
end;


const lua_audio_lib : array[0..14] of luaL_Reg = (
( name : 'register_sound';    func : @lua_audio_register_sound),
( name : 'register_music';    func : @lua_audio_register_music),
( name : 'sound_exists';      func : @lua_audio_sound_exists),
( name : 'music_exists';      func : @lua_audio_music_exists),
( name : 'set_music_volume';  func : @lua_audio_set_music_volume),
( name : 'get_music_volume';  func : @lua_audio_get_music_volume),
( name : 'set_sound_volume';  func : @lua_audio_set_sound_volume),
( name : 'get_sound_volume';  func : @lua_audio_get_sound_volume),
( name : 'silence';           func : @lua_audio_silence),
( name : 'stop_sound';        func : @lua_audio_stop_sound),
( name : 'get_sound_id';      func : @lua_audio_get_sound_id),
( name : 'play_music';        func : @lua_audio_play_music),
( name : 'play_music_once';   func : @lua_audio_play_music_once),
( name : 'play_sound';        func : @lua_audio_play_sound),
( name : nil;                 func : nil; )
);


class procedure TSound.RegisterLuaAPI(L: Pointer; const aTableName: AnsiString);
begin
  vlua_register( L, aTableName, lua_audio_lib );
end;


end.

