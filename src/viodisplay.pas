{$INCLUDE valkyrie.inc}
unit viodisplay;
interface
uses vnode, vutil, viotypes, vconfiguration;

type TIOWindowPolicy = record
  MinimumPixels   : TIOPoint; // smallest usable drawable size
  Resizable       : Boolean;
  PresetBase      : TIOPoint; // zero = standard 16:9 list, otherwise integer multiples
  FixedFullscreen : Boolean;  // windowed mode and its settings are unavailable
end;

// Launch override (--fullscreen / --windowed); never persisted.
type TIOForcedMode = ( VIO_FORCE_NONE, VIO_FORCE_WINDOWED, VIO_FORCE_FULLSCREEN );

type TIOPointArray = array of TIOPoint;

// Pure geometry and identity rules. Window sizes and usable areas are
// logical; a zero usable area means unknown and does not limit.
function IOMinimumWindowSize( const aMinimumPixels : TIOPoint; aDensity : Single ) : TIOPoint;
function IOClampWindowSize( const aSize, aUsable, aMinimum : TIOPoint ) : TIOPoint;
// Ascending; never empty: falls back to the minimum clamped to the usable area.
function IOWindowPresets( const aPolicy : TIOWindowPolicy; const aUsable : TIOPoint; aDensity : Single ) : TIOPointArray;
function IOPresetIndex( const aPresets : TIOPointArray; const aSize : TIOPoint ) : Integer;
// Largest preset that fits within aSize, otherwise the smallest.
function IONearestPreset( const aPresets : TIOPointArray; const aSize : TIOPoint ) : Integer;
// Largest preset within ~85% of the usable area, otherwise the clamped minimum.
function IOFirstWindowSize( const aPresets : TIOPointArray; const aUsable, aMinimum : TIOPoint ) : TIOPoint;
function IOParseDisplayIdentity( const aValue : AnsiString; out aName : AnsiString; out aOrdinal : Integer ) : Boolean;
function IOExactDisplay( aDisplays : TIODisplayInfoArray; const aValue : AnsiString ) : Integer;
// Launch validation of a saved display value. Returns the display index, or
// -1 for Auto. aReason explains any fallback and is empty otherwise.
function IOFindDisplay( aDisplays : TIODisplayInfoArray; const aValue : AnsiString; out aReason : AnsiString ) : Integer;
function IODisplayLabel( aDisplays : TIODisplayInfoArray; aIndex : Integer ) : AnsiString;

// Display controller: mode state, pending requests, Alt+Enter, display choice,
// presets, window geometry and the shared display settings. Requests are
// recorded and applied by Update at the frame boundary. Inactive, and without
// effect on the driver, until the game sets a policy.
type TIODisplay = class( TVObject )
  constructor Create( aDriver : TIOWindowDriver );
  destructor Destroy; override;
  // Activates the controller on the first call. Set again when the game's
  // font or console changes.
  procedure SetPolicy( const aPolicy : TIOWindowPolicy );
  procedure RequestMode( aMode : TIOWindowMode );
  procedure RequestToggle;
  // Stored as the windowed size at once; applied when the window is windowed.
  procedure RequestWindowSize( const aSize : TIOPoint );
  // '' = Auto.
  procedure RequestDisplay( const aChoice : AnsiString );
  // "Auto", the connected displays, and a saved but disconnected choice.
  // Borrowed; rebuilt on each call.
  function DisplayChoices : TIOStringArray;
  function DisplayChoiceIndex : Integer;
  // The value RequestDisplay expects for a DisplayChoices index.
  function DisplayChoiceValue( aIndex : Integer ) : AnsiString;
  function DisplayChoiceVisible : Boolean;
  // Filtered by the usable area of the window's display and the minimum.
  function Presets : TIOPointArray;
  // -1 = Custom (resizable policies only).
  function PresetIndex : Integer;
  // Called by TIO at the frame boundary; True when a layout pass is needed.
  function Update( out aMetrics : TIODisplayMetrics ) : Boolean;
  class procedure RegisterSettings( aGroup : TConfigurationGroup );
  // Reads the shared entries; the launch placement runs at the next Update.
  procedure LoadSettings( aConfig : TConfigurationManager );
  procedure StoreSettings( aConfig : TConfigurationManager );
  // Seeds the windowed size from a game's legacy size entry when no windowed
  // size is saved and the legacy size fits the current display. Call after
  // LoadSettings and before the first Update.
  procedure MigrateLegacyWindowSize( const aLegacySize : TIOPoint );
private
  procedure ApplyLaunch;
  procedure ApplyRequests;
  procedure ReadState;
  procedure RecordState;
  function Transitioning : Boolean;
  function LaunchMode : TIOWindowMode;
  function CurrentUsable : TIOPoint;
  function CurrentDensity : Single;
  function CurrentMinimum : TIOPoint;
  function FitWindowSize( const aSize, aUsable : TIOPoint; aDensity : Single ) : TIOPoint;
private
  FDriver         : TIOWindowDriver;
  FPolicy         : TIOWindowPolicy;
  FActive         : Boolean;
  FInitialized    : Boolean;
  FLimitsPending  : Boolean;
  FLaunchPending  : Boolean;
  FLayoutPending  : Boolean;
  FModePending    : Boolean;
  FRequestedMode  : TIOWindowMode;
  FTargetMode     : TIOWindowMode;
  FTargetSince    : DWord;
  FSizePending    : Boolean;
  FPendingSize    : TIOPoint;
  FDisplayPending : Integer;
  FMetrics        : TIODisplayMetrics; // last read
  FLaidOut        : TIODisplayMetrics; // last passed to layout
  FForcedMode     : TIOForcedMode;
  FFullscreenSetting : Boolean;
  FWindowSizeSetting : TIOPoint;
  FDisplaySetting    : AnsiString;
  FChoices        : TIOStringArray;
public
  property Driver     : TIOWindowDriver read FDriver;
  property Active     : Boolean         read FActive;
  property Policy     : TIOWindowPolicy read FPolicy;
  // Actual mode as last read; meaningful once Active.
  property Mode       : TIOWindowMode   read FMetrics.Mode;
  // Saved windowed size (logical); zero until known.
  property WindowSize : TIOPoint        read FWindowSizeSetting;
  // Set before the first Update. While forced, mode changes are not stored.
  property ForcedMode : TIOForcedMode   read FForcedMode write FForcedMode;
end;

implementation

uses sysutils, math, vdebug;

const StandardPresets : array[0..5] of TIOPoint = (
  ( x : 1280; y : 720 ),
  ( x : 1600; y : 900 ),
  ( x : 1920; y : 1080 ),
  ( x : 2560; y : 1440 ),
  ( x : 3200; y : 1800 ),
  ( x : 3840; y : 2160 )
);

// How long a requested mode change may take before the actual state is
// adopted (macOS Spaces, Wayland, GameScope, or a refused request).
const TransitionTimeoutMs = 2000;

// First windowed size uses at most this share of the usable area.
const FirstWindowShare = 0.85;

function IOMinimumWindowSize( const aMinimumPixels : TIOPoint; aDensity : Single ) : TIOPoint;
begin
  if aDensity <= 0 then aDensity := 1.0;
  Result.Init( Ceil( aMinimumPixels.X / aDensity ), Ceil( aMinimumPixels.Y / aDensity ) );
end;

function IOClampWindowSize( const aSize, aUsable, aMinimum : TIOPoint ) : TIOPoint;
var iLower : TIOPoint;
begin
  iLower := aMinimum;
  Result := aSize;
  if aUsable.X > 0 then
  begin
    iLower.X := Min( iLower.X, aUsable.X );
    Result.X := Min( Result.X, aUsable.X );
  end;
  if aUsable.Y > 0 then
  begin
    iLower.Y := Min( iLower.Y, aUsable.Y );
    Result.Y := Min( Result.Y, aUsable.Y );
  end;
  Result.X := Max( Result.X, iLower.X );
  Result.Y := Max( Result.Y, iLower.Y );
end;

function Fits( const aSize, aUsable : TIOPoint ) : Boolean;
begin
  Exit( ( ( aUsable.X <= 0 ) or ( aSize.X <= aUsable.X ) ) and
        ( ( aUsable.Y <= 0 ) or ( aSize.Y <= aUsable.Y ) ) );
end;

function AtLeast( const aSize, aMinimum : TIOPoint ) : Boolean;
begin
  Exit( ( aSize.X >= aMinimum.X ) and ( aSize.Y >= aMinimum.Y ) );
end;

procedure AddPreset( var aPresets : TIOPointArray; const aSize : TIOPoint );
begin
  SetLength( aPresets, Length( aPresets ) + 1 );
  aPresets[ High( aPresets ) ] := aSize;
end;

function IOWindowPresets( const aPolicy : TIOWindowPolicy; const aUsable : TIOPoint; aDensity : Single ) : TIOPointArray;
var iMinimum : TIOPoint;
    iSize    : TIOPoint;
    i        : Integer;
begin
  Result   := nil;
  iMinimum := IOMinimumWindowSize( aPolicy.MinimumPixels, aDensity );
  if ( aPolicy.PresetBase.X > 0 ) and ( aPolicy.PresetBase.Y > 0 ) then
  begin
    // Unknown usable area: offer the base only.
    i := 1;
    repeat
      iSize := aPolicy.PresetBase * i;
      if not Fits( iSize, aUsable ) then Break;
      if AtLeast( iSize, iMinimum ) then AddPreset( Result, iSize );
      Inc( i );
    until ( aUsable.X <= 0 ) or ( aUsable.Y <= 0 );
  end
  else
    for i := Low( StandardPresets ) to High( StandardPresets ) do
      if Fits( StandardPresets[i], aUsable ) and AtLeast( StandardPresets[i], iMinimum ) then
        AddPreset( Result, StandardPresets[i] );
  if Length( Result ) = 0 then
    AddPreset( Result, IOClampWindowSize( iMinimum, aUsable, iMinimum ) );
end;

function IOPresetIndex( const aPresets : TIOPointArray; const aSize : TIOPoint ) : Integer;
var i : Integer;
begin
  for i := 0 to High( aPresets ) do
    if aPresets[i] = aSize then Exit( i );
  Exit( -1 );
end;

function IONearestPreset( const aPresets : TIOPointArray; const aSize : TIOPoint ) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := 0 to High( aPresets ) do
    if Fits( aPresets[i], aSize ) then
      Result := i;
end;

function IOFirstWindowSize( const aPresets : TIOPointArray; const aUsable, aMinimum : TIOPoint ) : TIOPoint;
var iLimit : TIOPoint;
    i      : Integer;
begin
  iLimit.Init( Trunc( aUsable.X * FirstWindowShare ), Trunc( aUsable.Y * FirstWindowShare ) );
  for i := High( aPresets ) downto 0 do
    if Fits( aPresets[i], iLimit ) then
      Exit( aPresets[i] );
  Exit( IOClampWindowSize( aMinimum, aUsable, aMinimum ) );
end;

function IOParseDisplayIdentity( const aValue : AnsiString; out aName : AnsiString; out aOrdinal : Integer ) : Boolean;
var iHash : Integer;
    i     : Integer;
begin
  aName    := '';
  aOrdinal := 0;
  iHash := 0;
  for i := Length( aValue ) downto 1 do
    if aValue[i] = '#' then
    begin
      iHash := i;
      Break;
    end;
  if iHash <= 1 then Exit( False );
  if not TryStrToInt( Copy( aValue, iHash + 1, MaxInt ), aOrdinal ) then Exit( False );
  if aOrdinal < 0 then Exit( False );
  aName := Copy( aValue, 1, iHash - 1 );
  Exit( True );
end;

function IOExactDisplay( aDisplays : TIODisplayInfoArray; const aValue : AnsiString ) : Integer;
var i : Integer;
begin
  for i := 0 to aDisplays.Size - 1 do
    if IODisplayIdentity( aDisplays[i] ) = aValue then
      Exit( i );
  Exit( -1 );
end;

function IOFindDisplay( aDisplays : TIODisplayInfoArray; const aValue : AnsiString; out aReason : AnsiString ) : Integer;
var iName    : AnsiString;
    iOrdinal : Integer;
    i        : Integer;
begin
  aReason := '';
  if aValue = '' then Exit( -1 );
  if not IOParseDisplayIdentity( aValue, iName, iOrdinal ) then
  begin
    aReason := Format( 'malformed display value "%s", using Auto', [ aValue ] );
    Exit( -1 );
  end;
  Result := IOExactDisplay( aDisplays, aValue );
  if Result >= 0 then Exit;
  for i := 0 to aDisplays.Size - 1 do
    if aDisplays[i].Name = iName then
    begin
      aReason := Format( 'display "%s" has no instance #%d, using #%d', [ iName, iOrdinal, aDisplays[i].Ordinal ] );
      Exit( i );
    end;
  aReason := Format( 'display "%s" is not connected, using Auto', [ iName ] );
  Exit( -1 );
end;

function IODisplayLabel( aDisplays : TIODisplayInfoArray; aIndex : Integer ) : AnsiString;
var iRepeated : Boolean;
    i         : Integer;
begin
  iRepeated := False;
  for i := 0 to aDisplays.Size - 1 do
    if ( i <> aIndex ) and ( aDisplays[i].Name = aDisplays[aIndex].Name ) then
      iRepeated := True;
  Result := aDisplays[aIndex].Name;
  if iRepeated then
    Result += ' #' + IntToStr( aDisplays[aIndex].Ordinal + 1 );
  Result += Format( ' (%d x %d)', [ aDisplays[aIndex].Bounds.w, aDisplays[aIndex].Bounds.h ] );
end;

{ TIODisplay }

constructor TIODisplay.Create( aDriver : TIOWindowDriver );
begin
  inherited Create;
  FDriver         := aDriver;
  FActive         := False;
  FInitialized    := False;
  FLimitsPending  := False;
  FLaunchPending  := False;
  FLayoutPending  := False;
  FModePending    := False;
  FSizePending    := False;
  FDisplayPending := -1;
  FForcedMode     := VIO_FORCE_NONE;
  FFullscreenSetting := True;
  FWindowSizeSetting := Point( 0, 0 );
  FDisplaySetting    := '';
  FillChar( FPolicy, SizeOf( FPolicy ), 0 );
  FillChar( FMetrics, SizeOf( FMetrics ), 0 );
  FLaidOut := FMetrics;
  FChoices := TIOStringArray.Create;
end;

destructor TIODisplay.Destroy;
begin
  FreeAndNil( FChoices );
  inherited Destroy;
end;

procedure TIODisplay.SetPolicy( const aPolicy : TIOWindowPolicy );
begin
  FPolicy        := aPolicy;
  FLimitsPending := True;
  FLayoutPending := True;
  if FActive then Exit;
  FActive := True;
  FDriver.DisplayManaged := True;
end;

procedure TIODisplay.RequestMode( aMode : TIOWindowMode );
begin
  if FPolicy.FixedFullscreen then aMode := VIO_FULLSCREEN;
  FRequestedMode := aMode;
  FModePending   := True;
end;

procedure TIODisplay.RequestToggle;
var iFrom : TIOWindowMode;
begin
  if FPolicy.FixedFullscreen then Exit;
  if FModePending
    then iFrom := FRequestedMode
    else iFrom := FTargetMode;
  if iFrom = VIO_FULLSCREEN
    then RequestMode( VIO_WINDOWED )
    else RequestMode( VIO_FULLSCREEN );
end;

procedure TIODisplay.RequestWindowSize( const aSize : TIOPoint );
begin
  FWindowSizeSetting := FitWindowSize( aSize, CurrentUsable, CurrentDensity );
  FPendingSize       := FWindowSizeSetting;
  FSizePending       := True;
end;

procedure TIODisplay.RequestDisplay( const aChoice : AnsiString );
var iReason : AnsiString;
    iIndex  : Integer;
begin
  FDisplaySetting := aChoice;
  // Auto keeps the window where it is; it only matters at launch.
  if aChoice = '' then Exit;
  iIndex := IOFindDisplay( FDriver.Displays, aChoice, iReason );
  if iReason <> '' then Log( LOGWARN, 'Display: ' + iReason );
  if iIndex >= 0 then FDisplayPending := iIndex;
end;

function TIODisplay.DisplayChoices : TIOStringArray;
var iDisplays : TIODisplayInfoArray;
    iName     : AnsiString;
    iOrdinal  : Integer;
    i         : Integer;
begin
  iDisplays := FDriver.Displays;
  FChoices.Clear;
  FChoices.Push( 'Auto' );
  for i := 0 to iDisplays.Size - 1 do
    FChoices.Push( IODisplayLabel( iDisplays, i ) );
  if ( IOExactDisplay( iDisplays, FDisplaySetting ) < 0 ) and
     IOParseDisplayIdentity( FDisplaySetting, iName, iOrdinal ) then
    FChoices.Push( iName + ' (disconnected)' );
  Exit( FChoices );
end;

function TIODisplay.DisplayChoiceIndex : Integer;
var iDisplays : TIODisplayInfoArray;
    iName     : AnsiString;
    iOrdinal  : Integer;
begin
  iDisplays := FDriver.Displays;
  Result := IOExactDisplay( iDisplays, FDisplaySetting );
  if Result >= 0 then Exit( Result + 1 );
  if IOParseDisplayIdentity( FDisplaySetting, iName, iOrdinal )
    then Exit( iDisplays.Size + 1 )
    else Exit( 0 );
end;

function TIODisplay.DisplayChoiceValue( aIndex : Integer ) : AnsiString;
var iDisplays : TIODisplayInfoArray;
begin
  iDisplays := FDriver.Displays;
  if aIndex <= 0 then Exit( '' );
  if aIndex <= iDisplays.Size then Exit( IODisplayIdentity( iDisplays[ aIndex - 1 ] ) );
  Exit( FDisplaySetting );
end;

function TIODisplay.DisplayChoiceVisible : Boolean;
begin
  if FPolicy.FixedFullscreen then Exit( False );
  Exit( ( FDriver.Displays.Size > 1 ) or ( FDisplaySetting <> '' ) );
end;

function TIODisplay.Presets : TIOPointArray;
begin
  Exit( IOWindowPresets( FPolicy, CurrentUsable, CurrentDensity ) );
end;

function TIODisplay.PresetIndex : Integer;
var iPresets : TIOPointArray;
begin
  iPresets := Presets;
  Result := IOPresetIndex( iPresets, FWindowSizeSetting );
  if ( Result < 0 ) and not FPolicy.Resizable then
    Result := IONearestPreset( iPresets, FWindowSizeSetting );
end;

function TIODisplay.Update( out aMetrics : TIODisplayMetrics ) : Boolean;
begin
  aMetrics := FLaidOut;
  if not FActive then Exit( False );

  if FDriver.TakeToggleRequest then RequestToggle;
  if FLimitsPending then
  begin
    FLimitsPending := False;
    if not FDriver.SetWindowLimits( FPolicy.MinimumPixels, FPolicy.Resizable ) then
      Log( LOGWARN, 'Display: window limits were not applied.' );
  end;
  if FLaunchPending then ApplyLaunch;

  if FDriver.DisplayDirty or not FInitialized then ReadState;
  ApplyRequests;
  if FDriver.DisplayDirty then ReadState;
  RecordState;

  // Keep the last layout while minimized or without a drawable.
  if FMetrics.Minimized or ( FMetrics.PixelSize.X <= 0 ) or ( FMetrics.PixelSize.Y <= 0 ) then
    Exit( False );
  Result := FLayoutPending
    or ( FMetrics.PixelSize <> FLaidOut.PixelSize )
    or ( FMetrics.PixelDensity <> FLaidOut.PixelDensity )
    or ( FMetrics.Mode <> FLaidOut.Mode );
  if Result then
  begin
    FLayoutPending := False;
    FLaidOut       := FMetrics;
  end;
  aMetrics := FLaidOut;
end;

procedure TIODisplay.ReadState;
begin
  if not FDriver.ReadMetrics( FMetrics ) then
    Log( LOGWARN, 'Display: could not read the drawable size.' );
  if FInitialized then Exit;
  FInitialized  := True;
  FTargetMode   := FMetrics.Mode;
  FTargetSince  := FDriver.GetMs;
end;

function TIODisplay.Transitioning : Boolean;
begin
  Exit( ( FMetrics.Mode <> FTargetMode ) and
        ( FDriver.GetMs - FTargetSince < TransitionTimeoutMs ) );
end;

procedure TIODisplay.ApplyRequests;
begin
  if FModePending then
  begin
    FModePending := False;
    if ( FRequestedMode <> FTargetMode ) or ( FRequestedMode <> FMetrics.Mode ) then
      if FDriver.SetWindowMode( FRequestedMode ) then
      begin
        FTargetMode  := FRequestedMode;
        FTargetSince := FDriver.GetMs;
        if FForcedMode = VIO_FORCE_NONE then
          FFullscreenSetting := FRequestedMode = VIO_FULLSCREEN;
      end
      else
        Log( LOGERROR, 'Display: window mode change refused, keeping the current mode.' );
  end;

  if FDisplayPending >= 0 then
  begin
    if not FDriver.MoveToDisplay( FDisplayPending ) then
      Log( LOGERROR, 'Display: could not move the window to display %d.', [ FDisplayPending ] );
    FDisplayPending := -1;
  end;

  // A fullscreen window discards size changes, so wait until it is windowed.
  if FSizePending and ( FTargetMode = VIO_WINDOWED ) and
     ( FMetrics.Mode = VIO_WINDOWED ) and not FMetrics.Minimized then
  begin
    FSizePending := False;
    if not FDriver.SetWindowSize( FPendingSize, True ) then
      Log( LOGERROR, 'Display: window size %dx%d refused.', [ FPendingSize.X, FPendingSize.Y ] );
  end;
end;

procedure TIODisplay.RecordState;
var iSize : TIOPoint;
begin
  if Transitioning then Exit;
  if FMetrics.Mode <> FTargetMode then
  begin
    // A change made outside the controller, or one that never completed.
    Log( 'Display: adopting the actual window mode.' );
    FTargetMode := FMetrics.Mode;
    if FForcedMode = VIO_FORCE_NONE then
      FFullscreenSetting := FTargetMode = VIO_FULLSCREEN;
  end;
  // Only a settled, plain windowed window owns the saved size.
  if ( FMetrics.Mode = VIO_WINDOWED ) and not FMetrics.Maximized and
     not FMetrics.Minimized and not FSizePending then
    if FDriver.GetWindowSize( iSize ) and ( iSize.X > 0 ) and ( iSize.Y > 0 ) then
      FWindowSizeSetting := iSize;
end;

function TIODisplay.LaunchMode : TIOWindowMode;
begin
  if FPolicy.FixedFullscreen then Exit( VIO_FULLSCREEN );
  case FForcedMode of
    VIO_FORCE_WINDOWED   : Exit( VIO_WINDOWED );
    VIO_FORCE_FULLSCREEN : Exit( VIO_FULLSCREEN );
  end;
  if FFullscreenSetting
    then Exit( VIO_FULLSCREEN )
    else Exit( VIO_WINDOWED );
end;

procedure TIODisplay.ApplyLaunch;
var iDisplays : TIODisplayInfoArray;
    iReason   : AnsiString;
    iTarget   : Integer;
    iActual   : Integer;
    iUsable   : TIOPoint;
    iSize     : TIOPoint;
begin
  FLaunchPending := False;
  iDisplays := FDriver.Displays;
  iTarget   := IOFindDisplay( iDisplays, FDisplaySetting, iReason );
  if iReason <> '' then Log( LOGWARN, 'Display: ' + iReason + ' for this session.' );
  if iTarget >= 0 then
  begin
    iUsable := iDisplays[ iTarget ].Usable.Dim;
    if not FDriver.MoveToDisplay( iTarget ) then
      Log( LOGWARN, 'Display: could not place the window on the chosen display.' );
    FDriver.SynchronizeWindow;
  end
  else
    iUsable := CurrentUsable;
  ReadState;

  if ( FWindowSizeSetting.X > 0 ) and ( FWindowSizeSetting.Y > 0 )
    then iSize := FitWindowSize( FWindowSizeSetting, iUsable, CurrentDensity )
    else iSize := IOFirstWindowSize( IOWindowPresets( FPolicy, iUsable, CurrentDensity ), iUsable, CurrentMinimum );
  FWindowSizeSetting := iSize;
  if FMetrics.Mode = VIO_WINDOWED then
  begin
    if not FDriver.SetWindowSize( iSize, True ) then
      Log( LOGWARN, 'Display: launch window size %dx%d refused.', [ iSize.X, iSize.Y ] );
  end
  else
  begin
    FPendingSize := iSize;
    FSizePending := True;
  end;
  FDriver.SynchronizeWindow;
  ReadState;

  if ( iTarget >= 0 ) and FDriver.GetWindowDisplay( iActual ) and ( iActual <> iTarget ) then
    Log( LOGWARN, 'Display: the window was placed on another display, keeping it there.' );

  FTargetMode    := FMetrics.Mode;
  FTargetSince   := FDriver.GetMs;
  FRequestedMode := LaunchMode;
  FModePending   := True;
end;

function TIODisplay.CurrentUsable : TIOPoint;
var iArea : TIORect;
begin
  if FDriver.GetUsableArea( iArea )
    then Exit( iArea.Dim )
    else Exit( Point( 0, 0 ) );
end;

function TIODisplay.CurrentDensity : Single;
begin
  if FMetrics.PixelDensity > 0
    then Exit( FMetrics.PixelDensity )
    else Exit( 1.0 );
end;

function TIODisplay.CurrentMinimum : TIOPoint;
begin
  Exit( IOMinimumWindowSize( FPolicy.MinimumPixels, CurrentDensity ) );
end;

// Clamps to the minimum and usable area; a fixed-size policy snaps to a preset.
function TIODisplay.FitWindowSize( const aSize, aUsable : TIOPoint; aDensity : Single ) : TIOPoint;
var iPresets : TIOPointArray;
begin
  Result := IOClampWindowSize( aSize, aUsable, IOMinimumWindowSize( FPolicy.MinimumPixels, aDensity ) );
  if FPolicy.Resizable then Exit;
  iPresets := IOWindowPresets( FPolicy, aUsable, aDensity );
  Result := iPresets[ IONearestPreset( iPresets, Result ) ];
end;

class procedure TIODisplay.RegisterSettings( aGroup : TConfigurationGroup );
begin
  aGroup.AddToggle( 'fullscreen', True );
  aGroup.AddInteger( 'window_width', 0 );
  aGroup.AddInteger( 'window_height', 0 );
  aGroup.AddString( 'display', '' );
end;

procedure TIODisplay.LoadSettings( aConfig : TConfigurationManager );
begin
  FFullscreenSetting := aConfig.GetBoolean( 'fullscreen' );
  FWindowSizeSetting.Init( aConfig.GetInteger( 'window_width' ), aConfig.GetInteger( 'window_height' ) );
  if ( FWindowSizeSetting.X <= 0 ) or ( FWindowSizeSetting.Y <= 0 ) then
    FWindowSizeSetting.Init( 0, 0 );
  FDisplaySetting := aConfig.GetString( 'display' );
  FLaunchPending  := True;
end;

procedure TIODisplay.StoreSettings( aConfig : TConfigurationManager );
begin
  aConfig.CastBoolean( 'fullscreen' ).Value    := FFullscreenSetting;
  aConfig.CastInteger( 'window_width' ).Value  := FWindowSizeSetting.X;
  aConfig.CastInteger( 'window_height' ).Value := FWindowSizeSetting.Y;
  aConfig.CastString( 'display' ).Value        := FDisplaySetting;
end;

procedure TIODisplay.MigrateLegacyWindowSize( const aLegacySize : TIOPoint );
begin
  if ( FWindowSizeSetting.X > 0 ) and ( FWindowSizeSetting.Y > 0 ) then Exit;
  if ( aLegacySize.X <= 0 ) or ( aLegacySize.Y <= 0 ) then Exit;
  if not Fits( aLegacySize, CurrentUsable ) then Exit;
  Log( 'Display: window size %dx%d migrated from legacy settings.', [ aLegacySize.X, aLegacySize.Y ] );
  FWindowSizeSetting := aLegacySize;
end;

end.
