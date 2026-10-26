{$INCLUDE valkyrie.inc}
// Generic keyboard and controller binding storage
unit vbindings;
interface
uses vconfiguration, vioevent, vgenerics;

type TBindingAction = Integer;

const BINDING_NONE        = -1;
      BINDING_FORWARD_LUA = High(TBindingAction);

type TBindingInfo = record
  Action      : TBindingAction;
  ID          : AnsiString;
  Group       : AnsiString;
  Default     : Integer;
  Name        : AnsiString;
  Description : AnsiString;
end;

// Connects binding metadata to integer configuration entries.
type TBindingCatalog = class
  constructor Create( const aInfo : array of TBindingInfo );
  procedure RegisterGroup( aGroup : TConfigurationGroup; const aGroupID : AnsiString );
  // Checks catalog registration only; omitted setting values
  // and unbound actions are valid.
  procedure ValidateRegistration;
  procedure ResetValues;
  function ActionForID( const aID : AnsiString ) : TBindingAction;
  // Looks up registered values, including device-specific unbound values.
  function ActionForValue( aValue : Integer ) : TBindingAction;
  // Assigns a keyboard chord and unbinds its other owners in this catalog.
  procedure SetKey( aAction : TBindingAction; aKey : TIOKeyCode );
  function ConfigurationValue( aAction : TBindingAction ) : Integer;
  function Info( aAction : TBindingAction ) : TBindingInfo;
protected
  FInfo          : array of TBindingInfo;
  procedure SetConfigurationValue( aAction : TBindingAction; aValue : Integer );
private
  FEntry         : array of TIntegerConfigurationEntry;
  FRegistrations : array of Integer;
  FIndexedAction : array of TBindingAction;
  FActionIndex   : array of Integer;
  function FindDefinition( aAction : TBindingAction ) : Integer;
end;

// Stores keyboard and controller bindings for one caller-defined
// input context.
type TBindingContext = class
  constructor Create;
  procedure Clear;
  procedure BindKey( aKey : TIOKeyCode; aAction : TBindingAction );
  procedure BindPad( aButton : TIOPadButton; aAction : TBindingAction );
  procedure LoadKeys( aCatalog : TBindingCatalog );
  procedure LoadPad( aCatalog : TBindingCatalog );
  function ResolveKey( aKey : TIOKeyCode ) : TBindingAction;
  function ResolvePad( aButton : TIOPadButton ) : TBindingAction;
  function GetKey( aAction : TBindingAction ) : TIOKeyCode;
  function GetPadButton( aAction : TBindingAction ) : TIOPadButton;
private
  FKeys : array[0..IOKeyCodeMax] of TBindingAction;
  FPads : array[TIOPadButton] of TBindingAction;
end;

type TBindingContextArray = specialize TGObjectArray< TBindingContext >;

// Owns the binding contexts created by engine consumers.
type TBindings = class
  constructor Create;
  function CreateContext : TBindingContext;
  destructor Destroy; override;
private
  FContexts : TBindingContextArray;
end;

implementation

uses SysUtils, vdebug, vutil;

// TBindingCatalog

constructor TBindingCatalog.Create( const aInfo : array of TBindingInfo );
var iInfo, iOther, iInsert, iDefinition : Integer;
    iAction : TBindingAction;
begin
  SetLength(FInfo, Length(aInfo));
  SetLength(FEntry, Length(aInfo));
  SetLength(FRegistrations, Length(aInfo));
  SetLength(FIndexedAction, Length(aInfo));
  SetLength(FActionIndex, Length(aInfo));

  for iInfo := 0 to High(aInfo) do
  begin
    if (aInfo[iInfo].Action < 0) or
       (aInfo[iInfo].Action = BINDING_FORWARD_LUA) then
      raise Exception.Create('TBindingCatalog - action '+IntToStr(aInfo[iInfo].Action)+' is reserved!');

    for iOther := 0 to iInfo - 1 do
    begin
      if aInfo[iOther].Action = aInfo[iInfo].Action then
        raise Exception.Create('TBindingCatalog - action '+IntToStr(aInfo[iInfo].Action)+' is duplicated!');
      if (aInfo[iInfo].ID <> '') and (aInfo[iOther].ID = aInfo[iInfo].ID) then
        raise Exception.Create('TBindingCatalog - ID '+aInfo[iInfo].ID+' is duplicated!');
    end;

    FInfo[iInfo] := aInfo[iInfo];
    FIndexedAction[iInfo] := aInfo[iInfo].Action;
    FActionIndex[iInfo] := iInfo;
  end;

  for iInfo := 1 to High(FActionIndex) do
  begin
    iAction := FIndexedAction[iInfo];
    iDefinition := FActionIndex[iInfo];
    iInsert := iInfo;
    while (iInsert > 0) and (FIndexedAction[iInsert - 1] > iAction) do
    begin
      FIndexedAction[iInsert] := FIndexedAction[iInsert - 1];
      FActionIndex[iInsert] := FActionIndex[iInsert - 1];
      Dec(iInsert);
    end;
    FIndexedAction[iInsert] := iAction;
    FActionIndex[iInsert] := iDefinition;
  end;
end;

function TBindingCatalog.FindDefinition( aAction : TBindingAction ) : Integer;
var iLow, iHigh, iMiddle : Integer;
begin
  iLow := 0;
  iHigh := High(FActionIndex);
  while iLow <= iHigh do
  begin
    iMiddle := iLow + (iHigh - iLow) div 2;
    if FIndexedAction[iMiddle] < aAction then
      iLow := iMiddle + 1
    else if FIndexedAction[iMiddle] > aAction then
      iHigh := iMiddle - 1
    else
      Exit(FActionIndex[iMiddle]);
  end;
  Result := -1;
end;

procedure TBindingCatalog.RegisterGroup( aGroup : TConfigurationGroup; const aGroupID : AnsiString );
var iInfo : Integer;
begin
  if aGroup = nil then
    raise Exception.Create('TBindingCatalog - configuration group is nil!');

  for iInfo := 0 to High(FInfo) do
    if (FInfo[iInfo].Group = aGroupID) and (FInfo[iInfo].ID <> '') then
    begin
      if FRegistrations[iInfo] <> 0 then
        raise Exception.Create('TBindingCatalog - ID '+FInfo[iInfo].ID+' is already registered!');
      FEntry[iInfo] := aGroup.AddInteger(FInfo[iInfo].ID, FInfo[iInfo].Default);
      FEntry[iInfo].SetName(FInfo[iInfo].Name);
      FEntry[iInfo].SetDescription(FInfo[iInfo].Description);
      Inc(FRegistrations[iInfo]);
    end;
end;

procedure TBindingCatalog.ValidateRegistration;
var iInfo : Integer;
begin
  for iInfo := 0 to High(FInfo) do
    if (FInfo[iInfo].ID <> '') and
       ((FRegistrations[iInfo] <> 1) or (FEntry[iInfo] = nil)) then
      raise Exception.Create('TBindingCatalog - ID '+FInfo[iInfo].ID+' is not registered!');
end;

procedure TBindingCatalog.ResetValues;
var iInfo : Integer;
begin
  for iInfo := 0 to High(FEntry) do
    if FEntry[iInfo] <> nil then FEntry[iInfo].Reset;
end;

function TBindingCatalog.ActionForID( const aID : AnsiString ) : TBindingAction;
var iInfo : TBindingInfo;
begin
  if aID <> '' then
    for iInfo in FInfo do
      if iInfo.ID = aID then Exit( iInfo.Action );
  Result := BINDING_NONE;
end;

function TBindingCatalog.ActionForValue( aValue : Integer ) : TBindingAction;
var iInfo : Integer;
begin
  for iInfo := 0 to High( FInfo ) do
    if ( FEntry[iInfo] <> nil ) and ( FEntry[iInfo].Value = aValue ) then
      Exit( FInfo[iInfo].Action );
  Result := BINDING_NONE;
end;

procedure TBindingCatalog.SetKey( aAction : TBindingAction; aKey : TIOKeyCode );
var iInfo : Integer;
begin
  SetConfigurationValue( aAction, aKey );
  if aKey = 0 then Exit;
  for iInfo := 0 to High( FInfo ) do
    if ( FInfo[iInfo].Action <> aAction ) and ( FEntry[iInfo] <> nil ) and
       ( FEntry[iInfo].Value = aKey ) then
      FEntry[iInfo].Value := 0;
end;

function TBindingCatalog.ConfigurationValue( aAction : TBindingAction ) : Integer;
var iInfo : Integer;
begin
  iInfo := FindDefinition(aAction);
  if iInfo < 0 then
    raise Exception.Create('TBindingCatalog - action '+IntToStr(aAction)+' is undefined!');
  if FEntry[iInfo] = nil then
    raise Exception.Create('TBindingCatalog - action '+IntToStr(aAction)+' is not registered!');
  Result := FEntry[iInfo].Value;
end;

procedure TBindingCatalog.SetConfigurationValue(
  aAction : TBindingAction;
  aValue  : Integer
);
var iInfo : Integer;
begin
  iInfo := FindDefinition(aAction);
  if iInfo < 0 then
    raise Exception.Create('TBindingCatalog - action '+IntToStr(aAction)+' is undefined!');
  if FEntry[iInfo] = nil then
    raise Exception.Create('TBindingCatalog - action '+IntToStr(aAction)+' is not registered!');
  FEntry[iInfo].Value := aValue;
end;

function TBindingCatalog.Info( aAction : TBindingAction ) : TBindingInfo;
var iInfo : Integer;
begin
  iInfo := FindDefinition(aAction);
  if iInfo < 0 then
    raise Exception.Create('TBindingCatalog - action '+IntToStr(aAction)+' is undefined!');
  Result := FInfo[iInfo];
end;

// TBindingContext

constructor TBindingContext.Create;
begin
  Clear;
end;

procedure TBindingContext.Clear;
var iKey : TIOKeyCode;
    iPad : TIOPadButton;
begin
  for iKey := 0 to IOKeyCodeMax do
    FKeys[iKey] := BINDING_NONE;
  for iPad := Low(TIOPadButton) to High(TIOPadButton) do
    FPads[iPad] := BINDING_NONE;
end;

procedure TBindingContext.BindKey( aKey : TIOKeyCode; aAction : TBindingAction );
begin
  if (aKey = 0) or (aKey > IOKeyCodeMax) then Exit;
  if FKeys[aKey] = aAction then Exit;
  FKeys[aKey] := aAction;
end;

procedure TBindingContext.BindPad( aButton : TIOPadButton; aAction : TBindingAction );
begin
  if (Ord(aButton) < Ord(VPAD_BUTTON_A)) or
     (Ord(aButton) > Ord(High(TIOPadButton))) then Exit;
  if FPads[aButton] = aAction then Exit;
  FPads[aButton] := aAction;
end;

procedure TBindingContext.LoadKeys( aCatalog : TBindingCatalog );
var iInfo, iValue : Integer;
begin
  for iInfo := 0 to High(aCatalog.FInfo) do
    if aCatalog.FEntry[iInfo] <> nil then
    begin
      iValue := aCatalog.ConfigurationValue(aCatalog.FInfo[iInfo].Action);
      if iValue = 0 then Continue;
      if (iValue > 0) and (iValue <= IOKeyCodeMax) then
        BindKey(TIOKeyCode(iValue), aCatalog.FInfo[iInfo].Action)
      else
        Log(LOGWARN, 'Invalid keyboard binding "'+aCatalog.FInfo[iInfo].ID+'" ignored.');
    end;
end;

procedure TBindingContext.LoadPad( aCatalog : TBindingCatalog );
var iInfo, iValue : Integer;
begin
  for iInfo := 0 to High(aCatalog.FInfo) do
    if aCatalog.FEntry[iInfo] <> nil then
    begin
      iValue := aCatalog.ConfigurationValue(aCatalog.FInfo[iInfo].Action);
      if iValue = Ord(VPAD_BUTTON_INVALID) then Continue;
      if (iValue >= Ord(VPAD_BUTTON_A)) and
         (iValue <= Ord(High(TIOPadButton))) then
        BindPad(TIOPadButton(iValue), aCatalog.FInfo[iInfo].Action)
      else
        Log(LOGWARN, 'Invalid controller binding "'+aCatalog.FInfo[iInfo].ID+'" ignored.');
    end;
end;

function TBindingContext.ResolveKey( aKey : TIOKeyCode ) : TBindingAction;
begin
  if aKey > IOKeyCodeMax then Exit(BINDING_NONE);
  Result := FKeys[aKey];
end;

function TBindingContext.ResolvePad( aButton : TIOPadButton ) : TBindingAction;
begin
  if (Ord(aButton) < Ord(VPAD_BUTTON_A)) or
     (Ord(aButton) > Ord(High(TIOPadButton))) then Exit(BINDING_NONE);
  Result := FPads[aButton];
end;

function TBindingContext.GetKey( aAction : TBindingAction ) : TIOKeyCode;
var iKey : TIOKeyCode;
begin
  Result := 0;
  if aAction = BINDING_NONE then Exit;

  for iKey := 1 to IOKeyCodeMax do
    if FKeys[iKey] = aAction then Exit(iKey);
end;

function TBindingContext.GetPadButton( aAction : TBindingAction ) : TIOPadButton;
var iPad : TIOPadButton;
begin
  Result := VPAD_BUTTON_INVALID;
  if aAction = BINDING_NONE then Exit;

  for iPad := VPAD_BUTTON_A to High(TIOPadButton) do
    if FPads[iPad] = aAction then Exit(iPad);
end;

// TBindings

constructor TBindings.Create;
begin
  FContexts := TBindingContextArray.Create( True );
end;

function TBindings.CreateContext : TBindingContext;
begin
  Result := TBindingContext.Create;
  FContexts.Push(Result);
end;

destructor TBindings.Destroy;
begin
  FreeAndNil(FContexts);
  inherited Destroy;
end;

end.
