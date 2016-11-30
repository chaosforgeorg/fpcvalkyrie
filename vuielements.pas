{$INCLUDE valkyrie.inc}
unit vuielements;
interface
uses SysUtils, vlualibrary, vnode, viotypes, vioevent, vuitypes, vuielement, vgenerics;

type TUIElementArray = specialize TGObjectArray< TUIElement >;

type IUIScrollable = interface ['vuielements.iuiscrollable']
  procedure SetScroll( const aScroll : DWord );
  function GetScroll : DWord;
  function GetCount : DWord;
  function GetVisibleCount : DWord;
  property Count          : DWord     read GetCount;
  property VisibleCount   : DWord     read GetVisibleCount;
  property Scroll         : DWord     read GetScroll write SetScroll;
end;

// TODO : Common base for Scrollable?
// TODO : Common base for others? events?
// TODO : test chunkify with scrollable big textfile

type TUICustomScrollable      = class( TUIElement, IUIScrollable )
  constructor Create( aParent : TUIElement; const aArea : TUIRect );
  procedure SetScroll( const aScroll : DWord ); virtual;
  function GetScroll : DWord; virtual;
  function GetCount : DWord; virtual;
  function GetVisibleCount : DWord; virtual;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  function OnMouseDown( const event : TIOMouseEvent ) : Boolean; override;
protected
  FScroll      : DWord;
  FCount       : DWord;
  FVisibleCount: DWord;
published
  property Count          : DWord     read GetCount;
  property VisibleCount   : DWord     read GetVisibleCount;
  property Scroll         : DWord     read GetScroll write SetScroll;
end;

type TUICustomLabel      = class( TUIElement )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; const aText : TUIString );
  procedure SetText( const aText : TUIString ); virtual;
protected
  FText : TUIString;
published
  property Text : TUIString read FText write SetText;
end;

type TUICustomText       = class( TUICustomScrollable )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; const aText : TUIString; aScrollable : Boolean = False );
  procedure SetText( const aText : TUIString ); virtual;
protected
  FText        : TUIString;
  FScrollable  : Boolean;
published
  property Text           : TUIString read FText write SetText;
  property Scrollable     : Boolean   read FScrollable;
end;

type TUICustomWindow     = class( TUIElement )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; const aTitle : TUIString );
  procedure SetTitle( const aTitle : TUIString ); virtual;
protected
  FTitle : TUIString;
published
  property Title : TUIString read FTitle write SetTitle;
end;

type TUICustomSeparator  = class( TUIElement )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aOrient : TUIOrientation; aPosition : Integer );
  procedure SetPosition( aPosition : DWord ); virtual;
protected
  FOrient      : TUIOrientation;
  FSepPos      : Integer;
  FTopLeft     : TUIElement;
  FBottomRight : TUIElement;
public
  property Left   : TUIElement read FTopLeft;
  property Top    : TUIElement read FTopLeft;
  property Right  : TUIElement read FBottomRight;
  property Bottom : TUIElement read FBottomRight;
end;


type TUICustomInputLine  = class( TUIElement )
  constructor Create( aParent : TUIElement; const aArea : TUIRect );
  procedure SetInput( const aInput : TUIString ); virtual;
  procedure SetCharSet( const aCharSet : TUICharSet ); virtual;
  procedure SetCursorPosition( aPosition : Word ); virtual;
  procedure SetMaxLength( aMaxLength : Word ); virtual;
  function OnChange : Boolean; virtual;
  function OnConfirm : Boolean; virtual;
  function OnCancel : Boolean; virtual;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
protected
  procedure RecalcDimensions; override;
protected
  FInput          : TUIString;
  FCharSet        : TUICharSet;
  FCursorPosition : Word;
  FMaxLength      : Word;

  FOnCancel       : TUINotifyEvent;
  FOnConfirm      : TUINotifyEvent;
  FOnChange       : TUINotifyEvent;
public
  property OnConfirmEvent : TUINotifyEvent write FOnConfirm;
  property OnCancelEvent  : TUINotifyEvent write FOnCancel;
  property OnSelectEvent  : TUINotifyEvent write FOnChange;

  property CharSet        : TUICharSet read FCharSet        write SetCharSet;
published
  property Input          : TUIString  read FInput          write SetInput;
  property CursorPosition : Word       read FCursorPosition write SetCursorPosition;
  property MaxLength      : Word       read FMaxLength      write SetMaxLength;
end;

type TUIMenuItem = class( TVObject )
  constructor Create( const aText : AnsiString; aActive : Boolean = True; aData : Pointer = nil; aColor : TUIColor = 0 );
private
  FText   : TUIString;
  FData   : Pointer;
  FActive : Boolean;
  FColor  : TUIColor;
public
  property Text   : TUIString read FText   write FText;
  property Data   : Pointer   read FData   write FData;
  property Active : Boolean   read FActive write FActive;
  property Color  : TUIColor  read FColor  write FColor;
end;

type TUIMenuItemArray = specialize TGObjectArray<TUIMenuItem>;
     TUIOnSelectEvent = function( aSender : TUIElement; aIndex : DWord; aItem : TUIMenuItem ) : Boolean of object;

type TUICustomMenu       = class( TUICustomScrollable )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aVisibleCount : DWord );
  procedure Add( aItem : TUIMenuItem ); virtual; overload; reintroduce;
  procedure Add( const aText : TUIString; aActive : Boolean = True; aData : Pointer = nil; aColor : TUIColor = 0 ); reintroduce;
  procedure Clear; virtual;
  function IsActive( aSelection : DWord ) : Boolean;
  function IsValid( aSelection : DWord ) : Boolean;
  function FindNextActive( aFrom : DWord; aNext : Boolean = True ) : DWord;
  function FindActive( aFrom : DWord; aNext : Boolean = True ) : DWord;
  function GetItem( aIndex : DWord ) : TUIMenuItem;
  function GetSelectedItem : TUIMenuItem;
  procedure SetSelected( aSelected : DWord; aNext : Boolean = True ); virtual;
  procedure SetSelectInactive( aSelectInactive : Boolean ); virtual;
  procedure RecalcScroll; virtual;
  function MousePosToEntry( const aPosition : TUIPoint ) : DWord; virtual;
  function OnSelect : Boolean; virtual;
  function OnConfirm : Boolean; virtual;
  function OnCancel : Boolean; virtual;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  function OnMouseDown( const event : TIOMouseEvent ) : Boolean; override;
  function OnMouseMove( const event : TIOMouseMoveEvent ) : Boolean; override;
  destructor Destroy; override;
protected
  FItems          : TUIMenuItemArray;
  FSelected       : DWord;
  FSelectInactive : Boolean;
  FConfirmInactive: Boolean;
  FForceChoice    : Boolean;

  FOnCancel       : TUINotifyEvent;
  FOnConfirm      : TUINotifyEvent;
  FOnSelect       : TUIOnSelectEvent;
public
  property SelectedItem : TUIMenuItem  read GetSelectedItem;
  property Items[ aIndex : DWord ] : TUIMenuItem read GetItem;

  property OnConfirmEvent : TUINotifyEvent     write FOnConfirm;
  property OnCancelEvent  : TUINotifyEvent     write FOnCancel;
  property OnSelectEvent  : TUIOnSelectEvent   write FOnSelect;
published
  property Selected        : DWord    read FSelected        write SetSelected;
  property SelectInactive  : Boolean  read FSelectInactive  write SetSelectInactive;
  property ConfirmInactive : Boolean  read FConfirmInactive write FConfirmInactive;
  property ForceChoice     : Boolean  read FForceChoice     write FForceChoice;
end;


type

{ TUICustomStringList }

 TUICustomStringList = class( TUICustomScrollable )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aVisibleCount : DWord; aContent : TUIStringArray = nil; aOwner : Boolean = True );
  procedure Add( aString : TUIString ); virtual; reintroduce;
  procedure SetContent( aContent : TUIStringArray; aOwner : Boolean = False ); virtual;
  function SetProperty( L : PLua_State; const aPropertyName : AnsiString; aValueIndex : Integer ) : Boolean; override;
  destructor Destroy; override;
private
  procedure SetContent(AValue: TUIStringArray);
protected
  FContent      : TUIStringArray;
  FOwner        : Boolean;
public
  property Owner   : Boolean        read FOwner   write FOwner;
  property Content : TUIStringArray read FContent write SetContent;
end;

type TUICustomStringBuffer = class( TUICustomScrollable )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aVisibleCount : DWord; aContent : TUIStringBuffer = nil; aOwner : Boolean = True );
  procedure Add( const aString : TUIString ); virtual; reintroduce;
  procedure SetContent( aContent : TUIStringBuffer; aOwner : Boolean = False ); virtual;
  destructor Destroy; override;
protected
  FContent      : TUIStringBuffer;
  FOwner        : Boolean;
public
  property Owner   : Boolean         read FOwner   write FOwner;
  property Content : TUIStringBuffer read FContent write SetContent;
end;

type TUICustomChunkBuffer = class( TUICustomScrollable )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aVisibleCount : DWord; aContent : TUIChunkBuffer; aOwner : Boolean = False );
  procedure SetContent( aContent : TUIChunkBuffer; aOwner : Boolean = False ); virtual;
  destructor Destroy; override;
protected
  FContent      : TUIChunkBuffer;
  FOwner        : Boolean;
public
  property Owner   : Boolean        read FOwner   write FOwner;
  property Content : TUIChunkBuffer read FContent write SetContent;
end;

type TUIMessageHighlight = record
    Wildcard : Ansistring;
    Color    : TUIColor;
  end;

TUIHighlightsArray = specialize TGArray<TUIMessageHighlight>;


type TUICustomMessages = class( TUICustomScrollable )
  // Creates a new message system. Pos and Size are used to place the Messages
  // on the textmode screen. BufferSize determines how many messages will be
  // stored by the system.
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aVisibleCount : DWord; aOnMore : TUINotifyEvent; aBufferSize : Word = 1000 ); reintroduce;
  // Adds a new message to the system. By default the message will be active.
  procedure Add( const aMessage : Ansistring ); virtual; reintroduce;
  // Handle more case
  procedure More; virtual;
  // Updates the messages -- all messages are marked as "read" - inactive.
  // Should be run immidately AFTER Updating the screen.
  procedure Update; virtual;
  // Destroy all messages and cleanup
  procedure Clear; virtual;
  // Push empty lines in the visible amount
  procedure Reset;
  // Destroys last added message line.
  procedure Pop;
  // Frees memory of the message system.
  destructor Destroy; override;
  // Returns amount of stored message lines.
  function Size : DWord;
  // add pending
  procedure AddPending;
  // Add highlight, Variants used for callback support
  procedure AddHighlightCallback( aKey, aValue : Variant );
protected
  function Chunkify( const aString : AnsiString; aStart : Integer; aColor : TUIColor = ColorNone ) : TUIChunkBuffer; virtual; abstract;
protected
  FHighlights  : TUIHighlightsArray;
  //
  FContent     : TUIChunkBuffer;
  // Number of highlights.
  FPending     : TUIChunkBuffer;
   // Number of highlights.
  FHighCount   : Word;
  // Holds the AMOUNT of active message lines
  FActive      : Word;
  //
  FOnMore      : TUINotifyEvent;
public
  property OnMoreEvent : TUINotifyEvent write FOnMore;
  property Active : Word read FActive write FActive;
  property Content : TUIChunkBuffer read FContent;
end;

type TUICustomPager = class( TUIElement )
  constructor Create( aParent : TUIElement; const aArea : TUIRect );
  procedure Add( aNode : TNode ); override;
  function GetPage( aIndex : DWord ) : TUIElement;
  function GetSelectedPage : TUIElement;
  procedure SetPage( aIndex : DWord ); virtual;
  function GetPageCount : DWord;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
protected
  FPage  : DWord;
  FPages : TUIElementArray;
public
  property Pages[ aIndex : DWord ] : TUIElement read GetPage;
  property PageCount    : DWord      read GetPageCount;
  property SelectedPage : TUIElement read GetSelectedPage;
  property Selected     : DWord      read FPage write SetPage;
end;


implementation

uses strutils, vluaui, vutil, vmath;

{ TUICustomPager }

constructor TUICustomPager.Create ( aParent : TUIElement; const aArea : TUIRect ) ;
begin
  inherited Create( aParent, aArea );
  FPage  := 0;
  FPages := TUIElementArray.Create( False );
end;

procedure TUICustomPager.Add ( aNode : TNode ) ;
begin
  if not ( aNode is TUIElement ) then Exit;
  inherited Add( aNode );
  FPages.Push( aNode as TUIElement );
  if FPages.Size > 1 then
    (aNode as TUIElement).Enabled := False;
end;

function TUICustomPager.GetPage ( aIndex : DWord ) : TUIElement;
begin
  Exit( FPages[ aIndex ] );
end;

function TUICustomPager.GetSelectedPage : TUIElement;
begin
  Exit( FPages[ FPage ] );
end;

procedure TUICustomPager.SetPage ( aIndex : DWord ) ;
begin
  if FPages.Size = 0 then Exit;
  FPages[ FPage ].Enabled := False;
  FPage := aIndex;
  FPages[ FPage ].Enabled := True;
end;

function TUICustomPager.GetPageCount : DWord;
begin
  Exit( FPages.Size );
end;

function TUICustomPager.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if event.ModState <> [] then Exit( False );
  case event.Code of
    VKEY_LEFT   : SetPage( TrueModulo( FPage - 1, FPages.Size ) );
    VKEY_RIGHT  : SetPage( TrueModulo( FPage + 1, FPages.Size ) );
  else Exit( False );
  end;
  Result := False;
end;

{ TUICustomScrollable }

constructor TUICustomScrollable.Create ( aParent : TUIElement; const aArea : TUIRect ) ;
begin
  inherited Create ( aParent, aArea ) ;
  FScroll      := 0;
  FCount       := 0;
  FVisibleCount:= 0;
end;

procedure TUICustomScrollable.SetScroll ( const aScroll : DWord ) ;
begin
  FScroll := Clamp( aScroll, 0, Max( FCount-FVisibleCount, 0 ) );
  FDirty := True;
end;

function TUICustomScrollable.GetScroll : DWord;
begin
  Exit( FScroll );
end;

function TUICustomScrollable.GetCount : DWord;
begin
  Exit( FCount );
end;

function TUICustomScrollable.GetVisibleCount : DWord;
begin
  Exit( FVisibleCount );
end;

function TUICustomScrollable.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if (event.ModState <> []) or (FCount < FVisibleCount) then Exit( inherited OnKeyDown ( event ) );
  case event.Code of
    VKEY_UP     : SetScroll( Max( FScroll - 1, 0 ) );
    VKEY_DOWN   : SetScroll( FScroll + 1 );
    VKEY_PGUP   : SetScroll( Max( FScroll - FVisibleCount + 2, 0 ) );
    VKEY_PGDOWN : SetScroll( FScroll + FVisibleCount - 2 );
    VKEY_HOME   : SetScroll( 0 );
    VKEY_END    : SetScroll( FCount - FVisibleCount );
  else
    Exit( inherited OnKeyDown ( event ) );
  end;
  Exit( True );
end;

function TUICustomScrollable.OnMouseDown ( const event : TIOMouseEvent ) : Boolean;
begin
  case event.Button of
    VMB_WHEEL_UP   : SetScroll( Max( 0, FScroll - 1 ) );
    VMB_WHEEL_DOWN : SetScroll( FScroll + 1 );
    else
      Exit( inherited OnMouseDown ( event ) );
  end;
  Exit( True );
end;

{ TUICustomLabel }

constructor TUICustomLabel.Create ( aParent : TUIElement; const aArea : TUIRect; const aText : TUIString ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass := 'label';
  FText := '';
  SetText( aText );
end;

procedure TUICustomLabel.SetText ( const aText : TUIString ) ;
begin
  FText := aText;
  FDirty := True;
end;

{ TUICustomText }

constructor TUICustomText.Create ( aParent : TUIElement; const aArea : TUIRect; const aText : TUIString; aScrollable : Boolean ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass := 'text';
  FText := '';
  FScrollable := aScrollable;
  if FScrollable then Include( FEventFilter, VEVENT_KEYDOWN );
  FScroll      := 0;
  FCount       := 1;
  FVisibleCount:= 1;
  SetText( aText );
  SetScroll( 0 );
end;

procedure TUICustomText.SetText ( const aText : TUIString ) ;
begin
  FText := aText;
  FDirty := True;
end;

{ TUICustomWindow }

constructor TUICustomWindow.Create ( aParent : TUIElement; const aArea : TUIRect; const aTitle : TUIString ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass := 'window';
  FTitle := '';
  SetTitle( aTitle );
end;

procedure TUICustomWindow.SetTitle ( const aTitle : TUIString ) ;
begin
  FTitle := aTitle;
  FDirty := True;
end;

{ TUICustomSeparator }

constructor TUICustomSeparator.Create ( aParent : TUIElement; const aArea : TUIRect; aOrient : TUIOrientation;
  aPosition : Integer ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass := 'separator';
  FOrient := aOrient;
  FTopLeft     := nil;
  FBottomRight := nil;
  SetPosition( aPosition );
end;

procedure TUICustomSeparator.SetPosition ( aPosition : DWord ) ;
begin
  FDirty := True;
  FreeAndNil( FTopLeft );
  FreeAndNil( FBottomRight );
  FSepPos := aPosition;
  if FOrient = VORIENT_HORIZONTAL then
  begin
    FTopLeft     := TUIElement.Create( Self, Rectangle( PointZero, Dim.x, FSepPos-1 ) );
    FBottomRight := TUIElement.Create( Self, Rectangle( 0, FSepPos, Dim.x, Dim.y - FSepPos ) );
  end
  else
  begin
    FTopLeft     := TUIElement.Create( Self, Rectangle( PointZero, FSepPos-1, Dim.y ) );
    FBottomRight := TUIElement.Create( Self, Rectangle( FSepPos, 0, Dim.x - FSepPos, Dim.y ) );
  end;
end;

{ TUICustomInputLine }

constructor TUICustomInputLine.Create ( aParent : TUIElement; const aArea : TUIRect ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass := 'input';
  Include( FEventFilter, VEVENT_KEYDOWN );
  FInput          := '';
  FCursorPosition := 1;
  FMaxLength      := 255;

  FOnConfirm      := nil;
  FOnCancel       := nil;
  FOnChange       := nil;

  SetInput( FInput );
  SetCursorPosition( 1 );
  SetCharSet( UIPrintableChars );
  SetMaxLength( 255 );
end;

procedure TUICustomInputLine.SetInput ( const aInput : TUIString ) ;
begin
  if Length( aInput ) > FMaxLength then
    FInput := Copy( aInput, 1, FMaxLength )
  else
    FInput := aInput;
  SetCursorPosition( FCursorPosition );
  OnChange;
end;

procedure TUICustomInputLine.SetCharSet ( const aCharSet : TUICharSet ) ;
begin
  FCharSet := aCharSet;
end;

procedure TUICustomInputLine.SetCursorPosition ( aPosition : Word ) ;
begin
  FDirty := True;
  FCursorPosition := Clamp( aPosition, 1, Length(FInput)+1 );
end;

procedure TUICustomInputLine.SetMaxLength ( aMaxLength : Word ) ;
begin
  FMaxLength := Max( aMaxLength, 1 );
end;

function TUICustomInputLine.OnChange : Boolean;
begin
  FDirty := True;
  if HasHook( UIHOOK_ONCHANGE ) then if RunHook( UIHOOK_ONCHANGE, [Input] ) then Exit( True );
  Exit( Assigned( FOnChange ) and FOnChange( Self ) );
end;

function TUICustomInputLine.OnConfirm : Boolean;
begin
  if HasHook( UIHOOK_ONCONFIRM ) then if RunHook( UIHOOK_ONCONFIRM, [Input] ) then Exit( True );
  Exit( Assigned( FOnConfirm ) and FOnConfirm( Self ) );
end;

function TUICustomInputLine.OnCancel : Boolean;
begin
  if HasHook( UIHOOK_ONCANCEL ) then if RunHook( UIHOOK_ONCANCEL, [] ) then Exit( True );
  Exit( Assigned( FOnCancel ) and FOnCancel( Self ) );
end;

function TUICustomInputLine.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
var iInput    : AnsiString;
begin
  if event.ASCII in FCharSet then
  begin
    if Length( FInput ) < FMaxLength then
    begin
      iInput := FInput;
      Insert( event.ASCII, iInput, FCursorPosition );
      SetInput( iInput );
      SetCursorPosition( FCursorPosition+1 );
    end;
    Exit( True );
  end;

  if event.ModState <> [] then Exit( inherited OnKeyDown ( event ) );

  case event.Code of
    VKEY_BACK   : if FCursorPosition > 1 then
                  begin
                    iInput := FInput;
                    Delete( iInput, FCursorPosition-1, 1 );
                    SetCursorPosition( FCursorPosition-1 );
                    SetInput( iInput );
                  end;
    VKEY_DELETE : if FCursorPosition <= Length( FInput ) then
                  begin
                    iInput := FInput;
                    Delete( iInput, FCursorPosition, 1 );
                    SetInput( iInput );
                  end;
    VKEY_END    : SetCursorPosition( Length( FInput ) + 1 );
    VKEY_HOME   : SetCursorPosition( 1 );
    VKEY_LEFT   : SetCursorPosition( FCursorPosition-1 );
    VKEY_RIGHT  : SetCursorPosition( FCursorPosition+1 );
    VKEY_ENTER  : Exit( OnConfirm );
    VKEY_ESCAPE : Exit( OnCancel );
  else
    Exit( inherited OnKeyDown ( event ) );
  end;
  Exit( True );
end;

procedure TUICustomInputLine.RecalcDimensions;
begin
  inherited RecalcDimensions;
  SetMaxLength( FMaxLength );
end;

{ TUIMenuItem }

constructor TUIMenuItem.Create ( const aText : AnsiString; aActive : Boolean = True; aData : Pointer = nil; aColor : TUIColor = 0 );
begin
  inherited Create;
  FText   := aText;
  FData   := aData;
  FActive := aActive;
  FColor  := aColor;
end;

{ TUICustomMenu }

constructor TUICustomMenu.Create ( aParent : TUIElement; const aArea : TUIRect; aVisibleCount : DWord );
begin
  inherited Create( aParent, aArea );
  FStyleClass := 'menu';
  Include( FEventFilter, VEVENT_KEYDOWN );
  Include( FEventFilter, VEVENT_MOUSEMOVE );
  Include( FEventFilter, VEVENT_MOUSEDOWN );
  FItems          := TUIMenuItemArray.Create( True );
  FSelected       := 0;
  FSelectInactive := True;
  FVisibleCount   := aVisibleCount;
  FForceChoice    := False;
  FConfirmInactive:= False;

  FOnConfirm      := nil;
  FOnCancel       := nil;
  FOnSelect       := nil;

  SetSelected(0);
  SetScroll(0);
  SetSelectInactive(True);

  // null item
  FItems.Push( nil );
end;

procedure TUICustomMenu.Add( aItem : TUIMenuItem );
begin
  FDirty := True;
  Inc( FCount );
  FItems.Push( aItem );
  if FSelected = 0 then SetSelected(1);
end;

procedure TUICustomMenu.Add ( const aText : TUIString; aActive : Boolean; aData : Pointer; aColor : TUIColor = 0 ) ;
begin
  Add( TUIMenuItem.Create( aText, aActive, aData, aColor ) );
end;

procedure TUICustomMenu.Clear;
begin
  FItems.Clear;
  FItems.Push( nil );
  FSelected       := 0;
  FCount          := 0;
  FScroll         := 0;
  SetSelected(0);
end;

function TUICustomMenu.IsActive ( aSelection : DWord ) : Boolean;
begin
  Result := (aSelection > 0) and (aSelection <= FCount) and FItems[ aSelection ].Active;
end;

function TUICustomMenu.IsValid ( aSelection : DWord ) : Boolean;
begin
  Result := (aSelection > 0) and (aSelection <= FCount);
end;

function TUICustomMenu.FindNextActive ( aFrom : DWord; aNext : Boolean ) : DWord;
var iStep : ShortInt;
begin
  if IsActive( aFrom ) then Exit( aFrom );
  if aNext
    then iStep := +1
    else iStep := -1;
  repeat
    aFrom += iStep;
    if not IsValid( aFrom ) then Exit( 0 );
  until IsActive( aFrom );
  Exit( aFrom );
end;

function TUICustomMenu.FindActive ( aFrom : DWord; aNext : Boolean ) : DWord;
begin
  if IsActive( aFrom ) then Exit( aFrom );
  aFrom := FindNextActive( aFrom, aNext );
  if aFrom <> 0 then Exit( aFrom );
  if aNext
    then Exit( FindNextActive( 0, aNext ) )
    else Exit( FindNextActive( FCount+1, aNext ) );
end;

function TUICustomMenu.GetItem ( aIndex : DWord ) : TUIMenuItem;
begin
  if IsValid( aIndex )
    then Exit( FItems[ aIndex ] )
    else Exit( nil );
end;

function TUICustomMenu.GetSelectedItem : TUIMenuItem;
begin
  if IsValid( FSelected )
    then Exit( FItems[ FSelected ] )
    else Exit( nil );
end;

procedure TUICustomMenu.SetSelected ( aSelected : DWord; aNext : Boolean = True  ) ;
begin
  FDirty := True;
  FSelected := Min( aSelected, FCount );
  if (not FSelectInactive) and (FSelected <> 0) and (FCount <> 0) and (not FItems[ FSelected ].Active) then
  begin
    SetSelected( FindActive( FSelected, aNext ) );
    Exit;
  end;

  OnSelect;
  RecalcScroll;
end;

procedure TUICustomMenu.SetSelectInactive ( aSelectInactive : Boolean ) ;
begin
  FSelectInactive := aSelectInactive;
  SetSelected( FSelected );
end;

procedure TUICustomMenu.RecalcScroll;
var iScroll : LongInt;
begin
  if FSelected = 0 then Exit;
  if (FCount = 0) or (FCount <= FVisibleCount) then
  begin
    SetScroll( 0 );
    Exit;
  end;

  iScroll := FScroll;
  if FSelected-FScroll >= FVisibleCount then
    iScroll := FSelected - FVisibleCount + 1
  else if FSelected-FScroll <= 1 then
    iScroll := FSelected-2;

  SetScroll( Clamp( iScroll, 0, FCount-FVisibleCount ) );
end;

function TUICustomMenu.MousePosToEntry ( const aPosition : TUIPoint ) : DWord;
begin
  Exit( 0 );
end;

function TUICustomMenu.OnSelect : Boolean;
begin
  if HasHook( UIHOOK_ONSELECT ) then if RunHook( UIHOOK_ONSELECT, [FSelected,DWord(SelectedItem.Data)] ) then Exit( True );
  Exit( Assigned( FOnSelect ) and FOnSelect( Self, FSelected, Items[ FSelected ] ) );
end;

function TUICustomMenu.OnConfirm : Boolean;
begin
  if HasHook( UIHOOK_ONCONFIRM ) then if RunHook( UIHOOK_ONCONFIRM, [FSelected,DWord(SelectedItem.Data)] ) then Exit( True );
  Exit( Assigned( FOnConfirm ) and FOnConfirm( Self ) );
end;

function TUICustomMenu.OnCancel : Boolean;
begin
  if HasHook( UIHOOK_ONCANCEL ) then if RunHook( UIHOOK_ONCANCEL, [] ) then Exit( True );
  Exit( Assigned( FOnCancel ) and FOnCancel( Self ) );
end;


function TUICustomMenu.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if event.ModState <> [] then Exit( False );

  case event.Code of
    VKEY_UP     : if FSelected > 1      then SetSelected( FSelected - 1, False ) else SetSelected( FCount, False );
    VKEY_DOWN   : if FSelected < FCount then SetSelected( FSelected + 1, True ) else SetSelected( 1, True );
    VKEY_PGUP   : if FSelected > 1      then SetSelected( Max( FSelected - FVisibleCount + 2, 1 ), True );
    VKEY_PGDOWN : if FSelected < FCount then SetSelected( Max( FSelected + FVisibleCount - 2, FCount ), False );
    VKEY_HOME   : SetSelected( 1, True );
    VKEY_END    : SetSelected( FCount, False );
    VKEY_ENTER  : if FConfirmInactive or IsActive( FSelected ) then Exit( OnConfirm );
    VKEY_ESCAPE : if not FForceChoice   then Exit( OnCancel );
  else
    Exit( False );
  end;
  Exit( True );
end;

function TUICustomMenu.OnMouseDown ( const event : TIOMouseEvent ) : Boolean;
var iSelection : DWord;
begin
  iSelection := MousePosToEntry( event.Pos );
  if iSelection <> 0 then
  begin
    if event.Button in [ VMB_BUTTON_LEFT, VMB_BUTTON_RIGHT ] then
    begin
      SetSelected( iSelection );
      if (iSelection = FSelected) and ( event.Button = VMB_BUTTON_LEFT ) and ( VEVENT_MOUSEMOVE in FEventFilter ) then
        if FConfirmInactive or IsActive( FSelected ) then  OnConfirm;
      Exit( True );
    end
    else
    case event.Button of
      VMB_WHEEL_UP   : if FSelected > 1      then SetSelected( FSelected - 1, True );
      VMB_WHEEL_DOWN : if FSelected < FCount then SetSelected( FSelected + 1, False );
      else
        Exit( False );

      end;
    Exit( True );
  end;
  Result := False;
end;

function TUICustomMenu.OnMouseMove ( const event : TIOMouseMoveEvent ) : Boolean;
var iSelection : DWord;
begin
  iSelection := MousePosToEntry( event.Pos );
  if ( IsValid( iSelection ) and ( FSelectInactive or IsActive( iSelection ) ) ) then
  begin
    SetSelected( iSelection );
    Exit( True );
  end;
  Result := inherited OnMouseMove ( event ) ;
end;

destructor TUICustomMenu.Destroy;
begin
  FreeAndNil( FItems );
  inherited Destroy;
end;

{ TUICustomStringList }

constructor TUICustomStringList.Create ( aParent : TUIElement; const aArea : TUIRect; aVisibleCount : DWord;
  aContent : TUIStringArray; aOwner : Boolean ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass := 'stringlist';
  FContent      := nil;
  FOwner        := True;
  FScroll       := 0;
  FVisibleCount := aVisibleCount;

  SetContent( aContent, aOwner );
  SetScroll( 0 );
end;

procedure TUICustomStringList.Add ( aString : TUIString ) ;
begin
  FDirty := True;
  FContent.Push( aString );
  FCount := FContent.Size;
end;

procedure TUICustomStringList.SetContent ( aContent : TUIStringArray; aOwner : Boolean ) ;
begin
  FDirty := True;
  if (FContent <> nil) and FOwner then FreeAndNil( FContent );
  FOwner := aOwner;
  FContent := aContent;
  if FContent <> nil then
    FCount := FContent.Size
  else
    FCount := 0;
  SetScroll(0);
end;

function TUICustomStringList.SetProperty(L: PLua_State;
  const aPropertyName: AnsiString; aValueIndex: Integer): Boolean;
begin
  if aPropertyName = 'content' then
  begin
    if FOwner then FreeAndNil( FContent );
    if lua_type( L, aValueIndex ) = LUA_TSTRING
      then SetContent( TextFileToUIStringArray( lua_tostring( L, aValueIndex ) ), True )
      else SetContent( vlua_tostringlist( L, aValueIndex ), True );
    Exit( True );
  end;
  Result:=inherited SetProperty(L, aPropertyName, aValueIndex);
end;

destructor TUICustomStringList.Destroy;
begin
  if FOwner then FreeAndNil( FContent );
  inherited Destroy;
end;

procedure TUICustomStringList.SetContent(AValue: TUIStringArray);
begin
  if FContent=AValue then Exit;
  FContent:=AValue;
end;

{ TUICustomStringBuffer }

constructor TUICustomStringBuffer.Create ( aParent : TUIElement;
  const aArea : TUIRect; aVisibleCount : DWord; aContent : TUIStringBuffer;
  aOwner : Boolean ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass   := 'stringlist';
  FContent      := nil;
  FOwner        := True;
  FScroll       := 0;
  FVisibleCount := aVisibleCount;

  SetContent( aContent, aOwner );
  SetScroll( FCount );
end;

procedure TUICustomStringBuffer.Add ( const aString : TUIString ) ;
begin
  FDirty := True;
  FContent.PushBack( aString );
  FCount := FContent.Size;
  SetScroll( FCount );
end;

procedure TUICustomStringBuffer.SetContent ( aContent : TUIStringBuffer; aOwner : Boolean ) ;
begin
  FDirty := True;
  if (FContent <> nil) and FOwner then FreeAndNil( FContent );
  FOwner := aOwner;
  FContent := aContent;
  if FContent <> nil then
    FCount := FContent.Size
  else
    FCount := 0;
  SetScroll(FCount);
end;

destructor TUICustomStringBuffer.Destroy;
begin
  if FOwner then FreeAndNil( FContent );
  inherited Destroy;
end;

{ TUICustomChunkBuffer }

constructor TUICustomChunkBuffer.Create ( aParent : TUIElement;
  const aArea : TUIRect; aVisibleCount : DWord; aContent : TUIChunkBuffer;
  aOwner : Boolean ) ;
begin
  inherited Create( aParent, aArea );
  FStyleClass   := 'stringlist';
  FContent      := nil;
  FOwner        := True;
  FScroll       := 0;
  FVisibleCount := aVisibleCount;

  SetContent( aContent, aOwner );
  SetScroll( FCount );
end;

destructor TUICustomChunkBuffer.Destroy;
begin
  if FOwner then FreeAndNil( FContent );
  inherited Destroy;
end;

procedure TUICustomChunkBuffer.SetContent ( aContent : TUIChunkBuffer; aOwner : Boolean ) ;
begin
  FDirty := True;
  if (FContent <> nil) and FOwner then FreeAndNil( FContent );
  FOwner := aOwner;
  FContent := aContent;
  if FContent <> nil then
    FCount := FContent.Size
  else
    FCount := 0;
  SetScroll(FCount);
end;

{ TUICustomMessages }

constructor TUICustomMessages.Create ( aParent : TUIElement;
  const aArea : TUIRect; aVisibleCount : DWord; aOnMore : TUINotifyEvent;
  aBufferSize : Word ) ;
begin
  inherited Create( aParent, aArea );
  FOnMore       := aOnMore;
  FStyleClass   := 'stringlist';
  FActive       := 0;
  FContent      := TUIChunkBuffer.Create( aBufferSize );
  FPending      := nil;
  FHighlights   := TUIHighlightsArray.Create;
  FScroll       := 0;
  FVisibleCount := aVisibleCount;
  FCount        := 0;
  SetScroll( 0 );
end;

procedure TUICustomMessages.Add ( const aMessage : Ansistring ) ;
var iCurrent : Integer;
    iCount   : Integer;
    iChunk   : TUIChunk;
    iColor   : TUIColor;
begin
  FDirty := True;
  // currently let's assume that more is not active
  iCurrent := 0;
  if (FActive > 0) and (High(FContent.Back) >= 0) then
  begin
    iChunk   := FContent.Back[ High(FContent.Back) ];
    iCurrent := Length( iChunk.Content ) + iChunk.Position.x+1;
  end;

  iColor := ColorNone;
  if FHighlights.Size > 0 then
  for iCount := 0 to FHighlights.Size-1 do
    with FHighlights[iCount] do
      if IsWild(aMessage,FHighlights[iCount].Wildcard,False) then
        iColor := FHighlights[iCount].Color;

  FPending := Chunkify( aMessage, iCurrent, iColor );
  if FPending = nil then Exit;

  // handle the left space
  if iCurrent > 0 then
  begin
    FContent[-1] := ChunkListAppend( FContent[-1], FPending.Front );
    if FPending.Size <= 1 then
    begin
      FreeAndNil( FPending );
      Exit;
    end;
    FPending.PopFront;
    iCurrent := 0;
  end;

  AddPending;
  SetScroll( FCount );
end;

procedure TUICustomMessages.More;
begin
  if Assigned( FOnMore ) then FOnMore( Self );
end;

procedure TUICustomMessages.Update;
begin
  FDirty := True;
  FActive := 0;
  AddPending;
end;

procedure TUICustomMessages.Clear;
begin
  FDirty := True;
  FContent.Clear;
  FActive := 0;
end;

procedure TUICustomMessages.Reset;
var iCount : Integer;
begin
  FDirty := True;
  for iCount := 1 to FVisibleCount do
    FContent.PushBack( nil );
end;

procedure TUICustomMessages.Pop;
begin
  FDirty := True;
  if FContent.Size > 0 then FContent.PopBack;
  if FActive > 0 then Dec( FActive );
end;

destructor TUICustomMessages.Destroy;
begin
  FreeAndNil( FHighlights );
  FreeAndNil( FContent );
  inherited Destroy;
end;

function TUICustomMessages.Size : DWord;
begin
  Exit( FContent.Size );
end;

procedure TUICustomMessages.AddPending;
begin
  FDirty := True;
  if FPending = nil then Exit;

  while (FActive < FVisibleCount) and (FPending.Size > 0) do
  begin
    FContent.PushBack( FPending.Front );
    FPending.PopFront;
    Inc(FActive);
  end;
  FCount := FContent.Size;

  if FPending.Size = 0 then
    FreeAndNil( FPending )
  else
    More;
end;

procedure TUICustomMessages.AddHighlightCallback ( aKey, aValue : Variant ) ;
var iHighlight : TUIMessageHighlight;
begin
  iHighlight.Color    := aValue;
  iHighlight.Wildcard := aKey;
  FHighlights.Push( iHighlight );
end;

initialization

end.

