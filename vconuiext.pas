{$INCLUDE valkyrie.inc}
unit vconuiext;
interface
uses SysUtils, vgenerics, vconui, viotypes, vioevent, vioconsole, vuitypes,
     vuielement, vuielements, vuiconsole;

type TConUIScrollableIcons = class( TUIElement )
  constructor Create( aParent : TUIElement; aSource : IUIScrollable; const aScrollArea : TUIRect; const aPercentPos : TUIPoint );
  constructor Create( aParent : TUIElement; aSource : IUIScrollable; const aScrollArea : TUIRect );
  procedure SetStyle( aStyle : TUIStyle ); override;
  function GetElement( const aPoint : TUIPoint ) : TUIElement; override;
  procedure OnRedraw; override;
protected
  FSource      : IUIScrollable;
  FPercent     : Boolean;
  FPercentPos  : TUIPoint;
  FIconColor   : TUIColor;
  FScrollChars : AnsiString;
public
  property IconColor   : TUIColor   read FIconColor   write FIconColor;
  property ScrollChars : AnsiString read FScrollChars write FScrollChars;
end;

type TConUIFullWindow = class( TUIElement )
  constructor Create( aParent : TUIElement; const aHeader,aFooter : TUIString );
  procedure OnRedraw; override;
  procedure SetStyle( aStyle : TUIStyle ); override;
private
  function GetHeader : TUIString; virtual;
  procedure SetHeader ( aValue : TUIString ) ; virtual;
  function GetFooter : TUIString; virtual;
  procedure SetFooter ( aValue : TUIString ) ; virtual;
protected
  FFrameChars : AnsiString;
  FFrameColor : TUIColor;
  FHeader     : TConUILabel;
  FFooter     : TConUILabel;
published
  property Header     : TUIString  read GetHeader   write SetHeader;
  property Footer     : TUIString  read GetFooter   write SetFooter;
  property FrameChars : AnsiString read FFrameChars write FFrameChars;
  property FrameColor : TUIColor   read FFrameColor write FFrameColor;
end;

type TConUIViewer = class( TConUIFullWindow )
  constructor Create( aParent : TUIElement; const aHeader : TUIString; const aContent : TUIStringArray; aOwner : Boolean = True );
  function OnConfirm : Boolean; virtual;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
protected
  FContent   : TConUIStringList;
  FOnConfirm : TUINotifyEvent;
public
  property OnConfirmEvent : TUINotifyEvent write FOnConfirm;
end;

type TConUIPagedViewer = class( TConUIFullWindow )
protected
  type TPages = specialize TGObjectArray< TUIStringArray >;
public
  constructor Create( aParent : TUIElement; aOwner : Boolean = True );
  procedure AddPage( aContent : TUIStringArray; const aTitle : TUIString );
  procedure SetPage( aIndex : DWord );
  function OnConfirm : Boolean; virtual;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  destructor Destroy; override;
protected
  FPage      : DWord;
  FPages     : TPages;
  FTitles    : TUIStringArray;
  FContent   : TConUIStringList;
  FOnConfirm : TUINotifyEvent;
public
  property OnConfirmEvent : TUINotifyEvent write FOnConfirm;
end;


implementation

uses vutil, vmath;

{ TConUIScrollableIcons }

constructor TConUIScrollableIcons.Create ( aParent : TUIElement;
  aSource : IUIScrollable; const aScrollArea : TUIRect;
  const aPercentPos : TUIPoint ) ;
begin
  inherited Create( aParent, aScrollArea );

  FSource     := aSource;
  FPercentPos := aPercentPos;
  FPercent    := True;
end;

constructor TConUIScrollableIcons.Create ( aParent : TUIElement;
  aSource : IUIScrollable; const aScrollArea : TUIRect ) ;
begin
  inherited Create( aParent, aScrollArea );

  FSource     := aSource;
  FPercent    := False;
end;

procedure TConUIScrollableIcons.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;

  FIconColor   := StyleValue[ 'icon_color' ];
  FScrollChars := StyleValue[ 'scroll_chars' ];
end;

function TConUIScrollableIcons.GetElement ( const aPoint : TUIPoint ) : TUIElement;
begin
  Exit( nil );
end;

procedure TConUIScrollableIcons.OnRedraw;
var iCon     : TUIConsole;
    iPercent : Byte;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  if not isVisible then Exit;

  if FSource.Count > FSource.VisibleCount then
  begin
    if FPercent then
    begin
      iPercent := Clamp(((FSource.Scroll + FSource.VisibleCount)*100) div FSource.Count, 0, 100 );
      iCon.RawPrint( FPercentPos, FIconColor, ' '+Format('%3d',[iPercent])+'% ' );
    end;

    if FSource.Scroll > 0 then
    begin
      iCon.RawPrint( Point( FAbsolute.x+1, FAbsolute.y-1 ), FIconColor, ' '+FScrollChars[1]+' ' );
      iCon.RawPrint( Point( FAbsolute.x2-3, FAbsolute.y-1 ), FIconColor, ' '+FScrollChars[1]+' ' );
    end;

    if FSource.Scroll < FSource.Count - FSource.VisibleCount then
    begin
      iCon.RawPrint( Point( FAbsolute.x+1, FAbsolute.y2+1 ), FIconColor, ' '+FScrollChars[2]+' ' );
      iCon.RawPrint( Point( FAbsolute.x2-3, FAbsolute.y2+1 ), FIconColor, ' '+FScrollChars[2]+' ' );
    end;
  end;
end;

{ TConUIFullWindow }

constructor TConUIFullWindow.Create ( aParent : TUIElement; const aHeader, aFooter : TUIString ) ;
var iRect : TUIRect;
begin
  inherited Create( aParent, aParent.GetDimRect );
  FStyleClass := 'full_window';
  iRect := Rectangle( PointZero, Dim );
  FHeader := TConUILabel.Create( Self, iRect.Pos, aHeader );
  FFooter := TConUILabel.Create( Self, iRect.BottomLeft, aFooter );
end;

procedure TConUIFullWindow.OnRedraw;
var iCon   : TUIConsole;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );
  iCon.RawPrint( Point( FAbsolute.x, FAbsolute.y+1 ), FFrameColor, StringOfChar(FFrameChars[1],FAbsolute.w+1) );
  iCon.RawPrint( Point( FAbsolute.x, FAbsolute.y2-1 ), FFrameColor, StringOfChar(FFrameChars[3],FAbsolute.w+1) );
end;

procedure TConUIFullWindow.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;
  FFrameChars := StyleValue[ 'frame_chars' ];
  FFrameColor := StyleValue[ 'frame_color' ];
end;

function TConUIFullWindow.GetFooter : TUIString;
begin
  Exit( FFooter.Text );
end;

function TConUIFullWindow.GetHeader : TUIString;
begin
  Exit( FHeader.Text );
end;

procedure TConUIFullWindow.SetFooter ( aValue : TUIString ) ;
begin
  FFooter.Text := aValue;
end;

procedure TConUIFullWindow.SetHeader ( aValue : TUIString ) ;
begin
  FHeader.Text := aValue;
end;

{ TConUIViewer }

constructor TConUIViewer.Create ( aParent : TUIElement; const aHeader : TUIString; const aContent : TUIStringArray; aOwner : Boolean ) ;
var iRect : TUIRect;
begin
  inherited Create( aParent, aHeader, ' Use arrows, PgUp, PgDown to scroll, Escape or Enter to exit. ' );
  FEventFilter := [ VEVENT_KEYDOWN, VEVENT_MOUSEDOWN ];
  iRect := Rectangle( PointZero, Dim ).Shrinked(1,2);
  FContent := TConUIStringList.Create( Self, iRect, aContent, aOwner );
  TConUIScrollableIcons.Create( Self, FContent, iRect, Point( FAbsolute.X2 - 5, FAbsolute.Y ) );
  FContent.EventFilter := [ VEVENT_KEYDOWN, VEVENT_MOUSEDOWN ];
end;

function TConUIViewer.OnConfirm : Boolean;
begin
  Exit( Assigned( FOnConfirm ) and FOnConfirm( Self ) );
end;

function TConUIViewer.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if (event.Code = VKEY_ESCAPE) or (event.Code = VKEY_ENTER) then Exit( OnConfirm );
  Result := inherited OnKeyDown ( event ) ;
end;

{ TConUIPagedViewer }

constructor TConUIPagedViewer.Create ( aParent : TUIElement; aOwner : Boolean ) ;
var iRect : TUIRect;
begin
  inherited Create( aParent, '', ' Use arrows, PgUp, PgDown to scroll, Left, Right to change pages, Escape or Enter to exit. ' );
  FEventFilter := [ VEVENT_KEYDOWN, VEVENT_MOUSEDOWN ];
  iRect := aParent.GetDimRect.Shrinked(1,2);
  FPages   := TPages.Create( aOwner );
  FTitles  := TUIStringArray.Create;
  FContent := TConUIStringList.Create( Self, iRect );
  TConUIScrollableIcons.Create( Self, FContent, iRect, Point( FAbsolute.X2 - 5, FAbsolute.Y ) );
  FContent.EventFilter := [ VEVENT_KEYDOWN, VEVENT_MOUSEDOWN ];
  FPage    := 0;
end;

procedure TConUIPagedViewer.AddPage ( aContent : TUIStringArray; const aTitle : TUIString );
begin
  FTitles.Push( aTitle );
  FPages.Push( aContent );
  if FContent.Content = nil then SetPage(0);
end;

procedure TConUIPagedViewer.SetPage ( aIndex : DWord ) ;
begin
  if aIndex >= FTitles.Size then Exit;
  FContent.SetContent( FPages[ aIndex ], False );
  Header := FTitles[ aIndex ];
  FPage  := aIndex;
  FDirty := True;
end;

function TConUIPagedViewer.OnConfirm : Boolean;
begin
  Exit( Assigned( FOnConfirm ) and FOnConfirm( Self ) );
end;

function TConUIPagedViewer.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if event.ModState <> [] then Exit( False );
  case event.Code of
    VKEY_LEFT   : if FTitles.Size > 0 then SetPage( TrueModulo( FPage - 1, FTitles.Size ) );
    VKEY_RIGHT  : if FTitles.Size > 0 then SetPage( TrueModulo( FPage + 1, FTitles.Size ) );
    VKEY_ESCAPE : Exit( OnConfirm );
    VKEY_ENTER  : Exit( OnConfirm );
  else Exit( inherited OnKeyDown ( event ) );
  end;
  Exit( True );
end;

destructor TConUIPagedViewer.Destroy;
begin
  FreeAndNil( FPages );
  FreeAndNil( FTitles );
  inherited Destroy;
end;

end.

