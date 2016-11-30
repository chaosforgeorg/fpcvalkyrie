{$INCLUDE valkyrie.inc}
unit vconui;
interface
uses Classes, SysUtils, vutil, vnode, viotypes, vioevent, vioconsole, vuitypes, vgenerics,
     vuielement, vuielements, vuiconsole;

type TConUIRoot = class( TUIRoot )
  constructor Create( aRenderer : TIOConsoleRenderer; aStyle : TUIStyle = nil );
  procedure DeviceChanged; override;
  procedure LoadDefaultStyle; virtual;
  procedure ElementDestroyed( aElement : TUIElement ); override;
  procedure Add( aNode : TNode ); override;
  procedure Render;
  function OnEvent( const event : TIOEvent ) : Boolean; override;
  destructor Destroy; override;
  function DeviceCoordToConsoleCoord( aCoord : TPoint ) : TPoint;
  function ConsoleCoordToDeviceCoord( aCoord : TPoint ) : TPoint;
private
  FConsole     : TUIConsole;
  FRenderer    : TIOConsoleRenderer;
  FNeedRedraw  : Boolean;
  FDeviceArea  : TUIRect;
  FConsoleArea : TUIRect;
  FDeviceDim   : TUIRect;
  FConsoleDim  : TUIRect;
  FCellX       : Integer;
  FCellY       : Integer;
  FAreaMatch   : Boolean;
public
  property Console  : TUIConsole         read FConsole;
  property Renderer : TIOConsoleRenderer read FRenderer;
  property NeedRedraw : Boolean          read FNeedRedraw write FNeedRedraw;
  property DeviceArea : TUIRect          read FDeviceArea;
end;

type TConUILabel      = class( TUICustomLabel )
  constructor Create( aParent : TUIElement; const aPos : TUIPoint; const aText : TUIString );
  procedure OnRedraw; override;
  procedure SetStyle( aStyle : TUIStyle ); override;
protected
  FOpaque : Boolean;
public
  property Opaque : Boolean read FOpaque write FOpaque;
end;

type TConUIText       = class( TUICustomText )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; const aText : TUIString; aScrolling : Boolean = False );
  constructor Create( aParent : TUIElement; const aText : TUIString; aScrolling : Boolean = False  );
  procedure OnRedraw; override;
  procedure SetText( const aText : TUIString ); override;
  procedure SetStyle( aStyle : TUIStyle ); override;
protected
  procedure RecalcDimensions; override;
private
  FTextChunks : TUIChunkList;
end;

type TConUIWindow     = class( TUICustomWindow )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; const aTitle : TUIString );
  function GetAvailableDim : TUIRect; override;
  procedure OnRedraw; override;
  procedure SetStyle( aStyle : TUIStyle ); override;
protected
  FFrameChars : AnsiString;
  FFrameColor : TUIColor;
public
  property FrameChars : AnsiString read FFrameChars write FFrameChars;
  property FrameColor : TUIColor   read FFrameColor write FFrameColor;
end;

type TConUISeparator = class( TUICustomSeparator )
  constructor Create( aParent : TUIElement; aOrient : TUIOrientation; aPosition : Integer );
  procedure OnRedraw; override;
  procedure SetStyle( aStyle : TUIStyle ); override;
protected
  FFrameChars : AnsiString;
  FFrameColor : TUIColor;
public
  property FrameChars : AnsiString read FFrameChars write FFrameChars;
  property FrameColor : TUIColor   read FFrameColor write FFrameColor;
end;

type TConUIInputLine  = class( TUICustomInputLine )
  constructor Create( aParent : TUIElement; const aPosition : TUIPoint; aMaxLength : DWord );
  procedure OnRedraw; override;
end;

type TConUIMenu       = class( TUICustomMenu )
  constructor Create( aParent : TUIElement; const aArea : TUIRect );
  constructor Create( aParent : TUIElement; const aPosition : TUIPoint );
  procedure OnRedraw; override;
  function MousePosToEntry( const aPosition : TUIPoint ) : DWord; override;
  procedure SetStyle( aStyle : TUIStyle ); override;
protected
  procedure RecalcDimensions; override;
protected
  FSelectedColor    : TUIColor;
  FInactiveColor    : TUIColor;
  FSelInactiveColor : TUIColor;
public
  property SelectedColor : TUIColor read FSelectedColor write FSelectedColor;
end;

type TConUIStringList = class( TUICustomStringList )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aContent : TUIStringArray = nil; aOwner : Boolean = True );
  procedure OnRedraw; override;
protected
  procedure RecalcDimensions; override;
end;

type TConUIStringBuffer = class( TUICustomStringBuffer )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aContent : TUIStringBuffer = nil; aOwner : Boolean = True );
  procedure OnRedraw; override;
protected
  procedure RecalcDimensions; override;
end;

type TConUIChunkBuffer = class( TUICustomChunkBuffer )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aContent : TUIChunkBuffer; aOwner : Boolean = False );
  procedure OnRedraw; override;
protected
  procedure RecalcDimensions; override;
end;

type TConUIMessages = class( TUICustomMessages )
  constructor Create( aParent : TUIElement; const aArea : TUIRect; aOnMore : TUINotifyEvent; aBufferSize : Word = 1000 ); reintroduce;
  procedure OnRedraw; override;
protected
  function Chunkify( const aString : AnsiString; aStart : Integer; aColor : TUIColor = ColorNone ) : TUIChunkBuffer; override;
end;

type TConUIConsoleHistory = specialize TGRingBuffer<AnsiString>;
type TConUIConsole = class;
type TConUIConsoleCallback = function ( aSender : TConUIConsole; const aLine : AnsiString ) : Boolean of object;
type TConUIConsole = class( TConUIWindow )
  constructor Create( aParent : TUIElement; aCallback : TConUIConsoleCallback );
  procedure Writeln( const aText : Ansistring );
  procedure LoadHistory( const aFileName : AnsiString );
  procedure SaveHistory( const aFileName : AnsiString );
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  function OnLineConfirm( aSender : TUIElement ) : Boolean;
protected
  FHPos     : DWord;
  FHistory  : TConUIConsoleHistory;
  FInput    : TConUIInputLine;
  FOutput   : TConUIStringList;
  FCallback : TConUIConsoleCallback;
public
  property History : TConUIConsoleHistory read FHistory;
end;

implementation

uses math;

{ TConUILabel }

constructor TConUILabel.Create ( aParent : TUIElement; const aPos : TUIPoint; const aText : TUIString ) ;
begin
  inherited Create( aParent, Rectangle( aPos, Length(aText), 1 ), aText );
  FOpaque := False;
end;

procedure TConUILabel.OnRedraw;
var iBackColor : TUIColor;
begin
  inherited OnRedraw;
  if FOpaque
    then iBackColor := ColorNone
    else iBackColor := FBackColor;
  if ( FExStyle ) then
    TConUIRoot(FRoot).Console.PrintEx( FAbsolute.Pos, FOffset, FFont, FForeColor, iBackColor, FText, FCodedContent )
  else
    TConUIRoot(FRoot).Console.Print( FAbsolute.Pos, FForeColor, iBackColor, FText, FCodedContent );
end;

procedure TConUILabel.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;
  FOpaque := StyleValue['opaque'];
end;

{ TConUIText }

constructor TConUIText.Create ( aParent : TUIElement; const aArea : TUIRect; const aText : TUIString;
  aScrolling : Boolean ) ;
begin
  inherited Create( aParent, aArea, '', aScrolling );
  FTextChunks := nil;
  FText := aText; // delayed
end;

constructor TConUIText.Create ( aParent : TUIElement; const aText : TUIString; aScrolling : Boolean ) ;
begin
  inherited Create( aParent, aParent.GetDimRect, aText, aScrolling );
  FText := aText; // delayed
end;

procedure TConUIText.OnRedraw;
var iCon   : TUIConsole;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );
  if ( FExStyle ) then
    iCon.PrintEx( FAbsolute.Pos - Point(0,FScroll), FOffset, FFont, FTextChunks, FForeColor, FBackColor, FAbsolute )
  else
    iCon.Print( FAbsolute.Pos - Point(0,FScroll), FTextChunks, FForeColor, FBackColor, FAbsolute );
end;

procedure TConUIText.SetText ( const aText : TUIString ) ;
var iCon   : TUIConsole;
begin
  inherited SetText ( aText );
  iCon.Init( TConUIRoot(FRoot).Renderer );

  if aText = ''
    then SetLength( FTextChunks, 0 )
    else if FScrollable
      then FTextChunks := iCon.Chunkify( aText, Point( FDimensions.X+1, High(Integer) ), FForeColor )
      else FTextChunks := iCon.Chunkify( aText, FDimensions, FForeColor );

  FCount := 0;
  if High(FTextChunks) >= 0 then
    FCount := FTextChunks[ High(FTextChunks) ].Position.y+1;
end;

procedure TConUIText.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;
  SetText( FText );
end;

procedure TConUIText.RecalcDimensions;
begin
  inherited RecalcDimensions;
  FVisibleCount := FAbsolute.h;
end;

{ TConUIWindow }

constructor TConUIWindow.Create ( aParent : TUIElement; const aArea : TUIRect; const aTitle : TUIString ) ;
begin
  inherited Create( aParent, aArea, aTitle );
  FFrameChars := '        ';
  FFrameColor := 0;
end;

function TConUIWindow.GetAvailableDim : TUIRect;
begin
  Exit( inherited GetAvailableDim.Shrinked(2) );
end;

procedure TConUIWindow.OnRedraw;
var iCon   : TUIConsole;
    C      : Integer;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );

  for C := FAbsolute.Pos.X+1 to FAbsolute.Pos.X+FAbsolute.Dim.X-2 do
    begin
      iCon.DrawChar(Point(C,FAbsolute.Pos.Y),FFrameColor,FFrameChars[1]);
      iCon.DrawChar(Point(C,FAbsolute.Pos.Y+FAbsolute.Dim.Y-1),FFrameColor,FFrameChars[3]);
    end;
  for C := FAbsolute.Pos.Y+1 to FAbsolute.Pos.Y+FAbsolute.Dim.Y-2 do
    begin
      iCon.DrawChar(Point(FAbsolute.Pos.X,C),FFrameColor,FFrameChars[2]);
      iCon.DrawChar(Point(FAbsolute.Pos.X+FAbsolute.Dim.X-1,C),FFrameColor,FFrameChars[4]);
    end;

  iCon.DrawChar( FAbsolute.TopLeft,     FFrameColor,FFrameChars[5] );
  iCon.DrawChar( FAbsolute.TopRight,    FFrameColor,FFrameChars[6] );
  iCon.DrawChar( FAbsolute.BottomLeft,  FFrameColor,FFrameChars[7] );
  iCon.DrawChar( FAbsolute.BottomRight, FFrameColor,FFrameChars[8] );

  if FTitle <> '' then
  begin
    C := FAbsolute.Pos.X + FAbsolute.W div 2 - Length( FTitle ) div 2;
    if ( FExStyle ) then
      iCon.PrintEx(Point(C-1,FAbsolute.Pos.Y),FOffset, FFont, FForeColor,FBackColor,' '+FTitle+' ', true)
    else
      iCon.RawPrint(Point(C-1,FAbsolute.Pos.Y),FForeColor,' '+FTitle+' ');
  end;
end;

procedure TConUIWindow.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;
  FFrameChars := StyleValue[ 'frame_chars' ];
  FFrameColor := StyleValue[ 'frame_color' ];
end;

{ TConUISeparator }

constructor TConUISeparator.Create ( aParent : TUIElement; aOrient : TUIOrientation; aPosition : Integer );
begin
  inherited Create( aParent, aParent.GetDimRect, aOrient, aPosition );
end;

procedure TConUISeparator.OnRedraw;
var iCon   : TUIConsole;
    c      : Integer;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FTopLeft.AbsDim, FBackColor );
  iCon.ClearRect( FBottomRight.AbsDim, FBackColor );
  if FOrient = VORIENT_HORIZONTAL then
  begin
    for c := 0 to Dim.X-1 do
      iCon.DrawChar( FAbsolute.Pos + Point(c,FSepPos-1), FFrameColor, FFrameChars[9] );
  end
  else
  begin
    for c := 0 to Dim.Y-1 do
      iCon.DrawChar( FAbsolute.Pos + Point(FSepPos-1,c), FFrameColor, FFrameChars[10] );
  end;

end;

procedure TConUISeparator.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;

  FFrameChars := StyleValue[ 'frame_chars' ];
  FFrameColor := StyleValue[ 'frame_color' ];
end;


{ TConUIInputLine }

constructor TConUIInputLine.Create ( aParent : TUIElement; const aPosition : TUIPoint; aMaxLength : DWord ) ;
begin
  inherited Create( aParent, Rectangle( aPosition, Point( aMaxLength, 1 ) ) );
  SetMaxLength( aMaxLength );
end;

procedure TConUIInputLine.OnRedraw;
var Con : TUIConsole;
begin
  inherited OnRedraw;
  Con.Init( TConUIRoot(FRoot).Renderer );
  Con.ClearRect( FAbsolute, FBackColor );
  if ( FExStyle ) then
    Con.PrintEx( FAbsolute.Pos, FOffset, FFont, FForeColor, FBackColor, FInput, true )
  else
    Con.RawPrint( FAbsolute.Pos, FForeColor, FBackColor, FInput );
  Con.Raw.MoveCursor( FAbsolute.Pos.X+FCursorPosition-1, FAbsolute.Pos.Y );
end;

{ TConUIMenu }

constructor TConUIMenu.Create ( aParent : TUIElement; const aArea : TUIRect ) ;
begin
  inherited Create( aParent, aArea, aArea.h );
end;

constructor TConUIMenu.Create ( aParent : TUIElement; const aPosition : TUIPoint ) ;
begin
  inherited Create( aParent, aParent.GetDimRect, 255 );
end;

procedure TConUIMenu.OnRedraw;
var Con : TUIConsole;
    c   : DWord;
  function Color( aIdx : DWord ) : TUIColor;
  begin
    if not IsActive(aIdx) then
      if aIdx = FSelected then Exit( FSelInactiveColor )
                          else Exit( FInactiveColor );
    if aIdx = FSelected   then Exit( FSelectedColor );
    if FItems[aIdx].Color <> 0 then Exit( FItems[aIdx].Color );
    Exit( FForeColor );
  end;

begin
  inherited OnRedraw;
  Con.Init( TConUIRoot(FRoot).Renderer );
  Con.ClearRect( FAbsolute, FBackColor );
  if FCount > 0 then
  for c := 1+FScroll to Min( FScroll+FVisibleCount, FCount ) do
    if ( FExStyle ) then
      Con.PrintEx( FAbsolute.Pos + Point(0, c-1-FScroll), FOffset, FFont, Color(c), FBackColor, FItems[ c ].Text, FCodedContent )
    else
      Con.Print( FAbsolute.Pos + Point(0, c-1-FScroll), Color(c), FBackColor, FItems[ c ].Text, FCodedContent )
end;

function TConUIMenu.MousePosToEntry ( const aPosition : TUIPoint ) : DWord;
begin
  if aPosition in FAbsolute then
  begin
    Result := aPosition.Y-FAbsolute.Y+1+FScroll;
    if IsValid( Result ) then Exit( Result );
  end;
  Exit( 0 );
end;

procedure TConUIMenu.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;
  FSelectedColor    := StyleValue[ 'selected_color' ];
  FInactiveColor    := StyleValue[ 'inactive_color' ];
  FSelInactiveColor := StyleValue[ 'selinactive_color' ];;
end;

procedure TConUIMenu.RecalcDimensions;
begin
  inherited RecalcDimensions;
  FVisibleCount := Min( Max(FCount, FVisibleCount), FAbsolute.h );
end;

{ TConUIStringList }

constructor TConUIStringList.Create ( aParent : TUIElement; const aArea : TUIRect; aContent : TUIStringArray; aOwner : Boolean ) ;
begin
  inherited Create( aParent, aArea, aArea.h, aContent, aOwner );
end;

procedure TConUIStringList.OnRedraw;
var iCon : TUIConsole;
    c    : DWord;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );
  if (FContent <> nil) and (FContent.Size > 0) then
  for c := 1+FScroll to Min( FScroll+FVisibleCount, FContent.Size ) do
     if ( FExStyle ) then
       iCon.PrintEx( FAbsolute.Pos + Point(0,c-1-FScroll), FOffset, FFont, FForeColor, FBackColor, FContent[ c-1 ], FCodedContent )
     else
       iCon.Print( FAbsolute.Pos + Point(0,c-1-FScroll), FForeColor, FBackColor, FContent[ c-1 ], FCodedContent );
end;

procedure TConUIStringList.RecalcDimensions;
begin
  inherited RecalcDimensions;
  FVisibleCount := FAbsolute.h
end;


{ TConUIStringBuffer }

constructor TConUIStringBuffer.Create ( aParent : TUIElement;
  const aArea : TUIRect; aContent : TUIStringBuffer; aOwner : Boolean ) ;
begin
  inherited Create( aParent, aArea, aArea.h, aContent, aOwner );
end;

procedure TConUIStringBuffer.OnRedraw;
var iCon : TUIConsole;
    c    : DWord;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );
  if (FContent <> nil) and (FContent.Size > 0) then
  for c := 1+FScroll to Min( FScroll+FVisibleCount, FContent.Size ) do
     if ( FExStyle ) then
       iCon.PrintEx( FAbsolute.Pos + Point(0,c-1-FScroll), FOffset, FFont, FForeColor, FBackColor, FContent[ c-1 ], FCodedContent )
     else
       iCon.Print( FAbsolute.Pos + Point(0,c-1-FScroll), FForeColor, FBackColor, FContent[ c-1 ], FCodedContent );
end;

procedure TConUIStringBuffer.RecalcDimensions;
begin
  inherited RecalcDimensions;
  FVisibleCount := FAbsolute.h
end;

{ TConUIChunkBuffer }

constructor TConUIChunkBuffer.Create ( aParent : TUIElement;
  const aArea : TUIRect; aContent : TUIChunkBuffer; aOwner : Boolean ) ;
begin
  inherited Create( aParent, aArea, aArea.h, aContent, aOwner );
end;

procedure TConUIChunkBuffer.OnRedraw;
var iCon : TUIConsole;
    c    : DWord;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );
  if (FContent <> nil) and (FContent.Size > 0) then
  for c := 1+FScroll to Min( FScroll+FVisibleCount, FContent.Size ) do
     iCon.Print( FAbsolute.Pos + Point(0,c-1-FScroll), FContent[ c-1 ], FForeColor, FBackColor, FAbsolute );
end;

procedure TConUIChunkBuffer.RecalcDimensions;
begin
  inherited RecalcDimensions;
  FVisibleCount := FAbsolute.h
end;

{ TConUIMessages }

constructor TConUIMessages.Create ( aParent : TUIElement;
  const aArea : TUIRect; aOnMore : TUINotifyEvent;
  aBufferSize : Word ) ;
begin
  inherited Create( aParent, aArea, aArea.h, aOnMore, aBufferSize );
end;

procedure TConUIMessages.OnRedraw;
var iCon   : TUIConsole;
    i      : DWord;
    iMax   : DWord;
    iColor : TUIColor;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );
  iMax := Min( FScroll+FVisibleCount, FContent.Size );
  if FContent.Size > 0 then
  for i := 1+FScroll to iMax do
  begin
    iColor := FForeColor;
    if i > iMax - FActive then iColor := iCon.BoldColor( FForeColor );
    iCon.Print( FAbsolute.Pos + Point(0,i-1-FScroll), FContent[ i-1 ], iColor, FBackColor, FAbsolute );
  end;
end;

function TConUIMessages.Chunkify ( const aString : AnsiString; aStart : Integer; aColor : TUIColor = ColorNone ) : TUIChunkBuffer;
var iCon       : TUIConsole;
    iChunkList : TUIChunkList;
    iPosition  : TUIPoint;
    iColor     : TUIColor;
begin
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iPosition := Point(aStart,0);
  iColor    := aColor;
  iCon.ChunkifyEx( iChunkList, iPosition, iColor, aString, iColor, Dim );
  Exit( iCon.LinifyChunkList( iChunkList ) );
end;

{ TConUIConsole }

constructor TConUIConsole.Create ( aParent : TUIElement; aCallback : TConUIConsoleCallback ) ;
begin
  inherited Create( aParent, Rectangle( PointZero, aParent.Dim.x, 15 ), 'Console' );
  FEventFilter := [ VEVENT_KEYDOWN ];
  FCallback := aCallback;
  FOutput   := TConUIStringList.Create( Self, Rectangle(0,0,Dim.X-4,Dim.Y-4 ), TUIStringArray.Create(), True );
  FInput    := TConUIInputLine.Create( Self, Point( 0, Dim.Y-4 ), Dim.X - 4 );
  FInput.OnConfirmEvent := @OnLineConfirm;
  FHistory  := nil;
  FHPos     := 0;
  FRoot.GrabInput(Self);
end;

procedure TConUIConsole.Writeln ( const aText : Ansistring ) ;
begin
  FOutput.Add( aText );
  FOutput.Scroll := FOutput.Count;
end;

procedure TConUIConsole.LoadHistory ( const aFileName : AnsiString ) ;
var iStream : TStream;
begin
  FreeAndNil( FHistory );
  if FileExists( aFileName ) then
  begin
    try
      iStream := TFileStream.Create( aFileName, fmOpenRead );
      FHistory := TConUIConsoleHistory.CreateFromStream( iStream );
    finally
      FreeAndNil( iStream );
    end;
  end;
  if FHistory = nil then FHistory := TConUIConsoleHistory.Create(256);
end;

procedure TConUIConsole.SaveHistory ( const aFileName : AnsiString ) ;
var iStream : TStream;
begin
  if FHistory <> nil then
  try
    iStream := TFileStream.Create( aFileName, fmCreate );
    FHistory.WriteToStream( iStream );
  finally
    FreeAndNil( iStream );
  end;
end;

function TConUIConsole.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if FHistory <> nil then
  case event.Code of
    VKEY_UP   : if FHPos < FHistory.Size then begin Inc( FHPos ); FInput.Input := FHistory.Get( -FHPos ); Exit( True ); end;
    VKEY_DOWN : if FHPos > 1             then begin Dec( FHPos ); FInput.Input := FHistory.Get( -FHPos ); Exit( True ); end;
  end;
  Result := inherited OnKeyDown ( event ) ;
end;

function TConUIConsole.OnLineConfirm ( aSender : TUIElement ) : Boolean;
begin
  if Assigned( FCallback ) then
  begin
    if Assigned( FHistory ) then
      if not ((FHistory.Size > 0) and (FHistory.Back = FInput.Input)) then
        FHistory.PushBack( FInput.Input );
    Result := FCallback( Self, FInput.Input );
    FHPos  := 0;
    FInput.Input := '';
    Exit;
  end;
  Exit( False );
end;

{ TConsoleUIRoot }

constructor TConUIRoot.Create ( aRenderer : TIOConsoleRenderer; aStyle : TUIStyle = nil ) ;
begin
  FConsole.Init( aRenderer );
  FRenderer := aRenderer;
  inherited Create( FConsole.GetRect );
  FStyle    := aStyle;

  if FStyle = nil then LoadDefaultStyle;

  DeviceChanged;
end;

procedure TConUIRoot.DeviceChanged;
begin
  FDeviceArea  := FRenderer.GetDeviceArea;
  FConsoleArea := FConsole.GetRect;
  FConsoleDim  := Rectangle( PointZero, FConsoleArea.Dim );
  FDeviceDim   := Rectangle( PointZero, FDeviceArea.Dim );
  FAreaMatch   := ( FDeviceArea.w = FConsoleArea.w ) and ( FDeviceArea.h = FConsoleArea.h );
  FCellX       := (FDeviceDim.Dim.X) div (FConsoleDim.Dim.X);
  FCellY       := (FDeviceDim.Dim.Y) div (FConsoleDim.Dim.Y);
  FNeedRedraw := True;

  inherited DeviceChanged;
end;

procedure TConUIRoot.LoadDefaultStyle;
begin
  FreeAndNil( FStyle );
  FStyle := TUIStyle.Create('default');
  FStyle.Add('','selected_color', White );
  FStyle.Add('','inactive_color', DarkGray );
  FStyle.Add('','selinactive_color', LightRed );
  FStyle.Add('','fore_color', LightGray );
  FStyle.Add('menu','fore_color', DarkGray );
  FStyle.Add('','icon_color', White );
  FStyle.Add('','scroll_chars', '^v' );
  FStyle.Add('','back_color', Black );
  FStyle.Add('','opaque', False );
  FStyle.Add('','frame_color', Blue );
  FStyle.Add('','frame_chars', #196+#179+#196+#179+#218+#191+#192+#217+#196+#179+'^v' );
  FStyle.Add('window','fore_color', White );
  FStyle.Add('window','back_color', $22222200 );
  FStyle.Add('input','fore_color', LightBlue );
  FStyle.Add('input','back_color', Blue );
  FStyle.Add('text','fore_color', LightBlue );
  FStyle.Add('text','back_color', Blue );
end;

procedure TConUIRoot.ElementDestroyed ( aElement : TUIElement ) ;
begin
  inherited ElementDestroyed ( aElement ) ;
  FNeedRedraw := True;
end;

procedure TConUIRoot.Add ( aNode : TNode ) ;
begin
  inherited Add ( aNode ) ;
  FNeedRedraw := True;
end;

procedure TConUIRoot.Render;
begin
  inherited Render( FNeedRedraw or FDirty );
  FConsole.Update;
end;

function TConUIRoot.OnEvent ( const event : TIOEvent ) : Boolean;
var iElement : TUIElement;
    iEvent   : TIOEvent;
begin
  iEvent := event;
  // TRANSLATE EVENT
  case event.EType of
    VEVENT_MOUSEDOWN : iEvent.MouseMove.Pos := DeviceCoordToConsoleCoord( event.MouseMove.Pos );
    VEVENT_MOUSEMOVE : iEvent.Mouse.Pos     := DeviceCoordToConsoleCoord( event.Mouse.Pos );
  end;
  //Writeln( IOEventToString( iEvent ) );

  // HANDLE EVENT
  case event.EType of
    VEVENT_SYSTEM    :
      begin
        if ((FEventRoot <> Self) and FEventRoot.OnEvent( event )) or (FEventRoot.ProcessEventDown( event )) then
        FNeedRedraw := True; Exit( False );
      end;
    VEVENT_KEYDOWN   :
      begin
        if ((FEventRoot <> Self) and FEventRoot.OnEvent( event )) or (FEventRoot.ProcessEventDown( event )) then
        begin FNeedRedraw := True; Exit( True ); end;
      end;
    VEVENT_MOUSEMOVE,
    VEVENT_MOUSEDOWN :
      begin
        iElement := FEventRoot.GetElement( iEvent.Mouse.Pos );
        if iElement <> nil then
        if iElement <> Self then
        if iElement.OnEvent( iEvent ) or iElement.ProcessEventUp( iEvent, Self ) then
          begin
            FNeedRedraw := True;
            Exit( True );
          end;
      end;
  end;
  if FEventRoot <> Self then Exit( True );
  Result := inherited OnEvent ( event ) ;
end;

destructor TConUIRoot.Destroy;
begin
  FreeAndNil( FStyle );
  inherited Destroy;
end;

function TConUIRoot.DeviceCoordToConsoleCoord ( aCoord : TPoint ) : TPoint;
begin
  if FAreaMatch then
    Exit( aCoord - FDeviceArea.Pos + FConsoleArea.Pos )
  else
  begin
    aCoord := aCoord - FDeviceArea.Pos;
    aCoord.x := ( aCoord.x div FCellX );
    aCoord.y := ( aCoord.y div FCellY );
    Exit( FConsoleArea.Pos + aCoord );
  end;
end;

function TConUIRoot.ConsoleCoordToDeviceCoord ( aCoord : TPoint ) : TPoint;
begin
  if FAreaMatch then
    Exit( aCoord - FConsoleArea.Pos + FDeviceArea.Pos )
  else
  begin
    aCoord := aCoord - FConsoleArea.Pos;
    aCoord.x := ( aCoord.x * FCellX );
    aCoord.y := ( aCoord.y * FCellY );
    Exit( FDeviceArea.Pos + aCoord );
  end;
end;

end.

