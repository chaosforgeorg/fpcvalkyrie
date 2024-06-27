{$INCLUDE valkyrie.inc}
unit vtig;
interface
uses vutil, viotypes, vtigstyle, vioconsole;

procedure VTIG_Initialize( aRenderer : TIOConsoleRenderer; aDriver : TIODriver; aClearOnRender : Boolean = True );
procedure VTIG_NewFrame;
procedure VTIG_EndFrame;
procedure VTIG_Render;
procedure VTIG_Clear;

procedure VTIG_Begin( aName : Ansistring ); overload;
procedure VTIG_Begin( aName : Ansistring; aSize : TIOPoint ); overload;
procedure VTIG_Begin( aName : Ansistring; aSize : TIOPoint; aPos : TIOPoint ); overload;
procedure VTIG_End;

implementation

uses Math, SysUtils, vtigcontext, vtigio, vioeventstate;

var GDefaultContext : TTIGContext;
    GCtx            : TTIGContext;

procedure VTIG_Initialize( aRenderer : TIOConsoleRenderer; aDriver : TIODriver; aClearOnRender : Boolean = True );
var iCanvas : TTIGWindow;
begin
  Assert( GCtx.Io.Driver = nil, 'TIG reinitialized' );
  GCtx.Io.Initialize( aRenderer, aDriver, aClearOnRender );
  GCtx.Size    := aRenderer.GetDeviceArea.Dim;
  GCtx.Color   := GCtx.Style^.Color[ VTIG_TEXT_COLOR ];
  GCtx.BGColor := GCtx.Style^.Color[ VTIG_BACKGROUND_COLOR ];

  iCanvas := TTIGWindow.Create;
  iCanvas.FClipContent := Rectangle( Point(0,0), GCtx.Size );
  iCanvas.DC.FContent  := iCanvas.FClipContent;
  iCanvas.DC.FClip     := iCanvas.FClipContent;
  iCanvas.FBackground  := GCtx.BGColor;

  GCtx.Windows.Push( iCanvas );
  GCtx.Current := iCanvas;
  GCtx.WindowStack.Push( iCanvas );
  GCtx.WindowOrder.Push( iCanvas );
  GCtx.DrawData.FCursor.CType := VTIG_CTNONE;
  GCtx.Time := GCtx.Io.Driver.GetMs;
end;

procedure VTIG_NewFrame;
var iWindow : TTIGWindow;
    iTime   : DWord;
    iLast   : Integer;
    iES     : TIOEventState;
begin
  GCtx.Size := GCtx.Io.Size;

  GCtx.Current.FClipContent := Rectangle( Point(0,0), GCtx.Size );
  GCtx.Current.DC.FContent  := GCtx.Current.FClipContent;
  GCtx.Current.DC.FClip     := GCtx.Current.FClipContent;

  for iWindow in GCtx.Windows do
  begin
    iWindow.DrawList.FCommands.Clear;
    iWindow.DrawList.FText.Clear;
  end;

  GCtx.Io.Update;
  iTime := GCtx.IO.Driver.GetMs;
  GCtx.DTime := iTime - GCtx.Time;
  GCtx.Time  := iTime;

  // teletype code not used

  iWindow := GCtx.WindowOrder.Top;
  GCtx.LastTop := iWindow;
  iLast := iWindow.FFocusInfo.Current;

  iES := GCtx.IO.EventState;

  if iES.Activated( VTIG_IE_DOWN, true )  then Inc( iWindow.FFocusInfo.Current );
  if iES.Activated( VTIG_IE_UP, true )    then Dec( iWindow.FFocusInfo.Current );
  if iES.Activated( VTIG_IE_HOME, false ) then iWindow.FFocusInfo.Current := 0;
  if iES.Activated( VTIG_IE_END, false )  then iWindow.FFocusInfo.Current := Max( iWindow.FFocusInfo.Count - 1, 0 );

  // TODO: change for multi-page
  if iES.activated( VTIG_IE_PGUP,   false ) then iWindow.FFocusInfo.Current := 0;
  if iES.activated( VTIG_IE_PGDOWN, false ) then iWindow.FFocusInfo.Current := Max( iWindow.FFocusInfo.Count - 1, 0 );

  if iWindow.FFocusInfo.Count = 0 then
    iWindow.FFocusInfo.Current := 0
  else
  begin
    iWindow.FFocusInfo.Current := (iWindow.FFocusInfo.Current + iWindow.FFocusInfo.Count) mod iWindow.FFocusInfo.Count;
    if iWindow.FFocusInfo.Current <> iLast then
      GCtx.IO.PlaySound( VTIG_SOUND_CHANGE );
  end;

  iWindow.FFocusInfo.Count := 0;
  GCtx.MouseCaptured       := False;
  GCtx.WindowOrder.Resize(1);
  GCtx.DrawData.FCursor.CType := VTIG_CTNONE;
end;

procedure VTIG_EndFrame;
var iWindow : TTIGWindow;
begin
  Assert( GCtx.WindowStack.Size = 1, 'Window stack size mismatch!' );
  // render
  GCtx.DrawData.FLists.Clear;
  for iWindow in GCtx.WindowOrder do
    GCtx.DrawData.FLists.Push( iWindow.DrawList );
  GCtx.Io.EndFrame;
end;

procedure VTIG_Render;
begin
  GCtx.Io.Render( GCtx.DrawData );
end;

procedure VTIG_Clear;
begin
  GCtx.Io.Clear;
end;

procedure VTIG_Begin( aName : Ansistring ); overload;
begin
  VTIG_Begin( aName, Point( -1, -1 ), Point( -1, -1 ) )
end;

procedure VTIG_Begin( aName : Ansistring; aSize : TIOPoint ); overload;
begin
  VTIG_Begin( aName, aSize, Point( -1, -1 ) )
end;

procedure VTIG_Begin( aName : Ansistring; aSize : TIOPoint; aPos : TIOPoint ); overload;
var iParent : TTIGWindow;
    iWindow : TTIGWindow;
    iFirst  : Boolean;
    iFClip  : TIORect;
    iFrame  : Ansistring;
    iCmd    : TTIGDrawCommand;
begin
  iParent := GCtx.Current;
  iWindow := GCtx.WindowStore.Get( aName, nil );

  if iWindow = nil then
  begin
    iFirst  := True;
    iWindow := TTIGWindow.Create;
    GCtx.Windows.Push( iWindow );
    GCtx.WindowStore[ aName ] := iWindow;
  end
  else
  begin
    iFirst         := iWindow.FReset;
    iWindow.FReset := False;
  end;

  if iFirst then
  begin
    iWindow.FScroll       := 0;
    iWindow.FSelectScroll := 0;
    iWindow.FMaxSize      := Point( -1, -1 );
    iWindow.FColor        := GCtx.Style^.Color[ VTIG_TEXT_COLOR ];
  end;
  iWindow.FBackground     := GCtx.Style^.Color[ VTIG_BACKGROUND_COLOR ];

  GCtx.WindowStack.Push( iWindow );
  GCtx.WindowOrder.Push( iWindow );
  GCtx.Current := iWindow;

  if ( aSize.X = -1 ) and ( iWindow.FMaxSize.X >= 0 ) then aSize.X := iWindow.FMaxSize.X + 4;
  if ( aSize.Y = -1 ) and ( iWindow.FMaxSize.Y >= 0 ) then aSize.Y := iWindow.FMaxSize.Y + 4;
  if ( aSize.X < -1 ) and ( iWindow.FMaxSize.X >= 0 ) then aSize.X := Max( iWindow.FMaxSize.X + 4, -aSize.X );
  if ( aSize.Y < -1 ) and ( iWindow.FMaxSize.Y >= 0 ) then aSize.Y := Max( iWindow.FMaxSize.Y + 4, -aSize.Y );

  if aPos.X = -1 then aPos.X := iParent.FClipContent.X + ( iParent.FClipContent.Dim.X - aSize.X ) div 2;
  if aPos.Y = -1 then aPos.Y := iParent.FClipContent.Y + ( iParent.FClipContent.Dim.Y - aSize.Y ) div 2;

  if aPos.X < -1 then aPos.X += iParent.FClipContent.X2;
  if aPos.Y < -1 then aPos.Y += iParent.FClipContent.Y2;

  iFClip := Rectangle( aPos, Max( aSize, Point(0,0) ) );
  iFrame := GCtx.Style^.Frame[ VTIG_BORDER_FRAME ];

  if iFrame = ''
    then iWindow.DC.FClip := iFClip
    else iWindow.DC.FClip := iFClip.Shrinked(1);
  iWindow.FClipContent := iWindow.DC.FClip.Shrinked(1);

  Inc( iWindow.FClipContent.Dim.Y );

  iWindow.DC.FContent := iWindow.FClipContent;
  iWindow.DC.FContent.Pos.y -= iWindow.FScroll;
  iWindow.DC.FContent.Dim.y += iWindow.FScroll;
  iWindow.DC.FCursor  := iWindow.DC.FContent.Pos;

  GCtx.BGColor := iWindow.FBackground;
  GCtx.Color   := iWindow.FColor;

  if ( aSize.X > -1 ) and ( aSize.Y > -1 ) then
  begin
    FillChar( iCmd, Sizeof( iCmd ), 0 );
    iCmd.CType := VTIG_CMD_CLEAR;
    iCmd.Clip  := iFClip;
    iCmd.Area  := iFClip;
    iCmd.FG    := GCtx.Color;
    iCmd.BG    := GCtx.BGColor;
    if iFrame <> '' then
    begin
      iCmd.CType  := VTIG_CMD_FRAME;
      iCmd.Text.X := iWindow.DrawList.FText.Size;
      iWindow.DrawList.FText.Append( PChar(iFrame), Length( iFrame ) );
      iCmd.Text.Y := iWindow.DrawList.FText.Size;
    end;
    iWindow.DrawList.FCommands.Push( iCmd );
  end;
end;

procedure VTIG_End;
begin
  Assert( GCtx.WindowStack.Size > 1, 'Too many end()''s!' );
  GCtx.WindowStack.Pop;
  GCtx.Current := GCtx.WindowStack.Top;
  GCtx.BGColor := GCtx.Current.FBackground;
end;

initialization

GDefaultContext := TTIGContext.Create;
GCtx            := GDefaultContext;

finalization

GCtx := nil;
FreeAndNil( GDefaultContext );

end.

