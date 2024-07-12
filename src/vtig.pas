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
procedure VTIG_End; overload;
procedure VTIG_End( aFooter : Ansistring ); overload;
procedure VTIG_Reset( aName : AnsiString );

procedure VTIG_BeginGroup( aSize : Integer = -1; aVertical : Boolean = False; aMaxHeight : Integer = -1 );
procedure VTIG_EndGroup;
procedure VTIG_Ruler;

procedure VTIG_BeginWindow( aName, aID : Ansistring ); overload;
procedure VTIG_BeginWindow( aName, aID : Ansistring; aSize : TIOPoint ); overload;
procedure VTIG_BeginWindow( aName, aID : Ansistring; aSize : TIOPoint; aPos : TIOPoint ); overload;
procedure VTIG_BeginWindow( aName : Ansistring ); overload;
procedure VTIG_BeginWindow( aName : Ansistring; aSize : TIOPoint ); overload;
procedure VTIG_BeginWindow( aName : Ansistring; aSize : TIOPoint; aPos : TIOPoint ); overload;

function VTIG_PositionResolve( aPos : TIOPoint ) : TIOPoint;
procedure VTIG_FreeLabel( aText : Ansistring; aPos : TIOPoint; aColor : TIOColor = 0 );
procedure VTIG_FreeLabel( aText : Ansistring; aArea : TIORect; aColor : TIOColor = 0 );
procedure VTIG_FreeLabel( aText : Ansistring; aPos : TIOPoint; aParams : array of const; aColor : TIOColor = 0 );
procedure VTIG_FreeLabel( aText : Ansistring; aArea : TIORect; aParams : array of const; aColor : TIOColor = 0 );
procedure VTIG_FreeChar( aChar : Char; aPos : TIOPoint; aColor : TIOColor = 0; aBGColor : TIOColor = 0 );
procedure VTIG_Text( aText : Ansistring; aColor : TIOColor = 0; aBGColor : TIOColor = 0 );
procedure VTIG_Text( aText : Ansistring; aParams : array of const; aColor : TIOColor = 0; aBGColor : TIOColor = 0 );

function VTIG_MouseCaptured : Boolean;
function VTIG_MouseInLastWindow : Boolean;
function VTIG_Event( aEvent : Integer ) : Boolean;
function VTIG_Event( aEvents : TFlags ) : Boolean;
function VTIG_EventConfirm : Boolean;
function VTIG_EventCancel : Boolean;
procedure VTIG_EventClear;

implementation

uses Math, vdebug, SysUtils, vtigcontext, vtigio, vioeventstate, viomousestate;

var GDefaultContext : TTIGContext;
    GCtx            : TTIGContext;

type TTIGStyleStack = object
private
  FColors  : array[0..15] of TIOColor; // Assuming a maximum depth of 16 nested styles
  FIndex   : Integer;
  FDefault : TIOColor;
public
  procedure Init( aDefault : TIOColor );
  procedure Push( aStyle: TIOColor );
  procedure Pop;
  function Current: TIOColor;
end;

procedure TTIGStyleStack.Init( aDefault : TIOColor );
begin
  FIndex   := -1;
  FDefault := aDefault;
  Push( aDefault );
end;

procedure TTIGStyleStack.Push( aStyle: TIOColor );
begin
  if FIndex < High(FColors) then
  begin
    Inc(FIndex);
    FColors[FIndex] := aStyle;
  end;
end;

procedure TTIGStyleStack.Pop;
begin
  if FIndex > 0 then
    Dec(FIndex);
end;

function TTIGStyleStack.Current: TIOColor;
begin
  if FIndex >= 0 then
    Result := FColors[FIndex]
  else
    Result := FDefault;
end;

procedure VTIG_RenderTextSegment( const aText: PAnsiChar; var aCurrentX, aCurrentY : Integer; aClip: TIORect; var aStyleStack: TTIGStyleStack; aParameters: array of const );
var iWindow        : TTIGWindow;
    i, iParamIndex : Integer;
    iPos, iWidth   : Integer;
    iLastSpace     : Integer;
    iSpaceLeft     : Integer;

  procedure Render( const aPart : PAnsiChar; aLength : Integer );
  var iCmd    : TTIGDrawCommand;
  begin
    FillChar( iCmd, Sizeof( iCmd ), 0 );
    iCmd.CType := VTIG_CMD_TEXT;
    iCmd.Clip  := aClip;
    iCmd.FG    := aStyleStack.Current;
    iCmd.BG    := 0; // TODO
    iCmd.Area  := Rectangle( Point( aCurrentX, aCurrentY ), aClip.Pos2 );
    iCmd.Text  := iWindow.DrawList.PushText( aPart, aLength );
    iWindow.DrawList.Push( iCmd );
    aCurrentX += aLength;
  end;

  procedure HandleParameter(aParameterIndex: Integer);
  var
    iParamStr    : PAnsiChar;
  begin
    if ( aParameterIndex >= 0) and ( aParameterIndex < Length(aParameters) ) then
    begin
      case aParameters[aParameterIndex].VType of
        vtAnsiString:
          begin
            iParamStr := PAnsiChar(AnsiString(aParameters[aParameterIndex].VAnsiString));
            VTIG_RenderTextSegment( iParamStr, aCurrentX, aCurrentY, aClip, aStyleStack, aParameters );
          end;
        // Add handling for other parameter types if needed
      end;
    end;
  end;


begin
  if aCurrentY > aClip.y2 then Exit;
  iWindow   := GCtx.Current;
  i         := 0;
  while aText[i] <> #0 do
  begin
    if aText[i] = '{' then
    begin
      Inc(i);
      if i <= Length(aText) then
      begin
        case aText[i] of
          'r' : aStyleStack.Push(Red);
          'R' : aStyleStack.Push(LightRed);
          'b' : aStyleStack.Push(Blue);
          'B' : aStyleStack.Push(LightBlue);
          'g' : aStyleStack.Push(Green);
          'G' : aStyleStack.Push(LightGreen);
          'v' : aStyleStack.Push(Magenta);
          'V' : aStyleStack.Push(LightMagenta);
          'c' : aStyleStack.Push(Cyan);
          'C' : aStyleStack.Push(LightCyan);
          'l' : aStyleStack.Push(LightGray);
          'L' : aStyleStack.Push(White);
          'd' : aStyleStack.Push(DarkGray);
          'D' : aStyleStack.Push(Black);
      'n','N' : aStyleStack.Push(Brown);
      'y','Y' : aStyleStack.Push(Yellow);
          '!' : aStyleStack.Push(GCtx.Style^.Color[VTIG_BOLD_COLOR]);
          '0'..'9':
            begin
              iParamIndex := Ord(aText[i]) - Ord('0');
              HandleParameter( iParamIndex );
            end;
        end;
        Inc(i);
      end;
    end
    else if aText[i] = '}' then
    begin
      aStyleStack.Pop;
      Inc(i);
    end
    else if aText[i] = #10 then // Handle newline
    begin
      aCurrentX := aClip.X;
      Inc(aCurrentY);
      Inc(i);
    end
    else
    begin
      // Reset line width and last space
      iWidth     := 0;
      iLastSpace := -1;
      iPos       := i;
      iSpaceLeft := aClip.x2 - aCurrentX;

      while iPos <= Length(aText) do
      begin
        if aText[iPos] = ' ' then
          iLastSpace := iWidth;
        if (aText[iPos] in [#10,#13,'{','}']) or (iWidth > iSpaceLeft) then
          break;
        Inc(iPos);
        Inc(iWidth);
      end;

      if ( iPos > Length(aText) ) or (aText[iPos] in [#10,#13,'{','}']) then
      begin
        if iPos > Length(aText) then
        begin
          Render( aText + i, iWidth - 1 );
          Exit; // nothing more to render, exit
        end;
        Render( aText + i, iWidth );
        i := iPos;
      end
      else // iWidth >= iSpaceLeft
      begin
        if iLastSpace > -1 then
        begin
          Render( aText + i, iLastSpace );
          aCurrentX := aClip.X;
          Inc(aCurrentY);
          i := iLastSpace + 1;
        end
        else
        // If there was no space, break at the line width
        begin
          Render( aText + i, iWidth );
          aCurrentX := aClip.X;
          Inc(aCurrentY);
          i := i + iWidth;
        end;

        // Stop if we've exceeded the clipping rectangle's height
        if aCurrentY > aClip.y2 then
          Exit;
      end;
    end;
  end;
end;

function VTIG_RenderText(const aText: AnsiString; aPosition: TIOPoint; aClip: TIORect; aParameters: array of const) : TIOPoint;
var iCurrentX, iCurrentY : Integer;
    iStyleStack          : TTIGStyleStack;
begin
  iCurrentX := aPosition.X;
  iCurrentY := aPosition.Y;
  iStyleStack.Init( GCtx.Color ); // Initialize the style stack
  VTIG_RenderTextSegment( PAnsiChar(aText), iCurrentX, iCurrentY, aClip, iStyleStack, aParameters );
  Exit( Point( iCurrentX, iCurrentY ) );
end;

procedure VTIG_RenderChar( aChar : Char; aPosition : TIOPoint );
var iCmd    : TTIGDrawCommand;
    iWindow : TTIGWindow;
    iClip   : TIORect;
begin
  iWindow := GCtx.Current;
  iClip   := iWindow.FClipContent;
  FillChar( iCmd, Sizeof( iCmd ), 0 );
  iCmd.CType := VTIG_CMD_TEXT;
  iCmd.Clip  := iClip;
  iCmd.Area  := Rectangle( aPosition, iClip.Dim - aPosition );
  iCmd.FG    := GCtx.Color;
  iCmd.BG    := GCtx.BGColor;
  iCmd.Text  := iWindow.DrawList.PushChar( aChar );
  iWindow.DrawList.Push( iCmd );
end;

procedure ClampTo( var aRect : TIORect; aClip : TIORect );
begin
  aRect.Pos := Max( aRect.Pos, aClip.Pos );
  aRect.Dim := Max( Min( aRect.pos2, aClip.pos2 ) - aRect.Pos + Point(1,1), Point(1,1) );
end;

procedure VTIG_Initialize( aRenderer : TIOConsoleRenderer; aDriver : TIODriver; aClearOnRender : Boolean = True );
var iCanvas : TTIGWindow;
begin
  Assert( GCtx.Io.Driver = nil, 'TIG reinitialized' );
  GCtx.Io.Initialize( aRenderer, aDriver, aClearOnRender );
  GCtx.Size    := Point( aRenderer.SizeX, aRenderer.SizeY );
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
  GCtx.Time := GCtx.Io.Driver.GetMs;
end;

procedure VTIG_NewFrame;
var iWindow : TTIGWindow;
    iTime   : DWord;
    iLast   : Integer;
    iES     : TIOEventState;
begin
  GCtx.Size := GCtx.Io.Size;

  GCtx.Current.FClipContent := Rectangle( Point(1,1), GCtx.Size );
  GCtx.Current.DC.FContent  := GCtx.Current.FClipContent;
  GCtx.Current.DC.FClip     := GCtx.Current.FClipContent;
  GCtx.Current.DC.FCursor   := GCtx.Current.DC.FContent.Pos;

  for iWindow in GCtx.Windows do
    iWindow.DrawList.Clear;

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
  GCtx.DrawData.CursorType := VTIG_CTNONE;
end;

procedure VTIG_EndFrame;
var iWindow : TTIGWindow;
begin
  Assert( GCtx.WindowStack.Size = 1, 'Window stack size mismatch!' );
  // render
  GCtx.DrawData.Lists.Clear;
  for iWindow in GCtx.WindowOrder do
    GCtx.DrawData.Lists.Push( iWindow.DrawList );
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
      iCmd.Text   := iWindow.DrawList.PushText( PChar(iFrame), Length( iFrame ) );
    end;
    iWindow.DrawList.Push( iCmd );
  end;
end;

procedure VTIG_End;
begin
  Assert( GCtx.WindowStack.Size > 1, 'Too many end()''s!' );
  GCtx.WindowStack.Pop;
  GCtx.Current := GCtx.WindowStack.Top;
  GCtx.BGColor := GCtx.Current.FBackground;
end;

procedure VTIG_End( aFooter : Ansistring );
var iClip : TIORect;
    iPos  : TIOPoint;
begin
  Assert( GCtx.WindowStack.Size > 1, 'Too many end()''s!' );
  iClip := GCtx.Current.DC.FClip.Expanded( 1 );
  GCtx.Color   := GCtx.Current.FColor;
  GCtx.BGColor := GCtx.Current.FBackground;
  iPos := iClip.BottomRight;
  iPos.X -= Length( aFooter ) + 5;
  // shall we worry about string alloc?
  VTIG_RenderText( '[ ' + aFooter + ' ]', iPos, iClip, [] );
//  VTIG_RenderText( '[ {1} ]', iPos, iClip, [aFooter] );
  VTIG_End;
end;

procedure VTIG_Reset( aName : Ansistring );
var iWindow : TTIGWindow;
begin
  iWindow := GCtx.WindowStore.Get( aName, nil );
  if Assigned( iWindow ) then
    iWindow.FReset := True;
end;

procedure VTIG_BeginGroup( aSize : Integer = -1; aVertical : Boolean = False; aMaxHeight : Integer = -1 );
var iWindow : TTIGWindow;
    iHeight : Integer;
    iCmd    : TTIGDrawCommand;
    iFrame  : AnsiString;
begin
  iWindow := GCtx.Current;
  if (not aVertical) and (aSize <> -1) then
  begin
    iHeight := iWindow.FClipContent.Y2 - (iWindow.DC.FCursor.y - 1);
    if aMaxHeight >= 0 then
      iHeight := Min( aMaxHeight, iHeight );
    iCmd.CType := VTIG_CMD_RULER;
    iCmd.Area  := Rectangle(
      Point( iWindow.DC.FCursor.X + aSize, iWindow.DC.FCursor.Y - 1 ),
      Point( 1, iHeight + 1 )
    );
    ClampTo( iCmd.Area, iWindow.DC.FClip );
    iCmd.FG := iWindow.FColor;
    iCmd.BG := iWindow.FBackground;
    iFrame  := GCtx.Style^.Frame[ VTIG_RULER_FRAME ];
    iCmd.Text := iWindow.DrawList.PushText( PChar(iFrame), Length( iFrame ) );

    iWindow.DrawList.Push( iCmd );
  end;
  iWindow.DC.BeginGroup( aSize, aVertical );
end;

procedure VTIG_EndGroup;
begin
  GCtx.Current.DC.EndGroup;
end;

procedure VTIG_Ruler;
var iWindow : TTIGWindow;
    iCmd    : TTIGDrawCommand;
    iFrame  : Ansistring;
begin
  iWindow := GCtx.Current;
  FillChar( iCmd, Sizeof( iCmd ), 0 );
  iCmd.CType := VTIG_CMD_RULER;
  iCmd.Area  := Rectangle(
    Point( iWindow.DC.FContent.Pos.X, iWindow.DC.FCursor.Y + 1 ),
    Point( iWindow.DC.FContent.Dim.X, 1 )
  );

  ClampTo( iCmd.Area, iWindow.DC.FClip );
  iCmd.FG := iWindow.FColor;
  iCmd.BG := iWindow.FBackground;

  if ( iWindow.DC.FCursor.Y + 1 <= iWindow.DC.FClip.y2 )
    and ( iWindow.DC.FCursor.Y + 1 >= iWindow.DC.FClip.y ) then
  begin
    iFrame    := GCtx.Style^.Frame[ VTIG_RULER_FRAME ];
    iCmd.Text := iWindow.DrawList.PushText( PChar(iFrame), Length( iFrame ) );
    iWindow.DrawList.Push( iCmd );
  end;
  iWindow.DC.FCursor.Y += 3;
end;

procedure VTIG_BeginWindow( aName, aID : Ansistring ); overload;
begin
  VTIG_BeginWindow( aName, aID, Point( -1, -1 ), Point( -1, -1 ) );
end;

procedure VTIG_BeginWindow( aName, aID : Ansistring; aSize : TIOPoint ); overload;
begin
  VTIG_BeginWindow( aName, aID, aSize, Point( -1, -1 ) );
end;

procedure VTIG_BeginWindow( aName, aID : Ansistring; aSize : TIOPoint; aPos : TIOPoint ); overload;
var iClip : TIORect;
    iPos  : TIOPoint;
begin
  if aID = '' then aID := aName;
  aSize.X := Min( aSize.X, GCtx.Size.X );
  VTIG_Begin( aID, aSize, aPos );
  iClip := GCtx.Current.DC.FClip.Expanded( 1 );
  GCtx.Color   := GCtx.Current.FColor;
  GCtx.BGColor := GCtx.Current.FBackground;
  iPos := iClip.Pos;
  iPos.X += iClip.w - ( Length( aName ) + 2 ) div 2;
  // shall we worry about string alloc?
  VTIG_RenderText( ' ' + aName + ' ', iPos, iClip, [] );
//  VTIG_RenderText( ' {1} ', iPos, iClip, [aName] );
end;

procedure VTIG_BeginWindow( aName : Ansistring ); overload;
begin
  VTIG_BeginWindow( aName, '', Point( -1, -1 ), Point( -1, -1 ) );
end;

procedure VTIG_BeginWindow( aName : Ansistring; aSize : TIOPoint ); overload;
begin
  VTIG_BeginWindow( aName, '', aSize, Point( -1, -1 ) );
end;

procedure VTIG_BeginWindow( aName : Ansistring; aSize : TIOPoint; aPos : TIOPoint ); overload;
begin
  VTIG_BeginWindow( aName, '', aSize, aPos );
end;

function VTIG_PositionResolve( aPos : TIOPoint ) : TIOPoint;
var iClip   : TIORect;
begin
  iClip   := GCtx.Current.DC.FContent;
  Result  := iClip.Pos + aPos;
  if aPos.x < 0 then Result.x += iClip.Dim.X;
  if aPos.y < 0 then Result.y += iClip.Dim.Y;
end;

function VTIG_GetClipRect : TIORect;
var iWindow : TTIGWindow;
begin
  iWindow := GCtx.Current;
  Result := iWindow.DC.FContent;
  Result.Dim.Y := iWindow.FClipContent.Dim.Y;
  ClampTo( Result, iWindow.DC.FClip );
end;

procedure VTIG_FreeLabel( aText : Ansistring; aPos : TIOPoint; aColor : TIOColor = 0 );
begin
  VTIG_FreeLabel( aText, aPos, [], aColor );
end;

procedure VTIG_FreeLabel( aText : Ansistring; aArea : TIORect; aColor : TIOColor = 0 );
begin
  VTIG_FreeLabel( aText, aArea, [], aColor );
end;

procedure VTIG_FreeLabel( aText : Ansistring; aPos : TIOPoint; aParams : array of const; aColor : TIOColor = 0 );
var iClip  : TIORect;
    iStart : TIOPoint;
begin
  if aColor = 0 then aColor := GCtx.Style^.Color[ VTIG_TEXT_COLOR ];
  GCtx.Color   := aColor;
  GCtx.BGColor := GCtx.Current.FBackground;
  iClip  := GCtx.Current.DC.FClip;
  iStart := VTIG_PositionResolve( aPos );
  iClip.Pos.X := iStart.X;
  iClip.Dim.X -= ( iStart.X - GCtx.Current.DC.FClip.Pos.X );
  if ( iStart.X > iClip.x2 ) or ( iStart.Y > iClip.y2 ) then Exit;
  VTIG_RenderText( aText, iStart, iClip, aParams );
end;

procedure VTIG_FreeLabel( aText : Ansistring; aArea : TIORect; aParams : array of const; aColor : TIOColor = 0 );
var iClip  : TIORect;
    iStart : TIOPoint;
begin
  if aColor = 0 then aColor := GCtx.Style^.Color[ VTIG_TEXT_COLOR ];
  GCtx.Color   := aColor;
  GCtx.BGColor := GCtx.Current.FBackground;
  iClip  := aArea;
  iStart := VTIG_PositionResolve( aArea.Pos );
  iClip.Pos.X := iStart.X;
  iClip.Dim.X -= ( iStart.X - aArea.Pos.X );
  if ( iStart.X > iClip.x2 ) or ( iStart.Y > iClip.y2 ) then Exit;
  VTIG_RenderText( aText, iStart, iClip, aParams );
end;

procedure VTIG_FreeChar( aChar : Char; aPos : TIOPoint; aColor : TIOColor = 0; aBGColor : TIOColor = 0 );
begin
  if aColor = 0   then aColor   := GCtx.Style^.Color[ VTIG_TEXT_COLOR ];
  if aBGColor = 0 then aBGColor := GCtx.Style^.Color[ VTIG_BACKGROUND_COLOR ];
  GCtx.Color   := aColor;
  GCtx.BGColor := aBGColor;
  VTIG_RenderChar( aChar, aPos );
end;

procedure VTIG_Text( aText : Ansistring; aParams : array of const; aColor : TIOColor = 0; aBGColor : TIOColor = 0 );
var iClip  : TIORect;
    iStart : TIOPoint;
    iCoord : TIOPoint;
begin
  if aColor = 0   then aColor   := GCtx.Style^.Color[ VTIG_TEXT_COLOR ];
  if aBGColor = 0 then aBGColor := GCtx.Style^.Color[ VTIG_BACKGROUND_COLOR ];
  GCtx.Color   := aColor;
  GCtx.BGColor := aBGColor;
  iClip  := VTIG_GetClipRect;
  iStart := GCtx.Current.DC.FCursor;
  if ( iStart.X > iClip.x2 ) then Exit;
  iCoord := VTIG_RenderText( aText, iStart, iClip, aParams );
  GCtx.Current.Advance( iCoord - iStart + Point(1,1) );
end;

procedure VTIG_Text( aText : Ansistring; aColor : TIOColor = 0; aBGColor : TIOColor = 0 );
begin
  VTIG_Text( aText, [], aColor, aBGColor );
end;

function VTIG_MouseCaptured : Boolean;
begin
  Result := GCtx.MouseCaptured;
end;

function VTIG_MouseInLastWindow : Boolean;
var iMouseState : TIOMouseState;
    iTop        : TTIGWindow;
begin
  iMouseState := GCtx.Io.MouseState;
  if iMouseState.Position = Point( -1, -1 ) then Exit( False );
  iTop := GCtx.WindowOrder.Top;
  Exit( iMouseState.Position in iTop.FClipContent.Expanded(2) );
end;

function VTIG_Event( aEvent : Integer ) : Boolean;
begin
  Result := GCtx.IO.EventState.Activated( aEvent );
end;

function VTIG_Event( aEvents : TFlags ) : Boolean;
begin
  Result := GCtx.IO.EventState.Activated( aEvents );
end;

function VTIG_EventConfirm : Boolean;
begin
  Result := GCtx.IO.EventState.Activated( VTIG_IE_CONFIRM )
         or ( GCtx.IO.EventState.Activated( VTIG_IE_MCONFIRM ) and VTIG_MouseInLastWindow );
end;

function VTIG_EventCancel : Boolean;
begin
  Result := GCtx.IO.EventState.Activated( VTIG_IE_CANCEL );
end;

procedure VTIG_EventClear;
begin
  GCtx.IO.EventState.Clear;
end;


initialization

GDefaultContext := TTIGContext.Create;
GCtx            := GDefaultContext;

finalization

GCtx := nil;
FreeAndNil( GDefaultContext );

end.

