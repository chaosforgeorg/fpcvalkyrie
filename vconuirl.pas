{$INCLUDE valkyrie.inc}
unit vconuirl;
interface

uses Classes, SysUtils, vconui, vutil, viotypes, vioevent, vioconsole, vuitypes,
     vuielement, vuielements, vuiconsole, vrltools, vgenerics, vvision, vanimation;

type TConUIHybridMenu = class( TConUIMenu )
  procedure Add( aItem : TUIMenuItem ); override;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  procedure OnRedraw; override;
end;

type TConUITextMenu = class( TConUIMenu )
  procedure Add( aItem : TUIMenuItem ); override;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  function OnMouseMove( const event : TIOMouseMoveEvent ) : Boolean; override;
  function OnMouseDown ( const event : TIOMouseEvent ) : Boolean; override;
end;

type TConUIBarFullWindow = class( TUIElement )
  constructor Create( aParent : TUIElement; const aTitle, aFooter : TUIString );
  procedure SetStyle( aStyle : TUIStyle ); override;
  procedure OnRedraw; override;
  function OnCancel : Boolean; virtual;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  function OnMouseDown( const event : TIOMouseEvent ) : Boolean; override;
protected
  FOnCancel    : TUINotifyEvent;
  FTitle       : TUIString;
  FFooter      : TUIString;
  FFrameChars  : AnsiString;
  FFrameColor  : TUIColor;
  FTitleColor  : TUIColor;
  FFooterColor : TUIColor;
public
  property OnCancelEvent : TUINotifyEvent write FOnCancel;
  property Footer : TUIString write FFooter;
end;

type TConUIPlotViewer = class( TUIElement )
  constructor Create( aParent : TUIElement; const aText : AnsiString; const aTextArea : TUIRect );
  procedure OnRedraw; override;
  procedure OnUpdate( aTime : DWord ); override;
  function OnKeyDown( const event : TIOKeyEvent ) : Boolean; override;
  function OnMouseDown( const event : TIOMouseEvent ) : Boolean; override;
protected
  FText      : TUIString;
  FTextArea  : TUIRect;
  FChunks    : TUIChunkList;
  FCharCount : DWord;
  FCharMax   : Integer;
  FStartTime : DWord;
  FSkip      : Boolean;
end;

type IConUIASCIIMap = interface
  function getGylph( const aCoord : TCoord2D ) : TIOGylph;
end;

type TConUIMapArea = class;

{ TConUIAnimation }

TConUIAnimation = class( TAnimation )
  constructor Create( aDuration : DWord; aDelay : DWord );
protected
  FMap      : TConUIMapArea;
end;

type TConUIAnimations = specialize TGObjectArray< TAnimation >;

type

{ TConUIMapArea }

 TConUIMapArea = class( TUIElement )
  constructor Create( aParent : TUIElement; aMap : IConUIASCIIMap = nil );
  constructor Create( aParent : TUIElement; aArea : TUIRect; aMap : IConUIASCIIMap = nil );
  destructor Destroy; override;
  procedure SetCenter( aCoord : TCoord2D );
  procedure SetShift( aShift : TUIPoint );
  procedure SetMap( aMap : IConUIASCIIMap );
  procedure Mark( aCoord : TCoord2D; aSign : char; aColor : TIOColor );
  procedure FreezeMarks;
  procedure ClearMarks;
  procedure ClearMark( aCoord : TCoord2D );
  procedure OnRedraw; override;
  procedure OnUpdate( aTime : DWord ); override;
  procedure AddAnimation( aAnimation : TAnimation );
  procedure ClearAnimations;
  function AnimationsFinished : Boolean;
  function Screen( aWorld : TCoord2D ) : TUIPoint; inline;
  function World( aScreen : TUIPoint ) : TCoord2D;  inline;
protected
  FMarkMap    : array of array of TIOGylph;
  FAnimations : TAnimations;
  FLength     : TUIPoint;
  FMap        : IConUIASCIIMap;
  FShift      : TUIPoint;
  FConsole    : TUIConsole;
public
  property Console : TUIConsole read FConsole;
  property Shift : TUIPoint read FShift write SetShift;
end;

type TConUIMarkAnimation = class(TConUIAnimation)
  constructor Create( aWhere : TCoord2D; aGylph : TIOGylph; aDuration : DWord; aDelay : DWord = 0 );
  procedure OnDraw; override;
protected
  FWhere : TCoord2D;
  FGylph : TIOGylph;
end;

type TConUIBlinkAnimation = class(TConUIAnimation)
  constructor Create( aGylph : TIOGylph; aDuration : DWord; aDelay : DWord = 0 );
  procedure OnDraw; override;
protected
  FGylph : TIOGylph;
end;

type TConUIBulletAnimation = class(TConUIAnimation)
  constructor Create( aMap : IVisionQuery; aSource, aTarget : TCoord2D; aGylph : TIOGylph; aDuration : DWord; aDelay : DWord; aVisionRange : Word = 0 );
  procedure OnDraw; override;
protected
  FRay      : TVisionRay;
  FDistance : DWord;
  FMaxDist  : DWord;
  FGylph    : TIOGylph;
  FRange    : Word;
end;

type TConUIRayAnimation = class(TConUIAnimation)
  constructor Create( aMap : IVisionQuery; aSource, aTarget : TCoord2D; aGylph : TIOGylph; aDuration : DWord; aDelay : DWord; aVisionRange : Word = 0  );
  procedure OnDraw; override;
protected
  FMapQuery : IVisionQuery;
  FSource   : TCoord2D;
  FTarget   : TCoord2D;
  FMaxDist  : DWord;
  FGylph    : TIOGylph;
  FRange    : Word;
end;

type TConUIClearMarkAnimation = class(TConUIAnimation)
  constructor Create( aDelay : DWord = 0 );
  procedure OnDraw; override;
end;

type TConUIExplosionArray = array of record Color : TIOColor; Time : DWord; end;
type

{ TConUIExplosionAnimation }

 TConUIExplosionAnimation = class(TConUIAnimation)
  constructor Create( aWhere : TCoord2D; aChar : Char; const aArray : TConUIExplosionArray; aDelay : DWord = 0 );
  procedure OnDraw; override;
  destructor Destroy; override;
protected
  FArray    : TConUIExplosionArray;
  FCount    : DWord;
  FTCount   : DWord;
  FWhere    : TCoord2D;
  FGylph    : Char;
end;


implementation

uses math;

const Letters = 'abcdefghijklmnopqrstuvwxyz';

{ TConUITextMenu }

procedure TConUITextMenu.Add(aItem: TUIMenuItem);
begin
  aItem.Text := '[@<'+Letters[FCount+1]+'@>] '+ aItem.Text;
  inherited Add ( aItem ) ;
  FSelected := 0;
end;

function TConUITextMenu.OnKeyDown(const event: TIOKeyEvent): Boolean;
var iSelection : Byte;
begin
  if (FCount > 0) and (event.ASCII in ['a'..Letters[FCount]]) then
  begin
    iSelection := (Ord(event.ASCII) - Ord('a')) + 1;
    SetSelected( iSelection );
    if FSelected = iSelection then OnConfirm;
    Exit( True );
  end;
  if event.Code = VKEY_ESCAPE
    then Exit( inherited OnKeyDown ( event ) )
    else Exit( False );
end;

function TConUITextMenu.OnMouseMove(const event: TIOMouseMoveEvent): Boolean;
begin
  Exit( False );
end;

function TConUITextMenu.OnMouseDown(const event: TIOMouseEvent): Boolean;
begin
  Exit( False );
end;

{ TConUIClearMarkAnimation }

constructor TConUIClearMarkAnimation.Create ( aDelay : DWord ) ;
begin
  inherited Create( 1, aDelay );
end;

procedure TConUIClearMarkAnimation.OnDraw;
begin
  FMap.ClearMarks;
end;

{ TConUIExplosionAnimation }

constructor TConUIExplosionAnimation.Create ( aWhere : TCoord2D; aChar : Char;
  const aArray : TConUIExplosionArray; aDelay : DWord ) ;
var iCount : DWord;
begin
  FDuration := 0;
  for iCount := Low( aArray ) to High( aArray ) do FDuration += aArray[ iCount ].Time;
  inherited Create( FDuration, aDelay );
  FArray  := aArray;
  FWhere  := aWhere;
  FGylph  := aChar;
  FCount  := Low( FArray );
  FTCount := 0;
end;

procedure TConUIExplosionAnimation.OnDraw;
begin
  if FDuration = 0 then Exit;
  while (FCount < High( FArray )) and ( FTime - FTCount > FArray[FCount].Time ) do
  begin
    FTCount += FArray[FCount].Time;
    Inc( FCount );
  end;
  FMap.Console.DrawChar( FMap.Screen(FWhere), FArray[FCount].Color, FGylph );
end;

destructor TConUIExplosionAnimation.Destroy;
begin
  FMap.ClearMark( FWhere );
  inherited Destroy;
end;

{ TConUIBulletAnimation }

constructor TConUIBulletAnimation.Create ( aMap : IVisionQuery; aSource, aTarget : TCoord2D;
  aGylph : TIOGylph; aDuration : DWord; aDelay : DWord; aVisionRange : Word = 0 ) ;
begin
  inherited Create( aDuration, aDelay );
  FRay.Init( aMap, aSource, aTarget );
  FDistance := 0;
  FGylph    := aGylph;
  FMaxDist  := (aSource - aTarget).LargerLength;
  FRange    := aVisionRange;
end;

procedure TConUIBulletAnimation.OnDraw;
var iRatio : Single;
    iChar  : Char;
begin
  if FDuration = 0 then Exit;
  if (FRange > 0) and (Distance( FRay.GetSource, FRay.GetC ) > FRange) then
  begin
    FTime := FDuration;
    Exit;
  end;
  iRatio := Round( FTime / FDuration * FMaxDist );
  while FDistance < iRatio do
  begin
    Inc( FDistance );
    if not FRay.Done then FRay.Next;
  end;
  iChar := FGylph.ASCII;
  if iChar = '-' then iChar := NewDirection(FRay.GetPrev,FRay.GetC).Picture;
  if not FRay.Map.blocksVision( FRay.GetC )
    then FMap.Console.DrawChar( FMap.Screen(FRay.GetC), FGylph.Color, iChar );
end;

constructor TConUIRayAnimation.Create ( aMap : IVisionQuery; aSource, aTarget : TCoord2D;
  aGylph : TIOGylph; aDuration : DWord; aDelay : DWord; aVisionRange : Word = 0 ) ;
begin
  inherited Create( aDuration, aDelay );
  FMapQuery := aMap;
  FSource   := aSource;
  FTarget   := aTarget;
  FGylph    := aGylph;
  FMaxDist  := (aSource - aTarget).LargerLength;
  FRange    := aVisionRange;
end;

procedure TConUIRayAnimation.OnDraw;
var iRay   : TVisionRay;
    iDist  : DWord;
    iChar  : Char;
begin
  iRay.Init( FMapQuery, FSource, FTarget );
  iDist := 0;
  while not iRay.Done do
  begin
    iRay.Next;
    Inc( iDist );
    if FMapQuery.blocksVision( iRay.GetC ) then Break;
    if iDist > FMaxDist then Break;
    if (FRange > 0) and (Distance( FSource, iRay.GetC ) > FRange) then Break;
    iChar := FGylph.ASCII;
    if iChar = '-' then iChar := NewDirection(iRay.GetPrev,iRay.GetC).Picture;
    FMap.Console.DrawChar( FMap.Screen(iRay.GetC), FGylph.Color, iChar );
  end;
end;

{ TConUIBlinkAnimation }

constructor TConUIBlinkAnimation.Create ( aGylph : TIOGylph; aDuration : DWord;
  aDelay : DWord ) ;
begin
  inherited Create( aDuration, aDelay );
  FGylph := aGylph;
end;

procedure TConUIBlinkAnimation.OnDraw;
var iPoint : TUIPoint;
begin
  with FMap.Console do
  for iPoint in FMap.AbsDim do
    DrawChar( iPoint, FGylph.Color, FGylph.ASCII );
end;

{ TConUIMarkAnimation }

constructor TConUIMarkAnimation.Create ( aWhere : TCoord2D; aGylph : TIOGylph; aDuration : DWord; aDelay : DWord ) ;
begin
  inherited Create( aDuration, aDelay );
  FWhere := aWhere;
  FGylph := aGylph;
end;

procedure TConUIMarkAnimation.OnDraw;
begin
  FMap.Console.DrawChar( FMap.Screen(FWhere), FGylph.Color, FGylph.ASCII );
end;

{ TConUIHybridMenu }

procedure TConUIHybridMenu.Add ( aItem : TUIMenuItem ) ;
begin
  aItem.Text := '[@<'+Letters[FCount+1]+'@>] '+ aItem.Text;
  inherited Add ( aItem ) ;
end;

function TConUIHybridMenu.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
var iSelection : Byte;
begin
  if (FCount > 0) and (event.ASCII in ['a'..Letters[FCount]]) then
  begin
    iSelection := (Ord(event.ASCII) - Ord('a')) + 1;
    SetSelected( iSelection );
    if FSelected = iSelection then OnConfirm;
    Exit( True );
  end;
  Result := inherited OnKeyDown ( event ) ;
end;

procedure TConUIHybridMenu.OnRedraw;
var iCon      : TUIConsole;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  if FSelected > 0 then
    iCon.DrawChar( FAbsolute.Pos + Point(-2, FSelected-1-FScroll),  FSelectedColor, '>' );
end;

constructor TConUIBarFullWindow.Create ( aParent : TUIElement; const aTitle,
  aFooter : TUIString ) ;
begin
  inherited Create( aParent, aParent.GetDimRect );
  FStyleClass := 'full_window';
  FEventFilter := [ VEVENT_KEYDOWN, VEVENT_MOUSEDOWN ];
  FFooter := aFooter;
  FTitle  := aTitle;
end;

procedure TConUIBarFullWindow.SetStyle ( aStyle : TUIStyle ) ;
begin
  inherited SetStyle ( aStyle ) ;

  FFrameChars := StyleValue[ 'frame_chars' ];
  FFrameColor := StyleValue[ 'frame_color' ];
  FTitleColor  := StyleValue[ 'title_color' ];
  FFooterColor := StyleValue[ 'footer_color' ];
end;

procedure TConUIBarFullWindow.OnRedraw;
var iCon   : TUIConsole;
    iTPos  : TUIPoint;
begin
  inherited OnRedraw;
  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, FBackColor );
  iCon.RawPrint( FAbsolute.TopLeft,    FFrameColor, StringOfChar(FFrameChars[1],FAbsolute.w+1) );
  iCon.RawPrint( FAbsolute.BottomLeft, FFrameColor, StringOfChar(FFrameChars[3],FAbsolute.w+1) );

  if FTitle <> '' then
  begin
    iTPos.Init( FAbsolute.x+(FAbsolute.w - Length( FTitle )) div 2, FAbsolute.y );
    iCon.Print( iTPos, FTitleColor, FBackColor, ' '+FTitle+' ', True );
  end;

  if FFooter <> '' then
  begin
    iTPos.Init( FAbsolute.x2-Length( FFooter )-2, FAbsolute.y2 );
    iCon.Print( iTPos, FFooterColor, FBackColor, '[ '+FFooter+' ]', True );
  end;
end;

function TConUIBarFullWindow.OnCancel : Boolean;
begin
  if Assigned( FOnCancel ) then Exit( FOnCancel( Self ) );
  Free;
  Exit( True );
end;

function TConUIBarFullWindow.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if event.ModState <> [] then Exit( inherited OnKeyDown( event ) );
  case event.Code of
    VKEY_SPACE,
    VKEY_ESCAPE,
    VKEY_ENTER  : Exit( OnCancel );
  else Exit( inherited OnKeyDown( event ) );
  end;
end;

function TConUIBarFullWindow.OnMouseDown ( const event : TIOMouseEvent ) : Boolean;
begin
  if event.Button in [ VMB_BUTTON_LEFT, VMB_BUTTON_MIDDLE, VMB_BUTTON_RIGHT ] then
    OnCancel;
  Exit( True );
end;

constructor TConUIPlotViewer.Create ( aParent : TUIElement; const aText : AnsiString; const aTextArea : TUIRect ) ;
begin
  inherited Create( aParent, aParent.GetDimRect );
  FStyleClass  := 'plot_viewer';
  FEventFilter := [ VEVENT_KEYDOWN, VEVENT_MOUSEDOWN ];

  FText      := aText;
  FTextArea  := aTextArea;
  FCharCount := 0;
  FStartTime := 0;
  FSkip      := False;
  FCharMax   := 0;
end;

procedure TConUIPlotViewer.OnRedraw;
var iCon    : TUIConsole;
    i,ii,cc : LongInt;
    iChunk  : TUIChunk;
    iPos    : TUIPoint;
begin
  inherited OnRedraw;

  if FCharMax = 0 then
  begin
    FChunks    := TConUIRoot(FRoot).Console.Chunkify( FText, FTextArea.Dim, FForeColor );
    for i := 0 to High( FChunks ) do
      FCharMax += Length( FChunks[i].Content );
  end;

  iCon.Init( TConUIRoot(FRoot).Renderer );
  iCon.ClearRect( FAbsolute, Black );
  iPos := FTextArea.Pos;
  FCharCount := Min( FStartTime div 40, FCharMax );

  cc := FCharCount;
  ii := 0;
  i  := 0;
  repeat
    iChunk := FChunks[i];
    ii := Min( Length( iChunk.Content ), cc );
    if ii < Length( iChunk.Content ) then
      iChunk.Content := Copy( iChunk.Content, 1, ii );
    iCon.RawPrint( iPos+iChunk.Position, iChunk.Color, iChunk.Content );
    cc -= ii;
    Inc(i)
  until (cc = 0) or (i > High(FChunks));
end;

procedure TConUIPlotViewer.OnUpdate( aTime : DWord );
begin
  if FSkip
    then FStartTime += 20*aTime
    else FStartTime += aTime;

  TConUIRoot( FRoot ).NeedRedraw := True;
end;

function TConUIPlotViewer.OnKeyDown ( const event : TIOKeyEvent ) : Boolean;
begin
  if (not FSkip) and (Integer(FCharCount) < FCharMax) then
    FSkip := True
  else
    if event.Code in [ VKEY_ENTER, VKEY_ESCAPE, VKEY_SPACE ] then
      Free;

  Exit( True );
end;

function TConUIPlotViewer.OnMouseDown ( const event : TIOMouseEvent ) : Boolean;
begin
  if (not FSkip) and (Integer(FCharCount) < FCharMax) then
    FSkip := True
  else
    if event.Button in [ VMB_BUTTON_LEFT, VMB_BUTTON_MIDDLE, VMB_BUTTON_RIGHT ] then
      Free;

  Exit( True );
end;

{ TConUIAnimation }

constructor TConUIAnimation.Create ( aDuration : DWord; aDelay : DWord ) ;
begin
  inherited Create( aDuration, aDelay, 0 );
  FMap      := nil;
end;

{ TConUIMapArea }

constructor TConUIMapArea.Create ( aParent : TUIElement; aMap : IConUIASCIIMap ) ;
begin
  inherited Create( aParent, aParent.GetDimRect );
  FLength.Init(0,0);
  FShift.Init(0,0);
  FAnimations := TAnimations.Create;
  SetMap(aMap);
  ClearMarks;
end;

constructor TConUIMapArea.Create ( aParent : TUIElement; aArea : TUIRect; aMap : IConUIASCIIMap ) ;
begin
  inherited Create( aParent, aArea );
  FLength.Init(0,0);
  FShift.Init(0,0);
  FAnimations := TAnimations.Create;
  SetMap(aMap);
  ClearMarks;
end;

destructor TConUIMapArea.Destroy;
begin
  FreeAndNil( FAnimations );
  inherited Destroy;
end;

procedure TConUIMapArea.SetCenter( aCoord : TCoord2D ) ;
begin
  FShift.X := aCoord.X - FAbsolute.w div 2;
  FShift.Y := aCoord.Y - FAbsolute.h div 2;
end;

procedure TConUIMapArea.SetShift ( aShift : TUIPoint ) ;
begin
  FShift := aShift;
end;

procedure TConUIMapArea.SetMap ( aMap : IConUIASCIIMap ) ;
begin
  FMap := aMap;
end;

procedure TConUIMapArea.Mark( aCoord : TCoord2D; aSign : char; aColor : TIOColor );
var iPoint : TUIPoint;
begin
  iPoint := Screen(aCoord)-FAbsolute.Pos;
  if (iPoint.X < 0) or (iPoint.Y < 0) or (iPoint.X >= FAbsolute.w) or (iPoint.Y >= FAbsolute.h) then Exit;
  FMarkMap[iPoint.x,iPoint.y].Init( aSign, aColor );
end;

procedure TConUIMapArea.FreezeMarks;
var x,y    : Integer;
    iReset : Boolean;
begin
  iReset := FLength <> FAbsolute.Dim;
  if iReset then SetLength( FMarkMap, FAbsolute.w );
  for x := 0 to FAbsolute.w-1 do
  begin
    if iReset then SetLength( FMarkMap[x],FAbsolute.h );
    for y := 0 to FAbsolute.h-1 do
       FMarkMap[x,y] := FMap.getGylph( NewCoord2D( x + FShift.x + 1, y + FShift.y + 1 ) );
  end;
  FLength := FAbsolute.Dim;
end;

procedure TConUIMapArea.ClearMarks;
var x,y    : DWord;
    iReset : Boolean;
begin
  iReset := FLength <> FAbsolute.Dim;
  if iReset then SetLength( FMarkMap, FAbsolute.w );
  for x := 0 to FAbsolute.w-1 do
  begin
    if iReset then SetLength( FMarkMap[x],FAbsolute.h );
    for y := 0 to FAbsolute.h-1 do
       FMarkMap[x,y].Color := 0;
  end;
  FLength := FAbsolute.Dim;
end;

procedure TConUIMapArea.ClearMark( aCoord: TCoord2D );
var iPoint : TUIPoint;
begin
  iPoint := Screen(aCoord)-FAbsolute.Pos;
  if (iPoint.X < 0) or (iPoint.Y < 0) or (iPoint.X >= FAbsolute.w) or (iPoint.Y >= FAbsolute.h) then Exit;
  FMarkMap[iPoint.x,iPoint.y].Color := 0;
end;

procedure TConUIMapArea.OnRedraw;
var x,y    : Word;
  procedure DrawTile( aScreen : TUIPoint );
  var iCoord   : TCoord2D;
      iPicture : TIOGylph;
  begin
    iCoord   := World( aScreen );
    iPicture := FMap.getGylph( iCoord );
    FConsole.DrawChar( aScreen, iPicture.Color, iPicture.ASCII );
    iPicture := FMarkMap[aScreen.x-FAbsolute.Pos.x,aScreen.y-FAbsolute.Pos.y];
    if iPicture.Color <> 0 then
      FConsole.DrawChar( aScreen, iPicture.Color, iPicture.ASCII );
  end;
begin
  inherited OnRedraw;
  FConsole.Init( TConUIRoot(FRoot).Renderer );
  for x := FAbsolute.pos.x to FAbsolute.pos.x+FAbsolute.dim.x-1 do
    for y := FAbsolute.pos.y to FAbsolute.pos.y+FAbsolute.dim.y-1 do
      DrawTile(Point(x,y));
  FAnimations.Draw;
end;

procedure TConUIMapArea.OnUpdate ( aTime : DWord ) ;
begin
  inherited OnUpdate ( aTime ) ;
  FAnimations.Update( aTime );
end;

procedure TConUIMapArea.AddAnimation ( aAnimation : TAnimation ) ;
begin
  if aAnimation is TConUIAnimation then TConUIAnimation(aAnimation).FMap := Self;
  FAnimations.AddAnimation( aAnimation );
end;

procedure TConUIMapArea.ClearAnimations;
begin
  FAnimations.Clear;
end;

function TConUIMapArea.AnimationsFinished : Boolean;
begin
  Exit( FAnimations.Finished );
end;

function TConUIMapArea.Screen ( aWorld : TCoord2D ) : TUIPoint;
begin
  Screen := Point(aWorld.x,aWorld.y)-FShift+FAbsolute.pos-Point(1,1);
end;

function TConUIMapArea.World ( aScreen : TUIPoint ) : TCoord2D;
begin
  aScreen  := FShift+aScreen-FAbsolute.pos+Point(1,1);
  World.Create( aScreen.X, aScreen.Y );
end;

end.

