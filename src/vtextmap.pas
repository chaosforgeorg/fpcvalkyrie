{$INCLUDE valkyrie.inc}
unit vtextmap;
interface
uses vanimation, vrltools, viotypes, vioconsole, vgenerics, vvision;

type ITextMap = interface
  function getGylph( const aCoord : TCoord2D ) : TIOGylph;
end;

type TTextMap = class;

type TTextAnimation = class( TAnimation )
  constructor Create( aDuration : DWord; aDelay : DWord );
protected
  FMap      : TTextMap;
end;

type TTextAnimations = specialize TGObjectArray< TAnimation >;

type TTextMap = class
  constructor Create( aConsole : TIOConsoleRenderer; aArea : TIORect; aMap : ITextMap = nil );
  destructor Destroy; override;
  procedure SetCenter( aCoord : TCoord2D );
  procedure SetMap( aMap : ITextMap );
  procedure Mark( aCoord : TCoord2D; aSign : char; aColor : TIOColor );
  procedure FreezeMarks;
  procedure ClearMarks;
  procedure ClearMark( aCoord : TCoord2D );
  procedure OnRedraw;
  procedure Update( aTime : DWord );
  procedure AddAnimation( aAnimation : TAnimation );
  procedure ClearAnimations;
  function AnimationsFinished : Boolean;
  function Screen( aWorld : TCoord2D ) : TIOPoint; inline;
  function World( aScreen : TIOPoint ) : TCoord2D;  inline;
protected
  FMarkMap    : array of array of TIOGylph;
  FArea       : TIORect;
  FAnimations : TAnimations;
  FMap        : ITextMap;
  FShift      : TIOPoint;
  FLength     : TIOPoint;
  FConsole    : TIOConsoleRenderer;
public
  property Shift   : TIOPoint           read FShift    write FShift;
  property Console : TIOConsoleRenderer read FConsole;
end;

type TTextMarkAnimation = class(TTextAnimation)
  constructor Create( aWhere : TCoord2D; aGylph : TIOGylph; aDuration : DWord; aDelay : DWord = 0 );
  procedure OnDraw; override;
protected
  FWhere : TCoord2D;
  FGylph : TIOGylph;
end;

type TTextBlinkAnimation = class(TTextAnimation)
  constructor Create( aGylph : TIOGylph; aDuration : DWord; aDelay : DWord = 0 );
  procedure OnDraw; override;
protected
  FGylph : TIOGylph;
end;

type TTextBulletAnimation = class(TTextAnimation)
  constructor Create( aMap : IVisionQuery; aSource, aTarget : TCoord2D; aGylph : TIOGylph; aDuration : DWord; aDelay : DWord; aVisionRange : Word = 0 );
  procedure OnDraw; override;
protected
  FRay      : TVisionRay;
  FDistance : DWord;
  FMaxDist  : DWord;
  FGylph    : TIOGylph;
  FRange    : Word;
end;

type TTextRayAnimation = class(TTextAnimation)
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

type TTextClearMarkAnimation = class(TTextAnimation)
  constructor Create( aDelay : DWord = 0 );
  destructor Destroy; override;
end;

type TTextExplosionArray = array of record Color : TIOColor; Time : DWord; end;

type TTextExplosionAnimation = class(TTextAnimation)
  constructor Create( aWhere : TCoord2D; aChar : Char; const aArray : TTextExplosionArray; aDelay : DWord = 0 );
  procedure OnDraw; override;
  destructor Destroy; override;
protected
  FArray    : TTextExplosionArray;
  FCount    : DWord;
  FTCount   : DWord;
  FWhere    : TCoord2D;
  FGylph    : Char;
end;

implementation

uses SysUtils, vutil;

constructor TTextAnimation.Create ( aDuration : DWord; aDelay : DWord ) ;
begin
  inherited Create( aDuration, aDelay, 0 );
  FMap      := nil;
end;

constructor TTextMap.Create ( aConsole : TIOConsoleRenderer; aArea : TIORect; aMap : ITextMap ) ;
begin
  FArea := aArea;
  FLength.Init(0,0);
  FShift.Init(0,0);
  FAnimations := TAnimations.Create;
  FConsole    := aConsole;
  SetMap(aMap);
  ClearMarks;
end;

destructor TTextMap.Destroy;
begin
  FreeAndNil( FAnimations );
  inherited Destroy;
end;

procedure TTextMap.SetCenter( aCoord : TCoord2D ) ;
begin
  FShift.X := aCoord.X - FArea.w div 2;
  FShift.Y := aCoord.Y - FArea.h div 2;
end;

procedure TTextMap.SetMap( aMap : ITextMap ) ;
begin
  FMap := aMap;
end;

procedure TTextMap.Mark( aCoord : TCoord2D; aSign : char; aColor : TIOColor );
var iPoint : TIOPoint;
begin
  iPoint := Screen(aCoord) - FArea.Pos;
  if (iPoint.X < 0) or (iPoint.Y < 0) or (iPoint.X >= FArea.w) or (iPoint.Y >= FArea.h) then Exit;
  FMarkMap[iPoint.x,iPoint.y].Init( aSign, aColor );
end;

procedure TTextMap.FreezeMarks;
var x,y    : Integer;
    iReset : Boolean;
begin
  iReset := FLength <> FArea.Dim;
  if iReset then SetLength( FMarkMap, FArea.w );
  for x := 0 to FArea.w-1 do
  begin
    if iReset then SetLength( FMarkMap[x],FArea.h );
    for y := 0 to FArea.h-1 do
       FMarkMap[x,y] := FMap.getGylph( NewCoord2D( x + FShift.x + 1, y + FShift.y + 1 ) );
  end;
  FLength := FArea.Dim;
end;

procedure TTextMap.ClearMarks;
var x,y    : DWord;
    iReset : Boolean;
begin
  iReset := FLength <> FArea.Dim;
  if iReset then SetLength( FMarkMap, FArea.w );
  for x := 0 to FArea.w-1 do
  begin
    if iReset then SetLength( FMarkMap[x],FArea.h );
    for y := 0 to FArea.h-1 do
       FMarkMap[x,y].Color := 0;
  end;
  FLength := FArea.Dim;
end;

procedure TTextMap.ClearMark( aCoord: TCoord2D );
var iPoint : TIOPoint;
begin
  iPoint := Screen(aCoord)-FArea.Pos;
  if (iPoint.X < 0) or (iPoint.Y < 0) or (iPoint.X >= FArea.w) or (iPoint.Y >= FArea.h) then Exit;
  FMarkMap[iPoint.x,iPoint.y].Color := 0;
end;

procedure TTextMap.OnRedraw;
var x,y    : Integer;
  procedure DrawTile( aX, aY : Integer );
  var iCoord   : TCoord2D;
      iPicture : TIOGylph;
  begin
    iCoord   := World( Point( aX, aY ) );
    iPicture := FMap.getGylph( iCoord );
    FConsole.OutputChar( aX, aY, iPicture.Color, iPicture.ASCII );
    iPicture := FMarkMap[aX-FArea.Pos.x,aY-FArea.Pos.y];
    if iPicture.Color <> 0 then
      FConsole.OutputChar( aX, aY, iPicture.Color, iPicture.ASCII );
  end;
begin
  for x := FArea.pos.x to FArea.pos.x+FArea.dim.x-1 do
    for y := FArea.pos.y to FArea.pos.y+FArea.dim.y-1 do
      DrawTile(x,y);
  FAnimations.Draw;
end;

procedure TTextMap.Update ( aTime : DWord ) ;
begin
  FAnimations.Update( aTime );
end;

procedure TTextMap.AddAnimation ( aAnimation : TAnimation ) ;
begin
  if aAnimation is TTextAnimation then TTextAnimation(aAnimation).FMap := Self;
  FAnimations.AddAnimation( aAnimation );
end;

procedure TTextMap.ClearAnimations;
begin
  FAnimations.Clear;
end;

function TTextMap.AnimationsFinished : Boolean;
begin
  Exit( FAnimations.Finished );
end;

function TTextMap.Screen ( aWorld : TCoord2D ) : TIOPoint;
begin
  Screen := Point(aWorld.x,aWorld.y)-FShift+FArea.pos-Point(1,1);
end;

function TTextMap.World ( aScreen : TIOPoint ) : TCoord2D;
begin
  aScreen  := FShift+aScreen-FArea.pos+Point(1,1);
  World.Create( aScreen.X, aScreen.Y );
end;

constructor TTextMarkAnimation.Create ( aWhere : TCoord2D; aGylph : TIOGylph;
                                        aDuration : DWord; aDelay : DWord ) ;
begin
  inherited Create( aDuration, aDelay );
  FWhere := aWhere;
  FGylph := aGylph;
end;

procedure TTextMarkAnimation.OnDraw;
var iPoint : TIOPoint;
begin
  iPoint := FMap.Screen( FWhere );
  FMap.Console.OutputChar( iPoint.X, iPoint.Y, FGylph.Color, FGylph.ASCII );
end;

constructor TTextBlinkAnimation.Create ( aGylph : TIOGylph; aDuration : DWord;
                                         aDelay : DWord ) ;
begin
  inherited Create( aDuration, aDelay );
  FGylph := aGylph;
end;

procedure TTextBlinkAnimation.OnDraw;
var iPoint : TIOPoint;
begin
  with FMap.Console do
  for iPoint in FMap.FArea do
    OutputChar( iPoint.X, iPoint.Y, FGylph.Color, FGylph.ASCII );
end;

constructor TTextBulletAnimation.Create ( aMap : IVisionQuery; aSource, aTarget : TCoord2D;
  aGylph : TIOGylph; aDuration : DWord; aDelay : DWord; aVisionRange : Word = 0 ) ;
begin
  inherited Create( aDuration, aDelay );
  FRay.Init( aMap, aSource, aTarget );
  FDistance := 0;
  FGylph    := aGylph;
  FMaxDist  := (aSource - aTarget).LargerLength;
  FRange    := aVisionRange;
end;

procedure TTextBulletAnimation.OnDraw;
var iRatio : Single;
    iChar  : Char;
    iPoint : TIOPoint;
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
  if not FRay.Map.blocksVision( FRay.GetC ) then
  begin
    iPoint := FMap.Screen( FRay.GetC );
    FMap.Console.OutputChar( iPoint.X, iPoint.Y, FGylph.Color, iChar );
  end;
end;

constructor TTextRayAnimation.Create ( aMap : IVisionQuery; aSource, aTarget : TCoord2D;
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

procedure TTextRayAnimation.OnDraw;
var iRay   : TVisionRay;
    iDist  : DWord;
    iChar  : Char;
    iPoint : TIOPoint;
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
    iPoint := FMap.Screen( iRay.GetC );
    FMap.Console.OutputChar( iPoint.X, iPoint.Y, FGylph.Color, iChar );
  end;
end;

constructor TTextClearMarkAnimation.Create ( aDelay : DWord ) ;
begin
  inherited Create( 1, aDelay );
end;

destructor TTextClearMarkAnimation.Destroy;
begin
  FMap.ClearMarks;
  inherited Destroy;
end;

constructor TTextExplosionAnimation.Create ( aWhere : TCoord2D; aChar : Char;
  const aArray : TTextExplosionArray; aDelay : DWord ) ;
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

procedure TTextExplosionAnimation.OnDraw;
var iPoint : TIOPoint;
begin
  if FDuration = 0 then Exit;
  while (FCount < High( FArray )) and ( FTime - FTCount > FArray[FCount].Time ) do
  begin
    FTCount += FArray[FCount].Time;
    Inc( FCount );
  end;
  iPoint := FMap.Screen(FWhere);
  FMap.Console.OutputChar( iPoint.X, iPoint.Y, FArray[FCount].Color, FGylph );
end;

destructor TTextExplosionAnimation.Destroy;
begin
  FMap.ClearMark( FWhere );
  inherited Destroy;
end;

end.

