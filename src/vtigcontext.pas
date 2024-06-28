{$INCLUDE valkyrie.inc}
unit vtigcontext;
interface
uses Math, vtigio, viotypes, vgenerics, vtigstyle;

type TTIGGroupInfo = record
  Cursor   : TIOPoint;
  Content  : TIORect;
  Clip     : TIORect;
  Size     : Integer;
  Vertical : Boolean;
end;

type TTIGWindowDC = class
  constructor Create;
  procedure BeginGroup( aSize : Integer = -1; aVertical : Boolean = false );
  procedure EndGroup;
//private
public
  FContent        : TIORect;
  FClip           : TIORect;
  FCursor         : TIOPoint;
private
  FGroupStackSize : Integer;
  FGroupStack     : array of TTIGGroupInfo;
end;

type TTIGFocusInfo = record
  Count   : Integer;
  Current : Integer;
end;

type TTIGWindow = class
  FClipContent  : TIORect;
  FBackground   : TIOColor;
  FColor        : TIOColor;

  FFocusInfo    : TTIGFocusInfo;
  FScroll       : Integer;
  FSelectScroll : Integer;

  FReset        : Boolean;
  FMaxSize      : TIOPoint;

  constructor Create;
  procedure Advance( aSize : TIOPoint );
  destructor Destroy; override;
private
  FDC           : TTIGWindowDC;
  FDrawList     : TTIGDrawList;
public
  property DrawList : TTIGDrawList read FDrawList;
  property DC       : TTIGWIndowDC read FDC;

end;

type TTIGWindowArray = specialize TGArray<TTIGWindow>;
     TTIGWindowTable = specialize TGHashMap<TTIGWindow>;

type TTIGContext = class
  Io                 : TTIGIOState;
  Style              : PTIGStyle;
  Size               : TIOPoint;

  Current            : TTIGWindow;
  LastTop            : TTIGWindow;

  Windows            : TTIGWindowArray;
  WindowStack        : TTIGWindowArray;
  WindowOrder        : TTIGWindowArray;
  WindowStore        : TTIGWindowTable;

  DrawData           : TTIGDrawData;
  MouseCaptured      : Boolean;

  DTime              : DWord;
  Time               : DWord;

  WindowTransparency : Boolean;

  Color              : TIOColor;
  BGColor            : TIOColor;

  constructor Create;
  destructor Destroy; override;
end;

implementation

uses SysUtils, vutil;

constructor TTIGWindowDC.Create;
begin
  FillChar( FContent, SizeOf( FContent ), 0 );
  FillChar( FClip,    SizeOf( FClip ), 0 );
  FillChar( FCursor,  SizeOf( FCursor ), 0 );
  FGroupStackSize := 0;
end;

procedure TTIGWindowDC.BeginGroup( aSize : Integer = -1; aVertical : Boolean = false );
var iGroup : ^TTIGGroupInfo;
begin
  if Length( FGroupStack ) <= FGroupStackSize then
    SetLength( FGroupStack, Max( 4, FGroupStackSize * 2 ) );
  Inc( FGroupStackSize );
  iGroup := @( FGroupStack[ FGroupStackSize - 1 ] );
  iGroup^.Cursor   := FCursor;
  iGroup^.Content  := FContent;
  iGroup^.Clip     := FClip;
  iGroup^.Vertical := aVertical;
  if aVertical then
  begin
   iGroup^.Size := aSize;
   FClip.Pos.Y  := FCursor.Y - 1;
   FClip.Dim.Y  := aSize;
   FContent.Pos.Y := FCursor.Y;
   FContent.Dim.Y := aSize - 1; // or -2?
  end
  else
  begin
    if aSize < 0 then aSize := FContent.Dim.Y + 2;
    iGroup^.Size   := aSize;
    FContent.Dim.X := aSize - 1;
    FClip.Pos.X    := FContent.Pos.X - 1;
    FClip.Dim.X    := FContent.Dim.X + 2;
  end;
end;

procedure TTIGWindowDC.EndGroup;
var iGroup : ^TTIGGroupInfo;
begin
  iGroup := @( FGroupStack[ FGroupStackSize - 1 ] );
  FCursor  := iGroup^.Cursor;
  FContent := iGroup^.Content;
  FClip    := iGroup^.Clip;

  if iGroup^.Vertical then
  begin
    FCursor.X := FContent.X;
    FCursor.Y += iGroup^.Size;
  end
  else
  begin
    FContent.Pos.X += iGroup^.Size + 2;
    FContent.Dim.X -= iGroup^.Size + 2;
    FCursor := FContent.Pos;
  end;
  Dec( FGroupStackSize );
end;

constructor TTIGWindow.Create;
begin
  FillChar( FClipContent, SizeOf( FClipContent ), 0 );
  FBackground   := 0;
  FColor        := 0;

  FillChar( FFocusInfo, SizeOf( FFocusInfo ), 0 );
  FScroll       := 0;
  FSelectScroll := 0;
  FReset        := False;
  FMaxSize      := Point( -1,-1 );

  FDC           := TTIGWindowDC.Create;
  FDrawList     := TTIGDrawList.Create;
end;

procedure TTIGWindow.Advance( aSize : TIOPoint );
begin
  FDC.FCursor        := Point( FDC.FContent.X, FDC.FCursor.Y + aSize.Y );
  FDC.FContent.Dim.Y := Max( FDC.FCursor.Y - FDC.FContent.Y + 1, FDC.FContent.Dim.Y );
  FMaxSize           := Max( FMaxSize, FDC.FCursor - FDC.FContent.Pos );
end;

destructor TTIGWindow.Destroy;
begin
  FreeAndNil( FDC );
  FreeAndNil( FDrawList );
  inherited Destroy;
end;

constructor TTIGContext.Create;
begin
  Io          := TTIGIOState.Create;
  Windows     := TTIGWindowArray.Create;
  WindowStack := TTIGWindowArray.Create;
  WindowOrder := TTIGWindowArray.Create;
  WindowStore := TTIGWindowTable.Create;
  DrawData    := TTIGDrawData.Create;

  Style              := @VTIGDefaultStyle;
  Size               := Point(0,0);

  Current            := nil;
  LastTop            := nil;
  MouseCaptured      := False;
  DTime              := 0;
  Time               := 0;
  WindowTransparency := True;
  Color              := 0;
  BGColor            := 0;
end;

destructor TTIGContext.Destroy;
var iWindow : TTIGWindow;
begin
  for iWindow in Windows do
    iWindow.Free;
  FreeAndNil( Windows );
  FreeAndNil( WindowStack );
  FreeAndNil( WindowOrder );
  FreeAndNil( WindowStore );
  FreeAndNil( DrawData );
  FreeAndNil( Io );
  inherited Destroy;
end;

end.

