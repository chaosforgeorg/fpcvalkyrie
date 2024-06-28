{$INCLUDE valkyrie.inc}
unit vtigio;
interface

uses viotypes, vioeventstate, viomousestate, vioconsole, vgenerics;

type TTIGColor         = TIOColor;
     TTIGRect          = TIORect;
     TTIGPoint         = TIOPoint;

type TTIGCursorType      = (
  VTIG_CTNONE, VTIG_CTINPUT
);

type TTIGDrawCommandType = (
  VTIG_CMD_TEXT,
  VTIG_CMD_CLEAR,
  VTIG_CMD_FRAME,
  VTIG_CMD_RULER,
  VTIG_CMD_BAR
);

type TTIGSoundEvent = (
  VTIG_SOUND_CHANGE,
  VTIG_SOUND_ACCEPT
);

const
  VTIG_IE_UNUSED    = 128;
  VTIG_IE_UP        = 129;
  VTIG_IE_DOWN      = 130;
  VTIG_IE_LEFT      = 131;
  VTIG_IE_RIGHT     = 132;
  VTIG_IE_HOME      = 133;
  VTIG_IE_END       = 134;
  VTIG_IE_PGUP      = 135;
  VTIG_IE_PGDOWN    = 136;
  VTIG_IE_CANCEL    = 137; // ESCAPE
  VTIG_IE_SELECT    = 138; // SPACE
  VTIG_IE_CONFIRM   = 139; // ENTER
  VTIG_IE_BACKSPACE = 140;
  VTIG_IE_MCONFIRM  = 141; // MOUSE LEFT

type TTIGDrawCommand = record
    CType : TTIGDrawCommandType;
    Clip  : TTIGRect;
    Area  : TTIGRect;
    Text  : TTIGPoint;
    FG    : TTIGColor;
    BG    : TTIGColor;
    XC    : TTIGColor;
  end;
type TTIGDrawCommandArray = specialize TGArray< TTIGDrawCommand >;
type TTIGTextArray        = specialize TGArray< Char >;

type TTIGDrawList = class
  public
    constructor Create;
    procedure Clear;
    function PushText( aText : PChar; aLength : Integer ) : TIOPoint;
    function PushChar( aChar : Char ) : TIOPoint;
    procedure Push( const aCmd : TTIGDrawCommand );
    destructor Destroy; override;
  private
    FCommands  : TTIGDrawCommandArray;
    FText      : TTIGTextArray;
  end;
type TTIGDrawListArray = specialize TGArray< TTIGDrawList >;

type TTIGDrawData = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    FLists          : TTIGDrawListArray;
    FCursorType     : TTIGCursorType;
    FCursorPosition : TTIGPoint;
  public
    property Lists : TTIGDrawListArray       read  FLists;
    property CursorType     : TTIGCursorType write FCursorType;
    property CursorPosition : TTIGPoint      write FCursorPosition;
  end;

type TTIGSoundCallback = procedure( aEvent : TTIGSoundEvent; aParam : Pointer );

type TTIGIOState = class
  public
    constructor Create;
    procedure Initialize( aRenderer : TIOConsoleRenderer; aDriver : TIODriver; aClearOnRender : Boolean = True );
    procedure Clear;
    procedure Render( aData : TTIGDrawData );
    procedure Update;
    procedure EndFrame;
    procedure PlaySound( aEvent : TTIGSoundEvent );
    destructor Destroy; override;
  private
    FEventState     : TIOEventState;
    FMouseState     : TIOMouseState;
    FRenderer       : TIOConsoleRenderer;
    FDriver         : TIODriver;
    FTime           : Single;
    FClearOnRender  : Boolean;
    FMousePosition  : TIOPoint;
    FSoundCallback  : TTIGSoundCallback;
    FSoundParameter : Pointer;
  private
    function GetSize : TIOPoint;
  public
    property SoundParameter : Pointer            write FSoundParameter;
    property SoundCallback  : TTIGSoundCallback  write FSoundCallback;
    property EventState     : TIOEventState      read FEventState;
    property MouseState     : TIOMouseState      read FMouseState;
    property Renderer       : TIOConsoleRenderer read FRenderer;
    property MousePosition  : TIOPoint           read FMousePosition write FMousePosition;
    property Size           : TIOPoint           read GetSize;
    property Driver         : TIODriver          read FDriver;
end;

implementation

uses SysUtils, vutil;

constructor TTIGDrawList.Create;
begin
  FCommands := TTIGDrawCommandArray.Create;
  FText     := TTIGTextArray.Create;
end;

procedure TTIGDrawList.Clear;
begin
  FCommands.Clear;
  FText.Clear;
end;

function TTIGDrawList.PushText( aText : PChar; aLength : Integer ) : TIOPoint;
begin
  Result.X := FText.Size;
  FText.Append( aText, aLength );
  Result.Y := FText.Size;
end;

function TTIGDrawList.PushChar( aChar : Char ) : TIOPoint;
begin
  Result.X := FText.Size;
  FText.Push( aChar );
  Result.Y := FText.Size;
end;

procedure TTIGDrawList.Push( const aCmd : TTIGDrawCommand );
begin
  FCommands.Push( aCmd );
end;

destructor TTIGDrawList.Destroy;
begin
  FreeAndNil( FCommands );
  FreeAndNil( FText );
  inherited Destroy;
end;

constructor TTIGDrawData.Create;
begin
  FCursorType     := VTIG_CTNONE;
  FCursorPosition := Point(0,0);
  FLists          := TTIGDrawListArray.Create;
end;

destructor TTIGDrawData.Destroy;
begin
  FreeAndNil( FLists );
end;

constructor TTIGIOState.Create;
begin
  FEventState := TIOEventState.Create;
  FMouseState := TIOMouseState.Create;
  FRenderer   := nil;
  FDriver     := nil;
  FTime           := 0.0;
  FClearOnRender  := False;
  FMousePosition  := Point( -1, -1 );
  FSoundCallback  := nil;
  FSoundParameter := nil;
end;

procedure TTIGIOState.Initialize( aRenderer : TIOConsoleRenderer; aDriver : TIODriver; aClearOnRender : Boolean = True );
begin
  FRenderer      := aRenderer;
  FDriver        := aDriver;
  FClearOnRender := aClearOnRender;
end;

procedure TTIGIOState.Clear;
begin
  if not Assigned( FRenderer ) then Exit;
  FRenderer.Clear;
  FRenderer.HideCursor;
  FRenderer.Update;
end;

procedure TTIGIOState.Render( aData : TTIGDrawData );
var i       : Integer;
    iX, iY  : Integer;
    iList   : TTIGDrawList;
    iCmd    : TTIGDrawCommand;
    iCoord  : TIOPoint;
    iChar   : DWord;
    iBorder : PChar;
    iGlyph  : Char;
begin
  if not Assigned( FRenderer ) then Exit;
  if FClearOnRender then
    FRenderer.Clear;

  if aData.FCursorType <> VTIG_CTNONE then
  begin
    FRenderer.ShowCursor;
    FRenderer.MoveCursor( aData.FCursorPosition.X, aData.FCursorPosition.Y );
  end
  else
    FRenderer.HideCursor;

  for iList in aData.FLists do
    for iCmd in iList.FCommands do
      case iCmd.CType of
        VTIG_CMD_CLEAR: FRenderer.ClearRect( iCmd.Area.x, iCmd.Area.y, iCmd.Area.x2, iCmd.Area.y2, iCmd.BG );
        VTIG_CMD_TEXT:
        begin
          iCoord := iCmd.Area.Pos;
          for i := iCmd.Text.X to iCmd.Text.Y - 1 do
          begin
            iChar := DWord( iList.FText[i] );
            if iChar = Ord(#13) then Continue;
            {
            if Char(iChar) > #$7F then
            begin
              iCode := 0;
              len := uchar32_from_utf8(@iCode, @(iList.FText[i]), @(iList.FText[dlist.TextLength]));
              if len > 0 then
              begin
                if FUTF8.TryGetValue(iCode, iChar) then
                  Inc(i, len - 1)
                else
                begin
                  NV_LOG_ERROR('unknown UTF codepoint - ', code);
                  Continue;
                end;
              end
              else
                iChar := 0;
            end;
            }
            if (iChar = Ord(#10)) or (iCoord.X > iCmd.Clip.X2) then
            begin
              Inc(iCoord.Y);
              if iCoord.Y >= iCmd.Clip.Y2 then Break;
              iCoord.X := iCmd.Area.X;
              if iChar = Ord(#10) then Continue;
            end;
            FRenderer.OutputChar(iCoord.X, iCoord.Y, iCmd.FG, iCmd.BG, Char(iChar));
            Inc(iCoord.X);
          end;
        end;
        VTIG_CMD_RULER:
        begin
          iBorder := PChar(@(iList.FText.Data[iCmd.Text.X]));
          iGlyph  := iBorder[0];
          if iCmd.Area.X = iCmd.Area.X2 then iGlyph := iBorder[1];
          for iCoord in iCmd.Area do
            FRenderer.OutputChar( iCoord.x, iCoord.y, iCmd.FG, iCmd.BG, iGlyph );
        end;
        VTIG_CMD_BAR:
        begin
          iBorder := PChar(@(iList.FText.Data[iCmd.Text.X]));
          for iCoord in iCmd.Area do
            FRenderer.OutputChar( iCoord.x, iCoord.y, iCmd.XC, iCmd.BG, iBorder[2] );
          FRenderer.OutputChar( iCmd.Area.X,  iCmd.Area.Y,  iCmd.FG, iCmd.BG, iBorder[0] );
          FRenderer.OutputChar( iCmd.Area.X2, iCmd.Area.Y2, iCmd.FG, iCmd.BG, iBorder[1] );
        end;
        VTIG_CMD_FRAME:
        begin
          FRenderer.ClearRect( iCmd.Area.X, iCmd.Area.Y, iCmd.Area.X2, iCmd.Area.Y2, iCmd.BG );
          iBorder := PChar(@(iList.FText.Data[iCmd.Text.X]));
          for iX := 0 to iCmd.Area.w - 1 do
          begin
            FRenderer.OutputChar( iCmd.Area.X + iX, iCmd.Area.Y,  iCmd.FG, iCmd.BG, iBorder[0]);
            FRenderer.OutputChar( iCmd.Area.X + iX, iCmd.Area.Y2, iCmd.FG, iCmd.BG, iBorder[1]);
          end;
          for iY := 0 to iCmd.Area.h - 1 do
          begin
            FRenderer.OutputChar( iCmd.Area.X,  iCmd.Area.Y + iY, iCmd.FG, iCmd.BG, iBorder[2]);
            FRenderer.OutputChar( iCmd.Area.X2, iCmd.Area.Y + iY, iCmd.FG, iCmd.BG, iBorder[3]);
          end;
          FRenderer.OutputChar( iCmd.Area.X,  iCmd.Area.Y,  iCmd.FG, iCmd.BG, iBorder[4]);
          FRenderer.OutputChar( iCmd.Area.X2, iCmd.Area.Y,  iCmd.FG, iCmd.BG, iBorder[5]);
          FRenderer.OutputChar( iCmd.Area.X,  iCmd.Area.Y2, iCmd.FG, iCmd.BG, iBorder[6]);
          FRenderer.OutputChar( iCmd.Area.X2, iCmd.Area.Y2, iCmd.FG, iCmd.BG, iBorder[7]);
        end;
      else
        // Handle default case if necessary
      end;

  if (FMousePosition.X <> -1) and (FMousePosition.Y <> -1) then
    FRenderer.OutputChar( FMousePosition.X, FMousePosition.Y, White, Chr(30) );

  FRenderer.Update;
end;

procedure TTIGIOState.Update;
var iMSTime : DWord;
    iCTime  : Single;
    iDTime  : Single;
begin
  if not Assigned( FRenderer ) then Exit;
  iMSTime := FDriver.GetMs;
  iCTime  := iMSTime / 1000.0;
  iDTime  := 1.0 / 60.0;
  if FTime > 0 then
    iDTime := iCTime - FTime;
  FTime := iCTime;
  FEventState.update( iDTime );
  FMouseState.update( iDTime );
end;

procedure TTIGIOState.EndFrame;
begin
  FEventState.EndFrame;
  FMouseState.EndFrame;
end;

procedure TTIGIOState.PlaySound( aEvent : TTIGSoundEvent );
begin
  if Assigned( FSoundCallback ) then FSoundCallback( aEvent, FSoundParameter );
end;

destructor TTIGIOState.Destroy;
begin
  FreeAndNil( FEventState );
  FreeAndNil( FMouseState );
  inherited Destroy;
end;

function TTIGIOState.GetSize : TIOPoint;
begin
  if Assigned( FRenderer ) then Exit( FRenderer.GetDeviceArea.Dim );
  Exit( Point(0,0) );
end;


end.

