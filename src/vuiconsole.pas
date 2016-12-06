{$INCLUDE valkyrie.inc}
unit vuiconsole;
interface
uses Classes, SysUtils, viotypes, vioconsole, vuitypes;

type TUICodingMode = ( VUI_CODING_USE, VUI_CODING_STRIP, VUI_CODING_IGNORE );

type

{ TUIConsole }

 TUIConsole = object
  procedure Init( aRenderer : TIOConsoleRenderer );
  procedure DrawChar( aPosition : TIOPoint; aForeColor : TIOColor; aChar : Char );
  procedure DrawChar( aPosition : TIOPoint; aForeColor, aBackColor : TIOColor; aChar : Char );
  procedure DrawCharEx( aPosition : TIOPoint; aOffset : TIOPoint; aFont : byte; aForeColor : TIOColor; aChar : Char );
  procedure DrawCharEx( aPosition : TIOPoint; aOffset : TIOPoint; aFont : byte; aForeColor, aBackColor : TIOColor; aChar : Char );

  procedure RawPrint( aPosition : TIOPoint; aForeColor : TIOColor; const aString : Ansistring );
  procedure RawPrint( aPosition : TIOPoint; aForeColor, aBackColor : TIOColor; const aString : Ansistring );
  procedure Print( aPosition : TIOPoint; aForeColor, aBackColor : TIOColor; const aString : Ansistring; aEncoding : TUICodingMode );
  procedure Print( aPosition : TIOPoint; aForeColor, aBackColor : TIOColor; const aString : Ansistring; aEncoded : Boolean );
  procedure PrintEx( aPosition : TIOPoint; aOffset : TIOPoint; aFont : byte; aForeColor, aBackColor : TIOColor; const aString : Ansistring; aEncoding : TUICodingMode );
  procedure PrintEx( aPosition : TIOPoint; aOffset : TIOPoint; aFont : byte; aForeColor, aBackColor : TIOColor; const aString : Ansistring; aEncoded : Boolean );

  procedure Update;
  procedure Clear;
  procedure ClearRect( aArea : TIORect; aBackColor : TIOColor = 0 );
  procedure HideCursor;
  procedure ShowCursor;
  procedure MoveCursor( aPosition : TIOPoint );
  procedure SetCursorType( aType : TIOCursorType );
  function  ScreenShot( const FileName : string; stype : byte = 0 ) : Boolean;
  function  BoldColor( aColor : TUIColor ) : TUIColor;
  function  CodeColor( aCode : Char; aColor : TUIColor ) : TUIColor; inline;
  function  GetRect : TIORect;
  procedure Print( aPosition : TUIPoint; aChunkList : TUIChunkList; const aFrontColor, aBackColor : TUIColor; const aClamp : TUIRect );
  procedure Print( aPosition : TUIPoint; aChunkList : TUIChunkList; const aBackColor : TUIColor );
  procedure Print( aPosition : TUIPoint; aChunkList : TUIChunkList );
  procedure PrintEx( aPosition : TUIPoint; aOffset : TIOPoint; aFont : byte; aChunkList : TUIChunkList; const aFrontColor, aBackColor : TUIColor; const aClamp : TUIRect );
  procedure PrintEx( aPosition : TUIPoint; aOffset : TIOPoint; aFont : byte; aChunkList : TUIChunkList; const aBackColor : TUIColor );
  procedure PrintEx( aPosition : TUIPoint; aOffset : TIOPoint; aFont : byte; aChunkList : TUIChunkList );
  function  ChunkifyEx( var aChunkList : TUIChunkList; var aPosition : TUIPoint; var aColor : TUIColor;
                      const aText : AnsiString; const aBaseColor : TUIColor; const aDimensions :TUIPoint;
                      aLineBreak : TUICodingMode = VUI_CODING_USE; aCoding : TUICodingMode = VUI_CODING_USE ) : Boolean;
  function  ChunkifyEx( var aChunkList : TUIChunkList; const aText : AnsiString; const aDimensions : TUIPoint; const aColor : TUIColor;
                      aLineBreak : TUICodingMode = VUI_CODING_USE; aCoding : TUICodingMode = VUI_CODING_USE ) : Boolean;
  function  Chunkify( const aText : AnsiString; const aDimensions : TUIPoint; const aColor : TUIColor ) : TUIChunkList;
  function  LinifyChunkList( var aChunkList : TUIChunkList ) : TUIChunkBuffer;
private
  FRenderer : TIOConsoleRenderer;
public
  property Raw : TIOConsoleRenderer read FRenderer;
end;

implementation

uses math, strutils, vutil, vdebug;

{ TUIConsole }

procedure TUIConsole.Init ( aRenderer : TIOConsoleRenderer ) ;
begin
  FRenderer := aRenderer;
end;

procedure TUIConsole.DrawChar ( aPosition : TIOPoint; aForeColor : TIOColor; aChar : Char ) ;
begin
  FRenderer.OutputChar( aPosition.x, aPosition.y, aForeColor, aChar );
end;

procedure TUIConsole.DrawChar ( aPosition : TIOPoint; aForeColor, aBackColor : TIOColor; aChar : Char ) ;
begin
  FRenderer.OutputChar( aPosition.x, aPosition.y, aForeColor, aBackColor, aChar );
end;

procedure TUIConsole.DrawCharEx(aPosition: TIOPoint; aOffset: TIOPoint;
  aFont: byte; aForeColor: TIOColor; aChar: Char);
begin
  FRenderer.OutputCharEx( aPosition.x, aPosition.y, aOffset.x, aOffset.y, aFont, aForeColor, aChar );
end;

procedure TUIConsole.DrawCharEx(aPosition: TIOPoint; aOffset: TIOPoint;
  aFont: byte; aForeColor, aBackColor: TIOColor; aChar: Char);
begin
  FRenderer.OutputCharEx( aPosition.x, aPosition.y, aOffset.x, aOffset.y, aFont, aForeColor, aBackColor, aChar );
end;

procedure TUIConsole.Print( aPosition : TIOPoint; aForeColor,aBackColor : TIOColor; const aString : Ansistring; aEncoding : TUICodingMode );
var iPosX, iMaxX  : LongInt;
    iPosY, iMaxY  : LongInt;
    iLength, iPos : DWord;
    iForeColor    : TIOColor;
    iChar         : Char;
begin
  iPosX   := aPosition.X;
  iPosY   := aPosition.Y;
  iMaxX   := FRenderer.SizeX;
  iMaxY   := FRenderer.SizeY;
  iLength := Length( aString );
  iPos    := 0;
  iForeColor := aForeColor;
  if iLength = 0 then Exit;
  repeat
    Inc(iPos);
    iChar := aString[iPos];
    if (iChar = #10) then
    begin
      Inc( iPosY );
      iPosX := aPosition.X;
      if iPosY > iMaxY then Exit;
      Continue;
    end;

    if (iChar = '@') and (aEncoding <> VUI_CODING_IGNORE) then
    begin
      Inc(iPos); if iPos > iLength then Break;
      iChar := aString[iPos];
      if (iChar <> '@') then
      begin
        if (aEncoding = VUI_CODING_USE)  then
        begin
          case iChar of
            '>' : iForeColor := aForeColor;
            '<' : iForeColor := BoldColor(aForeColor);
          else iForeColor := CodeColor( iChar, iForeColor );
          end;
        end;
        Continue;
      end;
    end;

    if (iPosX > 0) and (iPosX <= iMaxX) then
      FRenderer.OutputChar(iPosX,iPosY,iForeColor,aBackColor,iChar);
    Inc(iPosX);
  until (iPos >= iLength);
end;

procedure TUIConsole.Print ( aPosition : TIOPoint; aForeColor, aBackColor : TIOColor; const aString : Ansistring; aEncoded : Boolean ) ;
begin
  if aEncoded
    then Print( aPosition, aForeColor, aBackColor, aString, VUI_CODING_USE )
    else RawPrint( aPosition, aForeColor, aBackColor, aString );
end;

procedure TUIConsole.PrintEx(aPosition: TIOPoint; aOffset: TIOPoint;
  aFont: byte; aForeColor, aBackColor: TIOColor; const aString: Ansistring;
  aEncoding: TUICodingMode);
var iPosX, iMaxX  : LongInt;
    iPosY, iMaxY  : LongInt;
    iLength, iPos : DWord;
    iForeColor    : TIOColor;
    iChar         : Char;
begin
  iPosX   := aPosition.X;
  iPosY   := aPosition.Y;
  iMaxX   := FRenderer.SizeX;
  iMaxY   := FRenderer.SizeY;
  iLength := Length( aString );
  iPos    := 0;
  iForeColor := aForeColor;
  if iLength = 0 then Exit;
  repeat
    Inc(iPos);
    iChar := aString[iPos];
    if (iChar = #10) then
    begin
      Inc( iPosY );
      iPosX := aPosition.X;
      if iPosY > iMaxY then Exit;
      Continue;
    end;

    if (iChar = '@') and (aEncoding <> VUI_CODING_IGNORE) then
    begin
      Inc(iPos); if iPos > iLength then Break;
      iChar := aString[iPos];
      if (iChar <> '@') then
      begin
        if (aEncoding = VUI_CODING_USE)  then
        begin
          case iChar of
            '>' : iForeColor := aForeColor;
            '<' : iForeColor := BoldColor(aForeColor);
          else iForeColor := CodeColor( iChar, iForeColor );
          end;
        end;
        Continue;
      end;
    end;

    if (iPosX > 0) and (iPosX <= iMaxX) then
      FRenderer.OutputCharEx(iPosX,iPosY,aOffset.x, aOffset.y, aFont, iForeColor,aBackColor,iChar);
    Inc(iPosX);
  until (iPos >= iLength);
end;

procedure TUIConsole.PrintEx(aPosition: TIOPoint; aOffset: TIOPoint;
  aFont: byte; aForeColor, aBackColor: TIOColor; const aString: Ansistring;
  aEncoded: Boolean);
begin
  if aEncoded
    then PrintEx( aPosition, aOffset, aFont, aForeColor, aBackColor, aString, VUI_CODING_USE )
    else RawPrint( aPosition, aForeColor, aBackColor, aString );
end;

procedure TUIConsole.RawPrint ( aPosition : TIOPoint; aForeColor : TIOColor; const aString : Ansistring ) ;
var iLength,iPos,iMax : LongInt;
begin
  iMax    := FRenderer.SizeX;
  iLength := system.Length(aString);
  iPos    := 0;
  if iLength > 0 then
  repeat
    Inc(iPos);
    FRenderer.OutputChar(aPosition.x,aPosition.y,aForeColor,aString[iPos]);
    Inc(aPosition.x);
  until (aPosition.x >= iMax+1) or (iPos = iLength);
end;

procedure TUIConsole.RawPrint ( aPosition : TIOPoint; aForeColor, aBackColor : TIOColor; const aString : Ansistring ) ;
var iLength,iPos,iMax : LongInt;
begin
  iMax    := FRenderer.SizeX;
  iLength := system.Length(aString);
  iPos    := 0;
  if iLength > 0 then
  repeat
    Inc(iPos);
    FRenderer.OutputChar( aPosition.x,aPosition.y, aForeColor, aBackColor, aString[iPos] );
    Inc(aPosition.x);
  until (aPosition.x >= iMax+1) or (iPos = iLength);
end;

procedure TUIConsole.Update;
begin
  FRenderer.Update;
end;

procedure TUIConsole.Clear;
begin
  FRenderer.Clear;
end;

procedure TUIConsole.ClearRect ( aArea : TIORect; aBackColor : TIOColor ) ;
begin
  if aBackColor = ColorNone then Exit;
  FRenderer.ClearRect( aArea.Pos.x, aArea.Pos.y, aArea.Pos.x + aArea.Dim.x-1, aArea.Pos.y + aArea.dim.y - 1, aBackColor );
end;

procedure TUIConsole.HideCursor;
begin
  FRenderer.HideCursor;
end;

procedure TUIConsole.ShowCursor;
begin
  FRenderer.ShowCursor;
end;

procedure TUIConsole.MoveCursor ( aPosition : TIOPoint ) ;
begin
  FRenderer.MoveCursor( aPosition.x, aPosition.y );
end;

procedure TUIConsole.SetCursorType ( aType : TIOCursorType ) ;
begin
  FRenderer.SetCursorType( aType );
end;

function TUIConsole.ScreenShot ( const FileName : string; stype : byte ) : Boolean;
var T   : Text;
    x,y : Word;
    lc,c : Byte;
    ch  : Char;
begin
  {$PUSH}
  {$I-}
  Assign(T,FileName);
  Rewrite(T);
  lc := 0;
  if IOResult <> 0 then Exit;
  if stype = 1 then
  begin
    Write(T,'[hr][tt][color=#BBB]');
    for y := 1 to FRenderer.SizeY do
    begin
      for x := 1 to FRenderer.SizeX do
      begin
        c := FRenderer.GetColor(x,y) mod 16;
        ch := FRenderer.GetChar(x,y);
        if ch = '³' then
          ch:='|';
        if ch = 'Ä' then
          ch:='-';
        if (ch = 'Ú') or (ch ='¿') or (ch = 'À') or (ch = 'Ù') then
          ch:='+';
        if (ch = ' ') or (ch = #0) then
        begin
          Write(T,' ');
          Continue;
        end;
        if lc <> c then
        begin
          Write(T,'[/color][color='+BBColorNames[c]+']');
          lc := c;
        end;
        Write(T,ch);
      end;
      Writeln(T);
    end;
    Writeln(T,'[/color][/tt][hr]');
  end
  else
  begin
    for y := 1 to FRenderer.SizeY do
    begin
      for x := 1 to FRenderer.SizeX do
        Write(T,FRenderer.GetChar(x,y));
      Writeln(T);
    end;
  end;
  Close(T);
  {$POP} {restore $I}
  Exit(True);
end;

function TUIConsole.BoldColor ( aColor : TUIColor ) : TUIColor;
begin

  if aColor <= 7 then Exit(aColor+8) else
  if aColor = 8  then Exit(7)
                 else Exit(White);
end;

function TUIConsole.CodeColor ( aCode : Char; aColor : TUIColor ) : TUIColor;
begin
  case aCode of
    'r' : Result := Red;            'R' : Result := LightRed;
    'b' : Result := Blue;           'B' : Result := LightBlue;
    'g' : Result := Green;          'G' : Result := LightGreen;
    'v' : Result := Magenta;        'V' : Result := LightMagenta;
    'c' : Result := Cyan;           'C' : Result := LightCyan;
    'l' : Result := LightGray;      'L' : Result := White;
    'd' : Result := DarkGray;       'D' : Result := Black;
'n','N' : Result := Brown;      'y','Y' : Result := Yellow;
  else
    Result := aColor;
  end;
end;

function TUIConsole.GetRect : TIORect;
begin
  GetRect.Pos := PointUnit;
  GetRect.Dim.Init( FRenderer.SizeX, FRenderer.SizeY );
end;

procedure TUIConsole.Print( aPosition : TUIPoint; aChunkList : TUIChunkList; const aFrontColor, aBackColor : TUIColor; const aClamp : TUIRect );
var iLength : LongInt;
    iPos    : LongInt;
    iMaxX   : LongInt;
    iMinX   : LongInt;
    iMaxY   : LongInt;
    iMinY   : LongInt;
    i       : LongInt;
    iCoord  : TIOPoint;
    iColor  : TUIColor;
begin
  if High( aChunkList ) < 0 then Exit;

  iMinX := Max( aClamp.Pos.X, 1 );
  iMinY := Max( aClamp.Pos.Y, 1 );
  iMaxX := Min( aClamp.Pos.X+aClamp.Dim.X-1, FRenderer.SizeX );
  iMaxY := Min( aClamp.Pos.Y+aClamp.Dim.Y-1, FRenderer.SizeY );

  for i := 0 to High( aChunkList ) do
  with aChunkList[i] do
  begin
    iLength := system.Length(Content);
    if iLength = 0 then Continue;
    iCoord  := Position+aPosition;
    iColor  := Color;
    if iColor = ColorNone then iColor := aFrontColor;

    if iCoord.y < iMinY then Continue;
    if iCoord.x > iMaxX then Continue;
    if iCoord.y > iMaxY then Exit;
    iPos    := 0;
    repeat
      Inc(iPos);
      if iCoord.x >= iMinX then
        FRenderer.OutputChar(iCoord.x,iCoord.y,iColor,aBackColor,Content[iPos]);
      Inc(iCoord.x);
    until (iPos = iLength) or (iCoord.x > iMaxX);
  end;
end;

procedure TUIConsole.Print ( aPosition : TUIPoint; aChunkList : TUIChunkList; const aBackColor : TUIColor ) ;
begin
  Print( aPosition, aChunkList, 0, aBackColor, GetRect );
end;

procedure TUIConsole.Print ( aPosition : TUIPoint; aChunkList : TUIChunkList ) ;
begin
  Print( aPosition, aChunkList, 0, ColorNone, GetRect );
end;

procedure TUIConsole.PrintEx(aPosition: TUIPoint; aOffset: TIOPoint;
  aFont: byte; aChunkList: TUIChunkList; const aFrontColor,
  aBackColor: TUIColor; const aClamp: TUIRect);
var iLength : LongInt;
    iPos    : LongInt;
    iMaxX   : LongInt;
    iMinX   : LongInt;
    iMaxY   : LongInt;
    iMinY   : LongInt;
    i       : LongInt;
    iCoord  : TIOPoint;
    iColor  : TUIColor;
begin
  if High( aChunkList ) < 0 then Exit;

  iMinX := Max( aClamp.Pos.X, 1 );
  iMinY := Max( aClamp.Pos.Y, 1 );
  iMaxX := Min( aClamp.Pos.X+aClamp.Dim.X-1, FRenderer.SizeX );
  iMaxY := Min( aClamp.Pos.Y+aClamp.Dim.Y-1, FRenderer.SizeY );

  for i := 0 to High( aChunkList ) do
  with aChunkList[i] do
  begin
    iLength := system.Length(Content);
    if iLength = 0 then Continue;
    iCoord  := Position+aPosition;
    iColor  := Color;
    if iColor = ColorNone then iColor := aFrontColor;

    if iCoord.y < iMinY then Continue;
    if iCoord.x > iMaxX then Continue;
    if iCoord.y > iMaxY then Exit;
    iPos    := 0;
    repeat
      Inc(iPos);
      if iCoord.x >= iMinX then
        FRenderer.OutputCharEx(iCoord.x,iCoord.y,aOffset.x,aOffset.y,aFont,iColor,aBackColor,Content[iPos]);
      Inc(iCoord.x);
    until (iPos = iLength) or (iCoord.x > iMaxX);
  end;
end;

procedure TUIConsole.PrintEx(aPosition: TUIPoint; aOffset: TIOPoint;
  aFont: byte; aChunkList: TUIChunkList; const aBackColor: TUIColor);
begin
  PrintEx( aPosition, aOffset, aFont, aChunkList, 0, aBackColor, GetRect );
end;

procedure TUIConsole.PrintEx(aPosition: TUIPoint; aOffset: TIOPoint;
  aFont: byte; aChunkList: TUIChunkList);
begin
  PrintEx( aPosition, aOffset, aFont, aChunkList, 0, ColorNone, GetRect );
end;

{
function TUIConsole.ChunkifyEx( var aChunkList : TUIChunkList; var aPosition : TUIPoint; var aColor : TUIColor;
                      const aText : AnsiString; const aBaseColor : TUIColor; const aDimensions :TUIPoint;
                      aLineBreak : TUICodingMode = VUI_CODING_USE; aCoding : TUICodingMode = VUI_CODING_USE ) : Boolean;
var iLength      : DWord;
    iPos         : DWord;
    iLastPos     : Integer;
    iMaxChunks   : Integer;
    iChunk       : LongInt;
    iLine        : DWord;
    iBreakChars  : TSysCharSet = ['@',#13,#10];
    iWord        : AnsiString;
// TODO: implement!
//    iColorStack  : array[0..32] of TUIColor;
//    iColorStackP : Byte;
  procedure PushChunk( const aWhat : AnsiString );
  begin
    if iChunk >= iMaxChunks then
    begin
      SetLength( aChunkList, iChunk+1 );
      iMaxChunks := iChunk + 1;
    end;
    aChunkList[iChunk].Content  := aWhat;
    aChunkList[iChunk].Color    := aColor;
    aChunkList[iChunk].Position := aPosition;
    aPosition.x += Length(aWhat);
    iLine += Length(aWhat);
    Inc(iChunk);
  end;
  function NextLine : Boolean;
  begin
    aPosition.x := 0;
    aPosition.y := aPosition.y + 1;
    iLine := 0;
    Exit( aPosition.y < aDimensions.y );
  end;
  function WithCleanup( aResult : Boolean ) : Boolean; inline;
  begin
    SetLength( aChunkList, iChunk );
    Result := aResult;
  end;

begin
  if aText = '' then Exit( False );
  iChunk     := High( aChunkList )+1;
  iMaxChunks := High( aChunkList )+1;
  iLength    := Length( aText );

  iLine      := Max(aPosition.x, 0);

  if aLineBreak = VUI_CODING_IGNORE then iBreakChars -= [ #10, #13 ];
  if aCoding    = VUI_CODING_IGNORE then iBreakChars -= [ '@' ];

  if (PosSet( iBreakChars, aText ) = 0)
    and ((iLength+iLine <= aDimensions.x) or (aLineBreak <> VUI_CODING_USE)) then
  begin
    if (iLength+iLine > aDimensions.x) then
      begin
        PushChunk( Copy( aText, 1, aDimensions.x-iLine ) );
        Exit( False );
      end;
    PushChunk( aText );
    Exit( True );
  end;

  iPos     := 1;
  iLastPos := 1;
  iMaxChunks += WordCount( aText, iBreakChars ) * 2;
  SetLength( aChunkList, iMaxChunks );

  repeat
    // Special characters
    while aText[iPos] in iBreakChars do
    begin
      case aText[iPos] of
        '@' : if aCoding = VUI_CODING_USE then
        begin
          Inc( iPos );
          if iPos > iLength then Exit( WithCleanup( True ) );
          case aText[iPos] of
            '@' : PushChunk('@');
            '>' : aColor := aBaseColor;
            '<' : aColor := BoldColor( aBaseColor );
          else
            aColor := CodeColor( aText[iPos], aColor );
          end;
        end;
        #13 : ;
        #10 :
          if aLineBreak = VUI_CODING_USE then
            if not NextLine then
              Exit( WithCleanup( False ) );
      end;
      Inc( iPos );
      if iPos > iLength then Exit( WithCleanup( True ) );
    end;

    // Normal operation
    iLastPos := iPos;
    iWord    := ExtractSubStr( aText, iLastPos, iBreakChars );

    // Split check
    while Length(iWord) > aDimensions.x-iLine do
    begin
      iLastPos := RPosEx( ' ', iWord, aDimensions.x-iLine+1 );
      // second chance
      if (iLastPos = 0) and (iLine = 0) then
        iLastPos := RPosSetEx( StdWordDelims, iWord, aDimensions.x-iLine );
      if iLastPos = 0 then
      begin
        if iLine = 0 then
          SetLength( iWord, aDimensions.x-iLine )
        else
          if not NextLine then
            Exit( WithCleanup( False ) );
      end
      else
      begin
        if (iWord[iLastPos] <> ' ') then Inc(iLastPos);
        PushChunk( Copy( iWord, 1, iLastPos-1 ) );
        // TODO : rewrite so we don't need to update iWord here,
        //   instead using indices.
        iWord := Copy( iWord, iLastPos+1, iLength );
        if not NextLine then
          Exit( WithCleanup( False ) );
        iPos += iLastPos;
      end;
    end;

    PushChunk(iWord);
    iPos := iPos + Length(iWord);
  until iPos > iLength;
  Exit( WithCleanup( True ) );
end;
}
function TUIConsole.ChunkifyEx( var aChunkList : TUIChunkList; var aPosition : TUIPoint; var aColor : TUIColor;
                      const aText : AnsiString; const aBaseColor : TUIColor; const aDimensions :TUIPoint;
                      aLineBreak : TUICodingMode = VUI_CODING_USE; aCoding : TUICodingMode = VUI_CODING_USE ) : Boolean;
var iLength      : DWord;
    iPos         : DWord;
    iLastPos     : Integer;
    iMaxChunks   : Integer;
    iChunk       : LongInt;
    iLine        : DWord;
    iBreakChars  : TSysCharSet = ['@',#13,#10,' '];
    iWord        : AnsiString;
// TODO: implement!
//    iColorStack  : array[0..32] of TUIColor;
//    iColorStackP : Byte;
  procedure PushChunk( const aWhat : AnsiString );
  begin
    if iChunk >= iMaxChunks then
    begin
      SetLength( aChunkList, iChunk+1 );
      iMaxChunks := iChunk + 1;
    end;
    aChunkList[iChunk].Content  := aWhat;
    aChunkList[iChunk].Color    := aColor;
    aChunkList[iChunk].Position := aPosition;
    aPosition.x += Length(aWhat);
    iLine += Length(aWhat);
    Inc(iChunk);
  end;
  function NextLine : Boolean;
  begin
    aPosition.x := 0;
    aPosition.y := aPosition.y + 1;
    iLine := 0;
    Exit( aPosition.y < aDimensions.y );
  end;
  function WithCleanup( aResult : Boolean ) : Boolean; inline;
  begin
    SetLength( aChunkList, iChunk );
    Result := aResult;
  end;

begin
  if aText = '' then Exit( False );
  iChunk     := High( aChunkList )+1;
  iMaxChunks := High( aChunkList )+1;
  iLength    := Length( aText );

  iLine      := Max(aPosition.x, 0);

  if aLineBreak = VUI_CODING_IGNORE then iBreakChars -= [ #10, #13 ];
  if aCoding    = VUI_CODING_IGNORE then iBreakChars -= [ '@' ];

  if (PosSet( iBreakChars, aText ) = 0)
    and ((iLength+iLine <= aDimensions.x) or (aLineBreak <> VUI_CODING_USE)) then
  begin
    if (iLength+iLine > aDimensions.x) then
      begin
        PushChunk( Copy( aText, 1, aDimensions.x-iLine ) );
        Exit( False );
      end;
    PushChunk( aText );
    Exit( True );
  end;

  iPos     := 1;
  iLastPos := 1;
  iMaxChunks += WordCount( aText, iBreakChars );
  SetLength( aChunkList, iMaxChunks );

  repeat
    // Special characters
    while aText[iPos] in iBreakChars do
    begin
      case aText[iPos] of
        '@' : if aCoding = VUI_CODING_USE then
        begin
          Inc( iPos );
          if iPos > iLength then Exit( WithCleanup( True ) );
          case aText[iPos] of
            '@' : PushChunk('@');
            '>' : aColor := aBaseColor;
            '<' : aColor := BoldColor( aBaseColor );
          else
            aColor := CodeColor( aText[iPos], aColor );
          end;
        end;
        ' ' : begin Inc( aPosition.x ); Inc( iLine ); end;
        #13 : ;
        #10 :
          if aLineBreak = VUI_CODING_USE then
            if not NextLine then
              Exit( WithCleanup( False ) );
      end;
      Inc( iPos );
      if iPos > iLength then Exit( WithCleanup( True ) );
    end;

    // Normal operation
    iLastPos := iPos;
    iWord    := ExtractSubStr( aText, iLastPos, iBreakChars );
    if Length(iWord) > aDimensions.x-iLine then
      if not NextLine then
        Exit( WithCleanup( False ) );

    PushChunk(iWord);
    iPos := iPos + Length(iWord);
  until iPos > iLength;
  Exit( WithCleanup( True ) );
end;

function TUIConsole.ChunkifyEx ( var aChunkList : TUIChunkList; const aText : AnsiString; const aDimensions : TUIPoint;
  const aColor : TUIColor; aLineBreak : TUICodingMode; aCoding : TUICodingMode ) : Boolean;
var iPosition : TUIPoint;
    iColor    : TUIColor;
begin
  iPosition := PointZero;
  iColor    := aColor;
  Exit( ChunkifyEx( aChunkList, iPosition, iColor, aText, aColor, aDimensions, aLineBreak, aCoding ) );
end;

function TUIConsole.Chunkify ( const aText : AnsiString; const aDimensions : TUIPoint; const aColor : TUIColor ) : TUIChunkList;
var iPosition : TUIPoint;
    iColor    : TUIColor;
begin
  iPosition := PointZero;
  iColor    := aColor;
  ChunkifyEx( Result, iPosition, iColor, aText, aColor, aDimensions );
end;

function TUIConsole.LinifyChunkList ( var aChunkList : TUIChunkList ) : TUIChunkBuffer;
var i,iStart,iy : Integer;
begin
  if Length( aChunkList ) = 0 then Exit(nil);
  Result := TUIChunkBuffer.Create( FRenderer.SizeY );
  iy     := 0;
  iStart := 0;
  for i := 0 to High( aChunkList ) do
  begin
    if aChunkList[i].Position.y > iy then
    begin
      if i-istart = 0 then
        Result.PushBack( nil )
      else
        Result.PushBack( Copy( aChunkList, iStart, i - iStart ) );
      iStart := i;
      iy := aChunkList[i].Position.y;
    end;
    aChunkList[i].Position.y := 0;
  end;
  Result.PushBack( Copy( aChunkList, iStart, High( aChunkList ) - iStart + 1 ) );
end;

end.

