{$MODE OBJFPC}
unit vsdlconsole;

interface

procedure SDLConsoleSetDrivers(aCols, aRows: Word);
procedure SDLConsoleSetTitle(const aLongTitle: AnsiString);

implementation

uses keyboard, video, viotypes, vsdl2library, vsdl2imagelibrary;

var
  BlinkStart: Uint32 = 0;

function TKeyRecordFromSDLEvent(Event: PSDL_Event): TKeyRecord;
const
  ShiftedNums: array[0..9] of char = (')', '!', '@', '#', '$', '%', '^', '&', '*', '(');
var
  ShiftState: Byte;
  Code: Integer;
begin
  ShiftState := 0;
  if (Event^.key.keysym.mod_ and KMOD_LSHIFT) <> 0 then ShiftState := ShiftState or kbLeftShift;
  if (Event^.key.keysym.mod_ and KMOD_RSHIFT) <> 0 then ShiftState := ShiftState or kbRightShift;
  if (Event^.key.keysym.mod_ and KMOD_CTRL) <> 0 then ShiftState := ShiftState or kbCtrl;
  if (Event^.key.keysym.mod_ and KMOD_ALT) <> 0 then ShiftState := ShiftState or kbAlt;

  Result.Flags := kbFnKey;
  Result.ShiftState := ShiftState;
  Result.KeyCode := 0;

  Code := Event^.key.keysym.sym;

  case Code of
    32..126:
      begin
        if (ShiftState and kbShift) <> 0 then
        begin
          case Code of
            Ord('a')..Ord('z'): Code := SDL_toupper(Code);
            Ord('0')..Ord('9'): Code := Ord(ShiftedNums[Code - SDLK_0]);
            Ord('`'): Code := Ord('~');
            Ord('-'): Code := Ord('_');
            Ord('='): Code := Ord('+');
            Ord('['): Code := Ord('{');
            Ord(']'): Code := Ord('}');
            Ord('\'): Code := Ord('|');
            Ord(';'): Code := Ord(':');
            Ord(''''): Code := Ord('"');
            Ord(','): Code := Ord('<');
            Ord('.'): Code := Ord('>');
            Ord('/'): Code := Ord('?');
          end;
        end;

        Result.Flags := kbASCII;
        Result.KeyCode := Word(Code);
      end;
    SDLK_RETURN: begin Result.Flags := kbASCII; Result.KeyCode := $1C0D; end;
    SDLK_ESCAPE: begin Result.Flags := kbASCII; Result.KeyCode := $011B; end;
    SDLK_BACKSPACE: begin Result.Flags := kbASCII; Result.KeyCode := $0E08; end;
    SDLK_TAB: begin Result.Flags := kbASCII; Result.KeyCode := $0F09; end;
    SDLK_INSERT: Result.KeyCode := kbdInsert;
    SDLK_DELETE: Result.KeyCode := kbdDelete;
    SDLK_HOME: Result.KeyCode := kbdHome;
    SDLK_END: Result.KeyCode := kbdEnd;
    SDLK_PAGEUP: Result.KeyCode := kbdPgUp;
    SDLK_PAGEDOWN: Result.KeyCode := kbdPgDn;
    SDLK_UP: Result.KeyCode := kbdUp;
    SDLK_DOWN: Result.KeyCode := kbdDown;
    SDLK_LEFT: Result.KeyCode := kbdLeft;
    SDLK_RIGHT: Result.KeyCode := kbdRight;
    SDLK_F1..SDLK_F12: Result.KeyCode := kbdF1 + Word(Code - SDLK_F1);
    SDLK_KP_1..SDLK_KP_0:
      begin
        if (Event^.key.keysym.mod_ and KMOD_NUM) <> 0 then
        begin
          if Code = SDLK_KP_0 then Code := Ord('0')
          else Code := Ord('1') + Ord(Code - SDLK_KP_1);

          Result.Flags := kbASCII;
          Result.KeyCode := Word(Code);
        end
        else
        begin
          case Code of
            SDLK_KP_1: Result.KeyCode := kbdEnd;
            SDLK_KP_2: Result.KeyCode := kbdDown;
            SDLK_KP_3: Result.KeyCode := kbdPgDn;
            SDLK_KP_4: Result.KeyCode := kbdLeft;
            SDLK_KP_5: Result.KeyCode := 19456;
            SDLK_KP_6: Result.KeyCode := kbdRight;
            SDLK_KP_7: Result.KeyCode := kbdHome;
            SDLK_KP_8: Result.KeyCode := kbdUp;
            SDLK_KP_9: Result.KeyCode := kbdPgUp;
            SDLK_KP_0: Result.KeyCode := kbdInsert;
          end;
        end;
      end;
    SDLK_KP_DIVIDE: begin Result.Flags := kbASCII; Result.KeyCode := Word('/'); end;
    SDLK_KP_MULTIPLY: begin Result.Flags := kbASCII; Result.KeyCode := Word('*'); end;
    SDLK_KP_MINUS: begin Result.Flags := kbASCII; Result.KeyCode := Word('-'); end;
    SDLK_KP_PLUS: begin Result.Flags := kbASCII; Result.KeyCode := Word('+'); end;
    SDLK_KP_ENTER: begin Result.Flags := kbASCII; Result.KeyCode := $1C0D; end;
    SDLK_KP_PERIOD: begin Result.Flags := kbASCII; Result.KeyCode := Word('.'); end;
    else
      begin
        { Ignore unhandled keys. }
        Result.Flags := 0;
        Result.ShiftState := 0;
        Result.KeyCode := 0;
      end;
  end;
end;

procedure ProcessSDLEvents;
var
  Event: SDL_Event;
  Peeped: Integer;
begin
  repeat
    SDL_PumpEvents;

    Peeped := SDL_PeepEvents(@Event, 1, SDL_PEEKEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT);
    if Peeped < 0 then
    begin
      SDL_Quit;
      raise EIOException.Create('SDL_PeepEvents for ProcessSDLEvents failed : ' + SDL_GetError());
    end;

    if Peeped > 0 then
    begin
      case Event.type_ of
        SDL_KEYDOWN:
          begin
            if SDL_ShowCursor(-1) = 1 then SDL_ShowCursor(0);
            BlinkStart := SDL_GetTicks();
          end;
        SDL_MOUSEMOTION:
          begin
            if SDL_ShowCursor(-1) = 0 then SDL_ShowCursor(1);
            { Drop the event and process more. }
            SDL_PollEvent(@Event);
            Peeped := -1;
          end;
        else
          begin
            { Drop the event and process more. }
            SDL_PollEvent(@Event);
            Peeped := -1;
          end;
      end;
    end;
  until Peeped >= 0;
end;

{ ================== }
{ SDLConsoleKeyboard }
{ ================== }

var
  QueuedKeyEvent: TKeyEvent = 0;

function SDLConsoleKeyboardGetKeyEvent: TKeyEvent;
var
  Event: SDL_Event;
begin
  while QueuedKeyEvent = 0 do
  begin
    ProcessSDLEvents;

    if SDL_WaitEvent(@Event) = 0 then
    begin
      SDL_Quit;
      raise EIOException.Create('SDL_WaitEvent failed : ' + SDL_GetError());
    end;

    if Event.type_ = SDL_KEYDOWN then
      QueuedKeyEvent := TKeyEvent(TKeyRecordFromSDLEvent(@Event));
  end;

  Result := QueuedKeyEvent;
  QueuedKeyEvent := 0;
end;

function SDLConsoleKeyboardPollKeyEvent: TKeyEvent;
var
  Event: SDL_Event;
begin
  if QueuedKeyEvent = 0 then
  begin
    ProcessSDLEvents;

    if (SDL_PollEvent(@Event) = 1) and (Event.type_ = SDL_KEYDOWN) then
      QueuedKeyEvent := TKeyEvent(TKeyRecordFromSDLEvent(@Event));
  end;

  Result := QueuedKeyEvent;
end;

function SDLConsoleKeyboardGetShiftState: Byte;
var
  KeyMods: SDL_KeyMod;
begin
  if QueuedKeyEvent <> 0 then
    Result := TKeyRecord(QueuedKeyEvent).ShiftState
  else
  begin
    KeyMods := SDL_GetModState();
    Result := 0;
    if (KeyMods and KMOD_LSHIFT) <> 0 then Result := Result or kbLeftShift;
    if (KeyMods and KMOD_RSHIFT) <> 0 then Result := Result or kbRightShift;
    if (KeyMods and KMOD_CTRL) <> 0 then Result := Result or kbCtrl;
    if (KeyMods and KMOD_ALT) <> 0 then Result := Result or kbAlt;
  end;
end;

function SDLConsoleKeyboardTranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;
begin
  Exit(KeyEvent);
end;

const
  SDLConsoleKeyboardDriver: TKeyboardDriver = (
    InitDriver: nil;
    DoneDriver: nil;
    GetKeyEvent: @SDLConsoleKeyboardGetKeyEvent;
    PollKeyEvent: @SDLConsoleKeyboardPollKeyEvent;
    GetShiftState: @SDLConsoleKeyboardGetShiftState;
    TranslateKeyEvent: @SDLConsoleKeyboardTranslateKeyEvent;
    TranslateKeyEventUniCode: @SDLConsoleKeyboardTranslateKeyEvent;
  );

procedure SDLConsoleSetKeyboardDriver;
begin
  SetKeyboardDriver(SDLConsoleKeyboardDriver);
end;

{ =============== }
{ SDLConsoleVideo }
{ =============== }

const
  Colors: array [0..15,0..2] of Uint8 = (
    (   0,   0,   0 ), { black }
    (   0,   0, 191 ), { dark blue }
    (  78, 154,   6 ), { dark green }
    (   6, 152, 154 ), { dark cyan }
    ( 204,   0,   0 ), { dark red }
    ( 117,  80, 123 ), { dark magenta }
    ( 196, 160,   0 ), { dark yellow }
    ( 192, 192, 192 ), { gray }
    (  85,  87,  83 ), { dark gray }
    ( 114, 159, 207 ), { blue }
    ( 138, 226,  52 ), { green }
    (  52, 226, 226 ), { cyan }
    ( 239,  41,  41 ), { red }
    ( 173, 127, 168 ), { magenta }
    ( 252, 233,  79 ), { yellow }
    ( 255, 255, 255 )  { white }
  );

var
  InitCols, InitRows: Word;
  ConsoleWindow: PSDL_Window;
  Renderer: PSDL_Renderer;
  CharWidth, CharHeight: Integer;
  FontAtlas: PSDL_Texture;
  CursorType: Word = crHidden;

procedure SDLConsoleVideoInitDriver;
var
  DisplayMode: TSDL_DisplayMode;
  LoadedFont, Font, MultiColorFont, FontAtlasSurface: PSDL_Surface;
  Src, Dest: SDL_Rect;
  FontScale: Integer;
  i: Integer;
begin
  LoadSDL2;
  LoadSDL2Image;

  if SDL_Init(SDL_INIT_VIDEO) < 0 then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_Init failed : ' + SDL_GetError());
  end;

  IMG_Init(IMG_INIT_PNG);

  if SDL_GetDesktopDisplayMode(0, @DisplayMode) < 0 then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_GetDesktopDisplayMode failed : ' + SDL_GetError());
  end;

  LoadedFont := IMG_Load(PChar('font_console.png'));
  if LoadedFont = nil then
  begin
    SDL_Quit;
    raise EIOException.Create('IMG_Load(''font_console.png'') failed : ' + SDL_GetError());
  end;

  { Scale up the final font texture for better render quality on larger screens. }
  FontScale := 1;
  if FontScale < (DisplayMode.w div (LoadedFont^.w div 64 * InitCols)) then
    FontScale := DisplayMode.w div (LoadedFont^.w div 64 * InitCols);
  if FontScale > (DisplayMode.h div (LoadedFont^.h div 4 * InitRows)) then
    FontScale := DisplayMode.h div (LoadedFont^.h div 4 * InitRows);

  CharWidth := LoadedFont^.w div 64 * FontScale;
  CharHeight := LoadedFont^.h div 4 * FontScale;

  { Create multicolor font surface with room for 16 color variations. }
  MultiColorFont := SDL_CreateRGBSurface(
    0, LoadedFont^.w, LoadedFont^.h * 16, 32, $FF000000, $00FF0000, $0000FF00, $000000FF);
  if MultiColorFont = nil then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_CreateRGBSurface for MultiColorFont failed : ' + SDL_GetError());
  end;

  { Convert font surface for blitting onto multicolor font surface. }
  Font := SDL_ConvertSurfaceFormat(LoadedFont, MultiColorFont^.format^.format, 0);
  SDL_FreeSurface(LoadedFont);
  LoadedFont := nil;
  if Font = nil then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_ConvertSurfaceFormat failed : ' + SDL_GetError());
  end;

  { Blit colored copies of the font onto the multicolor font surface. }
  for i := Low(Colors) to High(Colors) do
  begin
    if SDL_SetSurfaceColorMod(Font, Colors[i,0], Colors[i,1], Colors[i,2]) < 0 then
    begin
      SDL_Quit;
      raise EIOException.Create('SDL_SetSurfaceColorMod for Font failed : ' + SDL_GetError());
    end;

    Dest.x := 0;
    Dest.y := Font^.h * i;
    Dest.w := Font^.w;
    Dest.h := Font^.h;

    if SDL_UpperBlit(Font, nil, MultiColorFont, @Dest) < 0 then
    begin
      SDL_Quit;
      raise EIOException.Create('SDL_UpperBlit failed : ' + SDL_GetError());
    end;
  end;

  SDL_FreeSurface(Font);
  Font := nil;

  { Create final upscaled font atlas surface. }
  FontAtlasSurface := SDL_CreateRGBSurface(
    0, MultiColorFont^.w * FontScale, MultiColorFont^.h * FontScale, 32, $FF000000, $00FF0000,
    $0000FF00, $000000FF);
  if FontAtlasSurface = nil then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_CreateRGBSurface for FontAtlasSurface failed : ' + SDL_GetError());
  end;

  { Scale blit the multicolor font surface onto the font atlas surface. }
  Src.x := 0;
  Src.y := 0;
  Src.w := MultiColorFont^.w;
  Src.h := MultiColorFont^.h;
  Dest.x := 0;
  Dest.y := 0;
  Dest.w := FontAtlasSurface^.w;
  Dest.h := FontAtlasSurface^.h;
  if SDL_UpperBlitScaled(MultiColorFont, @Src, FontAtlasSurface, @Dest) < 0 then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_UpperBlitScaled failed : ' + SDL_GetError());
  end;

  SDL_FreeSurface(MultiColorFont);
  MultiColorFont := nil;

  ScreenWidth := InitCols;
  ScreenHeight := InitRows;

  ConsoleWindow := SDL_CreateWindow(
    'Valkyrie SDL Console Window', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    InitCols * CharWidth, InitRows * CharHeight, SDL_WINDOW_RESIZABLE);
  if ConsoleWindow = nil then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_CreateWindow failed : ' + SDL_GetError());
  end;

  Renderer := SDL_CreateRenderer(ConsoleWindow, -1, SDL_RENDERER_ACCELERATED);
  if Renderer = nil then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_CreateRenderer failed : ' + SDL_GetError());
  end;

  if SDL_RenderSetLogicalSize(Renderer, InitCols * CharWidth, InitRows * CharHeight) < 0 then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_RenderSetLogicalSize failed : ' + SDL_GetError());
  end;

  FontAtlas := SDL_CreateTextureFromSurface(Renderer, FontAtlasSurface);
  SDL_FreeSurface(FontAtlasSurface);
  FontAtlasSurface := nil;
  if FontAtlas = nil then
  begin
    SDL_Quit;
    raise EIOException.Create('SDL_CreateTextureFromSurface failed : ' + SDL_GetError());
  end;
end;

procedure SDLConsoleVideoDoneDriver;
begin
  SDL_DestroyTexture(FontAtlas);
  SDL_DestroyRenderer(Renderer);
  SDL_DestroyWindow(ConsoleWindow);
  IMG_Quit;
end;

procedure SDLConsoleVideoUpdateScreen(Force: Boolean);
var
  i, tmp: Integer;
  x, y: Integer;
  c, fg, bg, blnk: Integer;
  BlinkNow: Boolean;
  Src, Dest: SDL_Rect;
begin
  SDL_SetRenderDrawColor(Renderer, 0, 0, 0, 255);
  SDL_RenderClear(Renderer);

  BlinkNow := (((SDL_GetTicks() - BlinkStart) div 500) and 1) = 0;

  Src.w := CharWidth;
  Src.h := CharHeight;
  Dest.w := CharWidth;
  Dest.h := CharHeight;

  for i := 0 to (VideoBufSize div SizeOf(TVideoCell)) - 1 do
  begin
    x := i mod ScreenWidth;
    y := i div ScreenWidth;

    c := VideoBuf^[i] and $00FF;
    fg := (VideoBuf^[i] shr 8) and $F;
    bg := (VideoBuf^[i] shr 12) and $7;
    blnk := (VideoBuf^[i] shr 15) and 1;

    if BlinkNow then
    begin
      { Swap foreground and background colors at cursor position. }
      if (x = CursorX) and (y = CursorY) and (CursorType <> crHidden) then
      begin
        tmp := fg;
        fg := bg;
        bg := tmp;
      end;

      if blnk = 1 then fg := fg xor $8;
    end;

    Dest.x := x * CharWidth;
    Dest.y := y * CharHeight;

    { If background isn't black, render filled block character with the background color. }
    if bg > 0 then
    begin
      Src.x := 27 * CharWidth;
      Src.y := (3 + bg * 4) * CharHeight;

      if SDL_RenderCopy(Renderer, FontAtlas, @Src, @Dest) < 0 then
      begin
        SDL_Quit;
        raise EIOException.Create('SDL_RenderCopy of bg failed : ' + SDL_GetError());
      end;
    end;

    { If character isn't space, render character with foreground color. }
    if c <> 32 then
    begin
      Src.x := (c mod 64) * CharWidth;
      Src.y := ((c div 64) + fg * 4) * CharHeight;

      if SDL_RenderCopy(Renderer, FontAtlas, @Src, @Dest) < 0 then
      begin
        SDL_Quit;
        raise EIOException.Create('SDL_RenderCopy of fg-colored char failed : ' + SDL_GetError());
      end;
    end;
  end;

  SDL_RenderPresent(Renderer);
end;

procedure SDLConsoleVideoSetCursorPos(NewCursorX: Word; NewCursorY: Word);
begin
  CursorX := NewCursorX;
  CursorY := NewCursorY;
end;

function SDLConsoleVideoGetCursorType: Word;
begin
  Exit(CursorType);
end;

procedure SDLConsoleVideoSetCursorType(NewType: Word);
begin
  CursorType := NewType;
end;

function SDLConsoleVideoGetCapabilities: Word;
begin
  Exit(cpBlink or cpColor);
end;

const
  SDLConsoleVideoDriver: TVideoDriver = (
    InitDriver: @SDLConsoleVideoInitDriver;
    DoneDriver: @SDLConsoleVideoDoneDriver;
    UpdateScreen: @SDLConsoleVideoUpdateScreen;
    ClearScreen: nil;
    SetVideoMode: nil;
    GetVideoModeCount: nil;
    GetVideoModeData: nil;
    SetCursorPos: @SDLConsoleVideoSetCursorPos;
    GetCursorType: @SDLConsoleVideoGetCursorType;
    SetCursorType: @SDLConsoleVideoSetCursorType;
    GetCapabilities: @SDLConsoleVideoGetCapabilities;
  );

procedure SDLConsoleSetVideoDriver(aCols, aRows: Word);
begin
  InitCols := aCols;
  InitRows := aRows;
  SetVideoDriver(SDLConsoleVideoDriver);
end;

{ vsdlconsole }

procedure SDLConsoleSetDrivers(aCols, aRows: Word);
begin
  SDLConsoleSetVideoDriver(aCols, aRows);
  SDLConsoleSetKeyboardDriver;
end;

procedure SDLConsoleSetTitle(const aLongTitle: AnsiString);
begin
  SDL_SetWindowTitle(ConsoleWindow, PChar(aLongTitle));
end;

end.
