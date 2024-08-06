unit vsdl2ttflibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary, vsdl2library;

type TSDLTTFException = class( Exception );

const
{$IFDEF WINDOWS}
  SDLTTFDefaultPath = 'SDL2_ttf.dll';
{$ELSE}
  {$IFDEF DARWIN}
  SDLTTFDefaultPath = 'SDL2_ttf.framework/SDL2_ttf';
  {$ELSE}
  SDLTTFDefaultPath = 'libSDL2_ttf-2.0.so.0';
  {$ENDIF}
{$ENDIF}

const
  SDL2_TTF_MAJOR_VERSION = 2;
  SDL2_TTF_MINOR_VERSION = 21;
  SDL2_TTF_PATCHLEVEL    = 0;

const
  UNICODE_BOM_NATIVE        = $FEFF;
  UNICODE_BOM_SWAPPED       = $FFFE;

  TTF_STYLE_NORMAL          = $00;
  TTF_STYLE_BOLD            = $01;
  TTF_STYLE_ITALIC          = $02;
  TTF_STYLE_UNDERLINE       = $04;
  TTF_STYLE_STRIKETHROUGH   = $08;

  TTF_HINTING_NORMAL        = 0;
  TTF_HINTING_LIGHT         = 1;
  TTF_HINTING_MONO          = 2;
  TTF_HINTING_NONE          = 3;
  TTF_HINTING_LIGHT_SUBPIXEL= 4;

  TTF_WRAPPED_ALIGN_LEFT    = 0;
  TTF_WRAPPED_ALIGN_CENTER  = 1;
  TTF_WRAPPED_ALIGN_RIGHT   = 2;

  TTF_DIRECTION_LTR         = 0;
  TTF_DIRECTION_RTL         = 1;
  TTF_DIRECTION_TTB         = 2;
  TTF_DIRECTION_BTT         = 3;

type
  PTTF_Font = ^TTTF_font;
  TTTF_Font = record end;

var
  TTF_Linked_Version          : function: PSDL_version; cdecl;

  TTF_GetFreeTypeVersion      : procedure(major, minor, patch: PInteger); cdecl;
  TTF_GetHarfBuzzVersion      : procedure(major, minor, patch: PInteger); cdecl;
  TTF_ByteSwappedUNICODE      : procedure(swapped: SDL_bool); cdecl;
  TTF_Init                    : function: Integer; cdecl;
  TTF_OpenFont                : function(const file_: PChar; ptsize: Integer): PTTF_Font; cdecl;
  TTF_OpenFontIndex           : function(const file_: PChar; ptsize: Integer; index: LongInt): PTTF_Font; cdecl;
  TTF_OpenFontRW              : function(src: PSDL_RWops; freesrc, ptsize: Integer): PTTF_Font; cdecl;
  TTF_OpenFontIndexRW         : function(src: PSDL_RWops; freesrc, ptsize: Integer; index: LongInt): PTTF_Font; cdecl;
  TTF_OpenFontDPI             : function(const file_: PChar; ptsize: Integer; hdpi, vdpi: UInt32): PTTF_Font; cdecl;
  TTF_OpenFontIndexDPI        : function(const file_: PChar; ptsize: Integer; index: LongInt; hdpi, vdpi: UInt32): PTTF_Font; cdecl;
  TTF_OpenFontDPIRW           : function(src: PSDL_RWops; freesrc, ptsize: Integer; hdpi, vdpi: UInt32): PTTF_Font; cdecl;
  TTF_OpenFontIndexDPIRW      : function(src: PSDL_RWops; freesrc, ptsize: Integer; index: LongInt; hdpi, vdpi: UInt32): PTTF_Font; cdecl;
  TTF_SetFontSize             : function(font: PTTF_Font; ptsize: Integer): Integer; cdecl;
  TTF_SetFontSizeDPI          : function(font: PTTF_Font; ptsize: Integer; hdpi, vdpi: UInt32): Integer; cdecl;

  TTF_GetFontStyle            : function(const font: PTTF_Font): Integer; cdecl;
  TTF_SetFontStyle            : procedure(font: PTTF_Font; style: Integer); cdecl;
  TTF_GetFontOutline          : function(const font: PTTF_Font): Integer; cdecl;
  TTF_SetFontOutline          : procedure(font: PTTF_Font; outline: Integer); cdecl;

  TTF_GetFontHinting          : function(const font: PTTF_Font): Integer; cdecl;
  TTF_SetFontHinting          : procedure(font: PTTF_Font; hinting: Integer); cdecl;

  TTF_GetFontWrappedAlign     : function(const font: PTTF_Font): Integer; cdecl;
  TTF_SetFontWrappedAlign     : procedure(font: PTTF_Font; align: Integer); cdecl;

  TTF_FontHeight              : function(const font: PTTF_Font): Integer; cdecl;
  TTF_FontAscent              : function(const font: PTTF_Font): Integer; cdecl;
  TTF_FontDescent             : function(const font: PTTF_Font): Integer; cdecl;
  TTF_FontLineSkip            : function(const font: PTTF_Font): Integer; cdecl;

  TTF_GetFontKerning          : function(const font: PTTF_Font): Integer; cdecl;
  TTF_SetFontKerning          : procedure(font: PTTF_Font; allowed: Integer); cdecl;

  TTF_FontFaces               : function(const font: PTTF_Font): LongInt; cdecl;
  TTF_FontFaceIsFixedWidth    : function(const font: PTTF_Font): Integer; cdecl;
  TTF_FontFaceFamilyName      : function(const font: PTTF_Font): PChar; cdecl;
  TTF_FontFaceStyleName       : function(const font: PTTF_Font): PChar; cdecl;

  TTF_GlyphIsProvided         : function(font: PTTF_Font; ch: Uint16): Integer; cdecl;
  TTF_GlyphIsProvided32       : function(font: PTTF_Font; ch: Uint32): Integer; cdecl;

  TTF_GlyphMetrics            : function(font: PTTF_Font; ch: Uint16; minx, maxx, miny, maxy, advance: PInteger): Integer; cdecl;
  TTF_GlyphMetrics32          : function(font: PTTF_Font; ch: Uint32; minx, maxx, miny, maxy, advance: PInteger): Integer; cdecl;

  TTF_SizeText                : function(font: PTTF_Font; const text: PChar; w, h: PInteger): Integer; cdecl;
  TTF_SizeUTF8                : function(font: PTTF_Font; const text: PChar; w, h: PInteger): Integer; cdecl;
  TTF_SizeUNICODE             : function(font: PTTF_Font; const text: PUint16; w, h: PInteger): Integer; cdecl;

  TTF_MeasureText             : function(font: PTTF_Font; const text: PChar; measure_width: Integer; extent, count: PInteger): Integer; cdecl;
  TTF_MeasureUTF8             : function(font: PTTF_Font; const text: PChar; measure_width: Integer; extent, count: PInteger): Integer; cdecl;
  TTF_MeasureUNICODE          : function(font: PTTF_Font; const text: PUint16; measure_width: Integer; extent, count: PInteger): Integer; cdecl;

  TTF_RenderText_Solid        : function(font: PTTF_Font; const text: PChar; fg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUTF8_Solid        : function(font: PTTF_Font; const text: PChar; fg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_Solid     : function(font: PTTF_Font; const text: PUint16; fg: SDL_Color): PSDL_Surface; cdecl;

  TTF_RenderText_Solid_Wrapped        : function(font: PTTF_Font; const text: PChar; fg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUTF8_Solid_Wrapped        : function(font: PTTF_Font; const text: PChar; fg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_Solid_Wrapped     : function(font: PTTF_Font; const text: PUint16; fg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;

  TTF_RenderGlyph_Solid               : function(font: PTTF_Font; ch: Uint16; fg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderGlyph32_Solid             : function(font: PTTF_Font; ch: Uint32; fg: SDL_Color): PSDL_Surface; cdecl;

  TTF_RenderText_Shaded               : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUTF8_Shaded               : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_Shaded            : function(font: PTTF_Font; const text: PUint16; fg, bg: SDL_Color): PSDL_Surface; cdecl;

  TTF_RenderText_Shaded_Wrapped       : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUTF8_Shaded_Wrapped       : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_Shaded_Wrapped    : function(font: PTTF_Font; const text: PUint16; fg, bg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;

  TTF_RenderGlyph_Shaded              : function(font: PTTF_Font; ch: Uint16; fg, bg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderGlyph32_Shaded            : function(font: PTTF_Font; ch: Uint32; fg, bg: SDL_Color): PSDL_Surface; cdecl;

  TTF_RenderText_Blended              : function(font: PTTF_Font; const text: PChar; fg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUTF8_Blended              : function(font: PTTF_Font; const text: PChar; fg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_Blended           : function(font: PTTF_Font; const text: PUint16; fg: SDL_Color): PSDL_Surface; cdecl;

  TTF_RenderText_Blended_Wrapped      : function(font: PTTF_Font; const text: PChar; fg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUTF8_Blended_Wrapped      : function(font: PTTF_Font; const text: PChar; fg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_Blended_Wrapped   : function(font: PTTF_Font; const text: PUint16; fg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;

  TTF_RenderGlyph_Blended             : function(font: PTTF_Font; ch: Uint16; fg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderGlyph32_Blended           : function(font: PTTF_Font; ch: Uint32; fg: SDL_Color): PSDL_Surface; cdecl;

  TTF_RenderText_LCD                  : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUTF8_LCD                  : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_LCD               : function(font: PTTF_Font; const text: PUint16; fg, bg: SDL_Color): PSDL_Surface; cdecl;

  TTF_RenderText_LCD_Wrapped          : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUTF8_LCD_Wrapped          : function(font: PTTF_Font; const text: PChar; fg, bg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;
  TTF_RenderUNICODE_LCD_Wrapped       : function(font: PTTF_Font; const text: PUint16; fg, bg: SDL_Color; wrapLength: Uint32): PSDL_Surface; cdecl;

  TTF_RenderGlyph_LCD                 : function(font: PTTF_Font; ch: Uint16; fg, bg: SDL_Color): PSDL_Surface; cdecl;
  TTF_RenderGlyph32_LCD               : function(font: PTTF_Font; ch: Uint32; fg, bg: SDL_Color): PSDL_Surface; cdecl;

  TTF_CloseFont                       : procedure(font: PTTF_Font); cdecl;
  TTF_Quit                            : procedure; cdecl;
  TTF_WasInit                         : function: Integer; cdecl;

  TTF_GetFontKerningSize              : function(font: PTTF_Font; prev_index, index: Integer): Integer; cdecl;
  TTF_GetFontKerningSizeGlyphs        : function(font: PTTF_Font; previous_ch, ch: Uint16): Integer; cdecl;
  TTF_GetFontKerningSizeGlyphs32      : function(font: PTTF_Font; previous_ch, ch: Uint32): Integer; cdecl;

  TTF_SetFontSDF                      : function(font: PTTF_Font; on_off: SDL_bool): Integer; cdecl;
  TTF_GetFontSDF                      : function(const font: PTTF_Font): SDL_bool; cdecl;

  TTF_SetFontDirection                : function(font: PTTF_Font; direction: Integer): Integer; cdecl;
  TTF_SetFontScriptName               : function(font: PTTF_Font; const script: PChar): Integer; cdecl;

  // Deprecated functions
  TTF_SetDirection                    : function(direction: Integer): Integer; cdecl;
  TTF_SetScript                       : function(script: Integer): Integer; cdecl;

var
  SDL2_ttf : TLibrary = nil;

function LoadSDL2TTF( const aPath : AnsiString = SDLTTFDefaultPath ) : Boolean;

implementation

function LoadSDL2TTF ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL2_ttf.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL2_ttf : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL2 = nil then LoadSDL2();
  if SDL2 = nil then Exit( False );
  if SDL2_ttf <> nil then Exit( True );
  SDL2_ttf := TLibrary.Load( aPath );
  if SDL2_ttf = nil then Exit( False );

  Pointer(TTF_Linked_Version) := GetSymbol('TTF_Linked_Version');
  Pointer(TTF_GetFreeTypeVersion) := GetSymbol('TTF_GetFreeTypeVersion');
  Pointer(TTF_GetHarfBuzzVersion) := GetSymbol('TTF_GetHarfBuzzVersion');
  Pointer(TTF_ByteSwappedUNICODE) := GetSymbol('TTF_ByteSwappedUNICODE');
  Pointer(TTF_Init) := GetSymbol('TTF_Init');
  Pointer(TTF_OpenFont) := GetSymbol('TTF_OpenFont');
  Pointer(TTF_OpenFontIndex) := GetSymbol('TTF_OpenFontIndex');
  Pointer(TTF_OpenFontRW) := GetSymbol('TTF_OpenFontRW');
  Pointer(TTF_OpenFontIndexRW) := GetSymbol('TTF_OpenFontIndexRW');
  Pointer(TTF_OpenFontDPI) := GetSymbol('TTF_OpenFontDPI');
  Pointer(TTF_OpenFontIndexDPI) := GetSymbol('TTF_OpenFontIndexDPI');
  Pointer(TTF_OpenFontDPIRW) := GetSymbol('TTF_OpenFontDPIRW');
  Pointer(TTF_OpenFontIndexDPIRW) := GetSymbol('TTF_OpenFontIndexDPIRW');
  Pointer(TTF_SetFontSize) := GetSymbol('TTF_SetFontSize');
  Pointer(TTF_SetFontSizeDPI) := GetSymbol('TTF_SetFontSizeDPI');

  Pointer(TTF_GetFontStyle) := GetSymbol('TTF_GetFontStyle');
  Pointer(TTF_SetFontStyle) := GetSymbol('TTF_SetFontStyle');
  Pointer(TTF_GetFontOutline) := GetSymbol('TTF_GetFontOutline');
  Pointer(TTF_SetFontOutline) := GetSymbol('TTF_SetFontOutline');

  Pointer(TTF_GetFontHinting) := GetSymbol('TTF_GetFontHinting');
  Pointer(TTF_SetFontHinting) := GetSymbol('TTF_SetFontHinting');

  Pointer(TTF_GetFontWrappedAlign) := GetSymbol('TTF_GetFontWrappedAlign');
  Pointer(TTF_SetFontWrappedAlign) := GetSymbol('TTF_SetFontWrappedAlign');
  Pointer(TTF_FontHeight) := GetSymbol('TTF_FontHeight');
  Pointer(TTF_FontAscent) := GetSymbol('TTF_FontAscent');
  Pointer(TTF_FontDescent) := GetSymbol('TTF_FontDescent');
  Pointer(TTF_FontLineSkip) := GetSymbol('TTF_FontLineSkip');
  Pointer(TTF_GetFontKerning) := GetSymbol('TTF_GetFontKerning');
  Pointer(TTF_SetFontKerning) := GetSymbol('TTF_SetFontKerning');
  Pointer(TTF_FontFaces) := GetSymbol('TTF_FontFaces');
  Pointer(TTF_FontFaceIsFixedWidth) := GetSymbol('TTF_FontFaceIsFixedWidth');
  Pointer(TTF_FontFaceFamilyName) := GetSymbol('TTF_FontFaceFamilyName');
  Pointer(TTF_FontFaceStyleName) := GetSymbol('TTF_FontFaceStyleName');
  Pointer(TTF_GlyphIsProvided) := GetSymbol('TTF_GlyphIsProvided');
  Pointer(TTF_GlyphIsProvided32) := GetSymbol('TTF_GlyphIsProvided32');
  Pointer(TTF_GlyphMetrics) := GetSymbol('TTF_GlyphMetrics');
  Pointer(TTF_GlyphMetrics32) := GetSymbol('TTF_GlyphMetrics32');
  Pointer(TTF_SizeText) := GetSymbol('TTF_SizeText');
  Pointer(TTF_SizeUTF8) := GetSymbol('TTF_SizeUTF8');
  Pointer(TTF_SizeUNICODE) := GetSymbol('TTF_SizeUNICODE');
  Pointer(TTF_MeasureText) := GetSymbol('TTF_MeasureText');
  Pointer(TTF_MeasureUTF8) := GetSymbol('TTF_MeasureUTF8');
  Pointer(TTF_MeasureUNICODE) := GetSymbol('TTF_MeasureUNICODE');
  Pointer(TTF_RenderText_Solid) := GetSymbol('TTF_RenderText_Solid');
  Pointer(TTF_RenderUTF8_Solid) := GetSymbol('TTF_RenderUTF8_Solid');
  Pointer(TTF_RenderUNICODE_Solid) := GetSymbol('TTF_RenderUNICODE_Solid');
  Pointer(TTF_RenderText_Solid_Wrapped) := GetSymbol('TTF_RenderText_Solid_Wrapped');
  Pointer(TTF_RenderUTF8_Solid_Wrapped) := GetSymbol('TTF_RenderUTF8_Solid_Wrapped');
  Pointer(TTF_RenderUNICODE_Solid_Wrapped) := GetSymbol('TTF_RenderUNICODE_Solid_Wrapped');
  Pointer(TTF_RenderGlyph_Solid) := GetSymbol('TTF_RenderGlyph_Solid');
  Pointer(TTF_RenderGlyph32_Solid) := GetSymbol('TTF_RenderGlyph32_Solid');
  Pointer(TTF_RenderText_Shaded) := GetSymbol('TTF_RenderText_Shaded');
  Pointer(TTF_RenderUTF8_Shaded) := GetSymbol('TTF_RenderUTF8_Shaded');
  Pointer(TTF_RenderUNICODE_Shaded) := GetSymbol('TTF_RenderUNICODE_Shaded');
  Pointer(TTF_RenderText_Shaded_Wrapped) := GetSymbol('TTF_RenderText_Shaded_Wrapped');
  Pointer(TTF_RenderUTF8_Shaded_Wrapped) := GetSymbol('TTF_RenderUTF8_Shaded_Wrapped');
  Pointer(TTF_RenderUNICODE_Shaded_Wrapped) := GetSymbol('TTF_RenderUNICODE_Shaded_Wrapped');
  Pointer(TTF_RenderGlyph_Shaded) := GetSymbol('TTF_RenderGlyph_Shaded');
  Pointer(TTF_RenderGlyph32_Shaded) := GetSymbol('TTF_RenderGlyph32_Shaded');
  Pointer(TTF_RenderText_Blended) := GetSymbol('TTF_RenderText_Blended');
  Pointer(TTF_RenderUTF8_Blended) := GetSymbol('TTF_RenderUTF8_Blended');
  Pointer(TTF_RenderUNICODE_Blended) := GetSymbol('TTF_RenderUNICODE_Blended');
  Pointer(TTF_RenderText_Blended_Wrapped) := GetSymbol('TTF_RenderText_Blended_Wrapped');
  Pointer(TTF_RenderUTF8_Blended_Wrapped) := GetSymbol('TTF_RenderUTF8_Blended_Wrapped');
  Pointer(TTF_RenderUNICODE_Blended_Wrapped) := GetSymbol('TTF_RenderUNICODE_Blended_Wrapped');
  Pointer(TTF_RenderGlyph_Blended) := GetSymbol('TTF_RenderGlyph_Blended');
  Pointer(TTF_RenderGlyph32_Blended) := GetSymbol('TTF_RenderGlyph32_Blended');
  Pointer(TTF_RenderText_LCD) := GetSymbol('TTF_RenderText_LCD');
  Pointer(TTF_RenderUTF8_LCD) := GetSymbol('TTF_RenderUTF8_LCD');
  Pointer(TTF_RenderUNICODE_LCD) := GetSymbol('TTF_RenderUNICODE_LCD');
  Pointer(TTF_RenderText_LCD_Wrapped) := GetSymbol('TTF_RenderText_LCD_Wrapped');
  Pointer(TTF_RenderUTF8_LCD_Wrapped) := GetSymbol('TTF_RenderUTF8_LCD_Wrapped');
  Pointer(TTF_RenderUNICODE_LCD_Wrapped) := GetSymbol('TTF_RenderUNICODE_LCD_Wrapped');
  Pointer(TTF_RenderGlyph_LCD) := GetSymbol('TTF_RenderGlyph_LCD');
  Pointer(TTF_RenderGlyph32_LCD) := GetSymbol('TTF_RenderGlyph32_LCD');
  Pointer(TTF_CloseFont) := GetSymbol('TTF_CloseFont');
  Pointer(TTF_Quit) := GetSymbol('TTF_Quit');
  Pointer(TTF_WasInit) := GetSymbol('TTF_WasInit');

  Pointer(TTF_SetDirection) := GetSymbol('TTF_SetDirection');
  Pointer(TTF_SetScript) := GetSymbol('TTF_SetScript');
  Pointer(TTF_SetFontDirection) := GetSymbol('TTF_SetFontDirection');
  Pointer(TTF_SetFontScriptName) := GetSymbol('TTF_SetFontScriptName');

  Pointer(TTF_GetFontKerningSize) := GetSymbol('TTF_GetFontKerningSize');
  Pointer(TTF_GetFontKerningSizeGlyphs) := GetSymbol('TTF_GetFontKerningSizeGlyphs');
  Pointer(TTF_GetFontKerningSizeGlyphs32) := GetSymbol('TTF_GetFontKerningSizeGlyphs32');
  Pointer(TTF_SetFontSDF) := GetSymbol('TTF_SetFontSDF');
  Pointer(TTF_GetFontSDF) := GetSymbol('TTF_GetFontSDF');

  Exit( True );
end;

finalization
  if SDL2_ttf <> nil then FreeAndNil( SDL2_ttf );

end.

