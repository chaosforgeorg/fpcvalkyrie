unit vsdlttflibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, Types, vlibrary, vsdllibrary;

type TSDLTTFException = class( Exception );

const
{$IFDEF WINDOWS}
  SDLTTFDefaultPath = 'SDL_ttf.dll';
{$ELSE}
  {$IFDEF DARWIN}
  SDLTTFDefaultPath = 'SDL_ttf.framework/SDL_ttf';
  {$ELSE}
  SDLTTFDefaultPath = 'libSDL_ttf-2.0.so.0';
  {$ENDIF}
{$ENDIF}

const
  SDL_TTF_MAJOR_VERSION = 2;
  SDL_TTF_MINOR_VERSION = 0;
  SDL_TTF_PATCHLEVEL    = 9;

const
  TTF_STYLE_NORMAL	  = $00;
  TTF_STYLE_BOLD      = $01;
  TTF_STYLE_ITALIC	  = $02;
  TTF_STYLE_UNDERLINE	= $04;

  UNICODE_BOM_NATIVE  = $FEFF;
  UNICODE_BOM_SWAPPED = $FFFE;

type
  PTTF_Font = ^TTTF_font;
  TTTF_Font = record
  end;


{$DEFINE calldecl := cdecl}

var
  TTF_Linked_Version        : function : PSDL_version; calldecl;
  TTF_ByteSwappedUNICODE    : procedure ( swapped : integer ); calldecl;
  TTF_Init                  : function : integer; calldecl;
  TTF_OpenFont              : function ( const filename : Pchar; ptsize : integer ) : PTTF_Font; calldecl;
  TTF_OpenFontIndex         : function ( const filename : Pchar; ptsize : integer; index : Longint ): PTTF_Font; calldecl;
  TTF_OpenFontRW            : function ( src : PSDL_RWops; freesrc : integer; ptsize : integer ): PTTF_Font; calldecl;
  TTF_OpenFontIndexRW       : function ( src : PSDL_RWops; freesrc : integer; ptsize : integer; index : Longint ): PTTF_Font; calldecl;
  TTF_GetFontStyle          : function ( font : PTTF_Font) : integer; calldecl;
  TTF_SetFontStyle          : procedure ( font : PTTF_Font; style : integer ); calldecl;
  TTF_FontHeight            : function ( font : PTTF_Font ) : Integer; calldecl;
  TTF_FontAscent            : function ( font : PTTF_Font ) : Integer; calldecl;
  TTF_FontDescent           : function ( font : PTTF_Font ) : Integer; calldecl;
  TTF_FontLineSkip          : function ( font : PTTF_Font ): Integer; calldecl;
  TTF_FontFaces             : function ( font : PTTF_Font ) : Longint; calldecl;
  TTF_FontFaceIsFixedWidth  : function ( font : PTTF_Font ): Integer; calldecl;
  TTF_FontFaceFamilyName    : function ( font : PTTF_Font ): PChar; calldecl;
  TTF_FontFaceStyleName     : function ( font : PTTF_Font ): PChar; calldecl;
  TTF_GlyphMetrics          : function ( font : PTTF_Font; ch : Uint16; var minx : integer; var maxx : integer; var miny : integer; var maxy : integer; var advance : integer ): Integer; calldecl;
  TTF_SizeText              : function ( font : PTTF_Font; const text : PChar; var w : integer; var y : integer ): Integer; calldecl;
  TTF_SizeUTF8              : function ( font : PTTF_Font; const text : PChar; var w : integer; var y : integer): Integer; calldecl;
  TTF_SizeUNICODE           : function ( font : PTTF_Font; const text : PUint16; var w : integer; var y : integer): Integer; calldecl;
  TTF_RenderText_Solid      : function ( font : PTTF_Font;const text : PChar; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderUTF8_Solid      : function ( font : PTTF_Font;const text : PChar; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderUNICODE_Solid   : function ( font : PTTF_Font;	const text :PUint16; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderGlyph_Solid     : function ( font : PTTF_Font; ch : Uint16; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderText_Shaded     : function ( font : PTTF_Font;	const text : PChar; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderUTF8_Shaded     : function ( font : PTTF_Font;	const text : PChar; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderUNICODE_Shaded  : function ( font : PTTF_Font; const text : PUint16; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderGlyph_Shaded    : function ( font : PTTF_Font; ch : Uint16; fg : TSDL_Color; bg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderText_Blended    : function ( font : PTTF_Font;	const text : PChar; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderUTF8_Blended    : function ( font : PTTF_Font; const text : PChar; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderUNICODE_Blended : function ( font : PTTF_Font; const text: PUint16; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_RenderGlyph_Blended   : function ( font : PTTF_Font; ch : Uint16; fg : TSDL_Color ): PSDL_Surface; calldecl;
  TTF_CloseFont             : procedure ( font : PTTF_Font ); calldecl;
  TTF_Quit                  : procedure; calldecl;
  TTF_WasInit               : function : integer; calldecl;

procedure SDL_TTF_VERSION( var X : TSDL_Version );
procedure TTF_SetError( fmt : PChar );
function TTF_GetError : PChar;

function TTF_OpenFontOrThrow(const _file: PChar; ptsize : Integer): PTTF_Font;
function TTF_OpenFontRWOrThrow(src: PSDL_RWops; freesrc: Integer; ptsize : Integer): PTTF_Font;

var
  SDL_ttf : TLibrary = nil;

function LoadSDLTTF( const aPath : AnsiString = SDLTTFDefaultPath ) : Boolean;

implementation

function LoadSDLTTF ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL_ttf.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL_ttf : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL = nil then LoadSDL();
  if SDL = nil then Exit( False );
  if SDL_ttf <> nil then Exit( True );
  SDL_ttf := TLibrary.Load( aPath );
  if SDL_ttf = nil then Exit( False );

  Pointer(TTF_Linked_Version)         := GetSymbol('TTF_Linked_Version');
  Pointer(TTF_ByteSwappedUNICODE)     := GetSymbol('TTF_ByteSwappedUNICODE');
  Pointer(TTF_Init)                   := GetSymbol('TTF_Init');
  Pointer(TTF_OpenFont)               := GetSymbol('TTF_OpenFont');
  Pointer(TTF_OpenFontIndex)          := GetSymbol('TTF_OpenFontIndex');
  Pointer(TTF_OpenFontRW)             := GetSymbol('TTF_OpenFontRW');
  Pointer(TTF_OpenFontIndexRW)        := GetSymbol('TTF_OpenFontIndexRW');
  Pointer(TTF_GetFontStyle)           := GetSymbol('TTF_GetFontStyle');
  Pointer(TTF_SetFontStyle)           := GetSymbol('TTF_SetFontStyle');
  Pointer(TTF_FontHeight)             := GetSymbol('TTF_FontHeight');
  Pointer(TTF_FontAscent)             := GetSymbol('TTF_FontAscent');
  Pointer(TTF_FontDescent)            := GetSymbol('TTF_FontDescent');
  Pointer(TTF_FontLineSkip)           := GetSymbol('TTF_FontLineSkip');
  Pointer(TTF_FontFaces)              := GetSymbol('TTF_FontFaces');
  Pointer(TTF_FontFaceIsFixedWidth)   := GetSymbol('TTF_FontFaceIsFixedWidth');
  Pointer(TTF_FontFaceFamilyName)     := GetSymbol('TTF_FontFaceFamilyName');
  Pointer(TTF_FontFaceStyleName)      := GetSymbol('TTF_FontFaceStyleName');
  Pointer(TTF_GlyphMetrics)           := GetSymbol('TTF_GlyphMetrics');
  Pointer(TTF_SizeText)               := GetSymbol('TTF_SizeText');
  Pointer(TTF_SizeUTF8)               := GetSymbol('TTF_SizeUTF8');
  Pointer(TTF_SizeUNICODE)            := GetSymbol('TTF_SizeUNICODE');
  Pointer(TTF_RenderText_Solid)       := GetSymbol('TTF_RenderText_Solid');
  Pointer(TTF_RenderUTF8_Solid)       := GetSymbol('TTF_RenderUTF8_Solid');
  Pointer(TTF_RenderUNICODE_Solid)    := GetSymbol('TTF_RenderUNICODE_Solid');
  Pointer(TTF_RenderGlyph_Solid)      := GetSymbol('TTF_RenderGlyph_Solid');
  Pointer(TTF_RenderText_Shaded)      := GetSymbol('TTF_RenderText_Shaded');
  Pointer(TTF_RenderUTF8_Shaded)      := GetSymbol('TTF_RenderUTF8_Shaded');
  Pointer(TTF_RenderUNICODE_Shaded)   := GetSymbol('TTF_RenderUNICODE_Shaded');
  Pointer(TTF_RenderGlyph_Shaded)     := GetSymbol('TTF_RenderGlyph_Shaded');
  Pointer(TTF_RenderText_Blended)     := GetSymbol('TTF_RenderText_Blended');
  Pointer(TTF_RenderUTF8_Blended)     := GetSymbol('TTF_RenderUTF8_Blended');
  Pointer(TTF_RenderUNICODE_Blended)  := GetSymbol('TTF_RenderUNICODE_Blended');
  Pointer(TTF_RenderGlyph_Blended)    := GetSymbol('TTF_RenderGlyph_Blended');
  Pointer(TTF_CloseFont)              := GetSymbol('TTF_CloseFont');
  Pointer(TTF_Quit)                   := GetSymbol('TTF_Quit');
  Pointer(TTF_WasInit)                := GetSymbol('TTF_WasInit');

  Exit( True );
end;

procedure SDL_TTF_VERSION( var X : TSDL_Version );
begin
  X.major := SDL_TTF_MAJOR_VERSION;
  X.minor := SDL_TTF_MINOR_VERSION;
  X.patch := SDL_TTF_PATCHLEVEL;
end;

procedure TTF_SetError( fmt : PChar );
begin
  SDL_SetError( fmt );
end;

function TTF_GetError : PChar;
begin
  result := SDL_GetError();
end;

function TTF_OpenFontOrThrow ( const _file : PChar; ptsize : Integer ) : PTTF_Font;
begin
  TTF_OpenFontOrThrow := TTF_OpenFont( _file, ptsize );
  if TTF_OpenFontOrThrow = nil then
    raise TSDLTTFException.Create('TTF_OpenFontOrThrow : '+TTF_GetError()+' (freetype library or font file missing?)' );
end;

function TTF_OpenFontRWOrThrow ( src : PSDL_RWops; freesrc : Integer; ptsize : Integer ) : PTTF_Font;
begin
  TTF_OpenFontRWOrThrow := TTF_OpenFontRW( src, freesrc, ptsize );
  if TTF_OpenFontRWOrThrow = nil then
    raise TSDLTTFException.Create('TTF_OpenFontRWOrThrow : '+TTF_GetError()+' (freetype library or font file missing?)' );

end;

finalization
  if SDL_ttf <> nil then FreeAndNil( SDL_ttf );

end.

