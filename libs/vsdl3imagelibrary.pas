unit vsdl3imagelibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary, vsdl3library;

type TSDL3ImageException = class( Exception );

const
{$IFDEF WINDOWS}
  SDL3ImageDefaultPath = 'SDL3_image.dll';
{$ELSE}
  {$IFDEF DARWIN}
  SDL3ImageDefaultPath = 'SDL3_image.framework/SDL_image';
  {$ELSE}
  SDL3ImageDefaultPath = 'libSDL3_image.so';
  {$ENDIF}
{$ENDIF}

const
  SDL_IMAGE_MAJOR_VERSION = 3;
  SDL_IMAGE_MINOR_VERSION = 2;
  SDL_IMAGE_PATCHLEVEL    = 4;

type
  PLongInt = ^LongInt;

  PIMG_Animation = ^IMG_Animation;
  IMG_Animation = record
    w, h: LongInt;
    count: LongInt;
    frames: PPSDL_Surface;
    delays: PLongInt;
  end;
  
var
  IMG_Version                        : function(): Sint32; cdecl;

  IMG_LoadTyped_IO                   : function(src: PSDL_IOStream; closeio: SDL_bool; type_: PAnsiChar): PSDL_Surface; cdecl;
  IMG_Load                           : function(file_: PAnsiChar): PSDL_Surface; cdecl;
  IMG_Load_IO                        : function(src: PSDL_IOStream; closeio: SDL_bool): PSDL_Surface; cdecl;

  IMG_LoadSizedSVG_IO                : function(src: PSDL_IOStream; width: Sint32; height: Sint32): PSDL_Surface; cdecl;

  IMG_SavePNG                        : function(surface: PSDL_Surface; file_: PAnsiChar): SDL_bool; cdecl;
  IMG_SavePNG_IO                     : function(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: SDL_bool): SDL_bool; cdecl;
  IMG_SaveJPG                        : function(surface: PSDL_Surface; file_: PAnsiChar; quality: Sint32): SDL_bool; cdecl;
  IMG_SaveJPG_IO                     : function(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: SDL_bool; quality: Sint32): SDL_bool; cdecl;

  IMG_LoadAnimation                  : function(file_: PAnsiChar): PIMG_Animation; cdecl;
  IMG_LoadAnimation_IO               : function(src: PSDL_IOStream; closeio: SDL_bool): PIMG_Animation; cdecl;
  IMG_LoadAnimationTyped_IO          : function(src: PSDL_IOStream; closeio: SDL_bool; type_: PAnsiChar): PIMG_Animation; cdecl;
  IMG_FreeAnimation                  : procedure(anim: PIMG_Animation); cdecl;
  IMG_LoadGIFAnimation_IO            : function(src: PSDL_IOStream): PIMG_Animation; cdecl;
  IMG_LoadWEBPAnimation_IO           : function(src: PSDL_IOStream): PIMG_Animation; cdecl;

var
  SDL3_image : TLibrary = nil;

function LoadSDL3Image( const aPath : AnsiString = SDL3ImageDefaultPath ) : Boolean;

implementation

function LoadSDL3Image ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL3_image.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL3_image : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL3 = nil then LoadSDL3();
  if SDL3 = nil then Exit( False );
  if SDL3_image <> nil then Exit( True );
  SDL3_image := TLibrary.Load( aPath );
  if SDL3_image = nil then Exit( False );

  Pointer(IMG_Version)                  := GetSymbol('IMG_Version');

  Pointer(IMG_LoadTyped_IO)             := GetSymbol('IMG_LoadTyped_IO');
  Pointer(IMG_Load)                     := GetSymbol('IMG_Load');
  Pointer(IMG_Load_IO)                  := GetSymbol('IMG_Load_IO');

  Pointer(IMG_LoadSizedSVG_IO)          := GetSymbol('IMG_LoadSizedSVG_IO');

  Pointer(IMG_SavePNG)                  := GetSymbol('IMG_SavePNG');
  Pointer(IMG_SavePNG_IO)               := GetSymbol('IMG_SavePNG_IO');
  Pointer(IMG_SaveJPG)                  := GetSymbol('IMG_SaveJPG');
  Pointer(IMG_SaveJPG_IO)               := GetSymbol('IMG_SaveJPG_IO');

  Pointer(IMG_LoadAnimation)            := GetSymbol('IMG_LoadAnimation');
  Pointer(IMG_LoadAnimation_IO)         := GetSymbol('IMG_LoadAnimation_IO');
  Pointer(IMG_LoadAnimationTyped_IO)    := GetSymbol('IMG_LoadAnimationTyped_IO');
  Pointer(IMG_FreeAnimation)            := GetSymbol('IMG_FreeAnimation');
  Pointer(IMG_LoadGIFAnimation_IO)      := GetSymbol('IMG_LoadGIFAnimation_IO');
  Pointer(IMG_LoadWEBPAnimation_IO)     := GetSymbol('IMG_LoadWEBPAnimation_IO');

  Exit( True );
end;


finalization
  if SDL3_image <> nil then FreeAndNil( SDL3_image );

end.

