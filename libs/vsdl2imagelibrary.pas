unit vsdl2imagelibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary, vsdl2library;

type TSDL2ImageException = class( Exception );

const
{$IFDEF WINDOWS}
  SDL2ImageDefaultPath = 'SDL2_image.dll';
{$ELSE}
  {$IFDEF DARWIN}
  SDL2ImageDefaultPath = 'SDL2_image.framework/SDL_image';
  {$ELSE}
  SDL2ImageDefaultPath = 'libSDL2_image-2.0.so.0';
  {$ENDIF}
{$ENDIF}

const
  SDL_IMAGE_MAJOR_VERSION = 2;
  SDL_IMAGE_MINOR_VERSION = 9;
  SDL_IMAGE_PATCHLEVEL    = 0;

const
  IMG_INIT_JPG  = $00000001;
  IMG_INIT_PNG  = $00000002;
  IMG_INIT_TIF  = $00000004;
  IMG_INIT_WEBP = $00000008;
  IMG_INIT_JXL  = $00000010;
  IMG_INIT_AVIF = $00000020;

type
  IMG_Animation = record
    w, h: Integer;
    count: Integer;
    frames: PPSDL_Surface;
    delays: PInteger;
  end;
  PIMG_Animation = ^IMG_Animation;

var
  IMG_Linked_Version              : function : PSDL_version; cdecl;

  IMG_Init                        : function (flags: Integer): Integer; cdecl;
  IMG_Quit                        : procedure; cdecl;

  IMG_LoadTyped_RW                : function (src: PSDL_RWops; freesrc: Integer; const _type: PChar): PSDL_Surface; cdecl;
  IMG_Load                        : function (const file_: PChar): PSDL_Surface; cdecl;
  IMG_Load_RW                     : function (src: PSDL_RWops; freesrc: Integer): PSDL_Surface; cdecl;
  IMG_LoadTexture                 : function (renderer: PSDL_Renderer; const file_: PChar): PSDL_Texture; cdecl;
  IMG_LoadTexture_RW              : function (renderer: PSDL_Renderer; src: PSDL_RWops; freesrc: Integer): PSDL_Texture; cdecl;
  IMG_LoadTextureTyped_RW         : function (renderer: PSDL_Renderer; src: PSDL_RWops; freesrc: Integer; const _type: PChar): PSDL_Texture; cdecl;

  IMG_isAVIF                      : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isICO                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isCUR                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isBMP                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isGIF                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isJPG                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isJXL                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isLBM                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isPCX                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isPNG                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isPNM                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isSVG                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isQOI                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isTIF                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isXCF                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isXPM                       : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isXV                        : function (src: PSDL_RWops): Integer; cdecl;
  IMG_isWEBP                      : function (src: PSDL_RWops): Integer; cdecl;

  IMG_LoadAVIF_RW                 : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadICO_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadCUR_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadBMP_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadGIF_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadJPG_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadJXL_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadLBM_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadPCX_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadPNG_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadPNM_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadSVG_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadQOI_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadTGA_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadTIF_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadXCF_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadXPM_RW                  : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadXV_RW                   : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadWEBP_RW                 : function (src: PSDL_RWops): PSDL_Surface; cdecl;
  IMG_LoadSizedSVG_RW             : function (src: PSDL_RWops; width: Integer; height: Integer): PSDL_Surface; cdecl;
  IMG_ReadXPMFromArray            : function (xpm: PPChar): PSDL_Surface; cdecl;
  IMG_ReadXPMFromArrayToRGB888    : function (xpm: PPChar): PSDL_Surface; cdecl;
  IMG_SavePNG                     : function (surface: PSDL_Surface; const file_: PChar): Integer; cdecl;
  IMG_SavePNG_RW                  : function (surface: PSDL_Surface; dst: PSDL_RWops; freedst: Integer): Integer; cdecl;
  IMG_SaveJPG                     : function (surface: PSDL_Surface; const file_: PChar; quality: Integer): Integer; cdecl;
  IMG_SaveJPG_RW                  : function (surface: PSDL_Surface; dst: PSDL_RWops; freedst: Integer; quality: Integer): Integer; cdecl;
  IMG_LoadAnimation               : function (const file_: PChar): PIMG_Animation; cdecl;
  IMG_LoadAnimation_RW            : function (src: PSDL_RWops; freesrc: Integer): PIMG_Animation; cdecl;
  IMG_LoadAnimationTyped_RW       : function (src: PSDL_RWops; freesrc: Integer; const _type: PChar): PIMG_Animation; cdecl;
  IMG_FreeAnimation               : procedure (anim: PIMG_Animation); cdecl;
  IMG_LoadGIFAnimation_RW         : function (src: PSDL_RWops): PIMG_Animation; cdecl;
  IMG_LoadWEBPAnimation_RW        : function (src: PSDL_RWops): PIMG_Animation; cdecl;

var
  SDL2_image : TLibrary = nil;

function LoadSDL2Image( const aPath : AnsiString = SDL2ImageDefaultPath ) : Boolean;

implementation

function LoadSDL2Image ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL2_image.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL2_image : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL2 = nil then LoadSDL2();
  if SDL2 = nil then Exit( False );
  if SDL2_image <> nil then Exit( True );
  SDL2_image := TLibrary.Load( aPath );
  if SDL2_image = nil then Exit( False );

  Pointer(IMG_Linked_Version) := GetSymbol('IMG_Linked_Version');
  Pointer(IMG_Init) := GetSymbol('IMG_Init');
  Pointer(IMG_Quit) := GetSymbol('IMG_Quit');
  Pointer(IMG_LoadTyped_RW) := GetSymbol('IMG_LoadTyped_RW');
  Pointer(IMG_Load) := GetSymbol('IMG_Load');
  Pointer(IMG_Load_RW) := GetSymbol('IMG_Load_RW');
  Pointer(IMG_LoadTexture) := GetSymbol('IMG_LoadTexture');
  Pointer(IMG_LoadTexture_RW) := GetSymbol('IMG_LoadTexture_RW');
  Pointer(IMG_LoadTextureTyped_RW) := GetSymbol('IMG_LoadTextureTyped_RW');
  Pointer(IMG_isAVIF) := GetSymbol('IMG_isAVIF');
  Pointer(IMG_isICO) := GetSymbol('IMG_isICO');
  Pointer(IMG_isCUR) := GetSymbol('IMG_isCUR');
  Pointer(IMG_isBMP) := GetSymbol('IMG_isBMP');
  Pointer(IMG_isGIF) := GetSymbol('IMG_isGIF');
  Pointer(IMG_isJPG) := GetSymbol('IMG_isJPG');
  Pointer(IMG_isJXL) := GetSymbol('IMG_isJXL');
  Pointer(IMG_isLBM) := GetSymbol('IMG_isLBM');
  Pointer(IMG_isPCX) := GetSymbol('IMG_isPCX');
  Pointer(IMG_isPNG) := GetSymbol('IMG_isPNG');
  Pointer(IMG_isPNM) := GetSymbol('IMG_isPNM');
  Pointer(IMG_isSVG) := GetSymbol('IMG_isSVG');
  Pointer(IMG_isQOI) := GetSymbol('IMG_isQOI');
  Pointer(IMG_isTIF) := GetSymbol('IMG_isTIF');
  Pointer(IMG_isXCF) := GetSymbol('IMG_isXCF');
  Pointer(IMG_isXPM) := GetSymbol('IMG_isXPM');
  Pointer(IMG_isXV) := GetSymbol('IMG_isXV');
  Pointer(IMG_isWEBP) := GetSymbol('IMG_isWEBP');
  Pointer(IMG_LoadAVIF_RW) := GetSymbol('IMG_LoadAVIF_RW');
  Pointer(IMG_LoadICO_RW) := GetSymbol('IMG_LoadICO_RW');
  Pointer(IMG_LoadCUR_RW) := GetSymbol('IMG_LoadCUR_RW');
  Pointer(IMG_LoadBMP_RW) := GetSymbol('IMG_LoadBMP_RW');
  Pointer(IMG_LoadGIF_RW) := GetSymbol('IMG_LoadGIF_RW');
  Pointer(IMG_LoadJPG_RW) := GetSymbol('IMG_LoadJPG_RW');
  Pointer(IMG_LoadJXL_RW) := GetSymbol('IMG_LoadJXL_RW');
  Pointer(IMG_LoadLBM_RW) := GetSymbol('IMG_LoadLBM_RW');
  Pointer(IMG_LoadPCX_RW) := GetSymbol('IMG_LoadPCX_RW');
  Pointer(IMG_LoadPNG_RW) := GetSymbol('IMG_LoadPNG_RW');
  Pointer(IMG_LoadPNM_RW) := GetSymbol('IMG_LoadPNM_RW');
  Pointer(IMG_LoadSVG_RW) := GetSymbol('IMG_LoadSVG_RW');
  Pointer(IMG_LoadQOI_RW) := GetSymbol('IMG_LoadQOI_RW');
  Pointer(IMG_LoadTGA_RW) := GetSymbol('IMG_LoadTGA_RW');
  Pointer(IMG_LoadTIF_RW) := GetSymbol('IMG_LoadTIF_RW');
  Pointer(IMG_LoadXCF_RW) := GetSymbol('IMG_LoadXCF_RW');
  Pointer(IMG_LoadXPM_RW) := GetSymbol('IMG_LoadXPM_RW');
  Pointer(IMG_LoadXV_RW) := GetSymbol('IMG_LoadXV_RW');
  Pointer(IMG_LoadWEBP_RW) := GetSymbol('IMG_LoadWEBP_RW');
  Pointer(IMG_LoadSizedSVG_RW) := GetSymbol('IMG_LoadSizedSVG_RW');
  Pointer(IMG_ReadXPMFromArray) := GetSymbol('IMG_ReadXPMFromArray');
  Pointer(IMG_ReadXPMFromArrayToRGB888) := GetSymbol('IMG_ReadXPMFromArrayToRGB888');
  Pointer(IMG_SavePNG) := GetSymbol('IMG_SavePNG');
  Pointer(IMG_SavePNG_RW) := GetSymbol('IMG_SavePNG_RW');
  Pointer(IMG_SaveJPG) := GetSymbol('IMG_SaveJPG');
  Pointer(IMG_SaveJPG_RW) := GetSymbol('IMG_SaveJPG_RW');
  Pointer(IMG_LoadAnimation) := GetSymbol('IMG_LoadAnimation');
  Pointer(IMG_LoadAnimation_RW) := GetSymbol('IMG_LoadAnimation_RW');
  Pointer(IMG_LoadAnimationTyped_RW) := GetSymbol('IMG_LoadAnimationTyped_RW');
  Pointer(IMG_FreeAnimation) := GetSymbol('IMG_FreeAnimation');
  Pointer(IMG_LoadGIFAnimation_RW) := GetSymbol('IMG_LoadGIFAnimation_RW');
  Pointer(IMG_LoadWEBPAnimation_RW) := GetSymbol('IMG_LoadWEBPAnimation_RW');

  Exit( True );
end;


finalization
  if SDL2_image <> nil then FreeAndNil( SDL2_image );

end.

