unit vsdlimagelibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, Types, vlibrary, vsdllibrary;

type TSDLImageException = class( Exception );

const
{$IFDEF WINDOWS}
  SDLImageDefaultPath = 'SDL_image.dll';
{$ELSE}
  {$IFDEF DARWIN}
  SDLImageDefaultPath = 'SDL_image.framework/SDL_image';
  {$ELSE}
  SDLImageDefaultPath = 'libSDL_image-1.2.so.0';
  {$ENDIF}
{$ENDIF}

const
  SDL_IMAGE_MAJOR_VERSION = 1;
  SDL_IMAGE_MINOR_VERSION = 2;
  SDL_IMAGE_PATCHLEVEL    = 6;

{$DEFINE calldecl := cdecl}

var
  IMG_Linked_Version   : function : PSDL_version; calldecl;
  IMG_LoadTyped_RW     : function (src: PSDL_RWops; freesrc: Integer; _type: PChar): PSDL_Surface; calldecl;
  IMG_Load             : function (const _file: PChar): PSDL_Surface; calldecl;
  IMG_Load_RW          : function (src: PSDL_RWops; freesrc: Integer): PSDL_Surface; calldecl;
  IMG_InvertAlpha      : function (_on: Integer): Integer; calldecl;

  IMG_isBMP            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isGIF            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isJPG            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isLBM            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isPCX            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isPNG            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isPNM            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isTIF            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isXCF            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isXPM            : function (src: PSDL_RWops): Integer; calldecl;
  IMG_isXV             : function (src: PSDL_RWops): Integer; calldecl;

  IMG_LoadBMP_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadGIF_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadJPG_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadLBM_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadPCX_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadPNM_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadPNG_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadTGA_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadTIF_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadXCF_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadXPM_RW       : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_LoadXV_RW        : function (src: PSDL_RWops): PSDL_Surface; calldecl;
  IMG_ReadXPMFromArray : function ( xpm : PPChar ): PSDL_Surface; calldecl;

procedure SDL_IMAGE_VERSION( var X : TSDL_Version );
procedure IMG_SetError( fmt : PChar );
function IMG_GetError : PChar;

function IMG_LoadOrThrow(const _file: PChar): PSDL_Surface;
function IMG_LoadRWOrThrow(src: PSDL_RWops; freesrc: Integer): PSDL_Surface;

var
  SDL_image : TLibrary = nil;

function LoadSDLImage( const aPath : AnsiString = SDLImageDefaultPath ) : Boolean;

implementation

function LoadSDLImage ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := SDL_image.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'SDL_image : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if SDL = nil then LoadSDL();
  if SDL = nil then Exit( False );
  if SDL_image <> nil then Exit( True );
  SDL_image := TLibrary.Load( aPath );
  if SDL_image = nil then Exit( False );

  Pointer(IMG_Linked_Version)    := GetSymbol('IMG_Linked_Version');
  Pointer(IMG_LoadTyped_RW)      := GetSymbol('IMG_LoadTyped_RW');
  Pointer(IMG_Load)              := GetSymbol('IMG_Load');
  Pointer(IMG_Load_RW)           := GetSymbol('IMG_Load_RW');
  Pointer(IMG_InvertAlpha)       := GetSymbol('IMG_InvertAlpha');

  Pointer(IMG_isBMP)             := GetSymbol('IMG_isBMP');
  Pointer(IMG_isGIF)             := GetSymbol('IMG_isGIF');
  Pointer(IMG_isJPG)             := GetSymbol('IMG_isJPG');
  Pointer(IMG_isLBM)             := GetSymbol('IMG_isLBM');
  Pointer(IMG_isPCX)             := GetSymbol('IMG_isPCX');
  Pointer(IMG_isPNG)             := GetSymbol('IMG_isPNG');
  Pointer(IMG_isPNM)             := GetSymbol('IMG_isPNM');
  Pointer(IMG_isTIF)             := GetSymbol('IMG_isTIF');
  Pointer(IMG_isXCF)             := GetSymbol('IMG_isXCF');
  Pointer(IMG_isXPM)             := GetSymbol('IMG_isXPM');
  Pointer(IMG_isXV)              := GetSymbol('IMG_isXV');

  Pointer(IMG_LoadBMP_RW)        := GetSymbol('IMG_LoadBMP_RW');
  Pointer(IMG_LoadGIF_RW)        := GetSymbol('IMG_LoadGIF_RW');
  Pointer(IMG_LoadJPG_RW)        := GetSymbol('IMG_LoadJPG_RW');
  Pointer(IMG_LoadLBM_RW)        := GetSymbol('IMG_LoadLBM_RW');
  Pointer(IMG_LoadPCX_RW)        := GetSymbol('IMG_LoadPCX_RW');
  Pointer(IMG_LoadPNM_RW)        := GetSymbol('IMG_LoadPNM_RW');
  Pointer(IMG_LoadPNG_RW)        := GetSymbol('IMG_LoadPNG_RW');
  Pointer(IMG_LoadTGA_RW)        := GetSymbol('IMG_LoadTGA_RW');
  Pointer(IMG_LoadTIF_RW)        := GetSymbol('IMG_LoadTIF_RW');
  Pointer(IMG_LoadXCF_RW)        := GetSymbol('IMG_LoadXCF_RW');
  Pointer(IMG_LoadXPM_RW)        := GetSymbol('IMG_LoadXPM_RW');
  Pointer(IMG_LoadXV_RW)         := GetSymbol('IMG_LoadXV_RW');
  Pointer(IMG_ReadXPMFromArray)  := GetSymbol('IMG_ReadXPMFromArray');

  Exit( True );
end;

procedure SDL_IMAGE_VERSION( var X : TSDL_Version );
begin
  X.major := SDL_IMAGE_MAJOR_VERSION;
  X.minor := SDL_IMAGE_MINOR_VERSION;
  X.patch := SDL_IMAGE_PATCHLEVEL;
end;

procedure IMG_SetError( fmt : PChar );
begin
  SDL_SetError( fmt );
end;

function IMG_GetError : PChar;
begin
  result := SDL_GetError();
end;

function IMG_LoadOrThrow ( const _file : PChar ) : PSDL_Surface;
begin
  IMG_LoadOrThrow := IMG_Load( _file );
  if IMG_LoadOrThrow = nil then
    raise TSDLImageException.Create('IMG_LoadOrThrow : '+IMG_GetError()+' (png/jpg library or file missing?)' );
end;

function IMG_LoadRWOrThrow ( src : PSDL_RWops; freesrc : Integer ) : PSDL_Surface;
begin
  IMG_LoadRWOrThrow := IMG_Load_RW( src, freesrc );
  if IMG_LoadRWOrThrow = nil then
    raise TSDLImageException.Create('IMG_LoadRWOrThrow : '+IMG_GetError()+' (png/jpg library or file missing?)' );
end;

finalization
  if SDL_image <> nil then FreeAndNil( SDL_image );

end.

