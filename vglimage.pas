unit vglimage;
{$include valkyrie.inc}
interface
uses Classes,
  vsdllibrary,
  vimage;

function LoadImage( const FileName : Ansistring ) : TImage;
function LoadImage( Stream : TStream; Size : DWord ) : TImage;
function UploadImage( Image : TImage; aBlend : Boolean ) : DWord;
procedure ReUploadImage( GLTexture : DWord; Image: TImage; aBlend : Boolean );
function LoadImage( SDLSurface : PSDL_Surface ) : TImage;

implementation

uses
  vgllibrary,
  vsdlimagelibrary,
  vmath;

function LoadImage( SDLSurface : PSDL_Surface ) : TImage;
var w, h    : Integer;
    image   : PSDL_Surface;
    area    : SDL_Rect;
begin
  Assert( SDLSurface <> nil );

  //SDL_UnlockSurface( SDLSurface );
  //LoadImage := TImage.Create( SDLSurface^.pixels, SDLSurface^.w, SDLSurface^.h );

  w := UpToPowerOf2(SDLSurface^.w);
  h := UpToPowerOf2(SDLSurface^.h);

  image := SDL_CreateRGBSurface( SDL_SWSURFACE, w, h, 32,
{$IFDEF ENDIAN_LITTLE}
            $000000FF,
            $0000FF00,
            $00FF0000,
            $FF000000
{$ELSE}
            $FF000000,
            $00FF0000,
            $0000FF00,
            $000000FF
{$ENDIF}
  );

  Assert( image <> nil );
  area.x := 0;
  area.y := 0;
  area.w := SDLSurface^.w;
  area.h := SDLSurface^.h;
  SDL_SetAlpha(SDLSurface, 0, 0);
  SDL_BlitSurface(SDLSurface, @area, image, @area);

  LoadImage := TImage.Create( image^.pixels, w, h );
  LoadImage.RawX := SDLSurface^.w;
  LoadImage.RawY := SDLSurface^.h;
  SDL_FreeSurface( image );

//  SDL_FreeSurface( SDLSurface );
end;

function LoadImage( const FileName: Ansistring ) : TImage;
begin
  LoadSDLImage;
  Exit( LoadImage( IMG_LoadOrThrow( PChar( Filename ) ) ) );
end;

function LoadImage( Stream : TStream; Size : DWord ): TImage;
begin
  LoadSDLImage;
  Exit( LoadImage( IMG_LoadRWOrThrow( SDL_RWopsFromStream( Stream, Size ), 0 ) ) );
end;

function UploadImage( Image: TImage; aBlend : Boolean ): DWord;
var GLTexture : DWord;
begin
  glGenTextures(1, @GLTexture);
  ReUploadImage( GLTexture, Image, aBlend );
  Exit( GLTexture );
end;

procedure ReUploadImage ( GLTexture : DWord; Image : TImage; aBlend : Boolean );
var GLBlend   : TGLInt;
begin
  if aBlend
    then GLBlend := GL_LINEAR
    else GLBlend := GL_NEAREST;

  glBindTexture(GL_TEXTURE_2D, GLTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GLBlend );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GLBlend );
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
    Image.SizeX, Image.SizeY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Image.Data );
end;


end.

