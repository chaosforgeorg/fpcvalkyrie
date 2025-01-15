{$INCLUDE valkyrie.inc}
unit vglframebuffer;
interface

type TGLFramebuffer = class
  private
    FWidth           : Integer;
    FHeight          : Integer;
    FFramebufferID   : Cardinal;
    FTextureIDs      : array of Cardinal;
    FAttachmentCount : Integer;
    FLinear          : Boolean;
  public
    constructor Create( aWidth, aHeight : Integer; aAttachmentCount: Integer = 1; aLinear : Boolean = True );
    destructor Destroy; override;
    procedure Resize( aNewWidth, aNewHeight: Integer );
    procedure BindAndClear;
    procedure Bind;
    procedure Unbind;
    function GetTextureID( aIndex: Integer = 0 ): Cardinal;
  end;

implementation

uses SysUtils, vgl3library;
          { TGLFramebuffer }

constructor TGLFramebuffer.Create( aWidth, aHeight, aAttachmentCount: Integer; aLinear : Boolean );
begin
  if aAttachmentCount < 1 then
    raise Exception.Create('Framebuffer must have at least one color attachment.');

  FAttachmentCount := aAttachmentCount;
  SetLength(FTextureIDs, FAttachmentCount);

  glGenFramebuffers(1, @FFramebufferID);
  glGenTextures(FAttachmentCount, @FTextureIDs[0]);

  FWidth := 0;
  FHeight := 0;
  FLinear := aLinear;
  Resize( aWidth, aHeight );
end;

procedure TGLFramebuffer.Resize( aNewWidth, aNewHeight: Integer );
var i        : Integer;
    iBuffers : array of Cardinal;
begin
  if ( aNewWidth = FWidth ) and ( aNewHeight = FHeight ) then Exit;

  if ( FWidth <> 0 ) then
    glDeleteTextures(FAttachmentCount, @FTextureIDs[0]);
  glGenTextures(FAttachmentCount, @FTextureIDs[0]);

  glBindFramebuffer(GL_FRAMEBUFFER, FFramebufferID);

  SetLength( iBuffers, FAttachmentCount );
  for i := 0 to FAttachmentCount - 1 do
  begin
    glBindTexture(GL_TEXTURE_2D, FTextureIDs[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, aNewWidth, aNewHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
    if FLinear then
    begin
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    end
    else
    begin
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
    end;

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i, GL_TEXTURE_2D, FTextureIDs[i], 0);

    iBuffers[i] := GL_COLOR_ATTACHMENT0 + i;
  end;

  glBindTexture( GL_TEXTURE_2D, 0 );
  glDrawBuffers( FAttachmentCount, @iBuffers[0] );

  if ( glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE ) then
    raise Exception.Create('Failed to resize framebuffer to complete state');

  FWidth := aNewWidth;
  FHeight := aNewHeight;

  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

destructor TGLFramebuffer.Destroy;
begin
  if FFramebufferID <> 0 then
    glDeleteFramebuffers( 1, @FFramebufferID );
  if Length( FTextureIDs ) > 0 then
    glDeleteTextures( FAttachmentCount, @FTextureIDs[0] );
  inherited Destroy;
end;

procedure TGLFramebuffer.Bind;
begin
  glBindFramebuffer( GL_FRAMEBUFFER, FFramebufferID );
  glViewport( 0, 0, FWidth, FHeight );
end;

procedure TGLFramebuffer.BindAndClear;
begin
  glBindFramebuffer( GL_FRAMEBUFFER, FFramebufferID );
  glViewport( 0, 0, FWidth, FHeight );
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
end;

procedure TGLFramebuffer.Unbind;
begin
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

function TGLFramebuffer.GetTextureID( aIndex : Integer = 0 ): Cardinal;
begin
  if ( aIndex < 0 ) or ( aIndex >= FAttachmentCount ) then
    raise Exception.Create('Invalid attachment index.');
  Result := FTextureIDs[aIndex];
end;

end.

