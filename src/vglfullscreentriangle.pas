{$INCLUDE valkyrie.inc}
unit vglfullscreentriangle;
interface

type TGLFullscreenTriangle = class
  private
    FVAO           : Cardinal;
    FVBO           : Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render( aTextureID : Integer );
  protected
    procedure Setup;
  end;

implementation

uses SysUtils, vgl3library;

{ TGLFullscreenPass }

constructor TGLFullscreenTriangle.Create;
begin
  inherited Create;
  Setup;
end;

destructor TGLFullscreenTriangle.Destroy;
begin
  if FVAO <> 0 then
    glDeleteVertexArrays( 1, @FVAO );
  if FVBO <> 0 then
    glDeleteBuffers( 1, @FVBO );
  inherited Destroy;
end;

procedure TGLFullscreenTriangle.Setup;
const
  FullscreenTriangleVertices : array[0..5] of GLfloat = (
    -1.0, -3.0,
     3.0,  1.0,
    -1.0,  1.0
  );
begin
  glGenVertexArrays( 1, @FVAO );
  glGenBuffers( 1, @FVBO );

  glBindVertexArray( FVAO );
  glBindBuffer( GL_ARRAY_BUFFER, FVBO );
  glBufferData( GL_ARRAY_BUFFER, SizeOf(FullscreenTriangleVertices), @FullscreenTriangleVertices, GL_STATIC_DRAW );

  glVertexAttribPointer( 0, 2, GL_FLOAT, GL_FALSE, 2 * SizeOf(GLfloat), nil );
  glEnableVertexAttribArray( 0 );

  glBindBuffer( GL_ARRAY_BUFFER, 0 );
  glBindVertexArray( 0 );
end;

procedure TGLFullscreenTriangle.Render( aTextureID : Integer );
begin
//  glUseProgram(FShaderProgram);
//  glUniform1i(glGetUniformLocation(FShaderProgram, 'screenTexture'), 0);
//  glUniform2f(glGetUniformLocation(FShaderProgram, 'screenSize'), ScreenWidth, ScreenHeight);

  glBindVertexArray( FVAO );
  glActiveTexture( GL_TEXTURE0 );
  glBindTexture( GL_TEXTURE_2D, aTextureID );
  glDrawArrays( GL_TRIANGLES, 0, 3 );
  glBindVertexArray( 0 );

  glBindTexture(GL_TEXTURE_2D, 0 );
//  glUseProgram(0);
end;

end.

