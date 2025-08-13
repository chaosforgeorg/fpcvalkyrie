{$INCLUDE valkyrie.inc}
unit vglprogram;
interface

uses Classes, SysUtils, vnode;

type

{ TGLProgram }

 TGLProgram = class( TVObject )
  constructor Create( const aVSSource, aFSSource : Ansistring );
  procedure Bind;
  function GetAttribLocation( const aName : AnsiString ) : Integer;
  function GetUniformLocation( const aName : AnsiString ) : Integer;
  procedure SetUniformi( const aName : AnsiString; aX : Integer );
  procedure SetUniformi( const aName : AnsiString; aX, aY : Integer );
  procedure SetUniformf( const aName : AnsiString; aX : Real );
  procedure SetUniformf( const aName : AnsiString; aX, aY : Real );
  procedure UnBind;
  destructor Destroy; override;
private
  procedure Compile( const aVSSource, aFSSource : Ansistring );
  function CompileShader( aType : Cardinal; const aSource : Ansistring ) : Cardinal;
private
  FProgramID : Cardinal;
  FVShaderID : Cardinal;
  FFShaderID : Cardinal;
end;

function SlurpFile( const aFileName : AnsiString ) : AnsiString;

implementation

uses vutil, vdebug, vgl3library;

function SlurpFile(const aFileName: AnsiString): AnsiString;
begin
  Result := '';
  with TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite) do
  try
    if Size > 0 then
    begin
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    end;
  finally
    Free;
  end;
end;

{ TGLProgram }

constructor TGLProgram.Create(const aVSSource, aFSSource: Ansistring);
begin
  FProgramID := 0;
  FVShaderID := 0;
  FFShaderID := 0;
  Compile( aVSSource, aFSSource );
end;

procedure TGLProgram.Bind;
begin
  glUseProgram( FProgramID );
end;

procedure TGLProgram.UnBind;
begin
  glUseProgram( 0 );
end;

function TGLProgram.GetAttribLocation(const aName: AnsiString): Integer;
begin
  Result := glGetAttribLocation( FProgramID, PChar(aName) );
  if Result = -1 then
    Log( 'Could not bind attribute '+aName+'!');
end;

function TGLProgram.GetUniformLocation(const aName: AnsiString): Integer;
begin
  Result := glGetUniformLocation( FProgramID, PChar(aName) );
  if Result = -1 then
    Log( 'Could not bind attribute '+aName+'!');
end;

procedure TGLProgram.SetUniformi( const aName : AnsiString; aX : Integer );
var iLocation : Integer;
begin
  iLocation := GetUniformLocation( aName );
  if iLocation <> -1 then
    glUniform1i( iLocation, aX );
end;

procedure TGLProgram.SetUniformi( const aName : AnsiString; aX, aY : Integer );
var iLocation : Integer;
begin
  iLocation := GetUniformLocation( aName );
  if iLocation <> -1 then
    glUniform2i( iLocation, aX, aY );
end;

procedure TGLProgram.SetUniformf( const aName : AnsiString; aX : Real );
var iLocation : Integer;
begin
  iLocation := GetUniformLocation( aName );
  if iLocation <> -1 then
    glUniform1f( iLocation, aX );
end;

procedure TGLProgram.SetUniformf( const aName : AnsiString; aX, aY : Real );
var iLocation : Integer;
begin
  iLocation := GetUniformLocation( aName );
  if iLocation <> -1 then
    glUniform2f( iLocation, aX, aY );
end;

destructor TGLProgram.Destroy;
begin
  inherited Destroy;
end;

procedure TGLProgram.Compile(const aVSSource, aFSSource: Ansistring);
var iCompileOK : LongInt;
    iLength    : LongInt;
    iBuffer    : AnsiString;
begin
  iCompileOK := GL_FALSE;
  FVShaderID := CompileShader( GL_VERTEX_SHADER, aVSSource );
  FFShaderID := CompileShader( GL_FRAGMENT_SHADER, aFSSource );
  if (FVShaderID = 0) or (FFShaderID = 0) then Exit;

  FProgramID := glCreateProgram();
  glAttachShader( FProgramID, FVShaderID );
  glAttachShader( FProgramID, FFShaderID );
  glLinkProgram( FProgramID );
  glGetProgramiv( FProgramID, GL_LINK_STATUS, @iCompileOK);
  if iCompileOK = 0 then
  begin
    iBuffer := '';
    SetLength( iBuffer, 1024 );
    glGetProgramInfoLog( FProgramID, 1024, @iLength, PChar(iBuffer) );
    SetLength( iBuffer, iLength );
    Log( 'glLinkProgram failure : '+ iBuffer );
  end;
end;

function TGLProgram.CompileShader( aType: Cardinal; const aSource: Ansistring ): Cardinal;
var iCompileOK : LongInt;
    iLength    : LongInt;
    iShaderID  : Cardinal;
    iPSource   : PChar;
    iPPSource  : PPChar;
    iBuffer    : AnsiString;
begin
  iCompileOK := GL_FALSE;
  iShaderID  := glCreateShader( aType );
  iPSource   := PChar(aSource);
  iPPSource  := @iPSource;
  glShaderSource( iShaderID, 1, iPPSource, nil );
  glCompileShader( iShaderID );
  glGetShaderiv( iShaderID, GL_COMPILE_STATUS, @iCompileOK );
  if iCompileOK = 0 then
  begin
    iBuffer := '';
    SetLength( iBuffer, 1024 );
    glGetShaderInfoLog( iShaderID, 1024, @iLength, PChar(iBuffer) );
    SetLength( iBuffer, iLength );
    Log( aSource );
    Log( LogError, 'Shader compile failure : '+ iBuffer );
    Readln;
    Exit( 0 );
  end;
  Exit( iShaderID );
end;

end.

