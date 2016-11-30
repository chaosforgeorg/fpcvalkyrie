unit vnetwork;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type TSocketErrorType = (
  seHostNotFound,
  seCreationFailed,
  seConnectFailed,
  seSendFailed,
  seRecvFailed
);

ESocketError = class(Exception)
  Code : TSocketErrorType;
  constructor Create( aCode: TSocketErrorType; const aMessageArgs: array of const);
end;

{ TSocket }

TSocket = class
private
  FSocket    : Integer;
  FHost      : AnsiString;
  FPort      : Word;
  FConnected : Boolean;
public
  constructor Create( const aHost : AnsiString; aPort : Word );
  procedure Connect;
  function Send( const aBuffer; aLength : DWord ) : Integer;
  function Receive( var aBuffer; aLength : DWord ) : Integer;
  destructor Destroy; override;
public
  property Socket    : Integer    read FSocket;
  property Host      : AnsiString read FHost;
  property Port      : Word       read FPort;
  property Connected : Boolean    read FConnected;
end;

EHTTPError        = class(Exception);
THTTPProgressFunc = procedure ( aProgress : DWord ) of object;


{ THTTPRequest }

THTTPRequest = class
private
  FSocket : TSocket;
public
  constructor Create( const aHost : AnsiString; aPort : Word );
  procedure Request( const aRequest : AnsiString );
  procedure SaveToFile( const aFileName : AnsiString );
  destructor Destroy; override;
private
  procedure CallOnProgress( aProgress : DWord );
  function GetResponseLine : AnsiString;
private
  FStatus     : AnsiString;
  FBuffer     : AnsiString;
  FTotal      : DWord;
  FOnProgress : THTTPProgressFunc;
public
  property Total      : DWord             read FTotal;
  property Status     : AnsiString        read FStatus;
  property OnProgress : THTTPProgressFunc read FOnProgress write FOnProgress;
end;


implementation

uses sockets, resolve, vlog, vdebug;

const
  MessageHostNotFound           = 'Host name resolution for "%s" failed';
  MessageSocketCreationFailed   = 'Creation of socket for "%s" failed (%d)';
  MessageSocketConnectFailed    = 'Connect to "%s" failed (%d)';
  MessageSocketSendFailed       = 'Send to "%s" failed (%d)';
  MessageSocketRecvFailed       = 'Receive from "%s" failed (%d)';

constructor ESocketError.Create( aCode : TSocketErrorType; const aMessageArgs : array of const );
var iMessage : AnsiString;
begin
  Code := aCode;
  case aCode of
    seHostNotFound   : iMessage := MessageHostNotFound;
    seCreationFailed : iMessage := MessageSocketCreationFailed;
    seConnectFailed  : iMessage := MessageSocketConnectFailed;
    seSendFailed     : iMessage := MessageSocketSendFailed;
    seRecvFailed     : iMessage := MessageSocketRecvFailed;
  end;
  iMessage := Format( iMessage, aMessageArgs );
  inherited Create( iMessage );
end;

{ TSocket }

constructor TSocket.Create( const aHost: AnsiString; aPort: Word );
begin
  FHost   := aHost;
  FPort   := aPort;
  FSocket := -1;
end;

procedure TSocket.Connect;
var iHostIP : THostAddr;
    iAddr   : TInetSockAddr;
begin
  if FSocket <> -1 then CloseSocket(FSocket);
  FSocket := fpsocket(AF_INET, SOCK_STREAM, 0);
  if FSocket = -1 then raise ESocketError.Create( seCreationFailed, [ FHost, SocketError ] );
  iHostIP := StrToHostAddr( FHost );
  if iHostIP.s_bytes[1] = 0 then
  with THostResolver.Create( nil ) do
  try
     if not NameLookup( FHost ) then
       raise ESocketError.Create( seHostNotFound, [FHost] );
     iHostIP := HostAddress;
  finally
    Free;
  end;
  iAddr.sin_family      := AF_INET;
  iAddr.sin_port        := ShortHostToNet( FPort );
  iAddr.sin_addr        := HostToNet( iHostIP );
  if fpconnect( FSocket, @iAddr, SizeOf(iAddr) ) <> 0 then
    raise ESocketError.Create(seConnectFailed, [ Format('%s:%d',[ FHost, FPort ] ), SocketError ]);
end;

function TSocket.Send(const aBuffer; aLength: DWord): Integer;
begin
  Send := fpsend( FSocket, @aBuffer, aLength, 0 );
  if Send = -1 then
    raise ESocketError.Create(seSendFailed, [ Format('%s:%d',[ FHost, FPort ] ), SocketError ]);
end;

function TSocket.Receive(var aBuffer; aLength: DWord): Integer;
begin
  Receive := fprecv( FSocket, @aBuffer, aLength, 0 );
  if Receive = -1 then
    raise ESocketError.Create(seRecvFailed, [ Format('%s:%d',[ FHost, FPort ] ), SocketError ]);
end;

destructor TSocket.Destroy;
begin
  if FSocket <> -1 then CloseSocket(FSocket);
  inherited Destroy;
end;

{ THTTPRequest }

const CRLF          = #13 + #10;
      CRLFCRLF      = #13 + #10 + #13 + #10;
      HTTPGetHeader = 'GET %s HTTP/1.0' + CRLF + 'Host: %s' + CRLF + 'Connection: close' + CRLFCRLF;

constructor THTTPRequest.Create(const aHost: AnsiString; aPort: Word);
begin
  FSocket := TSocket.Create( aHost, aPort );
  FBuffer := '';
end;

procedure THTTPRequest.Request(const aRequest: AnsiString);
var iRequest : AnsiString;
    iLine    : AnsiString;
begin
  FSocket.Connect;
  CallOnProgress( 0 );
  iRequest := Format( HTTPGetHeader, [ aRequest, FSocket.Host ] );
  FSocket.Send( iRequest[1], Length(iRequest) );
  FStatus := GetResponseLine;
  Log( FStatus );
  Delete( FStatus, 1, 9 );
  if LeftStr( FStatus, 3 ) <> '200' then
    raise EHTTPError.Create( Format( 'GET %s %s - %s', [ FSocket.Host, aRequest, FStatus ] ) );
  repeat
    iLine := GetResponseLine;
    if iLine = '' then Break;
  until False;
end;

procedure THTTPRequest.SaveToFile( const aFileName : AnsiString );
var iCount  : DWord;
begin
  FTotal := 0;
  with TFileStream.Create(aFileName, fmCreate) do
  try
    Write( FBuffer[1], Length( FBuffer ) );
    FTotal += Length( FBuffer );
    CallOnProgress( FTotal );
    SetLength( FBuffer, 1024 );
    repeat
      iCount := FSocket.Receive( FBuffer[1], Length(FBuffer) );
      Write( FBuffer[1], iCount );
      FTotal += iCount;
      CallOnProgress(FTotal);
    until iCount <= 0;
  finally
    Free;
  end;
end;

destructor THTTPRequest.Destroy;
begin
  FreeAndNil( FSocket );
  inherited Destroy;
end;

procedure THTTPRequest.CallOnProgress(aProgress: DWord);
begin
  if Assigned( FOnProgress ) then FOnProgress( aProgress );
end;

function THTTPRequest.GetResponseLine: AnsiString;
var iCount : DWord;
    iPos   : LongInt;
begin
  GetResponseLine := '';
  repeat
    if FBuffer = '' then
    begin
      SetLength( FBuffer, 1024 );
      iCount := FSocket.Receive( FBuffer[1], Length(FBuffer) );
      SetLength( FBuffer, iCount );
      if iCount = 0 then Exit;
    end;
    iPos := Pos( CRLF, FBuffer );
    if iPos > 0 then
    begin
      GetResponseLine := LeftStr( FBuffer, iPos-1 );
      Delete( FBuffer, 1, iPos + 1 );
      Exit;
    end;
    GetResponseLine += FBuffer;
    FBuffer := '';
  until False;
end;

end.

