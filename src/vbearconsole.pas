unit vbearconsole;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BearLibTerminal, vioconsole, viotypes, vutil;

type

{ TBearConsoleRenderer }

TBearConsoleRenderer = class( TIOConsoleRenderer )
  constructor Create( aSizeX, aSizeY : DWord; aReqCapabilities : TIOConsoleCapSet );
  destructor Destroy(); override;
  procedure Clear; override;
  procedure ClearRect(x1, y1, x2, y2: Integer; aBackColor: TIOColor=0);
    override;
  function GetBackColor(x, y: Integer): TIOColor; override;
  function GetChar(x, y: Integer): Char; override;
  function GetColor(x, y: Integer): TIOColor; override;
  function GetDeviceArea: TIORect; override;
  function GetSupportedCapabilities: TIOConsoleCapSet; override;
  procedure HideCursor; override;
  procedure MoveCursor(x, y: Integer); override;
  procedure OutputChar(x, y: Integer; aFrontColor, aBackColor: TIOColor;
    chr: char); override;
  procedure OutputChar(x, y: Integer; aColor: TIOColor; chr: char); override;
  procedure OutputCharEx( x, y, ox, oy : Integer; aFont: byte; aColor : TIOColor; chr : char ); override;
  procedure OutputCharEx( x, y, ox, oy : Integer; aFont: byte; aFrontColor, aBackColor : TIOColor; chr : char ); override;
  procedure ShowCursor; override;
  procedure Update; override;
private
  FPositionX    : Integer;
  FPositionY    : Integer;
  FColor        : TIOColor;
  FCursor       : TPoint;
end;

implementation

const BearByteColors : array[0..15] of UInt32 = (
      $FF000000,
      $FF0000A0,
      $FF00A000,
      $FF00A0A0,
      $FFA00000,
      $FFA000A0,
      $FFA0A000,
      $FFD8D8D8,
      $FF808080,
      $FF0000FF,
      $FF00FF00,
      $FF00FFFF,
      $FFFF0000,
      $FFFF00FF,
      $FFFFFF00,
      $FFFFFFFF
      );

function IOColorToBearColor( aColor : TIOColor ) : UInt32;
var a : byte;
begin
  if (aColor < 16) then
     Exit( BearByteColors[aColor] );
  a := aColor and $000000FF;
  Result := aColor shr 8 + a shl 24;
end;

{ TBearConsoleRenderer }

constructor TBearConsoleRenderer.Create( aSizeX, aSizeY : DWord; aReqCapabilities : TIOConsoleCapSet );
begin
  inherited Create( aSizeX, aSizeY, aReqCapabilities );
  FPositionX := 0;
  FPositionY := 0;
  FColor := 0;
end;

destructor TBearConsoleRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TBearConsoleRenderer.Clear;
begin
  terminal_clear();
end;

procedure TBearConsoleRenderer.ClearRect(x1, y1, x2, y2: Integer;
  aBackColor: TIOColor);
begin
  terminal_clear_area( x1-1, y1-1, x2-x1+1, y2-y1+1 );
end;

function TBearConsoleRenderer.GetBackColor(x, y: Integer): TIOColor;
begin
  Exit( terminal_pick_bkcolor(x, y) );
end;

function TBearConsoleRenderer.GetChar(x, y: Integer): Char;
begin
  Exit( Char(terminal_pick(x, y, 0)) );
end;

function TBearConsoleRenderer.GetColor(x, y: Integer): TIOColor;
begin
  Exit( terminal_pick_color(x, y, 0) );
end;

function TBearConsoleRenderer.GetDeviceArea: TIORect;
begin
  GetDeviceArea.Pos := Point( FPositionX, FPositionY );
  GetDeviceArea.Dim := Point( terminal_state(TK_WIDTH)*terminal_state(TK_CELL_WIDTH), terminal_state(TK_HEIGHT)*terminal_state(TK_CELL_HEIGHT) );
end;

function TBearConsoleRenderer.GetSupportedCapabilities: TIOConsoleCapSet;
begin
  Result := [ VIO_CON_BGCOLOR, VIO_CON_CURSOR, VIO_CON_EXTCOLOR, VIO_CON_BGSTABLE, VIO_CON_EXTOUT ];
end;

procedure TBearConsoleRenderer.HideCursor;
begin
  terminal_set('input.cursor-visible=false');
end;

procedure TBearConsoleRenderer.MoveCursor(x, y: Integer);
begin
  FCursor := Point(x,y);
end;

procedure TBearConsoleRenderer.OutputChar(x, y: Integer; aFrontColor,
  aBackColor: TIOColor; chr: char);
begin
  if ( FColor <> aFrontColor ) then
  begin
    terminal_color(IOColorToBearColor(aFrontColor));
    FColor := aFrontColor;
  end;

  terminal_bkcolor(IOColorToBearColor(aBackColor and $7f));
  terminal_put( x-1, y-1, Integer(chr) );
  terminal_bkcolor($FF000000);
end;

procedure TBearConsoleRenderer.OutputChar(x, y: Integer; aColor: TIOColor;
  chr: char);
begin
  if ( FColor <> aColor ) then
  begin
    terminal_color(IOColorToBearColor(aColor));
    FColor := aColor;
  end;
  terminal_put( x-1, y-1, Integer(chr) );
end;

procedure TBearConsoleRenderer.OutputCharEx(x, y, ox, oy: Integer; aFont: byte;
  aColor: TIOColor; chr: char);
begin
  if ( FColor <> aColor ) then
  begin
    terminal_color(IOColorToBearColor(aColor));
    FColor := aColor;
  end;
  terminal_put_ext( x-1, y-1, ox, oy, Integer(chr) + aFont shl 8);
end;

procedure TBearConsoleRenderer.OutputCharEx(x, y, ox, oy: Integer; aFont: byte;
  aFrontColor, aBackColor: TIOColor; chr: char);
begin
  if ( FColor <> aFrontColor ) then
  begin
    terminal_color(IOColorToBearColor(aFrontColor));
    FColor := aFrontColor;
  end;

  terminal_bkcolor(IOColorToBearColor(aBackColor and $7f));
  terminal_put_ext( x-1, y-1, ox, oy, Integer(chr) + aFont shl 8);
  terminal_bkcolor($FF000000);
end;

procedure TBearConsoleRenderer.ShowCursor;
begin
  terminal_set('input.cursor-visible=true');
end;

procedure TBearConsoleRenderer.Update;
begin
  terminal_move(FCursor.x-1, FCursor.y-1);
end;

end.

