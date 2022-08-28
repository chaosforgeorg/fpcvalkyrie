{$INCLUDE valkyrie.inc}
unit vparams;
{$H+}
interface
uses vnode, vgenerics, SysUtils;

type TParamsHashMap = specialize TGHashMap< AnsiString >;

type

{ TParams }

TParams = class(TVObject)
    Params : TParamsHashMap;
    Main   : AnsiString;
    constructor Create;
    function isSet(const ParamName : AnsiString) : boolean;
    function get(const ParamName : AnsiString) : AnsiString;
    destructor Destroy; override;
  end;


implementation

uses StrUtils;

{ TParams }

constructor TParams.Create;
var Count : Word;
    Param : string;
    Last  : string;
    Split : TStringArray;
begin
  Params := TParamsHashMap.Create;
  Main   := '';
  Last   := '';
  if ParamCount > 0 then
  for Count := 1 to ParamCount do
  begin
    Param := ParamStr(Count);
    if Param[1] = '-' then
    begin
      Delete(Param,1,1);
      Split := SplitString( Param, '=' );
      if High( Split ) > 0 then
      begin
        Param := LowerCase(Split[0]);
        Params[Param] := Split[1];
      end
      else
      begin
        Param := LowerCase(Param);
        Params[Param] := '';
        Last := Param;
      end;
    end
    else
      if Last = ''
        then Main := Param
        else Params[Last] := Param;
  end;
  Delete(Main,1,1);
end;

function TParams.isSet( const ParamName : AnsiString ) : boolean;
begin
  Exit( Params.Exists(LowerCase(ParamName)) );
end;

function TParams.get( const ParamName : AnsiString) : AnsiString;
begin
  Exit( Params[LowerCase(ParamName)] );
end;

destructor TParams.Destroy;
begin
  FreeAndNil(Params);
  inherited Destroy;
end;

end.

