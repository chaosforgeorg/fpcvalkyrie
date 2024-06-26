{$INCLUDE valkyrie.inc}
unit vtig;
interface
uses vutil, viotypes, vtigstyle, vioconsole;

procedure VTIG_Initialize( aRenderer : TIOConsoleRenderer; aClearOnRender : Boolean = True );
procedure VTIG_NewFrame;
procedure VTIG_EndFrame;
procedure VTIG_Render;
procedure VTIG_Clear;

implementation

uses SysUtils, vtigcontext;

var GDefaultContext : TTIGContext;
    GCtx            : TTIGContext;

procedure VTIG_Initialize( aRenderer : TIOConsoleRenderer; aClearOnRender : Boolean = True );
begin
// NV_ASSERT( g_tig->style == nullptr, "TIG reinitialized" );
  GCtx.Io.Initialize( aRenderer, aClearOnRender );
end;

procedure VTIG_NewFrame;
begin
end;

procedure VTIG_EndFrame;
begin
end;

procedure VTIG_Render;
begin
  GCtx.Io.Render( GCtx.DrawData );
end;

procedure VTIG_Clear;
begin
  GCtx.Io.Clear;
end;


initialization

GDefaultContext := TTIGContext.Create;
GCtx            := GDefaultContext;

finalization

GCtx := nil;
FreeAndNil( GDefaultContext );

end.

