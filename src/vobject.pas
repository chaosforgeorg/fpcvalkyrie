{$INCLUDE valkyrie.inc}
// @abstract(TVObject class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(September 15, 2026)
//
// Base object (TVObject), logging and streaming hooks - independent of node services.

unit vobject;
interface
uses classes, vutil;

type TVObject = class( TObject )
     // TVObject Interface for @link(grdebug.Log).
     procedure   Log( const aLogString : Ansistring ); virtual;
     // TVObject Interface for @link(grdebug.Log).
     procedure   Log( aLevel : TLogLevel; const aLogString : Ansistring ); virtual;
     // TVObject Interface for @link(grdebug.Log), Formatted version.
     procedure   Log( const aLogString : Ansistring; const aParam : array of Const );
     // TVObject Interface for @link(grdebug.Log), Formatted version.
     procedure   Log( aLevel : TLogLevel; const aLogString : Ansistring; const aParam : array of Const );
     // Returns wether the object has a parent -- in case of TVObject it's always false
     function hasParent : boolean; virtual;
     // Returns wether the object has a child -- in case of TVObject it's always false
     function hasChild : boolean; virtual;
     // Returns wether the object is a TVNode
     function isNode : boolean; virtual;
     // Stream constructor - should be overriden.
     constructor CreateFromStream( aStream : TStream ); virtual;
     // Write to stream - should be overriden.
     procedure WriteToStream( aStream : TStream ); virtual;
  end;

type TVObjectClass = class of TVObject;


implementation
uses sysutils, vdebug;

procedure TVObject.Log( const aLogString       : Ansistring );
begin
  vdebug.Log('<'+classname+'> '+aLogString);
end;

procedure TVObject.Log( aLevel: TLogLevel; const aLogString: Ansistring );
begin
  vdebug.Log( aLevel,'<'+classname+'> '+aLogString );
end;

procedure TVObject.Log( const aLogString: Ansistring; const aParam: array of const );
begin
  Log( Format( aLogString, aParam ) );
end;

procedure TVObject.Log( aLevel: TLogLevel; const aLogString: Ansistring; const aParam: array of const );
begin
  Log( aLevel, Format( aLogString, aParam ) );
end;

function TVObject.hasParent : boolean;
begin
  Exit(False);
end;

function TVObject.hasChild : boolean;
begin
  Exit(False);
end;

function TVObject.isNode : boolean; 
begin
  Exit(False);
end;

constructor TVObject.CreateFromStream( aStream : TStream ) ;
begin
  // noop
end;

procedure TVObject.WriteToStream( aStream : TStream ) ;
begin
  // noop
end;


end.
