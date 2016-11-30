{$INCLUDE valkyrie.inc}
// @abstract(Systems management for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(June 6, 2004)
//
//  @html <div class="license">
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or (at your
//  option) any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
//  for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this library; if not, write to the Free Software Foundation,
//  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//  @html </div>

unit vsystems;
interface
uses vdebug, vsystem;

// The System management class.
type TSystems = class(TSystem)
       // Prepares system manager.
       constructor Create; override;
       // Adds a system. SystemVar is a singleton variable, and System is the
       // initialized system.
       // Example : Systems.Add(MySystemSingleton,TMySystem.Init);
       function Add( System : TSystem ) : TSystem; reintroduce;
       // Closes all systems.
       destructor Destroy; override;
       private
       function IsInited(System : TSystem) : boolean;
     end;

// System management singleton.
const Systems : TSystems = nil;

implementation

uses vnode, SysUtils;

constructor TSystems.Create;
begin
  inherited Create;
end;

destructor TSystems.Destroy;
begin
  inherited Destroy;
end;


// Adds a system. Pointers passed should be uninitialized.
function TSystems.Add( System : TSystem) : TSystem;
begin
  if IsInited(System) then raise Exception.Create('System '+System.ClassName+' reinitialized!');
  inherited Add(System);
  Exit( System );
end;

function TSystems.IsInited(System : TSystem) : boolean;
var SystemScan : TNode;
begin
  if System = nil then Exit(false);
  if Child  = nil then Exit(false);
  SystemScan := Child;
  repeat 
    if SystemScan = System then Exit(True);
    SystemScan := SystemScan.Next;
  until SystemScan = Child;
  Exit(False);
end;


initialization

Systems := TSystems.Create;

finalization

FreeAndNil(Systems);

end.
