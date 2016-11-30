{$INCLUDE valkyrie.inc}
// @abstract(System interface for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
//
// Each singleton must derive from this class. The purpose
// of @link(TSystem) is to provide a general interface
// for debugging and calling.
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

unit vsystem;
interface
uses vnode;

// The generic System interface. All Systems must inherit it.
type TSystem = class(TNode)
       // Registers system execution.
       constructor Create; override;
       // System call. Used mainly for console like systems.
       procedure   Call; virtual;
       // Closes system execution.
       destructor Destroy; override;
     end;

implementation

constructor TSystem.Create;
begin
  inherited Create;
  Log('Initialized.');
end;

destructor TSystem.Destroy;
begin
  Log('Closed.');
  inherited Destroy;
end;

procedure TSystem.Call;
begin
  Log('Call method not implemented');
end;

end.
