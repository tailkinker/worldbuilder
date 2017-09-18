{ World Builder

  Copyright (C) 2017 Timothy Groves (timothy.red.groves@gmail.com)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit fbrowse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  fworld, gworld;

type

  { TfrmWorldBrowser }

  TfrmWorldBrowser = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    procedure FormCreate(Sender: TObject);
  private
    World : tWorld;
    { private declarations }
  public
    { public declarations }
  end;

var
  frmWorldBrowser: TfrmWorldBrowser;

implementation

{$R *.lfm}

{ TfrmWorldBrowser }

procedure TfrmWorldBrowser.FormCreate(Sender: TObject);
begin
  World := tWorld.Create(1);
  with (TfrmWorldWizard.Create (Application)) do begin
    NewWorld := World;
    ShowModal;
  end;
  Show;
end;

end.

