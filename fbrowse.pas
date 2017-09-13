unit fbrowse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  fworld;

type

  { TfrmWorldBrowser }

  TfrmWorldBrowser = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    procedure FormCreate(Sender: TObject);
  private
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
  with (TfrmWorldWizard.Create (Application)) do begin
    ShowModal;
  end;
end;

end.

