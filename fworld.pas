unit fworld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, drandom;

type

  { TfrmWorldWizard }

  TfrmWorldWizard = class(TForm)
    btnNext: TBitBtn;
    btnPrevious: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    nbWorldWizard: TNotebook;
    pgWelcome: TPage;
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmWorldWizard: TfrmWorldWizard;

implementation

{$R *.lfm}

{ TfrmWorldWizard }

procedure TfrmWorldWizard.FormResize(Sender: TObject);
begin
  Left := (Screen.Width  - Width ) div 2;
  Top :=  (Screen.Height - Height) div 2;
end;

end.

