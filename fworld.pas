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
unit fworld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, LCLType,
  gworld;

type

  { TfrmWorldWizard }

  TfrmWorldWizard = class(TForm)
    btnOpenFolder: TBitBtn;
    btnNext: TBitBtn;
    btnPrevious: TBitBtn;
    pgbFractalMap: TProgressBar;
    txtFractalLog: TMemo;
    picFractalMap: TImage;
    labStatus: TLabel;
    pgFractalMap: TPage;
    txtRandomSeed: TEdit;
    Label5: TLabel;
    Label7: TLabel;
    rdgClimateZones: TRadioGroup;
    txtWorldFolder: TEdit;
    Label6: TLabel;
    labWorldFolder: TLabel;
    txtWorldName: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    labWorldName: TLabel;
    nbWorldWizard: TNotebook;
    OpenDialog1: TOpenDialog;
    pgBase: TPage;
    pgWelcome: TPage;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnNextClick(Sender: TObject);
    procedure btnOpenFolderClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure txtRandomSeedChange(Sender: TObject);
    procedure txtWorldNameChange(Sender: TObject);
    procedure pgBaseUpdateState(Sender: TObject);
    procedure CreateFractalMap;
  private
    { private declarations }
    RandomSeed : longint;
  public
    { public declarations }
    NewWorld : tWorld;
  end;

implementation

{$R *.lfm}



{ TfrmWorldWizard }

procedure TfrmWorldWizard.FormResize(Sender: TObject);
begin
  Left := (Screen.Width  - Width ) div 2;
  Top :=  (Screen.Height - Height) div 2;
end;

procedure TfrmWorldWizard.txtRandomSeedChange(Sender: TObject);
var
  v : longint;
  t : string;
begin
  val (txtRandomSeed.Text, v);
  if (v <> 0) then
    RandomSeed := v;
  str (RandomSeed, t);
  txtRandomSeed.Text := t;
  pgBaseUpdateState (Sender);
end;

procedure TfrmWorldWizard.txtWorldNameChange(Sender: TObject);
var
  index : integer;
  c : char;
  t : string;
begin
  RandomSeed := 0;
  t := upcase (txtWorldName.Text);
  for index := 1 to length (t) do begin
    c := t [index];
    if (c in ['A'..'Z']) then
      RandomSeed := 26 * RandomSeed + (ord (c) - 65);
  end;
  RandomSeed += 1;
  str (RandomSeed, t);
  txtRandomSeed.Text := t;
  pgBaseUpdateState (Sender);
end;

procedure TfrmWorldWizard.FormCreate(Sender: TObject);
begin
  nbWorldWizard.PageIndex := 0;
end;

procedure TfrmWorldWizard.btnOpenFolderClick(Sender: TObject);
begin
  SelectDirectoryDialog1.Filename := txtWorldFolder.Text;
  if (SelectDirectoryDialog1.Execute) then
    txtWorldFolder.Text := SelectDirectoryDialog1.Filename;
  pgBaseUpdateState (Sender);
end;

procedure TfrmWorldWizard.btnNextClick(Sender: TObject);
begin
  case (nbWorldWizard.PageIndex) of
    0 :
      begin
        nbWorldWizard.PageIndex := 1;
        btnPrevious.Caption := 'Previous';
        btnNext.Caption := 'Next';
        btnNext.Enabled := FALSE;
      end;
    1 :
      begin
        nbWorldWizard.PageIndex := 2;
        btnNext.Enabled := FALSE;
        btnPrevious.Enabled := FALSE;
        CreateFractalMap;
      end;
  end;
end;

procedure TfrmWorldWizard.btnPreviousClick(Sender: TObject);
begin
  case (nbWorldWizard.PageIndex) of
    0 :
      begin
      end;
    1 :
      begin
        nbWorldWizard.PageIndex := 0;
        btnPrevious.Caption := 'Load';
        btnNext.Caption := 'New';
        btnNext.Enabled := TRUE;
      end;
    2 :
      begin
        if (Application.MessageBox ('Going back will undo a lot of work.',
          'Are you sure?', MB_ICONQUESTION + MB_YESNO) = ID_YES) then
            nbWorldWizard.PageIndex := 1;
      end;
  end;
end;

procedure TfrmWorldWizard.pgBaseUpdateState(Sender: TObject);
begin
  if ((length (txtWorldName.Text) >= 6) and
    (DirectoryExists (txtWorldFolder.Text))) then
      btnNext.Enabled := TRUE
    else
      btnNext.Enabled := FALSE;
end;

procedure TfrmWorldWizard.CreateFractalMap;
begin
  NewWorld.FractalMap.pgbFractalMap := pgbFractalMap;
  NewWorld.FractalMap.picFractalMap := picFractalMap;
  NewWorld.labStatus := labStatus;
  NewWorld.RandomSeed := RandomSeed;
  NewWorld.Genesis;

  NewWorld.FractalMap.pgbFractalMap := nil;
  NewWorld.FractalMap.picFractalMap := nil;
  NewWorld.labStatus := nil;

  btnNext.Enabled := TRUE;
  btnPrevious.Enabled := TRUE;
end;

end.

