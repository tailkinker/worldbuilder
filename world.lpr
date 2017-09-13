program world;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, fworld, drandom, fbrowse;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmWorldBrowser, frmWorldBrowser);
  Application.Run;
end.

