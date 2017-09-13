unit fworld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, LCLType,
  drandom;

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
    procedure txtWorldNameChange(Sender: TObject);
    procedure pgBaseUpdateState(Sender: TObject);
    procedure CreateFractalMap;
  private
    RandomSeed : longword;
    Rand : tRNG;
    FractalMap : array [0..512, 0..256] of real;
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TfrmWorldWizard }

procedure TfrmWorldWizard.FormResize(Sender: TObject);
begin
  Left := (Screen.Width  - Width ) div 2;
  Top :=  (Screen.Height - Height) div 2;
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
  Rand := tRNG.Create;
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
  function AltColour (Altitude : real) : TColor;
  var
    red,
    green,
    blue : byte;
  begin
    if (Altitude > 1023) then
      Altitude := 1023;
    if (Altitude < 0) then begin
      red := 0;
      green := 0;
      blue := trunc (255 - abs(Altitude) / 4);
    end else begin
      red := trunc (Altitude / 4);
      green := trunc (255 - Altitude / 4);
      blue := 0;
    end;
    AltColour := blue shl 16 + green shl 8 + red
  end;

var
  Mountains : array [0..512, 0..256] of real;
  index,
  ranges,
  roll,
  step,
  start,
  x,
  y,
  x1,
  x2,
  xt,
  y1,
  y2,
  yt : integer;
  distance,
  mean : real;
  Count : longint = 0;
begin
  // Set base random number seed
  Rand.Seed := RandomSeed;

  for x := 0 to 512 do
    for y := 0 to 256 do
      FractalMap [x, y] := 0;

  index := 9;
  picFractalMap.Canvas.Pen.Style := psClear;

  repeat
    step := 1 shl index;
    start := step shr 1;

    // Tops and Bottoms
    x := start;
    repeat
      y := 0;
      repeat
        mean := (FractalMap [x - start, y] + FractalMap [x + start, y]) / 2;
        mean := mean + (Rand.RollReal - 0.5) * step;
        FractalMap [x, y] := mean;
        with picFractalMap.Canvas do begin
          Brush.Color := AltColour (mean);
          FillRect (x, y, x + step, y + step);
        end;
        Count += 1;
        y += step;
      until y > 256;
      pgbFractalMap.Position := Count;
      pgbFractalMap.Repaint();
      picFractalMap.Repaint();
      x += step;
    until x > 512;

    // Sides
    x := 0;
    repeat
      y := start;
      repeat
        mean := (FractalMap [x, y - start] + FractalMap [x, y + start]) / 2;
        mean := mean + (Rand.RollReal - 0.5) * step;
        FractalMap [x, y] := mean;
        with picFractalMap.Canvas do begin
          Brush.Color := AltColour (mean);
          FillRect (x, y, x + step, y + step);
        end;
        Count += 1;
        y += step;
      until y > 256;
      pgbFractalMap.Position := Count;
      pgbFractalMap.Repaint();
      picFractalMap.Repaint();
      x += step;
    until x > 512;

    // Centers
    x := start;
    repeat
      y := start;
      repeat
        mean := (FractalMap [x - start, y] + FractalMap [x + start, y]
          + FractalMap [x, y - start] + FractalMap [x, y + start]) / 4;
        mean := mean + (Rand.RollReal - 0.5) * step;
        FractalMap [x, y] := mean;
        with picFractalMap.Canvas do begin
          Brush.Color := AltColour (mean);
          FillRect (x, y, x + step, y + step);
        end;
        Count += 1;
        y += step;
      until y > 256;
      pgbFractalMap.Position := Count;
      pgbFractalMap.Repaint();
      picFractalMap.Repaint();
      x += step;
    until x > 512;

    picFractalMap.Repaint();
    pgbFractalMap.Position := Count;
    pgbFractalMap.Repaint();

    index -= 1;
  until (index < 1);


  // Now insert mountains
  labStatus.Caption := 'Now adding mountains...';

  for x := 0 to 512 do
    for y := 0 to 256 do
      Mountains [x, y] := 0;

  ranges := Rand.Roll(4) + 2;
  for index := 1 to ranges do begin
    // Choose starting position, mountain elevation and run length
    x := Rand.Roll (257) + 127;
    y := Rand.Roll (127) + 63;
    start := trunc ((Rand.RollReal + Rand.RollReal
      + Rand.RollReal + Rand.RollReal) * 96);
    mean := Rand.RollReal * 128 + 128;
    roll := 1 + trunc (mean / 40);

    repeat
      step := round ((Rand.RollReal * roll + 2) * 2);

      // Find next insertion point
      distance := 512;
      for x1 := (x - step) to (x + step) do
        for y1 := (y - step) to (y + step) do
          if ((x1>=0) and (x1<=512) and (y1>=0) and (y1<=256)) then
            if not ((x1 = x) and (y1 = y)) then
              if (distance > abs(FractalMap [x1, y1] - FractalMap [x, y])) then
                if (Mountains [x1, y1] < (mean / 2)) then begin
                  xt := x1;
                  yt := y1;
                  distance := abs(FractalMap [x1, y1] - FractalMap [x, y]);
                end;

      // Raise a mountain
      for x1 := (x - step) to (x + step) do
        for y1 := (y - step) to (y + step) do
          if ((x1>=0) and (x1<=512) and (y1>=0) and (y1<=256)) then begin
            x2 := abs (x1 - x);
            y2 := abs (y1 - y);
            distance := (step - sqrt (abs(x2 * x2 + y2 * y2))) * (mean / step);
            if (Mountains [x1, y1] < distance) then begin
              Mountains [x1, y1] := distance;
              distance += FractalMap [x1, y1];
              with picFractalMap.Canvas do begin
                Pixels [x1, y1] := AltColour (distance);
              end;
            end;
          end;
      Repaint();

      // Move insertion point
      x := xt;
      y := yt;
      start -= step;
    until (start < 0);
  end;

  // Blit Mountains
  for x := 0 to 512 do
    for y := 0 to 256 do
      FractalMap [x, y] += Mountains [x, y];

  // Re-draw map
  for x := 0 to 512 do
    for y := 0 to 256 do
      with picFractalMap.Canvas do begin
        distance := FractalMap [x, y];
        if (distance > 1023) then
          distance := 1023;
        Pixels [x, y] := AltColour (distance);
      end;
  picFractalMap.Repaint();

  // Finished with map build
  labStatus.Caption := 'Done!';

  btnNext.Enabled := TRUE;
  btnPrevious.Enabled := TRUE;
end;

end.

