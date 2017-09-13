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

{$region polipal}
const
  polipal : array [0..60] of longword = (
    16777215, // 255 255 255
    15790080, // 240 240   0
         240, //   0   0 240
    15728880, // 240   0 240
    15728640, // 240   0   0
       61440, //   0 240   0
       61680, //   0 240 240
     5263360, //  80  80   0
    10526800, // 160 160  80
    10485760, // 160   0   0
       40960, //   0 160   0
    10547280, // 160 240  80
    10486000, // 160   0 240
    15749200, // 240  80  80
       41200, //   0 160 240
     5243120, //  80   0 240
     5283840, //  80 160   0
     5242880, //  80   0   0
    15790240, // 240 240 160
     5304400, //  80 240  80
     5263520, //  80  80 160
       61520, //   0 240  80
    10526720, // 160 160   0
         160, //   0   0 160
    15728720, // 240   0  80
     5243040, //  80   0 160
    15749360, // 240  80 240
    15728800, // 240   0 160
    10547440, // 160 240 240
     5304320, //  80 240   0
    10506400, // 160  80 160
    15749280, // 240  80 160
       20720, //   0  80 240
    10485920, // 160   0 160
       41040, //   0 160  80
    15769840, // 240 160 240
       20480, //   0  80   0
     5242960, //  80   0  80
    15769760, // 240 160 160
       61600, //   0 240 160
    15749120, // 240  80   0
    10506480, // 160  80 240
       20640, //   0  80 160
    15790160, // 240 240  80
    10547360, // 160 240 160
       20560, //   0  80  80
          80, //   0   0  80
    10547200, // 160 240   0
     5304480, //  80 240 160
     5263600, //  80  80 240
     5283920, //  80 160  80
     5284000, //  80 160 160
    15769600, // 240 160   0
    15769680, // 240 160  80
     5304560, //  80 240 240
    10506240, // 160  80   0
    10526960, // 160 160 240
       41120, //   0 160 160
    10485840, // 160   0  80
    10506320, // 160  80  80
     5284080  //  80 160 240
  );
{$endregion polipal}


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
  PScratch : array [0..512, 0..256] of shortint;
  Political : array [0..512, 0..256] of shortint;
  Capitals : array [1..60] of TPoint;
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
  Done,
  keep : boolean;
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
    mean := Rand.RollReal * 64 + 64;
    roll := 1 + trunc (mean / 20);

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

  // Political Entities
  labStatus.Caption := 'Adding nations...';
  labStatus.Repaint();

  for x := 0 to 512 do
    for y := 0 to 256 do begin
      if (FractalMap [x, y] < 0) then
        Political [x, y] := -1
      else
        Political [x, y] := 0;
      Mountains [x, y] := 0;
    end;

  start := 0;
  for x := 1 to 10 do
    start := start + Rand.Roll(6) + 1;

  for step := 1 to start do begin
    repeat
      x := Rand.Roll (503) + 5;
      y := Rand.Roll (247) + 5;
      Keep := TRUE;
      if (Political [x, y] < 0) then
        Keep := FALSE;
      for index := 1 to (step - 1) do begin
        x1 := Capitals [index].x - x;
        y1 := Capitals [index].y - y;
        distance := sqrt (abs (sqr (x1) + sqr (y1)));
        if (distance < 4) then
          Keep := FALSE;
      end;
    until Keep;
    Capitals [index].x := x;
    Capitals [index].y := y;
    Political [x, y] := step;
    Political [x + 1, y] := step;
    Political [x - 1, y] := step;
    Political [x, y + 1] := step;
    Political [x, y - 1] := step;
  end;


  {$region VoronoiDiagram}
  for y := 0 to 256 do begin
    for x := 0 to 512 do begin
      ranges := 0;
      mean := 1024.0;
      for index := 1 to start do begin
        xt := Capitals [index].x - x;
        yt := Capitals [index].y - y;
        distance := sqrt (abs (xt * xt + yt * yt));
        if (distance < mean) then begin
          ranges := index;
          mean := distance;
        end;
      end;
      Political [x, y] := ranges;
      with picFractalMap.Canvas do begin
        if (FractalMap [x, y] < 0) then
          Pixels [x, y] := AltColour (FractalMap [x, y])
        else
          Pixels [x, y] := polipal [Political [x, y]];
      end;
    end;
    // Re-draw map
    Repaint();
  end;

  {$endregion VoronoiDiagram}

  {$region GrowthCA}
  {
  for x := 0 to 512 do
    for y := 0 to 256 do
      PScratch [x, y] := Political [x, y];

  // Run Cellular Automation

  repeat
    Done := TRUE;
    for index := 1 to start do begin
      for x := 0 to 512 do
        for y := 0 to 512 do
          if (Political [x, y] = 0) then begin
            // Record neighbours of each cell
            for x1 := 0 to 60 do begin
              Capitals [x1].x := 0;
            end;
            for x1 := x - 1 to x + 1 do
              for y1 := y - 1 to y + 1 do
                if ((x1 >= 0) and (x1 <= 512) and (y1 >= 0) and (y1 <= 256)) then
                  if (Political [x1, y1] >= 0) then
                    Capitals [Political [x1, y1]].x += 1;
            // Find which political entity has the most neighbours to the cell
            Capitals [0].x := 0;
            x1 := 0;
            for y1 := 1 to 60 do
              if (Capitals [y1].x > Capitals [x1].x) then
                x1 := y1;
            // Set Political cell
            if (x1 > 0) then begin
              PScratch [x, y] := x1;
              Done := FALSE;
            end
          end;
    end;

    // Re-draw map
    for x := 0 to 512 do
      for y := 0 to 256 do
        with picFractalMap.Canvas do begin
          Political [x, y] := PScratch [x, y];
          if (FractalMap [x, y] < 0) then
            Pixels [x, y] := AltColour (FractalMap [x, y])
          else
            Pixels [x, y] := polipal [Political [x, y]];
        end;
    picFractalMap.Repaint();
  until Done;
  }
  {$endregion GrowthCA}

  // Finished with map build
  labStatus.Caption := 'Done!';

  btnNext.Enabled := TRUE;
  btnPrevious.Enabled := TRUE;
end;

end.

