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
unit gfracmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ComCtrls, StdCtrls, ExtCtrls,
  dpolipal, drandom;

type
  tFractalMap = class (tObject)
    private
      t_count : longint;
      rand : tRNG;
    public
      Elevation : array [0..512, 0..256] of real;
      Political : array [0..512, 0..256] of shortint;
      pgbFractalMap: TProgressBar;
      txtFractalLog: TMemo;
      picFractalMap: TImage;
      constructor Create (aRandomSeed : longint);
      destructor Destroy; override;
      function AltColour (Altitude : real) : TColor;
      procedure DoElevation (iter : integer);
      procedure Zero;
      procedure AddMountains;
  end;

implementation

constructor tFractalMap.Create (aRandomSeed : longint);
begin
  inherited Create;
  rand := tRNG.Create;
  rand.Seed := aRandomSeed;
end;

destructor tFractalMap.Destroy;
begin
  rand.Destroy;
  inherited Destroy;
end;

function tFractalMap.AltColour (Altitude : real) : TColor;
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

procedure tFractalMap.DoElevation (iter : integer);
var
  step,
  start,
  x,
  y : integer;
  mean : real;
begin
  step := 1 shl iter;
  start := step shr 1;

  // Tops and Bottoms
  x := start;
  repeat
    y := 0;
    repeat
      mean := (Elevation [x - start, y] + Elevation [x + start, y]) / 2;
      mean := mean + (rand.RollReal - 0.5) * step;
      Elevation [x, y] := mean;
      if (picFractalMap <> nil) then
        with picFractalMap.Canvas do begin
          Brush.Color := AltColour (mean);
          FillRect (x, y, x + step, y + step);
        end;
      t_count += 1;
      y += step;
    until y > 256;
    if (picFractalMap <> nil) then
      picFractalMap.Repaint();
    if (pgbFractalMap <> nil) then begin
      pgbFractalMap.Position := t_count;
      pgbFractalMap.Repaint();
    end;
    x += step;
  until x > 512;

  // Sides
  x := 0;
  repeat
    y := start;
    repeat
      mean := (Elevation [x, y - start] + Elevation [x, y + start]) / 2;
      mean := mean + (Rand.RollReal - 0.5) * step;
      Elevation [x, y] := mean;
      if (picFractalMap <> nil) then
        with picFractalMap.Canvas do begin
          Brush.Color := AltColour (mean);
          FillRect (x, y, x + step, y + step);
        end;
      t_count += 1;
      y += step;
    until y > 256;
    if (picFractalMap <> nil) then
      picFractalMap.Repaint();
    if (pgbFractalMap <> nil) then begin
      pgbFractalMap.Position := t_count;
      pgbFractalMap.Repaint();
    end;
    x += step;
  until x > 512;

  // Centers
  x := start;
  repeat
    y := start;
    repeat
      mean := (Elevation [x - start, y] + Elevation [x + start, y]
        + Elevation [x, y - start] + Elevation [x, y + start]) / 4;
      mean := mean + (Rand.RollReal - 0.5) * step;
      Elevation [x, y] := mean;
      if (picFractalMap <> nil) then
        with picFractalMap.Canvas do begin
          Brush.Color := AltColour (mean);
          FillRect (x, y, x + step, y + step);
        end;
      t_count += 1;
      y += step;
    until y > 256;

    if (picFractalMap <> nil) then
      picFractalMap.Repaint();
    if (pgbFractalMap <> nil) then begin
      pgbFractalMap.Position := t_count;
      pgbFractalMap.Repaint();
    end;
    x += step;
  until x > 512;

  if (picFractalMap <> nil) then
    picFractalMap.Repaint();
  if (pgbFractalMap <> nil) then begin
    pgbFractalMap.Position := t_count;
    pgbFractalMap.Repaint();
  end;
end;

procedure tFractalMap.AddMountains;
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
begin
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
              if (distance > abs(Elevation [x1, y1] - Elevation [x, y])) then
                if (Mountains [x1, y1] < (mean / 2)) then begin
                  xt := x1;
                  yt := y1;
                  distance := abs(Elevation [x1, y1] - Elevation [x, y]);
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
              distance += Elevation [x1, y1];
              if (picFractalMap <> nil) then
                with picFractalMap.Canvas do begin
                  Pixels [x1, y1] := AltColour (distance);
                end;
            end;
          end;
      if (picFractalMap <> nil) then
        picFractalMap.Repaint;

      // Move insertion point
      x := xt;
      y := yt;
      start -= step;
    until (start < 0);
  end;

  // Blit Mountains
  for x := 0 to 512 do
    for y := 0 to 256 do
      Elevation [x, y] += Mountains [x, y];

  // Re-draw map
  if (picFractalMap <> nil) then begin
    with picFractalMap.Canvas do
      for x := 0 to 512 do
        for y := 0 to 256 do begin
            distance := Elevation [x, y];
            if (distance > 1023) then
              distance := 1023;
            Pixels [x, y] := AltColour (distance);
          end;
    picFractalMap.Repaint;
  end;
end;

procedure tFractalMap.Zero;
var
  x,
  y : integer;
begin
  for x := 0 to 512 do
    for y := 0 to 256 do begin
      Elevation [x, y] := 0;
    end;
end;

end.

