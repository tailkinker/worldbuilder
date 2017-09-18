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
unit gworld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  dpolipal, drandom, gfracmap;

type
  tWorld = class (tObject)
    private
      rand : tRNG;
      procedure SetRandomSeed (aRandomSeed : longint);
      function GetRandomSeed : longint;
    public
      labStatus: TLabel;
      FractalMap : tFractalMap;
      Capitals : array [1..60] of TPoint;
      property RandomSeed : longint read GetRandomSeed write SetRandomSeed;
      constructor Create (aRandomSeed : longint);
      destructor Destroy; override;
      procedure Genesis;
      procedure PlaceNations;
  end;

implementation

{tWorld}

constructor tWorld.Create (aRandomSeed : longint);
begin
  inherited Create;
  rand := tRNG.Create;
  rand.Seed := aRandomSeed;
  FractalMap := tFractalMap.Create (aRandomSeed);
end;

destructor tWorld.Destroy;
begin
  rand.Destroy;
  FractalMap.Destroy;
  inherited Destroy;
end;

procedure tWorld.SetRandomSeed (aRandomSeed : longint);
begin
  rand.Seed := aRandomSeed;
end;

function tWorld.GetRandomSeed : longint;
begin
  GetRandomSeed := rand.Seed;
end;

procedure tWorld.Genesis;
var
  index : integer;
begin
  FractalMap.Zero;
  for index := 9 downto 0 do
    FractalMap.DoElevation (index);

  // Now insert mountains
  if (labStatus <> nil) then begin
    labStatus.Caption := 'Now adding mountains...';
    labStatus.Repaint;
  end;
  FractalMap.AddMountains;

  // Political Entities
  if (labStatus <> nil) then begin
    labStatus.Caption := 'Adding nations...';
    labStatus.Repaint;
  end;
  PlaceNations;

  if (labStatus <> nil) then begin
    labStatus.Caption := 'Done!';
    labStatus.Repaint;
  end;
end;

procedure tWorld.PlaceNations;
var
  Nations,
  select,
  index,
  step,
  x,
  y,
  x1,
  y1,
  xt,
  yt : integer;
  mean,
  distance : real;
  Keep : boolean;
begin
  // Set up Political array, marking water areas as ineligible and land areas as
  // currently unowned.
  for x := 0 to 512 do
    for y := 0 to 256 do
      with FractalMap do begin
        if (Elevation [x, y] < 0) then
          Political [x, y] := -1
        else
          Political [x, y] := 0;
      end;

  // Roll 10d6 for number of nations.
  Nations := 0;
  for x := 1 to 10 do
    Nations := Nations + Rand.Roll(6) + 1;

  for step := 1 to Nations do begin
    repeat
      // Pick some random co-ordinates.
      x := Rand.Roll (503) + 5;
      y := Rand.Roll (247) + 5;
      Keep := TRUE;
      // Discard if already owned.
      if (FractalMap.Political [x, y] < 0) then
        Keep := FALSE;
      // Discard if too close to another capital.
      for index := 1 to (step - 1) do begin
        x1 := Capitals [index].x - x;
        y1 := Capitals [index].y - y;
        distance := sqrt (abs (sqr (x1) + sqr (y1)));
        if (distance < 4) then
          Keep := FALSE;
      end;
    until Keep;

    // Set capital co-ordinates and give them some territory.
    Capitals [index].x := x;
    Capitals [index].y := y;
    with FractalMap do begin
      Political [x, y] := step;
      Political [x + 1, y] := step;
      Political [x - 1, y] := step;
      Political [x, y + 1] := step;
      Political [x, y - 1] := step;
    end;
  end;

  // Use Voronoi diagram maths to select ownership.
  with FractalMap do begin
    for y := 0 to 256 do begin
      for x := 0 to 512 do begin
        select := 0;
        mean := 1024.0;
        // For each square, find the closest capital.
        for index := 1 to Nations do begin
          xt := Capitals [index].x - x;
          yt := Capitals [index].y - y;
          distance := sqrt (abs (xt * xt + yt * yt));
          if (distance < mean) then begin
            select := index;
            mean := distance;
          end;
        end;
        // Give this square to the chosen capital.
        Political [x, y] := select;
        if (picFractalMap <> nil) then
          with picFractalMap.Canvas do
            if (FractalMap.Elevation [x, y] < 0) then
              Pixels [x, y] := AltColour (FractalMap.Elevation [x, y])
            else
              Pixels [x, y] := polipal [Political [x, y]];
      end;
      // Re-draw map
      if (picFractalMap <> nil) then
        picFractalMap.Repaint();
    end;
  end;
end;

end.

