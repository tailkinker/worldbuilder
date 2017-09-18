{ Random Number Generator object

  Copyright (C) 2017 Timothy Groves (timothy.red.groves@gmail.com)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit drandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tRNG = class (tObject)
    private
      t_z,
      t_w : longword;
      function GetSeed : longword;
      procedure SetSeed (aSeed : longword);
    public
      property Seed : longword read GetSeed write SetSeed;
      function Roll (aRange : longword) : longword;
      function RollReal : real;
  end;

implementation

function tRNG.GetSeed : longword;
{
Returns the current random number seed as a 32-bit unsigned integer.
}
begin
  GetSeed := (t_z AND 65535) SHL 16 + (t_w AND 65535);
end;

procedure tRNG.SetSeed (aSeed : longword);
{
Sets the current random number seed.
}
begin
  t_z := aSeed DIV 65536;
  t_w := aSeed MOD 65536;
end;

function tRNG.Roll (aRange : longword) : longword;
{
Returns a random 32-bit unsigned integer in [0..aRange-1].  Also changes the
seed.
}
begin
  t_z := 36969 * (t_z AND 65535) + t_z SHL 16;
  t_w := 18000 * (t_w AND 65535) + t_w SHL 16;
  Roll := (((t_z SHL 16) + t_w) MOD aRange);
end;


function tRNG.RollReal : real;
{
Returns a random real value between 0 inclusive and 1 non-inclusive.  Also
changes the seed.
}
begin
  t_z := 36969 * (t_z AND 65535) + t_z SHL 16;
  t_w := 18000 * (t_w AND 65535) + t_w SHL 16;
  RollReal := (((t_z SHL 16) + t_w)) / 4294967296;
end;

end.

