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
begin
  GetSeed := (t_z AND 65535) SHL 16 + (t_w AND 65535);
end;

procedure tRNG.SetSeed (aSeed : longword);
begin
  t_z := aSeed DIV 65536;
  t_w := aSeed MOD 65536;
end;

function tRNG.Roll (aRange : longword) : longword;
begin
  t_z := 36969 * (t_z AND 65535) + t_z SHL 16;
  t_w := 18000 * (t_w AND 65535) + t_w SHL 16;
  Roll := (((t_z SHL 16) + t_w) MOD aRange);
end;


function tRNG.RollReal : real;
begin
  t_z := 36969 * (t_z AND 65535) + t_z SHL 16;
  t_w := 18000 * (t_w AND 65535) + t_w SHL 16;
  RollReal := (((t_z SHL 16) + t_w)) / 4294967296;
end;

end.

