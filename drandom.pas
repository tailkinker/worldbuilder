unit drandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tRNG = class (tObject)
    private
      t_z,
      t_w : word;
      function GetSeed : longword;
      procedure SetSeed (aSeed : longword);
    public
      property Seed : longword read GetSeed write SetSeed;
      function Roll (aRange : longword) : longword;
  end;

implementation

function tRNG.GetSeed : longword;
begin
  GetSeed := t_z SHL 16 + t_w;
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

end.

